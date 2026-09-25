#' Generate Projected Size Composition Data
#'
#' Internal function to append one year of simulated size composition data to
#' the [compdata-class] object of every simulation during the projection
#' period.
#'
#' @param Proj A `hist` class object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector of all calendar years spanning the
#'   historical and projection periods.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices used to subset catch-at-size
#'   arrays.
#' @param nSim Integer. Number of simulation replicates.
#' @param type Character. Either `"LandingsAtSize"` or `"DiscardsAtSize"`.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The existing `CompData` of every simulation is returned unchanged in two
#' cases, both evaluated on simulation 1 (`slot(Proj@Data[[1]][[i]], type)`):
#'
#' - **No composition data exist**: the object is empty
#'   (`EmptyObject(CompData)`). Size compositions were not simulated
#'   historically, so projection data are not generated.
#' - **Year already present**: `DataYear` is already a row in
#'   `CompData@Value`, indicating this year has been appended previously.
#'
#' ## True Composition
#'
#' Catch-at-size for `DataYear` is extracted from `Proj@LandingsAtSize` or
#' `Proj@DiscardsAtSize` (selected by `type`) for each stock in `stocks` at
#' time step `TSIndex`, dropping the time dimension. The result is summed over
#' stocks and areas, computed independently for each fleet since fleets are
#' not required to share a size-class grid (see [compdata-class]), then
#' normalised to proportions \eqn{\mathbf{q}} within each simulation and
#' fleet.
#'
#' ## Value Resolution
#'
#' For each fleet, the new composition is resolved in order of precedence:
#'
#' - **OM data present**: if `Proj@OM@Data[[i]]` contains a pre-computed
#'   composition for `type` at `TSIndex`, it is used for all simulations
#'   without applying observation error.
#' - **Stochastic**: otherwise, the Dirichlet concentration vector for each
#'   simulation is constructed as:
#'
#' \deqn{\alpha_b = \mathrm{ESS} \cdot \Theta \cdot q_b \cdot \exp(\mathrm{Shift}_b)}
#'
#'   and a Dirichlet-Multinomial sample of size `SampleSize` is drawn via
#'   [rDirichletMultinomial()]. The slot defaults are:
#'
#'   - `ESS`: falls back to `SampleSize` if `NULL`
#'   - `Theta`: defaults to `1` if `NULL`
#'   - `Shift`: defaults to zero for all bins if `NULL`
#'
#'   Slots with fewer simulation rows than `nSim` are recycled from their last
#'   row.
#'
#' ## Obs .Structure
#'
#' Observation parameters are accessed via:
#'
#' ```r
#' Proj@OM@Obs[[i]][[fl]]@LandingsAtSize  # or @DiscardsAtSize
#' ```
#'
#' where `i` is the stock complex index and `fl` the fleet index. The relevant
#' [CompObs()] slots are:
#'
#' - `@SampleSize[sim, t]`: nominal sample size; output counts sum to this value
#' - `@ESS[sim, t]`: effective sample size scaling the concentration vector
#' - `@Theta[sim, t]`: Dirichlet-Multinomial dispersion parameter
#' - `@Shift[sim, t, bin]`: per-bin log-concentration offset
#'
#' See [obs-class] and [CompObs()] for full slot documentation.
#'
#' @return A list of length `nSim` of [compdata-class] objects, each with
#'   `DataYear` appended to `@Value`:
#'
#' - `@Value`: `[nYear+1 x nFleet x nSize]` array of composition counts
#'
#' @seealso [CompObs()], [CompData()], [compdata-class], [obs-class],
#'   [rDirichletMultinomial()], `.GenHistDataSizeComp()`, `.GenProjDataAgeComp()`
#' @keywords internal
.GenProjDataSizeComp <- function(Proj, DataYear, YearsAll, i, stocks, nSim,
                                    type = c('LandingsAtSize', 'DiscardsAtSize')) {
  type <- match.arg(type)

  CompData1 <- slot(Proj@Data[[1]][[i]], type)
  unchanged <- EmptyObject(CompData1) || DataYear %in% dimnames(CompData1@Value)[[1]]
  if (unchanged)
    return(purrr::map(Proj@Data, \(DataList) slot(DataList[[i]], type)))

  TSIndex     <- match(DataYear, YearsAll)
  FleetNames  <- .ResolveFleetNames(CompData1)
  nFleet      <- length(FleetNames)
  ClassesList <- CompData1@Classes
  nSizeMax    <- dim(CompData1@Value)[3]
  StockLevels <- slot(Proj, type)[stocks]

  NewValueAll <- array(NA_real_, dim = c(nSim, nFleet, nSizeMax))
  omData      <- Proj@OM@Data[[i]]

  for (fl in seq_len(nFleet)) {
    nSize <- length(ClassesList[[fl]])
    if (!nSize) next

    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(Obs) || is.null(Obs@SampleSize)) next

    hasOMVal <- !is.null(omData) && !is.null(slot(omData, type)@Value) &&
      dim(slot(omData, type)@Value)[1] >= TSIndex

    if (hasOMVal) {
      NewValueAll[, fl, seq_len(nSize)] <-
        matrix(slot(omData, type)@Value[TSIndex, fl, seq_len(nSize)], nSim, nSize, byrow = TRUE)
      next
    }

    true_n_all <- purrr::map(StockLevels, \(stock_level) {
      if (is.null(stock_level) || fl > length(stock_level)) return(NULL)
      catch_n <- stock_level[[fl]]
      catch_n[,, TSIndex,,drop=FALSE] |> abind::adrop(drop = 3) |> SumOverArea()
    })
    if (any(purrr::map_lgl(true_n_all, is.null))) next
    true_n_all <- true_n_all |> List2Array('Stock') |> SumOverStock()

    sim_ss <- pmin(seq_len(nSim), nrow(Obs@SampleSize))
    ss_all <- .ArraySubsetYear(Obs@SampleSize, DataYear)[sim_ss]

    ess_all <- if (!is.null(Obs@ESS)) {
      sim_ess <- pmin(seq_len(nSim), nrow(Obs@ESS))
      .ArraySubsetYear(Obs@ESS, DataYear)[sim_ess]
    } else ss_all

    th_all <- if (!is.null(Obs@Theta)) {
      sim_th <- pmin(seq_len(nSim), nrow(Obs@Theta))
      .ArraySubsetYear(Obs@Theta, DataYear)[sim_th]
    } else rep(1, nSim)

    true_n_all  <- true_n_all[, seq_len(nSize), drop = FALSE]
    total_n_all <- apply(true_n_all, 1, sum, na.rm = TRUE)

    for (x in seq_len(nSim)) {
      ss <- ss_all[x]
      if (is.na(ss) || ss == 0) next
      total_n <- total_n_all[x]
      if (is.na(total_n) || total_n == 0) next
      q <- true_n_all[x, ] / total_n

      shift_b <- if (!is.null(Obs@Shift)) {
        sim_sh <- min(x, dim(Obs@Shift)[1])
        abind::adrop(.ArraySubsetYear(Obs@Shift, DataYear)[sim_sh, ,,drop=FALSE], 1)[seq_len(nSize)]
      } else rep(0, nSize)

      alpha <- ess_all[x] * th_all[x] * q * exp(shift_b)
      if (any(is.na(alpha)) || sum(alpha) == 0) next
      seed_key <- paste(Proj@OM@Seed, DataYear, i, fl, type, x, sep = "_")
      NewValueAll[x, fl, seq_len(nSize)] <- .SeededDirichletMultinomial(seed_key, n = round(ss), alpha = alpha)
    }
  }

  purrr::map(seq_len(nSim), \(x) {
    CompData <- slot(Proj@Data[[x]][[i]], type)
    NewValue <- array(NA_real_, dim = c(1L, nFleet, nSizeMax),
                      dimnames = list(Year = DataYear, Fleet = FleetNames, Class = seq_len(nSizeMax)))
    NewValue[1, , ] <- NewValueAll[x, , ]
    CompData@Value <- abind::abind(CompData@Value, NewValue, along = 1, use.dnns = TRUE)
    CompData
  })
}
