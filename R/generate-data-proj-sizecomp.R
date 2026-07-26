#' Generate Projected Size Composition Data
#'
#' Internal function to append one year of simulated size composition data to
#' an existing [compdata-class] object during the projection period.
#'
#' @param x Integer index of the simulation replicate.
#' @param Proj A `hist` class object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector of all calendar years spanning the
#'   historical and projection periods.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices used to subset catch-at-size
#'   arrays.
#' @param type Character. Either `"LandingsAtSize"` or `"DiscardsAtSize"`.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The function returns `CompData` unchanged in two cases:
#'
#' - **No composition data exist**: `slot(Proj@Data[[x]][[i]], type)` is empty
#'   (`EmptyObject(CompData)`). Size compositions were not simulated
#'   historically, so projection data are not generated.
#' - **Year already present**: `DataYear` is already a row in
#'   `CompData@Value`, indicating this year has been appended previously.
#'
#' ## True Composition
#'
#' Catch-at-size for `DataYear` is extracted from `Proj@LandingsAtSize` or
#' `Proj@DiscardsAtSize` (selected by `type`) for each stock in `stocks` at
#' time step `TSIndex`, dropping the simulation and time dimensions. The
#' result is summed over stocks and areas, computed independently for each
#' fleet since fleets are not required to share a size-class grid (see
#' [compdata-class]), then normalised to proportions \eqn{\mathbf{q}} within
#' each fleet.
#'
#' ## Value Resolution
#'
#' For each fleet, the new composition is resolved in order of precedence:
#'
#' - **OM data present**: if `Proj@OM@Data[[i]]` contains a pre-computed
#'   composition for `type` at `TSIndex`, it is used directly without
#'   applying observation error.
#' - **Stochastic**: otherwise, the Dirichlet concentration vector is
#'   constructed as:
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
#'
#' @return A [compdata-class] object with `DataYear` appended to `@Value`:
#'
#' - `@Value`: `[nYear+1 x nFleet x nSize]` array of composition counts
#'
#' @seealso [CompObs()], [CompData()], [compdata-class], [obs-class],
#'   [rDirichletMultinomial()], `.GenHistDataSizeComp()`, `.GenProjDataAgeComp()`
#' @keywords internal
.GenProjDataSizeComp <- function(x, Proj, DataYear, YearsAll, i, stocks,
                                 type = c('LandingsAtSize', 'DiscardsAtSize')) {
  
  type     <- match.arg(type)
  CompData <- slot(Proj@Data[[x]][[i]], type)

  if (EmptyObject(CompData)) return(CompData)
  if (DataYear %in% dimnames(CompData@Value)[[1]]) return(CompData)

  TSIndex     <- match(DataYear, YearsAll)
  FleetNames  <- .ResolveFleetNames(CompData)
  nFleet      <- length(FleetNames)
  ClassesList <- CompData@Classes
  nSizeMax    <- dim(CompData@Value)[3]
  Value       <- CompData@Value

  CatchAtSizeByFleet <- purrr::map(seq_len(nFleet), \(fl) {
    purrr::map(slot(Proj, type)[stocks], \(stock_level) {
      catch_n <- stock_level[[fl]]
      sim_x   <- min(x, dim(catch_n)[1])
      catch_n[sim_x,, TSIndex,,drop=FALSE] |>
        abind::adrop(drop = c(1, 3)) |>
        SumOverArea()
    }) |> List2Array('Stock') |>
      SumOverStock()
  }) |> stats::setNames(FleetNames)

  NewValue <- array(NA_real_,
                    dim      = c(1L, nFleet, nSizeMax),
                    dimnames = list(Year  = DataYear,
                                    Fleet = FleetNames,
                                    Class = seq_len(nSizeMax)))

  for (fl in seq_len(nFleet)) {
    nSize <- length(ClassesList[[fl]])
    if (!nSize) next

    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(Obs) || is.null(Obs@SampleSize)) next

    omData   <- Proj@OM@Data[[i]]
    hasOMVal <- !is.null(omData) &&
      !is.null(slot(omData, type)@Value) &&
      dim(slot(omData, type)@Value)[1] >= TSIndex

    if (hasOMVal) {
      NewValue[1, fl, seq_len(nSize)] <- slot(omData, type)@Value[TSIndex, fl, seq_len(nSize)]
    } else {
      sim_ss <- min(x, nrow(Obs@SampleSize))
      ss     <- .ArraySubsetYear(Obs@SampleSize, DataYear)[sim_ss]

      if (is.na(ss) || ss == 0) next

      ess <- if (!is.null(Obs@ESS)) {
        sim_ess <- min(x, nrow(Obs@ESS))
        .ArraySubsetYear(Obs@ESS, DataYear)[sim_ess]
      } else {
        ss
      }

      th <- if (!is.null(Obs@Theta)) {
        sim_th <- min(x, nrow(Obs@Theta))
        .ArraySubsetYear(Obs@Theta, DataYear)[sim_th]
      } else {
        1
      }

      true_n  <- CatchAtSizeByFleet[[fl]]
      total_n <- sum(true_n, na.rm = TRUE)
      if (is.na(total_n) || total_n == 0) next

      q <- true_n / total_n

      shift_b <- if (!is.null(Obs@Shift)) {
        sim_sh <- min(x, dim(Obs@Shift)[1])
        abind::adrop(.ArraySubsetYear(Obs@Shift, DataYear)[sim_sh, ,,drop=FALSE],1)[seq_len(nSize)]
      } else {
        rep(0, nSize)
      }

      alpha <- ess * th * q * exp(shift_b)
      if (any(is.na(alpha)) || sum(alpha) == 0) next

      NewValue[1, fl, seq_len(nSize)] <- rDirichletMultinomial(n = round(ss), alpha = alpha)
    }
  }

  CompData@Value <- abind::abind(Value, NewValue, along = 1, use.dnns = TRUE)
  CompData
}
