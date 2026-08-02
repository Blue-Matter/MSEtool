#' Generate Projected Age Composition Data
#'
#' Internal function to append one year of simulated age composition data to
#' an existing [compdata-class] object during the projection period.
#'
#' @param x Integer index of the simulation replicate.
#' @param Proj A `hist` class object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector of all calendar years spanning the
#'   historical and projection periods.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices used to subset catch-at-age
#'   arrays.
#' @param type Character. Either `"LandingsAtAge"` or `"DiscardsAtAge"`.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The function returns `CompData` unchanged in two cases:
#'
#' - **No composition data exist**: `slot(Proj@Data[[x]][[i]], type)` is empty
#'   (`EmptyObject(CompData)`). Age compositions were not simulated
#'   historically, so projection data are not generated.
#' - **Year already present**: `DataYear` is already a row in
#'   `CompData@Value`, indicating this year has been appended previously.
#'
#' ## True Composition
#'
#' Catch-at-age for `DataYear` is extracted from `Proj@LandingsAtAge` or
#' `Proj@DiscardsAtAge` (selected by `type`) for each stock in `stocks` at
#' time step `TSIndex`, dropping the simulation and time dimensions. The
#' result is summed over stocks and areas, retaining the fleet and age
#' dimensions, then normalised to proportions \eqn{\mathbf{q}} within each
#' fleet.
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
#' Proj@OM@Obs[[i]][[fl]]@LandingsAtAge  # or @DiscardsAtAge
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
#' ## Appending
#'
#' The new year's `Value` array (dimensions `[1 x nFleet x nAge]`) is bound
#' to the existing array along the year dimension using
#' `abind::abind(..., along = 1)`, preserving dimension names.
#'
#' @return A [compdata-class] object with `DataYear` appended to `@Value`:
#'
#' - `@Value`: `[nYear+1 x nFleet x nAge]` array of composition counts
#'
#' @seealso [CompObs()], [CompData()], [compdata-class], [obs-class],
#'   [rDirichletMultinomial()], `.GenHistDataAgeComp()`, `.GenProjDataCatch()`
#' @keywords internal
.GenProjDataAgeComp <- function(x, Proj, DataYear, YearsAll, i, stocks,
                                type = c('LandingsAtAge', 'DiscardsAtAge')) {
  
  type     <- match.arg(type)
  CompData <- slot(Proj@Data[[x]][[i]], type)
  
  if (EmptyObject(CompData)) return(CompData)
  if (DataYear %in% dimnames(CompData@Value)[[1]]) return(CompData)
  
  TSIndex    <- match(DataYear, YearsAll)
  FleetNames <- .ResolveFleetNames(CompData)
  nFleet     <- length(FleetNames)
  AgeClasses <- CompData@Classes
  nAge       <- length(AgeClasses)
  Value      <- CompData@Value
  
  # Aggregate true catch-at-age over stocks and areas for DataYear
  # Result: [nFleet x nAge]
  CatchAtAge_yr <- purrr::map(slot(Proj, type)[stocks], \(catch_n) {
    sim_x <- min(x, dim(catch_n)[1])
    catch_n[sim_x,, TSIndex,,,drop=FALSE] |>
      abind::adrop(drop = c(1, 3)) |>
      SumOverArea()
  }) 
  
  ageclasses <- purrr::map(CatchAtAge_yr, \(st) as.numeric(dimnames(st)$Age))
  
  if (length(CatchAtAge_yr)>1 && ! all(duplicated(ageclasses)[-1])) {
    CatchAtAge_yr <- .AlignAgeDim(CatchAtAge_yr)
  }
  CatchAtAge_yr <- CatchAtAge_yr |> List2Array('Stock') |> SumOverStock()
  
  NewValue <- array(NA_real_,
                    dim      = c(1L, nFleet, nAge),
                    dimnames = list(Year  = DataYear,
                                    Fleet = FleetNames,
                                    Age   = AgeClasses))
  
  for (fl in seq_len(nFleet)) {
    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(Obs) || is.null(Obs@SampleSize)) next
    
    omData   <- Proj@OM@Data[[i]]
    hasOMVal <- !is.null(omData) &&
      !is.null(slot(omData, type)@Value) &&
      dim(slot(omData, type)@Value)[1] >= TSIndex
    
    if (hasOMVal) {
      NewValue[1, fl, ] <- slot(omData, type)@Value[TSIndex, fl, ]
    } else {
      sim_ss     <- min(x, nrow(Obs@SampleSize))
      ss         <- .ArraySubsetYear(Obs@SampleSize, DataYear)[sim_ss]
      
      if (is.na(ss) || ss == 0) next
      
      sim_ess <- min(x, nrow(Obs@ESS))
      ess     <- if (!is.null(Obs@ESS)) {
        .ArraySubsetYear(Obs@ESS, DataYear)[sim_ess]
      } else {
        ss
      }
      
      sim_th <- min(x, nrow(Obs@Theta))
      th     <- if (!is.null(Obs@Theta)) {
        .ArraySubsetYear(Obs@Theta, DataYear)[sim_th]
      } else {
        1
      }
      
      true_n  <- CatchAtAge_yr[, fl]
      total_n <- sum(true_n, na.rm = TRUE)
      if (is.na(total_n) || total_n == 0) next
      
      q <- true_n / total_n
      
      shift_b <- if (!is.null(Obs@Shift)) {
        sim_sh <- min(x, dim(Obs@Shift)[1])
        .ArraySubsetYear(Obs@Shift, DataYear)[sim_sh, ]
      } else {
        rep(0, nAge)
      }
      
      alpha <- ess * th * q * exp(shift_b)
      if (any(is.na(alpha)) || sum(alpha) == 0) next

      seed_key <- paste(Proj@OM@Seed, DataYear, i, fl, type, x, sep = "_")
      NewValue[1, fl, ] <- .SeededDirichletMultinomial(seed_key, n = round(ss), alpha = alpha)
    }
  }
  
  CompData@Value <- abind::abind(Value, NewValue, along = 1, use.dnns = TRUE)
  CompData
}

.GenProjDataAgeCompAll <- function(Proj, DataYear, YearsAll, i, stocks, nSim,
                                   type = c('LandingsAtAge', 'DiscardsAtAge')) {
  type <- match.arg(type)

  CompData1 <- slot(Proj@Data[[1]][[i]], type)
  unchanged <- EmptyObject(CompData1) || DataYear %in% dimnames(CompData1@Value)[[1]]
  if (unchanged)
    return(purrr::map(Proj@Data, \(DataList) slot(DataList[[i]], type)))

  TSIndex    <- match(DataYear, YearsAll)
  FleetNames <- .ResolveFleetNames(CompData1)
  nFleet     <- length(FleetNames)
  AgeClasses <- CompData1@Classes
  nAge       <- length(AgeClasses)

  CatchAtAge_yr <- purrr::map(slot(Proj, type)[stocks], \(catch_n) {
    catch_n[,, TSIndex,,,drop=FALSE] |> abind::adrop(drop = 3) |> SumOverArea()
  })
  ageclasses <- purrr::map(CatchAtAge_yr, \(st) as.numeric(dimnames(st)$Age))
  if (length(CatchAtAge_yr) > 1 && !all(duplicated(ageclasses)[-1]))
    CatchAtAge_yr <- .AlignAgeDim(CatchAtAge_yr)
  CatchAtAge_yr <- CatchAtAge_yr |> List2Array('Stock') |> SumOverStock()

  NewValueAll <- array(NA_real_, dim = c(nSim, nFleet, nAge))
  omData      <- Proj@OM@Data[[i]]

  for (fl in seq_len(nFleet)) {
    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(Obs) || is.null(Obs@SampleSize)) next

    hasOMVal <- !is.null(omData) && !is.null(slot(omData, type)@Value) &&
      dim(slot(omData, type)@Value)[1] >= TSIndex

    if (hasOMVal) {
      NewValueAll[, fl, ] <- matrix(slot(omData, type)@Value[TSIndex, fl, ], nSim, nAge, byrow = TRUE)
      next
    }

    sim_ss  <- pmin(seq_len(nSim), nrow(Obs@SampleSize))
    ss_all  <- .ArraySubsetYear(Obs@SampleSize, DataYear)[sim_ss]

    sim_ess <- pmin(seq_len(nSim), nrow(Obs@ESS))
    ess_all <- if (!is.null(Obs@ESS)) .ArraySubsetYear(Obs@ESS, DataYear)[sim_ess] else ss_all

    sim_th  <- pmin(seq_len(nSim), nrow(Obs@Theta))
    th_all  <- if (!is.null(Obs@Theta)) .ArraySubsetYear(Obs@Theta, DataYear)[sim_th] else rep(1, nSim)

    true_n_all  <- CatchAtAge_yr[, , fl, drop = FALSE] |> abind::adrop(3)
    total_n_all <- apply(true_n_all, 1, sum, na.rm = TRUE)

    for (x in seq_len(nSim)) {
      ss <- ss_all[x]
      if (is.na(ss) || ss == 0) next
      total_n <- total_n_all[x]
      if (is.na(total_n) || total_n == 0) next
      q <- true_n_all[x, ] / total_n

      shift_b <- if (!is.null(Obs@Shift)) {
        sim_sh <- min(x, dim(Obs@Shift)[1])
        .ArraySubsetYear(Obs@Shift, DataYear)[sim_sh, ]
      } else rep(0, nAge)

      alpha <- ess_all[x] * th_all[x] * q * exp(shift_b)
      if (any(is.na(alpha)) || sum(alpha) == 0) next
      seed_key <- paste(Proj@OM@Seed, DataYear, i, fl, type, x, sep = "_")
      NewValueAll[x, fl, ] <- .SeededDirichletMultinomial(seed_key, n = round(ss), alpha = alpha)
    }
  }

  purrr::map(seq_len(nSim), \(x) {
    CompData <- slot(Proj@Data[[x]][[i]], type)
    NewValue <- array(NA_real_, dim = c(1L, nFleet, nAge),
                      dimnames = list(Year = DataYear, Fleet = FleetNames, Age = AgeClasses))
    NewValue[1, , ] <- NewValueAll[x, , ]
    CompData@Value <- abind::abind(CompData@Value, NewValue, along = 1, use.dnns = TRUE)
    CompData
  })
}
