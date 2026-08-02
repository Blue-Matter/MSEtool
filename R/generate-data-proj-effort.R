#' Generate Projected Effort Data
#'
#' Internal function to append one year of simulated observed effort to an
#' existing [effortdata-class] object during the projection period.
#'
#' @param x Integer index of the simulation replicate.
#' @param Proj A `hist` class object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector of all calendar years spanning the
#'   historical and projection periods.
#' @param i Integer index of the stock complex.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The function returns `EffortData` unchanged in two cases:
#'
#' - **No effort data exist**: `Proj@Data[[x]][[i]]@Effort` is empty
#'   (`EmptyObject(EffortData)`). Effort was not simulated historically,
#'   so projection data are not generated.
#' - **Year already present**: `DataYear` is already a row in
#'   `EffortData@Value`, indicating this year has been appended previously.
#'
#' ## Observation Error Model
#'
#' For each fleet with a non-empty [EffortObs()] object and a populated
#' `Error` array, observed effort for `DataYear` is generated as:
#'
#' \deqn{\tilde{E}_{t} = E_{t} \cdot b \cdot \varepsilon_{t}}
#'
#' - \eqn{E_{t}} — true OM effort in year \eqn{t}, resolved via
#'   `.ResolveValue()` using `TSIndex` to locate `DataYear` in `YearsAll`
#' - \eqn{b} — multiplicative bias for replicate `x` (`effortobs@Bias[x]`);
#'   see [EffortObs()]
#' - \eqn{\varepsilon_{t}} — lognormal error multiplier for replicate `x`,
#'   year \eqn{t} (`effortobs@Error[x, t]`); see [EffortObs()]
#'
#' CVs for the new year are resolved via `.ResolveCV()`, which looks up the
#' fleet- and year-specific CV from the existing [effortdata-class] object.
#'
#' ## Obs .Structure
#'
#' Observation parameters are accessed via:
#'
#' ```r
#' Proj@OM@Obs[[i]][[fl]]@Effort  # returns an effortobs object
#' ```
#'
#' where `i` is the stock complex index and `fl` the fleet index. The relevant
#' [EffortObs()] slots are:
#'
#' - `@Bias[x]`: per-replicate multiplicative bias
#' - `@Error[x, t]`: per-replicate, per-year lognormal error multiplier
#'
#' See [obs-class] and [EffortObs()] for full slot documentation.
#'
#' ## Appending
#'
#' The new year's `Value` and `CV` arrays (dimensions `[1 x nFleet]`) are
#' bound to the existing arrays along the year dimension using
#' `abind::abind(..., along = 1)`, preserving dimension names.
#'
#' @return An [effortdata-class] object with `DataYear` appended to `@Value`
#'   and `@CV`:
#'
#' - `@Value`: `[nYear+1 x nFleet]` array of observed effort
#' - `@CV`: `[nYear+1 x nFleet]` array of CVs
#'
#' @seealso [EffortObs()], [EffortData()], [effortdata-class], [obs-class],
#'   `.GenHistDataEffort()`
#' @keywords internal
.GenProjDataEffort <- function(x, Proj, DataYear, YearsAll, i) {
  
  EffortData <- Proj@Data[[x]][[i]]@Effort
  
  if (EmptyObject(EffortData)) return(EffortData)
  if (DataYear %in% dimnames(EffortData@Value)[[1]]) return(EffortData)
  
  TSIndex     <- match(DataYear, YearsAll)
  Value       <- EffortData@Value
  CV          <- EffortData@CV
  FleetNames  <- .ResolveFleetNames(EffortData)
  nFleet      <- length(FleetNames)
  
  NewValue <- .EmptyFleetArray(DataYear, FleetNames)
  NewCV    <- .EmptyFleetArray(DataYear, FleetNames)

  for (fl in seq_len(nFleet)) {
    Obs <- Proj@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(Obs) || length(Obs@Error) < 1) next
    
    NewValue[, fl] <- .ResolveValue(Proj, 'Effort', i, fl, TSIndex, Obs, x, DataYear)
    NewCV[, fl]    <- .ResolveCV(Proj, 'Effort', i, fl, TSIndex, EffortData, DataYear)
  }
  
  EffortData@Value <- abind::abind(Value, NewValue, along = 1, use.dnns = TRUE)
  EffortData@CV    <- abind::abind(CV, NewCV, along = 1, use.dnns = TRUE)
  EffortData
}

.GenProjDataEffortAll <- function(Proj, DataYear, YearsAll, i, nSim) {

  EffortData1 <- Proj@Data[[1]][[i]]@Effort
  unchanged   <- EmptyObject(EffortData1) || DataYear %in% dimnames(EffortData1@Value)[[1]]
  if (unchanged)
    return(purrr::map(Proj@Data, \(DataList) DataList[[i]]@Effort))

  TSIndex    <- match(DataYear, YearsAll)
  FleetNames <- .ResolveFleetNames(EffortData1)
  nFleet     <- length(FleetNames)

  NewValueAll <- matrix(NA_real_, nSim, nFleet)
  NewCVAll    <- matrix(NA_real_, nSim, nFleet)

  for (fl in seq_len(nFleet)) {
    Obs <- Proj@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(Obs) || length(Obs@Error) < 1) next

    NewValueAll[, fl] <- .ResolveValueAll(Proj, 'Effort', i, fl, TSIndex, Obs, nSim, DataYear)

    for (x in seq_len(nSim))
      NewCVAll[x, fl] <- .ResolveCV(Proj, 'Effort', i, fl, TSIndex, Proj@Data[[x]][[i]]@Effort, DataYear)
  }

  purrr::map(seq_len(nSim), \(x) {
    EffortData <- Proj@Data[[x]][[i]]@Effort
    NewValue <- .EmptyFleetArray(DataYear, FleetNames)
    NewCV    <- .EmptyFleetArray(DataYear, FleetNames)
    NewValue[1, ] <- NewValueAll[x, ]
    NewCV[1, ]    <- NewCVAll[x, ]
    EffortData@Value <- abind::abind(EffortData@Value, NewValue, along = 1, use.dnns = TRUE)
    EffortData@CV    <- abind::abind(EffortData@CV, NewCV, along = 1, use.dnns = TRUE)
    EffortData
  })
}

.ResolveValueAll <- function(Proj, slotname, i, fl, TSIndex, Obs, nSim, DataYear) {
  omData <- Proj@OM@Data[[i]]

  if (!is.null(omData)) {
    omDataSlot <- slot(omData, slotname)@Value
    if (!is.null(omDataSlot) &&
        nrow(omDataSlot) >= TSIndex &&
        ncol(omDataSlot) >= fl) {
      return(rep(omDataSlot[TSIndex, fl], nSim))
    }
  }

  obsError <- .ArraySubsetYear(Obs@Error, DataYear)
  sim_ind  <- pmin(seq_len(nSim), nrow(obsError))
  obsError <- obsError[sim_ind]

  sim_ind_bias <- pmin(seq_len(nSim), length(Obs@Bias))
  obsBias      <- Obs@Bias[sim_ind_bias]

  projValue <- slot(Proj, slotname)[seq_len(nSim), TSIndex, fl]
  projValue * obsError * obsBias
}
