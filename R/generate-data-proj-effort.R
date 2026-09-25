#' Generate Projected Effort Data
#'
#' Internal function to append one year of simulated observed effort to the
#' [effortdata-class] object of every simulation during the projection period.
#'
#' @param Proj A `hist` class object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector of all calendar years spanning the
#'   historical and projection periods.
#' @param i Integer index of the stock complex.
#' @param nSim Integer. Number of simulation replicates.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The existing `EffortData` of every simulation is returned unchanged in two
#' cases, both evaluated on simulation 1 (`Proj@Data[[1]][[i]]@Effort`):
#'
#' - **No effort data exist**: the object is empty
#'   (`EmptyObject(EffortData)`). Effort was not simulated historically,
#'   so projection data are not generated.
#' - **Year already present**: `DataYear` is already a row in
#'   `EffortData@Value`, indicating this year has been appended previously.
#'
#' ## Observation Error Model
#'
#' For each fleet with a non-empty [EffortObs()] object and a populated
#' `Error` array, observed effort for `DataYear` is resolved via
#' `.ResolveValue()`. If `Proj@OM@Data[[i]]` contains an effort value at
#' `TSIndex`, it is used for all simulations without observation error.
#' Otherwise:
#'
#' \deqn{\tilde{E}_{x,t} = E_{x,t} \cdot b_x \cdot \varepsilon_{x,t}}
#'
#' - \eqn{E_{x,t}} — true OM effort for replicate \eqn{x} in year \eqn{t}
#' - \eqn{b_x} — multiplicative bias (`effortobs@Bias[x]`); see [EffortObs()]
#' - \eqn{\varepsilon_{x,t}} — lognormal error multiplier
#'   (`effortobs@Error[x, t]`); see [EffortObs()]
#'
#' `Bias` and `Error` with fewer rows than `nSim` are recycled from their
#' last row.
#'
#' CVs for the new year are resolved per simulation via `.ResolveCV()`, which
#' looks up the fleet- and year-specific CV from that simulation's existing
#' [effortdata-class] object.
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
#' bound to each simulation's existing arrays along the year dimension using
#' `abind::abind(..., along = 1)`, preserving dimension names.
#'
#' @return A list of length `nSim` of [effortdata-class] objects, each with
#'   `DataYear` appended to `@Value` and `@CV`:
#'
#' - `@Value`: `[nYear+1 x nFleet]` array of observed effort
#' - `@CV`: `[nYear+1 x nFleet]` array of CVs
#'
#' @seealso [EffortObs()], [EffortData()], [effortdata-class], [obs-class],
#'   `.GenHistDataEffort()`
#' @keywords internal
.GenProjDataEffort <- function(Proj, DataYear, YearsAll, i, nSim) {

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

    NewValueAll[, fl] <- .ResolveValue(Proj, 'Effort', i, fl, TSIndex, Obs, nSim, DataYear)

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
