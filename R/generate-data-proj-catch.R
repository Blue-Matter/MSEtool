#' Generate Projected Catch Data
#'
#' Internal function to append one year of simulated observed catch to an
#' existing [catchdata-class] object during the projection period.
#'
#' @param x Integer index of the simulation replicate.
#' @param Proj A `hist` class object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric vector of all calendar years spanning the
#'   historical and projection periods.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices used to subset catch-at-age
#'   arrays.
#' @param type Character. Either `"Landings"` or `"Discards"`.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The function returns `CatchData` unchanged in two cases:
#'
#' - **No catch data exist**: `slot(Proj@Data[[x]][[i]], type)` is empty
#'   (`EmptyObject(CatchData)`). Catch was not simulated historically, so
#'   projection data are not generated.
#' - **Year already present**: `DataYear` is already a row in
#'   `CatchData@Value`, indicating this year has been appended previously.
#'
#' ## True Catch Aggregation
#'
#' Catch-at-age for `DataYear` is extracted from `Proj@LandingsAtAge` or
#' `Proj@DiscardsAtAge` (selected by `type`) for each stock in `stocks`,
#' dropping the simulation and time dimensions. Per-fleet aggregation then
#' depends on `CatchData@Units[fl]`:
#'
#' - `"Number"`: summed over age and area via `resolveCatchNumber()`.
#' - `"Biomass"`: multiplied by `WeightFleet` before summing over age and
#'   area via `resolveCatchBiomass()`.
#'
#' ## Value Resolution
#'
#' For each fleet, the new observed value is resolved in order of precedence:
#'
#' - **OM data present**: if `Proj@OM@Data[[i]]` contains a pre-computed value
#'   for `type` at `TSIndex`, it is used directly without applying observation
#'   error.
#' - **Stochastic**: otherwise, the aggregated true catch is multiplied by
#'   bias and error from the [CatchObs()] object:
#'
#' \deqn{\tilde{C}_{t} = C_{t} \cdot b \cdot \varepsilon_{t}}
#'
#'   - \eqn{C_{t}} — true aggregated catch for `DataYear`
#'   - \eqn{b} — multiplicative bias for replicate `x` (`catchobs@Bias[x]`);
#'     see [CatchObs()]
#'   - \eqn{\varepsilon_{t}} — lognormal error multiplier for replicate `x`,
#'     year \eqn{t} (`catchobs@Error[x, t]`); see [CatchObs()]
#'
#' CVs for the new year are resolved via `resolveCV()`, which looks up the
#' fleet- and year-specific CV from the existing [catchdata-class] object.
#'
#' ## Obs Structure
#'
#' Observation parameters are accessed via:
#'
#' ```r
#' Proj@OM@Obs[[i]][[fl]]  # obs object; slot selected by `type`
#' ```
#'
#' where `i` is the stock complex index and `fl` the fleet index. The relevant
#' [CatchObs()] slots are:
#'
#' - `@Bias[x]`: per-replicate multiplicative bias
#' - `@Error[x, t]`: per-replicate, per-year lognormal error multiplier
#'
#' See [obs-class] and [CatchObs()] for full slot documentation.
#'
#' ## Appending
#'
#' The new year's `Value` and `CV` arrays (dimensions `[1 x nFleet]`) are
#' bound to the existing arrays along the year dimension using
#' `abind::abind(..., along = 1)`, preserving dimension names.
#'
#' @return A [catchdata-class] object with `DataYear` appended to `@Value`
#'   and `@CV`:
#'
#' - `@Value`: `[nYear+1 x nFleet]` array of observed catch
#' - `@CV`: `[nYear+1 x nFleet]` array of CVs
#'
#' @seealso [CatchObs()], [CatchData()], [catchdata-class], [obs-class],
#'   [GenHistData_Catch()], [GenProjData_Effort()]
#' @keywords internal
GenProjData_Catch <- function(x,
                              Proj,
                              DataYear,
                              YearsAll,
                              i,
                              stocks,
                              type = c('Landings', 'Discards')) {
  
  type      <- match.arg(type)
  
  CatchData <- slot(Proj@Data[[x]][[i]], type)
  CatchData@Units[is.na(CatchData@Units)] <- 'Biomass'
  
  if (EmptyObject(CatchData)) return(CatchData)
  if (DataYear %in% dimnames(CatchData@Value)[[1]]) return(CatchData)
  
  TSIndex    <- match(DataYear, YearsAll)
  nArea      <- nArea(Proj)
  Value      <- CatchData@Value
  CV         <- CatchData@CV
  FleetNames <- resolveFleetNames(CatchData)
  nFleet     <- length(FleetNames)
  CatchData  <- resolveUnits(CatchData, nFleet)
  
  Real_Catch_Number <- purrr::map(slot(Proj, paste0(type, 'AtAge'))[stocks],
                                  \(catch_n) {
                                    catch_n[x,,TSIndex,,,drop=FALSE] |>
                                      abind::adrop(drop = c(1, 3))
                                  })
  
  NewValue <- emptyFleetArray(DataYear, FleetNames)
  NewCV    <- emptyFleetArray(DataYear, FleetNames)
  
  for (fl in seq_len(nFleet)) {
    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(Obs) || length(Obs@Error) < 1) next
    
    omData   <- Proj@OM@Data[[i]]
    hasOMVal <- !is.null(omData) &&
      !is.null(slot(omData, type)@Value) &&
      nrow(slot(omData, type)@Value) >= TSIndex
    
    if (hasOMVal) {
      NewValue[, fl] <- slot(omData, type)@Value[TSIndex, fl]
    } else {
      error <- ArraySubsetYear(Obs@Error, DataYear)[x]
      bias  <- Obs@Bias[x]
      
      
      
      NewValue[, fl] <- switch(CatchData@Units[fl],
                               Number  = resolveCatchNumber(Real_Catch_Number, fl) * error * bias,
                               Biomass = resolveCatchBiomass(Proj, stocks, x, TSIndex, fl, nArea,
                                                             Real_Catch_Number) * error * bias
      )
    }
    
    NewCV[, fl] <- resolveCV(Proj, type, i, fl, TSIndex, CatchData, DataYear)
  }
  
  CatchData@Value <- abind::abind(Value, NewValue, along = 1, use.dnns = TRUE)
  CatchData@CV    <- abind::abind(CV, NewCV, along = 1, use.dnns = TRUE)
  CatchData
}