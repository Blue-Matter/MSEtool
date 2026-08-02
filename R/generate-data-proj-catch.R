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
#' - `"Number"`: summed over age and area via `.ResolveCatchNumber()`.
#' - `"Biomass"`: multiplied by `WeightFleetRetained` (`type = "Landings"`) or
#'   `WeightFleetSelected` (`type = "Discards"`) before summing over age and
#'   area via `.ResolveCatchBiomass()`.
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
#' CVs for the new year are resolved via `.ResolveCV()`, which looks up the
#' fleet- and year-specific CV from the existing [catchdata-class] object.
#'
#' ## Obs .Structure
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
#'   `.GenHistDataCatch()`, `.GenProjDataEffort()`
#' @keywords internal
.GenProjDataCatch <- function(x,
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
  FleetNames <- .ResolveFleetNames(CatchData)
  nFleet     <- length(FleetNames)
  CatchData  <- .ResolveUnits(CatchData, nFleet)
  
  Real_Catch_Number <- purrr::map(slot(Proj, paste0(type, 'AtAge'))[stocks],
                                  \(catch_n) {
                                    catch_n[x,,TSIndex,,,drop=FALSE] |>
                                      abind::adrop(drop = c(1, 3))
                                  })
  
  NewValue <- .EmptyFleetArray(DataYear, FleetNames)
  NewCV    <- .EmptyFleetArray(DataYear, FleetNames)
  
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
      error <- .ArraySubsetYear(Obs@Error, DataYear)[x]
      bias  <- Obs@Bias[x]
      
      NewValue[, fl] <- switch(CatchData@Units[fl],
                               Number  = .ResolveCatchNumber(Real_Catch_Number, fl) * error * bias,
                               Biomass = .ResolveCatchBiomass(Proj, stocks, x, TSIndex, fl, nArea,
                                                             Real_Catch_Number, type = type) * error * bias
      )
    }
    
    NewCV[, fl] <- .ResolveCV(Proj, type, i, fl, TSIndex, CatchData, DataYear)
  }
  
  CatchData@Value <- abind::abind(Value, NewValue, along = 1, use.dnns = TRUE)
  CatchData@CV    <- abind::abind(CV, NewCV, along = 1, use.dnns = TRUE)
  CatchData
}

.GenProjDataCatchAll <- function(Proj, DataYear, YearsAll, i, stocks, nSim,
                                 type = c('Landings', 'Discards')) {

  type <- match.arg(type)

  CatchData1 <- slot(Proj@Data[[1]][[i]], type)
  CatchData1@Units[is.na(CatchData1@Units)] <- 'Biomass'

  unchanged <- EmptyObject(CatchData1) || DataYear %in% dimnames(CatchData1@Value)[[1]]
  if (unchanged)
    return(purrr::map(Proj@Data, \(DataList) {
      cd <- slot(DataList[[i]], type)
      cd@Units[is.na(cd@Units)] <- 'Biomass'
      cd
    }))

  TSIndex    <- match(DataYear, YearsAll)
  nArea      <- nArea(Proj)
  FleetNames <- .ResolveFleetNames(CatchData1)
  nFleet     <- length(FleetNames)
  CatchData1 <- .ResolveUnits(CatchData1, nFleet)

  Real_Catch_Number_All <- purrr::map(slot(Proj, paste0(type, 'AtAge'))[stocks],
                                      \(catch_n) {
                                        catch_n[,,TSIndex,,,drop=FALSE] |>
                                          abind::adrop(drop = 3)
                                      })

  NewValueAll <- matrix(NA_real_, nSim, nFleet)
  NewCVAll    <- matrix(NA_real_, nSim, nFleet)
  omData      <- Proj@OM@Data[[i]]

  for (fl in seq_len(nFleet)) {
    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(Obs) || length(Obs@Error) < 1) next

    hasOMVal <- !is.null(omData) &&
      !is.null(slot(omData, type)@Value) &&
      nrow(slot(omData, type)@Value) >= TSIndex

    if (hasOMVal) {
      NewValueAll[, fl] <- slot(omData, type)@Value[TSIndex, fl]
    } else {
      error <- .ArraySubsetYear(Obs@Error, DataYear)
      sim_ind <- pmin(seq_len(nSim), nrow(error))
      error   <- error[sim_ind]

      sim_ind_bias <- pmin(seq_len(nSim), length(Obs@Bias))
      bias         <- Obs@Bias[sim_ind_bias]

      NewValueAll[, fl] <- switch(CatchData1@Units[fl],
        Number  = .ResolveCatchNumberAll(Real_Catch_Number_All, fl, nSim) * error * bias,
        Biomass = vapply(seq_len(nSim), \(x) {
          Real_Catch_Number_x <- purrr::map(Real_Catch_Number_All, \(a)
            a[x, , , , drop = FALSE] |> abind::adrop(1)
          )
          .ResolveCatchBiomass(Proj, stocks, x, TSIndex, fl, nArea,
                              Real_Catch_Number_x, type = type)
        }, numeric(1)) * error * bias
      )
    }

    for (x in seq_len(nSim))
      NewCVAll[x, fl] <- .ResolveCV(Proj, type, i, fl, TSIndex,
                                    slot(Proj@Data[[x]][[i]], type), DataYear)
  }

  purrr::map(seq_len(nSim), \(x) {
    CatchData <- slot(Proj@Data[[x]][[i]], type)
    CatchData@Units[is.na(CatchData@Units)] <- 'Biomass'
    CatchData <- .ResolveUnits(CatchData, nFleet)
    NewValue <- .EmptyFleetArray(DataYear, FleetNames)
    NewCV    <- .EmptyFleetArray(DataYear, FleetNames)
    NewValue[1, ] <- NewValueAll[x, ]
    NewCV[1, ]    <- NewCVAll[x, ]
    CatchData@Value <- abind::abind(CatchData@Value, NewValue, along = 1, use.dnns = TRUE)
    CatchData@CV    <- abind::abind(CatchData@CV, NewCV, along = 1, use.dnns = TRUE)
    CatchData
  })
}

.ResolveCatchNumberAll <- function(Real_Catch_Number_All, fl, nSim) {
  per_stock <- purrr::map(Real_Catch_Number_All, \(catch_n) {
    catch_n[, , fl, , drop = FALSE] |> abind::adrop(drop = 3)  # [Sim, Age, Area]
  })
  totals_by_stock <- purrr::map(per_stock, \(a) apply(a, 1, sum))  # length-nSim per stock
  Reduce(`+`, totals_by_stock)
}
