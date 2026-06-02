#' Generate Historical Catch Data
#'
#' Internal function to generate simulated historical fleet catch (landings or
#' discards) when real observations are not available.
#'
#' @param x Integer index of the simulation replicate.
#' @param Data Existing `data` object from `OM@Data`.
#' @param Hist A `hist` class object.
#' @param HistYears Numeric vector of historical years.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices in the complex.
#' @param FleetNames Character vector of fleet names.
#' @param defaultCV Numeric default coefficient of variation applied when no
#'   fleet-specific CV is provided. Default `0.2`.
#' @param type Character. Either `"Landings"` or `"Discards"`.
#'
#' @details
#'
#' ## Early Exit Conditions
#'
#' The function returns `slot(Data, type)` unchanged in two cases:
#'
#' - **Real data exist**: `slot(Data, type)` is already populated
#'   (`!EmptyObject(slot(Data, type))`).
#' - **No observation structure defined**: every fleet's [CatchObs()] object
#'   is a default (unconditioned) object, as determined by `isNewObject()`.
#'
#' ## True Catch Aggregation
#'
#' Before applying observation error, true catch is aggregated from
#' `Hist@LandingsAtAge` or `Hist@DiscardsAtAge` (selected by `type`) across
#' stocks, ages, and areas for replicate `x`. The aggregation unit depends on
#' `CatchObs@Units` for each fleet:
#'
#' - `"Number"`: catch-at-age is summed over age and area, then summed over
#'   stocks within the complex.
#' - `"Biomass"`: catch-at-age is multiplied by `WeightFleet` before summing
#'   over age and area, then summed over stocks within the complex. 
#'
#' Any value of `Units` other than `"Number"` or `"Biomass"` raises an error.
#' Units default to `"Biomass"` unless specified in the [CatchObs()] object.
#'
#' ## Observation Error Model
#'
#' For each fleet with a non-empty [CatchObs()] object, observed catch is
#' generated as:
#'
#' \deqn{\tilde{C}_{t} = C_{t} \cdot b \cdot \varepsilon_{t}}
#'
#' - \eqn{C_{t}} — true aggregated catch in year \eqn{t} (see above)
#' - \eqn{b} — multiplicative bias for replicate `x` (`catchobs@Bias[x]`);
#'   see [CatchObs()]
#' - \eqn{\varepsilon_{t}} — lognormal error multiplier for replicate `x`,
#'   year \eqn{t} (`catchobs@Error[x, t]`); see [CatchObs()]
#'
#' The `Error` array is subset to `HistYears` only before multiplication,
#' dropping any years outside the historical period.
#'
#' ## CV Handling
#'
#' CVs are initialised to `defaultCV` for all fleets and years. Fleet-specific
#' CVs from the [CatchObs()] object are not currently applied here; `defaultCV`
#' serves as the working assumption for downstream MPs.
#'
#' ## Obs Structure
#'
#' Observation parameters are accessed via:
#'
#' ```r
#' Hist@OM@Obs[[i]][[fl]]  # obs object; slot selected by `type`
#' ```
#'
#' where `i` is the stock complex index and `fl` the fleet index. The relevant
#' [CatchObs()] slots are:
#'
#' - `@Bias[x]`: per-replicate multiplicative bias
#' - `@Error[x, t]`: per-replicate, per-year lognormal error multiplier
#' - `@Units`: catch units (`"Biomass"` or `"Number"`), propagated to the
#'   output [catchdata-class]
#'
#' See [obs-class] and [CatchObs()] for full slot documentation.
#'
#' @return A [catchdata-class] object with:
#'
#' - `@Name`: character vector of fleet names
#' - `@Value`: `[nYear x nFleet]` array of observed catch
#' - `@CV`: `[nYear x nFleet]` array initialised to `defaultCV`
#' - `@Units`: per-fleet units, from [CatchObs()] `@Units` where defined,
#'   otherwise `"Biomass"`
#'
#' @seealso [CatchObs()], [CatchData()], [catchdata-class], [obs-class],
#'   [GenHistData_Effort()]
#' @keywords internal
GenHistData_Catch <- function(x, Data, Hist, HistYears, i, stocks, FleetNames,
                              defaultCV = 0.2, type = c('Landings', 'Discards')) {
  
  type <- match.arg(type, c('Landings', 'Discards'))
  
  # Return real data unchanged if already populated
  if (!EmptyObject(slot(Data, type)))
    return(slot(Data, type))
  
  AllObs <- lapply(Hist@OM@Obs[[i]], slot, type)
  no_obs <- all(unlist(lapply(AllObs, isNewObject)))
  
  # No Obs specified for any fleet — skip simulation
  if (no_obs)
    return(slot(Data, type))
  
  nTS    <- length(HistYears)
  nFleet <- length(FleetNames)
  
  CatchData       <- new('catchdata')
  CatchData@Name  <- FleetNames
  CatchData@Units <- rep('Biomass', nFleet)
  
  Value <- array(NA, dim = c(nTS, nFleet),
                 dimnames = list(Year = HistYears, Fleet = FleetNames))
  CV   <- Value
  CV[] <- defaultCV
  
  Real_Catch_Number <- slot(Hist, paste0(type, 'AtAge'))[stocks]
  
  # Loop over fleets and apply observation error
  for (fl in seq_len(nFleet)) {
    CatchObs <- slot(Hist@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(CatchObs))
      next()
    
    if (!is.null(CatchObs@Units))
      CatchData@Units[fl] <- CatchObs@Units
    
    if (CatchData@Units[fl] == "Number") {
      real_catch <- purrr::map(Real_Catch_Number, \(catch_n) {
        catch_n[x,,,fl,] |> SumOverAge() |> SumOverArea()
      }) |> List2Array('Stock') |>
        apply('Year', sum) |> SumOverStock()
      
    } else if (CatchData@Units[fl] == "Biomass") {
      real_catch <- purrr::map2(Real_Catch_Number, Hist@OM@Fleet[stocks],
                                \(catch_n, fleet_list) {
                                  fleet_weight <- fleet_list[[fl]]@WeightFleet
                                  dd <- dim(fleet_weight)
                                  fl_x <- min(x, dd[1])
                                  
                                  fleet_weight <- SubsetYear(fleet_weight[fl_x,,,drop=FALSE], HistYears) |>
                                    abind::adrop(1)
                                  
                                  dd <- dim(catch_n)
                                  catch_x <- min(x, dd[1])
                                  catch_age <- catch_n[catch_x,,,fl,, drop=FALSE] |> SumOverArea() |>
                                    DropDimension('Sim') |>
                                    DropDimension('Fleet')
                                  catch_age_biomass <- ArrayMultiply(catch_age, fleet_weight)
                                  SumOverAge(catch_age_biomass)
                                  
                                }) |> List2Array('Stock') |>
        apply('Year', sum)
      
    } else {
      cli::cli_abort(
        'Only {.val Biomass} or {.val Number} are valid {.val Units} in {.val Obs@Landings} and {.val Obs@Discards}',
        .internal = TRUE
      )
    }
    
    Value[, fl] <- real_catch *
      CatchObs@Bias[x] *
      ArraySubsetYear(CatchObs@Error, HistYears)[x, ]
  }
  
  CatchData@Value <- Value
  CatchData@CV    <- CV
  CatchData
}