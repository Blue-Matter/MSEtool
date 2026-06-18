#' Generate Historical Fishing Effort Data
#'
#' Internal function to generate simulated historical fleet effort
#' when real observations are not available.
#'
#' @param x Integer index of the simulation replicate to extract.
#' @param Data Existing `data` object from `OM@Data`
#' @param Hist A `hist` class object.
#' @param HistYears Numeric vector of historical years.
#' @param i Integer index of the stock complex.
#' @param stocks Integer vector of stock indices in the complex.
#' @param FleetNames Character vector of fleet names.
#' @param defaultCV Numeric default coefficient of variation applied
#'   when no fleet-specific CV is provided. Default `0.2`.
#'
#' @details
#'  
#' ## Early Exit Conditions
#'
#' The function returns `Data@Effort` unchanged in two cases:
#'
#' - **Real data exist**: `Data@Effort` is already populated
#'   (`!EmptyObject(Data@Effort)`).
#' - **No observation structure defined**: every fleet's [EffortObs()] object
#'   is a default (unconditioned) object, as determined by `isNewObject()`.
#'
#' Fleets without an [EffortObs()] object receive `NA` effort values; only
#' fleets with a configured [EffortObs()] have simulated effort generated.
#'
#' ## Observation Error Model
#'
#' For each fleet with a non-empty [EffortObs()] object, observed effort
#' is generated as:
#'
#' \deqn{\tilde{E}_{t} = E_{t} \cdot b \cdot \varepsilon_{t}}
#'
#' - \eqn{E_{t}} — true OM effort in year \eqn{t} (`Hist@Effort[x, t, fl]`)
#' - \eqn{b} — multiplicative bias for replicate `x` (`effortobs@Bias[x]`);
#'   see [EffortObs()]
#' - \eqn{\varepsilon_{t}} — lognormal error multiplier for replicate `x`,
#'   year \eqn{t} (`effortobs@Error[x, t]`); see [EffortObs()]
#'
#' The `Error` array is subset to `HistYears` only before multiplication,
#' dropping any years outside the historical period.
#'
#' ## CV Handling
#'
#' CVs are initialised to `defaultCV` for all fleets and years. Fleet-specific
#' CVs from the [EffortObs()] object are not currently applied here;
#' `defaultCV` serves as the working assumption for downstream MPs.
#'
#' ## Obs Structure
#'
#' Observation parameters are accessed via:
#'
#' ```
#' Hist@OM@Obs[[i]][[fl]]@Effort  # returns an effortobs object
#' ```
#'
#' where `i` is the stock complex index and `fl` the fleet index. The relevant
#' [EffortObs()] slots are:
#'
#' - `@Bias[x]`: per-replicate multiplicative bias
#' - `@Error[x, t]`: per-replicate, per-year lognormal error multiplier
#' - `@Units`: effort units propagated to the output [effortdata-class]
#'
#' See [obs-class] and [EffortObs()] for full slot documentation.
#'
#' @return An [effortdata-class] object with:
#'
#' - `@Name`: character vector of fleet names
#' - `@Value`: `[nYear x nFleet]` array of observed effort
#' - `@CV`:  `[nYear x nFleet]` array initialised to `defaultCV`
#' - `@Units`: per-fleet units, from [EffortObs()] `@Units` where defined,
#'   otherwise `"unitless"`
#'
#' @seealso [EffortObs()], [EffortData()], [effortdata-class] [obs-class]
#' @keywords internal
GenHistData_Effort <- function(x, Data, Hist, HistYears, i, stocks, FleetNames, defaultCV = 0.2) {
  
  # Return real data unchanged if already populated
  if (!EmptyObject(Data@Effort))
    return(Data@Effort)
  
  AllEffortObs <- lapply(Hist@OM@Obs[[i]], slot, "Effort")
  no_obs <- all(unlist(lapply(AllEffortObs, isNewObject)))
  
  # No Obs specified for any fleet — skip simulation
  if (no_obs)
    return(Data@Effort)
  
  nTS    <- length(HistYears)
  nFleet <- length(FleetNames)
  
  EffortData       <- new('effortdata')
  EffortData@Name  <- FleetNames
  EffortData@Units <- rep('unitless', nFleet)
  
  Value <- array(NA, dim = c(nTS, nFleet),
                 dimnames = list(Year = HistYears, Fleet = FleetNames))
  CV      <- Value
  CV[]    <- defaultCV
  # Loop over fleets and apply observation error — only fleets with EffortObs get values
  for (fl in seq_len(nFleet)) {
    EffortObs <- Hist@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(EffortObs))
      next()

    if (!is.null(EffortObs@Units))
      EffortData@Units[fl] <- EffortObs@Units

    Value[, fl] <- Hist@Effort[x, , fl] *
      EffortObs@Bias[x] *
      ArraySubsetYear(EffortObs@Error, HistYears)[x, ]
  }
  
  EffortData@Value <- Value
  EffortData@CV    <- CV
  EffortData
}

