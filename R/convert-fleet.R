#' Convert a Legacy Fleet Object to the Current `fleet` Class
#'
#' Converts a legacy [Fleet-legacy-class] object to the current [fleet-class]
#' by mapping each component to its corresponding new S4 sub-object.
#'
#' @param Fleet A [Fleet-legacy-class] object to convert. An [OM-legacy-class]
#'   object with a legacy `Fleet` slot is also accepted, in which case the
#'   `Fleet` slot is extracted and converted.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' ## Slot Mapping
#'
#' The following legacy slots are mapped to their current equivalents:
#'
#' | Legacy slot | New location |
#' |---|---|
#' | `Name` | `fleet@Name` |
#' | `EffYears`, `EffLower`, `EffUpper`, `Esd` | `Effort@Effort` (data frame) |
#' | `qcv`, `qinc` | `Catchability@qCV`, `@qInc` |
#' | `L5`, `LFS`, `Vmaxlen` | `Selectivity@Pars`, `@Model = DoubleNormal` |
#' | `LR5`, `LFR`, `Rmaxlen` | `Retention@Pars`, `@Model = DoubleNormalRetention` |
#'
#' ## Effort
#'
#' Historical effort is constructed as a data frame with columns `Year`,
#' `Lower`, `Upper`, and `CV`, passed to [Effort()]. The legacy `Esd` slot
#' contains a two-element vector of lower and upper bounds for a uniform
#' distribution of effort variability. Only `Esd[1]` is currently used as a
#' scalar CV; `Esd[2]` is silently dropped. 
#'
#' ## Selectivity
#'
#' `L5`, `LFS`, and `Vmaxlen` map to the [DoubleNormal()] double-normal
#' selectivity-at-length model. `Vmaxlen < 1` produces a dome-shaped
#' selectivity curve. See [SelectivityModels()] for the full set of available
#' selectivity models.
#'
#' ## Retention
#'
#' `LR5`, `LFR`, and `Rmaxlen` map to the [DoubleNormalRetention()]
#' double-normal retention-at-length model. `Rmaxlen < 1` produces a
#' dome-shaped retention curve. See [RetentionModels()] for the full set of
#' available retention models.
#'
#' ## Slots Not Converted
#'
#' The following slots have no equivalent in the legacy [Fleet-legacy-class]
#' and retain their default values in the converted object:
#'
#' - `DiscardMortality`: new functionality; no legacy equivalent.
#' - `Closure`: new functionality; no legacy equivalent.
#' - `Targeting`: new functionality; no legacy equivalent.
#' - `WeightFleet`: new functionality; no legacy equivalent.
#'
#' @return A [fleet-class] object.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [ConvertStock()],
#'   [ConvertObs()], [ConvertImp()], [Effort()], [Catchability()],
#'   [Selectivity()], [Retention()], [SelectivityModels()],
#'   [RetentionModels()]
#'
#' @examples
#' \dontrun{
#' fleet_legacy <- readRDS("MyLegacyFleet.rds")
#' fleet_new <- ConvertFleet(fleet_legacy)
#' }
#'
#' @export
ConvertFleet <- function(Fleet, silent = FALSE) {
  CheckClass(Fleet, c("Fleet", "OM"), "Fleet")
  
  if (!silent)
    cli::cli_alert("Converting object of class {.cls Fleet} to class {.cls fleet}")
  
  fleet               <- Fleet2Name(Fleet)
  fleet@Effort        <- Fleet2Effort(Fleet)
  fleet@Catchability  <- Fleet2Catchability(Fleet)
  fleet@Selectivity   <- Fleet2Selectivity(Fleet)
  fleet@Retention     <- Fleet2Retention(Fleet)
  # Not converted:
  # fleet@DiscardMortality
  # fleet@Closure
  # fleet@Targeting
  # fleet@WeightFleet
  fleet
}

Fleet2Name <- function(Fleet) {
  fleet <- Fleet()
  if (grepl("Stock:", Fleet@Name) & grepl("Fleet:", Fleet@Name)) {
    Fleet@Name <- gsub(".*Fleet:", '', Fleet@Name)
    Fleet@Name <- gsub("Obs model.*", '', Fleet@Name) |> trimws()
  }
  
  fleet@Name <- Fleet@Name
  fleet@Name <- gsub("REPLACED -- ", '', fleet@Name)
  
  fleet
}

Fleet2Effort <- function(Fleet) {
  Effort(Effort=data.frame(Year=Fleet@EffYears,
                          Lower=Fleet@EffLower,
                          Upper=Fleet@EffUpper,
                          CV=Fleet@Esd[1]))
}

Fleet2Catchability <- function(Fleet) {
  Catchability(qCV =  Fleet@qcv,
               qInc = Fleet@qinc)
}

Fleet2Selectivity <- function(Fleet) {
  Selectivity(Pars=list(L5=Fleet@L5,
                        LFS=Fleet@LFS,
                        Vmaxlen=Fleet@Vmaxlen))
}

Fleet2Retention <- function(Fleet) {
  Retention(Pars=list(LR5=Fleet@LR5,
                        LFR=Fleet@LFR,
                        Rmaxlen=Fleet@Rmaxlen))
}

