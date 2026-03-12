#' Convert a Legacy Fleet Object to a New fleet Class
#'
#' Converts a legacy [Fleet-legacy-class] object to the current [fleet-class]
#' by extracting and mapping each component to its corresponding new S4 class.
#'
#' @param Fleet A [Fleet-legacy-class] object to convert.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @return A [fleet-class] object with `Name`, `Effort`, `Catchability`,
#'   `Selectivity`, and `Retention` populated. Note that `DiscardMortality`,
#'   `Closure`, `Targeting`, and `WeightFleet` are not currently converted and
#'   retain their default values.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [Effort()],
#'   [Catchability()], [Selectivity()], [Retention()]
#'
#' @examples
#' \dontrun{
#' Fleetlegacy <- readRDS("MyLegacyFleet.rds")
#' fleet_new <- ConvertFleet(Fleetlegacy)
#' }
#'
ConvertFleet <- function(Fleet, silent = FALSE) {
  CheckClass(Fleet, "Fleet", "Fleet")
  
  if (!silent) {
    cli::cli_alert("Converting object of class {.cls Fleet} to class {.cls fleet}")
  }
  
  fleet <- Fleet2Name(Fleet)
  fleet@Effort <- Fleet2Effort(Fleet)
  fleet@Catchability <- Fleet2Catchability(Fleet)
  fleet@Selectivity <- Fleet2Selectivity(Fleet)
  fleet@Retention <- Fleet2Retention(Fleet)
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

