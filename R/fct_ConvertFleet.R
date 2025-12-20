#' @rdname Convert
#' @param Fleet A [Fleet-class] object
#' @export
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
  fleet@Name <- Fleet@Name
  fleet@Name <- gsub("REPLACED -- ", '', fleet@Name)
  fleet
}

Fleet2Effort <- function(Fleet) {
  Effort(Value=data.frame(Year=Fleet@EffYears,
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

