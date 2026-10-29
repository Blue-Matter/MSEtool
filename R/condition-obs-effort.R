
#' Condition Observed Effort for a Stock Complex
#'
#' Internal function to condition observation error from observed fishing effort.
#'
#' Computes simulated effort per fleet, derives fleet-specific bias and
#' lognormal observation error from the ratio of simulated to observed effort,
#' and generates lognormal observation error for projection years.
#'
#' @param Hist A [Hist()] object populated with historical fishery dynamics.
#' @param FisheryData A [Data()] object with real fishery data.
#' @param HistYears Numeric vector of historical years.
#' @param ProjYears Numeric vector of projection years.
#' @param stocks Integer vector of stock indices in the complex (unused for
#'   effort, which is fleet-level; retained for API consistency).
#' @param i Integer index of the observed data set.
#'
#' @keywords internal
.ConditionObsEffort <- function(Hist,
                                FisheryData,
                                HistYears,
                                ProjYears,
                                stocks,
                                i) {

  nHistTS <- length(HistYears)
  nProjTS <- length(ProjYears)
  nFleet  <- nFleet(Hist)
  nSim    <- nSim(Hist)

  fleetnames <- FleetNames(Hist)

  ObservedEffort_Fleet <- FisheryData@Effort@Value |> .AlignObsHistYears(HistYears)

  if (is.null(ObservedEffort_Fleet)) return(Hist)

  # Simulated effort: Sim x Year x Fleet -> subset to hist years
  SimEffort_All <- .ArraySubsetYear(Hist@Effort, HistYears)   # Sim x Year x Fleet

  for (fl in seq_len(nFleet)) {

    fleet_name <- fleetnames[fl]
    fl_ind     <- match(fleet_name, colnames(ObservedEffort_Fleet))
    if (is.na(fl_ind)) next

    ObservedEffort <- ObservedEffort_Fleet[, fl_ind]
    if (all(is.na(ObservedEffort))) next

    EffortObs <- Hist@OM@Obs[[i]][[fleet_name]]@Effort

    SimValue  <- SimEffort_All[, , fl, drop = FALSE] |> abind::adrop(3)  # Sim x Year
    EffortObs <- .ConditionBiasError(EffortObs, ObservedEffort, SimValue, HistYears, ProjYears, nSim)
    if (is.null(EffortObs)) next

    Hist@OM@Obs[[i]][[fleet_name]]@Effort <- EffortObs
  }

  Hist
}
