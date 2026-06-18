
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
ConditionObs_Effort <- function(Hist,
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

  ObservedEffort_Fleet <- FisheryData@Effort@Value |> ArraySubsetYear(Years = HistYears)

  if (is.null(ObservedEffort_Fleet)) return(Hist)

  # Simulated effort: Sim x Year x Fleet -> subset to hist years
  SimEffort_All <- ArraySubsetYear(Hist@Effort, HistYears)   # Sim x Year x Fleet

  for (fl in seq_len(nFleet)) {

    fleet_name <- fleetnames[fl]
    fl_ind     <- match(fleet_name, colnames(ObservedEffort_Fleet))
    if (is.na(fl_ind)) next

    ObservedEffort <- ObservedEffort_Fleet[, fl_ind]
    if (all(is.na(ObservedEffort))) next

    EffortObs <- Hist@OM@Obs[[i]][[fleet_name]]@Effort

    # Years to use for conditioning — default all historical
    if (is.null(EffortObs@Years)) EffortObs@Years <- FisheryData@Years

    SimValue <- SimEffort_All[, , fl, drop = FALSE] |> abind::adrop(3)  # Sim x Year
    SimValue[SimValue < 0] <- 0

    SimValue      <- ArraySubsetYear(SimValue,      EffortObs@Years)
    ObsValue      <- ArraySubsetYear(ObservedEffort, EffortObs@Years)

    d1 <- dim(SimValue)
    d2 <- dim(ObsValue)

    if (is.null(d2)) {
      ObsValue <- matrix(ObsValue, d1[1], length(ObsValue), byrow = TRUE)
      dimnames(ObsValue) <- list(Sim  = seq_len(d1[1]),
                                 Year = names(ObservedEffort))
    }

    # Bias: mean ratio of simulated to observed across years, per sim
    Bias <- SimValue / ObsValue
    Bias[Bias < 0.001]    <- NA
    Bias[!is.finite(Bias)] <- NA

    BiasMean <- apply(Bias, 'Sim', mean, na.rm = TRUE)
    Bias <- array(BiasMean,
                  dim      = c(length(BiasMean), length(EffortObs@Years)),
                  dimnames = list(Sim  = seq_along(BiasMean),
                                  Year = EffortObs@Years))

    EffortObs@Bias <- Bias[, 1]

    # Lognormal error: standardised residuals of obs / (sim / bias)
    ErrorHist <- matrix(ObservedEffort, nSim, length(ObservedEffort), byrow = TRUE) /
      (SimValue * Bias)
    ErrorHist[!is.finite(ErrorHist)] <- NA

    row_mean <- rowMeans(ErrorHist, na.rm = TRUE)
    row_mean[!is.finite(row_mean) | row_mean == 0] <- NA_real_
    StHistError <- ErrorHist / row_mean
    SD <- sqrt(rowMeans((StHistError - rowMeans(StHistError, na.rm = TRUE))^2,
                        na.rm = TRUE))
    SD[!is.finite(SD)] <- 1e-6

    EffortObs@CV <- SD

    # Generate lognormal observation error for projection years
    ErrorProj <- exp(matrix(
      rnorm(nSim * nProjTS,
            mean = rep(-(SD^2) / 2, each = nProjTS),
            sd   = rep(SD,          each = nProjTS)),
      nrow   = nSim,
      ncol   = nProjTS,
      byrow  = TRUE
    ))
    dimnames(ErrorProj) <- list(Sim = seq_len(nSim), Year = ProjYears)

    Error <- abind::abind(ErrorHist, ErrorProj, along = 2, use.dnns = TRUE)
    EffortObs@Error <- Error

    Hist@OM@Obs[[i]][[fleet_name]]@Effort <- EffortObs
  }

  Hist
}
