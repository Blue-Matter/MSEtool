
#' Condition Observed Catch for a Stock or Stock Complex
#'
#' Internal function to condition observation error from observed catch (landings or
#' discards).
#'
#' Computes simulated catch in number or biomass across stocks and fleets,
#' derives fleet-specific bias and observation error, and generates
#' lognormal observation error for projection years.
#'
#'
#' @param Hist A [Hist()] object populated with historical fishery dynamics.
#' @param FisheryData A [Data()] object with real fishery data
#' @param HistYears Numeric vector of historical years
#' @param ProjYears Numeric vector of projection years
#' @param stocks Integer vector of stock indices in the complex
#' @param i Integer index of observed data set
#' @param type Character, either `Landings` or `Discards`
#'
#' @keywords internal
.ConditionObsCatch <- function(Hist,
                               FisheryData, 
                               HistYears,
                               ProjYears, 
                               stocks,    # stocks in this complex
                               i,         # observe data set number
                               type=c('Landings', 'Discards')) {
  
  type <- match.arg(type, c('Landings', 'Discards'))
  
  nHistTS <- length(HistYears)
  nProjTS <- length(ProjYears)
  nFleet <- nFleet(Hist)
  nSim <- nSim(Hist)
  
  fleetnames <- FleetNames(Hist)
  
  ObservedCatch_Fleet <- slot(FisheryData, type)@Value |> .AlignObsHistYears(HistYears)
  
  if (is.null(ObservedCatch_Fleet)) return(Hist)

  # catch number [stock] sim, age, year, fleet, area
  Sim_Catch_Number_List <- slot(Hist, paste0(type,'AtAge'))[stocks]
  
  # catch biomass - sim, year, fleet
  # Landings use the retention-weighted schedule; Discards use the
  # selectivity-weighted schedule -- see the equivalent note in
  # .GenHistDataCatch().
  weight_slot <- if (type == 'Landings') 'WeightFleetRetained' else 'WeightFleetSelected'
  Sim_Catch_Biomass <- purrr::map2(Sim_Catch_Number_List,
                                   Hist@OM@Fleet[stocks],
    \(catch_n_at_age_area, fleetlist) {

          FleetWeight <- purrr::map(fleetlist, \(fleet) {
            slot(fleet, weight_slot) |> Subset(Years=HistYears)
          }) |> List2Array(pos=4)
          
          catch_n_at_age <- SumOverArea(catch_n_at_age_area)
          
          # sum over ages
          SumOverAge(ArrayMultiply(catch_n_at_age, FleetWeight))
    
  }) |> List2Array('Stock', pos=2) |>
    apply(c('Sim', 'Year', 'Fleet'), sum)
  
  # catch number - sim, stock, year, fleet
  Sim_Catch_Number <- purrr::map(Sim_Catch_Number_List, \(catch_n_at_age_area) {
    
    # sum over areas
    catch_n_at_age <- SumOverArea(catch_n_at_age_area)
    # sum over ages
    SumOverAge(catch_n_at_age)
    
  }) |> List2Array('Stock', pos=2) |>
    apply(c('Sim', 'Year', 'Fleet'), sum)
  
  # Fleet Units 
  FleetUnits <- slot(FisheryData,type)@Units
  if (is.null(FleetUnits)) FleetUnits <- 'Biomass'
  if (length(FleetUnits)!=nFleet) FleetUnits <- rep(FleetUnits, nFleet)[1:nFleet]
  
  # Loop over fleets and assign values to `Obs`
  for (fl in 1:nFleet) {
    fleet_name <-  fleetnames[fl]
    fl_ind <- match(fleet_name, colnames(ObservedCatch_Fleet))
    if (is.na(fl_ind)) next
    
    ObservedCatch <- ObservedCatch_Fleet[,fl_ind]
    if (all(is.na(ObservedCatch))) next
    CatchObs <- slot(Hist@OM@Obs[[i]][[fleet_name]], type)
   
    Units <- FleetUnits[fl]
    if (is.na(Units))
      Units <- 'Biomass'
    CatchObs@Units <- Units

    if (.CatchUnitType(Units)=='Biomass') {
      SimValue <- Sim_Catch_Biomass[,,fl, drop=FALSE] |> abind::adrop(3)  
    } else {
      SimValue <- Sim_Catch_Number[,,fl, drop=FALSE] |> abind::adrop(3)  
    }

    CatchObs <- .ConditionBiasError(CatchObs, ObservedCatch, SimValue, HistYears, ProjYears, nSim)
    if (is.null(CatchObs)) next
    slot(Hist@OM@Obs[[i]][[fl]], type) <- CatchObs
  }
  
  Hist
}

# [HistYears x Fleet] observed values; NA in years with no row
.AlignObsHistYears <- function(Value, HistYears) {
  if (is.null(Value)) return(NULL)
  RowYears <- rownames(Value) %||% HistYears[seq_len(nrow(Value))]
  Out <- matrix(NA_real_, length(HistYears), ncol(Value),
                dimnames = list(Year = HistYears, Fleet = colnames(Value)))
  ind <- match(round(HistYears, 6), round(as.numeric(RowYears), 6))
  Out[!is.na(ind), ] <- Value[ind[!is.na(ind)], , drop = FALSE]
  Out
}

# Bias, CV and [Sim x Year] Error of a catch/effort obs object; NULL if no years to condition on
.ConditionBiasError <- function(object, ObservedValue, SimValue, HistYears, ProjYears, nSim) {
  nHistTS  <- length(HistYears)
  SimValue <- .ArraySubsetYear(SimValue, HistYears)
  SimValue[SimValue < 0] <- 0

  CondInd <- which(!is.na(ObservedValue) & ObservedValue > 0)
  if (!is.null(object@Years))
    CondInd <- intersect(CondInd, which(round(HistYears, 6) %in% round(as.numeric(object@Years), 6)))
  if (!length(CondInd)) return(NULL)
  object@Years <- HistYears[CondInd]

  ObsMat <- matrix(ObservedValue, nSim, nHistTS, byrow = TRUE)
  Ratio  <- ObsMat[, CondInd, drop = FALSE] / SimValue[, CondInd, drop = FALSE]
  Ratio[!is.finite(Ratio) | Ratio < 0.001] <- NA
  Bias <- rowMeans(Ratio, na.rm = TRUE)
  Bias[!is.finite(Bias)] <- 1

  if (.UserSupplied(object, "Bias")) {
    Bias <- .SimValues(object@Bias, nSim)
  } else {
    object@Bias <- .NameSims(Bias, nSim)
  }

  ErrorHist <- ObsMat / (SimValue * Bias)
  ErrorHist[!is.finite(ErrorHist)] <- NA
  dimnames(ErrorHist) <- list(Sim = seq_len(nSim), Year = HistYears)

  CondError <- ErrorHist[, CondInd, drop = FALSE]
  row_mean <- rowMeans(CondError, na.rm = TRUE)
  row_mean[!is.finite(row_mean) | row_mean == 0] <- NA_real_
  StHistError <- CondError / row_mean
  SD <- sqrt(rowMeans((StHistError - rowMeans(StHistError, na.rm = TRUE))^2, na.rm = TRUE)) # faster than `sd`
  SD[!is.finite(SD)] <- 1E-6

  if (.UserSupplied(object, "CV")) {
    SD <- sdconv(1, .SimValues(object@CV, nSim))
  } else {
    object@CV <- .NameSims(SD, nSim)
  }

  ErrorProj <- .ProjObsError(object, SD, nSim, ProjYears)
  object@Error <- abind::abind(ErrorHist, ErrorProj, along = 2, use.dnns = TRUE)
  object
}

# projection-year catch/effort observation error: user-supplied `Error`, else lognormal with log-scale `SD`
.ProjObsError <- function(object, SD, nSim, ProjYears) {
  if (.UserSupplied(object, "Error"))
    return(.ProjYearValues(object@Error, nSim, ProjYears))
  nProjTS <- length(ProjYears)
  ErrorProj <- exp(matrix(rnorm(nSim * nProjTS,
                                mean = rep(-(SD^2) / 2, each = nProjTS),
                                sd   = rep(SD, each = nProjTS)),
                          nrow = nSim, ncol = nProjTS, byrow = TRUE))
  dimnames(ErrorProj) <- list(Sim = seq_len(nSim), Year = ProjYears)
  ErrorProj
}
