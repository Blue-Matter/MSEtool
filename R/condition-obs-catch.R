
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
  
  ObservedCatch_Fleet <- slot(FisheryData, type)@Value |> .ArraySubsetYear(Years=HistYears)
  
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
    CatchObs@Units <- FleetUnits[fl]
    
    # Years to use condition the observation error - default all historical
    if (is.null(CatchObs@Years)) CatchObs@Years <- FisheryData@Years
    
    if (Units=='Biomass') {
      SimValue <- Sim_Catch_Biomass[,,fl, drop=FALSE] |> abind::adrop(3)  
    } else {
      SimValue <- Sim_Catch_Number[,,fl, drop=FALSE] |> abind::adrop(3)  
    }
    SimValue[SimValue<0] <- 0
    
    SimValue <- .ArraySubsetYear(SimValue, CatchObs@Years)
    ObsValue <- .ArraySubsetYear(ObservedCatch, CatchObs@Years)
    
    d1 <- dim(SimValue)
    d2 <- dim(ObsValue)
    
    if (is.null(d2)) {
      ObsValue <- matrix(ObsValue, d1[1], length(ObsValue), byrow=TRUE)
      dimnames(ObsValue) <- list(Sim = seq_len(d1[1]),
                                 Year = names(ObservedCatch))
    }
      
    # Bias 
    Bias <- SimValue / ObsValue
    Bias[Bias<0.001] <- NA
    Bias[!is.finite(Bias)] <- NA
    
    BiasMean <- apply(Bias, 'Sim', mean, na.rm=TRUE)
    BiasMean[!is.finite(BiasMean)] <- 1
    Bias <- array(BiasMean, dim=c(length(BiasMean),
                                  length(CatchObs@Years)),
                                  dimnames=list(Sim=seq_along(BiasMean),
                                                Year=CatchObs@Years)
    )
    
    CatchObs@Bias <- Bias[,1]
    
    # Error
    ErrorHist <- matrix(ObservedCatch, nSim, length(ObservedCatch), byrow = TRUE)/(SimValue*Bias)
    ErrorHist[!is.finite(ErrorHist)] <- NA
    
    row_mean <- rowMeans(ErrorHist, na.rm = TRUE)
    row_mean[!is.finite(row_mean) | row_mean == 0] <- NA_real_
    StHistError <- ErrorHist / row_mean
    SD <- sqrt(rowMeans((StHistError - rowMeans(StHistError, na.rm = TRUE))^2, na.rm = TRUE)) # faster than `sd`
    
    SD[!is.finite(SD)] <- 1E-6
    
    CatchObs@CV <- SD
    
    # Generate obs error for projections 
    ErrorProj <- exp(matrix(rnorm(nSim * nProjTS,
                                  mean = rep(-(SD^2) / 2, each = nProjTS),
                                  sd   = rep(SD, each = nProjTS)),
                            nrow = nSim,
                            ncol = nProjTS,
                            byrow = TRUE
      )
    )
    dimnames(ErrorProj) <- list(Sim=1:nSim,
                                Year=ProjYears)
    
    Error <- abind::abind(ErrorHist,ErrorProj, along = 2, use.dnns = TRUE)
    CatchObs@Error <- Error
    slot(Hist@OM@Obs[[i]][[fl]], type) <- CatchObs
  }
  
  Hist
}
