
# HistSim <- HistSimList$`1`


GenerateHistoricalData <- function(HistSim) {
  
  if (length(HistSim@Data) > 0)
    stop("Real data not done yet")
  
  
  # TODO 
  # - check if it needs to do this for every sim - prior to this function 
  
  OM <- HistSim@OM
  
  HistSim@Data <- MakeNamedList(StockNames(OM), new('data'))
  nStock <- nStock(OM)
  
  for (stock in 1:nStock) {
    HistSim@Data[[stock]] <- GenerateHistoricalDataStock(stock, HistSim)
    
  }
  

    
   
    
  HistSim
  
}

GenerateHistoricalDataStock <- function(stock, HistSim) {
  Data <- HistSim@Data[[stock]]
  Data@Name <- HistSim@OM@Stock[[stock]]@Name
  Data@Time <- TimeSteps(HistSim@OM, 'Historical')
  Data@TimeLH <- Data@Time[length(Data@Time)]
  Data@Units <- HistSim@OM@Stock[[stock]]@Ages@Units
  
  Data@Catch <- GenerateHistoricalData_Catch(Data, HistSim, stock)
  

  Data@Index
  Data@CAA
  Data@CAL
  
  Data
}
  
  
GenerateHistoricalData_Catch <- function(Data, HistSim, stock) {
  HistTimeSteps <- TimeSteps(HistSim@OM, 'Historical')
  nTS <- length(HistTimeSteps)
  FleetNames <- HistSim@OM@Fleet[[stock]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  Data@Catch <- new('catch')
  Data@Catch@Value <- array(NA, dim=c(nTS, nFleet),
                               dimnames=list(TimeStep=HistTimeSteps,
                                             Fleet=FleetNames))
  
  Data@Catch@CV <- Data@Catch@Value 
  Data@Catch@CV[] <- 0.2
  
  Removals <- apply(List2Array(HistSim@Removals[[stock]]), c(2,4), sum) |> t()
  dimnames(Removals) <- list(TimeStep=HistTimeSteps, 
                             Fleet=FleetNames)
  
  Landings <- apply(List2Array(HistSim@Landings[[stock]]), c(2,4), sum) |> t()
  dimnames(Landings) <- list(TimeStep=HistTimeSteps, 
                             Fleet=FleetNames)


  for (fl in 1:nFleet) {
    Obs <- HistSim@OM@Obs[[stock]][[fl]]
    if (Obs@Catch@Type == 'removals') {
      Data@Catch@Value[,fl] <- Removals[,fl] * ArraySubsetTimeStep(Obs@Catch@Error, HistTimeSteps) * Obs@Catch@Bias[1]
    } else {
      Data@Catch@Value[,fl] <- Landings[,fl] * ArraySubsetTimeStep(Obs@Catch@Error, HistTimeSteps) * Obs@Catch@Bias[1]
    }
    
    NA_TS <- which(!HistTimeSteps %in% Obs@Catch@TimeSteps)
    if (length(NA_TS)>0) {
      Data@Catch@Value[NA_TS,fl] <- NA
    }
    Data@Catch@Type[fl] <- Obs@Catch@Type
  }
  
  Data@Catch@Units <- 'biomass'
  
  
  Data@Catch
}
  

  
  
  