
GenHistData_Effort <- function(x, Data, Hist, HistYears, i, stocks, FleetNames, defaultCV=0.2) {
  
  # don't simulate data if real data exists
  if (!EmptyObject(Data@Effort))
    return(Data@Effort)
  
  AllEffortObs <- lapply(Hist@OM@Obs[[i]], slot, "Effort")
  no_obs <- all(lapply(AllEffortObs, isNewObject) |> unlist()  == TRUE)
  
  # no Obs specified, don't simulate
  if (no_obs)
    return(Data@Effort)
  
  nTS <- length(HistYears)
  nFleet <- length(FleetNames)
  
  EffortData <- new('effortdata')
  EffortData@Name <- FleetNames
  
  Value <- array(NA, dim=c(nTS, nFleet),
                 dimnames=list(Year=HistYears,
                               Fleet=FleetNames))
  
  CV <- Value 
  CV[] <- defaultCV
  
  # Real Effort for sim x 
  Value[] <- Hist@Effort[x,,]
  
  EffortData@Units <- rep('unitless', nFleet)
  
  # loop over fleets and add obs error 
  for (fl in 1:nFleet) {
    EffortObs <- Hist@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(EffortObs)) 
      next()
    
    if (!is.null(EffortObs@Units))
        EffortData@Units[fl] <- EffortObs@Units
    
    Value[,fl] <- Value[,fl] * EffortObs@Bias[x] * ArraySubsetYear(EffortObs@Error, HistYears)[x,]
  }
  
  EffortData@Value <- Value
  EffortData@CV <- CV

  EffortData
  
}

