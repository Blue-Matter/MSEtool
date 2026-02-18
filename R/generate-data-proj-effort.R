
GenProjData_Effort <- function(x, Proj, DataYear, YearsAll, i, stocks) {
  
  EffortData <- Proj@Data[[x]][[i]]@Effort
  
  if (EmptyObject(EffortData))
    return(EffortData)
  
  TSIndex <- match(DataYear, YearsAll)
  
  Value <- EffortData@Value
  CV <- EffortData@CV
  
  if (DataYear %in% dimnames(Value)[[1]]) 
    return(EffortData)
  
  FleetNames <- EffortData@Name
  if (is.null(FleetNames)) {
    dd <- dim(Value)
    FleetNames <- paste("Fleet", 1:dd[2])
  }
  
  nFleet <- length(FleetNames)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(Year=DataYear,
                                    Fleet=FleetNames))
  NewCV <- NewValue
  
  for (fl in 1:nFleet) {
    Obs <- Proj@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(Obs)) next()
    
    if (length(Obs@Error)<1)  next()
    
    if (!is.null(Proj@OM@Data[[i]]) && nrow(Proj@OM@Data[[i]]@Effort@Value)>=TSIndex) {
      NewValue[,fl] <- Proj@OM@Data[[i]]@Effort@Value[TSIndex,fl]
    } else {
      error <- ArraySubsetYear(Obs@Error, DataYear)[x]
      bias <- Obs@Bias[x] 
      NewValue[,fl] <- Proj@Effort[x,TSIndex, fl] * error * bias
    }
    
    # CV 
    if (!is.null(Proj@OM@Data[[i]]) &&  nrow(Proj@OM@Data[[i]]@Effort@CV)>=TSIndex) {
      NewCV[,fl] <- Proj@OM@Data[[i]]@Effort@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- SubsetYear(EffortData@CV, DataYear)[fl]
    }
    
  }
  
  EffortData@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  EffortData@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  
  EffortData
}