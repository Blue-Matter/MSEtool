GenProjData_Catch <- function(x, Proj, DataYear, YearsAll, y, stocks, type=c('Landings', 'Discards')) {
  
  type <- match.arg(type, c('Landings', 'Discards'))
  
  CatchData <- slot(Proj@Data[[x]][[i]], type)
  if (EmptyObject(CatchData))
    return(Proj)
  
 
  FleetNames <- CatchData@Name
  if (is.null(FleetNames)) {
    dd <- dim(CatchData@Value)
    FleetNames <- paste("Fleet", 1:dd[2])
  }
  
  nFleet <- length(FleetNames)
  
  # check units 
  if (length(CatchData@Units) != nFleet) {
    if (is.null(CatchData@Units)) {
      CatchData@Units <- rep('Biomass', nFleet)
    } else {
      CatchData@Units <- rep(CatchData@Units, nFleet)[1:nFleet]
    }
  }
  
  # time step index for DataYear
  TSIndex <- match(DataYear, YearsAll)
  
  # catch - number this time step (DataYear)
  Real_Catch_Number <- purrr::map( slot(Proj, paste0(type, 'AtAge'))[stocks], \(catch_n) {
    catch_n[x,,TSIndex,,,drop=FALSE] |>
      abind::adrop(drop=c(1,3))
  }) 
  

  
  # create output arrays
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(Year=DataYear,
                                    Fleet=FleetNames))
  NewCV <- NewValue
  
  for (fl in 1:nFleet) {
    Obs <- slot(Proj@OM@Obs[[i]][[fl]], type)
    
    if (length(Obs@Error)<1)
      next()
    
    # Catch 
    if (!is.null(Proj@OM@Data[[i]]) && nrow(slot(Proj@OM@Data[[i]],type)@Value)>=TSIndex) {
      NewValue[,fl] <- slot(Proj@OM@Data[[i]],type)@Value[TSIndex,fl]
    } else {
      error <- Obs@Error[x, TSIndex]
      bias <- Obs@Bias[x]
      
      if (CatchData@Units[fl] == 'Number') {
        real_catch <- purrr::map(Real_Catch_Number, \(catch_n) {
          catch_n[,fl,, drop=FALSE] |> SumOverAge() |> SumOverArea()
        }) |> List2Array('Stock') |> SumOverStock()
        
        NewValue[,fl] <- real_catch * error * bias
        
      } else if (DataCatch@Units[fl] == 'Biomass') {
        # Convert to Biomass
        
        # up to here !!
        
        
      } else {
        cli::cli_alert_warning('Landings & Discards data can only be in units of `Biomass` or `Number`') 
      }
    }
    
    
    
  } # end fleet loop
  

  
  CatchData
}