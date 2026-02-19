GenProjData_Catch <- function(x, 
                              Proj, 
                              DataYear,
                              YearsAll,
                              i,
                              stocks, 
                              type=c('Landings', 'Discards')) {
  
  type <- match.arg(type, c('Landings', 'Discards'))
  
  CatchData <- slot(Proj@Data[[x]][[i]], type)
  if (EmptyObject(CatchData))
    return(CatchData)
  
  nArea <- nArea(Proj)
  
  Value <- CatchData@Value
  CV <- CatchData@CV
 
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
    
    if (EmptyObject(Obs)) next()
    
    if (length(Obs@Error)<1) next()
    
    # Catch 
    if (!is.null(Proj@OM@Data[[i]]) && nrow(slot(Proj@OM@Data[[i]],type)@Value)>=TSIndex) {
      NewValue[,fl] <- slot(Proj@OM@Data[[i]],type)@Value[TSIndex,fl]
    } else {
      error <- ArraySubsetYear(Obs@Error, DataYear)[x]
      bias <- Obs@Bias[x] 
      
      if (CatchData@Units[fl] == 'Number') {
        real_catch <- purrr::map(Real_Catch_Number, \(catch_n) {
          catch_n[,fl,, drop=FALSE] |> sum()
        }) |> List2Array('Stock') |> sum()
        
        NewValue[,fl] <- real_catch * error * bias
        
      } else if (CatchData@Units[fl] == 'Biomass') {
        # Convert to Biomass
        real_catch_b <- purrr::map2(Real_Catch_Number, Proj@OM@Fleet[stocks], 
                                    \(catch_n, FleetList) {
                                      fleet <- FleetList[[fl]]
                                      catch_fleet <- catch_n[,fl,, drop=FALSE] |> abind::adrop(2)
                                      fleetwght <- fleet@WeightFleet
                                      dd <- dim(fleetwght)
                                      flwsim <- min(dd[1], x)
                                      fleetwght <- fleet@WeightFleet[flwsim,,TSIndex, drop=FALSE] |> 
                                        abind::adrop(c(1,3), one.d.array = TRUE) |>
                                        AddDimension('Area') |>
                                        ExtendAreas(1:nArea)
                                      ArrayMultiply(catch_fleet, fleetwght)
                                    }) |>
          List2Array('Stock') |>
          sum()
        
        NewValue[,fl] <- real_catch_b * error * bias
      } else {
        cli::cli_alert_warning('Landings & Discards data can only be in units of `Biomass` or `Number`') 
      }
    }  # end catch
    
    # CV
    if (!is.null(Proj@OM@Data[[i]]) && 
        !is.null(slot(Proj@OM@Data[[i]],type)@CV) &&  
        nrow(slot(Proj@OM@Data[[i]],type)@CV)>=TSIndex) {
      NewCV[,fl] <- slot(Proj@OM@Data[[i]],type)@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- SubsetYear(CatchData@CV, DataYear)[fl] 
    }
  } # end fleet loop
  
  CatchData@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  CatchData@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  CatchData
}