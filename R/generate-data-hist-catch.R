GenHistData_Catch <- function(x, Data, Hist, HistYears, i, stocks, FleetNames, 
                               defaultCV=0.2, type=c('Landings', 'Discards')) {
  
  type <- match.arg(type, c('Landings', 'Discards'))
  
  # don't simulate data if real data exists
  if (!EmptyObject(slot(Data, type))) 
    return(slot(Data, type))
  
  AllObs <- lapply(Hist@OM@Obs[[i]], slot, type)
  no_obs <- all(lapply(AllObs, isNewObject) |> unlist()  == TRUE)
  
  # no Obs specified, don't simulate
  if (no_obs)
    return(slot(Data, type))
  
  
  nTS <- length(HistYears)
  nFleet <- length(FleetNames)
  
  CatchData <- new('catchdata')
  CatchData@Name <- FleetNames
  CatchData@Value <- array(NA, dim=c(nTS, nFleet),
                           dimnames=list(Year=HistYears,
                                         Fleet=FleetNames))
  CatchData@CV <- CatchData@Value 
  CatchData@CV[] <- defaultCV
  
  Real_Catch_Number <- slot(Hist, paste0(type, 'AtAge'))[stocks]
  
  CatchData@Units <- rep('Biomass', nFleet)
  
  for (fl in 1:nFleet) {
    CatchObs <- slot(Hist@OM@Obs[[i]][[fl]], type)
    if (EmptyObject(CatchObs)) 
      next()
  
    
    if (!is.null(CatchObs@Units))
      CatchData@Units[fl] <- CatchObs@Units
    
    
    if (CatchData@Units[fl] == "Number") {
      real_catch <- purrr::map(Real_Catch_Number, \(catch_n) {
        catch_n[x,,,fl,] |> SumOverAge() |> SumOverArea()
      }) |> List2Array('Stock') |>
        apply('Year', sum)
      

    } else if (CatchData@Units[fl] == "Biomass") {
      real_catch <- purrr::map2(Real_Catch_Number, Hist@OM@Fleet[stocks], 
                                  \(catch_n, fleet_list) {
                                    
                                    fleet_weight <- SubsetYear(fleet_list[[fl]]@WeightFleet[x,,,drop=FALSE], HistYears) |>
                                      abind::adrop(1)
                                    catch_age <- catch_n[x,,,fl,] |> SumOverArea()
                                    catch_age_biomass <- ArrayMultiply(catch_age, fleet_weight)
                                    SumOverAge(catch_age_biomass)
                                    
        
      }) |> List2Array('Stock') |>
        apply('Year', sum)
      
    } else {
      cli::cli_abort('Only {.val Biomass} or {.val Number} are valid {.val Units} in  {.val Obs@Landings} and {.val Obs@Discards}', .internal=TRUE)
    }
    
    Value[,fl] <- real_catch * CatchObs@Bias[x] * ArraySubsetYear(CatchObs@Error, HistYears)[x,]
  }
  
  CatchData@Value <- Value
  CatchData@CV <- CV
  CatchData
  
}
