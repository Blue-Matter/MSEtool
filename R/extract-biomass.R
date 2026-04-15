
extract_timeseries <- function(object, slot_name = 'Biomass', df = FALSE, 
                     byAge = FALSE, 
                     byArea = FALSE,
                     byFleet = FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  
  if (!df)
    return(
      slot(object, slot_name)
      )
  
  if (inherits(object, 'hist')) {
    return(
      extract_timeseries_hist(object, 
                              slot_name = slot_name,
                              df = df, 
                              byAge = byAge,
                              byArea = byArea,
                              byFleet = byFleet)
      )
  }
  
  
}

extract_timeseries_hist <- function(object, 
                                    slot_name = 'Biomass',
                                    df = FALSE, 
                                    byAge = FALSE, 
                                    byArea = FALSE,
                                    byFleet = FALSE) {
  
  array <- slot(object, slot_name)
  
  
  array2DF(array) |> ConvertDF()
  
}


Biomass <- function(object, df = FALSE) {
  extract_timeseries(object, 'Biomass', df)
}



# Biomass
# SBiomass
# SProdcution