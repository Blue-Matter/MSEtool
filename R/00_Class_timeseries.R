#' @include 00_Class_unions.R

setClass("timeseries",
         slots=c(Number='list', # list `nStock`of array dimensions: nSim, nAge, Year, Area
                 Biomass='array', # nSim, nStock, Year
                 SBiomass='array', # nSim, nStock, Year
                 SProduction='array',  # nSim, nStock, Year
            
                 LandingsAtAge='list', # list `nStock`of array dimensions: nSim, nAge, Year, Fleet, Area - numbers
                 DiscardsAtAge='list', # list `nStock`of array dimensions: nSim, nAge, Year, Fleet, Area - numbers
                 LandingsAtSize='list', # list `nStock`of array dimensions: nSim, nClass, Year, Fleet, Area - size = Length unless selectivity at weight
                 DiscardsAtSize='list', # list `nStock`of array dimensions: nSim, nClass, Year, Fleet, Area - size = Length unless selectivity at weight
                 
                 Effort='array', # Sim, Stock, Year, Fleet
                 Distribution='array.list.null', # Sim, Stock, Year, Fleet, Area # fraction effort by area
                 FDead='array.list.null',  # list `nStock`of array dimensions: nSim, nAge, Year, Fleet
                 FRetain='array.list.null', # list `nStock`of array dimensions: nSim, nAge, Year, Fleet
                 
                 FDeadArea='array.list.null', # list `nStock`of array dimensions: nSim, nAge, Year, Fleet, Area
                 FRetainArea='array.list.null', # list `nStock`of array dimensions: nSim, nAge, Year, Fleet, Area
                 
                 Misc='list'
         )
)
