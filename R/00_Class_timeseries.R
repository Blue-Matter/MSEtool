#' @include 00_Class_unions.R

setClass("timeseries",
         slots=c(Number='list', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Area
                 Biomass='array', # nSim, nStock, TimeStep
                 SBiomass='array', # nSim, nStock, TimeStep
                 SProduction='array',  # nSim, nStock, TimeStep
                 Landings='list', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 Discards='list', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 Effort='array', # Sim, Stock, TimeStep, Fleet
                 Distribution='array.list.null', # Sim, Stock, TimeStep, Fleet, Area # fraction effort by area
                 FDead='array.list.null',  # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet
                 FRetain='array.list.null', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet
                 FDeadArea='array.list.null', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 FRetainArea='array.list.null', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 Misc='list'
         )
)
