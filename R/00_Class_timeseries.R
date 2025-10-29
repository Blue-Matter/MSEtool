#' @include 00_Class_unions.R

setClass("timeseries",
         slots=c(Number='list', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Area
                 Biomass='array', # nSim, nStock, TimeStep
                 SBiomass='array', # nSim, nStock, TimeStep
                 SProduction='array',  # nSim, nStock, TimeStep
                 Landings='list', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 Discards='list', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 Effort='array', # sim, stock, timestep, fleet
                 FDead='array.list.null',  # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet
                 FRetain='array.list.null', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet
                 EffortArea='array.list.null', # list `nStock`of array dimensions: nSim, TimeStep, Fleet, Area
                 FDeadArea='array.list.null', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 FRetainArea='array.list.null', # list `nStock`of array dimensions: nSim, nAge, TimeStep, Fleet, Area
                 Misc='list'
         )
)
