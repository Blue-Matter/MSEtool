setClass("perrecruit",
         slots=c(apicalF='numeric', 
                 NPR0='array.list.null', # Sim, Age, TimeStep or list by Stock
                 NPR0_SP='array.list.null', # Sim, Age, TimeStep or list by Stock
                 NPRF='array.list.null', # Sim, Age, TimeStep, F or list by Stock
                 NPRF_SP='array.list.null', # Sim, Age, TimeStep, F or list by Stock
                 SPR0='array.numeric.null', # Sim, Stock, TimeStep, F
                 SPRF='array.numeric.null', # Sim, Stock,  TimeStep, F
                 SPR='array.numeric.null', # Sim, Stock, TimeStep, F
                 Biomass='array.numeric.null', # Sim,Stock,  TimeStep, F
                 SBiomass='array.numeric.null', # Sim, Stock, TimeStep, F
                 SProduction='array.numeric.null', # Sim, Stock, TimeStep, F
                 Removals='array.numeric.null', # Sim, Stock, TimeStep, F
                 Landings='array.numeric.null', # Sim, Stock, TimeStep, F
                 Misc='list'
         )
)

