setClass("perrecruit",
         slots=c(apicalF='numeric', 
                 NPR0='array.list.null', # Sim, Age, Year or list by Stock
                 NPR0_SP='array.list.null', # Sim, Age, Year or list by Stock
                 NPRF='array.list.null', # Sim, Age, Year, F or list by Stock
                 NPRF_SP='array.list.null', # Sim, Age, Year, F or list by Stock
                 SPR0='array.numeric.null', # Sim, Stock, Year, F
                 SPRF='array.numeric.null', # Sim, Stock,  Year, F
                 SPR='array.numeric.null', # Sim, Stock, Year, F
                 Biomass='array.numeric.null', # Sim,Stock,  Year, F
                 SBiomass='array.numeric.null', # Sim, Stock, Year, F
                 SProduction='array.numeric.null', # Sim, Stock, Year, F
                 Removals='array.numeric.null', # Sim, Stock, Year, F
                 Landings='array.numeric.null', # Sim, Stock, Year, F
                 Misc='list'
         )
)

