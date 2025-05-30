
# Equilibrium values for a given F 
setClass("curves",
         slots=c(FValues='numeric',
                 NPR='list',
                 NPRS='list',
                 SPR='list',
                 YPR='list',
                 RPR='list',
                 RelRec='list',
                 Recruit='list',
                 Yield='list',
                 Removal='list',
                 Biomass='list',
                 SBiomass='list',
                 SP='list',
                 Misc='list'
         )
)
