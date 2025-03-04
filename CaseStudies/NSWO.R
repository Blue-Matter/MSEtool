library(MSEtool)

la <- devtools::load_all

la()

# ---- Multi Stock & Multi Fleet (MOM) ----

dir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'


MOM <- SS2MOM(SSdir=dir,nsim=200, Name='North Atlantic Swordfish') 

# TODO Import - new SS2MOM for new OM structure 

# hack to fix discard mortality reverting to 0 in projections for some fleets
for (st in 1:2) {
  for (fl in 1:length(MOM@cpars[[1]])) {
    # d <- dim(MOM@cpars[[st]][[fl]]$Fdisc_array1)
    MOM@cpars[[st]][[fl]]$Fdisc_array1[,,72:121] <- MOM@cpars[[st]][[fl]]$Fdisc_array1[,,71, drop=FALSE]
    MOM@cpars[[st]][[fl]]$Fdisc_array2[,,72:121] <- MOM@cpars[[st]][[fl]]$Fdisc_array2[,,71, drop=FALSE]
  }
}

OMa <- Convert(MOM, Populate = FALSE)  # convert from `MOM` to `om`
OM <- Populate(OMa)



messages='default'
nSim=NULL
parallel=FALSE
silent=FALSE

SimulateDEV
             


replist <- r4ss::SS_output(dir)

replist$derived_quants$Label |> unique()

replist$derived_quants |> dplyr::filter(Label%in% c('annF_MSY', 'Dead_Catch_MSY', 'Ret_Catch_MSY',
                                                    "SSB_Virgin",  "SSB_Initial", "SSB_1950" ))


# TODO - fix OM in new Import function, current selectivity curves are wrong, then check MSY ref points against SS




multiHist <- Simulate(MOM)



multiHist # object.size

multiHist[[1]][[1]]@Ref$ByYear$FMSY[1,1]
multiHist[[1]][[1]]@Ref$ByYear$MSY[1,1] + multiHist[[2]][[1]]@Ref$ByYear$MSY[1,1]

multiHist[[1]][[1]]@Ref$ByYear$N0[1,1]
multiHist[[1]][[1]]@Ref$ByYear$SSB0[1,1]

multiHist[[1]][[1]]@Ref$ByYear$MSY[1,1]
multiHist[[1]][[1]]@Ref$ByYear$F_SPR[1,,1]

multiHist[[1]][[1]]@Ref$ByYear$F01_YPR[1,1]
multiHist[[1]][[1]]@Ref$ByYear$Fmax_YPR[1,1]

multiHist[[2]][[1]]@Ref$ByYear$Fcrash[1,]
multiHist[[2]][[1]]@Ref$ByYear$SPRcrash[1,]


names(multiHist[[1]][[1]]@Ref$ByYear) |> sort()
names(multiHist[[1]][[1]]@Ref$Dynamic_Unfished) |> sort()


x <- om@Fleet[[1]][[1]]
t <- ReduceArraysTS(x)


OM@Stock$Female@Length@MeanAtAge
OM@Stock$Female@Weight@MeanAtAge |> dim()

multiHist[[1]][[1]]@Ref$ByYear$F_SPR[1,,1]



array <- OM@Fleet[[1]][[4]]@FishingMortality@ApicalF 




OM@Fleet$Female$SPN_1@FishingMortality@ApicalF



multiHist[[1]][[1]]@Ref$ByYear$SSB0/multiHist[[1]][[1]]@Ref$ByYear$R0


# Unfished Equilibrium

multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,,1,] |> rowSums()
multiHist[[1]][[1]]@TSdata$Unfished_Equilibrium$N_at_age[1,,2,] |> rowSums()


multiHist[[1]][[1]]@Misc$MOM@Allocation[[1]][1,]
multiHist[[1]][[1]]@Misc$MOM@Allocation[[2]][1,]

multiHist[[1]][[1]]@Ref$ReferencePoints |> names()

multiHist[[1]][[1]]@Ref$ByYear |> names()




# Historical

# - Unfished
# - Reference Points
# - Initial Time Step
#   - optimize for initial depletion
# - Optimize catchability given Depletion
# - Simulate Population Dynamics
#   - N-at-age at beginning of next time step - age, growth, and mortality
#   - move N-at-age across areas
#   - calculate B etc by area
#   - calc F distribution by area and fleet
#   - calculate overall F
#   - repeat for all hist time-steps
# - Simulate Fishery Data
# - Return Historical



parallel=FALSE
messages='default'
nSim=NULL
silent=FALSE


SimulateDEV


# Projection 

# TODO
# OM@Allocation - dimension length
# OM@Efactor - dimension length
# Hermaphroditism do in Populate



# newMOM <- Convert(OM)  # convert `om` back to `MOM`


# setClass("hist",
#          slots=c(OM='om',
#                  Unfished='unfished',
#                  RefPoints='list',
#                  BatAge='list',
#                  SBatAge='list',
#                  SPatAge='list',
#                  Misc='list'
#          ),
#          contains='Created_ModifiedClass'
# )
# 
# hist <- new('hist')
# hist@Unfished@Equilibrium@SBatAge

# OM - Operating Model
# Reference Points
# Unfished
# PopulationDynamics
# FleetDynamics


# Hist |> Unfished() |> Equilibrium() |> NatAge() # TODO




