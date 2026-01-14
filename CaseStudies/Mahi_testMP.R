
# Sorry it's super slow still, that will be fixed!!!!!

Hist <- readRDS("G:/My Drive/1_PROJECTS/openMSE_dev/CaseStudies/Mahi.hist")

Hist <- ReduceNSim(Hist, 2)
  


# ----- Effort ----
# Global Effort
EffortGlobal <- function(Data) {
  advice <- Advice()
  advice@Effort <- 1
  advice
}
class(EffortGlobal) = "mp"

# Effort by Fleet
EffortFleet <- function(Data) {
  nFleet <- 8
  advice <- Advice()
  advice@Effort <- seq(0.5, 2, length.out=nFleet)
  advice
}
class(EffortFleet) = "mp"


# Effort by Fleet & Area
# NOT DONE YET!
# EffortFleetArea <- function(Data) {
#   nFleet <- 8
#   nArea <- 5
#   advice <- Advice()
#   advice@Effort <- matrix(seq(0.5, 2, length.out=nFleet*nArea), nFleet, nArea)
#   advice
# }
# class(EffortFleetArea) = "mp"

MPs <- c("EffortGlobal",
         "EffortFleet")

MSE_Effort = Project(Hist, MPs)

# Effort Global
data.frame(FirstProj=MSE_Effort@Effort[1,1,1,,1]/MSE_Effort@Hist@Effort[1,1,148,],
           LastProj=MSE_Effort@Effort[1,1,80,,1]/MSE_Effort@Hist@Effort[1,1,148,])

# Effort by Fleet
data.frame(FirstProj=MSE_Effort@Effort[1,1,1,,2]/MSE_Effort@Hist@Effort[1,1,148,],
           LastProj=MSE_Effort@Effort[1,1,80,,2]/MSE_Effort@Hist@Effort[1,1,148,])


# ---- TAC ----
LastHistCatch <- Hist@Landings[1,1,148,]
globalTAC  <- sum(LastHistCatch) # 1871003
byfleetTAC <- LastHistCatch
byfleetAreaTAC <- matrix(LastHistCatch/5, 8,5)

# Global TAC 

# NOTE:: !! numerical issue if the TAC is set too low << historical catches !!

TACGlobal <- function(Data) {
  advice <- Advice()
  advice@TAC <- globalTAC
  advice
}
class(TACGlobal) = "mp"

# TAC by Fleet
TACFleet <- function(Data) {
  advice <- Advice()
  advice@TAC <- byfleetTAC
  advice
}
class(TACFleet) = "mp"

# TAC by Fleet & Area
TACFleetArea <- function(Data) {
  nFleet <- 8
  advice <- Advice()
  advice@TAC <- byfleetAreaTAC
  advice
}
class(TACFleetArea) = "mp"

MPs <- c("TACGlobal", "TACFleet", "TACFleetArea")

MSE_TAC = Project(Hist, MPs)

# Global and TAC by Fleet
round(MSE_TAC@Landings[1,1,1,,1:2]) # need to check how allocation is being calculated - or set OM@Allocation
apply(MSE_TAC@Landings[1,1,1,,1:2], 2, sum) / globalTAC

# TAC by Fleet and Area

# Temporary function to catch biomass by Area
Landings_WeightArea <- function(MSE) {
  NumberList <- MSE@LandingsAtAge
  years <- dimnames(NumberList$Dolphinfish)[["Year"]]
  list <- purrr::map2(Hist@OM@Fleet, NumberList, \(fleet, numbers) {
    fleetWeight <- fleet@WeightFleet |> 
      MSEtool:::AddDimension('Area') |> 
      MSEtool:::AddDimension('MP') |>
      MSEtool:::SubsetYear(years)
    catchb <- ArrayMultiply(fleetWeight, numbers)
    apply(catchb, c('Sim', 'Year', 'Fleet', 'Area', 'MP'), sum)
  })
  names(list) <- names(NumberList)
  list
}  

# !!! Something not quite right !!!! - optimizer is screwing up 
byfleetAreaTAC
Landings_WeightArea(MSE_TAC)[[1]][1,1,,,3]



# ---- Size Limit ----
LengthClasses <- dimnames(Hist@OM@Fleet$Dolphinfish@Selectivity@MeanAtLength)[["Class"]] |> as.numeric()

LengthClasses

SelectivityModels()

# Global Size Limit 
SizeLim <- function(Data) {
  advice <- Advice()
  # set selectivity-at-length directly 
  advice@Selectivity@MeanAtLength <- rep(1, 20) # all length classes selected 
  advice
}
class(SizeLim) = "mp"


# Size Limit by Fleet
SizeLimFleet <- function(Data) {
  advice <- Advice()
  FleetList <- vector('list', 8)
  FleetList[[1]] <- Selectivity(Pars=list(SL50=150, SL50_95=100))
  FleetList[[2]] <- Selectivity(Pars=list(SL50=250, SL50_95=100))
  FleetList[[3]] <- Selectivity(Pars=list(SL50=350, SL50_95=100))
  FleetList[[4]] <- Selectivity(Pars=list(SL50=450, SL50_95=100))
  FleetList[[5]] <- Selectivity(Pars=list(SL50=550, SL50_95=100))
  FleetList[[6]] <- Selectivity(Pars=list(SL50=650, SL50_95=100))
  FleetList[[7]] <- Selectivity(Pars=list(SL50=750, SL50_95=100))
  FleetList[[8]] <- Selectivity(Pars=list(SL50=850, SL50_95=100))
  
  advice@Selectivity <- FleetList
  advice
  
}
class(SizeLimFleet) = "mp"


MPs <- c("SizeLim", "SizeLimFleet")

MSE_SizeLim = Project(Hist, MPs)

MSE_SizeLim@Misc$Selectivity$SizeLim$Dolphinfish[1,,1,]
MSE_SizeLim@Misc$Selectivity$SizeLimFleet$Dolphinfish[1,,1,] |> round()




