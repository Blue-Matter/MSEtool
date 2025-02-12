# TODO 
# get working with single stock single fleet
# then expand to multi-fleet
# then multi-stock?

M <- 0.2
Effort <- 2000
q <- 0.00025
F <- Effort*q
Z <- M+F
TotalBiomass <- 1000
TotalCatch <- F/Z * (1-exp(-Z)) * TotalBiomass

RelativeSize <- c(1, 1)
RelativeSize <- RelativeSize/sum(RelativeSize)
RelativeDist <- c(1, 1)
RelativeDist <- RelativeDist/sum(RelativeDist)
BiomassByArea <- TotalBiomass * RelativeDist
Density <- (BiomassByArea/RelativeSize)
RelDensity <- Density/sum(Density)
RelQ <- q * RelDensity

# solve spatTarg by fleet
opt <- function(spatTarg) {
  
 # EffortDist <- BiomassByArea^spatTarg
  
  EffortDist <- RelDensity^spatTarg
  
  # EffortDist <- EffortDist/sum(EffortDist)
  
  FArea <- Effort * EffortDist * q / RelativeSize

  ZArea <- FArea + M 
  CatchArea <- FArea/ZArea * (1-exp(-ZArea)) * BiomassByArea
  sum(CatchArea)
  TotalCatch
  
  FArea <- CalcFfromCatchArea(Catch=CatchArea,
                              PopatAge=TotalBiomass,
                              MatAge=M,
                              SelectAtAge=1)
  
  # return((sum(FArea)-F)^2)
  EffortArea <- FArea/sum(FArea) * Effort
  CatchArea/EffortArea
  CPUEArea <- CatchArea/FArea
  
  # utility over stocks
  
  ssq <- (CPUEArea[2]-CPUEArea[1])^2
  ssq[2] <- (sum(CatchArea)-TotalCatch)^2
  
  sum(ssq)
}


spatTarg <- c(-10, 10)
dopt <- optimise(opt, spatTarg)
dopt

spatTarg <- dopt$minimum



#
opt <- function(logFArea) {
  
  FArea <- exp(logFArea)
  
  ZArea <- FArea + M 
  CatchArea <- FArea/ZArea * (1-exp(-ZArea)) * BiomassByArea
  
  FArea <- CalcFfromCatchArea(Catch=CatchArea,
                              PopatAge=TotalBiomass,
                              MatAge=M,
                              SelectAtAge=1)
  
  EffortArea <- FArea/q 
  CPUEArea <- CatchArea/EffortArea * RelDensity
  
  ssq <- (CPUEArea[2]-CPUEArea[1])^2
  ssq[2] <- (sum(CatchArea)-TotalCatch)^2
  sum(ssq)
}


logFArea <- log(F * 1/RelativeSize)
dopt <- optim(logFArea, opt)
dopt
logFArea <- dopt$par
exp(logFArea)




