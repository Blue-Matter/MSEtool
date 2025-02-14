la = devtools::load_all
la()

# ----- Non-Age Structured -----
M <- 0.2
Effort <- 2000
q <- 0.00025
F <- Effort*q
Z <- M+F
TotalBiomass <- 10112.64
TotalCatch <- F/Z * (1-exp(-Z)) * TotalBiomass

RelativeSize <- c(0.5, 1)
RelativeDist <- c(1, 1)

RelativeSize <- RelativeSize/sum(RelativeSize)
RelativeDist <- RelativeDist/sum(RelativeDist)
BiomassByArea <- TotalBiomass * RelativeDist
RelBiomassByArea <- BiomassByArea/sum(BiomassByArea)
Density <- (BiomassByArea/RelativeSize)
RelDensity <- Density/sum(Density)

# solve spatTarg by fleet
opt <- function(logFleetAreaF) {
  
  FArea <- exp(logFleetAreaF) 
  
  # EffortDist <- RelBiomassByArea^spatTarg
  # FArea <- Effort * EffortDist * q / RelativeSize
  
  ZArea <- FArea + M 
  CatchArea <- FArea/ZArea * (1-exp(-ZArea)) * BiomassByArea
  
  sum(CatchArea)
  TotalCatch
  
  EffortDist <- CatchArea/sum(CatchArea)/RelativeSize

  EffortArea <- EffortDist/sum(EffortDist) * Effort
  CPUEArea <- CatchArea/EffortArea
  
  
  
  
  # utility over stocks
  ssq <- sum((1-CPUEArea/mean(CPUEArea))^2)
  ssq[2] <- (sum(CatchArea)-TotalCatch)^2
  
  sum(ssq)
}


logFleetAreaF <- log(c(0.1,0.1))
dopt <- optim(logFleetAreaF, opt,  method = "BFGS")
c(dopt$value, exp(dopt$par))
logFleetAreaF <- dopt$par

1.363621 2.727242
1.363621 2.727242


spatTarg <- c(-30, 30)
dopt <- optimise(opt, spatTarg)
dopt

spatTarg <- dopt$minimum


# ----- Age Structured Two Area -----

Ages <- 0:10
nAge <- length(Ages)
nArea <- 2

M <- rep(0.2, nAge)

Select <- c(rep(0,3), 0.25, 0.5,0.75, rep(1,5))
Select <- rep(1, nAge)             

Effort <- 2000

q <- 0.00025
F <- Effort*q*Select
Z <- M+F

N <- 1000*c(1,1*exp(-cumsum(M)))
N <- N[1:nAge]
L <- 100 * (1-exp(-0.25 * Ages))
W <- 1E-5*L^3
TotalBiomass <- N * W
TotalCatch <- F/Z * (1-exp(-Z)) * TotalBiomass

RelativeSize <- c(5, 1)
RelativeDist <- c(1, 1)

RelativeSize <- RelativeSize/sum(RelativeSize)
RelativeDist <- RelativeDist/sum(RelativeDist)
BiomassByArea <- TotalBiomass * matrix(RelativeDist, nAge, nArea, byrow=T)
RelBiomassByArea <- BiomassByArea/sum(BiomassByArea)
Density <- BiomassByArea/matrix(RelativeDist, nAge, nArea, byrow=T)
RelDensity <- Density/sum(Density)

# solve spatTarg by fleet
opt <- function(logFleetAreaF) {
  
  FArea <- exp(logFleetAreaF)
  
 # FArea <- c(0.0006116309, 1.4883426203)
  
  FArea2 <- t(replicate(nAge,FArea)) * replicate(nArea, Select)
  
  ZArea <- FArea2 + M 
  CatchArea <- FArea2/ZArea * (1-exp(-ZArea)) * BiomassByArea
 
  sum(CatchArea)
  sum(TotalCatch)

  Fs <- colSums(CatchArea)/sum(CatchArea)
  
  EffortArea <-  Fs
  EffortArea <- EffortArea/sum(EffortArea) * Effort
  
  # utility per unit effort over areas
  # EffortArea <- FAreaTotal/sum(FAreaTotal) * Effort
  
  CPUEArea <- colSums(CatchArea)/EffortArea
  CPUEArea
  
  ssq <- sum((CPUEArea/mean(CPUEArea) -1)^2)
  ssq[2] <- sum((rowSums(CatchArea) - TotalCatch)^2)
  
  sum(ssq)
  
  # sum((rowSums(CatchArea) - TotalCatch)^2)
  
}

overallF <- Effort*q / nArea
logFleetAreaF <- log(rep(overallF, nArea))
dopt <- optim(logFleetAreaF, opt,  method = "BFGS")
c(dopt$value, exp(dopt$par))

logFleetAreaF <- dopt$par









dopt <- optimise(opt, c(-20, 20))
dopt

spatTarg <- dopt$minimum



RelativeSize <- c(1, 1)
RelativeDist <- c(0.5, 1)

RelDensity # 0.3333333 0.6666667
$minimum
[1] 1.150803

$objective
[1] 11.79168




RelativeSize <- c(2, 1)
RelativeDist <- c(1, 1)
RelDensity # 0.3333333 0.6666667

$minimum
[1] 1.215494

$objective
[1] 236.4984


