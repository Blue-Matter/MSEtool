la = devtools::load_all
la()


Ages <- 0:10
nAge <- length(Ages)
nArea <- 2

M <- rep(0.2, nAge)

Select <- c(rep(0,3), 0.25, 0.5,0.75, rep(1,5))

Effort <- 2000

q <- 0.00025
# F <- Effort*q*Select
# Z <- M+F

N <- 1000*c(1,1*exp(-cumsum(M)))
N <- N[1:nAge]
L <- 100 * (1-exp(-0.25 * Ages))
W <- 1E-5*L^3
TotalBiomass <- N * W
# TotalCatch <- F/Z * (1-exp(-Z)) * TotalBiomass

RelativeSize <- c(1, 1)
RelativeDist <- c(2, 1)

RelativeSize <- RelativeSize/sum(RelativeSize)
RelativeDist <- RelativeDist/sum(RelativeDist)
BiomassByArea <- TotalBiomass * matrix(RelativeDist, nAge, nArea, byrow=T)
VBiomassByArea <- TotalBiomass * Select * matrix(RelativeDist, nAge, nArea, byrow=T)

Density <- VBiomassByArea/matrix(RelativeSize, nAge, nArea, byrow=T)
RelDensity <- colSums(Density)/sum(Density)



EffortDist <- RelDensity#^spatTarg
FArea <- Effort * EffortDist * q # RelativeSize
FArea2 <- t(replicate(nAge,FArea)) * replicate(nArea, Select)

ZArea <- FArea2 + M 
CatchArea <- FArea2/ZArea * (1-exp(-ZArea)) * BiomassByArea

sum(CatchArea)


Fs <- colSums(CatchArea)/sum(CatchArea)

EffortArea <-  Fs*RelDensity
EffortArea <- EffortArea/sum(EffortArea) * Effort

CPUEArea <- colSums(CatchArea)/EffortArea
CPUEArea

TotalCatch <- rowSums(CatchArea)

### back calculate F 



opt2 <- function(logFArea) {
  
  FArea <- exp(logFArea)*RelDensity
  # EffortDist <- RelDensity^spatTarg
  # FArea <- Effort * EffortDist * q / RelativeSize
  FArea2 <- t(replicate(nAge,FArea)) * replicate(nArea, Select)
  
  ZArea <- FArea2 + M 
  CatchArea <- FArea2/ZArea * (1-exp(-ZArea)) * BiomassByArea
  
  sum(CatchArea)
  sum(TotalCatch)
  
  Fs <- colSums(CatchArea)/sum(CatchArea)
  
  EffortArea <-  Fs/q/RelativeSize
  
  EffortArea/sum(EffortArea)  *Effort
  
  CPUEArea <- colSums(CatchArea)/EffortArea
  CPUEArea
  
  ssq <- sum((CPUEArea/mean(CPUEArea) -1)^2)
  ssq[2] <- sum((rowSums(CatchArea) - TotalCatch)^2)
  
  # plot(rowSums(CatchArea))
  # lines(TotalCatch)
  
  sum(ssq[2])
  
}

logFArea <- rep(Effort*q, 2) |> log()
dopt <- optim(logFArea, opt2, method='BFGS')
dopt

logFArea <- dopt$par









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

RelativeSize <- c(5, 1)
RelativeDist <- c(1, 1)

RelativeSize <- RelativeSize/sum(RelativeSize)
RelativeDist <- RelativeDist/sum(RelativeDist)
BiomassByArea <- TotalBiomass * RelativeDist
RelBiomassByArea <- BiomassByArea/sum(BiomassByArea)
Density <- (BiomassByArea/RelativeSize)
RelDensity <- Density/sum(Density)


# solve spatTarg by fleet
opt <- function(spatTarg, opt=1) {
  
  # FArea <- exp(logFleetAreaF) 
  EffortDist <- RelDensity*spatTarg
  FArea <- Effort * EffortDist * q / RelativeSize
  
  ZArea <- FArea + M 
  CatchArea <- FArea/ZArea * (1-exp(-ZArea)) * BiomassByArea
  
  sum(CatchArea)
  TotalCatch
  
  EffortArea <- EffortDist/sum(EffortDist) * Effort
  CPUEArea <- CatchArea/EffortArea
  
  # utility over stocks
  ssq <- sum((1-CPUEArea/mean(CPUEArea))^2)
  ssq[2] <- (sum(CatchArea)-TotalCatch)^2
  
  if (opt==1)
    return(sum(ssq))
  sum(ssq[2])
}

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
VBiomassByArea <- TotalBiomass * Select * matrix(RelativeDist, nAge, nArea, byrow=T)

Density <- VBiomassByArea/matrix(RelativeSize, nAge, nArea, byrow=T)
RelDensity <- colSums(Density)/sum(Density)


# solve spatTarg by fleet

# If you have existing effort, solve for targetting

# opt <- function(spatTarg) {
  
  EffortDist <- RelDensity#^spatTarg
  FArea <- Effort * EffortDist * q / RelativeSize
  FArea2 <- t(replicate(nAge,FArea)) * replicate(nArea, Select)
  
  ZArea <- FArea2 + M 
  CatchArea <- FArea2/ZArea * (1-exp(-ZArea)) * BiomassByArea
 
  sum(CatchArea)
  sum(TotalCatch)

  Fs <- colSums(CatchArea)/sum(CatchArea)
  
  EffortArea <-  Fs*RelDensity
  EffortArea <- EffortArea/sum(EffortArea) * Effort
  
  CPUEArea <- colSums(CatchArea)/EffortArea
  CPUEArea
  
#   ssq <- sum((CPUEArea/mean(CPUEArea) -1)^2)
#   ssq[2] <- sum((rowSums(CatchArea) - TotalCatch)^2)
#   
#   # plot(rowSums(CatchArea))
#   # lines(TotalCatch)
#   
#   sum(ssq[2])
#   
# }
  
  TotalCatch <- rowSums(CatchArea)

dopt <- optimise(opt, c(-20, 20))
dopt

spatTarg <- dopt$minimum

# If you have total catch, solve for F

# Solve for F 
opt2 <- function(logFArea) {
  
  FArea <- exp(logFArea)
  # EffortDist <- RelDensity^spatTarg
  FArea <- Effort * EffortDist * q / RelativeSize
  FArea2 <- t(replicate(nAge,FArea)) * replicate(nArea, Select)
  
  ZArea <- FArea2 + M 
  CatchArea <- FArea2/ZArea * (1-exp(-ZArea)) * BiomassByArea
  
  sum(CatchArea)
  sum(TotalCatch)
  
  Fs <- colSums(CatchArea)/sum(CatchArea)
  
  EffortArea <-  Fs/q*RelDensity
  
  CPUEArea <- colSums(CatchArea)/EffortArea
  CPUEArea
  
  ssq <- sum((CPUEArea/mean(CPUEArea) -1)^2)
  ssq[2] <- sum((rowSums(CatchArea) - TotalCatch)^2)
  
  # plot(rowSums(CatchArea))
  # lines(TotalCatch)
  
  sum(ssq[2])
  
}

logFArea <- rep(Effort*q, 2) |> log()
dopt <- optim(logFArea, opt2, method='BFGS')
dopt

logFArea <- dopt$par
