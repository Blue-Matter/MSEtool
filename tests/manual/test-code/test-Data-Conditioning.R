
testthat::context("Test Data conditioning")
library('MSEtool')
rm(list=ls())

OM <- testOM
OM@nsim <- 5
Hist <- Simulate(OM)

sim <- sample(1:OM@nsim, 1)

Data <- new("Data")
Data@Cat<- matrix(rowSums(Hist@TSdata$Landings[sim,,]),nrow=1)
Data@CV_Cat <- array(0.1, dim=dim(Data@Cat))

OM@cpars$Data <- Data

Hist <- Simulate(OM)

MSE <- Project(Hist, MPs="FMSYref50", extended = T)

Catch <- apply(MSE@Misc$extended$Catch[sim,,1,,], 2, sum)

testthat::expect_equal(MSE@Catch[sim,1,], Catch[(OM@nyears+1):(OM@nyears+OM@proyears)])
testthat::expect_equal(Catch[1:(OM@nyears+OM@proyears-1)],MSE@PPD[[1]]@Cat[sim,])

# Add bias
val <- 0.2
bias <- rlnorm(1, MSEtool:::mconv(1, val), MSEtool:::sdconv(1, val))

Catch <- rowSums(Hist@TSdata$Landings[sim,,]) * bias

Data <- new("Data")
Data@Cat<- matrix(Catch,nrow=1)
Data@CV_Cat <- array(0.1, dim=dim(Data@Cat))

OM@cpars$Data <- Data
Hist <- Simulate(OM)

testthat::expect_equal(Hist@SampPars$Obs$Cbias[sim],bias)

MSE <- Project(Hist, MPs="FMSYref50", extended = T)

Catch <- apply(MSE@Misc$extended$Catch[sim,,1,,], 2, sum)

testthat::expect_equal(mean(MSE@PPD[[1]]@Cat[sim,]/Catch[1:(OM@nyears+OM@proyears-1)]), bias)

# Add error
Csd <- 0.3
Cerr_y <- array(rlnorm(OM@nyears + OM@proyears,
                       MSEtool:::mconv(1, rep(Csd, OM@nyears + OM@proyears)),
                       MSEtool:::sdconv(1, rep(Csd, OM@nyears + OM@proyears))),
                c(OM@nyears + OM@proyears))


Catch <- rowSums(Hist@TSdata$Landings[sim,,]) * Cerr_y[1:OM@nyears]

Data <- new("Data")
Data@Cat<- matrix(Catch,nrow=1)
Data@CV_Cat <- array(0.1, dim=dim(Data@Cat))

OM@cpars$Data <- Data
Hist <- Simulate(OM)

testthat::expect_equal(round(sd(Cerr_y[1:OM@nyears]),2),
                       round(sd(Hist@SampPars$Obs$Cerr_y[sim,]),2))


# Add error and bias
val <- 0.2
bias <- rlnorm(1, MSEtool:::mconv(1, val), MSEtool:::sdconv(1, val))
Csd <- 0.3
Cerr_y <- rlnorm(OM@nyears + OM@proyears,
                       MSEtool:::mconv(1, rep(Csd, OM@nyears + OM@proyears)),
                       MSEtool:::sdconv(1, rep(Csd, OM@nyears + OM@proyears)))

Catch <- rowSums(Hist@TSdata$Landings[sim,,]) * bias * Cerr_y[1:OM@nyears]

Data <- new("Data")
Data@Cat<- matrix(Catch,nrow=1)
Data@CV_Cat <- array(0.1, dim=dim(Data@Cat))

OM@cpars$Data <- Data
Hist <- Simulate(OM)

testthat::expect_equal(round(Hist@SampPars$Obs$Cbias[sim],1), round(bias,1))

testthat::expect_equal(Hist@SampPars$Obs$Cobs_y[sim,1:OM@nyears], bias * Cerr_y[1:OM@nyears])



# Test Index
Biomass <- rowSums(Hist@TSdata$Biomass[sim,,])

Data <- new("Data")
Data@Ind <- matrix(Biomass,nrow=1)
Data@CV_Ind <- array(0.1, dim=dim(Data@Ind))

OM@cpars$Data <- Data

Hist <- Simulate(OM)

testthat::expect_equal(round(mean(Hist@SampPars$Obs$Ierr_y[sim,]),2), 1)

MSE <- Project(Hist, MPs="FMSYref50", extended = T)

ind <- 1:OM@nyears
B <- apply(MSE@Misc$extended$B[sim,,1,,], 2, sum)

Ind <- MSE@PPD[[1]]@Ind[sim,]

testthat::expect_equal(round(B[1:(OM@nyears+OM@proyears-1)]/Ind,2), rep(1, OM@nyears+OM@proyears-1))


# Add bias
beta <- runif(1, 0.5, 2)
Biomass <- rowSums(Hist@TSdata$Biomass[sim,,])^beta

Data <- new("Data")
Data@Ind <- matrix(Biomass,nrow=1)
Data@CV_Ind <- array(0.1, dim=dim(Data@Ind))

OM@cpars$Data <- Data

Hist <- Simulate(OM)

testthat::expect_equal(round(Hist@SampPars$Obs$I_beta[sim], 2), round(beta,2))
testthat::expect_equal(round(mean(Hist@SampPars$Obs$Ierr_y[sim,]),2), 1)

MSE <- Project(Hist, MPs="FMSYref50", extended = T)

ind <- 1:OM@nyears
B <- apply(MSE@Misc$extended$B[sim,,1,,], 2, sum)

Ind <- MSE@PPD[[1]]@Ind[sim,]

fit <- MSEtool:::indfit(B[1:(OM@nyears+OM@proyears-1)],Ind)

testthat::expect_equal(round(fit$beta,2), round(beta,2))
testthat::expect_equal(round(fit$sd,2),0)


# Test VInd
Data <- new("Data")
Data@VInd <- matrix(rowSums(Hist@TSdata$VBiomass[sim,,]),nrow=1)
Data@CV_VInd <- array(0.1, dim=dim(Data@VInd))

OM@cpars$Data <- Data

Hist <- Simulate(OM)

MSE <- Project(Hist, MPs="FMSYref50", extended = T)

ind <- 1:OM@nyears
VB <- apply(MSE@Misc$extended$VB[sim,,1,,], 2, sum)

Ind <- MSE@PPD[[1]]@VInd[sim,]
testthat::expect_equal(round(VB[1:(OM@nyears+OM@proyears-1)]/Ind,2), rep(1, OM@nyears+OM@proyears-1))


# Test SpInd
Data <- new("Data")
Data@SpInd <- matrix(rowSums(Hist@TSdata$SBiomass[sim,,]),nrow=1)
Data@CV_SpInd <- array(0.1, dim=dim(Data@SpInd))

OM@cpars$Data <- Data

Hist <- Simulate(OM)

MSE <- Project(Hist, MPs="FMSYref50", extended = T)

ind <- 1:OM@nyears
SB <- apply(MSE@Misc$extended$SSB[sim,,1,,], 2, sum)

Ind <- MSE@PPD[[1]]@SpInd[sim,]
testthat::expect_equal(round(SB[1:(OM@nyears+OM@proyears-1)]/Ind,2), rep(1, OM@nyears+OM@proyears-1))


# Test Additional Index
OM <- testOM
OM@nsim <- 5
Hist <- Simulate(OM)

sim <- sample(1:OM@nsim, 1)

Biomass <- apply(Hist@TSdata$Biomass, 1:2, sum)

Data <- new("Data")
Data@AddInd <- array(Biomass,dim=c(1,OM@nsim, ncol(Biomass)))
Data@CV_AddInd <- array(0.1,dim=c(1,OM@nsim, ncol(Biomass)))

OM@cpars$Data <- Data

Hist <- Simulate(OM)

Hist@SampPars$Obs$AddIbeta
Hist@SampPars$Obs$AddIerr[1,1,]
Hist@SampPars$Obs$AddIerr[2,2,]

MSE <- Project(Hist, MPs="FMSYref50", extended = T)

Bio <- apply(MSE@Misc$extended$B, c(1,4), sum)


plot(Bio[sim,])
lines(MSE@PPD[[1]]@AddInd[sim,sim,])

testthat::expect_equal(Bio[sim,1:(OM@nyears+OM@proyears-1)], MSE@PPD[[1]]@AddInd[sim,sim,])


