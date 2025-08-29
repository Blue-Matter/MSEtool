testthat::context("Test Custom Obs Parameters for Real Data Conditioning")

library('MSEtool')
rm(list=ls())

OM <- testOM
OM@nsim <- 5


# ---- Catch Data ----
baseOM <- testOM
baseOM@nsim <- 5

OM <- baseOM
sim <- sample(1:OM@nsim, 1)

Hist <- Simulate(OM)

Data <- new("Data")
Data@Cat<- matrix(rowSums(Hist@TSdata$Landings[sim,,]),nrow=1)
Data@CV_Cat <- array(0.1, dim=dim(Data@Cat))

OM@cpars$Data <- Data

# Total catch observation error
OM@cpars$Cobs_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)

Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$Cobs_y==1))

# Catch bias
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$Cbias <- rep(1,OM@nsim)

Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$Cbias==1))

# Catch variability
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$Cerr_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)

Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$Cerr_y==1))

# ---- Indices ----
Biomass <- rowSums(Hist@TSdata$Biomass[sim,,])

# Total index
Data <- new("Data")
Data@Ind <- matrix(Biomass,nrow=1)
Data@CV_Ind <- array(0.1, dim=dim(Data@Ind))

# I_beta
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$I_beta <- rep(1, OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$I_beta==1))

# Ierr_y
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$Ierr_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$Ierr_y==1))

# Both
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$Ierr_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)
OM@cpars$I_beta <- rep(1, OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$Ierr_y==1))
testthat::expect_true(all(Hist@SampPars$Obs$I_beta==1))

# --- Spawning index ---
Data <- new("Data")
Data@SpInd <- matrix(Biomass,nrow=1)
Data@CV_SpInd <- array(0.1, dim=dim(Data@SpInd))

# I_beta
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$SpI_beta <- rep(1, OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$SpI_beta==1))

# Ierr_y
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$SpIerr_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$SpIerr_y==1))

# Both
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$SpIerr_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)
OM@cpars$SpI_beta <- rep(1, OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$SpIerr_y==1))
testthat::expect_true(all(Hist@SampPars$Obs$SpI_beta==1))


# --- Vulnerable index ---
Data <- new("Data")
Data@VInd <- matrix(Biomass,nrow=1)
Data@CV_VInd <- array(0.1, dim=dim(Data@VInd))

# I_beta
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$VI_beta <- rep(1, OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$VI_beta==1))

# Ierr_y
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$VIerr_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$VIerr_y==1))

# Both
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$VIerr_y <- matrix(1, nrow=OM@nsim, ncol=OM@nyears+OM@proyears)
OM@cpars$VI_beta <- rep(1, OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$VIerr_y==1))
testthat::expect_true(all(Hist@SampPars$Obs$VI_beta==1))


# ---- Additional indices ----
Biomass <- apply(Hist@TSdata$Biomass, 1:2, sum)

Data <- new("Data")
Data@AddInd <- array(Biomass,dim=c(1,OM@nsim, ncol(Biomass)))
Data@CV_AddInd <- array(0.1,dim=c(1,OM@nsim, ncol(Biomass)))

# AddIbeta
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$AddIbeta <- matrix(1, nrow=OM@nsim, ncol=OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$AddIbeta==1))


# AddIerr
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$AddIerr <- array(1, dim=c(OM@nsim, OM@nsim, OM@nyears+OM@proyears))
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$AddIerr==1))


# Both
OM <- baseOM
OM@cpars$Data <- Data
OM@cpars$AddIerr <- array(1, dim=c(OM@nsim, OM@nsim, OM@nyears+OM@proyears))
OM@cpars$AddIbeta <- matrix(1, nrow=OM@nsim, ncol=OM@nsim)
Hist <- Simulate(OM)
testthat::expect_true(all(Hist@SampPars$Obs$AddIbeta==1))
testthat::expect_true(all(Hist@SampPars$Obs$AddIerr==1))

