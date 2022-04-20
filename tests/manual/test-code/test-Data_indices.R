testthat::context("test conditioning on indices in Data object")

# Simulate some data #
library(MSEtool)

OM <- MSEtool::testOM

OM@nsim <- 5
Hist <- runMSE(OM, Hist=TRUE, silent=TRUE)
MPs <- "FMSYref"
# Grab indices frOM one sim
sim <- sample(1:OM@nsim,1)
Bind <- rowSums(Hist@TSdata$Biomass[sim,,])
VBind <- rowSums(Hist@TSdata$VBiomass[sim,,])
SpBInd<- rowSums(Hist@TSdata$SBiomass[sim,,])

NA.ind <- 22:28
Bind[NA.ind] <- NA
VBind[NA.ind] <- NA
SpBInd[NA.ind] <- NA

Data <- new("Data")
Data@Ind <- matrix(Bind, nrow=1, ncol=length(Bind))
Data@CV_Ind <- matrix(0.1, nrow=1, ncol=length(Bind))

Data@SpInd <- matrix(SpBInd, nrow=1, ncol=length(Bind))
Data@CV_SpInd <- matrix(0.1, nrow=1, ncol=length(Bind))

Data@VInd <- matrix(VBind, nrow=1, ncol=length(Bind))
Data@CV_VInd <- matrix(0.1, nrow=1, ncol=length(Bind))

OM@cpars$Data <- Data


Hist <- Simulate(OM, silent=TRUE)


Biomass <- Hist@AtAge$Biomass
SBiomass <- Hist@AtAge$SBiomass
VBiomass <- Hist@AtAge$VBiomass
ObsPars <- Hist@SampPars$Obs

testthat::test_that("works with historical biomass", {
  stInd <- matrix(Data@Ind[1,]/mean(Data@Ind[1,], na.rm=T), nrow=OM@nsim, ncol=OM@nyears, byrow=T)
  II <- (apply(Biomass, c(1, 3), sum, na.rm=T)^ObsPars$I_beta) * ObsPars$Ierr_y[, 1:OM@nyears]
  II <- II/apply(II, 1, mean, na.rm=T)  # normalize
  testthat::expect_equal(stInd,II)
})


testthat::test_that("works with historical spawning biomass", {
  stInd <- matrix(Data@SpInd[1,]/mean(Data@SpInd[1,], na.rm=T), nrow=OM@nsim, ncol=OM@nyears, byrow=T)
  II <- (apply(SBiomass, c(1, 3), sum, na.rm=T)^ObsPars$SpI_beta) * ObsPars$SpIerr_y[, 1:OM@nyears]
  II <- II/apply(II, 1, mean, na.rm=T)  # normalize
  testthat::expect_equal(stInd,II)
})


testthat::test_that("works with historical vulnerable biomass", {
  stInd <- matrix(Data@VInd[1,]/mean(Data@VInd[1,]), nrow=OM@nsim, ncol=OM@nyears, byrow=T)
  II <- (apply(VBiomass, c(1, 3), sum)^ObsPars$VI_beta) * ObsPars$VIerr_y[, 1:OM@nyears]
  II <- II/apply(II, 1, mean)  # normalize
  testthat::expect_equal(stInd,II)
})

# Check in the projection years as well
MSE <- Project(Hist, MPs="NFref", silent=TRUE)
testthat::test_that("Historical observed biomass is correct after projections", {
  # MSE@PPD@Ind is calculated by applying observation error to the simualated biomass
  testthat::expect_equal(matrix(Data@Ind[1,], nrow=OM@nsim, ncol=OM@nyears, byrow=T),MSE@PPD[[1]]@Ind[,1:OM@nyears])
})


# Check additional indices 
Data <- new("Data")
Data@AddInd <- array(rbind(Bind, VBind, SpBInd), dim=c(1, 3, OM@nyears))
Data@CV_AddInd <- array(0.1, dim=c(1, 3, OM@nyears))
Data@AddIndType <- c(1,3,2)
OM@cpars$Data <- Data
Hist <- Simulate(OM, silent=TRUE)
MSE <- Project(Hist, MPs="NFref", silent=TRUE)

testthat::test_that("Historical observed biomass is correct after projections", {
  # MSE@PPD@Ind is calculated by applying observation error to the simualated biomass
  testthat::expect_equal(matrix(Data@AddInd[1,1,], nrow=OM@nsim, ncol=OM@nyears, byrow=T),MSE@PPD[[1]]@AddInd[,1,1:OM@nyears])
})




