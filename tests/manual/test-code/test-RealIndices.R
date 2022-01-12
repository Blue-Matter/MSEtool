
testthat::context("test Real indices in Data object")

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


testthat::test_that("Works with 3 indices", {
  Data <- new("Data")
  Data@AddInd <- array(rbind(Bind, VBind, SpBInd), dim=c(1, 3, OM@nyears))
  Data@CV_AddInd <- array(0.1, dim=c(1, 3, OM@nyears))
  Data@AddIndType <- c(1,3,2)
  OM@cpars$Data <- Data
  MSE <- runMSE(OM, MPs=MPs, silent = TRUE)
  testthat::expect_is(MSE, "MSE")
})

testthat::test_that("Works with 2 indices", {
  Data <- new("Data")
  Data@AddInd <- array(rbind(Bind, SpBInd), dim=c(1, 2, OM@nyears))
  Data@CV_AddInd <- array(0.1, dim=c(1, 2, OM@nyears))
  Data@AddIndType <- c(1,2)
  OM@cpars$Data <- Data
  MSE <- runMSE(OM, MPs=MPs, silent = TRUE)
  testthat::expect_is(MSE, "MSE")
})


testthat::test_that("Works with 1 index", {
  Data <- new("Data")
  Data@AddInd <- array(Bind, dim=c(1, 1, OM@nyears))
  Data@CV_AddInd <- array(0.1, dim=c(1, 1, OM@nyears))
  Data@AddIndType <- 1
  MSE <- runMSE(OM, MPs=MPs, silent = TRUE)
  testthat::expect_is(MSE, "MSE")
})


testthat::test_that("Import 3 real indices from Excel", {
  Data <- Hist@Data
  Data@AddInd <- array(rbind(Bind, VBind, SpBInd), dim=c(1, 3, OM@nyears))
  Data@CV_AddInd <- array(0.1, dim=c(1, 3, OM@nyears))
  Data@AddIndType <- c(1,3,2)
  Data@LHYear <- 50

  Data2csv(Data, file='test.csv', overwrite = TRUE)

  DataIn <- new("Data", "test.csv")

  testthat::expect_equal(DataIn@AddIndType, Data@AddIndType)
  testthat::expect_equal(DataIn@AddInd[1,,], Data@AddInd[1,,])
})

unlink('test.csv')



testthat::test_that("Works with 2 Real indices longer than nyears", {
  om <- OM
  om@nyears <- 30

  Data <- new("Data")
  Data@AddInd <- array(rbind(Bind, SpBInd), dim=c(1, 2, OM@nyears))
  Data@CV_AddInd <- array(0.1, dim=c(1, 2, OM@nyears))
  Data@AddIndType <- c(1,2)
  om@cpars$Data <- Data

  MSE <- runMSE(om, MPs=MPs, silent = TRUE)
  Dataout <- MSE@PPD[[1]]
  testthat::expect_equal(Dataout@AddInd[1,1,1:50], om@cpars$Data@AddInd[1,1,])
  testthat::expect_equal(Dataout@AddInd[1,2,1:50], om@cpars$Data@AddInd[1,2,])
})


testthat::test_that("Works with 2 Real indices shorter than nyears", {
  om <- OM
  om@nyears <- 60
  Data <- new("Data")
  Data@AddInd <- array(rbind(Bind, SpBInd), dim=c(1, 2, OM@nyears))
  Data@CV_AddInd <- array(0.1, dim=c(1, 2, OM@nyears))
  Data@AddIndType <- c(1,2)
  om@cpars$Data <- Data

  MSE <- runMSE(om, MPs=MPs, silent = TRUE)

  Dataout <- MSE@PPD[[1]]
  testthat::expect_equal(Dataout@AddInd[1,1,1:50], om@cpars$Data@AddInd[1,1,])
  testthat::expect_equal(Dataout@AddInd[1,2,1:50], om@cpars$Data@AddInd[1,2,])

})




