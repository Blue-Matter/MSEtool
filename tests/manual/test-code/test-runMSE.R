
testthat::context("Test of basic population dynamics")

library(MSEtool)

OM <- MSEtool::testOM
OM@qinc <- OM@qcv <- c(0,0)
OM@interval <- 1

Hist <- Simulate(OM)

testthat::expect_s4_class(Hist, 'Hist')


MSE <- Project(Hist, MPs=c('FMSYref', 'curEref'))

testthat::expect_s4_class(MSE, 'MSE')

histF <- array(MSE@FM_hist[,OM@nyears], dim=dim(MSE@FM[,2,]))

# curEref should result in projection Fs = last historical F
testthat::expect_equal(round(MSE@FM[,2,],2), round(histF,2))

# FMSYref should result in F/FMSY = 1
testthat::expect_equal(round(MSE@F_FMSY[,1,],2), round(array(1, dim=dim(MSE@F_FMSY[,1,])),2))


