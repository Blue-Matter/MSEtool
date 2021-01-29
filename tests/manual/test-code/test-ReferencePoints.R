library('testthat')
testthat::context("Test Dynamic Unfished Ref Points")
library('MSEtool')
rm(list=ls())


# hOMs <- readRDS('C:/users/adrian/downloads/hOMs.rda')
#
#
# OM <- hOMs[[2]]

OM <- testOM
OM@nsim <- 50

OM@cpars$qs <- rep(0, OM@nsim)

Hist <- Simulate(OM)
MSE <- Project(Hist, MPs='NFref', extended = TRUE)
# Test that reported dynamic SSB0 is the same as historical and projected SB with F=0
testthat::expect_equal(MSE@RefPoint$Dynamic_Unfished$SSB0,
                       cbind(apply(MSE@Hist@TSdata$SBiomass, 1:2, sum), MSE@SSB[,1,]))

# simulate unfished population
Ages <- 0:OM@maxage
N <- SB <- B <- matrix(NA, nrow=OM@maxage+1, ncol=OM@nyears+OM@proyears)


sim <- sample(1:OM@nsim, 1)

R0 <- Hist@SampPars$Stock$R0[sim]
M_array <- Hist@SampPars$Stock$M_ageArray[sim,,]
RecDevs <- Hist@SampPars$Stock$Perr_y[sim,]
Weight <- Hist@SampPars$Stock$Wt_age[sim,,]
Mature <- Hist@SampPars$Stock$Mat_age[sim,,]

N[1,1] <- R0 * RecDevs[OM@maxage+1]
for (a in 1:OM@maxage) {
  dev <- RecDevs[OM@maxage+1-a]
  N[a+1,1] <- R0 * exp(-sum(M_array[1:a,1])) * dev
}
N[OM@maxage+1,1] <- N[OM@maxage+1,1] + N[OM@maxage+1,1] * exp(-M_array[OM@maxage+1,1])/(1-exp(-M_array[OM@maxage+1,1]))

# Test that unfished N is the same
testthat::expect_equal(N[,1],  rowSums(Hist@AtAge$Number[sim,, 1,]))

B[,1] <- N[,1] * Weight[,1]
SB[,1] <- B[,1] * Mature[,1]

# Test that unfished B and SB in first year
testthat::expect_equal(B[,1],  rowSums(Hist@AtAge$Biomass[sim,, 1,]))
testthat::expect_equal(SB[,1],  rowSums(Hist@AtAge$SBiomass[sim,, 1,]))

# Calc recruitment
CalcRec <- function(SRrel, SSBcurr, recdev, h, aR, R0, SSBpR, SSB0) {
  bR <- log(5*h)/(0.8*SSB0)
  if (SRrel == 1) { # BH rec
    rec <- recdev * (4*R0 * h * SSBcurr)/(SSBpR * R0 * (1-h) + (5*h-1) * SSBcurr)
  } else { # Ricker rec
    rec <-  recdev  * aR * SSBcurr * exp(-bR *SSBcurr)
  }
  rec
}

# calc for all years
for (y in 2:(OM@nyears+OM@proyears)) {
  for (a in 1:OM@maxage) {
    N[a+1,y] <- N[a,y-1] * exp(-(M_array[a,y-1]))
  }
  N[OM@maxage+1,y] <- N[OM@maxage+1,y] + N[OM@maxage+1,y] * exp(-M_array[OM@maxage+1,y-1])/(1-exp(-M_array[OM@maxage+1,y-1]))


  # Calc B & SB
  B[,y] <- N[,y] * Weight[,y]
  SB[,y] <- B[,y] * Mature[,y]

  # Calc Recruitment
  SSBcurr <- sum(SB[,y], na.rm=T)
  recdev <- RecDevs[OM@maxage+y]

  N[1,y] <- CalcRec(OM@SRrel, SSBcurr, recdev,
                    h=Hist@SampPars$Stock$hs[sim],
                    Hist@SampPars$Stock$aR[sim,1],
                    R0,
                    Hist@SampPars$Stock$SSBpR[sim,1],
                    Hist@SampPars$Stock$SSB0[sim])
}

# Test that unfished N is the same
runMSE_Ns <- apply(MSE@Misc$extended$N[sim,,1,,], 1:2, sum)
testthat::expect_equal(N,  runMSE_Ns)

DynamicSSB0 <- apply(SB, 2, sum, na.rm=TRUE)
testthat::expect_equal(Hist@Ref$Dynamic_Unfished$SSB0[sim,],  DynamicSSB0)


# par(mfrow=c(2,1), mar=c(4,4,1,1))
# plot(Hist@Ref$Dynamic_Unfished$SSB0[sim,], type="l", ylab="Dynamic SSB0", xlab="Year")
#
# lines(DynamicSSB0, col="blue")
# legend('topright', col=c('black', 'blue'),
#        c('runMSE', 'manualtest'), bty="n", lty=1)
#
# plot(RecDevs[(OM@maxage+1):length(RecDevs)], type="l",
#      ylab="Rec Devs", xlab="Year")





