
# TODO - not complete 


testthat::context("Quick test of pop dyn")

library(MSEtool)
# 
# Fmort <- runif(1, 0.05, 0.2)
# OM <- tinyErr(testOM, silent = TRUE)
# OM@SRrel <- 1
# OM@Size_area_1 <- OM@Frac_area_1 <- OM@Prob_staying <- c(0.5,0.5)
# 
# 
# # Simple test of the population dynamics model
# OM <- tinyErr(testOM, silent = TRUE)
# OM@Vmaxlen <- c(1,1)
# 
# OM@CAL_ESS <- OM@CAL_nsamp <- rep(5000,2)
# 
# OM@isRel <- FALSE
# OM@Linf <- rep(100, 2)
# OM@L50 <- rep(55, 2)
# OM@L5  <- c(25, 25)
# OM@LFS <- c(40, 40)
# OM@Vmaxlen <- c(1,1)
# OM@h <- rep(0.999999, 2)
# 
# OM@cpars$qs <- rep(1, OM@nsim)
# OM@cpars$Find <- matrix(Fmort, nrow=OM@nsim, ncol=OM@nyears)
# 
# 
# 
# # Compare with a simple population dynamics model
# 
# simpop <- function(sim=1,logapicF, OM) {
#   
#   set.seed(OM@seed)
#   # Stock Parameters 
#   StockPars <- SampleStockPars(OM, OM@nsim, OM@nyears, OM@proyears, OM@cpars, msg=FALSE)
#   
#   # Fleet Parameters 
#   FleetPars <- SampleFleetPars(SubOM(OM, "Fleet"), Stock=StockPars, OM@nsim,
#                                OM@nyears, OM@proyears, cpars=OM@cpars)
# 
#   Len <- StockPars$Len_age[sim,,1]
#   Wght <- StockPars$a * Len^StockPars$b
#   a50 <- -((log(1 - StockPars$L50[sim]/StockPars$Linf[sim]))/StockPars$K[sim]) + StockPars$t0[sim]
#   a95 <- -((log(1 - StockPars$L95[sim]/StockPars$Linf[sim]))/StockPars$K[sim]) + StockPars$t0[sim]
#   ages <- 0:OM@maxage
#   n_age <- length(ages)
#   maa <- 1/(1 + exp(-log(19) * ((ages - a50)/(a95-a50)))) # maturity-at-age
#   maa[1] <- 0 # age-0 aren't mature
#   
#   Hist@SampPars$Stock$Mat_age[sim,,1] ==maa
#   
#   # selectivity
#   sl <- (FleetPars$LFS_y[sim,1] - FleetPars$L5_y[sim,1]) /((-log(0.05,2))^0.5)
#   sr <- (StockPars$Linf[sim] - FleetPars$LFS_y[sim,1]) / ((-log(FleetPars$Vmaxlen_y[sim,1],2))^0.5) # selectivity parameters are constant for all years
#   SL <- getsel(lens=StockPars$CAL_binsmid, lfs=FleetPars$LFS_y[sim,1], sls=sl, srs=sr)
#   SL_age <- calcVatAge(len_at_age=matrix(StockPars$Len_age[sim,,1], length(StockPars$Len_age[sim,,1]), 1),
#                        len_aa_sd=matrix(StockPars$LatASD[sim,,1], length(StockPars$Len_age[sim,,1]), 1),
#                        sel_at_length=matrix(SL, nrow=length(SL), ncol=1), 
#                        n_age=n_age, 
#                        nyears=1, 
#                        proyears=0, 
#                        CAL_binsmid=StockPars$CAL_binsmid)
# 
#   M_array <- rep(StockPars$M[sim],n_age)
#   FAA <- exp(logapicF) * SL_age
# 
#   ages <- 0:OM@maxage
#   N <- VB <- matrix(NA, OM@nyears, n_age)
#   Rec <- SB <- rep(NA, OM@nyears)
#   N[1,1] <- StockPars$R0[sim]
#   surv <- rep(1, n_age)
#   surv[2:n_age] <- exp(-cumsum(M_array[1:OM@maxage]))
#   surv[n_age] <- surv[n_age]/(1-exp(-StockPars$M_ageArray[sim,n_age,1]))
#   N[1,] <- StockPars$R0[sim] * surv
# 
#   ZAA <- FAA + M_array
#   
#   survF <- rep(1, n_age)
#   survF[2:n_age] <- exp(-cumsum(ZAA[1:OM@maxage]))
#   survF[n_age] <- survF[n_age]/(1-exp(-ZAA[n_age]))
#   Nf <-  StockPars$R0[sim] * survF
#   
#   eggs0 <- sum(N[1,] * maa * Wght)
#   eggsF <- sum(Nf * maa * Wght)
#   SPR <- eggsF/eggs0
#   
#   
#   BHSRR <- function(SBcurr, SB0, R0, steepness) {
#     (4 * R0 * steepness * SBcurr)/(SB0/R0 * R0 * (1-steepness) + (5*steepness-1)*SBcurr)
#   }
#   
#   SB[1] <- sum(N[1,] * maa * Wght)
#   SB0 <- sum(SB[1])
#   Rec[1] <- StockPars$R0[sim]
#   for (yr in 2:OM@nyears) {
#     N[yr,2:n_age] <- N[yr-1, 1:OM@maxage] * exp(-ZAA[1:OM@maxage])
#     N[yr,n_age] <- N[yr,n_age]+ N[yr-1,n_age] *exp(-ZAA[n_age])
#     SB[yr] <- sum(N[yr,]* maa * Wght, na.rm=TRUE)
#     Rec[yr] <- BHSRR(SB[yr], SB0, StockPars$R0[sim], steepness=StockPars$hs[sim])
#     N[yr,1] <- Rec[yr]
#   }
#   dep <- SB[yr]/SB0
#   
#   out <- list()
#   out$N <- N
#   out$SPR <- SPR
#   out$SB <- SB
#   out$dep <- dep
#   out
# }
# 
# Hist <- Simulate(OM, silent=TRUE)
# Mod <- lapply(1:OM@nsim, simpop, logapicF=log(Fmort), OM=OM)
# 
# # Compare N-at-Age
# SimNList <- lapply(Mod, '[[', 1)
# SimN <- array(NA, dim=c(OM@nsim, OM@maxage+1, OM@nyears))
# for (i in 1:OM@nsim)
#   SimN[i,,] <- t(SimNList[[i]])
# 
# 
# OMN <- apply(Hist@AtAge$Number, 1:3, sum)
# 
# testthat::expect_equal(SimN,OMN)
# 
# SimN[2,,3]
# OMN[2,,3]
# 
# 
# 
# # Compare Depletion 
# 
# # Compare N 
# 
# # Compare SB 
# 
# 
# 
# 
# 
# 
# 
# chk <- rep(NA, OM@nsim)
# for (sim in 1:OM@nsim) {
#   opt <- optimize(simpop, interval=log(c(0.01, 0.9)), OM=OM, FleetPars=FleetPars,
#                   StockPars=StockPars, sim=sim, opt=1)
#   simple <- simpop(opt$minimum, OM=OM, FleetPars=FleetPars,
#                    StockPars=StockPars, sim=sim, opt=2)
#   
#   chk[sim] <- prod(round(apply(Hist@AtAge$Number[sim,,,], 1:2, sum)/t(simple$N) ,0))
# }
# chk
# }
# 
# testthat::expect_equal(testpopdyn(OM), rep(1, OM@nsim))
# 
# 
# 
# # Test N-at-age is identical
# 
# 
# Hist@SampPars$Stock$D[sim]
# dep
# 
# # Test equilibrium SPR is identical
# Hist@TSdata$SPR$Equilibrium[sim,]
# SPR
# 
# # Compare size comps 
# 
# sim <- 2
# mod <- LBSPR2_(sim, Hist@Data)
# 
# plot(Hist@Data@CAL_mids, Hist@Data@CAL[sim,OM@nyears,])
# lines(Hist@Data@CAL_mids, mod$Fit[[5]], col='blue')
# mod$Ests
# 
# Hist@TSdata$SPR$Equilibrium[sim,]
# Fmort/Hist@SampPars$Stock$M[sim]
# 
# 
# 
# 
# 
# 
# 
# 
# 



testpopdyn <- function(OM) {

  
  

  dnormal<-function(lens,lfs,sl,sr){
    cond<-lens<=lfs
    sel<-rep(NA,length(lens))
    sel[cond]<-2.0^-((lens[cond]-lfs)/sl*(lens[cond]-lfs)/sl)
    sel[!cond]<-2.0^-((lens[!cond]-lfs)/sr*(lens[!cond]-lfs)/sr)
    sel
  }
  


  simpop <- function(logapicF, OM, FleetPars, StockPars, sim=1, opt=1) {
    for (X in 1:length(StockPars)) assign(names(StockPars)[X], StockPars[[X]])
    for (X in 1:length(FleetPars)) assign(names(FleetPars)[X], FleetPars[[X]])

    Len <- Len_age[sim,,1]
    Wght <- a * Len^b
    MAA <- 1/(1 + exp(-log(19) * ((Len - L50[sim])/(L95[sim]-L50[sim]))))

    # selectivity-at-length - fishery
    sl <- (LFS_y[1,sim] - L5_y[1,sim]) /((-log(0.05,2))^0.5)
    sr <- (Linf[sim] - LFS_y[1,sim]) / ((-log(Vmaxlen_y[1,sim],2))^0.5) # selectivity parameters are constant for all years
    SAA <- dnormal(Len, LFS_y[1,sim], sl, sr)

    M_array <- rep(M[sim],maxage+1)
    FAA <- exp(logapicF) * SAA
    ZAA <- (FAA * Find[sim,1]) + M_array
    ages <- 1:maxage
    N <- VB <- matrix(NA, OM@nyears, maxage+1)
    Rec <- SB <- rep(NA, OM@nyears)
    N[1,1] <- R0[sim]
    N[1,2:(maxage+1)] <- R0[sim] * exp(-cumsum(M_array[2:(maxage+1)]))

    SB[1] <- sum(N[1,] * MAA * Wght)
    SB0 <- sum(SB[1])
    Rec[1] <- R0[sim]
    for (yr in 2:OM@nyears) {
      Rec[yr] <- BHSRR(SB[yr-1], SB0, R0[sim], steepness=hs[sim])
      N[yr,1] <- Rec[yr]
      ZAA <- (FAA * Find[sim,yr-1]) + M_array
      N[yr,2:(maxage+1)] <- N[yr-1, 1:(maxage)] * exp(-ZAA[1:(maxage)])
      SB[yr] <- sum(N[yr,]* MAA * Wght)
    }
    dep <- SB[yr]/SB0

    if (opt==1) return((dep-StockPars$D[sim])^2)
    return(list(N=N, SAA=SAA, LenCV=LenCV, Len=Len))
  }
  chk <- rep(NA, OM@nsim)
  for (sim in 1:OM@nsim) {
    opt <- optimize(simpop, interval=log(c(0.01, 0.9)), OM=OM, FleetPars=FleetPars,
                    StockPars=StockPars, sim=sim, opt=1)
    simple <- simpop(opt$minimum, OM=OM, FleetPars=FleetPars,
                     StockPars=StockPars, sim=sim, opt=2)

    chk[sim] <- prod(round(apply(Hist@AtAge$Number[sim,,,], 1:2, sum)/t(simple$N) ,0))
  }
  chk
}

testthat::expect_equal(testpopdyn(OM), rep(1, OM@nsim))


