# --- Calculate B-low ---- 
# (SSB where it takes MGThorizon x MGT to reach Bfrac of BMSY)
# Znow<-apply(Z[,,nyears,]*N[,,nyears,],1:2,sum)/apply(N[,,nyears,],1:2,sum)
# MGTsurv<-t(exp(-apply(Znow,1,cumsum)))
# MGT<-apply(Agearray*(Mat_age[,,nyears]*MGTsurv),1,sum)/apply(Mat_age[,,nyears]*MGTsurv,1,sum)

MarrayArea <- replicate(StockPars$nareas, StockPars$M_ageArray[,,1:nyears])
Mnow<-apply(MarrayArea[,,nyears,]*N[,,nyears,],1:2,sum)/apply(N[,,nyears,],1:2,sum)
MGTsurv<-t(exp(-apply(Mnow,1,cumsum)))
MGTsurv <- cbind(rep(1, nsim), MGTsurv)
MGTsurv <- MGTsurv[,1:n_age] # add age-0
MGT<-apply(Agearray*(StockPars$Mat_age[,,nyears]*MGTsurv),1,sum)/apply(StockPars$Mat_age[,,nyears]*MGTsurv,1,sum)

Blow <- rep(NA,nsim)
if(CalcBlow){
  if(!silent) message("Calculating B-low reference points")            
  MGThorizon<-floor(HZN*MGT)
  Blow <- sapply(1:nsim,getBlow,
                 StockPars$N, 
                 StockPars$Asize, 
                 StockPars$SSBMSY,
                 StockPars$SSBpR, 
                 FleetPars$MPA, 
                 StockPars$SSB0, 
                 StockPars$nareas, 
                 FleetPars$retA, 
                 MGThorizon,
                 FleetPars$Find,
                 StockPars$Perr_y,
                 StockPars$M_ageArray,
                 StockPars$hs,
                 StockPars$Mat_age, 
                 StockPars$Wt_age,
                 StockPars$R0a,
                 FleetPars$V,
                 StockPars$nyears,
                 StockPars$maxage,
                 StockPars$mov,
                 FleetPars$Spat_targ,
                 StockPars$SRrel,
                 StockPars$aR,
                 StockPars$bR,
                 Bfrac, 
                 maxF) 
}

