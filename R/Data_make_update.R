

makeData <- function(Biomass, CBret, Cret, N, SSB, VBiomass, StockPars,
                     FleetPars, ObsPars, ImpPars, RefPoints,
                     SampCpars, initD, Sample_Area,
                     Name,
                     nyears,
                     proyears,
                     nsim,
                     nareas,
                     reps,
                     CurrentYr,
                     silent=FALSE) {

  if(!silent) message("Simulating observed data")

  Data <- new("Data")  # create a blank DLM data object
  if (reps == 1) Data <- OneRep(Data)  # make stochastic variables certain for only one rep
  Data <- replic8(Data, nsim)  # make nsim sized slots in the DLM data object

  Data@Name <- Name
  Data@Year <- (CurrentYr - nyears+1):CurrentYr

  # --- Observed catch ----
  # Simulated observed retained catch (biomass)
  Data@Cat <- ObsPars$Cobs_y[,1:nyears] * apply(CBret*Sample_Area$Catch[,,1:nyears,], c(1, 3), sum)
  Data@CV_Cat <- matrix(Data@CV_Cat[,1], nrow=nsim, ncol=nyears)

  # --- Index of total abundance ----
  # Index of abundance from total biomass - beginning of year before fishing
  # apply hyperstability / hyperdepletion
  II <- (apply(Biomass*Sample_Area$BInd[,,1:nyears,], c(1, 3), sum)^ObsPars$betas) *
    ObsPars$Ierr_y[, 1:nyears]
  II <- II/apply(II, 1, mean)  # normalize
  Data@Ind <- II # index of total abundance
  Data@CV_Ind <- matrix(Data@CV_Ind[,1], nrow=nsim, ncol=nyears)

  # --- Index of spawning abundance ----
  # Index of abundance from total biomass - beginning of year before fishing
  # apply hyperstability / hyperdepletion
  II <- (apply(SSB*Sample_Area$SBInd[,,1:nyears,], c(1, 3), sum)^ObsPars$betas) *
    ObsPars$SpIerr_y[, 1:nyears]
  II <- II/apply(II, 1, mean)  # normalize
  Data@SpInd <- II # index of spawning abundance
  Data@CV_SpInd <- matrix(Data@CV_SpInd[,1], nrow=nsim, ncol=nyears)

  # --- Index of vulnerable abundance ----
  # Index of abundance from vulnerable biomass - beginning of year before fishing
  # apply hyperstability / hyperdepletion
  II <- (apply(VBiomass*Sample_Area$VInd[,,1:nyears,], c(1, 3), sum)^ObsPars$betas) *
    ObsPars$VIerr_y[, 1:nyears]
  II <- II/apply(II, 1, mean)  # normalize
  Data@VInd <- II # index of vulnerable abundance
  Data@CV_VInd <- matrix(Data@CV_VInd[,1], nrow=nsim, ncol=nyears)

  # --- Index of recruitment ----
  Data@Rec <- apply(N[, 1, , ]*Sample_Area$RecInd[,1:nyears,], c(1, 2), sum) *
    ObsPars$Recerr_y[, 1:nyears]
  Data@t <- rep(nyears, nsim) # number of years of data

  # --- Average catch ----
  Data@AvC <- apply(Data@Cat, 1, mean, na.rm=TRUE) # average catch over all years

  # --- Depletion ----
  # observed depletion
  Depletion <- apply(SSB[,,nyears,],1,sum)/RefPoints$SSB0 # current depletion
  Data@Dt <- Depletion * ObsPars$Derr_y[,nyears]
  Data@Dep <- Depletion * ObsPars$Derr_y[,nyears]

  # --- Life-history parameters ----
  Data@vbLinf <- StockPars$Linfarray[,nyears] * ObsPars$Linfbias # observed vB Linf
  Data@vbK <- StockPars$Karray[,nyears] * ObsPars$Kbias # observed vB K
  Data@vbt0 <- StockPars$t0array[,nyears] * ObsPars$t0bias # observed vB t0
  Data@Mort <- StockPars$Marray[,nyears] * ObsPars$Mbias # natural mortality
  Data@L50 <- StockPars$L50array[,nyears] * ObsPars$lenMbias # observed length at 50% maturity
  Data@L95 <- StockPars$L95array[,nyears] * ObsPars$lenMbias # observed length at 95% maturity
  Data@L95[Data@L95 > 0.9 * Data@vbLinf] <- 0.9 * Data@vbLinf[Data@L95 > 0.9 * Data@vbLinf]  # Set a hard limit on ratio of L95 to Linf
  Data@L50[Data@L50 > 0.9 * Data@L95] <- 0.9 * Data@L95[Data@L50 > 0.9 * Data@L95]  # Set a hard limit on ratio of L95 to Linf
  Data@LenCV <- StockPars$LenCV # variability in length-at-age - no error at this time
  Data@sigmaR <- StockPars$procsd * ObsPars$sigmaRbias # observed sigmaR -
  Data@MaxAge <- StockPars$maxage # maximum age - no error - used for setting up matrices only

  # Observed steepness values
  hs <- StockPars$hs
  Data@steep <- hs * ObsPars$hbias # observed steepness

  # --- Reference points ----
  # Simulate observation error in BMSY/B0
  ntest <- 20  # number of trials
  test <- array(RefPoints$SSBMSY_SSB0 * ObsPars$BMSY_B0bias, dim = c(nsim, ntest))  # the simulated observed BMSY_B0
  indy <- array(rep(1:ntest, each = nsim), c(nsim, ntest))  # index
  indy[test > max(0.9, max(RefPoints$SSBMSY_SSB0))] <- NA  # interval censor
  ObsPars$BMSY_B0bias <- ObsPars$BMSY_B0bias[cbind(1:nsim, apply(indy, 1, min, na.rm = T))]  # sample such that BMSY_B0<90%

  Data@FMSY_M <- RefPoints$FMSY_M * ObsPars$FMSY_Mbias # observed FMSY/M
  Data@BMSY_B0 <- RefPoints$SSBMSY_SSB0 * ObsPars$BMSY_B0bias # observed BMSY/B0
  Data@Cref <- RefPoints$MSY * ObsPars$Crefbias # Catch reference - MSY with error
  Data@Bref <- RefPoints$VBMSY * ObsPars$Brefbias # Vuln biomass ref - VBMSY with error

  # Generate values for reference SBMSY/SB0
  # should be calculated from unfished - won't be correct if initD is set
  I3 <- apply(Biomass, c(1, 3), sum)^ObsPars$betas  # apply hyperstability / hyperdepletion
  I3 <- I3/apply(I3, 1, mean)  # normalize index to mean 1
  if (!is.null(initD)) {
    b1 <- apply(Biomass, c(1, 3), sum)
    b2 <- matrix(RefPoints$BMSY, nrow=nsim, ncol=nyears)
    ind <- apply(abs(b1/ b2 - 1), 1, which.min) # find years closest to BMSY
    Iref <- diag(I3[1:nsim,ind])  # return the real target abundance index closest to BMSY
  } else {
    Iref <- apply(I3[, 1:5], 1, mean) * RefPoints$BMSY_B0  # return the real target abundance index corresponding to BMSY
  }
  Data@Iref <- Iref * ObsPars$Irefbias # index reference with error

  # --- Abundance ----
  # Calculate vulnerable and spawning biomass abundance --
  M_array <- array(0.5*StockPars$M_ageArray[,,nyears], dim=c(nsim, StockPars$maxage+1, nareas))
  A <- apply(VBiomass[, , nyears, ] * exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
  Asp <- apply(SSB[, , nyears, ] * exp(-M_array), 1, sum)  # Spawning abundance (mid-year before fishing)
  OFLreal <- A * (1-exp(-RefPoints$FMSY))  # the true simulated Over Fishing Limit

  # Observed total abundance
  Data@Abun <- A * ObsPars$Aerr_y[,nyears]

  # observed spawning abundance
  Data@SpAbun <- Asp * ObsPars$Aerr_y[,nyears]

  # --- Catch-at-age ----
  Cret2 <- apply(Cret * Sample_Area$CAA[,,1:nyears,], 1:3, sum)
  Data@CAA <- simCAA(nsim, nyears, StockPars$maxage+1, Cret2, ObsPars$CAA_ESS, ObsPars$CAA_nsamp)

  # --- Catch-at-length ----
  vn <- apply(N*Sample_Area$CAL[,,1:nyears,], c(1,2,3), sum) * FleetPars$retA[,,1:nyears]
  # numbers at age in population that would be retained
  vn <- aperm(vn, c(1,3, 2))

  CALdat <- simCAL(nsim, nyears, StockPars$maxage, ObsPars$CAL_ESS,
                   ObsPars$CAL_nsamp, StockPars$nCALbins, StockPars$CAL_binsmid,
                   vn, FleetPars$retL, StockPars$Linfarray,
                   StockPars$Karray, StockPars$t0array, StockPars$LenCV)

  Data@CAL_bins <- StockPars$CAL_bins
  Data@CAL_mids <- StockPars$CAL_binsmid
  Data@CAL <- CALdat$CAL # observed catch-at-length
  Data@ML <- CALdat$ML # mean length
  Data@Lc <- CALdat$Lc # modal length
  Data@Lbar <- CALdat$Lbar # mean length above Lc

  Data@LFC <- FleetPars$L5_y[,nyears] * ObsPars$LFCbias # length at first capture
  Data@LFS <- FleetPars$LFS_y[,nyears] * ObsPars$LFSbias # length at full selection

  # --- Previous Management Recommendations ----
  Data@MPrec <- apply(CBret, c(1, 3), sum)[,nyears] # catch in last year
  Data@MPeff <- rep(1, nsim) # effort in last year = 1

  # --- Store OM Parameters ----
  # put all the operating model parameters in one table
  ind <- which(lapply(StockPars, length) == nsim)
  stock <- as.data.frame(StockPars[ind])
  stock$Fdisc <- NULL
  stock$CAL_bins <- NULL
  stock$CAL_binsmid <- NULL
  ind <- which(lapply(FleetPars, length) == nsim)
  fleet <- as.data.frame(FleetPars[ind])

  ind <- which(lapply(ImpPars, length) == nsim)
  imp <- as.data.frame(ImpPars[ind])
  refs <- RefPoints[!names(RefPoints) %in% names(stock)]

  OMtable <- data.frame(stock, fleet, imp, refs, ageM=StockPars$ageM[,nyears],
                        L5=FleetPars$L5[,nyears ], LFS=FleetPars$LFS[,nyears ],
                        Vmaxlen=FleetPars$Vmaxlen[,nyears ],
                        LR5=FleetPars$LR5[,nyears], LFR=FleetPars$LFR[,nyears],
                        Rmaxlen=FleetPars$Rmaxlen[,nyears],
                        DR=FleetPars$DR[,nyears], OFLreal, maxF=StockPars$maxF,
                        A=A, Asp=Asp, CurrentYr=CurrentYr)

  OMtable <- OMtable[,order(names(OMtable))]
  Data@OM <- OMtable

  # --- Store Obs Parameters ----
  ObsParsDF <- ObsPars
  ind <- which(lapply(ObsParsDF, length)==nsim) %>% as.numeric()
  ObsParsDF <- ObsParsDF[ind]
  ObsTable <- as.data.frame(ObsParsDF)
  ObsTable <- ObsTable[,order(names(ObsTable))]
  Data@Obs <- ObsTable # put all the observation error model parameters in one table

  # --- Misc ----
  Data@Units <- "unitless"
  Data@Ref_type <- "Simulated OFL"
  Data@wla <- rep(StockPars$a, nsim)
  Data@wlb <- rep(StockPars$b, nsim)
  Data@nareas <- nareas
  Data@Ref <- OFLreal
  Data@LHYear <- CurrentYr  # Last historical year is nyears (for fixed MPs)
  Data@Misc <- vector("list", nsim)

  Data
}


updateData <- function(Data, OM, MPCalcs, Effort, Biomass, N, Biomass_P, CB_Pret,
                       N_P, SSB, SSB_P, VBiomass, VBiomass_P, RefPoints,
                       retA_P,
                       retL_P, StockPars, FleetPars, ObsPars,
                       V_P,
                       upyrs, interval, y=2,
                       mm=1, Misc, RealData, Sample_Area) {

  yind <- upyrs[match(y, upyrs) - 1]:(upyrs[match(y, upyrs)] - 1) # index

  nyears <- OM@nyears
  proyears <- OM@proyears
  nsim <- OM@nsim
  nareas <- StockPars$nareas
  reps <- OM@reps

  Data@Year <- (OM@CurrentYr - nyears+1):(OM@CurrentYr+ y - 1)
  Data@t <- rep(nyears + y, nsim)

  # --- Simulate catches ----
  CBtemp <- CB_Pret[, , yind, , drop=FALSE] * Sample_Area$Catch[,,nyears+yind,, drop=FALSE]
  CBtemp[is.na(CBtemp)] <- tiny
  CBtemp[!is.finite(CBtemp)] <- tiny

  yr.index <- max(which(!is.na(Data@CV_Cat[1,])))
  newCV_Cat <- matrix(Data@CV_Cat[,yr.index], nrow=nsim, ncol=length(yind))
  Data@CV_Cat <- cbind(Data@CV_Cat, newCV_Cat)


  # --- Observed catch ----
  # Simulated observed retained catch (biomass)
  Cobs <- ObsPars$Cobs_y[,nyears + yind] * apply(CBtemp, c(1, 3), sum, na.rm = TRUE)
  Data@Cat <- cbind(Data@Cat, Cobs)

  if (!is.null(RealData) && ncol(RealData@Cat)>nyears &&
      !all(is.na(RealData@Cat[1,(nyears+1):length(RealData@Cat[1,])]))) {
    # update projection catches with observed catches
    addYr <- min(y,ncol(RealData@Cat) - nyears)

    Data@Cat[,(nyears+1):(nyears+addYr)] <- matrix(RealData@Cat[1,(nyears+1):(nyears+addYr)],
                                                   nrow=nsim, ncol=addYr, byrow=TRUE)

    Data@CV_Cat[,(nyears+1):(nyears+addYr)] <- matrix(RealData@CV_Cat[1,(nyears+1):(nyears+addYr)],
                                                      nrow=nsim, ncol=addYr, byrow=TRUE)
  }

  # --- Index of total abundance ----
  yr.ind <- max(which(!is.na(ObsPars$Ierr_y[1,1:nyears])))
  I2 <- cbind(apply(Biomass*Sample_Area$BInd[,,1:nyears,], c(1, 3), sum)[,yr.ind:nyears],
              apply(Biomass_P*Sample_Area$BInd[,,(nyears+1):(nyears+proyears),], c(1, 3), sum)[, 1:(y - 1)])

  # standardize, apply  beta & obs error
  I2 <- exp(lcs(I2))^ObsPars$betas * ObsPars$Ierr_y[,yr.ind:(nyears + (y - 1))]
  year.ind <- max(which(!is.na(Data@Ind[1,1:nyears])))
  scaler <- Data@Ind[,year.ind]/I2[,1]
  scaler <- matrix(scaler, nrow=nsim, ncol=ncol(I2))
  I2 <- I2 * scaler # convert back to historical index scale

  I2 <- cbind(Data@Ind[,1:(yr.ind)], I2[,2:ncol(I2)])
  Data@Ind <- I2

  yr.index <- max(which(!is.na(Data@CV_Ind[1,1:nyears])))
  newCV_Ind <- matrix(Data@CV_Ind[,yr.index], nrow=nsim, ncol=length(yind))
  Data@CV_Ind <- cbind(Data@CV_Ind, newCV_Ind)

  if (!is.null(RealData) && ncol(RealData@Ind)>nyears &&
      !all(is.na(RealData@Ind[1,(nyears+1):length(RealData@Ind[1,])]))) {
    # update projection index with observed index if it exists
    addYr <- min(y,ncol(RealData@Ind) - nyears)
    Data@Ind[,(nyears+1):(nyears+addYr)] <- matrix(RealData@Ind[1,(nyears+1):(nyears+addYr)],
                                                   nrow=nsim, ncol=addYr, byrow=TRUE)

    Data@CV_Ind[,(nyears+1):(nyears+addYr)] <- matrix(RealData@CV_Ind[1,(nyears+1):(nyears+addYr)],
                                                      nrow=nsim, ncol=addYr, byrow=TRUE)
  }

  # --- Index of spawning abundance ----
  yr.ind <- max(which(!is.na(ObsPars$SpIerr_y[1,1:nyears])))
  I2 <- cbind(apply(SSB*Sample_Area$SBInd[,,1:nyears,], c(1, 3), sum)[,yr.ind:nyears],
              apply(SSB_P*Sample_Area$SBInd[,,(nyears+1):(nyears+proyears),], c(1, 3), sum)[, 1:(y - 1)])

  # standardize, apply  beta & obs error
  I2 <- exp(lcs(I2))^ObsPars$betas * ObsPars$SpIerr_y[,yr.ind:(nyears + (y - 1))]
  year.ind <- max(which(!is.na(Data@SpInd[1,1:nyears])))
  scaler <- Data@SpInd[,year.ind]/I2[,1]
  scaler <- matrix(scaler, nrow=nsim, ncol=ncol(I2))
  I2 <- I2 * scaler # convert back to historical index scale

  I2 <- cbind(Data@SpInd[,1:(yr.ind)], I2[,2:ncol(I2)])
  Data@SpInd <- I2

  yr.index <- max(which(!is.na(Data@CV_SpInd[1,1:nyears])))
  newCV_Ind <- matrix(Data@CV_SpInd[,yr.index], nrow=nsim, ncol=length(yind))
  Data@CV_SpInd <- cbind(Data@CV_SpInd, newCV_Ind)

  if (!is.null(RealData) && ncol(RealData@SpInd)>nyears &&
      !all(is.na(RealData@SpInd[1,(nyears+1):length(RealData@SpInd[1,])]))) {
    # update projection index with observed index if it exists
    addYr <- min(y,ncol(RealData@SpInd) - nyears)
    Data@SpInd[,(nyears+1):(nyears+addYr)] <- matrix(RealData@SpInd[1,(nyears+1):(nyears+addYr)],
                                                     nrow=nsim, ncol=addYr, byrow=TRUE)

    Data@CV_SpInd[,(nyears+1):(nyears+addYr)] <- matrix(RealData@CV_SpInd[1,(nyears+1):(nyears+addYr)],
                                                        nrow=nsim, ncol=addYr, byrow=TRUE)
  }

  # --- Index of vulnerable abundance ----
  yr.ind <- max(which(!is.na(ObsPars$VIerr_y[1,1:nyears])))
  I2 <- cbind(apply(VBiomass*Sample_Area$VInd[,,1:nyears,], c(1, 3), sum)[,yr.ind:nyears],
              apply(VBiomass_P*Sample_Area$VInd[,,(nyears+1):(nyears+proyears),], c(1, 3), sum)[, 1:(y - 1)])

  # standardize, apply  beta & obs error
  I2 <- exp(lcs(I2))^ObsPars$betas * ObsPars$VIerr_y[,yr.ind:(nyears + (y - 1))]
  year.ind <- max(which(!is.na(Data@VInd[1,1:nyears])))
  scaler <- Data@VInd[,year.ind]/I2[,1]
  scaler <- matrix(scaler, nrow=nsim, ncol=ncol(I2))
  I2 <- I2 * scaler # convert back to historical index scale

  I2 <- cbind(Data@VInd[,1:(yr.ind)], I2[,2:ncol(I2)])
  Data@VInd <- I2

  yr.index <- max(which(!is.na(Data@CV_VInd[1,1:nyears])))
  newCV_Ind <- matrix(Data@CV_VInd[,yr.index], nrow=nsim, ncol=length(yind))
  Data@CV_VInd <- cbind(Data@CV_VInd, newCV_Ind)

  if (!is.null(RealData) && ncol(RealData@VInd)>nyears &&
      !all(is.na(RealData@VInd[1,(nyears+1):length(RealData@VInd[1,])]))) {
    # update projection index with observed index if it exists
    addYr <- min(y,ncol(RealData@VInd) - nyears)
    Data@VInd[,(nyears+1):(nyears+addYr)] <- matrix(RealData@VInd[1,(nyears+1):(nyears+addYr)],
                                                    nrow=nsim, ncol=addYr, byrow=TRUE)

    Data@CV_VInd[,(nyears+1):(nyears+addYr)] <- matrix(RealData@CV_VInd[1,(nyears+1):(nyears+addYr)],
                                                       nrow=nsim, ncol=addYr, byrow=TRUE)
  }

  # --- Update additional indices (if they exist) ----
  AddIunits <- Data@AddIunits
  AddIndType <- Data@AddIndType

  if (length(ObsPars$AddIerr)>0) {
    n.ind <- dim(ObsPars$AddIerr)[2]
    AddInd <- array(NA, dim=c(nsim, n.ind, nyears+y-1))
    CV_AddInd  <- array(NA, dim=c(nsim, n.ind, nyears+y-1))
    for (i in 1:n.ind) {
      if (all(is.na(RealData@AddIndV[1, , ]))) {
        Ind_V <- rep(1, Data@MaxAge+1)
      } else {
        Ind_V <- RealData@AddIndV[1,i, ]
      }
      Ind_V <- matrix(Ind_V, nrow=Data@MaxAge+1, ncol= nyears+proyears)
      Ind_V <- replicate(nsim, Ind_V) %>% aperm(c(3,1,2))

      yr.ind <- max(which(!is.na(ObsPars$AddIerr[1,i, 1:nyears])))

      if (AddIunits[i]) { # Biomass-based index
        if (AddIndType[i]==1) {
          # total biomass
          b1 <- apply(Biomass[,,yr.ind:nyears,, drop=FALSE], c(1, 2, 3), sum)
          b2 <- apply(Biomass_P, c(1, 2, 3), sum)
        }

        if (AddIndType[i]==2) {
          # spawning biomass
          b1 <- apply(SSB[,,yr.ind:nyears,, drop=FALSE], c(1, 2, 3), sum)
          b2 <- apply(SSB_P, c(1, 2, 3), sum)
        }
        if (AddIndType[i]==3) {
          # vulnerable biomass
          b1 <- apply(VBiomass[,,yr.ind:nyears,, drop=FALSE], c(1, 2, 3), sum)
          b2 <- apply(VBiomass_P, c(1, 2, 3), sum)
        }
      } else {
        if (AddIndType[i]==1) {
          # total stock
          b1 <- apply(N[,,yr.ind:nyears,, drop=FALSE], c(1, 2, 3), sum) # Abundance-based index
          b2 <- apply(N_P, c(1, 2, 3), sum)
        }
        if (AddIndType[i]==2) {
          # spawning stock
          b1 <- apply(N[,,yr.ind:nyears,, drop=FALSE], c(1, 2, 3), sum) * StockPars$Mat_age[,,yr.ind:nyears,  drop=FALSE]
          b2 <- apply(N_P, c(1, 2, 3), sum)  * StockPars$Mat_age[,,(nyears+1):(nyears+proyears),  drop=FALSE]
        }
        if (AddIndType[i]==3) {
          # vuln stock
          b1 <- apply(N[,,yr.ind:nyears,, drop=FALSE], c(1, 2, 3), sum) * V_P[,,yr.ind:nyears,  drop=FALSE]
          b2 <- apply(N_P, c(1, 2, 3), sum) * V_P[,,(nyears+1):(nyears+proyears),  drop=FALSE]
        }
      }

      b1 <- apply(b1 * Ind_V[,,yr.ind:nyears, drop=FALSE], c(1,3), sum)
      b2 <- apply(b2 * Ind_V[,,(nyears+1):(nyears+proyears), drop=FALSE], c(1,3), sum)
      tempI <- cbind(b1, b2[, 1:(y - 1)])

      # standardize, apply  beta & obs error
      tempI <- exp(lcs(tempI))^ObsPars$AddIbeta[,i] * ObsPars$AddIerr[,i,yr.ind:(nyears + (y - 1))]
      year.ind <- max(which(!is.na(RealData@AddInd[1,i,])))

      scaler <- RealData@AddInd[1,i,year.ind]/tempI[,1]
      scaler <- matrix(scaler, nrow=nsim, ncol=ncol(tempI))
      tempI <- tempI * scaler # convert back to historical index scale

      AddInd[,i,] <- cbind(Data@AddInd[1:nsim,i,1:yr.ind], tempI[,2:ncol(tempI)])

      yr.index <- max(which(!is.na(Data@CV_AddInd[1,i,1:nyears])))
      newCV_Ind <- matrix(Data@CV_AddInd[,i,yr.index], nrow=nsim, ncol=length(yind))
      CV_AddInd[,i,] <- cbind(Data@CV_AddInd[,i,], newCV_Ind)

      if (!is.null(RealData) && length(RealData@AddInd[1,i,])>nyears &&
          !all(is.na(RealData@AddInd[1,i,(nyears+1):length(RealData@AddInd[1,i,])]))) {
        # update projection index with observed index if it exists
        addYr <- min(y-1,length(RealData@AddInd[1,i,]) - nyears)

        AddInd[,i,(nyears+1):(nyears+addYr)] <- matrix(RealData@AddInd[1,i,(nyears+1):(nyears+addYr)],
                                                       nrow=nsim, ncol=addYr, byrow=TRUE)

        CV_AddInd[,i,(nyears+1):(nyears+addYr)] <- matrix(RealData@CV_AddInd[1,i,(nyears+1):(nyears+addYr)],
                                                          nrow=nsim, ncol=addYr, byrow=TRUE)
      }
    }

    Data@AddInd <- AddInd
    Data@CV_AddInd <- CV_AddInd
  }

  # --- Index of recruitment ----
  Recobs <- ObsPars$Recerr_y[, nyears + yind] * apply(array(N_P[, 1, yind, ] *
                                                            Sample_Area$RecInd[,nyears+yind,],
                                                          c(nsim, interval[mm], nareas)),
                                                    c(1, 2), sum)


  Data@Rec <- cbind(Data@Rec, Recobs)

  # --- Average catch ----
  Data@AvC <- apply(Data@Cat, 1, mean)

  # --- Depletion ----
  Depletion <- apply(SSB_P[, , y, ], 1, sum)/RefPoints$SSB0
  Depletion[Depletion < tiny] <- tiny
  Data@Dt <- Depletion * ObsPars$Derr_y[,nyears+y]
  Data@Dep <-  Depletion * ObsPars$Derr_y[,nyears+y]

  # --- Update life-history parameter estimates for current year ----
  Data@vbLinf <- StockPars$Linfarray[,nyears+y] * ObsPars$Linfbias # observed vB Linf
  Data@vbK <- StockPars$Karray[,nyears+y] * ObsPars$Kbias # observed vB K
  Data@vbt0 <- StockPars$t0array[,nyears+y] * ObsPars$t0bias # observed vB t0
  Data@Mort <- StockPars$Marray[,nyears+y] * ObsPars$Mbias # natural mortality
  Data@L50 <- StockPars$L50array[,nyears+y] * ObsPars$lenMbias # observed length at 50% maturity
  Data@L95 <- StockPars$L95array[,nyears+y] * ObsPars$lenMbias # observed length at 95% maturity
  Data@L95[Data@L95 > 0.9 * Data@vbLinf] <- 0.9 * Data@vbLinf[Data@L95 > 0.9 * Data@vbLinf]  # Set a hard limit on ratio of L95 to Linf
  Data@L50[Data@L50 > 0.9 * Data@L95] <- 0.9 * Data@L95[Data@L50 > 0.9 * Data@L95]  # Set a hard limit on ratio of L95 to Linf


  # --- Abundance ----
  # Calculate vulnerable and spawning biomass abundance --
  M_array <- array(0.5*StockPars$M_ageArray[,,nyears+y], dim=c(nsim, StockPars$maxage+1, nareas))
  A <- apply(VBiomass_P[, , y, ] * exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
  Asp <- apply(SSB_P[, , y, ] * exp(-M_array), 1, sum)  # Spawning abundance (mid-year before fishing)
  Data@Abun <- A * ObsPars$Aerr_y[,nyears+yind]
  Data@SpAbun <- Asp *  ObsPars$Aerr_y[,nyears+yind]

  # --- Catch-at-age ----
  # previous CAA
  oldCAA <- Data@CAA
  Data@CAA <- array(0, dim = c(nsim, nyears + y - 1, StockPars$maxage+1))
  Data@CAA[, 1:(nyears + y - interval[mm] - 1), ] <- oldCAA[, 1:(nyears + y - interval[mm] - 1), ]

  # update CAA
  CNtemp <- retA_P[,,yind+nyears, drop=FALSE] *
    apply(N_P[,,yind,, drop=FALSE]*Sample_Area$CAA[,,nyears+yind,, drop=FALSE], 1:3, sum)
  CNtemp[is.na(CNtemp)] <- tiny
  CNtemp[!is.finite(CNtemp)] <- tiny

  CAA <- simCAA(nsim, yrs=length(yind), StockPars$maxage+1, Cret=CNtemp, ObsPars$CAA_ESS, ObsPars$CAA_nsamp)

  Data@CAA[, nyears + yind, ] <- CAA

  # --- Catch-at-length ----
  oldCAL <- Data@CAL
  Data@CAL <- array(0, dim = c(nsim, nyears + y - 1, StockPars$nCALbins))
  Data@CAL[, 1:(nyears + y - interval[mm] - 1), ] <- oldCAL[, 1:(nyears + y - interval[mm] - 1), ]

  CAL <- array(NA, dim = c(nsim, interval[mm], StockPars$nCALbins))
  vn <- (apply(N_P*Sample_Area$CAL[,,(nyears+1):(nyears+proyears),], c(1,2,3), sum) * retA_P[,,(nyears+1):(nyears+proyears)]) # numbers at age that would be retained
  vn <- aperm(vn, c(1,3,2))

  CALdat <- simCAL(nsim, nyears=length(yind), StockPars$maxage, ObsPars$CAL_ESS,
                   ObsPars$CAL_nsamp, StockPars$nCALbins, StockPars$CAL_binsmid,
                   vn=vn[,yind,, drop=FALSE], retL=retL_P[,,nyears+yind, drop=FALSE],
                   Linfarray=StockPars$Linfarray[,nyears + yind, drop=FALSE],
                   Karray=StockPars$Karray[,nyears + yind, drop=FALSE],
                   t0array=StockPars$t0array[,nyears + yind,drop=FALSE],
                   LenCV=StockPars$LenCV)

  Data@CAL[, nyears + yind, ] <- CALdat$CAL # observed catch-at-length
  Data@ML <- cbind(Data@ML, CALdat$ML) # mean length
  Data@Lc <- cbind(Data@Lc, CALdat$Lc) # modal length
  Data@Lbar <- cbind(Data@Lbar, CALdat$Lbar) # mean length above Lc

  Data@LFC <- FleetPars$L5_y[,nyears+y] * ObsPars$LFCbias # length at first capture
  Data@LFS <- FleetPars$LFS_y[,nyears+y] * ObsPars$LFSbias # length at full selection

  # --- Previous Management Recommendations ----
  Data@MPrec <- MPCalcs$TACrec # last MP  TAC recommendation
  if (length(dim(Effort)) == 5) {
    Data@MPeff <- Effort[, 1,1,mm, y-1] # last recommended effort
  } else {
    Data@MPeff <- Effort[, mm, y-1] # last recommended effort
  }


  Data@Misc <- Misc

  Data
}

#' Simulate Catch-at-Age Data
#'
#' CAA generated with a multinomial observation model from retained catch-at-age
#' data
#'
#' @param nsim Number of simulations
#' @param yrs Number of years
#' @param n_age Number of age classes
#' @param Cret Retained Catch at age in numbers - array(sim, years, maxage+1)
#' @param CAA_ESS CAA effective sample size
#' @param CAA_nsamp CAA sample size
#'
#' @return CAA array
simCAA <- function(nsim, yrs, n_age, Cret, CAA_ESS, CAA_nsamp) {
  # generate CAA from retained catch-at-age
  CAA <- array(NA, dim = c(nsim, yrs, n_age))  # Catch  at age array

  # a multinomial observation model for catch-at-age data
  for (i in 1:nsim) {
    for (j in 1:yrs) {
      if (!sum(Cret[i, ,j])) {
        CAA[i, j, ] <- 0
      } else {
        CAA[i, j, ] <- ceiling(-0.5 + rmultinom(1, CAA_ESS[i], Cret[i, ,j]) * CAA_nsamp[i]/CAA_ESS[i])
      }
    }
  }
  CAA
}

#' Simulate Catch-at-Length Data
#'
#' Simulate CAL and calculate length-at-first capture (LFC),
#' mean length (ML), modal length (Lc), and mean length over modal length (Lbar)
#'
#' @param nsim Number of simulations
#' @param nyears Number of years
#' @param maxage Maximum age
#' @param CAL_ESS CAA effective sample size
#' @param CAL_nsamp CAA sample size
#' @param nCALbins number of CAL bins
#' @param CAL_binsmid mid-points of CAL bins
#' @param vn Vulnerable numbers-at-age
#' @param retL Retention at length curve
#' @param Linfarray Array of Linf values by simulation and year
#' @param Karray Array of K values by simulation and year
#' @param t0array Array of t0 values by simulation and year
#' @param LenCV CV of length-at-age#'
#' @return named list with CAL array and LFC, ML, & Lc vectors
simCAL <- function(nsim, nyears, maxage,  CAL_ESS, CAL_nsamp, nCALbins, CAL_binsmid,
                   vn, retL, Linfarray, Karray, t0array, LenCV) {
  # a multinomial observation model for catch-at-length data
  # assumed normally-distributed length-at-age truncated at 2 standard deviations from the mean
  CAL <- array(NA, dim=c(nsim,  nyears, nCALbins))

  # Generate size comp data with variability in age
  runParallel <- snowfall::sfIsRunning()
  if (runParallel) {
    tempSize <- snowfall::sfLapply(1:nsim, genSizeCompWrap, vn, CAL_binsmid,
                                   retL, CAL_ESS, CAL_nsamp,
                                   Linfarray, Karray, t0array, LenCV, truncSD=2)
  } else {
    tempSize <- lapply(1:nsim, genSizeCompWrap, vn, CAL_binsmid, retL, CAL_ESS,
                       CAL_nsamp,
                       Linfarray, Karray, t0array, LenCV, truncSD=2)
  }
  CAL <- aperm(array(as.numeric(unlist(tempSize, use.names=FALSE)),
                     dim=c(nyears, length(CAL_binsmid), nsim)), c(3,1,2))

  # calculate LFC - length-at-first capture - 5th percentile
  LFC <- rep(NA, nsim)
  LFC <- unlist(lapply(tempSize, function(x) getfifth(x[nyears, ], CAL_binsmid)))
  LFC[is.na(LFC)] <- 1
  LFC[LFC<1] <- 1

  # Mean Length
  temp <- CAL * rep(CAL_binsmid, each = nsim * nyears)
  ML <- apply(temp, 1:2, sum)/apply(CAL, 1:2, sum)
  ML[!is.finite(ML)] <- 0

  # Lc - modal length
  Lc <- array(CAL_binsmid[apply(CAL, 1:2, which.max)], dim = c(nsim, nyears))

  # Lbar - mean length above Lc
  nuCAL <- CAL
  for (i in 1:nsim) {
    for (j in 1:nyears) {
      # nuCAL[i, j, 1:match(max(1, Lc[i, j]), CAL_binsmid, nomatch=1)] <- NA
      lcbin <- max(1,match(max(1, Lc[i, j]), CAL_binsmid, nomatch=1)-1)
      nuCAL[i, j, 1:lcbin] <- NA
    }
  }

  temp <- nuCAL * rep(CAL_binsmid, each = nsim * nyears)
  Lbar <- apply(temp, 1:2, sum, na.rm=TRUE)/apply(nuCAL, 1:2, sum, na.rm=TRUE)
  Lbar[!is.finite(Lbar)] <- 0
  out <- list()
  out$CAL <- CAL
  out$LFC <- LFC
  out$ML <- ML
  out$Lc <- Lc
  out$Lbar <- Lbar

  out
}

#' Wrapper for C++ function to generate length composition
#'
#' And other internal related functions
#'
#' @param i Simulation number
#' @param vn Array of vulnerable numbers
#' @param CAL_binsmid Mid-points of CAL bins
#' @param retL Array of retention-at-length
#' @param CAL_ESS CAL effective sample size
#' @param CAL_nsamp CAL sample size
#' @param Linfarray Matrix of Linf
#' @param Karray Matrix of K values
#' @param t0array Matrix of t0 values
#' @param LenCV Vector of LenCV
#' @param truncSD Numeric. Number of standard deviations to truncate normal d
#' distribution
#'
#' @return Generated length composition from `genSizeComp`
#'
#' @keywords internal
genSizeCompWrap <- function(i, vn, CAL_binsmid, retL,
                            CAL_ESS, CAL_nsamp,
                            Linfarray, Karray, t0array,
                            LenCV, truncSD=2) {
  VulnN <- as.matrix(vn[i,,])
  if (ncol(VulnN)>1) {
    VulnN <- VulnN/rowSums(VulnN) * CAL_nsamp[i] # get relative numbers at age
  } else {
    VulnN <- VulnN/sum(VulnN) * CAL_nsamp[i] # get relative numbers at age
  }

  VulnN <- round(VulnN,0) # convert to integers
  nyrs <- nrow(as.matrix(Linfarray[i,]))
  if (nyrs == 1) VulnN <- t(VulnN)
  retLa <- as.matrix(retL[i,,])

  # assumes lengths sampled throughout years
  # lens <- genSizeComp(VulnN, CAL_binsmid, retLa,
  #                     CAL_ESS=CAL_ESS[i], CAL_nsamp=CAL_nsamp[i],
  #                     Linfs=Linfarray[i,], Ks=Karray[i,], t0s=t0array[i,],
  #                     LenCV=LenCV[i], truncSD)

  # snapshot length comp
  lens <- genSizeComp2(VulnN, CAL_binsmid, retLa,
                      CAL_ESS=CAL_ESS[i], CAL_nsamp=CAL_nsamp[i],
                      Linfs=Linfarray[i,], Ks=Karray[i,], t0s=t0array[i,],
                      LenCV=LenCV[i], truncSD)


  lens[!is.finite(lens)] <- 0
  lens

}

#' @describeIn genSizeCompWrap Internal function to calculate fifth percentile of size composition
#' @param lenvec Vector of lengths
getfifth <- function(lenvec, CAL_binsmid) {
  temp <- rep(CAL_binsmid, lenvec)
  if(sum(lenvec)==0) return(NA)
  dens <- try(density(temp), silent=TRUE)
  if(class(dens)!="density") return(NA)
  dens$x[min(which(cumsum(dens$y/sum(dens$y)) >0.05))]
}

UpdateObs <- function(sl, obsval, OMval, RealData, SimData, msg){
  RealVal <- slot(RealData, sl)[1]
  SimVal <- slot(SimData, sl)
  if (!is.na(RealVal) & !all(SimVal == tiny)) {
    if (msg)
      message(paste0('Updating Observation Error for `OM@cpars$Data@', sl, '`'))

    # bias
    calcBias <- RealVal/OMval
    return(calcBias)
  } else {
    return(obsval)
  }
}

UpdateSlot <- function(sl, RealData, SimData, msg) {
  RealVal <- slot(RealData, sl)[1]
  SimVal <- slot(SimData, sl)
  if (!is.na(RealVal) & !all(SimVal == tiny)) {
    if (msg)
      message(paste0('Using `OM@cpars$Data@', sl, '` (', RealVal, ')'))
    nrep <- length(slot(SimData, sl))

    return(rep(RealVal, nrep))
  }
  return(slot(SimData, sl))
}


AddRealData <- function(SimData, RealData, ObsPars, StockPars, FleetPars, nsim,
                        nyears, proyears, SampCpars, msg) {
  Data_out <- SimData

  if (msg)
    message('Updating Simulated Data with Real Data from `OM@cpars$Data`')

  # check last year
  if (!is.na(RealData@LHYear) && !SimData@LHYear == RealData@LHYear) {
    warning('`Fleet@CurrentYear` (', SimData@LHYear, ') is not the same as `OM@cpars$Data@LHYear` (', RealData@LHYear, ')')
  }

  # check maxage
  if (!is.na(RealData@MaxAge) && !SimData@MaxAge == RealData@MaxAge) {
    warning('`Stock@MaxAge` (', SimData@MaxAge, ') is not the same as `OM@cpars$Data@MaxAge` (', RealData@MaxAge, ')')
  }

  # check dimensions of real data CAA
  if (!all(is.na(RealData@CAA)) && dim(RealData@CAA)[3] > 1) {
    if (!dim(RealData@CAA)[3] == SimData@MaxAge+1)
      stop('`OM@cpars$Data@CAA` has incorrect dimensions, should be `Stock@maxage+1` age-classes')
  }

  # update static slots
  slts <- c('Name', 'Common_Name', 'Species', 'Region', 'LHYear',
            'Units')
  for (sl in slts)
    slot(Data_out, sl) <- slot(RealData, sl)

  Data_out@MaxAge <- SimData@MaxAge
  Data_out@MPrec <- SimData@MPrec
  Data_out@MPeff <- SimData@MPeff
  Data_out@nareas <- SimData@nareas

  # ---- Update Life-history parameters ----
  Data_out@Mort <- UpdateSlot('Mort', RealData, SimData, msg)
  ObsPars$Mbias <- UpdateObs('Mort', ObsPars$Mbias, StockPars$Marray[, nyears],
                             RealData, SimData, msg)

  Data_out@vbLinf <- UpdateSlot('vbLinf', RealData, SimData, msg)
  ObsPars$Linfbias <- UpdateObs('vbLinf', ObsPars$Linfbias, StockPars$Linfarray[, nyears],
                                RealData, SimData, msg)

  Data_out@vbK <- UpdateSlot('vbK', RealData, SimData, msg)
  ObsPars$Kbias <- UpdateObs('vbK', ObsPars$Kbias, StockPars$Karray[, nyears],
                             RealData, SimData, msg)

  Data_out@vbt0 <- UpdateSlot('vbt0', RealData, SimData, msg)
  ObsPars$t0bias <- UpdateObs('vbt0', ObsPars$t0bias, StockPars$t0array[, nyears],
                              RealData, SimData, msg)

  Data_out@CV_Mort <- UpdateSlot('CV_Mort', RealData, SimData, msg)
  Data_out@CV_vbLinf <- UpdateSlot('CV_vbLinf', RealData, SimData, msg)
  Data_out@CV_vbK <- UpdateSlot('CV_vbK', RealData, SimData, msg)
  Data_out@CV_vbt0 <- UpdateSlot('CV_vbt0', RealData, SimData, msg)

  Data_out@wla <- UpdateSlot('wla', RealData, SimData, msg)
  Data_out@wlb <- UpdateSlot('wlb', RealData, SimData, msg)
  Data_out@CV_wla <- UpdateSlot('CV_wla', RealData, SimData, msg)
  Data_out@CV_wlb <- UpdateSlot('CV_wlb', RealData, SimData, msg)

  Data_out@steep <- UpdateSlot('steep', RealData, SimData, msg)
  Data_out@CV_steep <- UpdateSlot('CV_steep', RealData, SimData, msg)
  ObsPars$hbias <- UpdateObs('steep', ObsPars$hbias, StockPars$hs,
                             RealData, SimData, msg)

  Data_out@sigmaR <- UpdateSlot('sigmaR', RealData, SimData, msg)
  Data_out@CV_sigmaR <- UpdateSlot('CV_sigmaR', RealData, SimData, msg)

  # TODO ObsPars$sigmaRbias

  Data_out@L50 <- UpdateSlot('L50', RealData, SimData, msg)
  Data_out@CV_L50 <- UpdateSlot('CV_L50', RealData, SimData, msg)

  ObsPars$lenMbias <- UpdateObs('L50', ObsPars$lenMbias, StockPars$L50,
                                RealData, SimData, msg)

  Data_out@L95 <- UpdateSlot('L95', RealData, SimData, msg)
  Data_out@LenCV <- UpdateSlot('LenCV', RealData, SimData, msg)

  Data_out@LFC <- UpdateSlot('LFC', RealData, SimData, msg)
  Data_out@CV_LFC <- UpdateSlot('CV_LFC', RealData, SimData, msg)
  ObsPars$LFCbias <- UpdateObs('LFC', ObsPars$LFCbias, FleetPars$L5_y[,nyears],
                               RealData, SimData, msg)

  Data_out@LFS <- UpdateSlot('LFS', RealData, SimData, msg)
  Data_out@CV_LFS <- UpdateSlot('CV_LFS', RealData, SimData, msg)
  ObsPars$LFSbias <- UpdateObs('LFS', ObsPars$LFSbias, FleetPars$LFS_y[,nyears],
                               RealData, SimData, msg)

  Data_out@Vmaxlen <- UpdateSlot('Vmaxlen',  RealData, SimData, msg)

  if (length(RealData@Year)>1) {
    # check years
    if (all(RealData@Year !=SimData@Year)) {
      stop('`OM$cpars$Data@Year` does not match `SimData@Year`. \nAre `Fleet@nyears` and `Fleet@CurrentYr` correct?')
    }
    Data_out@Year <- RealData@Year
  }

  # ---- Update Catch ----
  if (!all(is.na(RealData@Cat[1,]))) {
    if (msg)
      message('Updating Simulated Catch from `OM@cpars$Data@Cat` (OM Catch observation parameters also updated)')

    Data_out@Cat <- matrix(RealData@Cat[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)
    Data_out@CV_Cat <- matrix(RealData@CV_Cat[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)

    simcatch <- apply(StockPars$CBret, c(1,3), sum)

    Cbias <- matrix(apply(Data_out@Cat, 1, mean)/apply(simcatch, 1, mean),
                    nrow=nsim, ncol=nyears+proyears)

    Cerr <- Data_out@Cat/(simcatch*Cbias[,1:nyears])
    t1<-  Cerr[,max(nyears-10, 1):nyears]/apply(Cerr[,max(nyears-10, 1):nyears],1,mean)
    SDs <- apply(log(t1), 1, sd)
    Cerr_proj <- matrix(NA, nsim, proyears)
    for (i in 1:nsim) {
      Cerr_proj[i,] <- exp(rnorm(proyears, -((SDs[i]^2)/2), SDs[i]))
    }
    Cerr <- cbind(Cerr, Cerr_proj)

    ObsPars$Cbias <- Cbias[,1]
    ObsPars$Cerr_y <- Cerr * Cbias

  }


  # ---- Update Effort -----
  # TODO

  # ---- Update Index (total biomass) ----
  if (!all(is.na(RealData@Ind[1,]))) { # Index exists
    if (msg)
      message('Updating Simulated Total Index from `OM@cpars$Data@Ind` (OM Index observation parameters are ignored).')
    Data_out@Ind <- matrix(RealData@Ind[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)
    Data_out@CV_Ind <- matrix(RealData@CV_Ind[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)

    # Calculate Error
    SimBiomass <- apply(StockPars$Biomass, c(1, 3), sum)
    I_Err <- lapply(1:nsim, function(i) indfit(SimBiomass[i,],  Data_out@Ind[i,]))
    I_Err <- do.call('rbind', I_Err)

    Ierr <- exp(lcs(Data_out@Ind))/exp(lcs(SimBiomass))^I_Err$beta

    if (!is.null(SampCpars$Ierr_y)) {
      if (msg) message('Total Index Observation Error found (cpars$Ierr_y) - not updating observation error')
    } else {
      if (msg) message('Updating Total Index Observation Error based on Observed Data')
      ObsPars$Ierr_y[, 1:nyears] <- Ierr # update Obs Error
      # Sample for projection years
      yr.ind <- max(which(!is.na(RealData@Ind[1,1:nyears])))
      ObsPars$Ierr[, (nyears+1):(nyears+proyears)] <- generateRes(df=I_Err,
                                                                  nsim, proyears,
                                                                  lst.err=log(ObsPars$Ierr[,yr.ind]))
    }

    ObsPars$Ind_Stat <- I_Err
  }

  # ---- Update Index (spawning biomass) ----
  if (!all(is.na(RealData@SpInd[1,]))) { # Index exists
    if (msg)
      message('Updating Simulated Spawning Index from `OM@cpars$Data@SpInd` (OM Index observation parameters are ignored).')
    Data_out@SpInd <- matrix(RealData@SpInd[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)
    Data_out@CV_SpInd <- matrix(RealData@CV_SpInd[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)

    # Calculate Error
    SimBiomass <- apply(StockPars$SSB, c(1, 3), sum)
    I_Err <- lapply(1:nsim, function(i) indfit(SimBiomass[i,],  Data_out@SpInd[i,]))
    I_Err <- do.call('rbind', I_Err)

    Ierr <- exp(lcs(Data_out@SpInd))/exp(lcs(SimBiomass))^I_Err$beta

    ObsPars$SpIerr[,1:nyears] <- Ierr

    # Sample for projection years
    yr.ind <- max(which(!is.na(RealData@SpInd[1,1:nyears])))
    ObsPars$SpIerr[, (nyears+1):(nyears+proyears)] <- generateRes(df=I_Err, nsim, proyears, lst.err=log(ObsPars$SpIerr[,yr.ind]))
    ObsPars$SpInd_Stat <- I_Err # return index statistics
  }

  # ---- Index (vulnerable biomass) ----
  if (!all(is.na(RealData@VInd[1,]))) { # Index exists
    if (msg)
      message('Updating Simulated Vulnerable Index from `OM@cpars$Data@VInd` (OM Index observation parameters are ignored).')
    Data_out@VInd <- matrix(RealData@VInd[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)
    Data_out@CV_VInd <- matrix(RealData@CV_VInd[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)

    # Calculate Error
    SimBiomass <- apply(StockPars$VBiomass, c(1, 3), sum)
    I_Err <- lapply(1:nsim, function(i) indfit(SimBiomass[i,],  Data_out@VInd[i,]))
    I_Err <- do.call('rbind', I_Err)

    Ierr <- exp(lcs(Data_out@VInd))/exp(lcs(SimBiomass))^I_Err$beta

    ObsPars$VIerr[,1:nyears] <- Ierr

    # Sample for projection years
    yr.ind <- max(which(!is.na(RealData@VInd[1,1:nyears])))
    ObsPars$VIerr[, (nyears+1):(nyears+proyears)] <- generateRes(df=I_Err, nsim, proyears, lst.err=log(ObsPars$VIerr[,yr.ind]))
    ObsPars$VInd_Stat <- I_Err # return index statistics
  }


  # ---- Add Additional Indices ----
  if (!all(is.na(RealData@AddInd))) {
    if (msg)
      message('Adding Additional Indices to Simulated Data from `OM@cpars$Data@AddInd`')
    n.ind <- dim(RealData@AddInd)[2]
    Data_out@AddInd <- Data_out@CV_AddInd <- array(NA, dim=c(nsim, n.ind, nyears))

    fitbeta <- fitIerr <- TRUE
    if (!is.null(SampCpars$AddIbeta)) {
      if (any(dim(SampCpars$AddIbeta) != c(nsim, n.ind)))
        stop("cpars$AddIbeta must be dimensions c(nsim, n.ind)")
      ObsPars$AddIbeta <- SampCpars$AddIbeta
      fitbeta <- FALSE
    } else {
      ObsPars$AddIbeta <- matrix(NA, nsim, n.ind)
    }

    if (!is.null(SampCpars$AddIerr)) {
      if (any(dim(SampCpars$AddIerr) != c(nsim, n.ind, nyears+proyears)))
        stop("cpars$AddIerr must be dimensions c(nsim, n.ind, nyears+proyears)")
      ObsPars$AddIerr <- SampCpars$AddIerr
      fitIerr <- FALSE
    } else {
      ObsPars$AddIerr <- array(NA, dim=c(nsim, n.ind, nyears+proyears))
    }

    if (!is.null(SampCpars$AddIunits) && length(SampCpars$AddIunits) != n.ind)
      stop("cpars$AddIunits must be length n.ind")

    AddIunits <- RealData@AddIunits
    if (all(is.na(AddIunits))) AddIunits <- rep(1, n.ind)
    if (!is.null(SampCpars$AddIunits)) AddIunits <- SampCpars$AddIunits
    Data_out@AddIunits <- AddIunits

    AddIndType <- RealData@AddIndType
    if (all(is.na(AddIndType))) AddIndType <- rep(1, n.ind)
    Data_out@AddIndType <- AddIndType

    ObsPars$AddInd_Stat <- list()

    UnitsTab <- data.frame(n=1:0, units=c('biomass', 'numbers'))
    TypeTab <- data.frame(n=1:3, type=c('total', 'spawning', 'vuln.'))
    for (i in 1:n.ind) {
      units <- UnitsTab$units[match(AddIunits[i], UnitsTab$n)]
      type <- TypeTab$type[match(AddIndType[i], TypeTab$n)]

      if(msg) message("Additional index ", i, ' - ', type, ' stock', paste0(' (', units, ')'))
      nyrs <- min(length(RealData@AddInd[1,i,]), nyears)
      ind <- RealData@AddInd[1,i,1:nyrs]
      cv_ind <- RealData@CV_AddInd[1,i,1:nyrs]
      if (nyrs < nyears) {
        ind <- c(ind, rep(NA,nyears-nyrs))
        cv_ind <- c(cv_ind, rep(NA,nyears-nyrs))
      }
      Data_out@AddInd[,i,] <- matrix(ind, nrow=nsim, ncol=nyears, byrow=TRUE)
      Data_out@CV_AddInd[,i,] <- matrix(cv_ind, nrow=nsim, ncol=nyears, byrow=TRUE)

      # Calculate observation error for future projections
      if (all(is.na(RealData@AddIndV[1,, ]))) {
        # no vulnerability-at-age included
        Ind_V <- rep(1, Data_out@MaxAge+1)
      } else {
        Ind_V <- RealData@AddIndV[1,i, ]
      }

      # check dimensions
      if (!length(Ind_V) == Data_out@MaxAge+1)
        stop('Vulnerability-at-age for additional index ', i, ' is not length `maxage`+1' )

      if (AddIunits[i]) {
        if (AddIndType[i]==1) SimIndex <- apply(StockPars$Biomass, c(1, 2, 3), sum) # Total Biomass-based index
        if (AddIndType[i]==2) SimIndex <- apply(StockPars$SSB, c(1, 2, 3), sum) # Spawning Biomass-based index
        if (AddIndType[i]==3) SimIndex <- apply(StockPars$VBiomass, c(1, 2, 3), sum) # vuln Biomass-based index
      } else {
        if (AddIndType[i]==1) SimIndex <- apply(StockPars$N, c(1, 2, 3), sum) # Total Abundance-based index
        if (AddIndType[i]==2) SimIndex <- apply(StockPars$N, c(1, 2, 3), sum) * StockPars$Mat_age[,,1:nyears] # Spawning abundance-based index
        if (AddIndType[i]==3) SimIndex <- apply(StockPars$N, c(1, 2, 3), sum) * FleetPars$V[,,1:nyears] # Spawning abundance-based index
      }

      Ind_V <- matrix(Ind_V, nrow=SimData@MaxAge+1, ncol= nyears)
      Ind_V <- replicate(nsim, Ind_V) %>% aperm(., c(3,1,2))
      SimIndex <- apply(SimIndex*Ind_V, c(1,3), sum) # apply vuln curve

      I_Err <- lapply(1:nsim, function(i) indfit(SimIndex[i,],  ind))
      I_Err <- do.call('rbind', I_Err)
      ind <- matrix(ind, nrow=nsim, ncol=nyears, byrow=TRUE)
      Ierr <- exp(lcs(ind))/exp(lcs(SimIndex))^I_Err$beta
      if (fitIerr) ObsPars$AddIerr[,i, 1:nyears] <- Ierr
      if (fitbeta) ObsPars$AddIbeta[,i] <- I_Err$beta

      # Sample for projection years
      if (fitIerr) {
        yr.ind <- max(which(!is.na(RealData@AddInd[1,i,1:nyrs])))
        diff <- nyears-nyrs

        ObsPars$AddIerr[,i, (nyrs+1):(nyears+proyears)] <- generateRes(df=I_Err, nsim, proyears+diff, lst.err=log(ObsPars$AddIerr[,i,yr.ind]))
      }

      if (nyrs < nyears) {
        # add simulated index for missing years
        tempI <- SimIndex

        # standardize, apply  beta & obs error
        tempI <- exp(lcs(tempI))^ObsPars$AddIbeta[,i] * ObsPars$AddIerr[,i,1:nyears]
        year.ind <- 1 #  max(which(!is.na(Data_out@AddInd[1,i,1:nyears])))
        scaler <- Data_out@AddInd[1,i,year.ind]/tempI[,1]
        scaler <- matrix(scaler, nrow=nsim, ncol=ncol(tempI))
        tempI <- tempI * scaler # convert back to historical index scale
        Data_out@AddInd[,i,1:nyears] <- tempI
      }

      ObsPars$AddInd_Stat[[i]] <- I_Err # index fit statistics
    }
  }

  # ---- Update Recruitment ----
  if (!all(is.na(RealData@Rec))) {
    if (msg)
      message('Updating Simulated Recruitment Data from `OM@cpars$Data@Rec`')

    Data_out@Rec <- matrix(RealData@Rec[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)

    dd <- dim(RealData@CV_Rec)
    if (dd[2]<nyears) {
      RealData@CV_Rec <- matrix(RealData@CV_Rec[1,1], nrow=nsim, ncol=nyears, byrow=TRUE)
    }

    Data_out@CV_Rec <- matrix(RealData@CV_Rec[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)

    # Calculate Error
    Rec <- apply(StockPars$N[, 1, , ], c(1, 2), sum) # simulated recruitment

    Recbias <- matrix(apply(Data_out@Rec, 1, mean)/apply(Rec, 1, mean),
                      nrow=nsim, ncol=nyears+proyears)

    Rec_err <- Data_out@Rec/(Rec*Recbias[,1:nyears])

    t1 <-  Rec_err[,max(nyears-10, 1):nyears]/apply(Rec_err[,max(nyears-10, 1):nyears],1,mean) # last 10 years used for projections
    SDs <- apply(log(t1), 1, sd)
    Rec_err_proj <- matrix(NA, nsim, proyears)
    for (i in 1:nsim) {
      Rec_err_proj[i,] <- exp(rnorm(proyears, -((SDs[i]^2)/2), SDs[i]))
    }
    Rec_err_proj <- Rec_err_proj * Recbias[,(nyears+1):(nyears+proyears)]
    Rec_err <- cbind(Rec_err, Rec_err_proj)

    ObsPars$Recerr_y <- Rec_err * Recbias
    ObsPars$Recsd <- SDs
    ObsPars$Recbias <- Recbias
  }

  # ---- Update CAA ----
  if (!all(is.na(RealData@CAA)) & !all(RealData@CAA ==0)) {
    if (msg)
      message('Updating Simulated Catch-at-Age Data from `OM@cpars$Data@CAA`. Note: CAA_ESS is currently NOT updated')

    Data_out@CAA <- aperm(replicate(nsim, RealData@CAA[1,1:nyears,]),c(3,1,2))
    Data_out@Vuln_CAA <- RealData@Vuln_CAA

    # Get average sample size
    nsamp <- ceiling(mean(apply(RealData@CAA[1,1:nyears,], 1, sum)))
    ObsPars$CAA_nsamp <- rep(nsamp, nsim)

  }

  # ---- Update CAL ----
  if (!all(is.na(RealData@CAL)) & !all(RealData@CAL ==0)) {

    # check length bins
    if (!all(RealData@CAL_bins %in% StockPars$CAL_bins)) {
      warning('cpars$Data@CAL_bins cannot be matched with Simulated Data@CAL_bins. Add cpars$Data@CAL_bins to cpars$CAL_bins. cpars$Data@CAL are NOT being used')
    } else {
      if (msg)
        message('Updating Simulated Catch-at-Length Data from `OM@cpars$Data@CAL`. Note: CAL_ESS is currently NOT updated')

      # match length bins
      ind <- match(RealData@CAL_mids, StockPars$CAL_binsmid)
      dd <- dim(Data_out@CAL)
      Data_out@CAL <- array(0, dim=dd)
      CAL <- Data_out@CAL[1,,]
      CAL[,ind] <- RealData@CAL[1,,] # replace with real CAL
      CAL <- aperm(replicate(nsim, CAL[1:nyears,]),c(3,1,2))
      Data_out@CAL <- CAL
      Data_out@Vuln_CAL <- RealData@Vuln_CAL

      # Get average sample size
      nsamp <- ceiling(mean(apply(RealData@CAL[1,1:nyears,], 1, sum, na.rm=TRUE), na.rm=TRUE))
      ObsPars$CAL_nsamp <- rep(nsamp, nsim)
    }
  }



  # ---- Depletion ----
  Data_out@Dep <- UpdateSlot('Dep', RealData, SimData, msg)
  Data_out@CV_Dep <- UpdateSlot('CV_Dep', RealData, SimData, msg)
  ObsPars$Dbias <- UpdateObs('Dep', ObsPars$Dbias, StockPars$Depletion,
                             RealData, SimData, msg)


  # ---- Index Reference -----
  Data_out@Iref <- UpdateSlot('Iref', RealData, SimData, msg)
  Data_out@CV_Iref <- UpdateSlot('CV_Iref', RealData, SimData, msg)
  ObsPars$Irefbias <- rep(NA, nsim) # not calculated


  # TODO - not currently updated
  NotUpdated <- function(RealData, sl, msg) {
    if (!all(is.na(slot(RealData, sl)))) {
      if (msg)
        message(paste0('Data detected in `OM@cpars$Data@', sl, '` but is NOT being used.'))
    }
  }


  NotUpdated(RealData, 'ML', msg)
  NotUpdated(RealData, 'Lc', msg)
  NotUpdated(RealData, 'Lbar', msg)
  NotUpdated(RealData, 'Abun', msg)
  NotUpdated(RealData, 'SpAbun', msg)
  NotUpdated(RealData, 'FMSY_M', msg)
  NotUpdated(RealData, 'BMSY_B0', msg)
  NotUpdated(RealData, 'Cref', msg)
  NotUpdated(RealData, 'Bref', msg)

  NotUpdated(RealData, 'AvC', msg)
  NotUpdated(RealData, 'Dt', msg)
  NotUpdated(RealData, 'Ref', msg)

  list(Data=Data_out, ObsPars=ObsPars)
}


