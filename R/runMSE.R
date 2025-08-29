


Project_Hist <- function (Hist=NULL, MPs=NA, parallel=FALSE,
                     silent=FALSE, extended=FALSE, checkMPs=FALSE) {

  # ---- Setup ----
  if (!methods::is(Hist,'Hist'))
    stop('Must provide an object of class `Hist`')
  
  OM <- Hist@OM
  set.seed(OM@seed) # set seed for reproducibility
  nsim <- OM@nsim # number of simulations
  nyears <- OM@nyears # number of historical years
  proyears <- OM@proyears # number of projection years
  interval <- OM@interval # management interval (annual)
  maxF <- OM@maxF # maximum apical F
  pstar <- OM@pstar # Percentile of the sample of the TAC for each MP
  reps <- OM@reps # Number of samples of the management recommendation for each MP

  control <- OM@cpars$control; OM@cpars$control <- NULL

  # ---- Check MPs ----
  if (checkMPs)
    MPs <- CheckMPs(MPs=MPs, silent=silent)

  # ---- Set up parallel processing of MPs ---
  runparallel <- FALSE 
  if (any(parallel==TRUE)) runparallel <- TRUE
  if (methods::is(parallel, 'list')) runparallel <- TRUE

  nMP <- length(MPs)  # the total number of methods used
  if (nMP < 1) stop("No valid MPs found", call.=FALSE)

  isrunning <- snowfall::sfIsRunning()
  if (!runparallel & isrunning) snowfall::sfStop()

  # Don't run MPs in parallel unless specified
  parallel_MPs <-  rep(FALSE, nMP) 
  if (methods::is(parallel, 'list')) {
    parallel <- parallel[parallel==TRUE]
    ind <- match(names(parallel), MPs)
    ind <- ind[!is.na(ind)]
    parallel_MPs[ind] <- TRUE
  }

  if (runparallel & any(parallel_MPs)) {
    if (!isrunning) setup()
    Export_customMPs(MPs)
  }
  
  # ---- Set Management Interval for each MP ----
  if (length(interval) != nMP) interval <- rep(interval, nMP)[1:nMP]
  if (!all(interval == interval[1])) {
    if (!silent) message("Variable management intervals:")
    df <- data.frame(MP=MPs,interval=interval)
    for (i in 1:nrow(df)) {
      message(df$MP[i], 'has management interval:', df$interval[i])
    }
  }

  # --- Add nMP dimension to MSY stats  ----
  # extract from Hist object
  MSY_y <- Hist@Ref$ByYear$MSY
  FMSY_y <- Hist@Ref$ByYear$FMSY
  SSBMSY_y <- Hist@Ref$ByYear$SSBMSY
  BMSY_y <- Hist@Ref$ByYear$BMSY
  VBMSY_y <- Hist@Ref$ByYear$VBMSY
  F01_YPR_y <- Hist@Ref$ByYear$F01_YPR
  Fmax_YPR_y <- Hist@Ref$ByYear$Fmax_YPR
  F_SPR_y <- Hist@Ref$ByYear$F_SPR
  SPR_target <- F_SPR_y %>% dimnames() %>% getElement(2) %>% substr(3,4) %>% as.numeric()
  SPR_target <- SPR_target/100

  # store MSY for each sim, MP and year
  MSY_y <- array(MSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  FMSY_y <- array(FMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  SSBMSY_y <- array(SSBMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  BMSY_y <- array(BMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  VBMSY_y <- array(VBMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  F01_YPR_y <- array(F01_YPR_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  Fmax_YPR_y <- array(Fmax_YPR_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  F_SPR_y <- array(F_SPR_y, dim=c(nsim, length(SPR_target), nyears+proyears, nMP)) %>% aperm(c(1,4,2,3))

  # ---- Set-up arrays and objects for projections ----
  # create a data object for each method
  # (they have identical historical data and branch in projected years)
  Data <- Hist@Data
  # no need to replicate Data@Misc across MPs
  Data_Misc <- Data@Misc
  Data@Misc <- list()
  MSElist <- list(Data)[rep(1, nMP)]

  SB_SBMSY_a <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected SB_SBMSY
  F_FMSYa <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected F_FMSY
  Ba <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected Biomass
  SSBa <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected SSB
  VBa <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected vulnerable biomass
  FMa <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected fishing mortality rate
  Ca <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected removed catch
  CaRet <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected retained catch
  TACa <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected TAC recommendation
  Effort <- array(NA, dim = c(nsim, nMP, proyears))  # store the Effort
  # PAAout <- array(NA, dim = c(nsim, nMP, maxage))  # store the population-at-age in last projection year
  # CAAout <- array(NA, dim = c(nsim, nMP, maxage))  # store the catch-at-age in last projection year
  # CALout <- array(NA, dim = c(nsim, nMP, nCALbins))  # store the population-at-length in last projection year
  SPReqa <- array(NA,dim=c(nsim,nMP,proyears)) # store the equilibrium Spawning Potential Ratio
  SPRdyna <- array(NA,dim=c(nsim,nMP,proyears)) # store the dynamic Spawning Potential Ratio

  Cost_out <- array(NA, dim = c(nsim, nMP, proyears))  # store Total Cost
  Rev_out <- array(NA, dim = c(nsim, nMP, proyears))  # store Total Revenue
  LatEffort_out<- array(NA, dim = c(nsim, nMP, proyears))  # store the Latent Effort
  TAE_out <- array(NA, dim = c(nsim, nMP, proyears)) # store the TAE

  # ---- Grab Stock, Fleet, Obs, and Imp Pars from Hist ----
  StockPars <- Hist@SampPars$Stock
  FleetPars <- Hist@SampPars$Fleet
  ObsPars <- Hist@SampPars$Obs
  ImpPars <- Hist@SampPars$Imp

  StockPars$N <- Hist@AtAge$Number
  StockPars$Z <- Hist@AtAge$Z.Mortality
  StockPars$FM <- Hist@AtAge$F.Mortality
  StockPars$FMret <- Hist@AtAge$Fret.Mortality
  StockPars$CB <- Hist@AtAge$Removals
  StockPars$CBret <- Hist@AtAge$Landings
  StockPars$Biomass <- Hist@AtAge$Biomass
  StockPars$SSB <- Hist@AtAge$SBiomass
  StockPars$VBiomass <- Hist@AtAge$VBiomass
  
  n_age <- StockPars$n_age
  nareas <- StockPars$nareas

  RealData <- Hist@OM@cpars$Data

  ReferencePoints <- Hist@Ref$ReferencePoints

  LatentEff <- Hist@Misc$BioEco$LatentEff
  RevCurr <- Hist@Misc$BioEco$RevCurr
  CostCurr <- Hist@Misc$BioEco$CostCurr
  Response <- Hist@Misc$BioEco$Response
  CostInc <- Hist@Misc$BioEco$CostInc
  RevInc <- Hist@Misc$BioEco$RevInc

  # projection arrays for storing all info (by simulation, age, MP, years, areas)
  N_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))
  B_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))
  SB_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))
  VB_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))
  Catch_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))
  Removals_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))
  FM_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))
  FMret_P_mp <- array(NA, dim = c(nsim, n_age, nMP, proyears, nareas))

  spawn_time_frac <- StockPars$spawn_time_frac
  spawn_time_frac <- replicate(StockPars$maxage+1, spawn_time_frac)
  spawn_time_frac <- replicate(proyears, spawn_time_frac)
  spawn_time_frac <- replicate(nareas, spawn_time_frac)
 
  # ---- Begin loop over MPs ----
  mm <- 1 # for debugging

  for (mm in 1:nMP) {  # MSE Loop over methods
    Data_MP <- MSElist[[mm]]
    Data_MP@Misc <- Data_Misc # add StockPars etc back to Data object

    if(!silent) message(mm, "/", nMP, " Running MSE for", MPs[mm], ifelse(parallel_MPs[mm], "in parallel", ""))
    checkNA <- rep(0, OM@proyears) # save number of NAs
    # years management is updated
    upyrs <- seq(from=1, to=proyears, by=interval[mm])

    # reset selectivity & retention parameters for projections
    L5_P <- FleetPars$L5_y
    LFS_P <- FleetPars$LFS_y
    Vmaxlen_P <- FleetPars$Vmaxlen_y
    # updated to use the realized selectivity/retention curves
    SLarray_P <- FleetPars$SLarray_real # selectivity at length array - projections
    V_P <- FleetPars$V  #  selectivity at age array - selectivity of gear
    V_P_real <- FleetPars$V_real  #  selectivity at age array - realized selectivity (max may be less than 1)
    V_P_real_2 <- FleetPars$V_real_2  #  selectivity at age array - realized selectivity (max = 1)
    
    if (is.null(V_P_real_2)) # hack for backward compatability
      V_P_real_2 <- V_P_real
    
    LR5_P <- FleetPars$LR5_y
    LFR_P <- FleetPars$LFR_y
    Rmaxlen_P <- FleetPars$Rmaxlen_y
    retA_P <- FleetPars$retA # retention at age array - projections
    retA_P_real <- FleetPars$retA_real # retention at age array -  realized selectivity (max may be less than 1) 
    retA_P_real_2 <- FleetPars$retA_real_2
    
    if (is.null(retA_P_real_2)) # hack for backward compatability
      retA_P_real_2 <- retA_P_real
    
    retL_P <- FleetPars$retL_real # retention at length array - projections
    Fdisc_P <- FleetPars$Fdisc_array1 # Discard mortality for projections - by age and year
    DR_P <- FleetPars$DR_y # Discard ratio for projections by year
    LatentEff_MP <- LatentEff # Historical latent effort

    # projection arrays
    N_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    Biomass_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    VBiomass_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    SSN_P <-array(NA, dim = c(nsim, n_age, proyears, nareas))
    SSB_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    FM_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    FM_Pret <- array(NA, dim = c(nsim, n_age, proyears, nareas)) # retained F
    Z_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    CB_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    CB_Pret <- array(NA, dim = c(nsim, n_age, proyears, nareas)) # retained catch

    # indexes
    SAYRL <- as.matrix(expand.grid(1:nsim, 1:n_age, nyears, 1:nareas))  # Final historical year
    SAYRt <- as.matrix(expand.grid(1:nsim, 1:n_age, 1 + nyears, 1:nareas))  # Trajectory year
    SAYR <- as.matrix(expand.grid(1:nsim, 1:n_age, 1, 1:nareas))
    SYt <- SAYRt[, c(1, 3)]
    SAYt <- SAYRt[, 1:3]
    SR <- SAYR[, c(1, 4)]
    SA1 <- SAYR[, 1:2]
    S1 <- SAYR[, 1]
    SY1 <- SAYR[, c(1, 3)]
    SAY1 <- SAYRt[, 1:3]
    SYA <- as.matrix(expand.grid(1:nsim, 1, 1:n_age))  # Projection year
    SY <- SYA[, 1:2]
    SA <- SYA[, c(1, 3)]
    SAY <- SYA[, c(1, 3, 2)]
    S <- SYA[, 1]

    # -- First projection year ----
    y <- 1
    if (requireNamespace("pbapply", quietly = TRUE)) {
      pb <- pbapply::timerProgressBar(min = 1, max = proyears, style = 3, width = min(getOption("width"), 50))
    } else {
      pb <- txtProgressBar(min = 1, max = proyears, style = 3, width = min(getOption("width"), 50))
    }
    # Mortality in first year
    NextYrN <- lapply(1:nsim, function(x)
      popdynOneTScpp(nareas, StockPars$maxage,
                     Ncurr=StockPars$N[x,,nyears,],
                     Zcurr=StockPars$Z[x,,nyears,],
                     plusgroup = StockPars$plusgroup))
 
    # The stock at the beginning of projection period
    N_P[,,1,] <- aperm(array(unlist(NextYrN), dim=c(n_age, nareas, nsim, 1)), c(3,1,4,2))
    Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAY1]  # Calculate biomass
    VBiomass_P[SAYR] <- N_P[SAYR] * FleetPars$Wt_age_C[SAY1] * V_P_real[SAYt]  # Calculate vulnerable biomass
    SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAY1]  # Calculate spawning stock numbers
    SSB_P[SAYR] <- N_P[SAYR] * StockPars$Fec_Age[SAY1]

    # Movement of stock at beginning of first projection year
    Ntemp <- lapply(1:nsim, function(x)
      movestockCPP(nareas,  StockPars$maxage,
                   StockPars$mov[x,,,,y+nyears],
                   N_P[x,,y,])
    )
    N_P[,,y,] <- array(unlist(Ntemp), dim=c(n_age, nareas, nsim)) %>% aperm(c(3,1,2))
    Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAY1]  # Calculate biomass
    VBiomass_P[SAYR] <- N_P[SAYR] * FleetPars$Wt_age_C[SAY1] * V_P_real[SAYt]  # Calculate vulnerable biomass
    SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAY1]  # Calculate spawning stock numbers
    SSB_P[SAYR] <- N_P[SAYR] * StockPars$Fec_Age[SAY1]
    
    StockPars$N_P <- N_P
    
    # -- Apply MP in initial projection year ----
    Data_MP@Misc$StockPars <- StockPars
    Data_MP@Misc$StockPars$CB_Pret <- CB_Pret
    Data_MP@Misc$StockPars$Biomass_P <- Biomass_P
    Data_MP@Misc$StockPars$SSB_P <- SSB_P
    Data_MP@Misc$StockPars$VBiomass_P <- VBiomass_P
    Data_MP@Misc$StockPars$N_P <- N_P
    runMP <- applyMP(Data=Data_MP, MPs = MPs[mm], reps = reps, silent=TRUE, 
                     parallel = parallel_MPs[mm])  # Apply MP

    MPRecs <- runMP[[1]][[1]] # MP recommendations
    Data_p <- runMP[[2]] # Data object object with saved info from MP
    Data_p@TAC <- MPRecs$TAC

    LastSpatial <- array(FleetPars$MPA[nyears,], dim=c(nareas, nsim)) #
    LastAllocat <- rep(1, nsim) # default assumption of reallocation of effort to open areas
    LastTAC <- LastCatch <- apply(StockPars$CBret[,,nyears,], 1, sum)

    # calculate pstar quantile of TAC recommendation dist
    TACused <- apply(Data_p@TAC, 2, quantile, p = pstar, na.rm = T)
    if (length(MPRecs$TAC) >0) {
      # a TAC has been recommended
      checkNA[y] <- sum(is.na(TACused))
      TACused[is.na(TACused)] <- LastTAC[is.na(TACused)] # set to last yr TAC if NA
      TACused[TACused<tiny] <- tiny
      TACa[, mm, y] <- TACused # recommended TAC
    }

    # -- Bio-Economics ----
    # Calculate Profit from last historical year
    RevPC <- RevCurr/LastCatch # cost-per unit catch in last historical year
    PMargin <- 1 - CostCurr/(RevPC * LastCatch) # profit margin in last historical year
    Profit <- (RevPC * LastCatch) - CostCurr # profit in last historical year
    HistEffort <- rep(1, nsim) # future effort is relative to today's effort
    Effort_pot <- HistEffort + Response*Profit # potential effort in first projection year
    Effort_pot[Effort_pot<0] <- tiny #

    # Latent Effort - Maximum Effort Limit
    if (!all(is.na(LatentEff_MP))) {
      LastTAE <- histTAE <- HistEffort / (1 - LatentEff_MP) # current TAE limit exists
    } else {
      LastTAE <- histTAE <- rep(NA, nsim) # no current TAE exists
    }

    # -- Calc stock dynamics ----
    MPCalcs <- CalcMPDynamics(MPRecs, y, nyears, proyears, nsim, Biomass_P,
                              VBiomass_P, LastTAE, histTAE, LastSpatial, LastAllocat,
                              LastTAC, TACused, maxF,LR5_P, LFR_P, Rmaxlen_P,
                              retL_P, retA_P, retA_P_real, retA_P_real_2, 
                              L5_P, LFS_P, Vmaxlen_P,
                              SLarray_P, 
                              V_P, V_P_real, V_P_real_2,
                              Fdisc_P, DR_P, FM_P,
                              FM_Pret, Z_P, CB_P, CB_Pret, Effort_pot,
                              StockPars, FleetPars, ImpPars, control=control)

    TACa[, mm, y] <- MPCalcs$TACrec # recommended TAC
    LastSpatial <- MPCalcs$Si
    LastAllocat <- MPCalcs$Ai
    LastTAE <- MPCalcs$TAE # TAE set by MP
    LastTAC <- MPCalcs$TACrec # TAC et by MP
    Effort[, mm, y] <- MPCalcs$Effort
    CB_P <- MPCalcs$CB_P # removals
    CB_Pret <- MPCalcs$CB_Pret # retained catch

    FM_P <- MPCalcs$FM_P # fishing mortality
    FM_Pret <- MPCalcs$FM_Pret # retained fishing mortality
    Z_P <- MPCalcs$Z_P # total mortality
    retL_P <- MPCalcs$retL_P # retained-at-length
    SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length
    FMa[,mm,y] <- MPCalcs$Ftot
    
    retA_P <- MPCalcs$retA_P # retained-at-age
    retA_P_real <- MPCalcs$retA_P_real 
    retA_P_real_2 <- MPCalcs$retA_P_real_2 
    retL_P <- MPCalcs$retL_P # retained-at-length
    V_P <- MPCalcs$V_P  # vulnerable-at-age
    V_P_real <- MPCalcs$V_P_real  # vulnerable-at-age
    V_P_real_2 <- MPCalcs$V_P_real_2  # vulnerable-at-age
    
    LR5_P <- MPCalcs$LR5_P
    LFR_P <- MPCalcs$LFR_P
    Rmaxlen_P <- MPCalcs$Rmaxlen_P
    L5_P <- MPCalcs$L5_P
    LFS_P <- MPCalcs$LFS_P
    Vmaxlen_P <- MPCalcs$Vmaxlen_P
    Fdisc_P <- MPCalcs$Fdisc_P
    DR_P <- MPCalcs$DR_P
    
    # ---- Account for timing of spawning (if spawn_time_frac >0) ----
    N_Psp <- N_P
    for (a in 1:n_age) {
      N_Psp[,a,1,] <- N_Psp[,a,1,] * exp(-(Z_P[,a,1,]*spawn_time_frac[,a,1,]))
    }
    SSN_P[SAYR] <- N_Psp[SAYR] * StockPars$Mat_age[SAY1] # update spawning stock numbers
    SSB_P[SAYR] <- N_Psp[SAYR] * StockPars$Fec_Age[SAY1] # update spawning biomass

    # recruitment in first projection year
    SSBcurr <- apply(SSB_P[,,1,],c(1,3), sum)
    recdev <- StockPars$Perr_y[, nyears+n_age]
    rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars$SRrel, 
                       SSBcurr=SSBcurr,
                       recdev=recdev, hs=StockPars$hs,
                       aR= StockPars$aR, bR=StockPars$bR, R0a=StockPars$R0a,
                       SSBpR=StockPars$SSBpR,
                       SRRfun=StockPars$SRRfun,
                       SRRpars=StockPars$SRRpars)
    
    N_P[,1,y,] <- t(rec_area)
    # update to include recruitment
    Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAY1]  # Calculate biomass
    VBiomass_P[SAYR] <- N_P[SAYR] * FleetPars$Wt_age_C[SAY1] * V_P[SAYt]  # Calculate vulnerable biomass
    
    StockPars$N_P <- N_P
  
    # ---- Bio-economics ----
    RetainCatch <- apply(CB_Pret[,,y,], 1, sum) # retained catch this year
    RetainCatch[RetainCatch<=0] <- tiny
    Cost_out[,mm,y] <-  Effort[, mm, y] * CostCurr*(1+CostInc/100)^y # cost of effort this year
    Rev_out[,mm,y] <- (RevPC*(1+RevInc/100)^y * RetainCatch)
    PMargin <- 1 - Cost_out[,mm,y]/Rev_out[,mm,y] # profit margin this year
    Profit <- Rev_out[,mm,y] - Cost_out[,mm,y] # profit this year
    Effort_pot <- Effort_pot + Response*Profit # bio-economic effort next year
    Effort_pot[Effort_pot<0] <- tiny #
    LatEffort_out[,mm,y] <- LastTAE - Effort[, mm, y]  # store the Latent Effort
    TAE_out[,mm,y] <- LastTAE # store the TAE

    # ---- Begin projection years ----
    for (y in 2:proyears) {
      if (!silent) setTxtProgressBar(pb, y) # Works with pbapply

      SelectChanged <- FALSE
      if (any(range(retA_P[,,nyears+y] -  FleetPars$retA[,,nyears+y]) !=0)) SelectChanged <- TRUE
      if (any(range(V_P[,,nyears+y] - FleetPars$V[,,nyears+y]) !=0))  SelectChanged <- TRUE

      # -- Calculate MSY stats for this year ----
      if (SelectChanged) { #
        y1 <- nyears + y
        MSYrefsYr <- sapply(1:nsim, optMSY_eq, 
                            yr.ind=y1, StockPars,
                            V_P, FleetPars$Wt_age_C)
        MSY_y[,mm,y1] <- MSYrefsYr[1, ]
        FMSY_y[,mm,y1] <- MSYrefsYr[2,]
        SSBMSY_y[,mm,y1] <- MSYrefsYr[3,]

        per_recruit_F <- lapply(1:nsim, per_recruit_F_calc,
                                yr.ind=y1,
                                StockPars=StockPars,
                                V=V_P,
                                Wt_age_C=FleetPars$Wt_age_C,
                                SPR_target=SPR_target)

        F_SPR_y[,mm,,y1] <- sapply(per_recruit_F, getElement, 1) %>% t()
        F01_YPR_y[,mm,y1] <- sapply(per_recruit_F, function(x) x[[2]][1])
        Fmax_YPR_y[,mm,y1] <- sapply(per_recruit_F, function(x) x[[2]][2])
      }

      # TACa[, mm, y] <- TACa[, mm, y-1] # TAC same as last year unless changed
      SAYRt <- as.matrix(expand.grid(1:nsim, 1:n_age, y + nyears, 1:nareas))  # Trajectory year
      SAYt <- SAYRt[, 1:3]
      SAYtMP <- cbind(SAYt, mm)
      SYt <- SAYRt[, c(1, 3)]
      SAY1R <- as.matrix(expand.grid(1:nsim, 1:n_age, y - 1, 1:nareas))
      SAYR <- as.matrix(expand.grid(1:nsim, 1:n_age, y, 1:nareas))
      SY <- SAYR[, c(1, 3)]
      SA <- SAYR[, 1:2]
      S1 <- SAYR[, 1]
      SAY <- SAYR[, 1:3]
      S <- SAYR[, 1]
      SR <- SAYR[, c(1, 4)]
      SA2YR <- as.matrix(expand.grid(1:nsim, 2:n_age, y, 1:nareas))
      SA1YR <- as.matrix(expand.grid(1:nsim, 1:(n_age - 1), y -1, 1:nareas))

      # --- Age & Growth ----
      NextYrN <- lapply(1:nsim, function(x)
        popdynOneTScpp(nareas, StockPars$maxage,
                       Ncurr=N_P[x,,y-1,],
                       Zcurr=Z_P[x,,y-1,],
                       plusgroup=StockPars$plusgroup))

      N_P[,,y,] <- aperm(array(unlist(NextYrN), dim=c(n_age, nareas, nsim, 1)), c(3,1,4,2))

      Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAYt]  # Calculate biomass
      SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAYt]  # Calculate spawning stock numbers (beginning of year)
      SSB_P[SAYR] <- N_P[SAYR] * StockPars$Fec_Age[SAYt]  # Calculate spawning stock biomass (beginning of year)

      # movement this year
      Ntemp <- lapply(1:nsim, function(x)
        movestockCPP(nareas,  StockPars$maxage,
                     StockPars$mov[x,,,,y+nyears],
                     N_P[x,,y,])
      )
      N_P[,,y,] <- array(unlist(Ntemp), dim=c(n_age, nareas, nsim)) %>% aperm(c(3,1,2))

      Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAYt]  # Calculate biomass
      VBiomass_P[SAYR] <- N_P[SAYR] * FleetPars$Wt_age_C[SAYt] * V_P[SAYt]  # Calculate vulnerable biomass
      SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAYt]  # Calculate spawning stock numbers
      SSB_P[SAYR] <- N_P[SAYR] * StockPars$Fec_Age[SAYt]  # Calculate spawning stock biomass
      
      StockPars$N_P <- N_P
      # --- An update year - update data and run MP ----
      if (y %in% upyrs) {
        # --- Update Data object ----
        Data_MP <- updateData(Data=Data_MP, OM, MPCalcs, Effort,
                              Biomass=StockPars$Biomass,
                              N=StockPars$N,
                              Biomass_P, CB_Pret, N_P, SSB=StockPars$SSB,
                              SSB_P, VBiomass=StockPars$VBiomass, VBiomass_P,
                              RefPoints=ReferencePoints,
                              retA_P, retL_P, FM_Pret, Z_P, StockPars,
                              FleetPars, ObsPars, ImpPars, V_P,
                              upyrs, interval, y, mm,
                              Misc=Data_p@Misc, RealData,
                              Sample_Area=ObsPars$Sample_Area)
        
        Data_MP@Misc$StockPars$CB_Pret <- CB_Pret
        Data_MP@Misc$StockPars$Biomass_P <- Biomass_P
        Data_MP@Misc$StockPars$SSB_P <- SSB_P
        Data_MP@Misc$StockPars$VBiomass_P <- VBiomass_P
        Data_MP@Misc$StockPars$N_P <- N_P

        # --- apply MP ----
        runMP <- applyMP(Data=Data_MP, MPs = MPs[mm], reps = reps, silent=TRUE, 
                         parallel = parallel_MPs[mm])  # Apply MP
        MPRecs <- runMP[[1]][[1]] # MP recommendations
        Data_p <- runMP[[2]] # Data object object with saved info from MP
        Data_p@TAC <- MPRecs$TAC
      }

      # calculate pstar quantile of TAC recommendation dist
      TACused <- apply(Data_p@TAC, 2, quantile, p = pstar, na.rm = T)
      if (length(MPRecs$TAC) >0) {
        # a TAC has been recommended
        checkNA[y] <- sum(is.na(TACused))
        TACused[is.na(TACused)] <- LastTAC[is.na(TACused)] # set to last yr TAC if NA
        TACused[TACused<tiny] <- tiny
        TACa[, mm, y] <- TACused # recommended TAC
      }

      # ----- Calc stock dynamics ----
      MPCalcs <- CalcMPDynamics(MPRecs, y, nyears, proyears, nsim, Biomass_P,
                                VBiomass_P, LastTAE, histTAE, LastSpatial, LastAllocat,
                                LastTAC, TACused, maxF,LR5_P, LFR_P, Rmaxlen_P,
                                retL_P, retA_P, retA_P_real, retA_P_real_2, 
                                L5_P, LFS_P, Vmaxlen_P,
                                SLarray_P, 
                                V_P, V_P_real, V_P_real_2,
                                Fdisc_P, DR_P, FM_P,
                                FM_Pret, Z_P, CB_P, CB_Pret, Effort_pot,
                                StockPars, FleetPars, ImpPars, control=control)
     
      LastSpatial <- MPCalcs$Si
      LastAllocat <- MPCalcs$Ai
      LastTAE <- MPCalcs$TAE # adjustment to TAE
      Effort[, mm, y] <- MPCalcs$Effort
      FMa[,mm,y] <- MPCalcs$Ftot

      CB_P <- MPCalcs$CB_P # removals
      CB_Pret <- MPCalcs$CB_Pret # retained catch
      LastTAC <- TACa[, mm, y] # apply(CB_Pret[,,y,], 1, sum, na.rm=TRUE)
      FM_P <- MPCalcs$FM_P # fishing mortality
      FM_Pret <- MPCalcs$FM_Pret # retained fishing mortality
      Z_P <- MPCalcs$Z_P # total mortality
      retA_P <- MPCalcs$retA_P # retained-at-age
      retA_P_real <- MPCalcs$retA_P_real 
      retA_P_real_2 <- MPCalcs$retA_P_real_2 
      retL_P <- MPCalcs$retL_P # retained-at-length
      V_P <- MPCalcs$V_P  # vulnerable-at-age
      V_P_real <- MPCalcs$V_P_real  # vulnerable-at-age
      V_P_real_2 <- MPCalcs$V_P_real_2  # vulnerable-at-age
      
      SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length

      LR5_P <- MPCalcs$LR5_P
      LFR_P <- MPCalcs$LFR_P
      Rmaxlen_P <- MPCalcs$Rmaxlen_P
      L5_P <- MPCalcs$L5_P
      LFS_P <- MPCalcs$LFS_P
      Vmaxlen_P <- MPCalcs$Vmaxlen_P
      Fdisc_P <- MPCalcs$Fdisc_P
      DR_P <- MPCalcs$DR_P
      
      
      # ---- Account for timing of spawning (if spawn_time_frac >0) ----
      N_Psp <- N_P
      for (a in 1:n_age) {
        N_Psp[,a,y,] <- N_Psp[,a,y,] * exp(-(Z_P[,a,y,]*spawn_time_frac[,a,y,]))
      }
      SSN_P[SAYR] <- N_Psp[SAYR] * StockPars$Mat_age[SAYt] # update spawning stock numbers
      SSB_P[SAYR] <- N_Psp[SAYR] * StockPars$Fec_Age[SAYt] # update spawning biomass
      
      # recruitment in this year
      SSBcurr <- apply(SSB_P[,,y,],c(1,3), sum)
      recdev <- StockPars$Perr_y[, y+nyears+n_age-1]
      rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars$SRrel,
                         SSBcurr=SSBcurr,
                         recdev=recdev, hs=StockPars$hs, aR=StockPars$aR,
                         bR=StockPars$bR, R0a=StockPars$R0a, 
                         SSBpR=StockPars$SSBpR,
                         SRRfun=StockPars$SRRfun,
                         SRRpars=StockPars$SRRpars)
      
      N_P[,1,y,] <- t(rec_area)

      StockPars$N_P <- N_P
      
      # update to include recruitment
      Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAYt]  # Calculate biomass
      VBiomass_P[SAYR] <- N_P[SAYR] * FleetPars$Wt_age_C[SAYt] * V_P[SAYt]  # Calculate vulnerable biomass

      # ---- Bio-economics ----
      RetainCatch <- apply(CB_Pret[,,y,], 1, sum) # retained catch this year
      RetainCatch[RetainCatch<=0] <- tiny
      Cost_out[,mm,y] <-  Effort[, mm, y] * CostCurr*(1+CostInc/100)^y # cost of effort this year
      Rev_out[,mm,y] <- (RevPC*(1+RevInc/100)^y * RetainCatch)
      Profit <- Rev_out[,mm,y] - Cost_out[,mm,y] # profit this year
      Effort_pot <- Effort_pot + Response*Profit # bio-economic effort next year
      Effort_pot[Effort_pot<0] <- tiny #
      LatEffort_out[,mm,y] <- LastTAE - Effort[, mm, y]  # store the Latent Effort
      TAE_out[,mm,y] <- LastTAE # store the TAE
      
      if ("progress" %in% names(control)) {
        if (control$progress) {
          if (requireNamespace("shiny", quietly = TRUE)) {
            shiny::setProgress((mm - 1 + y/proyears)/nMP, 
                               detail = paste0(round((mm - 1 + y/proyears)/nMP * 100), 
                                               "% \n Management procedure ", mm, "/", nMP, " (", MPs[mm], ")"))
          } else {
            warning('package `shiny` needs to be installed for progress bar')
          }
        }
      }

    }  # end of year loop
    
    if (!silent) close(pb) # use pbapply::closepb(pb) for shiny related stuff

    if (max(upyrs) < proyears) { # One more call to complete Data object
      Data_MP <- updateData(Data=Data_MP, OM, MPCalcs, Effort,
                                  StockPars$Biomass, StockPars$N,
                                  Biomass_P, CB_Pret, N_P, StockPars$SSB, SSB_P,
                                  StockPars$VBiomass, VBiomass_P,
                                  RefPoints=ReferencePoints,
                                  retA_P, retL_P, FM_Pret, Z_P, StockPars,
                                  FleetPars, ObsPars, ImpPars, V_P,
                                  upyrs=c(upyrs, proyears),
                                  interval=rep(proyears-max(upyrs), length(interval)), y, mm,
                                  Misc=Data_p@Misc, RealData, ObsPars$Sample_Area
      )
    }

    SB_SBMSY_a[, mm, ] <- apply(SSB_P, c(1, 3), sum, na.rm=TRUE)/SSBMSY_y[,mm,(OM@nyears+1):(OM@nyears+OM@proyears)]  # SSB relative to SSBMSY
    F_FMSYa[, mm, ] <- FMa[, mm, ]/FMSY_y[,mm,(OM@nyears+1):(OM@nyears+OM@proyears)]

    Ba[, mm, ] <- apply(Biomass_P, c(1, 3), sum, na.rm=TRUE) # biomass
    SSBa[, mm, ] <- apply(SSB_P, c(1, 3), sum, na.rm=TRUE) # spawning stock biomass
    VBa[, mm, ] <- apply(VBiomass_P, c(1, 3), sum, na.rm=TRUE) # vulnerable biomass

    Ca[, mm, ] <- apply(CB_P, c(1, 3), sum, na.rm=TRUE) # removed
    CaRet[, mm, ] <- apply(CB_Pret, c(1, 3), sum, na.rm=TRUE) # retained catch
    
    SPReqa[, mm, ] <- CalcSPReq(FM_P, StockPars, n_age, nareas, nyears, proyears, nsim)
    SPRdyna[, mm, ] <- CalcSPRdyn(abind::abind(StockPars$FM, FM_P, along = 3), 
                                  StockPars, n_age, nareas, nyears, proyears, nsim)

    if (!silent) {
      cat("\n")
      if (all(checkNA[upyrs] != nsim) & !all(checkNA == 0)) {
        ntot <- sum(checkNA[upyrs])
        totyrs <- sum(checkNA[upyrs] >0)
        nfrac <- round(ntot/(length(upyrs)*nsim),2)*100
        message(totyrs, ' years had TAC = NA for some simulations (', nfrac, "% of total simulations)")
        message('Used TAC_y = TAC_y-1')
      }
    }

    # Store all info (return if argument `extended=TRUE`)
    N_P_mp[,,mm,,] <- N_P
    B_P_mp[,,mm,,] <- Biomass_P
    SB_P_mp[,,mm,,] <- SSB_P
    VB_P_mp[,,mm,,] <- VBiomass_P
    Catch_P_mp[,,mm,,] <- CB_Pret
    Removals_P_mp[,,mm,,] <- CB_P
    FM_P_mp[,,mm,,] <- FM_P
    FMret_P_mp[,,mm,,] <- FM_Pret

    # drop StockPars etc from Data_MP - already reported in Hist object
    if (!extended) Data_MP@Misc$StockPars <- Data_MP@Misc$FleetPars <- Data_MP@Misc$ReferencePoints <- NULL

    MSElist[[mm]] <- Data_MP # update MSElist with PPD for this MP
  } # end of MP loop


  # ---- Create MSE Object ----
  CB_hist <- apply(Hist@TSdata$Landings, 1:2, sum)
  FM_hist <- Hist@TSdata$Find * Hist@SampPars$Fleet$qs
  SSB_hist <- apply(Hist@TSdata$SBiomass, 1:2, sum)

  Misc <- list()

  Misc$extended <- list()

  if (extended) {
    histN <- replicate(nMP, StockPars$N) %>% aperm(c(1,2,5,3,4))
    N_all <- abind::abind(histN, N_P_mp, along=4)

    histB <- replicate(nMP, StockPars$Biomass) %>% aperm(c(1,2,5,3,4))
    B_all <- abind::abind(histB, B_P_mp, along=4)

    histSSB <- replicate(nMP, StockPars$SSB) %>% aperm(c(1,2,5,3,4))
    SB_all <- abind::abind(histSSB, SB_P_mp, along=4)

    histVB <- replicate(nMP, StockPars$VBiomass) %>% aperm(c(1,2,5,3,4))
    VB_all <- abind::abind(histVB, VB_P_mp, along=4)

    histCatch <- replicate(nMP, StockPars$CBret) %>% aperm(c(1,2,5,3,4))
    Catch_all <- abind::abind(histCatch, Catch_P_mp, along=4)

    histRemovals <- replicate(nMP, StockPars$CB) %>% aperm(c(1,2,5,3,4))
    Removals_all <- abind::abind(histRemovals, Removals_P_mp, along=4)

    histF <- replicate(nMP, StockPars$FM) %>% aperm(c(1,2,5,3,4))
    FM_all <- abind::abind(histF, FM_P_mp, along=4)

    histFret <- replicate(nMP, StockPars$FMret) %>% aperm(c(1,2,5,3,4))
    FMret_all <- abind::abind(histFret, FMret_P_mp, along=4)

    Misc$extended <- list(
      N = N_all,
      B = B_all,
      SSB = SB_all,
      VB = VB_all,
      Catch = Catch_all,
      Removals = Removals_all,
      FM=FM_all,
      FMret=FMret_all
    )
    Hist_out <- Hist
  } else {
    Hist_out <- new("Hist")
    Hist_out@AtAge <- Hist@AtAge
    Hist_out@TSdata <- Hist@TSdata
    # Hist@Data <- new('Data')
    # Hist@OMPars <- data.frame()
    # Hist@AtAge <- list()
    # Hist@Ref <- list()
    # Hist@SampPars <- list()
  }

  MSEout <- new("MSE",
                Name = OM@Name,
                nyears=nyears, proyears=proyears, nMPs=nMP,
                MPs=MPs, nsim=nsim,
                OM=Data@OM,
                Obs=Data@Obs,
                SB_SBMSY=SB_SBMSY_a,
                F_FMSY=F_FMSYa,
                N=N_P_mp, # apply(N_P_mp, c(1,3,4), sum),
                B=Ba,
                SSB=SSBa,
                VB=VBa,
                FM=FMa,
                SPR=list(Equilibrium = SPReqa, Dynamic = SPRdyna),
                Catch=CaRet,
                Removals=Ca,
                Effort=Effort,
                TAC=TACa,
                TAE=TAE_out,
                BioEco=list(LatEffort=LatEffort_out,
                            Revenue=Rev_out,
                            Cost=Cost_out
                            ),
                RefPoint=list(MSY=MSY_y,
                              FMSY=FMSY_y,
                              SSBMSY=SSBMSY_y,
                              F_SPR=F_SPR_y,
                              Dynamic_Unfished=Hist@Ref$Dynamic_Unfished,
                              ByYear=Hist@Ref$ByYear
                              ),
                CB_hist=CB_hist,
                FM_hist=FM_hist,
                SSB_hist=SSB_hist,
                Hist=Hist_out,
                PPD=MSElist,
                Misc=Misc
  )

  # Store MSE info
  attr(MSEout, "version") <- packageVersion("MSEtool")
  attr(MSEout, "date") <- date()
  attr(MSEout, "R.version") <- R.version

  MSEout
}

#' Run a Management Strategy Evaluation
#'
#' Functions to run the Management Strategy Evaluation (closed-loop
#' simulation) for a specified operating model
#'
#' @param OM An operating model object (class [MSEtool::OM-class] or class `Hist`). Also works for
#' `MOM` objects, as a wrapper for `ProjectMOM`
#' @param MPs A vector of methods (character string) of class MP
#' @param Hist Should model stop after historical simulations? Returns an object of
#' class 'Hist' containing all historical data
#' @param silent Should messages be printed out to the console?
#' @param nsim Optional. numeric value to override `OM@nsim`.
#' @param parallel Logical or a named list. Should MPs be run using parallel processing? 
#' For \code{runMSE}, can also be \code{"sac"} to run the entire MSE in parallel
#' using the split-apply-combine technique. See Details for more information. 
#' @param extended Logical. Return extended projection results?
#' if TRUE, `MSE@Misc$extended` is a named list with extended data
#' (including historical and projection by area), extended version of `MSE@Hist`
#' is returned, and returns `MSE@PPD` with StockPars, FleetPars, and ReferencePoints in `MSE@PPD`
#' @param checkMPs Logical. Check if the specified MPs exist and can be run on `SimulatedData`?
#'
#' @describeIn runMSE Run the Historical Simulations and Forward Projections
#'  from an object of class `OM
#' @details 
#' ## Running MPs in parallel
#' 
#' For most MPs, running in parallel can actually lead to an increase in computation time, due to the overhead in sending the 
#' information over to the cores. Consequently, by default the MPs will not be run in parallel if `parallel=TRUE` 
#' (although other internal code will be run in parallel mode).
#' 
#' To run MPs in parallel, specify a named list with the name of the MP(s) assigned as TRUE. For example,`parallel=list(AvC=TRUE`)
#' will run the `AvC` MP in parallel mode.
#' 
#' ## Split-apply-combine MSE in parallel
#' 
#' Additional savings in computation time can be achieved by running the entire simulation in batches. Individual simulations of the operating model 
#' are divided into separate cores using \link{SubCpars}, `Simulate` and `Project` are applied independently for each core via `snowfall::sfClusterApplyLB`, and the 
#' output (a list of MSE objects) is stitched back together into a single MSE object using \link{joinMSE}. 
#' 
#' The ideal number of cores will be determined based on the number of simulations and available cores.
#'  
#' There are several issues to look out for when using this split-apply-combine technique:
#' 
#' \itemize{
#' \item Numerical optimization for depletion may fail in individual cores when \code{OM@cpars$qs} is not specified.
#' \item Length bins should be specified in the operating model in \code{OM@cpars$CAL_bins}. Otherwise, length bins can vary by core and
#' create problems when combining into a single object.
#' \item Compared to non-parallel runs, sampled parameters in the operating model will vary despite the same value in \code{OM@seed}.
#' \item If there is an error in individual cores or while combining the parallel output into a single Hist or MSE object, the list of output (from the cores) will be returned.
#' }
#'
#' @return Functions return objects of class \linkS4class{Hist} or \linkS4class{MSE}
#' \itemize{
#'   \item Simulate - An object of class \linkS4class{Hist}
#'   \item Project - An object of class \linkS4class{MSE}
#'   \item runMSE - An object of class \linkS4class{MSE} if \code{Hist = TRUE} otherwise a class \linkS4class{Hist} object
#' }
#' @export
runMSE <- function(OM=MSEtool::testOM, MPs = NA, Hist=FALSE, silent=FALSE,
                   parallel=FALSE, extended=FALSE, checkMPs=FALSE) {

  # ---- Initial Checks and Setup ----
  if (methods::is(OM,'OM')) {
    if (OM@nsim <=1) stop("OM@nsim must be > 1", call.=FALSE)

  } else if (methods::is(OM,'Hist')) {
    if (!silent) message("Using `Hist` object to reproduce historical dynamics")

    # # --- Extract cpars from Hist object ----
    # cpars <- list()
    # cpars <- c(OM@SampPars$Stock, OM@SampPars$Fleet, OM@SampPars$Obs,
    #            OM@SampPars$Imp, OM@OMPars, OM@OM@cpars)
    # 
    # # --- Populate a new OM object ----
    # newOM <- OM@OM
    # newOM@cpars <- cpars
    # OM <- newOM
  } else {
    stop("You must specify an operating model")
  }

  # check MPs
  if (checkMPs & !Hist)
    MPs <- CheckMPs(MPs=MPs, silent=silent)
  
  if (is.character(parallel) && parallel == "sac") {
    MSEout <- runMSE_sac(OM, MPs, Hist = Hist, silent = silent, extended = extended)
    return(MSEout)
  }

  if (methods::is(OM,'OM')) {
    HistSims <- Simulate(OM, parallel, silent)  
  } else {
    HistSims <- OM
  }
  
  if (Hist) {
    if(!silent) message("Returning historical simulations")
    return(HistSims)
  }

  if(!silent) message("Running forward projections")
  MSEout <- try(Project(Hist=HistSims, MPs, parallel, silent, extended = extended, checkMPs=FALSE), silent=TRUE)
  if (methods::is(MSEout, 'try-error')) {
    message('The following error occured when running the forward projections: ',
            crayon::red(attributes(MSEout)$condition))
    message('Returning the historical simulations (class `Hist`). To avoid re-running spool up, ',
            'the forward projections can be run with ',
            '`runMSE(Hist, MPs, ...)`')
    return(HistSims)
  }

  MSEout

}



runMSE_sac <- function(OM, MPs, Hist = FALSE, silent = FALSE, extended = FALSE) {
  
  if (OM@nsim <= 48) stop("OM@nsim should be greater than 48 to effectively use split-apply-combine.")
  if (is.null(OM@cpars$CAL_bins)) warning("OM@cpars$CAL_bins was not provided which can create issues with parallel = \"sac\"")
  ncpus <- set_parallel(TRUE)
  
  nsim <- OM@nsim
  nits <- ceiling(nsim/48)
  itsim <- rep(48,nits)
  if (nits < ncpus) {
    if (nits < 4) {
      nits <- 4
      itsim <- rep(ceiling(nsim/4), 4)
    } else{
      nits <- ncpus
      itsim <- rep(ceiling(nsim/ncpus), ncpus)
    }
  }
  cnt <- 1
  while (sum(itsim) != nsim | any(itsim<2)) {
    diff <-  nsim - sum(itsim)
    if (diff >0) {
      itsim[cnt] <- itsim[cnt]+1
    }
    if(diff < 0) {
      itsim[cnt] <- itsim[cnt]-1
    }
    cnt <- cnt+1
    if (cnt > length(itsim)) cnt <- 1
  }
  
  #### Split
  if (!silent) {
    message("Running MSE using split-apply-combine on ", length(itsim), " cores with these number of simulations:\n",
            paste0(itsim, collapse = ", "))
  }
  sims <- lapply(1:length(itsim), function(i) {
    if (i > 1) {
      (sum(itsim[1:(i-1)]) + 1): sum(itsim[1:i])
    } else {
      1:itsim[i]
    }
  })
  
  #### Apply Simulate() in parallel
  if (!silent) message("Running Simulate() in parallel..")
  HistList <- snowfall::sfClusterApplyLB(1:nits, function(i, OM, iter) {
    OM@seed <- OM@seed + i
    tryCatch(Simulate(SubCpars(OM, sims = iter[[i]]), silent = TRUE), error = function(e) as.character(e))
  }, OM = OM, iter = sims)
  
  # Error check Hist objects
  HistErr <- sapply(HistList, function(x) !inherits(x, "Hist"))
  if (any(HistErr)) {
    warning("Returning list of Hist objects. There was an error when running Simulate() for core(s): ", paste0(c(1:length(HistErr))[HistErr], collapse = ", "))
    return(HistList)
  }
  
  # Combine Hist objects and exit
  if (Hist) {
    Histout <- try(joinHist(HistList), silent = TRUE)
    if (methods::is(Histout, "try-error")) {
      warning("Error in joinHist() for combining Hist objects. Returning list of Hist objects.")
      Histout <- HistList
    }
    return(Histout)
  }
  
  #### Apply Project() in parallel
  Export_customMPs(MPs)
  if (!silent) message("Running Project() in parallel with ", length(MPs), " MPs..")
  MSElist <- snowfall::sfClusterApplyLB(HistList, function(i, MPs, extended) {
    tryCatch(Project(i, MPs = MPs, parallel = FALSE, silent = TRUE, extended = extended, checkMPs = FALSE),
             error = function(e) as.character(e))
  }, MPs = MPs, extended = extended)
  
  #### Combine MSE objects
  MSEout <- try(joinMSE(MSElist), silent = TRUE)
  if (methods::is(MSEout, "try-error")) {
    warning("Error in joinMSE() for combining MSE objects. Returning list of MSE objects.")
    MSEout <- MSElist
  }
  return(MSEout)
}
