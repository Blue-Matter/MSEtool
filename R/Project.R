#' Run Forward Projections
#' 
#' 
#' @param Hist An Historical Simulation object (class `Hist`)
#' @param MPs A vector of methods (character string) of class MP
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical. Should the MSE be run using parallel processing?
#' 
#' @return An object of class \linkS4class{Hist}
#' @export
Project <- function (Hist=NULL, MPs=NA, parallel=FALSE, silent=FALSE) {
  
  # ---- Setup ----
  if (class(Hist) !='Hist')
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
  # Not currently included 
  # if (CheckMPs) {
  #   if(!silent) message("Determining available methods")
  #   PosMPs <- Can( Data, timelimit = timelimit)
  #   if (!is.na(allMPs[1])) {
  #     cant <- allMPs[!allMPs %in% PosMPs]
  #     if (length(cant) > 0) {
  #       if(!silent) stop(paste0("Cannot run some MPs:",
  #                               DLMtool::DLMdiag(Data, "not available",
  #                                                funcs1=cant, timelimit = timelimit)))
  #     }
  #   }
  # }
  
  # TODO - do this to runMSE as well
  if (all(is.na(MPs))) {
    if (!silent) message('No MPs have been specified, running with some demo MPs')
    MPs <- c('AvC') # TODO
  }
  nMP <- length(MPs)  # the total number of methods used
  if (nMP < 1) stop("No valid MPs found", call.=FALSE)
  
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
  # store MSY for each sim, MP and year
  MSY_y <- array(MSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2)) 
  FMSY_y <- array(FMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2)) 
  SSBMSY_y <- array(SSBMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2))
  BMSY_y <- array(BMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2)) 
  VBMSY_y <- array(VBMSY_y, dim=c(nsim, nyears+proyears, nMP)) %>% aperm(c(1,3,2)) 
  
  # ---- Set-up arrays and objects for projections ----
  # create a data object for each method 
  # (they have identical historical data and branch in projected years)
  Data <- Hist@Data
  MSElist <- list(Data)[rep(1, nMP)]  
  # TODO - update names of stored values
  B_BMSYa <- array(NA, dim = c(nsim, nMP, proyears))  # store the projected B_BMSY
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
  # SPRa <- array(NA,dim=c(nsim,nMP,proyears)) # store the Spawning Potential Ratio
  
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
  StockPars$CBret <- Hist@AtAge$Landings
  StockPars$Biomass <- Hist@AtAge$Biomass
  StockPars$SSB <- Hist@AtAge$SBiomass
  StockPars$VBiomass <- Hist@AtAge$VBiomass
  
  n_age <- StockPars$n_age
  nareas <- StockPars$nareas
  
  RealData <- Hist@OM@cpars$Data
  
  ReferencePoints <- Hist@Ref$ReferencePoints
  
  # Add to Hist - TODO -----
  # Bio-economic parameters - and add to model 
  LatentEff <- RevCurr <- CostCurr <- Response <- CostInc <-  RevInc <- rep(NA, nsim)
  
  # ---- Begin loop over MPs ----
  mm <- 1 # for debugging
  
  for (mm in 1:nMP) {  # MSE Loop over methods
    if(!silent) message(mm, "/", nMP, " Running MSE for ", MPs[mm])
    checkNA <- rep(0, OM@proyears) # save number of NAs
    # years management is updated
    upyrs <- seq(from=1, to=proyears, by=interval[mm])
    
    # reset selectivity & retention parameters for projections
    L5_P <- FleetPars$L5_y
    LFS_P <- FleetPars$LFS_y
    Vmaxlen_P <- FleetPars$Vmaxlen_y
    SLarray_P <- FleetPars$SLarray # selectivity at length array - projections
    V_P <- FleetPars$V  #  selectivity at age array - projections
    LR5_P <- FleetPars$LR5_y
    LFR_P <- FleetPars$LFR_y
    Rmaxlen_P <- FleetPars$Rmaxlen_y
    retA_P <- FleetPars$retA # retention at age array - projections
    retL_P <- FleetPars$retL # retention at length array - projections
    Fdisc_P <- FleetPars$Fdisc # Discard mortality for projections
    DR_P <- FleetPars$DR # Discard ratio for projections
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
    if(!silent) {
      cat("."); flush.console()
    }
    # Movement and mortality in first year
    NextYrN <- lapply(1:nsim, function(x)
      popdynOneTScpp(nareas, StockPars$maxage, 
                     Ncurr=StockPars$N[x,,nyears,],
                     Zcurr=StockPars$Z[x,,nyears,],
                     mov=StockPars$mov[x,,,,nyears+1],
                     plusgroup = StockPars$plusgroup))
    
    # The stock at the beginning of projection period
    N_P[,,1,] <- aperm(array(unlist(NextYrN), dim=c(n_age, nareas, nsim, 1)), c(3,1,4,2))
    Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAY1]  # Calculate biomass
    VBiomass_P[SAYR] <- Biomass_P[SAYR] * V_P[SAYt]  # Calculate vulnerable biomass
    SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAY1]  # Calculate spawning stock numbers
    SSB_P[SAYR] <- SSN_P[SAYR] * StockPars$Wt_age[SAY1]
    
    # recruitment in first projection year
    SSBcurr <- apply(SSB_P[,,1,],c(1,3), sum)
    recdev <- StockPars$Perr_y[, nyears+n_age]
    rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars$SRrel, SSBcurr=SSBcurr,
                       recdev=recdev, hs=StockPars$hs,
                       aR= StockPars$aR, bR=StockPars$bR, R0a=StockPars$R0a,
                       SSBpR=StockPars$SSBpR)
    
    N_P[,1,y,] <- t(rec_area)
    Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAY1]  # Calculate biomass
    VBiomass_P[SAYR] <- Biomass_P[SAYR] * V_P[SAYt]  # Calculate vulnerable biomass
    
    # Update abundance estimates - used for FMSY ref methods so that FMSY is applied to current abundance
    # TODO
    M_array <- array(0.5*StockPars$M_ageArray[,,nyears+y], dim=c(nsim, n_age, nareas))
    Atemp <- apply(VBiomass_P[, , y, ] * exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
    MSElist[[mm]]@OM$A <- Atemp
    
    # -- Apply MP in initial projection year ----
    runMP <- applyMP(Data=MSElist[[mm]], MPs = MPs[mm], reps = reps, silent=TRUE)  # Apply MP
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
                              retL_P, retA_P, L5_P, LFS_P, Vmaxlen_P,
                              SLarray_P, V_P, Fdisc_P, DR_P, FM_P,
                              FM_Pret, Z_P, CB_P, CB_Pret, Effort_pot,
                              StockPars, FleetPars, ImpPars)
    
    TACa[, mm, y] <- MPCalcs$TACrec # recommended TAC
    LastSpatial <- MPCalcs$Si
    LastAllocat <- MPCalcs$Ai
    LastTAE <- MPCalcs$TAE # TAE set by MP
    LastTAC <- MPCalcs$TACrec # TAC et by MP
    Effort[, mm, y] <- MPCalcs$Effort
    CB_P <- MPCalcs$CB_P # removals
    CB_Pret <- MPCalcs$CB_Pret # retained catch
    
    # apply(CB_Pret[,,1,], 1, sum)
    FM_P <- MPCalcs$FM_P # fishing mortality
    FM_Pret <- MPCalcs$FM_Pret # retained fishing mortality
    Z_P <- MPCalcs$Z_P # total mortality
    retA_P <- MPCalcs$retA_P # retained-at-age
    retL_P <- MPCalcs$retL_P # retained-at-length
    V_P <- MPCalcs$V_P  # vulnerable-at-age
    SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length
    FMa[,mm,y] <- MPCalcs$Ftot
    
    LR5_P <- MPCalcs$LR5_P
    LFR_P <- MPCalcs$LFR_P
    Rmaxlen_P <- MPCalcs$Rmaxlen_P
    L5_P <- MPCalcs$L5_P
    LFS_P <- MPCalcs$LFS_P
    Vmaxlen_P <- MPCalcs$Vmaxlen_P
    Fdisc_P <- MPCalcs$Fdisc_P
    DR_P <- MPCalcs$DR_P
    
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
    
    # --- Begin projection years ----
    for (y in 2:proyears) {
      if(!silent) {
        cat("."); flush.console()
      }
      
      SelectChanged <- FALSE
      if (any(range(retA_P[,,nyears+y] -  FleetPars$retA[,,nyears+y]) !=0)) SelectChanged <- TRUE
      if (any(range(V_P[,,nyears+y] - FleetPars$V[,,nyears+y]) !=0))  SelectChanged <- TRUE
      
      # -- Calculate MSY stats for this year ----
      if (SelectChanged) { #
        y1 <- nyears + y
        MSYrefsYr <- sapply(1:nsim, optMSY_eq, StockPars$M_ageArray, StockPars$Wt_age,
                            StockPars$Mat_age,
                            V_P, StockPars$maxage,StockPars$ R0, StockPars$SRrel,
                            StockPars$hs, yr.ind=y1, plusgroup=StockPars$plusgroup)
        MSY_y[,mm,y1] <- MSYrefsYr[1, ]
        FMSY_y[,mm,y1] <- MSYrefsYr[2,]
        SSBMSY_y[,mm,y1] <- MSYrefsYr[3,]
      }
      
      TACa[, mm, y] <- TACa[, mm, y-1] # TAC same as last year unless changed
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
                       mov=StockPars$mov[x,,,, nyears+y],
                       plusgroup=StockPars$plusgroup))
      
      N_P[,,y,] <- aperm(array(unlist(NextYrN), dim=c(n_age, nareas, nsim, 1)), c(3,1,4,2))
      Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAYt]  # Calculate biomass
      VBiomass_P[SAYR] <- Biomass_P[SAYR] * V_P[SAYt]  # Calculate vulnerable biomass
      SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAYt]  # Calculate spawning stock numbers
      SSB_P[SAYR] <- SSN_P[SAYR] * StockPars$Wt_age[SAYt]  # Calculate spawning stock biomass
      
      # recruitment in this year
      SSBcurr <- apply(SSB_P[,,y,],c(1,3), sum)
      recdev <- StockPars$Perr_y[, y+nyears+n_age-1]
      rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars$SRrel,
                         SSBcurr=SSBcurr,
                         recdev=recdev, hs=StockPars$hs, aR=StockPars$aR,
                         bR=StockPars$bR, R0a=StockPars$R0a, SSBpR=StockPars$SSBpR)
      
      N_P[,1,y,] <- t(rec_area)
      Biomass_P[SAYR] <- N_P[SAYR] * StockPars$Wt_age[SAY1]  # Calculate biomass
      VBiomass_P[SAYR] <- Biomass_P[SAYR] * V_P[SAYt]  # Calculate vulnerable biomass
      
      # --- An update year ----
      if (y %in% upyrs) {
        # --- Update Data object ----
        MSElist[[mm]] <- updateData(Data=MSElist[[mm]], OM, MPCalcs, Effort, StockPars$Biomass, StockPars$N,
                                    Biomass_P, CB_Pret, N_P, StockPars$SSB, SSB_P, StockPars$VBiomass, VBiomass_P,
                                    RefPoints=ReferencePoints, 
                                    retA_P, retL_P, StockPars,
                                    FleetPars, ObsPars, V_P,
                                    upyrs, interval, y, mm,
                                    Misc=Data_p@Misc, RealData, ObsPars$Sample_Area,
                                    AddIunits, AddIndType)
        
        # Update Abundance and FMSY for FMSYref MPs
        # TODO
        M_array <- array(0.5*StockPars$M_ageArray[,,nyears+y], dim=c(nsim, n_age, nareas))
        Atemp <- apply(VBiomass_P[, , y, ] * exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
        MSElist[[mm]]@OM$A <- Atemp
        MSElist[[mm]]@OM$FMSY <- FMSY_y[,mm,y+OM@nyears]
        
        # --- apply MP ----
        runMP <- applyMP(Data=MSElist[[mm]], MPs = MPs[mm], reps = reps, silent=TRUE)  # Apply MP
        MPRecs <- runMP[[1]][[1]] # MP recommendations
        Data_p <- runMP[[2]] # Data object object with saved info from MP
        Data_p@TAC <- MPRecs$TAC
        # calculate pstar quantile of TAC recommendation dist
        
        
        TACused <- apply(Data_p@TAC, 2, quantile, p = pstar, na.rm = T)
        if (length(MPRecs$TAC) >0) {
          # a TAC has been recommended
          checkNA[y] <- sum(is.na(TACused))
          TACused[is.na(TACused)] <- LastTAC[is.na(TACused)] # set to last yr TAC if NA
          TACused[TACused<tiny] <- tiny
          TACa[, mm, y] <- TACused # recommended TAC
        }
        
        # -- Calc stock dynamics ----
        MPCalcs <- CalcMPDynamics(MPRecs, y, nyears, proyears, nsim, Biomass_P,
                                  VBiomass_P, LastTAE, histTAE, LastSpatial, LastAllocat,
                                  LastTAC, TACused, maxF,LR5_P, LFR_P, Rmaxlen_P,
                                  retL_P, retA_P, L5_P, LFS_P, Vmaxlen_P,
                                  SLarray_P, V_P, Fdisc_P, DR_P, FM_P,
                                  FM_Pret, Z_P, CB_P, CB_Pret, Effort_pot,
                                  StockPars, FleetPars, ImpPars)
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
        retL_P <- MPCalcs$retL_P # retained-at-length
        V_P <- MPCalcs$V_P  # vulnerable-at-age
        SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length
        
        LR5_P <- MPCalcs$LR5_P
        LFR_P <- MPCalcs$LFR_P
        Rmaxlen_P <- MPCalcs$Rmaxlen_P
        L5_P <- MPCalcs$L5_P
        LFS_P <- MPCalcs$LFS_P
        Vmaxlen_P <- MPCalcs$Vmaxlen_P
        Fdisc_P <- MPCalcs$Fdisc_P
        DR_P <- MPCalcs$DR_P
        
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
      } else {
        # --- Not an update yr ----
        NoMPRecs <- MPRecs # TAC & TAE stay the same
        NoMPRecs[lapply(NoMPRecs, length) > 0 ] <- NULL
        NoMPRecs$Spatial <- NA
        MPCalcs <- CalcMPDynamics(NoMPRecs, y, nyears, proyears, nsim, Biomass_P,
                                  VBiomass_P, LastTAE, histTAE, LastSpatial, LastAllocat,
                                  LastTAC, TACused, maxF,LR5_P, LFR_P, Rmaxlen_P,
                                  retL_P, retA_P, L5_P, LFS_P, Vmaxlen_P,
                                  SLarray_P, V_P, Fdisc_P, DR_P, FM_P,
                                  FM_Pret, Z_P, CB_P, CB_Pret, Effort_pot,
                                  StockPars, FleetPars, ImpPars)
        
        TACa[, mm, y] <- TACused #
        LastSpatial <- MPCalcs$Si
        LastAllocat <- MPCalcs$Ai
        LastTAE <- MPCalcs$TAE
        Effort[, mm, y] <- MPCalcs$Effort
        CB_P <- MPCalcs$CB_P # removals
        CB_Pret <- MPCalcs$CB_Pret # retained catch
        FMa[,mm,y] <- MPCalcs$Ftot
        LastTAC <- TACa[, mm, y]  # apply(CB_Pret[,,y,], 1, sum, na.rm=TRUE)
        FM_P <- MPCalcs$FM_P # fishing mortality
        FM_Pret <- MPCalcs$FM_Pret # retained fishing mortality
        Z_P <- MPCalcs$Z_P # total mortality
        retA_P <- MPCalcs$retA_P # retained-at-age
        retL_P <- MPCalcs$retL_P # retained-at-length
        V_P <- MPCalcs$V_P  # vulnerable-at-age
        SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length
        
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
      } # end of update loop
      
    }  # end of year loop
    if (max(upyrs) < proyears) { # One more call to complete Data object
      MSElist[[mm]] <- updateData(Data=MSElist[[mm]], OM, MPCalcs, Effort, StockPars$Biomass, StockPars$N,
                                  Biomass_P, CB_Pret, N_P, StockPars$SSB, SSB_P, StockPars$VBiomass, VBiomass_P,
                                  RefPoints=ReferencePoints, 
                                  retA_P, retL_P, StockPars,
                                  FleetPars, ObsPars, V_P,
                                  upyrs=c(upyrs, proyears), interval, y, mm,
                                  Misc=Data_p@Misc, RealData, ObsPars$Sample_Area,
                                  AddIunits, AddIndType)
    }
    
    B_BMSYa[, mm, ] <- apply(SSB_P, c(1, 3), sum, na.rm=TRUE)/SSBMSY_y[,mm,(OM@nyears+1):(OM@nyears+OM@proyears)]  # SSB relative to SSBMSY
    F_FMSYa[, mm, ] <- FMa[, mm, ]/FMSY_y[,mm,(OM@nyears+1):(OM@nyears+OM@proyears)]
    
    Ba[, mm, ] <- apply(Biomass_P, c(1, 3), sum, na.rm=TRUE) # biomass
    SSBa[, mm, ] <- apply(SSB_P, c(1, 3), sum, na.rm=TRUE) # spawning stock biomass
    VBa[, mm, ] <- apply(VBiomass_P, c(1, 3), sum, na.rm=TRUE) # vulnerable biomass
    
    Ca[, mm, ] <- apply(CB_P, c(1, 3), sum, na.rm=TRUE) # removed
    CaRet[, mm, ] <- apply(CB_Pret, c(1, 3), sum, na.rm=TRUE) # retained catch
    
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
    
    if("progress"%in%names(control))
      if(control$progress) {
        if (requireNamespace("shiny", quietly = TRUE)) {
          shiny::incProgress(1/nMP, detail = round(mm*100/nMP))
        } else {
          warning('package `shiny` needs to be installed for progress bar')
        }
      }
  }
  
  # Miscellaneous reporting
  Misc <- list()
  Misc$Data <- MSElist
  
  # Report profit margin and latent effort
  Misc$LatEffort <- LatEffort_out
  Misc$Revenue <- Rev_out
  Misc$Cost <- Cost_out
  Misc$TAE <- TAE_out
  Misc$Removals <- Ca # total removals
  
  ## Create MSE Object #### 
  MSEout <- new("MSE", Name = OM@Name, nyears, proyears, nMPs=nMP, MPs, nsim, 
                Data@OM, Obs=Data@Obs, B_BMSY=B_BMSYa, F_FMSY=F_FMSYa, B=Ba, 
                SSB=SSBa, VB=VBa, FM=FMa, CaRet, TAC=TACa, SSB_hist = StockPars$SSB, 
                CB_hist = StockPars$CBret, FM_hist = StockPars$FM, Effort = Effort, PAA=array(), 
                CAA=array(), CAL=array(), CALbins=numeric(), Misc = Misc)
  # Store MSE info
  attr(MSEout, "version") <- packageVersion("OMtool")
  attr(MSEout, "date") <- date()
  attr(MSEout, "R.version") <- R.version
  
  MSEout 
  
}

