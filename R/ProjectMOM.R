
#' Run Forward Projections
#' 
#' @param multiHist An Historical Simulation object (class `multiHist`)
#' @param MPs A vector of methods (character string) of class MP
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical. Should the MSE be run using parallel processing?
#' 
#' @return An object of class `multiHist` from `SimulateMOM`
#' @export
ProjectMOM <- function (multiHist=NULL, MPs=NA, parallel=FALSE, silent=FALSE) {
  
  # ---- Setup ----
  if (class(multiHist) !='multiHist')
    stop('Must provide an object of class `multiHist`')
  
  # Unpack historical simulation data
  MOM <- multiHist[[1]][[1]]@Misc$MOM
  set.seed(MOM@seed) # set seed for reproducibility 
  nsim <- MOM@nsim # number of simulations
  nyears <- MOM@Fleets[[1]][[1]]@nyears # number of historical years
  proyears <- MOM@proyears # number of projection years
  interval <- MOM@interval # management interval (annual)
  maxF <- MOM@maxF # maximum apical F
  pstar <- MOM@pstar # Percentile of the sample of the TAC for each MP 
  reps <- MOM@reps # Number of samples of the management recommendation for each MP
  
  allyears <- nyears+proyears
  Stocks <- MOM@Stocks
  Fleets <- MOM@Fleets
  Obs <- MOM@Obs
  Imps <- MOM@Imps
  Rel <- MOM@Rel
  SexPars <- MOM@SexPars
  Complexes <- MOM@Complexes
  CatchFrac <- MOM@CatchFrac
  
  control <- MOM@cpars$control; MOM@cpars$control <- NULL
  
  np <- length(Stocks)
  nf <- length(Fleets[[1]])
  
  # ---- Detect MP Specification ----
  MPcond <- "unknown"
  if(np==1&nf==1){
    if (!silent)
      message("runMSE checking: you have specified a single stock and fleet. ",
              "For analysis you should be using runMSE(). Use this only for debugging ",
              "against runMSE.")
    if (class(MPs) !="character") {
      stop('`MPs` must be specified as a vector of character names if there is only', 
           ' 1 stock and 1 fleet. `MPs` is currently a ', class(MPs))
    }
    MPcond <- "complex"
    nMP <- length(MPs)
    MPrefs <- array(NA,c(nMP,nf,np))
    MPrefs[] <- unlist(MPs)
    
    # check class of MPs 
    tt <- try(sapply(MPs, get), silent=TRUE)
    if (class(tt) == 'try-error') 
      stop('Error in the MPs -', strsplit(tt,':')[[1]][2])
    MP_classes <- sapply(tt, class)
    if (!all(MP_classes == MP_classes[1]))
      stop('All MPs must be same class (`MP`)')
  }
  
  if(class(MPs) == 'character' & MPcond=='unknown') {
    # check class of MPs 
    tt <- try(sapply(MPs, get), silent=TRUE)
    if (class(tt) == 'try-error') 
      stop('Error in the MPs -', strsplit(tt,':')[[1]][2])
    MP_classes <- sapply(tt, class)
    if (!all(MP_classes == MP_classes[1]))
      stop('All MPs must be same class (`MP` or `MMP`)')
    MP_class <- unique(MP_classes)
    
    if(MP_class=="MMP"){
      message("MMP mode: you have specified multi-fleet, multi-stock MPs of ",
              "class MMP. This class of MP accepts all data objects (stocks x fleets) ",
              "to simultaneously make a recommendation specific to each stock and fleet")
      MPcond <- "MMP"
      nMP <- length(MPs)
      MPrefs <- array(NA,c(nMP,nf,np))
      MPrefs[] <- MPs
    }
    if(MP_class=="MP"){
      message("Complex mode: you have specified a vector of MPs rather than a ",
              "list of MPs, one list position for MP type. The same MP will ",
              "be applied to the aggregate data for all stocks and fleets. ",
              "The MP will, for example, be used to set a single TAC for all ",
              "stocks and fleets combined. This will be allocated among fleets ",
              "according to recent catches and among stocks according to ",
              "available, vulnerable biomass")
      MPcond <- "complex"
      MPtemp <- MPs
      nMP <- length(MPs)
      MPrefs <- array(NA,c(nMP,nf,np))
      MPrefs[] <- unlist(MPs)
    }
  }
  
  if (class(MPs) == 'list' & MPcond=='unknown') {
    
    if(identical(ldim(MPs), ldim(Fleets))){
      message("Byfleet mode: you have specified an MP for each stock and fleet. ",
              "Only fleet-specific data (e.g. catches and indices) will be used to set ",
              "advice for each fleet for each stock")
    }
    MPcond <- "byfleet"
    nMP <- length(MPs[[1]][[1]])
    MPrefs <- array(NA,c(nMP,nf,np))
    MPrefs[]<-unlist(MPs)
  } else if (ldim(MPs)==ldim(Fleets)[1]){ # not a two-tier list
    message("Bystock mode: you have specified a vector of MPs for each stock, ",
            "but not a vector of MPs for each stock and fleet. The catch data for these",
            " fleets will be combined, a single MP will be used to set a single TAC ",
            "for all fleets combined that will be allocated between the fleets ",
            "according to recent catches")
    MPcond<-"bystock"
    checkN <- unlist(lapply(MPs, length))
    if (!all(checkN == checkN[1]))
      stop('Must have the same number of MPs for each stock')
    nMP<-length(MPs[[1]])
    MPrefs<-array(NA,c(nMP,nf,np))
    for(p in 1:np)MPrefs[,,p]<-MPs[[p]]
  }
 
  if (MPcond == 'unknown')
    stop('`MPs` is not a vector or list with correct dimensions. See `?multiMSE`')
  

  if(class(MPs)=="list"){
    allMPs<-unlist(MPs)
  }else{
    allMPs<-MPs
  }
  
  if (nMP < 1) stop("No valid MPs found", call.=FALSE)
  
  # ---- Check MPs ----
  # Not currently included - TODO
  # if (CheckMPs & MPcond != "MMP") {
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
  # 

  # ---- Set Management Interval for each MP ----
  # TODO - make same structure as MPs argument
  if (length(interval) != nMP) interval <- rep(interval, nMP)[1:nMP]
  if (!all(interval == interval[1])) {
    if (!silent) message("Variable management intervals:")
    df <- data.frame(MP=MPs,interval=interval)
    for (i in 1:nrow(df)) {
      message(df$MP[i], 'has management interval:', df$interval[i])
    }
  }
  
  # --- Store MSY statistics for each projection year ----
  MSY_y <- FMSY_y <- SSBMSY_y <- BMSY_y <- VBMSY_y <- array(NA, dim=c(nsim,np, nMP, nyears+proyears))
  # MSY stats from historical simulations
  for (p in 1:np) {
    MSY_y[,p,,] <- aperm(replicate(nMP, multiHist[[p]][[1]]@Ref$ByYear$MSY), c(1,3,2))
    FMSY_y[,p,,] <- aperm(replicate(nMP, multiHist[[p]][[1]]@Ref$ByYear$FMSY), c(1,3,2))
    SSBMSY_y[,p,,] <- aperm(replicate(nMP, multiHist[[p]][[1]]@Ref$ByYear$SSBMSY), c(1,3,2))
    BMSY_y[,p,,] <- aperm(replicate(nMP, multiHist[[p]][[1]]@Ref$ByYear$BMSY), c(1,3,2))
    VBMSY_y[,p,,] <- aperm(replicate(nMP, multiHist[[p]][[1]]@Ref$ByYear$VBMSY), c(1,3,2))
  }
  
  # ---- Set-up arrays and objects for projections ----
  # create a data object for each method 
  # (they have identical historical data and branch in projected years)
  MSElist <- list('list')
  for(p in 1:np){
    MSElist[[p]] <- new('list')
    for(f in 1:nf){
      Data <- multiHist[[p]][[f]]@Data # Historical data for this stock and fleet
      MSElist[[p]][[f]] <- list(Data)[rep(1, nMP)]
    }
  }
  # TODO - update names of stored values
  B_BMSYa <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected B_BMSY
  Ba <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected Biomass
  SSBa <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected SSB
  VBa <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected vulnerable biomass
  
  FMa <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected fishing mortality rate
  F_FMSYa <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected F_FMSY
  Ca <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected removed catch
  CaRet <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected retained catch
  TACa <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected TAC recommendation
  Effort <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the Effort
 
  # ---- Grab Stock, Fleet, Obs and Imp values from Hist ----
  StockPars <- FleetPars <- ObsPars <- ImpPars <- list()
  for(p in 1:np){
    FleetPars[[p]] <- ObsPars[[p]] <- ImpPars[[p]] <- new('list')
    for(f in 1:nf){
      StockPars[[p]] <- multiHist[[p]][[1]]@SampPars$Stock
      FleetPars[[p]][[f]] <- multiHist[[p]][[f]]@SampPars$Fleet
      ObsPars[[p]][[f]] <- multiHist[[p]][[f]]@SampPars$Obs
      ImpPars[[p]][[f]] <- multiHist[[p]][[f]]@SampPars$Imp
    }
  }
  
  nareas <- StockPars[[1]]$nareas
  maxage <- StockPars[[1]]$maxage
  n_age <- maxage + 1 
  plusgroup <- multiHist[[1]][[1]]@SampPars$Stock$plusgroup
  
  # ---- Grab Historical N-at-age etc ----
  N <- array(NA, dim=c(nsim, np, n_age, nyears, nareas))
  Biomass <- SSB <- VBiomass <- N  
  
  
  FM <- FMret <- array(NA, dim=c(nsim, np, nf, n_age, nyears, nareas))
  VF<- array(NA, dim=c(nsim, np, nf, n_age, nyears+proyears))
  CB <- CB_ret <- FM
  MPA <- array(NA, dim=c(np, nf, nyears+proyears, nareas))
                    
  for (p in 1:np) {
    N[,p,,,] <- multiHist[[p]][[1]]@AtAge$Number
    Biomass[,p,,,] <- multiHist[[p]][[1]]@AtAge$Biomass
    SSB[,p,,,] <- multiHist[[p]][[1]]@AtAge$SBiomass
    VBiomass[,p,,,] <- multiHist[[p]][[1]]@AtAge$VBiomass
    
    for (f in 1:nf) {
      FM[,p,f,,,] <- multiHist[[p]][[f]]@AtAge$F.Mortality
      FMret[,p,f,,,] <- multiHist[[p]][[f]]@AtAge$Fret.Mortality
      VF[,p,f,,] <- FleetPars[[p]][[f]]$V
      MPA[p,f,,] <- FleetPars[[p]][[f]]$MPA
      CB[,p,f,,,] <- multiHist[[p]][[f]]@AtAge$Removals
      CB_ret[,p,f,,,] <- multiHist[[p]][[f]]@AtAge$Landings
    }
  }
  # need to make a copy because R is doing weird things with elements with similar names
  HistFleetPars <- FleetPars 
  Snames <- SIL(Stocks,"Name")
  Fnames <- matrix(make.unique(SIL(Fleets,"Name")),nrow=nf)
  
  # ---- Begin loop over MPs ----
  mm <- 1 # for debugging
  
  TAC_A <- array(NA,c(nsim,np,nf)) # Temporary store of the TAC
  TAE_A <- array(NA,c(nsim,np,nf)) # Temporary store of the TAE
  MPrecs_A_blank<-list() # Temporary Hierarchical list of MPrec objects
  for(p in 1:np) MPrecs_A_blank[[p]]<-list()
  LastTAE <- histTAE <- Effort_pot <-LastAllocat <-LastCatch <-TACused <-
    array(NA,c(nsim,np,nf))
  LastSpatial <- array(NA,c(nareas,np,nf,nsim))
  # temporary vulnerability for MSY calcs combined over fleets
  V_Pt <- array(NA,c(nsim,nf,n_age,nyears+proyears))
  
  for (mm in 1:nMP) {
    if(!silent){
      message(" ----- ", mm, "/", nMP, " MPs, Running MSE for: ")  # print a progress report
      for(p in 1:np){
        MPrep<-data.frame(MPrefs[mm,,p])
        row.names(MPrep)<-Fnames[,p]
        names(MPrep)=Snames[p]
        print(MPrep)
      }
      message(" --------------------------------- ")
    }
    
    checkNA <- array(0,c(np,nf,proyears)) # save number of NAs
    
    # reset selectivity & retention parameters for projections
    for(p in 1:np){
      for(f in 1:nf){
        # reset selectivity parameters for projections
        FleetPars[[p]][[f]]$L5_P <- HistFleetPars[[p]][[f]]$L5
        FleetPars[[p]][[f]]$LFS_P <- HistFleetPars[[p]][[f]]$LFS
        FleetPars[[p]][[f]]$Vmaxlen_P <- HistFleetPars[[p]][[f]]$Vmaxlen
        # selectivity at length array - projections
        FleetPars[[p]][[f]]$SLarray_P <- HistFleetPars[[p]][[f]]$SLarray
        #  selectivity at age array - projections
        FleetPars[[p]][[f]]$V_P <- HistFleetPars[[p]][[f]]$V
        
        # reset retention parameters for projections
        FleetPars[[p]][[f]]$LR5_P <- HistFleetPars[[p]][[f]]$LR5
        FleetPars[[p]][[f]]$LFR_P <- HistFleetPars[[p]][[f]]$LFR
        FleetPars[[p]][[f]]$Rmaxlen_P <- HistFleetPars[[p]][[f]]$Rmaxlen
        # retention at age array - projections
        FleetPars[[p]][[f]]$retA_P <- HistFleetPars[[p]][[f]]$retA
        # retention at length array - projections
        FleetPars[[p]][[f]]$retL_P <- HistFleetPars[[p]][[f]]$retL
        # Discard ratio for projections
        FleetPars[[p]][[f]]$DR_P <- HistFleetPars[[p]][[f]]$DR
        
        FleetPars[[p]][[f]]$FM_P <- array(NA,
                                          dim = c(nsim, n_age, proyears, nareas))
        FleetPars[[p]][[f]]$FM_Pret <- array(NA,
                                             dim = c(nsim, n_age, proyears, nareas))
        # stores prospective F before reallocation to new areas
        FleetPars[[p]][[f]]$FM_nospace <- array(NA,
                                                dim = c(nsim, n_age,
                                                        proyears, nareas))
        # last apical F
        FleetPars[[p]][[f]]$FML <- array(NA, dim = c(nsim, nareas))
        FleetPars[[p]][[f]]$Z_P <- array(NA,
                                         dim = c(nsim, n_age, proyears, nareas))
        FleetPars[[p]][[f]]$CB_P <- array(NA,
                                          dim = c(nsim,n_age, proyears, nareas))
        # retained catch
        FleetPars[[p]][[f]]$CB_Pret <- array(NA,
                                             dim = c(nsim,n_age, proyears, nareas))
        
      }
      # Discard mortality for projections
      StockPars[[p]]$Fdisc_P <- StockPars[[p]]$Fdisc
      StockPars[[p]]$N_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
      StockPars[[p]]$Biomass_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
      StockPars[[p]]$VBiomass_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
      StockPars[[p]]$SSN_P <-array(NA, dim = c(nsim,n_age, proyears, nareas))
      StockPars[[p]]$SSB_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
    }
    
    # projection arrays
    N_P <- array(NA, dim = c(nsim, np, n_age, proyears, nareas))
    Biomass_P <- array(NA, dim = c(nsim,np, n_age, proyears, nareas))
    VBiomass_P <- array(NA, dim = c(nsim,np, n_age, proyears, nareas))
    SSN_P <-array(NA, dim = c(nsim,np, n_age, proyears, nareas))
    SSB_P <- array(NA, dim = c(nsim,np, n_age, proyears, nareas))
    FMt_P <- array(NA, dim = c(nsim, np, n_age, proyears, nareas))
    Z_P <- array(NA, dim = c(nsim, np, n_age, proyears, nareas))
    FM_P <- array(NA, dim = c(nsim, np,nf,n_age, proyears, nareas))
    FMret_P <- array(NA, dim = c(nsim,np,nf, n_age, proyears, nareas))
    VBF_P<-array(NA, dim = c(nsim,np,nf, n_age, proyears, nareas))
    
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
    
    Perr<-hs<-R0<-SRrel<-K<-Linf<-t0<-M<-array(NA,c(nsim,np))
    aR<-bR<-R0a<-SSBpR<-Asize<-array(NA,c(nsim,np,nareas))
    mov<-array(NA,c(nsim,np,n_age,nareas,nareas,nyears+proyears))
    Spat_targ_y<-array(NA,c(nsim,np,nf))
    M_agecur_y<-Mat_agecur_y<-array(NA,c(nsim,np,n_age))
    a_y<-b_y<-rep(NA,np)
    
    for(p in 1:np){
      Perr[,p]<-StockPars[[p]]$Perr_y[,nyears+n_age-1]
      hs[,p]<-StockPars[[p]]$hs
      aR[,p,]<-StockPars[[p]]$aR
      bR[,p,]<-StockPars[[p]]$bR
      mov[,p,,,,]<-StockPars[[p]]$mov
      for(f in 1:nf)Spat_targ_y[,p,f]<-FleetPars[[p]][[f]]$Spat_targ
      SRrel[,p]<-StockPars[[p]]$SRrel
      M_agecur_y[,p,]<-StockPars[[p]]$M_ageArray[,,nyears]
      Mat_agecur_y[,p,]<-StockPars[[p]]$Mat_age[,,nyears]
      K[,p]<-StockPars[[p]]$Karray[,nyears]
      Linf[,p]<-StockPars[[p]]$Linfarray[,nyears]
      t0[,p]<-StockPars[[p]]$t0array[,nyears]
      M[,p]<-StockPars[[p]]$M
      R0[,p]<-StockPars[[p]]$R0
      R0a[,p,]<-StockPars[[p]]$R0a
      SSBpR[,p,]<-StockPars[[p]]$SSBpR
      a_y[p]<-StockPars[[p]]$a
      b_y[p]<-StockPars[[p]]$b
      Asize[,p,]<-StockPars[[p]]$Asize
    }
    
    # note that Fcur is apical F but, in popdynOneMICE it is DIVIDED in future
    # years between the two areas depending on vulnerable biomass.
    # So to get Fcur you need to sum over areas (a bit weird)
    NextYrN <- sapply(1:nsim, function(x)
      popdynOneMICE(np,nf,nareas, maxage,
                    Ncur=array(N[x,,,nyears,],
                               c(np,n_age,nareas)),
                    Vcur=array(VF[x,,,,nyears],c(np,nf,n_age)),
                    FMretx=array(FMret[x,,,,nyears,],c(np,nf,n_age,nareas)),
                    FMx=array(FM[x,,,,nyears,],c(np,nf,n_age,nareas)),
                    PerrYrp=Perr[x,], hsx=hs[x,], aRx=matrix(aR[x,,],nrow=np),
                    bRx=matrix(bR[x,,],nrow=np),
                    movy=array(mov[x,,,,,nyears],c(np,n_age,nareas,nareas)),
                    Spat_targ=array(Spat_targ_y[x,,],c(np,nf)), SRrelx=SRrel[x,],
                    M_agecur=matrix(M_agecur_y[x,,],nrow=np),
                    Mat_agecur=matrix(Mat_agecur_y[x,,],nrow=np),
                    Asizex=matrix(Asize[x,,],ncol=nareas),Kx =K[x,],
                    Linfx=Linf[x,],t0x=t0[x,],Mx=M[x,],
                    R0x=R0[x,],R0ax=matrix(R0a[x,,],nrow=np),
                    SSBpRx=matrix(SSBpR[x,,],nrow=np),ax=a_y,
                    bx=b_y,Rel=Rel,SexPars=SexPars,x=x,
                    plusgroup=plusgroup))
    
    # TODO - make sure above is using correct weight-at-age etc for last historical year 
    
    N_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[1,], use.names=FALSE)),
                              dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
    
    Biomass_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[23,],
                                                      use.names=FALSE)),
                                    dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
    SSN_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[24,],
                                                  use.names=FALSE)),
                                dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
    SSB_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[25,], use.names=FALSE)),
                                dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
    VBiomass_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[19,],
                                                       use.names=FALSE)),
                                     dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
    FML <- apply(array(FM[, ,,, nyears, ],c(nsim,np,nf,n_age,nareas)),
                 c(1, 3), max)
    
    for(p in 1:np) {
      StockPars[[p]]$N_P<-N_P[,p,,,]
      StockPars[[p]]$Biomass_P<-Biomass_P[,p,,,]
      StockPars[[p]]$SSN_P<-SSN_P[,p,,,]
      StockPars[[p]]$SSB_P<-SSB_P[,p,,,]
      StockPars[[p]]$VBiomass_P<-VBiomass_P[,p,,,]
    }
    
    # ---- Update true abundance ----
    # - used for FMSY ref methods so that FMSY is applied to current abundance
    for (p in 1:np) {
      for (f in 1:nf) {
        M_array <- array(0.5*StockPars[[p]]$M_ageArray[,,nyears+y],
                         dim=c(nsim, n_age, nareas))
        Atemp <- apply(StockPars[[p]]$VBiomass_P[, , y, ] *
                         exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
        
        MSElist[[p]][[f]][[mm]]@OM$A <- Atemp
      } # end fleets
    } # end stocks
    
    # --- Apply MP in initial projection year ----
    # - Combined MP -
    if(MPcond=="MMP"){
      # returns a hierarchical list object stock then fleet of Data objects
      DataList<-getDataList(MSElist,mm)
      # returns a hierarchical list object stock then fleet then slot type of Rec
      MPRecs_A <- applyMMP(DataList, MP = MPs[mm], reps = 1, silent=TRUE)
      Data_p_A <- MPrecs_A_blank
      for(p in 1:np)for(f in 1:nf){
        Data_p_A[[p]][[f]]<-MSElist[[p]][[f]][[mm]]
        Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC # record TAC rec in Data
      }
      
    }else if(MPcond=="complex"){
      # A temporary blank hierarchical list object stock by fleet
      MPRecs_A <- Data_p_A <- MPrecs_A_blank
      # need this for aggregating data and distributing TACs over stocks
      realVB<-apply(VBiomass[,,,1:nyears,, drop=FALSE],c(1,2,4),sum,na.rm=T)
      
      curdat<-multiDataS(MSElist,StockPars,np,mm,nf,realVB)
      runMP <- applyMP(curdat, MPs = MPs[mm], reps = 1, silent=TRUE)  # Apply MP
      
      Stock_Alloc<-realVB[,,nyears, drop=FALSE]/apply(realVB[,,nyears, drop=FALSE],1,sum)
      
      for(p in 1:np)  for(f in 1:nf){
        MPRecs_A[[p]][[f]]<-runMP[[1]][[1]]
        MPRecs_A[[p]][[f]]$TAC<-runMP[[1]][[1]]$TAC*MOM@Allocation[[p]][,f]*
          Stock_Alloc[,p,1]
        MPRecs_A[[p]][[f]]$Effort<-runMP[[1]][[1]]$Effort*MOM@Efactor[[p]][,f]
        
        if(length(MPRecs_A[[p]][[f]]$Effort)>0)
          if(is.na(MPRecs_A[[p]][[f]]$Effort[1,1]))
            MPRecs_A[[p]][[f]]$Effort <- matrix(NA,
                                                nrow=0,
                                                ncol=ncol(MPRecs_A[[p]][[f]]$Effort))
        if(length(MPRecs_A[[p]][[f]]$TAC)>0)
          if(is.na(MPRecs_A[[p]][[f]]$TAC[1,1]))
            MPRecs_A[[p]][[f]]$TAC <- matrix(NA,
                                             nrow=0,
                                             ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
        if(is.na(MPRecs_A[[p]][[f]]$Spatial[1,1]))
          MPRecs_A[[p]][[f]]$Spatial <- matrix(NA,
                                               nrow=0,
                                               ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
        
        Data_p_A[[p]][[f]]<-runMP[[2]]
        Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC
      }
    }else{
      # A temporary blank hierarchical list object stock by fleet
      MPRecs_A <- Data_p_A <- MPrecs_A_blank
      for(p in 1:np){
        if(MPcond=="bystock"){
          if(nf>1){
            curdat<-multiData(MSElist,StockPars,p,mm,nf)
          }else{
            curdat<-MSElist[[p]][[f]][[mm]]
          }
          runMP <- DLMtool::applyMP(curdat, MPs = MPs[[p]][mm], reps = 1,
                                    silent=TRUE)  # Apply MP
          
          # Do allocation calcs
          TAC_A[,p,] <- array(as.vector(unlist(runMP[[1]][[1]]$TAC))*
                                MOM@Allocation[[p]],c(nsim,nf))
          TAE_A[,p,] <- array(as.vector(unlist(runMP[[1]][[1]]$Effort))*
                                MOM@Efactor[[p]],c(nsim,nf))
          
          for(f in 1:nf){
            MPRecs_A[[p]][[f]]<-runMP[[1]][[1]]
            MPRecs_A[[p]][[f]]$TAC<-matrix(TAC_A[,p,f],nrow=1) # copy allocated TAC
            MPRecs_A[[p]][[f]]$Effort<-matrix(TAE_A[,p,f],nrow=1)
            # This next line is to make the NULL effort recommendations of an
            # output control MP compatible with CalcMPdynamics (expects a null matrix)
            if(is.na(MPRecs_A[[p]][[f]]$Effort[1,1]))
              MPRecs_A[[p]][[f]]$Effort <- matrix(NA,
                                                  nrow=0,
                                                  ncol=ncol(MPRecs_A[[p]][[f]]$Effort))
            if(is.na(MPRecs_A[[p]][[f]]$TAC[1,1]))
              MPRecs_A[[p]][[f]]$TAC<-matrix(NA,
                                             nrow=0,
                                             ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
            if(is.na(MPRecs_A[[p]][[f]]$Spatial[1,1]))
              MPRecs_A[[p]][[f]]$Spatial<-matrix(NA,
                                                 nrow=0,
                                                 ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
            
            Data_p_A[[p]][[f]]<-runMP[[2]]
            Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC   # copy allocated tAC
          }
        }else if(MPcond=="byfleet"){
          for(f in 1:nf){
            curdat<-MSElist[[p]][[f]][[mm]]
            runMP <- DLMtool::applyMP(curdat, MPs = MPrefs[mm,f,p], reps = 1, silent=TRUE)  # Apply MP
            MPRecs_A[[p]][[f]]<-runMP[[1]][[1]]
            Data_p_A[[p]][[f]]<-runMP[[2]]
            Data_p_A[[p]][[f]]@TAC <- MPRecs_A[[p]][[f]]$TAC
          }
        }
      } # end of stocks
    }
    
    MPCalcs_list <- vector('list', np)
  
    for(p in 1:np) {
      MPCalcs_list[[p]] <- vector('list', nf)
      for(f in 1:nf) {
        TACused[,p,f] <- apply(Data_p_A[[p]][[f]]@TAC, 2, quantile,
                               p = MOM@pstar, na.rm = T)
        checkNA[p,f,y] <- sum(is.na(TACused[,p,f]))
        LastTAE[,p,f] <-  rep(NA, nsim) # no current TAE exists
        histTAE[,p,f] <- rep(NA, nsim) # no existing TAE
        LastSpatial[,p,f,] <- array(MPA[p,f,nyears,], dim=c(nareas, nsim)) #
        # default assumption of reallocation of effort to open areas
        LastAllocat[,p,f] <- rep(1, nsim)
        LastCatch[,p,f] <- apply(CB[,p,f,,nyears,], 1, sum)
        Effort_pot[,p,f] <- rep(NA, nsim) # No bio-economic model
        
        MPCalcs <- CalcMPDynamics(MPRecs=MPRecs_A[[p]][[f]], y, 
                                  nyears, proyears, nsim, 
                                  Biomass_P=StockPars[[p]]$Biomass_P,
                                  VBiomass_P=StockPars[[p]]$VBiomass_P, 
                                  LastTAE=LastTAE[,p,f], 
                                  histTAE=histTAE[,p,f], 
                                  LastSpatial=LastSpatial[,p,f,], 
                                  LastAllocat=LastAllocat[,p,f],
                                  LastTAC=LastCatch[,p,f], 
                                  TACused=TACused[,p,f], 
                                  maxF=maxF,
                                  LR5_P=FleetPars[[p]][[f]]$LR5_P,
                                  LFR_P=FleetPars[[p]][[f]]$LFR_P, 
                                  Rmaxlen_P=FleetPars[[p]][[f]]$Rmaxlen_P,
                                  retL_P=FleetPars[[p]][[f]]$retL_P,
                                  retA_P=FleetPars[[p]][[f]]$retA_P,
                                  L5_P=FleetPars[[p]][[f]]$L5_P, 
                                  LFS_P=FleetPars[[p]][[f]]$LFS_P,
                                  Vmaxlen_P=FleetPars[[p]][[f]]$Vmaxlen_P,
                                  SLarray_P=FleetPars[[p]][[f]]$SLarray_P,
                                  V_P=FleetPars[[p]][[f]]$V_P, 
                                  Fdisc_P=StockPars[[p]]$Fdisc_P,
                                  DR_P=FleetPars[[p]][[f]]$DR_P, 
                                  FM_P=FleetPars[[p]][[f]]$FM_P,
                                  FM_Pret=FleetPars[[p]][[f]]$FM_Pret,
                                  Z_P=FleetPars[[p]][[f]]$Z_P, 
                                  CB_P=FleetPars[[p]][[f]]$CB_P, 
                                  CB_Pret=FleetPars[[p]][[f]]$CB_Pret,
                                  Effort_pot=Effort_pot[,p,f],
                                  StockPars=StockPars[[p]],
                                  FleetPars=FleetPars[[p]][[f]],
                                  ImpPars=ImpPars[[p]][[f]])
        
      
        if(length(SexPars)>0) MPCalcs<- MPCalcsNAs(MPCalcs) # Zeros caused by SexPars
        
        TACa[,p,f, mm, y] <- TACused[,p,f]#MPCalcs$TACrec # recommended TAC
        LastSpatial[,p,f,] <- MPCalcs$Si
        LastAllocat[,p,f] <- MPCalcs$Ai
        
        LastTAE[,p,f] <- MPCalcs$TAE # TAE set by MP
        LastCatch[,p,f] <- MPCalcs$TACrec # TAC et by MP
        
        Effort[,p,f, mm, y] <- rep(MPCalcs$Effort,nsim)[1:nsim]
        FleetPars[[p]][[f]]$CB_P <- MPCalcs$CB_P # removals
        FleetPars[[p]][[f]]$CB_Pret <- MPCalcs$CB_Pret # retained catch
        FleetPars[[p]][[f]]$FM_P <- MPCalcs$FM_P # fishing mortality
        FM_P[,p,f,,,]<- MPCalcs$FM_P
        
        FleetPars[[p]][[f]]$FM_Pret <- MPCalcs$FM_Pret # retained fishing mortality
        FMret_P[,p,f,,,]<- MPCalcs$FM_Pret
        #FretA[,p,f,,]<- MPCalcs$FM_Pret
        FleetPars[[p]][[f]]$Z_P <- MPCalcs$Z_P # total mortality
        FleetPars[[p]][[f]]$retA_P <- MPCalcs$retA_P # retained-at-age
        
        FleetPars[[p]][[f]]$retL_P <- MPCalcs$retL_P # retained-at-length
        FleetPars[[p]][[f]]$V_P <- MPCalcs$V_P  # vulnerable-at-age
        VF[,p,f,,]<- MPCalcs$V_P
        FleetPars[[p]][[f]]$SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length
        
        MPCalcs_list[[p]][[f]] <- MPCalcs
      }
    }
    
    # the years in which there are updates
    upyrs <- 1 + (0:(floor(proyears/interval[mm]) - 1)) * interval[mm]
    if(!silent) {
      cat(".")
      flush.console()
    }
    
    # --- Begin projection years ----
    for (y in 2:proyears) {
      if(!silent) {
        cat(".")
        flush.console()
      }
      
      # -- Calculate MSY stats for this year ----
      # if selectivity has changed
      
      for (p in 1:np) {
        for (f in 1:nf) {
          SelectChanged <- FALSE
          if (any(
            FleetPars[[p]][[f]]$retA_P[,,nyears+y] - 
            FleetPars[[p]][[f]]$retA_P[,,nyears+y] !=0))  SelectChanged <- TRUE
          if (any(
            FleetPars[[p]][[f]]$V_P[,,nyears+y] - 
            FleetPars[[p]][[f]]$V[,,nyears+y] !=0))  SelectChanged <- TRUE
          
          if (SelectChanged) {
            # recalculate MSY ref points because selectivity has changed
            V_Pt[,f,,]<-FleetPars[[p]][[f]]$V_P*
              apply(CB[,p,f,,nyears,], 1, sum) # Weighted by catch frac
            #summed over fleets and normalized to 1
            V_P<-nlz(apply(V_Pt,c(1,3,4),sum),c(1,3),"max")
            y1 <- nyears + y
            MSYrefsYr <- sapply(1:nsim, optMSY_eq, 
                                StockPars[[p]]$M_ageArray, 
                                StockPars[[p]]$Wt_age,
                                StockPars[[p]]$Mat_age,
                                V_P,
                                StockPars[[p]]$maxage,
                                StockPars[[p]]$R0, 
                                StockPars[[p]]$SRrel,
                                StockPars[[p]]$hs,
                                yr.ind=y1, 
                                plusgroup=StockPars[[p]]$plusgroup[p])
            MSY_y[,p,mm,y1] <- MSYrefsYr[1,]
            FMSY_y[,p,mm,y1] <- MSYrefsYr[2,]
            SSBMSY_y[,p,mm,y1] <- MSYrefsYr[3,]
            BMSY_y[,p,mm,y1] <- MSYrefsYr[6,]
            VBMSY_y[,p,mm,y1] <- MSYrefsYr[7,]
          }
        }
      } # end of annual MSY
      
      TACa[,,, mm, y] <- TACa[,,, mm, y-1] # TAC same as last year unless changed
      
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
      
      for(p in 1:np){
        Perr[,p]<-StockPars[[p]]$Perr_y[,y+nyears+n_age-1]
        M_agecur_y[,p,]<-StockPars[[p]]$M_ageArray[,,nyears+y]
        Mat_agecur_y[,p,]<-StockPars[[p]]$Mat_age[,,nyears+y]
        K[,p]<-StockPars[[p]]$Karray[,nyears+y]
        Linf[,p]<-StockPars[[p]]$Linfarray[,nyears+y]
        t0[,p]<-StockPars[[p]]$t0array[,nyears+y]
      }
      
      # note that Fcur is apical F but, in popdynOneMICE it is DIVIDED in future
      # years between the two areas depending on vulnerabile biomass. So to get
      # Fcur you need to sum over areas (a bit weird)
      NextYrN <- sapply(1:nsim, function(x)
        popdynOneMICE(np,nf,nareas, maxage,
                      Ncur=array(N_P[x,,,y-1,],c(np,n_age,nareas)),
                      Vcur=array(VF[x,,,,nyears+y-1],c(np,nf,n_age)),
                      
                      FMretx=array(FMret_P[x,,,,y-1,],c(np,nf,n_age,nareas)),
                      FMx=array(FM_P[x,,,,y-1,],c(np,nf,n_age,nareas)),
                      PerrYrp=Perr[x,], hsx=hs[x,], aRx=matrix(aR[x,,],nrow=np),
                      bRx=matrix(bR[x,,],nrow=np),
                      movy=array(mov[x,,,,,nyears+y],c(np,n_age,nareas,nareas)),
                      Spat_targ=array(Spat_targ_y[x,,],c(np,nf)),
                      SRrelx=SRrel[x,],
                      M_agecur=matrix(M_agecur_y[x,,],nrow=np),
                      Mat_agecur=matrix(Mat_agecur_y[x,,],nrow=np),
                      Asizex=matrix(Asize[x,,],ncol=nareas), Kx =K[x,],
                      Linfx=Linf[x,],t0x=t0[x,],Mx=M[x,],
                      R0x=R0[x,],R0ax=matrix(R0a[x,,],nrow=np),
                      SSBpRx=matrix(SSBpR[x,,],nrow=np),ax=a_y,
                      bx=b_y,Rel=Rel,SexPars=SexPars,x=x,
                      plusgroup=plusgroup))
      
      N_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[1,], use.names=FALSE)),
                              dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      Biomass_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[23,],
                                                      use.names=FALSE)),
                                    dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      SSN_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[24,],
                                                  use.names=FALSE)),
                                dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      SSB_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[25,],
                                                  use.names=FALSE)),
                                dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      
      VBiomass_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[19,],
                                                       use.names=FALSE)),
                                     dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      FML <- apply(array(FM_P[, ,,, y-1, ],c(nsim,np,nf,n_age,nareas)),
                   c(1, 3), max)
      
      for(p in 1:np){
        StockPars[[p]]$N_P<-N_P[,p,,,]
        StockPars[[p]]$Biomass_P<-Biomass_P[,p,,,]
        StockPars[[p]]$SSN_P<-SSN_P[,p,,,]
        StockPars[[p]]$SSB_P<-SSB_P[,p,,,]
        StockPars[[p]]$VBiomass_P<-VBiomass_P[,p,,,]
        #for(f in 1:nf)FleetPars[[p]][[f]]$FML<-FML[]
      }
      
      # --- An update year ----
      if (y %in% upyrs) {
        # --- Update Data object ----
        for (p in 1:np) {
          for (f in 1:nf) {
            
            # TODO - remove this
            OM <- suppressMessages(new('OM')) # temporary while DLMtool::makeData requires this
            OM@nyears <- nyears
            OM@hbiascv <- MOM@Obs[[p]][[f]]@hbiascv
            OM@maxF <- MOM@maxF
            OM@CurrentYr <- nyears ## TODO - add currentyear to MOM
            OM@reps <- MOM@reps
            OM@nsim <- nsim
            OM@BMSY_B0biascv <- MOM@Obs[[p]][[f]]@BMSY_B0biascv
            OM@proyears <- proyears
            
            MSElist[[p]][[f]][[mm]] <- updateData(Data=MSElist[[p]][[f]][[mm]],
                                                  OM, 
                                                  MPCalcs=MPCalcs_list[[p]][[f]],
                                                  Effort=Effort[,p,f,, ,drop=FALSE],
                                                  Biomass=Biomass[,p,,,], 
                                                  N=N[,p,,,],
                                                  Biomass_P=Biomass_P[,p,,,], 
                                                  CB_Pret=FleetPars[[p]][[f]]$CB_Pret, 
                                                  N_P=N_P[,p,,,], 
                                                  SSB=SSB[,p,,,],
                                                  SSB_P=SSB_P[,p,,,],
                                                  VBiomass=VBiomass[,p,,,],
                                                  VBiomass_P=VBiomass_P[,p,,,],
                                                  RefPoints=StockPars[[p]]$ReferencePoints$ReferencePoints,
                                                  
                                                  retA_P=FleetPars[[p]][[f]]$retA, 
                                                  retL_P=FleetPars[[p]][[f]]$retL_P, 
                                                  StockPars=StockPars[[p]],
                                                  FleetPars=FleetPars[[p]][[f]], 
                                                  ObsPars=ObsPars[[p]][[f]], 
                                                  V_P=FleetPars[[p]][[f]]$V_P,
                                                  upyrs=upyrs,
                                                  interval=interval,
                                                  y=y,
                                                  mm=mm,
                                                  Misc=MSElist[[p]][[f]][[mm]]@Misc, 
                                                  RealData=NULL, #TODO 
                                                  Sample_Area=ObsPars[[p]][[f]]$Sample_Area,
                                                  AddIunits=NA,  # TODO
                                                  AddIndType=NA)
            
            # ---- Update true abundance ----
            M_array <- array(0.5*StockPars[[p]]$M_ageArray[,,nyears+y],
                             dim=c(nsim, n_age, nareas))
            Atemp <- apply(StockPars[[p]]$VBiomass_P[, , y, ] *
                             exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
            MSElist[[p]][[f]][[mm]]@OM$A <- Atemp
            
          } # end of fleet
        } # end of stock
        
        if(MPcond=="MMP"){
          # returns a hierarchical list object stock then fleet of Data objects
          DataList <- getDataList(MSElist,mm)
          # # returns a hierarchical list object stock then fleet then slot type of Rec
          MPRecs_A <- applyMMP(DataList, MP = MPs[mm], reps = 1, silent=TRUE)
          Data_p_A <- MPrecs_A_blank
          for(p in 1:np)for(f in 1:nf){
            Data_p_A[[p]][[f]]<-MSElist[[p]][[f]][[mm]]
            Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC # record TAC rec in Data
          }
        }else if(MPcond=="complex"){
          # A temporary blank hierarchical list object stock by fleet
          MPRecs_A <- Data_p_A <- MPrecs_A_blank
          # need this for aggregating data and distributing TACs over stocks
          realVB<-abind::abind(apply(VBiomass[,,,1:nyears,, drop=FALSE],c(1,2,4),sum,na.rm=T),
                               apply(VBiomass_P[,,,1:(y-1), , drop=FALSE],c(1,2,4),sum,na.rm=T),
                               along=3)
          
          
          curdat<-multiDataS(MSElist,StockPars,np,mm,nf,realVB)
          runMP <- DLMtool::applyMP(curdat, MPs = MPs[mm], reps = 1, silent=TRUE)  # Apply MP
          
          Stock_Alloc <- realVB[,,nyears, drop=FALSE]/
            apply(realVB[,,nyears, drop=FALSE],1,sum)
          
          for(p in 1:np)for(f in 1:nf){
            MPRecs_A[[p]][[f]] <- runMP[[1]][[1]]
            MPRecs_A[[p]][[f]]$TAC <- runMP[[1]][[1]]$TAC *
              MOM@Allocation[[p]][,f] * Stock_Alloc[,p,1]
            MPRecs_A[[p]][[f]]$Effort <- runMP[[1]][[1]]$Effort * MOM@Efactor[[p]][,f]
            
            if(length(MPRecs_A[[p]][[f]]$Effort)>0)
              if(is.na(MPRecs_A[[p]][[f]]$Effort[1,1]))
                MPRecs_A[[p]][[f]]$Effort <- matrix(NA,
                                                    nrow=0,
                                                    ncol=ncol(MPRecs_A[[p]][[f]]$Effort))
            if(length(MPRecs_A[[p]][[f]]$TAC)>0)
              if(is.na(MPRecs_A[[p]][[f]]$TAC[1,1]))
                MPRecs_A[[p]][[f]]$TAC <- matrix(NA,
                                                 nrow=0,
                                                 ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
            if(is.na(MPRecs_A[[p]][[f]]$Spatial[1,1]))
              MPRecs_A[[p]][[f]]$Spatial <- matrix(NA,
                                                   nrow=0,
                                                   ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
            
            Data_p_A[[p]][[f]]<-runMP[[2]]
            Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC
            
          }
        } else {
          # A temporary blank hierarchical list object stock by fleet
          MPRecs_A <- Data_p_A <- MPrecs_A_blank
          
          for(p in 1:np){
            
            if(MPcond=="bystock"){
              if(nf>1){
                curdat<-multiData(MSElist,StockPars,p,mm,nf)
              }else{
                curdat<-MSElist[[p]][[f]][[mm]]
              }
              runMP <- DLMtool::applyMP(curdat, MPs = MPs[[p]][mm],
                                        reps = MOM@reps, silent=TRUE)  # Apply MP
              # Do allocation calcs
              TAC_A[,p,]<-array(as.vector(unlist(runMP[[1]][[1]]$TAC))*MOM@Allocation[[p]],c(nsim,nf))
              TAE_A[,p,]<-array(as.vector(unlist(runMP[[1]][[1]]$Effort))*MOM@Efactor[[p]],c(nsim,nf))
              
              for(f in 1:nf){
                MPRecs_A[[p]][[f]]<-runMP[[1]][[1]]
                MPRecs_A[[p]][[f]]$TAC<-matrix(TAC_A[,p,f],nrow=1) # Just pass the allocated TAC
                MPRecs_A[[p]][[f]]$Effort<-matrix(TAE_A[,p,f],nrow=1)
                # This next line is to make the NULL effort recommendations of
                # an output control MP compatible with CalcMPdynamics (expects a null matrix)
                if(is.na(MPRecs_A[[p]][[f]]$Effort[1,1]))
                  MPRecs_A[[p]][[f]]$Effort<-matrix(NA,
                                                    nrow=0,
                                                    ncol=ncol(MPRecs_A[[p]][[f]]$Effort))
                if(is.na(MPRecs_A[[p]][[f]]$TAC[1,1]))
                  MPRecs_A[[p]][[f]]$TAC<-matrix(NA,
                                                 nrow=0,
                                                 ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
                if(is.na(MPRecs_A[[p]][[f]]$Spatial[1,1]))
                  MPRecs_A[[p]][[f]]$Spatial<-matrix(NA,
                                                     nrow=0,
                                                     ncol=ncol(MPRecs_A[[p]][[f]]$TAC))
                Data_p_A[[p]][[f]]<-runMP[[2]]
                Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC # Copy the allocated TAC
              }
            } else if(MPcond=="byfleet"){
              for(f in 1:nf){
                curdat<-MSElist[[p]][[f]][[mm]]
                runMP <- DLMtool::applyMP(curdat, MPs = MPrefs[mm,f,p],
                                          reps = MOM@reps, silent=TRUE)  # Apply MP
                MPRecs_A[[p]][[f]]<-runMP[[1]][[1]]
                Data_p_A[[p]][[f]]<-runMP[[2]]
                Data_p_A[[p]][[f]]@TAC <- MPRecs_A[[p]][[f]]$TAC
              }
            } # end of MPcond conditional
          } # end of stock loop
        } # end of MMP
        for(p in 1:np){
          for(f in 1:nf){
            # calculate pstar quantile of TAC recommendation dist
            TACused[,p,f] <- apply(Data_p_A[[p]][[f]]@TAC, 2, quantile,
                                   p = MOM@pstar, na.rm = T)
            checkNA[p,f,y] <-checkNA[p,f,y] + sum(is.na(TACused[,p,f]))
            
            MPCalcs <- CalcMPDynamics(MPRecs=MPRecs_A[[p]][[f]], y, 
                                      nyears, proyears, nsim, 
                                      Biomass_P=StockPars[[p]]$Biomass_P,
                                      VBiomass_P=StockPars[[p]]$VBiomass_P, 
                                      LastTAE=LastTAE[,p,f], 
                                      histTAE=histTAE[,p,f], 
                                      LastSpatial=LastSpatial[,p,f,], 
                                      LastAllocat=LastAllocat[,p,f],
                                      LastTAC=LastCatch[,p,f], 
                                      TACused=TACused[,p,f], 
                                      maxF=maxF,
                                      LR5_P=FleetPars[[p]][[f]]$LR5_P,
                                      LFR_P=FleetPars[[p]][[f]]$LFR_P, 
                                      Rmaxlen_P=FleetPars[[p]][[f]]$Rmaxlen_P,
                                      retL_P=FleetPars[[p]][[f]]$retL_P,
                                      retA_P=FleetPars[[p]][[f]]$retA_P,
                                      L5_P=FleetPars[[p]][[f]]$L5_P, 
                                      LFS_P=FleetPars[[p]][[f]]$LFS_P,
                                      Vmaxlen_P=FleetPars[[p]][[f]]$Vmaxlen_P,
                                      SLarray_P=FleetPars[[p]][[f]]$SLarray_P,
                                      V_P=FleetPars[[p]][[f]]$V_P, 
                                      Fdisc_P=StockPars[[p]]$Fdisc_P,
                                      DR_P=FleetPars[[p]][[f]]$DR_P, 
                                      FM_P=FleetPars[[p]][[f]]$FM_P,
                                      FM_Pret=FleetPars[[p]][[f]]$FM_Pret,
                                      Z_P=FleetPars[[p]][[f]]$Z_P, 
                                      CB_P=FleetPars[[p]][[f]]$CB_P, 
                                      CB_Pret=FleetPars[[p]][[f]]$CB_Pret,
                                      Effort_pot=Effort_pot[,p,f],
                                      StockPars=StockPars[[p]],
                                      FleetPars=FleetPars[[p]][[f]],
                                      ImpPars=ImpPars[[p]][[f]])
            # Zeros caused by SexPars
            if(length(SexPars)>0) MPCalcs<-MPCalcsNAs(MPCalcs)
            
            TACa[,p,f, mm, y] <- MPCalcs$TACrec # recommended TAC
            LastSpatial[,p,f,] <- MPCalcs$Si
            LastAllocat[,p,f] <- MPCalcs$Ai
            LastTAE[,p,f] <- MPCalcs$TAE # adjustment to TAE
            LastCatch[,p,f] <- MPCalcs$TACrec
            
            Effort[,p,f, mm, y] <- rep(MPCalcs$Effort,nsim)[1:nsim]
            FleetPars[[p]][[f]]$CB_P <- MPCalcs$CB_P # removals
            FleetPars[[p]][[f]]$CB_Pret <- MPCalcs$CB_Pret # retained catch
            FleetPars[[p]][[f]]$FM_P <- MPCalcs$FM_P # fishing mortality
            FM_P[,p,f,,,]<- MPCalcs$FM_P
            FleetPars[[p]][[f]]$FM_Pret <- MPCalcs$FM_Pret # retained fishing mortality
            FleetPars[[p]][[f]]$Z_P <- MPCalcs$Z_P # total mortality
            FleetPars[[p]][[f]]$retA_P <- MPCalcs$retA_P # retained-at-age
            
            FleetPars[[p]][[f]]$retL_P <- MPCalcs$retL_P # retained-at-length
            FleetPars[[p]][[f]]$V_P <- MPCalcs$V_P  # vulnerable-at-age
            VF[,p,f,,]<- MPCalcs$V_P
            FleetPars[[p]][[f]]$SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length
          } # end of fleets
        } # end of stocks
            
        # end of update year
      } else { 
        # ---- Not an update year ----
        for(p in 1:np){
          for(f in 1:nf){
            NoMPRecs <- MPRecs_A[[p]][[f]]
            NoMPRecs$Spatial <- NA
            
            MPCalcs <- CalcMPDynamics(MPRecs=NoMPRecs, y, 
                                      nyears, proyears, nsim, 
                                      Biomass_P=StockPars[[p]]$Biomass_P,
                                      VBiomass_P=StockPars[[p]]$VBiomass_P, 
                                      LastTAE=LastTAE[,p,f], 
                                      histTAE=histTAE[,p,f], 
                                      LastSpatial=LastSpatial[,p,f,], 
                                      LastAllocat=LastAllocat[,p,f],
                                      LastTAC=LastCatch[,p,f], 
                                      TACused=TACused[,p,f], 
                                      maxF=maxF,
                                      LR5_P=FleetPars[[p]][[f]]$LR5_P,
                                      LFR_P=FleetPars[[p]][[f]]$LFR_P, 
                                      Rmaxlen_P=FleetPars[[p]][[f]]$Rmaxlen_P,
                                      retL_P=FleetPars[[p]][[f]]$retL_P,
                                      retA_P=FleetPars[[p]][[f]]$retA_P,
                                      L5_P=FleetPars[[p]][[f]]$L5_P, 
                                      LFS_P=FleetPars[[p]][[f]]$LFS_P,
                                      Vmaxlen_P=FleetPars[[p]][[f]]$Vmaxlen_P,
                                      SLarray_P=FleetPars[[p]][[f]]$SLarray_P,
                                      V_P=FleetPars[[p]][[f]]$V_P, 
                                      Fdisc_P=StockPars[[p]]$Fdisc_P,
                                      DR_P=FleetPars[[p]][[f]]$DR_P, 
                                      FM_P=FleetPars[[p]][[f]]$FM_P,
                                      FM_Pret=FleetPars[[p]][[f]]$FM_Pret,
                                      Z_P=FleetPars[[p]][[f]]$Z_P, 
                                      CB_P=FleetPars[[p]][[f]]$CB_P, 
                                      CB_Pret=FleetPars[[p]][[f]]$CB_Pret,
                                      Effort_pot=Effort_pot[,p,f],
                                      StockPars=StockPars[[p]],
                                      FleetPars=FleetPars[[p]][[f]],
                                      ImpPars=ImpPars[[p]][[f]])
            
            if(length(SexPars)>0)
              MPCalcs <- MPCalcsNAs(MPCalcs) # Zeros caused by SexPars
            TACa[,p,f, mm, y] <- TACused[,p,f] # recommended TAC
            #TACa[,p,f, mm, y] <- MPCalcs$TACrec # recommended TAC
            LastSpatial[,p,f,] <- MPCalcs$Si
            LastAllocat[,p,f] <- MPCalcs$Ai
            
            LastTAE[,p,f] <- MPCalcs$TAE
            # LastEi[,p,f] <- MPCalcs$Ei # adjustment to effort
            LastCatch[,p,f] <- MPCalcs$TACrec
            
            Effort[,p,f, mm, y] <- rep(MPCalcs$Effort,nsim)[1:nsim]
            FleetPars[[p]][[f]]$CB_P <- MPCalcs$CB_P # removals
            FleetPars[[p]][[f]]$CB_Pret <- MPCalcs$CB_Pret # retained catch
            FleetPars[[p]][[f]]$FM_P <- MPCalcs$FM_P # fishing mortality
            FM_P[,p,f,,,]<- MPCalcs$FM_P
            FleetPars[[p]][[f]]$FM_Pret <- MPCalcs$FM_Pret # retained fishing mortality
            FMret_P[,p,f,,,]<- MPCalcs$FM_Pret
            #FretA[,p,f,,]<- MPCalcs$FM_Pret
            FleetPars[[p]][[f]]$Z_P <- MPCalcs$Z_P # total mortality
            FleetPars[[p]][[f]]$retA_P <- MPCalcs$retA_P # retained-at-age
            
            FleetPars[[p]][[f]]$retL_P <- MPCalcs$retL_P # retained-at-length
            FleetPars[[p]][[f]]$V_P <- MPCalcs$V_P  # vulnerable-at-age
            VF[,p,f,,]<- MPCalcs$V_P
            FleetPars[[p]][[f]]$SLarray_P <- MPCalcs$SLarray_P # vulnerable-at-length
            
          } # end of fleets
        } # end of stocks
      } # end of not update year
    } # end of projection years
    
    # SSB relative to SSBMSY
    B_BMSYa[, ,mm, ] <- apply(SSB_P, c(1,2, 4), sum, na.rm=TRUE)/array(SSBMSY_y[,,mm,],
                                                                       c(nsim,np,proyears))
    
    for(p in 1:np) for(f in 1:nf)
      FMa[,p,f, mm, ] <- -log(1 - apply(FleetPars[[p]][[f]]$CB_P, c(1, 3), sum,
                                        na.rm=TRUE)/apply(VBiomass_P[,p,,,]+
                                                            FleetPars[[p]][[f]]$CB_P,
                                                          c(1, 3), sum, na.rm=TRUE))
    for(f in 1:nf)
      F_FMSYa[, ,f,mm, ] <- FMa[,,f, mm, ]/FMSY_y[,,mm,(nyears+1):(nyears+proyears)]
    
    Ba[, ,mm, ] <- apply(Biomass_P, c(1, 2,4), sum, na.rm=TRUE) # biomass
    SSBa[, ,mm, ] <- apply(SSB_P, c(1, 2,4), sum, na.rm=TRUE) # spawning stock biomass
    VBa[, ,mm, ] <- apply(VBiomass_P, c(1, 2, 4), sum, na.rm=TRUE) # vulnerable biomass
    
    for(p in 1:np) for(f in 1:nf)
      Ca[, p,f,mm, ] <- apply(FleetPars[[p]][[f]]$CB_P, c(1, 3),
                              sum, na.rm=TRUE) # removed
    for(p in 1:np) for(f in 1:nf)
      CaRet[, p,f,mm, ] <- apply(FleetPars[[p]][[f]]$CB_Pret, c(1, 3),
                                 sum, na.rm=TRUE) # retained catch
    
    if (!silent) {
      cat("\n")
      if (all(checkNA != nsim) & !all(checkNA == 0)) {
        # print number of NAs
        # message(checkNA)
        # message(checkNA[upyrs])
        ntot <- sum(checkNA[,,upyrs])
        totyrs <- sum(checkNA[,,upyrs] >0)
        nfrac <- round(ntot/(length(upyrs)*nsim),2)*100
        
        message(totyrs, ' years had TAC = NA for some simulations (',
                nfrac, "% of total simulations)")
        message('Used TAC_y = TAC_y-1')
      }
 
      if("progress"%in%names(control))
        if(control$progress)
          shiny::incProgress(1/nMP, detail = round(mm*100/nMP))
    }
    
  } # end of MP loop
  
  OM<-Obsout<-list()
  for(p in 1:np) {
    OM[[p]]<-Obsout[[p]]<-list()

    for(f in 1:nf) {
      OM[[p]][[f]]<-MSElist[[p]][[f]][[1]]@OM
      Obsout[[p]][[f]]<-MSElist[[p]][[f]][[1]]@Obs
    }
  }
  Misc <- list()
  Misc$Data <-MSElist
  Misc[['MOM']]<-MOM
  
  # need to reformat MMP and complex mode to work with MSEout slot
  if(class(MPs)=="character") MPs<-list(MPs)
  
  # ---- Create MSE Object ---
  MSEout <- new("MMSE", Name = MOM@Name, nyears, proyears, nMPs=nMP, MPs=MPs,
                MPcond=MPcond,MPrefs=MPrefs,nsim, nstocks=np, nfleets=nf,
                Snames=Snames, Fnames=Fnames, Stocks=Stocks, Fleets=Fleets,
                Obss=Obs, Imps=Imps,OM=OM, Obs=Obsout, B_BMSY=B_BMSYa,
                F_FMSY=F_FMSYa, B=Ba, SSB=SSBa, VB=VBa, FM=FMa, CaRet, TAC=TACa,
                SSB_hist = SSB, CB_hist = CB, FM_hist = FM, Effort = Effort,
                PAA=array(), CAA=array(), CAL=list(), CALbins=list(),
                MSY_P = MSY_P, FMSY_P = FMSY_P, SSBMSY_P = SSBMSY_P,
                Misc = Misc)
}