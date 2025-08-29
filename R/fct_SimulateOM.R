# Simulate for old `OM` class objects

SimulateOM <- function(OM=MSEtool::testOM, parallel=FALSE, silent=FALSE, nsim=NULL) {
  
  if (!is.null(nsim)) {
    if (nsim<OM@nsim) 
      OM@nsim <- nsim
  }
  
  if (inherits(OM, 'MOM')) {
    hist <- SimulateMOM(OM, parallel, silent)
    return(hist)
  }
  
  # ---- Initial Checks and Setup ---
  OM <- CheckOM(OM, !silent)
  
  if (OM@nsim <=1) 
    stop("OM@nsim must be > 1", call.=FALSE)
  
  # Set up parallel processing 
  ncpus <- set_parallel(any(unlist(parallel)))
  
  # Set pbapply functions 
  
  
  if (requireNamespace("pbapply", quietly = TRUE) && !silent) {
    .lapply <- pbapply::pblapply
    .sapply <- pbapply::pbsapply
    
    # Argument to pass parallel cluster (if running)
    formals(.lapply)$cl <- formals(.sapply)$cl <- substitute(if (snowfall::sfIsRunning()) snowfall::sfGetCluster() else NULL)
  } else if (snowfall::sfIsRunning()) {
    .lapply <- snowfall::sfLapply
    .sapply <- snowfall::sfSapply
  } else {
    .lapply <- base::lapply
    .sapply <- base::sapply
  }
  
  set.seed(OM@seed) # set seed for reproducibility
  nsim <- OM@nsim # number of simulations
  nyears <- OM@nyears # number of historical years
  proyears <- OM@proyears # number of projection years
  interval <- OM@interval # management interval (annual)
  maxF <- OM@maxF # maximum apical F
  pstar <- OM@pstar # Percentile of the sample of the TAC for each MP
  reps <- OM@reps # Number of samples of the management recommendation for each MP
  
  # plusgroup
  plusgroup <- 1 # default now is to use plusgroup
  if(!is.null(OM@cpars$plusgroup) && !OM@cpars$plusgroup) {
    plusgroup <- 0; OM@cpars$plusgroup <- NULL
  }
  
  # ---- Control Parameters Options ----
  control <- OM@cpars$control; OM@cpars$control <- NULL
  
  # Shiny progress bar
  inc.progress <- FALSE
  if("progress" %in% names(control)) {
    if(control$progress) {
      if (requireNamespace("shiny", quietly = TRUE)) {
        inc.progress <- TRUE
      } else {
        warning('package `shiny` needs to be installed for progress bar')
      }
    }
  }
  if (inc.progress)
    shiny::incProgress(0.2, detail = 'Sampling OM Parameters')
  
  # Option to optimize depletion for vulnerable biomass instead of spawning biomass
  control$optVB <- FALSE
  if (!is.null(control$D) && control$D == "VB") control$optVB <- TRUE
  
  # Option to optimize depletion for sp biomass MSY instead of spawning biomass
  control$optSBMSY <- FALSE
  if (!is.null(control$D) && control$D == "SBMSY") control$optSBMSY <- TRUE
  
  # --- Sample OM parameters ----
  if(!silent) message("Loading operating model")
  
  # custom parameters exist - sample and write to list
  SampCpars <- list()
  if (length(OM@cpars)>0)  {
    SampCpars <- SampleCpars(cpars=OM@cpars, nsim, silent=silent)
  }
  
  set.seed(OM@seed) # set seed again after cpars has been sampled
  
  # Stock Parameters
  StockPars <- SampleStockPars(Stock=OM,
                               nsim,
                               nyears,
                               proyears,
                               cpars=SampCpars,
                               msg=!silent)
  
  # Check for custom stock-recruit function
  StockPars <- Check_custom_SRR(StockPars, SampCpars, nsim)
  
  
  # Checks
  if (any(range(StockPars$M_ageArray) <=tiny))
    stop("range of StockPars$M_ageArray is: ", paste0(range(StockPars$M_ageArray), collapse="-"))
  
  # Fleet Parameters
  FleetPars <- SampleFleetPars(Fleet=SubOM(OM, "Fleet"),
                               Stock=StockPars,
                               nsim,
                               nyears,
                               proyears,
                               cpars=SampCpars)
  
  # Obs Parameters
  ObsPars <- SampleObsPars(OM, nsim, cpars=SampCpars, Stock=StockPars,
                           nyears, proyears)
  
  # Imp Parameters
  ImpPars <- SampleImpPars(OM, nsim, cpars=SampCpars, nyears, proyears)
  
  
  
  # Bio-Economic Parameters
  BioEcoPars <- c("RevCurr", "CostCurr", "Response", "CostInc", "RevInc", "LatentEff")
  if (all(lapply(SampCpars[BioEcoPars], length) == 0)) {
    # no bio-economic model
    # if (!silent) message("No bio-economic model parameters found. \nTAC and TAE assumed to be caught in full")
    RevCurr <- CostCurr <- Response <- CostInc <- RevInc <- LatentEff <- rep(NA, nsim)
  } else {
    if (!silent) message("Bio-economic model parameters found.")
    # Checks
    if (length(SampCpars$CostCurr) != nsim) stop("OM@cpars$CostCurr is not length OM@nsim", call.=FALSE)
    if (length(SampCpars$RevCurr) != nsim) stop("OM@cpars$RevCurr is not length OM@nsim", call.=FALSE)
    if (length(SampCpars$Response) != nsim) stop("OM@cpars$Response is not length OM@nsim", call.=FALSE)
    if (length(SampCpars$RevInc) != nsim) SampCpars$RevInc <- rep(0, nsim)
    if (length(SampCpars$CostInc) != nsim) SampCpars$CostInc <- rep(0, nsim)
    if (length(SampCpars$LatentEff) != nsim) SampCpars$LatentEff <- rep(NA, nsim)
    RevCurr <- SampCpars$RevCurr
    CostCurr <- SampCpars$CostCurr
    Response <- SampCpars$Response
    CostInc <- SampCpars$CostInc
    RevInc <- SampCpars$RevInc
    LatentEff <- SampCpars$LatentEff
  }
  
  # ---- Initialize Arrays ----
  n_age <- StockPars$maxage + 1 # number of age classes (starting at age-0)
  StockPars$n_age <- n_age
  nareas <- StockPars$nareas
  N <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # stock numbers array
  Biomass <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # stock biomass array
  VBiomass <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # vulnerable biomass array
  SSN <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # spawning stock numbers array
  SSB <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # spawning stock biomass array
  FM <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # fishing mortality rate array
  FMret <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # fishing mortality rate array for retained fish
  Z <- array(NA, dim = c(nsim, n_age, nyears, nareas))  # total mortality rate array
  Agearray <- array(rep(0:StockPars$maxage, each = nsim), dim = c(nsim, n_age))  # Age array
  
  #  ---- Pre Equilibrium calcs ----
  # Set up array indexes sim (S) age (A) year (Y) region/area (R)
  SAYR <- as.matrix(expand.grid(1:nareas, 1, 1:n_age, 1:nsim)[4:1])
  SAY <- SAYR[, 1:3]
  SAR <- SAYR[, c(1,2,4)]
  SA <- Sa <- SAYR[, 1:2]
  SR <- SAYR[, c(1, 4)]
  S <- SAYR[, 1]
  SY <- SAYR[, c(1, 3)]
  Sa[,2]<- n_age-Sa[,2] + 1 # This is the process error index for initial year
  
  # ---- Calculate initial distribution if mov provided in cpars ----
  if (is.null(StockPars$initdist)) {
    # mov has been passed in cpars - initdist hasn't been defined
    StockPars$initdist <- CalcDistribution(StockPars, FleetPars, SampCpars, nyears,
                                           maxF,
                                           plusgroup, checks=FALSE)
  }
  
  # Unfished recruitment by area - INITDIST OF AGE 1.
  StockPars$R0a <- matrix(StockPars$R0, nrow=nsim, ncol=nareas, byrow=FALSE) * StockPars$initdist[,1,] #
  
  # ---- Unfished Equilibrium calcs ----
  # unfished survival 
  surv <- lapply(1:nsim, calc_survival, StockPars=StockPars, 
                 plusgroup=plusgroup, inc_spawn_time=FALSE) %>% 
    abind(., along=3) %>% aperm(., c(3,1,2))
  
  # unfished survival (spawning)
  SBsurv <- lapply(1:nsim, calc_survival, StockPars=StockPars, 
                   plusgroup=plusgroup, inc_spawn_time=TRUE) %>% 
    abind(., along=3) %>% aperm(., c(3,1,2))
  
  Nfrac <- SBsurv * StockPars$Mat_age  # predicted numbers of mature ages in all years (accounting for `spawn_time_frac`)
  
  # indices for all years
  SAYR_a <- as.matrix(expand.grid(1:nareas, 1:(nyears+proyears), 1:n_age, 1:nsim)[4:1])
  SAY_a <- SAYR_a[, 1:3]
  SAR_a <- SAYR_a[, c(1,2,4)]
  SA_a <- SAYR_a[, 1:2]
  SR_a <- SAYR_a[, c(1, 4)]
  S_a <- SAYR_a[, 1]
  SY_a <- SAYR_a[, c(1, 3)]
  
  # arrays for unfished biomass for all years
  SSN_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  N_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  Biomass_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  SSB_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  
  # Calculate initial spawning stock numbers for all years
  SSN_a[SAYR_a] <- Nfrac[SAY_a] * StockPars$R0[S_a] * StockPars$initdist[SAR_a]
  N_a[SAYR_a] <- StockPars$R0[S_a] * surv[SAY_a] * StockPars$initdist[SAR_a] 
  Biomass_a[SAYR_a] <- N_a[SAYR_a] * StockPars$Wt_age[SAY_a] 
  SSB_a[SAYR_a] <- SBsurv[SAY_a]  * StockPars$R0[S_a] * StockPars$initdist[SAR_a] * StockPars$Fec_Age[SAY_a]   
  
  SSN0_a <- apply(SSN_a, c(1,3), sum) # unfished spawning numbers for each year
  N0_a <- apply(N_a, c(1,3), sum) # unfished numbers for each year)
  SSB0_a <- apply(SSB_a, c(1,3), sum) # unfished spawning biomass for each year
  SSB0a_a <- apply(SSB_a, c(1, 3,4), sum)  # Calculate unfished spawning stock biomass by area for each year
  B0_a <- apply(Biomass_a, c(1,3), sum) # unfished biomass for each year
  VB0_a <- apply(apply(Biomass_a, c(1,2,3), sum) * FleetPars$V_real, c(1,3), sum) # unfished vulnerable biomass for each year
  
  # ---- Unfished Reference Points ----
  SSBpRa <- array(SSB0_a/matrix(StockPars$R0, nrow=nsim, ncol=nyears+proyears),
                  dim = c(nsim, nyears+proyears))
  
  UnfishedRefs <- sapply(1:nsim, CalcUnfishedRefs, ageM=StockPars$ageM, N0_a=N0_a, SSN0_a=SSN0_a,
                         SSB0_a=SSB0_a, B0_a=B0_a, VB0_a=VB0_a, SSBpRa=SSBpRa, SSB0a_a=SSB0a_a)
  N0 <- UnfishedRefs[1,] %>% unlist() # average unfished numbers
  SSN0 <- UnfishedRefs[2,] %>% unlist() # average unfished spawning numbers
  SSB0 <- UnfishedRefs[3,] %>% unlist() # average unfished spawning biomass
  B0 <- UnfishedRefs[4,] %>% unlist() # average unfished biomass
  VB0 <- UnfishedRefs[5,] %>% unlist() # average unfished vulnerable biomass
  
  Unfished_Equilibrium <- list(
    N_at_age=N_a,
    B_at_age=Biomass_a,
    SSB_at_age=SSB_a,
    SSN_at_age=SSN_a,
    VB_at_age=Biomass_a * replicate(nareas, FleetPars$V_real)
  )
  
  # average spawning stock biomass per recruit
  SSBpR <- matrix(UnfishedRefs[6,] %>% unlist(), nrow=nsim, ncol=nareas)
  # average unfished biomass
  SSB0a <- UnfishedRefs[7,] %>% unlist() %>% matrix(nrow=nsim, ncol=nareas, byrow = TRUE)
  bR <- matrix(log(5 * StockPars$hs)/(0.8 * SSB0a), nrow=nsim)  # Ricker SR params
  aR <- matrix(exp(bR * SSB0a)/SSBpR, nrow=nsim)  # Ricker SR params
  
  # --- Optimize for Initial Depletion ----
  initD <- SampCpars$initD #
  if (all(is.na(initD)))
    initD <- NULL
  if (!is.null(initD)) { # initial depletion is not unfished
    if (!silent) message("Optimizing for user-specified depletion in first historical year")
    Perrmulti <- sapply(1:nsim, optDfunwrap, initD=initD, 
                        R0=StockPars$R0,
                        initdist=StockPars$initdist,
                        Perr_y=StockPars$Perr_y, 
                        surv=surv[,,1], 
                        Fec_age=StockPars$Fec_Age,
                        SSB0=SSB0,
                        n_age=StockPars$n_age)
    
    StockPars$Perr_y[,1:StockPars$maxage] <- StockPars$Perr_y[, 1:StockPars$maxage] *
      matrix(Perrmulti, nrow=nsim, ncol=StockPars$maxage, byrow=FALSE)
  }
  
  # --- Non-equilibrium Initial Year ----
  SSN[SAYR] <- Nfrac[SAY] * StockPars$R0[S] *
    StockPars$initdist[SAR]*StockPars$Perr_y[Sa]  # Calculate initial spawning stock numbers
  N[SAYR] <- StockPars$R0[S] * surv[SAY] * StockPars$initdist[SAR]*StockPars$Perr_y[Sa]  # Calculate initial stock numbers
  Biomass[SAYR] <- N[SAYR] * StockPars$Wt_age[SAY]  # Calculate initial stock biomass
  SSB[SAYR] <- SBsurv[SAY] * StockPars$R0[S] *  StockPars$initdist[SAR]*StockPars$Perr_y[Sa] * StockPars$Fec_Age[SAY]    # Calculate spawning stock biomass
  VBiomass[SAYR] <- N[SAYR] * FleetPars$Wt_age_C[SAY] * FleetPars$V_real[SAY]  # Calculate vulnerable biomass
  
  
  StockPars$aR <- aR
  StockPars$bR <- bR
  StockPars$SSB0 <- SSB0
  StockPars$VB0 <- VB0
  StockPars$N <- N
  StockPars$plusgroup <- plusgroup[1]
  StockPars$maxF <- OM@maxF
  StockPars$SSBpR <- SSBpR
  
  if (!is.null(control$checks) && !is.null(initD)) { # check initial depletion
    plot(apply(SSB[,,1,], 1, sum)/SSB0, initD)
    if (!any(round(apply(SSB[,,1,], 1, sum)/SSB0, 2) == round(initD,2)))
      warning('problem with initial depletion')
  }
  
  # --- Historical Spatial closures ----
  if (!is.null(SampCpars$MPA)) {
    # MPA by year passed in cpars
    MPA <- SampCpars$MPA
    if (any(dim(MPA) != c(nyears+proyears, nareas))) {
      stop('cpars$MPA must be a matrix with `nyears+proyears` rows and `nareas` columns', .call=FALSE)
    }
    if (any(MPA !=1 & MPA!=0))
      stop('values in cpars$MPA must be either 0 (closed) or open (1)', .call=FALSE)
    if (any(MPA!=1)) {
      for (a in 1:nareas) {
        yrs <- which(MPA[,a] == 0)
        if (length(yrs)>0) {
          if (!silent)
            message('Spatial closure detected in area ', a, ' in years ',
                    paste(findIntRuns(yrs), collapse=", "))
        }
      }
    }
  } else {
    MPA <- matrix(1, nrow=nyears+proyears, ncol=nareas)
    if (FleetPars$MPA) {
      if (!silent) message('Historical MPA in Area 1 for all years')
      MPA[,1] <- 0
    }
  }
  FleetPars$MPA <- MPA
  
  # --- Calculate MSY statistics for each year ----
  if (inc.progress)
    shiny::incProgress(0.2, detail = 'Calculating Reference Points')
  
  # ignores spatial closures
  # assumes all vulnerable fish are caught - ie no discarding
  MSY_y <- array(0, dim=c(nsim, nyears+proyears)) # store MSY for each sim and year
  FMSY_y <- MSY_y # store FMSY for each sim, and year
  SSBMSY_y <- MSY_y # store SSBMSY for each sim, and year
  BMSY_y <- MSY_y # store BMSY for each sim, and year
  VBMSY_y <- MSY_y # store VBMSY for each sim, and year
  R0_y <- MSY_y # store R0 for each sim, and year
  h_y <- MSY_y # store h for each sim, and year
  N0_y <- MSY_y
  SN0_y <- MSY_y
  B0_y <- MSY_y
  SSB0_y <- MSY_y
  VB0_y <- MSY_y
  
  F01_YPR_y <- MSY_y # store F01 for each sim, and year
  Fmax_YPR_y <- MSY_y # store Fmax for each sim, and year
  Fcrash_y <- MSY_y # store Fcrash for each sim, and year
  SPRcrash_y  <- MSY_y # store SPRcrash for each sim, and year
  Fmed_y <- MSY_y # store Fmed (F that generates the median historical SSB/R) for each sim, and year
  
  SPR_target <- seq(0.2, 0.6, 0.05)
  F_SPR_y <- array(0, dim = c(nsim, length(SPR_target), nyears + proyears)) %>%
    structure(dimnames = list(NULL, paste0("F_", 100*SPR_target, "%"), NULL)) #array of F-SPR% by sim, SPR%, year
  
  if(!silent) message("Calculating MSY reference points for each year")
  # average life-history parameters over ageM years
  
  # Assuming all vulnerable fish are kept; ie MSY is total removals
  MSYrefsYr <- .lapply(1:nsim, function(x) {
    sapply(1:(nyears+proyears), function(y) {
      optMSY_eq(x, 
                yr.ind=y,
                StockPars,
                FleetPars$V_real_2,
                FleetPars$Wt_age_C) 
    })
  })
  
  MSY_y[] <- sapply(MSYrefsYr, function(x) x["Yield", ]) %>% t()
  FMSY_y[] <- sapply(MSYrefsYr, function(x) x["F", ]) %>% t()
  SSBMSY_y[] <- sapply(MSYrefsYr, function(x) x["SB", ]) %>% t()
  BMSY_y[] <- sapply(MSYrefsYr, function(x) x["B", ]) %>% t()
  VBMSY_y[] <- sapply(MSYrefsYr, function(x) x["VB", ]) %>% t()
  
  R0_y[] <- sapply(MSYrefsYr, function(x) x["R0", ]) %>% t()
  h_y[] <- sapply(MSYrefsYr, function(x) x["h", ]) %>% t()
  N0_y[] <- sapply(MSYrefsYr, function(x) x["N0", ]) %>% t()
  SN0_y[] <- sapply(MSYrefsYr, function(x) x["SN0", ]) %>% t()
  B0_y[] <- sapply(MSYrefsYr, function(x) x["B0", ]) %>% t()
  SSB0_y[] <- sapply(MSYrefsYr, function(x) x["SB0", ]) %>% t()
  VB0_y[] <- sapply(MSYrefsYr, function(x) x["VB", ]/x["VB_VB0", ]) %>% t()
  
  if (any(FMSY_y == 0)) {
    message_warn("FMSY = 0 for some simulations and years.")
  }
  
  # --- MSY reference points ----
  MSYRefPoints <- sapply(1:nsim, CalcMSYRefs, MSY_y=MSY_y, FMSY_y=FMSY_y,
                         SSBMSY_y=SSBMSY_y, BMSY_y=BMSY_y, VBMSY_y=VBMSY_y,
                         ageM=StockPars$ageM, nyears=nyears)
  
  MSY <- MSYRefPoints[1,] %>% unlist() # record the MSY results (Vulnerable)
  FMSY <- MSYRefPoints[2,] %>% unlist()  # instantaneous FMSY (Vulnerable)
  SSBMSY <- MSYRefPoints[3,] %>% unlist()  # Spawning Stock Biomass at MSY
  BMSY <- MSYRefPoints[4,] %>% unlist() # total biomass at MSY
  VBMSY <- MSYRefPoints[5,] %>% unlist() # Biomass at MSY (Vulnerable)
  UMSY <- MSY/VBMSY  # exploitation rate
  FMSY_M <- FMSY/StockPars$M  # ratio of true FMSY to natural mortality rate M
  SSBMSY_SSB0 <- SSBMSY/SSB0 # SSBMSY relative to unfished (SSB)
  BMSY_B0 <- BMSY/B0 # Biomass relative to unfished (B0)
  VBMSY_VB0 <- VBMSY/VB0 # VBiomass relative to unfished (VB0)
  
  # --- Dynamic Unfished Reference Points ----
  Unfished <- sapply(1:nsim, function(x)
    popdynCPP(nareas, StockPars$maxage,
              Ncurr=StockPars$N[x,,1,],
              nyears+proyears,
              M_age=StockPars$M_ageArray[x,,],
              Asize_c=StockPars$Asize[x,],
              MatAge=StockPars$Mat_age[x,,],
              FecAge=StockPars$Fec_Age[x,,],
              WtAge=StockPars$Wt_age[x,,],
              Vuln=FleetPars$V_real[x,,],
              Retc=FleetPars$retA_real[x,,],
              Prec=StockPars$Perr_y[x,],
              movc=split_along_dim(StockPars$mov[x,,,,],4),
              SRrelc=StockPars$SRrel[x],
              Effind=rep(0, nyears+proyears),
              Spat_targc=FleetPars$Spat_targ[x],
              hc=StockPars$hs[x],
              R0c=StockPars$R0a[x,],
              SSBpRc=StockPars$SSBpR[x,],
              aRc=StockPars$aR[x,],
              bRc=StockPars$bR[x,],
              Qc=0,
              Fapic=0,
              MPA=FleetPars$MPA,
              maxF=StockPars$maxF,
              control=1,
              SSB0c=StockPars$SSB0[x],
              SRRfun=StockPars$SRRfun,
              SRRpars = StockPars$SRRpars[[x]],
              plusgroup=StockPars$plusgroup,
              spawn_time_frac=StockPars$spawn_time_frac[x]))
  
  N_unfished <- aperm(array(as.numeric(unlist(Unfished[1,], use.names=FALSE)),
                            dim=c(n_age, nyears+proyears, nareas, nsim)), c(4,1,2,3))
  
  Biomass_unfished <- aperm(array(as.numeric(unlist(Unfished[2,], use.names=FALSE)),
                                  dim=c(n_age, nyears+proyears, nareas, nsim)), c(4,1,2,3))
  
  SSN_unfished <- aperm(array(as.numeric(unlist(Unfished[3,], use.names=FALSE)),
                              dim=c(n_age, nyears+proyears, nareas, nsim)), c(4,1,2,3))
  
  SSB_unfished <- aperm(array(as.numeric(unlist(Unfished[4,], use.names=FALSE)),
                              dim=c(n_age, nyears+proyears, nareas, nsim)), c(4,1,2,3))
  
  VBiomass_unfished <- aperm(array(as.numeric(unlist(Unfished[5,], use.names=FALSE)),
                                   dim=c(n_age, nyears+proyears, nareas, nsim)), c(4,1,2,3))
  
  StockPars$MSY <- MSY
  StockPars$FMSY <- FMSY
  StockPars$SSBMSY <- SSBMSY
  StockPars$BMSY <- BMSY
  StockPars$VBMSY <- VBMSY
  StockPars$UMSY <- UMSY
  StockPars$FMSY_M <- FMSY_M
  StockPars$SSBMSY_SSB0 <- SSBMSY_SSB0
  StockPars$BMSY_B0 <- BMSY_B0
  StockPars$VBMSY_VB0 <- VBMSY_VB0
  
  # --- Optimize catchability (q) to fit depletion ----
  if (inc.progress)
    shiny::incProgress(0.2, detail = 'Optimizing for Depletion')
  
  bounds <- c(0.0001, 15) # q bounds for optimizer
  # find the q that gives current stock depletion - optVB = depletion for vulnerable biomass else SB
  if (!is.null(SampCpars$qs)) {
    if(!silent)
      message_info("Skipping optimization for depletion - using catchability (q) from OM@cpars.")
    qs <- SampCpars$qs
  } else {
    if(!silent)
      message("Optimizing for user-specified depletion in last historical year")
    
    qs <- .sapply(1:nsim, CalculateQ, StockPars, FleetPars, pyears=nyears, bounds, control=control)
    
  }
  
  # tictoc::tic()
  # t <- CalculateQ(5, StockPars, FleetPars, pyears=nyears, bounds, control=control)
  # tictoc::toc()
  
  
  # --- Check that q optimizer has converged ----
  LimBound <- c(1.1, 0.9)*range(bounds)
  # bounds for q (catchability). Flag if bounded optimizer hits the bounds
  if (!is.null(SampCpars$qs)) {
    probQ <- numeric(0)
  } else{
    probQ <- which(qs > max(LimBound) | qs < min(LimBound))
    Nprob <- length(probQ)
  }
  
  fracD <- 0.05; ntrials <- 50
  if (!is.null(control$ntrials)) ntrials <- control$ntrials
  if (!is.null(control$fracD)) fracD <- control$fracD
  
  # If q has hit bound, re-sample depletion and try again. Tries 'ntrials' times and then alerts user
  if (length(probQ) > 0) {
    Err <- TRUE
    if(!silent) message_info(Nprob,' simulations have final biomass that is not close to sampled depletion')
    if(!silent) message_info('Re-sampling depletion, recruitment error, and fishing effort')
    
    count <- 0
    OM2 <- OM
    while (Err & count < ntrials) {
      # Re-sample Stock Parameters
      Nprob <- length(probQ)
      OM2@nsim <- Nprob
      SampCpars2 <- list()
      
      if (length(OM2@cpars)>0) SampCpars2 <- SampleCpars(OM2@cpars, OM2@nsim, silent=TRUE)
      
      ResampStockPars <- SampleStockPars(OM2, cpars=SampCpars2, msg=FALSE)
      ResampStockPars$CAL_bins <- StockPars$CAL_bins
      ResampStockPars$CAL_binsmid <- StockPars$CAL_binsmid
      ResampStockPars$nCALbins <- length(StockPars$CAL_binsmid )
      
      StockPars$D[probQ] <- ResampStockPars$D  # Re-sampled depletion
      StockPars$procsd[probQ] <- ResampStockPars$procsd # Re-sampled recruitment deviations
      StockPars$AC[probQ] <- ResampStockPars$AC
      StockPars$Perr_y[probQ,] <- ResampStockPars$Perr_y
      StockPars$hs[probQ] <- ResampStockPars$hs # Re-sampled steepness
      
      # Re-sample historical fishing effort
      ResampFleetPars <- SampleFleetPars(SubOM(OM2, "Fleet"), Stock=ResampStockPars,
                                         OM2@nsim, nyears, proyears, cpars=SampCpars2)
      FleetPars$Esd[probQ] <- ResampFleetPars$Esd
      FleetPars$Find[probQ, ] <- ResampFleetPars$Find
      FleetPars$dFfinal[probQ] <- ResampFleetPars$dFfinal
      
      # Optimize for q
      if (!snowfall::sfIsRunning()) {
        qs[probQ] <- sapply(probQ, CalculateQ, StockPars, FleetPars,
                            pyears=nyears, bounds, control)
      } else {
        qs[probQ] <- snowfall::sfSapply(probQ, CalculateQ, StockPars, FleetPars,
                                        pyears=nyears, bounds, control=control)
      }
      
      probQ <- which(qs > max(LimBound) | qs < min(LimBound))
      count <- count + 1
      if (length(probQ) == 0) Err <- FALSE
    }
    if (Err) { # still a problem
      tooLow <- length(which(qs > max(LimBound)))
      tooHigh <- length(which(qs < min(LimBound)))
      prErr <- length(probQ)/nsim
      if (prErr > fracD & length(probQ) > 1) {
        if (length(tooLow) > 0) message_info(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0) message_info(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent) message_info("More than ", fracD*100, "% of simulations can't get to the specified level of depletion with these Operating Model parameters")
        stop("Change OM@seed and try again for a complete new sample, modify the input parameters, or increase OM@cpars$control$ntrials (default 50)")
      } else {
        if (length(tooLow) > 0) message_info(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0) message_info(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent) message_info("More than ", 100-fracD*100, "% simulations can get to the sampled depletion.\nContinuing")
      }
    }
  }
  
  # --- Simulate historical years ----
  if(!silent) message("Calculating historical stock and fishing dynamics")  # Print a progress update
  
  if (inc.progress)
    shiny::incProgress(0.2, detail = 'Simulating Historical Dynamics')
  
  if(!is.null(control$unfished)) { # generate unfished historical simulations
    if(!silent) message("Simulating unfished historical period")
    Hist <- TRUE
    qs <- rep(0, nsim) # no fishing
  }
  FleetPars$qs <- qs
  
  histYrs <- sapply(1:nsim, function(x)
    popdynCPP(nareas, StockPars$maxage,
              Ncurr=StockPars$N[x,,1,],
              nyears,
              M_age=StockPars$M_ageArray[x,,],
              Asize_c=StockPars$Asize[x,],
              MatAge=StockPars$Mat_age[x,,],
              WtAge=StockPars$Wt_age[x,,],
              FecAge=StockPars$Fec_Age[x,,],
              Vuln=FleetPars$V_real[x,,],
              Retc=FleetPars$retA_real[x,,],
              Prec=StockPars$Perr_y[x,],
              movc=split_along_dim(StockPars$mov[x,,,,],4),
              SRrelc=StockPars$SRrel[x],
              Effind=FleetPars$Find[x,],
              Spat_targc=FleetPars$Spat_targ[x],
              hc=StockPars$hs[x],
              R0c=StockPars$R0a[x,],
              SSBpRc=StockPars$SSBpR[x,],
              aRc=StockPars$aR[x,],
              bRc=StockPars$bR[x,],
              Qc=FleetPars$qs[x],
              Fapic=0,
              maxF=StockPars$maxF,
              MPA=FleetPars$MPA,
              control=1,
              SSB0c=StockPars$SSB0[x],
              SRRfun=StockPars$SRRfun,
              SRRpars = StockPars$SRRpars[[x]],
              plusgroup=StockPars$plusgroup,
              spawn_time_frac = StockPars$spawn_time_frac[x]))
  
  # Number at the beginning of each year
  N <- aperm(array(as.numeric(unlist(histYrs[1,], use.names=FALSE)),
                   dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  
  # Biomass at the beginning of each year
  Biomass <- aperm(array(as.numeric(unlist(histYrs[2,], use.names=FALSE)),
                         dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  # Spawning numbers at the beginning of each year
  SSN <- aperm(array(as.numeric(unlist(histYrs[3,], use.names=FALSE)),
                     dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  # Spawning biomass at the beginning of each year
  SSB <- aperm(array(as.numeric(unlist(histYrs[4,], use.names=FALSE)),
                     dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  # Vulnerable biomass at the beginning of each year
  VBiomass <- aperm(array(as.numeric(unlist(histYrs[5,], use.names=FALSE)),
                          dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  # Fishing mortality during each year
  FM <- aperm(array(as.numeric(unlist(histYrs[6,], use.names=FALSE)),
                    dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  # Retained fishing mortality during each year
  FMret <- aperm(array(as.numeric(unlist(histYrs[7,], use.names=FALSE)),
                       dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  # Total mortality during each year
  Z <- aperm(array(as.numeric(unlist(histYrs[8,], use.names=FALSE)),
                   dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  
  if (control$optVB) {
    Depletion <- apply(VBiomass[,,nyears,],1,sum)/VB0
  } else {
    Depletion <- apply(SSB[,,nyears,],1,sum)/SSB0
  }
  
  # Historical Fishing Effort
  # Effort <- matrix(NA, nrow=nsim, ncol=nyears)
  # for (x in 1:nsim) {
  #   for (y in 1:nyears) {
  #     Effort[x,y] <- max(FM[x,,y,] /  FleetPars$V_real[x,,y] / FleetPars$qs[x])
  #   }
  # }
  
  StockPars$N <- N
  StockPars$Biomass <- Biomass
  StockPars$SSN <- SSN
  StockPars$SSB <- SSB
  StockPars$VBiomass <- VBiomass
  StockPars$FM <- FM
  StockPars$FMret <- FMret
  StockPars$Z <- Z
  StockPars$Depletion <- Depletion
  
  # update OM@D
  if (!is.null(OM@cpars$qs)) StockPars$D <- StockPars$Depletion
  
  # Check that depletion is correct
  if (!is.null(control$checks)) {
    if (prod(round(StockPars$D, 2)/ round(StockPars$Depletion,2)) != 1) {
      print(cbind(round(StockPars$D,2), round(StockPars$Depletion,2)))
      warning("Possible problem in depletion calculations")
    }
  }
  
  if (!is.null(control$checks)) {
    Btemp <- apply(StockPars$SSB, c(1,3), sum)
    x <- Btemp[,nyears]/StockPars$SSBMSY
    y <- StockPars$D/StockPars$SSBMSY_SSB0
    plot(x,y, xlim=c(0,max(x)), ylim=c(0,max(y)), xlab="SSB/SSBMSY", ylab="D/SSBMSY_SSB0")
    lines(c(-10,10),c(-10,10))
  }
  
  # ---- Calculate per-recruit reference points ----
  if (!silent) message("Calculating per-recruit reference points")
  per_recruit_F <- .lapply(1:nsim, function(x) {
    lapply(1:(nyears+proyears), function(y) {
      per_recruit_F_calc(x, yr.ind=y, 
                         StockPars=StockPars,
                         V=FleetPars$V_real_2,
                         Wt_age_C=FleetPars$Wt_age_C,
                         SPR_target=SPR_target)
    })
  })
  F_SPR_y[] <- lapply(per_recruit_F, function(x) sapply(x, getElement, 1)) %>%
    simplify2array() %>% aperm(c(3, 1, 2))
  F01_YPR_y[] <- sapply(per_recruit_F, function(x) sapply(x, function(y) y$FYPR["YPR_F01"])) %>% t()
  Fmax_YPR_y[] <- sapply(per_recruit_F, function(x) sapply(x, function(y) y$FYPR["YPR_Fmax"])) %>% t()
  SPRcrash_y[] <- sapply(per_recruit_F, function(x) sapply(x, function(y) y$FYPR["SPRcrash"])) %>% t()
  Fcrash_y[] <- sapply(per_recruit_F, function(x) sapply(x, function(y) y$FYPR["Fcrash"])) %>% t()
  Fmed_y[] <- sapply(per_recruit_F, function(x) sapply(x, function(y) y$FYPR["Fmed"])) %>% t()
  
  # ---- Calculate annual SPR ----
  SPR_hist <- list()
  SPR_hist$Equilibrium <- CalcSPReq(StockPars$FM, StockPars, n_age, nareas, nyears, proyears, nsim, Hist = TRUE)
  SPR_hist$Dynamic <- CalcSPRdyn(StockPars$FM, StockPars, n_age, nareas, nyears, proyears, nsim, Hist = TRUE)
  
  # ---- Calculate Mean Generation Time ----
  MarrayArea <- replicate(nareas, StockPars$M_ageArray[,,1:nyears])
  Mnow<-apply(MarrayArea[,,nyears,]*N[,,nyears,],1:2,sum)/apply(N[,,nyears,],1:2,sum)
  MGTsurv<-t(exp(-apply(Mnow,1,cumsum)))
  MGT<-apply(Agearray*(StockPars$Mat_age[,,nyears]*MGTsurv),1,sum)/apply(StockPars$Mat_age[,,nyears]*MGTsurv,1,sum)
  if(all(is.na(MGT))) MGT <- StockPars$ageMarray[, nyears]
  
  # --- Calculate B-low ----
  Blow <- rep(NA,nsim)
  HZN=2; Bfrac=0.5
  if (!is.null(control$HZN)) HZN <- control$HZN
  if (!is.null(control$Bfrac)) Bfrac <- control$Bfrac
  if(!silent) message("Calculating B-low reference points")
  
  MGThorizon<-floor(HZN*MGT)
  if (!snowfall::sfIsRunning()) {
    Blow <- sapply(1:nsim,getBlow,
                   StockPars$N,
                   StockPars$Asize,
                   StockPars$SSBMSY,
                   StockPars$SSBpR,
                   FleetPars$MPA,
                   StockPars$SSB0,
                   StockPars$nareas,
                   FleetPars$retA_real,
                   MGThorizon,
                   FleetPars$Find,
                   StockPars$Perr_y,
                   StockPars$M_ageArray,
                   StockPars$hs,
                   StockPars$Mat_age,
                   StockPars$Wt_age,
                   StockPars$Fec_Age,
                   StockPars$R0a,
                   FleetPars$V_real,
                   nyears,
                   StockPars$maxage,
                   StockPars$mov,
                   FleetPars$Spat_targ,
                   StockPars$SRrel,
                   StockPars$aR,
                   StockPars$bR,
                   Bfrac,
                   maxF,
                   SRRfun=StockPars$SRRfun, 
                   SRRpars=StockPars$SRRpars,
                   spawn_time_frac=StockPars$spawn_time_frac)
  } else {
    Blow <- sfSapply(1:nsim,getBlow,
                     StockPars$N,
                     StockPars$Asize,
                     StockPars$SSBMSY,
                     StockPars$SSBpR,
                     FleetPars$MPA,
                     StockPars$SSB0,
                     StockPars$nareas,
                     FleetPars$retA_real,
                     MGThorizon,
                     FleetPars$Find,
                     StockPars$Perr_y,
                     StockPars$M_ageArray,
                     StockPars$hs,
                     StockPars$Mat_age,
                     StockPars$Wt_age,
                     StockPars$Fec_Age,
                     StockPars$R0a,
                     FleetPars$V_real,
                     nyears,
                     StockPars$maxage,
                     StockPars$mov,
                     FleetPars$Spat_targ,
                     StockPars$SRrel,
                     StockPars$aR,
                     StockPars$bR,
                     Bfrac,
                     maxF,
                     SRRfun=StockPars$SRRfun, 
                     SRRpars=StockPars$SRRpars,
                     spawn_time_frac=StockPars$spawn_time_frac)
  }
  
  StockPars$Blow <- Blow
  
  # --- Calculate Reference Yield ----
  if(!silent) message("Calculating reference yield - best fixed F strategy")
  if (!snowfall::sfIsRunning()) {
    RefY <- sapply(1:nsim, calcRefYield, StockPars, FleetPars, proyears,
                   Ncurr=StockPars$N[,,nyears,], nyears, proyears)
  } else {
    RefY <- sfSapply(1:nsim, calcRefYield, StockPars, FleetPars, proyears,
                     Ncurr=StockPars$N[,,nyears,], nyears, proyears)
  }
  
  # ---- Store Reference Points ----
  # arrays for unfished biomass for all years
  SSN_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  N_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  Biomass_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  SSB_a <- array(NA, dim = c(nsim, n_age, nyears+proyears, nareas))
  
  # Calculate initial spawning stock numbers for all years
  SSN_a[SAYR_a] <- Nfrac[SAY_a] * StockPars$R0[S_a] * StockPars$initdist[SAR_a]
  N_a[SAYR_a] <- StockPars$R0[S_a] * surv[SAY_a] * StockPars$initdist[SAR_a] # Calculate initial stock numbers for all years
  Biomass_a[SAYR_a] <- N_a[SAYR_a] * StockPars$Wt_age[SAY_a] # Calculate initial stock biomass
  SSB_a[SAYR_a] <- SSN_a[SAYR_a] * StockPars$Wt_age[SAY_a]  # Calculate spawning stock biomass
  
  SSN0_a <- apply(SSN_a, c(1,3), sum) # unfished spawning numbers for each year
  N0_a <- apply(N_a, c(1,3), sum) # unfished numbers for each year)
  SSB0_a <- apply(SSB_a, c(1,3), sum) # unfished spawning biomass for each year
  SSB0a_a <- apply(SSB_a, c(1, 3,4), sum)  # Calculate unfished spawning stock biomass by area for each year
  B0_a <- apply(Biomass_a, c(1,3), sum) # unfished biomass for each year
  VB0_a <- apply(apply(Biomass_a, c(1,2,3), sum) * FleetPars$V, c(1,3), sum) # unfished vulnerable biomass for each year
  
  ReferencePoints <- list(
    ByYear=list(
      N0=N0_y,
      SN0=SN0_y,
      B0=B0_y,
      SSB0=SSB0_y,
      VB0=VB0_y,
      R0=R0_y,
      h=h_y,
      MSY=MSY_y,
      FMSY=FMSY_y,
      SSBMSY=SSBMSY_y,
      BMSY=BMSY_y,
      VBMSY=VBMSY_y,
      F01_YPR=F01_YPR_y,
      Fmax_YPR=Fmax_YPR_y,
      F_SPR=F_SPR_y,
      Fcrash=Fcrash_y,
      Fmed=Fmed_y,
      SPRcrash=SPRcrash_y
    ),
    Dynamic_Unfished=list(
      N0=apply(N_unfished, c(1,3), sum),
      B0=apply(Biomass_unfished, c(1,3), sum),
      SN0=apply(SSN_unfished, c(1,3), sum),
      SSB0=apply(SSB_unfished, c(1,3), sum),
      VB0=apply(VBiomass_unfished, c(1,3), sum),
      Rec=apply(N_unfished[,1,,], c(1,2), sum)
    ),
    ReferencePoints=data.frame(
      N0=N0,
      B0=B0,
      SSB0=SSB0,
      SSN0=SSN0,
      VB0=VB0,
      MSY=MSY,
      FMSY=FMSY,
      SSBMSY=SSBMSY,
      BMSY=BMSY,
      VBMSY=VBMSY,
      UMSY=UMSY,
      FMSY_M=FMSY_M,
      SSBMSY_SSB0=SSBMSY_SSB0,
      BMSY_B0=BMSY_B0,
      VBMSY_VB0=VBMSY_VB0,
      RefY=RefY,
      MGT=MGT,
      Blow=Blow
    )
  )
  
  # --- Calculate Historical Catch ----
  # Calculate catch-at-age
  if (!is.null( FleetPars$Wt_age_C)) {
    w_at_age <- replicate(nareas, FleetPars$Wt_age_C[,,1:OM@nyears])
    B2 <- N * w_at_age
    CB <- B2 * (1 - exp(-Z)) * (FM/Z)
    CB[is.na(CB)] <- 0
  } else {
    CB <- Biomass * (1 - exp(-Z)) * (FM/Z)  # Catch in biomass (removed from population)
    CB[is.na(CB)] <- 0
    
  }
  
  # Calculate retained-at-age
  Cret <- N * (1 - exp(-Z)) * (FMret/Z)  # Retained catch in numbers
  Cret[is.na(Cret)] <- 0
  
  if (!is.null(FleetPars$Wt_age_C)) {
    CBret <- B2 * (1 - exp(-Z)) * (FMret/Z)  # Retained catch in biomass
    CBret[is.na(CBret)] <- 0
    
  } else {
    CBret <- Biomass * (1 - exp(-Z)) * (FMret/Z)  # Retained catch in biomass
    CBret[is.na(CBret)] <- 0
  }
  
  
  # --- Sampling by area ----
  valNames <- c("Catch", 'BInd', 'SBInd', 'VInd', 'RecInd',
                'CAA', 'CAL')
  Sample_Area_array <- array(1, dim=c(nsim, nyears+proyears, StockPars$nareas))
  Sample_Area <- rep(list(Sample_Area_array), length(valNames))
  names(Sample_Area) <- valNames
  
  if (!is.null(OM@cpars$Sample_Area)) {
    Sample_Area_in <- OM@cpars$Sample_Area
    inval <- names(Sample_Area_in)[!names(Sample_Area_in) %in% valNames]
    if (length(inval)>0)
      stop("Invalid names in OM@cpars$Sample_Area.\nValid names are:\n", paste(valNames, collapse="\n"))
    
    for (nm in names(Sample_Area_in)) {
      dd <- dim(Sample_Area_in[[nm]])
      if (length(dd)!=4) { # Sample_area from Hist
        Sample_Area[[nm]] <- Sample_Area_in[[nm]]
        if (any(dim(Sample_Area_in[[nm]]) != c(nsim, nyears+proyears, nareas)))
          stop("OM@cpars$Sample_Area$", nm, " must be dimensions: nsim, nareas, nyears+proyears", call. = FALSE)
      }
    }
  }
  
  nms <- c("Catch", "BInd", "SBInd", "VInd", "CAA", "CAL")
  for (nm in nms) {
    dd <- dim(Sample_Area[[nm]])
    if (length(dd)!=4) { # Sample_area from Hist
      temp <- replicate(n_age, Sample_Area[[nm]])
      Sample_Area[[nm]] <- aperm(temp, c(1,4,2,3))
    }
  }
  
  # -- TODO --
  # add to ObsPars
  ObsPars$Sample_Area <- Sample_Area
  
  if (inc.progress)
    shiny::incProgress(0.2, detail = 'Simulating Data')
  
  # --- Populate Data object with Historical Data ----
  Data <- makeData(Biomass,
                   CBret,
                   Cret,
                   N,
                   SSB,
                   VBiomass,
                   StockPars,
                   FleetPars,
                   ObsPars,
                   ImpPars,
                   RefPoints=ReferencePoints$ReferencePoints,
                   SampCpars,
                   StockPars$initD,
                   Sample_Area,
                   Name=OM@Name,
                   nyears,
                   proyears,
                   nsim,
                   nareas=StockPars$nareas,
                   reps,
                   CurrentYr=OM@CurrentYr,
                   silent=silent,
                   control)
  
  # --- Condition Simulated Data on input Data object (if it exists) & calculate error stats ----
  StockPars$CBret <- CBret
  StockPars$Biomass <- Biomass
  StockPars$SSB <- SSB
  StockPars$VBiomass <- VBiomass
  StockPars$N <- N
  
  if (methods::is(SampCpars$Data, "Data")) {
    # real data has been passed in cpars
    updatedData <- AddRealData(SimData=Data,
                               RealData=SampCpars$Data,
                               ObsPars,
                               StockPars,
                               FleetPars,
                               nsim,
                               nyears,
                               proyears,
                               SampCpars,
                               msg=!silent,
                               control,
                               Sample_Area)
    Data <- updatedData$Data
    ObsPars <- updatedData$ObsPars
  }
  
  OMPars <- Data@OM
  
  # ---- Add Stock & Fleet Dynamics to Data ----
  Data@Misc$StockPars <- StockPars
  Data@Misc$FleetPars <- FleetPars
  Data@Misc$ReferencePoints <- ReferencePoints
  
  # --- Return Historical Simulations and Data from last historical year ----
  
  Hist <- new("Hist")
  # Data@Misc <- list()
  Hist@Data <- Data
  
  ind <- vapply(ObsPars, function(x) is.atomic(x) && length(x) == nsim, logical(1))
  obs <- data.frame(ObsPars[ind])
  OMPars <- data.frame(OMPars, obs)
  
  Hist@OMPars <- OMPars
  Hist@AtAge <- list(Length=StockPars$Len_age,
                     Weight=StockPars$Wt_age,
                     Select=FleetPars$V_real_2,
                     Retention=FleetPars$retA_real,
                     Maturity=StockPars$Mat_age,
                     N.Mortality=StockPars$M_ageArray,
                     Z.Mortality=StockPars$Z,
                     F.Mortality=FM,
                     Fret.Mortality=FMret,
                     Number=StockPars$N,
                     Biomass=StockPars$Biomass,
                     VBiomass=StockPars$VBiomass,
                     SBiomass=StockPars$SSB,
                     Removals=CB,
                     Landings=CBret,
                     Discards=CB-CBret
  )
  
  Hist@TSdata <- list(
    Number=apply(StockPars$N,c(1,3,4), sum),
    Biomass=apply(StockPars$Biomass,c(1,3,4), sum),
    VBiomass=apply(StockPars$VBiomass,c(1,3,4), sum),
    SBiomass=apply(StockPars$SSB,c(1,3,4), sum),
    Removals=apply(CB, c(1,3,4), sum),
    Landings=apply(CBret,c(1,3,4), sum),
    Discards=apply(CB-CBret,c(1,3,4), sum),
    Find=FleetPars$Find,
    RecDev=StockPars$Perr_y,
    SPR=SPR_hist,
    Unfished_Equilibrium=Unfished_Equilibrium
  )
  
  Hist@Ref <- ReferencePoints
  Hist@SampPars <- list()
  
  # cpars_Stock <- StockPars[which(lapply(StockPars, length) != nsim)]
  # cpars_Fleet <- FleetPars[which(lapply(FleetPars, length) != nsim)]
  # cpars_Obs <- ObsPars[which(lapply(ObsPars, length) != nsim)]
  # cpars_Imp <- ImpPars[which(lapply(ImpPars, length) != nsim)]
  
  StockPars$N <- StockPars$Biomass <- StockPars$SSN <- StockPars$SSB <-
    StockPars$VBiomass <- StockPars$FM <- StockPars$FMret <- StockPars$Z <- NULL
  
  
  Hist@SampPars <- list(
    Stock=StockPars,
    Fleet=FleetPars,
    Obs=ObsPars,
    Imp=ImpPars
  )
  temp <- OM@cpars$Data
  OM@cpars <- list()
  OM@cpars$control <- control
  OM@cpars$Data <- temp
  
  Hist@OM <- OM
  Hist@Misc <- list()
  
  Hist@Misc$BioEco <- data.frame(RevCurr, CostCurr, Response, CostInc, RevInc, LatentEff)
  
  attr(Hist, "version") <- packageVersion("MSEtool")
  attr(Hist, "date") <- date()
  attr(Hist, "R.version") <- R.version
  
  Hist
}
