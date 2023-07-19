#' @describeIn runMSE Run the Historical Simulations from an object of class `OM`
#' @export
#
Simulate <- function(OM=MSEtool::testOM, parallel=FALSE, silent=FALSE) {
  # ---- Initial Checks and Setup ----
  if (methods::is(OM,'OM')) {
    if (OM@nsim <=1) stop("OM@nsim must be > 1", call.=FALSE)
    OM <- CheckOM(OM, msg=!silent)

  } else if (methods::is(OM,'Hist')) {
    if (!silent) message("Using `Hist` object to reproduce historical dynamics")

    # --- Extract cpars from Hist object ----
    cpars <- list()
    cpars <- c(OM@SampPars$Stock, OM@SampPars$Fleet, OM@SampPars$Obs,
               OM@SampPars$Imp, OM@OMPars, OM@OM@cpars)

    # --- Populate a new OM object ----
    newOM <- OM@OM
    newOM@cpars <- cpars
    OM <- newOM
  } else {
    stop("You must specify an operating model")
  }

  # ---- Set up parallel processing ----
  ncpus <- set_parallel(any(unlist(parallel)))
  
  # ---- Set pbapply functions ----
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
  if (length(nyears)<1) stop('OM@nyears is missing')
  proyears <- OM@proyears # number of projection years
  if (length(proyears)<1) stop('OM@proyears is missing')
  interval <- OM@interval # management interval (annual)
  if (length(interval)<1) {
    warning('OM@interval missing. Assuming OM@interval=1')
    OM@interval <- interval <- 1
  }
  maxF <- OM@maxF # maximum apical F
  if (length(maxF)<1) {
    warning('OM@maxF missing. Assuming OM@maxF=3')
    OM@maxF <- maxF <- 3
  }
  pstar <- OM@pstar # Percentile of the sample of the TAC for each MP
  if (length(pstar)<1) {
    warning('OM@pstar missing. Assuming OM@pstar=0.5')
    OM@pstar <- pstar <- 0.5
  }
  reps <- OM@reps # Number of samples of the management recommendation for each MP
  if (length(reps)<1) {
    warning('OM@reps missing. Assuming OM@reps=1')
    OM@reps <- reps <- 1
  }
  # ---- Custom Parameters (cpars) Options ----
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

  # plusgroup
  plusgroup <- 1 # default now is to use plusgroup
  if(!is.null(OM@cpars$plusgroup) && !OM@cpars$plusgroup) {
    plusgroup <- 0; OM@cpars$plusgroup <- NULL
  }

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
  if (!is.null(initD)) { # initial depletion is not unfished
    if (!silent) message("Optimizing for user-specified depletion in first historical year")
    Perrmulti <- sapply(1:nsim, optDfunwrap, initD=initD, R0=StockPars$R0,
                        StockPars$initdist,
                        Perr_y=StockPars$Perr_y, surv=surv[,,1], Fec_age=StockPars$Fec_Age,
                        SSB0=SSB0,
                        StockPars$n_age)
    StockPars$Perr_y[,1:StockPars$maxage] <- StockPars$Perr_y[, 1:StockPars$maxage] * Perrmulti
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
                FleetPars$V_real)
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
                         V=FleetPars$V_real,
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
  CB <- Biomass * (1 - exp(-Z)) * (FM/Z)  # Catch in biomass (removed from population)
  CB[is.na(CB)] <- 0

  # Calculate retained-at-age
  Cret <- N * (1 - exp(-Z)) * (FMret/Z)  # Retained catch in numbers
  Cret[is.na(Cret)] <- 0
  CBret <- Biomass * (1 - exp(-Z)) * (FMret/Z)  # Retained catch in biomass
  CBret[is.na(CBret)] <- 0

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
                     Select=FleetPars$V_real,
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

#' @describeIn runMSE Run the Forward Projections
#'
#' @param Hist An Historical Simulation object (class `Hist`)
#'
#' @export
Project <- function (Hist=NULL, MPs=NA, parallel=FALSE, silent=FALSE,
                     extended=FALSE, checkMPs=TRUE) {

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
    V_P <- FleetPars$V_real  #  selectivity at age array - projections
    LR5_P <- FleetPars$LR5_y
    LFR_P <- FleetPars$LFR_y
    Rmaxlen_P <- FleetPars$Rmaxlen_y
    retA_P <- FleetPars$retA_real # retention at age array - projections
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
    VBiomass_P[SAYR] <- N_P[SAYR] * FleetPars$Wt_age_C[SAY1] * V_P[SAYt]  # Calculate vulnerable biomass
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
    VBiomass_P[SAYR] <- N_P[SAYR] * FleetPars$Wt_age_C[SAY1] * V_P[SAYt]  # Calculate vulnerable biomass
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
                              retL_P, retA_P, L5_P, LFS_P, Vmaxlen_P,
                              SLarray_P, V_P, Fdisc_P, DR_P, FM_P,
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
      if (any(range(retA_P[,,nyears+y] -  FleetPars$retA_real[,,nyears+y]) !=0)) SelectChanged <- TRUE
      if (any(range(V_P[,,nyears+y] - FleetPars$V_real[,,nyears+y]) !=0))  SelectChanged <- TRUE

      # -- Calculate MSY stats for this year ----
      if (SelectChanged) { #
        y1 <- nyears + y
        MSYrefsYr <- sapply(1:nsim, optMSY_eq, 
                            yr.ind=y1, StockPars,
                            V_P)
        MSY_y[,mm,y1] <- MSYrefsYr[1, ]
        FMSY_y[,mm,y1] <- MSYrefsYr[2,]
        SSBMSY_y[,mm,y1] <- MSYrefsYr[3,]

        per_recruit_F <- lapply(1:nsim, per_recruit_F_calc,
                                yr.ind=y1,
                                StockPars=StockPars,
                                V=V_P,
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
                              Biomass=StockPars$Biomass, StockPars$N,
                              Biomass_P, CB_Pret, N_P, SSB=StockPars$SSB,
                              SSB_P, VBiomass=StockPars$VBiomass, VBiomass_P,
                              RefPoints=ReferencePoints,
                              retA_P, retL_P, StockPars,
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
                                retL_P, retA_P, L5_P, LFS_P, Vmaxlen_P,
                                SLarray_P, V_P, Fdisc_P, DR_P, FM_P,
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
                                  retA_P, retL_P, StockPars,
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
    Data_MP@Misc$StockPars <- Data_MP@Misc$FleetPars <- Data_MP@Misc$ReferencePoints <- NULL

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
#' @param OM An operating model object (class `OM` or class `Hist`)
#' @param MPs A vector of methods (character string) of class MP
#' @param Hist Should model stop after historical simulations? Returns an object of
#' class 'Hist' containing all historical data
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical or a named list. Should MPs be run using parallel processing? 
#' For \code{runMSE}, can also be \code{"sac"} to run the entire MSE in parallel
#' using the split-apply-combine technique. See Details for more information. 
#' @param extended Logical. Return extended projection results?
#' if TRUE, `MSE@Misc$extended` is a named list with extended data
#' (including historical and projection by area), and extended version of `MSE@Hist`
#' is returned.
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
                   parallel=FALSE, extended=FALSE, checkMPs=TRUE) {

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
