
#' @describeIn multiMSE Simulate historical dynamics for multi-OM
#'
#' @export
#'
SimulateMOM <- function(MOM=MSEtool::Albacore_TwoFleet, parallel=TRUE, silent=FALSE) {

  # ---- Initial Checks and Setup ----
  if (methods::is(MOM,'MOM')) {
    if (MOM@nsim <=1) stop("MOM@nsim must be > 1", call.=FALSE)

  } else {
    stop("You must specify an operating model of class `MOM`")
  }

  # ---- Set up parallel processing ----
  ncpus <- set_parallel(parallel)
  
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

  set.seed(MOM@seed) # set seed for reproducibility
  nsim <- MOM@nsim
  nyears <- MOM@Fleets[[1]][[1]]@nyears  # number of historical years
  proyears <- MOM@proyears
  allyears <- nyears+proyears
  Stocks <- MOM@Stocks
  Fleets <- MOM@Fleets
  Obs <- MOM@Obs
  Imps <- MOM@Imps
  Rel <- MOM@Rel
  SexPars <- MOM@SexPars
  Complexes <- MOM@Complexes
  CatchFrac <- MOM@CatchFrac
  np <- length(Stocks)
  nf <- length(Fleets[[1]])

  if (MOM@CatchFrac[[1]] %>% nrow() != nsim) { # re-calculate CatchFrac
    CatchFrac <- lapply(MOM@CatchFrac, function(x) x[1:nsim, , drop = FALSE])
    MOM@CatchFrac <- CatchFrac
  }

  if (!length(Rel) && np == 1 && nf == 1) {
    if (!silent) 
      cli::cli_alert_info(c("You have specified only a single stock and fleet with no MICE relationships. ",
            "You should really be using the function {.fun MSEtool::runMSE()}"))
  } else if(np > 1 && !length(MOM@Rel) && !length(MOM@SexPars)) {
    if (!silent) 
        cli::cli_alert_info("You have specified more than one stock but no MICE relationships (slot `MOM@Rel`) or sex-specific relationships (slot MOM@SexPars) among these.")
        cli::cli_par()
        cli::cli_text("As they are independent, consider doing MSE for one stock at a time for computational efficiency.")
  }

  maxF <- MOM@maxF
  Snames <- SIL(Stocks, "Name")
  Fnames <- SIL(Fleets, "Name") %>% make.unique() %>% matrix(nrow = nf)
  cpars <- MOM@cpars

  # ---- Custom Parameters (cpars) Options ----
  control <- cpars$control; cpars$control <- NULL
  
  # Ignore MICE in historical period
  if (!is.null(control$HistRel) && !control$HistRel) {
    HistRel <- list()
  } else {
    HistRel <- Rel
  }

  # Option to optimize depletion for vulnerable biomass instead of spawning biomass
  optVB <- FALSE
  if (!is.null(control$D) && control$D == "VB") optVB <- TRUE

  # Allocation
  if(!length(MOM@Allocation)) {
    MOM@Allocation <- CatchFrac
    if (!silent) 
    cli::cli_alert_info(
      "Slot {.val Allocation} of MOM object not specified. Setting slot {.val Allocation} equal to slot {.val CatchFrac} - current catch fractions"
    )
  }

  if(!length(MOM@Efactor)) {
    MOM@Efactor <- lapply(1:np, function(x) matrix(1, nsim, nf))
    if (!silent) 
    cli::cli_alert_info(
      "Slot {.val Efactor} of MOM object not specified. Setting slot {.val Efactor} to current effort for all fleets"
    )
  }

  # All stocks and sampled parameters must have compatible array sizes (maxage)
  maxage_s <- unique(SIL(Stocks, "maxage"))
  maxage <- max(maxage_s)
  if (length(maxage_s) > 1) {
    if (!silent) 
      cli::cli_alert_info(
      "Stocks of varying maximum ages have been specified all simulations will run to {.val {max(maxage_s)}} ages"
    )
    Stocks <- lapply(Stocks, function(x) {
      x@maxage <- max(maxage)
      return(x)
    })
  }
  
  if (!silent) cli::cli_alert("Loading operating model")
  StockPars <- FleetPars <- ObsPars <- ImpPars <- SampCpars <- new('list')

  plusgroup <- rep(1, np)
  for (p in 1:np) {
    
    # Check for plusgroup now, then remove from cpars before SampleCpars call (see Simulate)
    if (!is.null(cpars[[p]][[1]]$plusgroup) && all(!cpars[[p]][[1]]$plusgroup)) {
      plusgroup[p] <- 0
      for (f in 1:nf) cpars[[p]][[f]]$plusgroup <- NULL
    }
    
    if (!is.null(cpars[[p]]) & !silent)
    if (!silent) 
      cli::cli_alert("Sampling Custom Parameters (`cpars`) for Stock: {.val {Snames[p]}}")
    
    SampCpars[[p]] <- lapply(1:nf, function(f) {
      if (length(cpars) && length(cpars[[p]][[f]])) {
        SampleCpars(cpars[[p]][[f]], nsim, silent=TRUE)
      } else {
        list()
      }
    })
    
    set.seed(MOM@seed) # set seed again after cpars has been sampled
    # --- Sample Stock Parameters ----
    StockPars[[p]] <- SampleStockPars(Stock = Stocks[[p]], nsim, nyears,
                                      proyears, cpars = SampCpars[[p]][[1]],
                                      msg = !silent)
    
    StockPars[[p]]$plusgroup <- plusgroup[p]
    StockPars[[p]]$maxF <- MOM@maxF
    StockPars[[p]]$n_age <- StockPars[[p]]$maxage+1
    
    # --- custom SRR function ---
    # not implemented
    StockPars[[p]] <- Check_custom_SRR(StockPars[[p]], SampCpars[[p]][[1]], nsim)
    #if (any(StockPars[[p]]$SRrel>2))
    #  stop('Custom stock recruit function not supported in `multiMSE`')
    
    # --- Sample Fleet Parameters ----
    FleetPars[[p]] <- lapply(1:nf, function(f) {
      SampleFleetPars(Fleet = Fleets[[p]][[f]],
                      Stock = StockPars[[p]],
                      nsim, nyears, proyears,
                      cpars = SampCpars[[p]][[f]],
                      msg=!silent)
    })
    
    # --- Sample Obs Parameters ----
    ObsPars[[p]] <- lapply(1:nf, function(f) {
      SampleObsPars(MOM@Obs[[p]][[f]], nsim,
                    cpars = SampCpars[[p]][[f]],
                    Stock = StockPars[[p]],
                    nyears, proyears)
    })
    
    # --- Sample Imp Parameters ----
    ImpPars[[p]] <- lapply(1:nf, function(f) {
      SampleImpPars(MOM@Imps[[p]][[f]], nsim,
                    cpars = SampCpars[[p]][[f]],
                    nyears, proyears)
    })
  }
  
  # --- Update Parameters for two-sex stocks ----
  # Depletion, stock-recruit parameters, recdevs, Fleet, Obs, and Imp copied
  # from females to males
  if (length(SexPars)) {
    if (length(SexPars$Herm)) {
      MOM@SexPars$Herm <- SexPars$Herm <- checkHerm(SexPars$Herm, maxage, nsim, nyears, proyears)
    }
    if (is.null(SexPars$share_par) || SexPars$share_par == TRUE) {
      sexmatches <- sapply(1:nrow(SexPars$SSBfrom), function(x) paste(SexPars$SSBfrom[x, ], collapse = "_"))
      parcopy <- match(sexmatches, sexmatches)
      StockPars_t <- StockPars
      FleetPars_t <- FleetPars
      
      # slot_s <- c("D", "hs", "AC", "R0", "R0a", "Perr_y")
      slot_s <- c("D", "hs", "AC",  "Perr_y")
      # slot_f <- c("Esd", "Find", "dFFinal", "Spat_targ", "qinc", "qcv", "qvar", "FinF")
      slot_f <- c("Esd", "Spat_targ", "qinc", "qcv", "qvar")
      
      # if (!silent)
      #   cli::cli_alert_info(
      #     "You have specified sex-specific dynamics. These parameters will be mirrored across sex types according to SexPars$SSBfrom: {.val {paste(c(slot_s, slot_f), collapse = ', ')}} and all observation and implementation parameters"
      #   )
      for (p in 1:np) {
        StockPars[[p]][slot_s] <- StockPars_t[[parcopy[p]]][slot_s]
        
        # keep historical rec devs 
        StockPars[[p]][slot_s]$Perr_y[,1:nyears] <-  StockPars_t[[p]][slot_s]$Perr_y[,1:nyears]
        
        for (f in 1:nf) {
          FleetPars[[p]][[f]][slot_f] <- FleetPars_t[[parcopy[p]]][[f]][slot_f]
          ObsPars[[p]][[f]] <- ObsPars[[parcopy[p]]][[f]]
          ImpPars[[p]][[f]] <- ImpPars[[parcopy[p]]][[f]]
        }
      }
    }
  } # end of sexpars

  nareas_s <- NIL(StockPars, "nareas", lev1 = TRUE)
  nareas <- unique(nareas_s)
  if(length(unique(nareas_s)) != 1) {
    stop("Stocks must have the same specified number of areas - check cpars$mov",
         " for each stock object")
  }

  # ---- Bio-Economic Parameters ----
  # TODO

  # ---- Initialize arrays ----
  n_age <- maxage + 1 # number of age classes (starting at age-0)
  N <- Biomass <- Z<- VBiomass<- SSN <- SSB <- array(NA,
                                                     dim = c(nsim, np, n_age,
                                                             nyears, nareas))
  VF <- FretA <- array(NA, dim = c(nsim, np, nf, n_age, allyears))
  VBF <- FM <- FMret <- array(NA, dim = c(nsim, np, nf, n_age, nyears, nareas))
  SPR <- array(NA, dim = c(nsim, np, n_age, nyears)) # store the Spawning Potential Ratio
  MPA <- array(1,c(np,nf, nyears+proyears,nareas))
  Agearray <- array(rep(1:n_age, each = nsim), dim = c(nsim, n_age))  # Age array

  # ---- Hermaphroditism -----
  # (this is the fraction to be kept (after sex change))
  # E.g. protygynous (Female to male) is H_1_2 where 1 is female 2 is male
  # [sim, stock, maxage] Defaults to all 1s if length(SexPars$Herm)==0
  if (!is.null(control$HermEq) && !control$HermEq) {
    HermFrac <- expandHerm(list(), maxage = maxage, np = np, nsim = nsim)
  } else {
    HermFrac <- expandHerm(SexPars$Herm, maxage = maxage, np = np, nsim = nsim)
  }

  Unfished_Equilibrium <- list()
  for(p in 1:np){
    #  --- Pre Equilibrium calcs ----

    # Set up some array indexes sim (S) age (A) year (Y) region/area (R)
    SPAYR <- as.matrix(expand.grid(1:nareas, 1, 1:n_age, p, 1:nsim)[5:1])
    SPA <- SPAYR[,1:3]
    SAY <- SPAYR[, c(1,3,4)]
    SAR <- SPAYR[, c(1,3,5)]
    SA <- Sa <- SPAYR[, c(1,3)]
    SR <- SPAYR[, c(1,5)]
    S <- SPAYR[, 1]
    SY <- SPAYR[, c(1, 4)]
    Sa[,2] <- n_age-Sa[,2]+1 # This is the process error index for initial year

    # Calculate initial distribution if mov provided in cpars
    # ---- Calculate initial distribution if mov provided in cpars ----
    if (is.null(StockPars[[p]]$initdist)) {
      # mov has been passed in cpars - initdist hasn't been defined
      StockPars[[p]]$initdist <- CalcDistribution(StockPars=StockPars[[p]],
                                                  FleetPars=FleetPars[[p]][[1]],
                                                  SampCpars=SampCpars[[p]][[1]],
                                                  nyears, maxF,
                                                  plusgroup[p], checks=FALSE)
    }

    #*HermFrac[,p,1]  # !!!! INITDIST OF AGE 1. Unfished recruitment by area
    R0a <- matrix(StockPars[[p]]$R0, nrow=nsim, ncol=nareas, byrow=FALSE) *
      StockPars[[p]]$initdist[,1,]

    # ---- Unfished Equilibrium calcs ----
    # unfished survival for every year 
    surv <- lapply(1:nsim, calc_survival, StockPars=StockPars[[p]], 
                   plusgroup=plusgroup[p], inc_spawn_time=FALSE) %>% 
      abind(., along=3) %>% aperm(., c(3,1,2))
    
    # unfished survival (spawning)
    SBsurv <- lapply(1:nsim, calc_survival, StockPars=StockPars[[p]], 
                     plusgroup=plusgroup[p], inc_spawn_time=TRUE) %>% 
      abind(., along=3) %>% aperm(., c(3,1,2))

    Nfrac <- surv * StockPars[[p]]$Mat_age  # predicted numbers of mature ages in all years

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
    SSN_a[SAYR_a] <- Nfrac[SAY_a] * StockPars[[p]]$R0[S_a] * StockPars[[p]]$initdist[SAR_a]
    N_a[SAYR_a] <- StockPars[[p]]$R0[S_a] * surv[SAY_a] * StockPars[[p]]$initdist[SAR_a]
    Biomass_a[SAYR_a] <- N_a[SAYR_a] * StockPars[[p]]$Wt_age[SAY_a]  # Calculate initial stock biomass
    # SSB_a[SAYR_a] <- SSN_a[SAYR_a] * StockPars[[p]]$Wt_age[SAY_a]    # Calculate spawning stock biomass
    SSB_a[SAYR_a] <- SBsurv[SAY_a] * StockPars[[p]]$R0[S_a] * StockPars[[p]]$initdist[SAR_a] * StockPars[[p]]$Fec_Age[SAY_a]  # Calculate spawning stock biomass
    SSN0_a <- apply(SSN_a, c(1,3), sum) # unfished spawning numbers for each year
    N0_a <- apply(N_a, c(1,3), sum) # unfished numbers for each year)
    SSB0_a <- apply(SSB_a, c(1,3), sum) # unfished spawning biomass for each year
    SSB0a_a <- apply(SSB_a, c(1, 3,4), sum)  # Calculate unfished spawning stock biomass by area for each year
    B0_a <- apply(Biomass_a, c(1,3), sum) # unfished biomass for each year

    Vraw <- array(NIL(listy=FleetPars[[p]],namey="V_real"),c(nsim,n_age,allyears,nf))
    Vind <- as.matrix(expand.grid(1:nsim,p,1:nf,1:n_age,1:allyears))
    VF[Vind] <- Vraw[Vind[,c(1,4,5,3)]]

    Fretraw <- array(NIL(listy=FleetPars[[p]],namey="retA_real"),c(nsim,n_age,allyears,nf))
    FretA[Vind] <- Fretraw[Vind[,c(1,4,5,3)]]

    if(nf==1){
      V <- VF[,p,1,,] #<-SOL(FleetPars[[p]],"V")
    }else{
      #Weight by catch fraction
      V <- array(0,c(nsim,n_age,allyears))
      for(f in 1:nf){
        V <- V+VF[,p,f,,]*CatchFrac[[p]][,f]
      }
      #V<-nlz(V,c(1,3),"max") # currently assume unfished vulnerability is equally weighted among fleets
      # V includes discards
    }
    # unfished vulnerable biomass for each year
    VB0_a <- apply(apply(Biomass_a, c(1,2,3), sum) * V, c(1,3), sum)

    # ---- Unfished Reference Points ----
    SSBpRa <- array(SSB0_a/matrix(StockPars[[p]]$R0, nrow=nsim, ncol=nyears+proyears),
                    dim = c(nsim, nyears+proyears))

    UnfishedRefs <- sapply(1:nsim, CalcUnfishedRefs, ageM=StockPars[[p]]$ageM, N0_a=N0_a, SSN0_a=SSN0_a,
                           SSB0_a=SSB0_a, B0_a=B0_a, VB0_a=VB0_a, SSBpRa=SSBpRa, SSB0a_a=SSB0a_a)

    N0 <- UnfishedRefs[1,] %>% unlist() # average unfished numbers
    SSN0 <- UnfishedRefs[2,] %>% unlist() # average unfished spawning numbers
    SSB0 <- UnfishedRefs[3,] %>% unlist() # average unfished spawning biomass
    B0 <- UnfishedRefs[4,] %>% unlist() # average unfished biomass
    VB0 <- UnfishedRefs[5,] %>% unlist() # average unfished vulnerable biomass

    Unfished_Equilibrium[[p]] <- list(
      N_at_age=N_a,
      B_at_age=Biomass_a,
      SSB_at_age=SSB_a,
      SSN_at_age=SSN_a,
      VB_at_age=Biomass_a * replicate(nareas,V)
    )

    # average spawning stock biomass per recruit
    SSBpR <- matrix(UnfishedRefs[6,] %>% unlist(), nrow=nsim, ncol=nareas)

    # average unfished biomass
    SSB0a <- UnfishedRefs[7,] %>% unlist() %>% matrix(nrow=nsim, ncol=nareas, byrow = TRUE)
    bR <- matrix(log(5 * StockPars[[p]]$hs)/(0.8 * SSB0a), nrow=nsim)  # Ricker SR params
    aR <- matrix(exp(bR * SSB0a)/SSBpR, nrow=nsim)  # Ricker SR params

    # --- Optimize for Initial Depletion ----
    # currently done in SS2MOM

    #  --- Non-equilibrium Initial Year ----
    SSN[SPAYR] <- Nfrac[SAY] * StockPars[[p]]$R0[S] * StockPars[[p]]$initdist[SAR] *
      StockPars[[p]]$Perr_y[Sa]
    # Calculate initial stock numbers
    N[SPAYR] <- StockPars[[p]]$R0[S] * surv[SAY] * HermFrac[SPA] *
      StockPars[[p]]$initdist[SAR] * StockPars[[p]]$Perr_y[Sa]

    # Calculate initial stock biomass
    Biomass[SPAYR] <- N[SPAYR] * StockPars[[p]]$Wt_age[SAY]
    # Calculate spawning stock biomass
    SSB[SPAYR] <- SBsurv[SAY] * StockPars[[p]]$R0[S] * StockPars[[p]]$initdist[SAR] * StockPars[[p]]$Fec_Age[SAY] 
    
    # Calculate vunerable biomass
    VBiomass[SPAYR] <- Biomass[SPAYR] * V[SAY]

    # Assign stock parameters to StockPars object
    StockPars[[p]]$SSBpR <- SSBpR
    StockPars[[p]]$aR <- aR
    StockPars[[p]]$bR <- bR
    StockPars[[p]]$SSB0 <- SSB0
    StockPars[[p]]$SSN0 <- SSN0
    StockPars[[p]]$VB0 <- VB0
    StockPars[[p]]$R0a <- R0a
    StockPars[[p]]$surv <- surv
    StockPars[[p]]$B0 <- B0
    StockPars[[p]]$N0 <- N0

    # loop over fleets
    for(f in 1:nf) {
      FleetPars[[p]][[f]]$V_real<-VF[,p,f,,] # update fleet vulnerability for this stock
      
      # --- Historical Spatial closures ----
      if (!is.null(SampCpars[[p]][[f]]$MPA)) {
        thisMPA <- SampCpars[[p]][[f]]$MPA
        if (any(dim(thisMPA) != c(nyears+proyears, nareas))) {
          stop('cpars$MPA must be a matrix with dimensions `nyears+proyears, nareas`', .call = FALSE)
        }
        if (any(thisMPA != 1 & thisMPA != 0)) {
          stop('values in cpars$MPA must be either 0 (closed) or open (1)', .call = FALSE)
        }
        if (any(thisMPA != 1)) {
          for (a in 1:nareas) {
            yrs <- which(thisMPA[, a] == 0)
            if (length(yrs)) {
              if (!silent) {
                message_info('Spatial closure detected for Stock ' , p, ' and Fleet ', f, 
                             ' in area ', a, ' in years ',
                             paste(findIntRuns(yrs), collapse=", "))
              }
            }
          }
        }
        MPA[p, f,, ] <- thisMPA
      } else {
        if (!is.na(FleetPars[[p]][[f]]$MPA) && all(FleetPars[[p]][[f]]$MPA==TRUE)) {
          MPA[p, f,, 1] <- 0
          if (!silent) message_info('Historical MPA in Area 1 for all years')
        }
      }
      FleetPars[[p]][[f]]$MPA <- MPA[p,f,,]
      
    } # end of loop over fleets
  } # end of loop over stocks
  
  # ---- SexPars - Update SSB0 and Ricker SRR parameters for male stock ----
  # Other parameters have been updated (R0, h, rec devs) earlier
  if (length(SexPars)) {
    if (is.null(SexPars$share_par) || SexPars$share_par == TRUE) {
      # if (!silent) 
      #   cli::cli_alert_info("You have specified sex-specific dynamics, unfished spawning biomass and specified stock depletion will be mirrored across sex types according to {.val SexPars$SSBfrom}")
      sexmatches <- sapply(1:nrow(SexPars$SSBfrom), function(x) paste(SexPars$SSBfrom[x, ], collapse = "_"))
      parcopy <- match(sexmatches, sexmatches)
      StockPars_t <- StockPars # need to store a temporary object for copying to/from
      
      SSB0s <- matrix(NIL(StockPars_t, "SSB0"), nsim, np) # sim, p
      for (p in 1:np) {
        StockPars[[p]]$SSB0 <- apply(matrix(SexPars$SSBfrom[p, ], nsim, np, byrow = TRUE) * SSB0s, 1, sum)
        StockPars[[p]]$SSBpR <- array(StockPars[[p]]$SSB0/StockPars_t[[p]]$R0,c(nsim,nareas)) # SSBpR hardwired to be the same among areas !!!!
        
        # Ricker SR params
        SSB0a <- StockPars[[p]]$SSB0 * StockPars_t[[p]]$R0a/apply(StockPars_t[[p]]$R0a, 1, sum)
        StockPars[[p]]$bR <- matrix(log(5 * StockPars_t[[p]]$hs)/(0.8 * SSB0a), nrow=nsim)
        StockPars[[p]]$aR <- matrix(exp(StockPars[[p]]$bR * SSB0a)/StockPars[[p]]$SSBpR, nrow=nsim)
      }
    }
    if (length(SexPars$Herm)) {
      if (!silent) message_info("You have specified sequential hermaphroditism (SexPars$Herm).",
              "Unfished stock numbers will be calculated from this vector of fractions ",
              "at age. Population dynamics will move individuals from one sex to another.")
    }
  } # end of SexPars loop

  # --- Optimize catchability (q) to fit depletion ----
  optD <- TRUE

  # skip optimization if qs are provided in cpars
  qs <- matrix(NA, nrow = nsim, ncol = np)
  qfrac <- array(1, dim = c(nsim, np, nf))
  for (p in 1:np) {
    for (f in 1:nf) {
      if (!is.null(SampCpars[[p]][[f]]$qs)) {
        optD <- FALSE
        qs[,p] <- SampCpars[[p]][[f]]$qs
        if (all(SampCpars[[p]][[f]]$qs==0)) qfrac[, p, f] <- 0
        FleetPars[[p]][[f]]$qs <- qs[, p] * qfrac[, p, f]
      }
    }
  }
  qs[qs==0] <- 1

  bounds <- c(0.0001, 15) # q bounds for optimizer
  if (optD) {
    exp.time <- (np * nf)/(9*ncpus) * nsim
    exp.time <- round(exp.time,2)
    
    if(!silent)
      cli::cli_alert("Optimizing for user-specified depletion for {nsim} simulations, {np} stocks, and {nf} fleets (could take a while!)")
    
    out <- .lapply(1:nsim, getq_multi_MICE, StockPars, FleetPars, np, nf, nareas,
                   maxage, nyears, N, VF, FretA, maxF=MOM@maxF,
                   MPA,CatchFrac, bounds=bounds,tol=1E-6,HistRel,SexPars,
                   plusgroup=plusgroup, optVB=optVB, silent=silent)

    qs <- NIL(out,"qtot") %>% matrix(nsim, np, byrow = TRUE)
    qfrac <- NIL(out,"qfrac") %>% array(c(np, nf, nsim)) %>% aperm(c(3, 1, 2))

    for (p in 1:np) {
      for (f in 1:nf) {
        FleetPars[[p]][[f]]$qs <- qs[, p] * qfrac[, p, f]
      }
    }
  }


  # --- Check that q optimizer has converged ----
  # bounds for q (catchability). Flag if bounded optimizer hits the bounds
  LimBound <- c(1.1, 0.9)*range(bounds)
  probQ <- which(apply(qs > max(LimBound) | qs < min(LimBound),1,sum)>0)
  Nprob <- length(probQ)

  # If q has hit bound, re-sample depletion and try again.
  # Tries 'ntrials' times and then alerts user
  fracD <- 0.05; ntrials <- 50
  if (!is.null(control$ntrials)) ntrials <- control$ntrials
  if (!is.null(control$fracD)) fracD <- control$fracD

  if (length(probQ) > 0 & optD) {
    Err <- TRUE
    if(!silent) {
      cli::cli_alert_info('{.val {Nprob}} simulations have final biomass that is not close to sampled depletion')
      cli::cli_alert('Re-sampling depletion, recruitment error and fishing effort')
    }

    count <- 0
    MOM2 <- MOM
    while (Err & count < ntrials) {
      Nprob <- length(probQ)
      SampCpars2 <- vector("list", nf)
      for(p in 1:np){
        for(f in 1:nf){
          if(length(cpars)>0 && length(cpars[[p]][[f]])>0){
            # check each list object has the same length and if not stop and error report
            ncparsim <- cparscheck(cpars[[p]][[f]])
            SampCpars2[[f]] <- SampleCpars(cpars[[p]][[f]], Nprob, silent=TRUE)
          }
        }

        ResampStockPars <- SampleStockPars(MOM2@Stocks[[p]],
                                           nsim=Nprob,nyears=nyears,
                                           proyears=proyears,
                                           cpars=SampCpars2[[1]],
                                           msg=FALSE)

        ResampStockPars$CAL_bins <- StockPars[[p]]$CAL_bins
        ResampStockPars$CAL_binsmid <- StockPars[[p]]$CAL_binsmid
        ResampStockPars$nCALbins <- length(StockPars[[p]]$CAL_binsmid )

        # Re-sample depletion
        StockPars[[p]]$D[probQ] <- ResampStockPars$D

        # Re-sample recruitment deviations
        StockPars[[p]]$procsd[probQ] <- ResampStockPars$procsd
        StockPars[[p]]$AC[probQ] <- ResampStockPars$AC
        StockPars[[p]]$Perr_y[probQ,] <- ResampStockPars$Perr_y
        StockPars[[p]]$hs[probQ] <- ResampStockPars$hs
      } # end of P
      # Re-sample historical fishing effort
      ResampFleetPars<- vector("list", nf)
      for(p in 1:np){
        for(f in 1:nf){
          ResampFleetPars <- SampleFleetPars(MOM2@Fleets[[p]][[f]],
                                             Stock=ResampStockPars,
                                             nsim=Nprob,
                                             nyears=nyears,
                                             proyears=proyears,
                                             cpars=SampCpars2[[f]],
                                             msg=FALSE)
          FleetPars[[p]][[f]]$Esd[probQ] <- ResampFleetPars$Esd
          FleetPars[[p]][[f]]$Find[probQ, ] <- ResampFleetPars$Find
          FleetPars[[p]][[f]]$dFfinal[probQ] <- ResampFleetPars$dFfinal
        }
      }

      out2 <- .lapply(probQ,getq_multi_MICE,StockPars, FleetPars, np,nf, nareas,
                      maxage, nyears, N, VF, FretA, maxF=MOM@maxF,
                      MPA,CatchFrac, bounds= bounds,tol=1E-6,HistRel,SexPars,
                      plusgroup=plusgroup, optVB=optVB, silent=silent)

      qs2<-t(matrix(NIL(out2,"qtot"),nrow=np))
      qout2<-array(NIL(out2,"qfrac"),c(np,nf,nsim))
      qfrac2<-array(NA,c(Nprob,np,nf))
      qind2<-TEG(dim(qfrac2))
      qfrac2[qind2]<-qout2[qind2[,c(2,3,1)]]
      qfrac[probQ,,]<-qfrac2
      qs[probQ,]<-qs2

      probQ <- which(apply(qs > max(LimBound) | qs < min(LimBound),1,sum)>0)
      count <- count + 1
      if (length(probQ) == 0) Err <- FALSE

    } # end of while loop
    if (Err) { # still a problem

      tooLow <- length(which(qs > max(LimBound)))
      tooHigh <- length(which(qs < min(LimBound)))
      prErr <- length(probQ)/nsim
      if (prErr > fracD & length(probQ) >= 1) {
        if (length(tooLow) > 0)
          if (!silent) message(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0)
          if (!silent) message(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent)
          message("More than ", fracD*100, "% of simulations can't get to the ",
                  "specified level of depletion with these Operating Model parameters")
        stop("Change OM@seed and try again for a complete new sample, modify the ",
             "input parameters, or increase ntrials")
      } else {
        if (length(tooLow) > 0)
          if (!silent) message(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0)
          if (!silent) message(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent)
          message("More than ", 100-fracD*100, "% simulations can get to the ",
                  "sampled depletion.\nContinuing")
      }
    }
    for(p in 1:np)for(f in 1:nf) FleetPars[[p]][[f]]$qs<-qs[,p]*qfrac[,p,f]
  } # end of re-optimization conditional

  if(!silent)
    cli::cli_alert("Calculating historical stock and fishing dynamics")

  # ---- Run Historical Simulations ----
  histYrs <- .sapply(1:nsim, HistMICE, 
                     StockPars=StockPars,
                     FleetPars=FleetPars,
                     np=np,
                     nf=nf,
                     nareas=nareas,
                     maxage=maxage,
                     nyears=nyears,
                     N=N,
                     VF=VF,
                     FretA=FretA,
                     maxF=MOM@maxF,
                     MPA=MPA,
                     Rel=HistRel,
                     SexPars=SexPars,
                     qs=qs,
                     qfrac=qfrac,
                     plusgroup=plusgroup
                     )

  N <- aperm(array(as.numeric(unlist(histYrs[1,], use.names=FALSE)),
                   dim=c(np,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  Biomass <- aperm(array(as.numeric(unlist(histYrs[2,], use.names=FALSE)),
                         dim=c(np ,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  SSN <- aperm(array(as.numeric(unlist(histYrs[3,], use.names=FALSE)),
                     dim=c(np,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  SSB <- aperm(array(as.numeric(unlist(histYrs[4,], use.names=FALSE)),
                     dim=c(np,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  VBiomass <- aperm(array(as.numeric(unlist(histYrs[5,], use.names=FALSE)),
                          dim=c(np, n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  FM <- aperm(array(as.numeric(unlist(histYrs[6,], use.names=FALSE)),
                    dim=c(np,nf,n_age, nyears, nareas, nsim)), c(6,1,2,3,4,5))
  
  FMret <- aperm(array(as.numeric(unlist(histYrs[7,], use.names=FALSE)),
                       dim=c(np,nf,n_age, nyears, nareas, nsim)), c(6,1,2,3,4,5))
  
  Linfarray <- aperm(array(as.numeric(unlist(histYrs[8,], use.names=FALSE)),
                           dim=c(np, nyears+1, nsim)), c(3,1,2))

  Karray <- aperm(array(as.numeric(unlist(histYrs[9,], use.names=FALSE)),
                        dim=c(np, nyears+1, nsim)), c(3,1,2))

  t0array <- aperm(array(as.numeric(unlist(histYrs[10,], use.names=FALSE)),
                         dim=c(np, nyears+1, nsim)), c(3,1,2))

  Len_age <- aperm(array(as.numeric(unlist(histYrs[11,], use.names=FALSE)),
                         dim=c(np, n_age, nyears+1, nsim)), c(4,1,2,3))

  Wt_age <- aperm(array(as.numeric(unlist(histYrs[12,], use.names=FALSE)),
                        dim=c(np, n_age, nyears+1, nsim)), c(4,1,2,3))
  
  Fec_Age <- aperm(array(as.numeric(unlist(histYrs[21,], use.names=FALSE)),
                        dim=c(np, n_age, nyears+1, nsim)), c(4,1,2,3))

  VBF <- aperm(array(as.numeric(unlist(histYrs[17,], use.names=FALSE)),
                     dim=c(np,nf,n_age, nyears, nareas, nsim)), c(6,1,2,3,4,5))
  
  Z <- aperm(array(as.numeric(unlist(histYrs[18,], use.names=FALSE)),
                   dim=c(np,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  FMt<-aperm(array(as.numeric(unlist(histYrs[19,], use.names=FALSE)),
                   dim=c(np,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  M_ageArray <- aperm(array(as.numeric(unlist(histYrs[20,], use.names=FALSE)),
                            dim=c(np, n_age, nyears+1, nsim)), c(4,1,2,3))
  Marray <- aperm(array(as.numeric(unlist(histYrs[13, ], use.names=FALSE)),
                        dim=c(np, nyears+1, nsim)), c(3,1,2))
  
  spat_targ_out <- array(as.numeric(unlist(histYrs[22,], use.names=FALSE)),
                     dim=c(np, nf, nsim)) 
  # update StockPars (MICE)
  for (p in 1:np) {
    StockPars[[p]]$Linfarray[, 0:nyears + 1] <- Linfarray[, p, ]
    StockPars[[p]]$Karray[, 0:nyears + 1] <- Karray[, p, ]
    StockPars[[p]]$t0array[, 0:nyears + 1] <- t0array[, p, ]
    StockPars[[p]]$Len_age[, , 0:nyears + 1] <- Len_age[, p, , ]
    StockPars[[p]]$Wt_age[, , 0:nyears + 1] <- Wt_age[, p, , ]
    StockPars[[p]]$Fec_Age[, , 0:nyears + 1] <- Fec_Age[, p, , ]
    StockPars[[p]]$M_ageArray[, , 1:nyears] <- M_ageArray[, p, , 1:nyears]
    StockPars[[p]]$Marray[, 1:nyears] <- Marray[, p, 1:nyears]
    
    # update Spat_targ 
    for (fl in 1:nf) {
      FleetPars[[p]][[fl]]$Spat_targ <- spat_targ_out[p,fl,]
    }
  }

  # TODO - selectivity-at-age should update if growth changes
  # Depletion check
  SSB0_specified <- array(NIL(StockPars,'SSB0'),c(nsim, np))
  D_specified <- array(NIL(StockPars,'D'), c(nsim, np))
  if (optVB) {
    VB0_specified <- array(NIL(StockPars,'VB0'),c(nsim,np))
    Depletion <- apply(VBiomass[,,,nyears,,drop = FALSE], 1:2, sum)/ VB0_specified
  } else {
    Depletion <- apply(SSB[,,,nyears,,drop = FALSE], 1:2, sum)/ SSB0_specified
  }

  if (length(SexPars)) {
    if (is.null(SexPars$share_par) || SexPars$share_par == TRUE) { # need to copy over depletion for a sex-specific model
      sexmatches <- sapply(1:nrow(SexPars$SSBfrom), function(x) paste(SexPars$SSBfrom[x, ], collapse = "_"))
      parcopy <- match(sexmatches, sexmatches)
      Depletion[, 1:np] <- Depletion[, parcopy]
    }
  }

  for(p in 1:np) StockPars[[p]]$Depletion <- Depletion[, p]  # add actual Depletion to StockPars

  if (!is.null(control$checks)) {
    if (prod(round(Depletion,2)/ round(D_specified,2)) != 1) {
      print(cbind(round(Depletion, 4), round(D_specified, 4)) %>%
              structure(dimnames = list(Sim = 1:nsim, Depletion = c("Specified", "Estimated"))))
      warning("Possible problem in depletion calculations")
    }
  }

  # --- Calculate MSY statistics for each year ----
  # ignores spatial closures
  # assumes all vulnerable fish are caught - ie no discarding
  if(!silent) cli::cli_alert("Calculating MSY and per-recruit reference points for each year")
  # average life-history parameters over ageM years
  SPR_hist <- list()
  for (p in 1:np) {
    
    V <- local({
      FMt_future <- aperm(replicate(proyears, FMt[,,,nyears,, drop = FALSE]), c(1,2,3,4,6,5)) # Terminal year sel
      FMt_all <- abind::abind(FMt[,p,,,], FMt_future[,p,,1,,], along = 3)
      V <- apply(FMt_all, 1:3, sum) # sum across years and areas, works if areas are evenly distributed
      V[V<=0] <- tiny
      nlz(V, c(1, 3), "max")
    })

    MSYrefsYr <- lapply(1:nsim, function(x) {
      sapply(1:(nyears+proyears), function(y) {
        optMSY_eq(x,
                  yr.ind=y, 
                  StockPars=StockPars[[p]], 
                  V=V)
      })
    })
    
    # --- Annual reference points ----
    StockPars[[p]]$MSY_y <- sapply(MSYrefsYr, function(x) x["Yield", ]) %>% t()
    StockPars[[p]]$FMSY_y <- sapply(MSYrefsYr, function(x) x["F", ]) %>% t()
    StockPars[[p]]$SSBMSY_y <- sapply(MSYrefsYr, function(x) x["SB", ]) %>% t()
    StockPars[[p]]$BMSY_y <- sapply(MSYrefsYr, function(x) x["B", ]) %>% t()
    StockPars[[p]]$VBMSY_y <- sapply(MSYrefsYr, function(x) x["VB", ]) %>% t()

    StockPars[[p]]$R0_y <- sapply(MSYrefsYr, function(x) x["R0", ]) %>% t()
    StockPars[[p]]$h_y <- sapply(MSYrefsYr, function(x) x["h", ]) %>% t()
    StockPars[[p]]$N0_y <- sapply(MSYrefsYr, function(x) x["N0", ]) %>% t()
    StockPars[[p]]$SN0_y <- sapply(MSYrefsYr, function(x) x["SN0", ]) %>% t()
    StockPars[[p]]$B0_y <- sapply(MSYrefsYr, function(x) x["B0", ]) %>% t()
    StockPars[[p]]$SSB0_y <- sapply(MSYrefsYr, function(x) x["SB0", ]) %>% t()
    StockPars[[p]]$VB0_y <- sapply(MSYrefsYr, function(x) x["VB", ]/x["VB_VB0", ]) %>% t()

    # --- MSY reference points ----
    MSYRefPoints <- sapply(1:nsim, CalcMSYRefs,
                           MSY_y=StockPars[[p]]$MSY_y,
                           FMSY_y= StockPars[[p]]$FMSY_y,
                           SSBMSY_y=StockPars[[p]]$SSBMSY_y,
                           BMSY_y=StockPars[[p]]$BMSY_y,
                           VBMSY_y=StockPars[[p]]$VBMSY_y,
                           ageM=StockPars[[p]]$ageM,
                           nyears=nyears)

    MSY <- MSYRefPoints[1,] %>% unlist() # record the MSY results (Vulnerable)
    FMSY <- MSYRefPoints[2,] %>% unlist()  # instantaneous FMSY (Vulnerable)
    SSBMSY <- MSYRefPoints[3,] %>% unlist()  # Spawning Stock Biomass at MSY
    BMSY <- MSYRefPoints[4,] %>% unlist() # total biomass at MSY
    VBMSY <- MSYRefPoints[5,] %>% unlist() # Biomass at MSY (Vulnerable)
    UMSY <- MSY/VBMSY  # exploitation rate
    FMSY_M <- FMSY/StockPars$M  # ratio of true FMSY to natural mortality rate M
    SSBMSY_SSB0 <- SSBMSY/SSB0 # SSBMSY relative to unfished (SSB)
    BMSY_B0 <- BMSY/B0 # Biomass relative to unfished (B0)
    VBMSY_VB0 <- VBMSY/StockPars[[p]]$VB0 # VBiomass relative to unfished (VB0)

    StockPars[[p]]$MSY <- MSY
    StockPars[[p]]$FMSY <- FMSY
    StockPars[[p]]$SSBMSY <- SSBMSY
    StockPars[[p]]$BMSY <- BMSY
    StockPars[[p]]$VBMSY <- VBMSY
    StockPars[[p]]$UMSY <- UMSY
    StockPars[[p]]$FMSY_M <- FMSY_M
    StockPars[[p]]$SSBMSY_SSB0 <- SSBMSY_SSB0
    StockPars[[p]]$FMSY_M <-  StockPars[[p]]$FMSY/StockPars[[p]]$M
    StockPars[[p]]$BMSY_B0 <- BMSY_B0
    StockPars[[p]]$VBMSY_VB0 <- VBMSY_VB0


    # --- Dynamic Unfished Reference Points ---- assumes no MICE rel
    Unfished <- sapply(1:nsim, function(x)
      popdynCPP(nareas, StockPars[[p]]$maxage,
                Ncurr=N[x,p,,1,],
                nyears+proyears,
                M_age=StockPars[[p]]$M_ageArray[x,,],
                Asize_c=StockPars[[p]]$Asize[x,],
                MatAge=StockPars[[p]]$Mat_age[x,,],
                WtAge=StockPars[[p]]$Wt_age[x,,],
                FecAge=StockPars[[p]]$Fec_Age[x,,],
                Vuln=FleetPars[[p]][[1]]$V_real[x,,],
                Retc=FleetPars[[p]][[1]]$retA_real[x,,],
                Prec=StockPars[[p]]$Perr_y[x,],
                movc=split_along_dim(StockPars[[p]]$mov[x,,,,],4),
                SRrelc=StockPars[[p]]$SRrel[x],
                Effind=rep(0, nyears+proyears),
                Spat_targc=FleetPars[[p]][[1]]$Spat_targ[x],
                hc=StockPars[[p]]$hs[x],
                R0c=StockPars[[p]]$R0a[x,],
                SSBpRc=StockPars[[p]]$SSBpR[x,],
                aRc=StockPars[[p]]$aR[x,],
                bRc=StockPars[[p]]$bR[x,],
                Qc=0,
                Fapic=0,
                MPA=MPA[p,f,,],
                maxF=maxF,
                control=1,
                SSB0c=StockPars[[p]]$SSB0[x],
                SRRfun=StockPars[[p]]$SRRfun,
                SRRpars=StockPars[[p]]$SRRpars[[x]],
                plusgroup=StockPars[[p]]$plusgroup,
                spawn_time_frac =StockPars[[p]]$spawn_time_frac[x]))

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


    # ---- Calculate per-recruit reference points ----
    StockPars[[p]]$SSB <- SSB[,p,,,]
    StockPars[[p]]$N <- N[,p,,,]

    F01_YPR_y <- array(0, dim=c(nsim, nyears+proyears)) # store F01 for each sim, and year
    Fmax_YPR_y <- F01_YPR_y # store Fmax for each sim, and year
    Fcrash_y <- F01_YPR_y # store Fcrash for each sim, and year
    SPRcrash_y  <- F01_YPR_y # store SPRcrash for each sim, and year
    Fmed_y <- F01_YPR_y # store Fmed (F that generates the median historical SSB/R) for each sim, and year

    SPR_target <- seq(0.2, 0.6, 0.05)
    
    F_SPR_y <- array(0, dim = c(nsim, length(SPR_target), nyears + proyears)) %>%
      structure(dimnames = list(NULL, paste0("F_", 100*SPR_target, "%"), NULL)) #array of F-SPR% by sim, SPR%, year

    per_recruit_F <- lapply(1:nsim, function(x) {
      lapply(1:(nyears+proyears), function(y) {
        per_recruit_F_calc(x,
                           yr.ind=y,
                           StockPars=StockPars[[p]],
                           V=V,
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
    SPR_hist[[p]] <- list()

    SPR_hist[[p]]$Equilibrium <- CalcSPReq(FMt[,p,,,], StockPars[[p]],
                                      n_age, nareas, nyears, proyears, nsim,
                                      Hist = TRUE)

    SPR_hist[[p]]$Dynamic <- CalcSPRdyn(FMt[,p,,,], StockPars[[p]],
                                   n_age, nareas, nyears, proyears, nsim, Hist = TRUE)

    # ---- Calculate Mean Generation Time ----
    MarrayArea <- replicate(nareas, StockPars[[p]]$M_ageArray[,,1:nyears])
    Mnow<-apply(MarrayArea[,,nyears,]*N[,p,,nyears,],1:2,sum)/apply(N[,p,,nyears,],1:2,sum)
    MGTsurv<-t(exp(-apply(Mnow,1,cumsum)))
    StockPars[[p]]$MGT<-apply(Agearray*(StockPars[[p]]$Mat_age[,,nyears]*MGTsurv),1,sum)/apply(StockPars[[p]]$Mat_age[,,nyears]*MGTsurv,1,sum)

    # ---- Calculate Reference Yield ----
    # if(!silent) message("Calculating reference yield - best fixed F strategy")
    ## TODO - add RefY calcs
    StockPars[[p]]$RefY <- StockPars[[p]]$MSY

    # ---- Store Reference Points ----
    StockPars[[p]]$ReferencePoints <- list(
      ByYear=list(
        N0=StockPars[[p]]$N0_y,
        SN0=StockPars[[p]]$SN0_y,
        B0=StockPars[[p]]$B0_y,
        SSB0=StockPars[[p]]$SSB0_y,
        VB0=StockPars[[p]]$VB0_y,
        R0=StockPars[[p]]$R0_y,
        h=StockPars[[p]]$h_y,
        MSY=StockPars[[p]]$MSY_y,
        FMSY=StockPars[[p]]$FMSY_y,
        SSBMSY=StockPars[[p]]$SSBMSY_y,
        BMSY=StockPars[[p]]$BMSY_y,
        VBMSY=StockPars[[p]]$VBMSY_y,
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
        N0=StockPars[[p]]$N0,
        B0=StockPars[[p]]$B0,
        SSB0=StockPars[[p]]$SSB0,
        SSN0=StockPars[[p]]$SSN0,
        VB0=StockPars[[p]]$VB0,
        MSY=StockPars[[p]]$MSY,
        FMSY=StockPars[[p]]$FMSY,
        SSBMSY=StockPars[[p]]$SSBMSY,
        BMSY=StockPars[[p]]$BMSY,
        VBMSY=StockPars[[p]]$VBMSY,
        UMSY=StockPars[[p]]$UMSY,
        FMSY_M=StockPars[[p]]$FMSY_M,
        SSBMSY_SSB0=StockPars[[p]]$SSBMSY_SSB0,
        BMSY_B0=StockPars[[p]]$BMSY_B0,
        VBMSY_VB0=StockPars[[p]]$VBMSY_VB0,
        RefY=StockPars[[p]]$RefY,
        MGT=StockPars[[p]]$MGT
      )
    )
  }

  # --- Calculate Historical Catch ----
  # Calculate catch-at-age
  # empirical weight-at-age for the catch
  Wt_age_C <- array(NA,c(nsim,np,nf,n_age,nyears,nareas))
  for (p in 1:np) {
    for (f in 1:nf){
      Wt_age_C[,p,f,,,] <- replicate(nareas, FleetPars[[p]][[f]]$Wt_age_C[,,1:nyears])
    }
  }
  CB <- CBret <- Cret <- array(NA,c(nsim,np,nf,n_age,nyears,nareas))
  CNind <- TEG(dim(CB))
  Nind<-CNind[,c(1,2,4,5,6)]  # sim, stock, n_age, nyears, nareas

  Biomass_C <- array(0, dim=dim(FMret))
  Biomass_C[CNind] <- N[Nind] * Wt_age_C[CNind]

  CB[CNind] <- Biomass_C[CNind]*(1-exp(-Z[Nind]))*(FM[CNind]/Z[Nind])
  CB[!is.finite(CB)] <- 0 # fix for Z=0


  if(!is.null(control$checks)) {
    for(p in 1:np){
      Cp <- local({
        num <- apply(CB[, p, , , nyears, , drop = FALSE], c(1, 3), sum) # nsim x nf
        num/rowSums(num)
      })
      if(prod(round(CatchFrac[[p]], 4)/round(Cp, 4)) != 1) {
        warning("Possible problem in catch fraction calculations")
        print("Possible problem in catch fraction calculations")
        print(Snames[p])
        print(cbind(CatchFrac[[p]], round(Cp, 4)) %>% 
                structure(dimnames = list(Sim = 1:nsim, CatchFrac = c("Specified", "Estimated"))))
      }
    }
  }
  
  # Calculate retained-at-age
  Cret[CNind] <- N[Nind] * (1-exp(-Z[Nind])) * (FMret[CNind]/Z[Nind]) #apply(Cret,1:5,sum)
  Cret[!is.finite(Cret)] <- 0

  CBret[CNind] <- Biomass_C[CNind] * (1-exp(-Z[Nind])) * (FMret[CNind]/Z[Nind])
  CBret[!is.finite(CBret)] <- 0

  # Add to FleetPars
  for (p in 1:np) {
    for (f in 1:nf){
      FleetPars[[p]][[f]]$CBret <- CBret[,p,f,,,]
      FleetPars[[p]][[f]]$CB <- CB[,p,f,,,]
      FleetPars[[p]][[f]]$Fishing_Mortality <- apply(FM[,p,f,,,], c(1,3), max)
    }
  }

  # --- Sampling by area ----
  valNames <- c("Catch", 'BInd', 'SBInd', 'VInd', 'RecInd',
                'CAA', 'CAL')
  Sample_Area_array <- array(1, dim=c(nsim, nyears+proyears, nareas))
  Sample_Area <- rep(list(Sample_Area_array), length(valNames))
  names(Sample_Area) <- valNames

  # Not currently working
  # if (!is.null(OM@cpars$Sample_Area)) {
  #   Sample_Area_in <- OM@cpars$Sample_Area
  #   inval <- names(Sample_Area_in)[!names(Sample_Area_in) %in% valNames]
  #   if (length(inval)>0)
  #     stop("Invalid names in OM@cpars$Sample_Area.\nValid names are:\n", paste(valNames, collapse="\n"))
  #
  #   for (nm in names(Sample_Area_in)) {
  #     dd <- dim(Sample_Area_in[[nm]])
  #     if (length(dd)!=4) { # Sample_area from Hist
  #       Sample_Area[[nm]] <- Sample_Area_in[[nm]]
  #       if (any(dim(Sample_Area_in[[nm]]) != c(nsim, nyears+proyears, nareas)))
  #         stop("OM@cpars$Sample_Area$", nm, " must be dimensions: nsim, nareas, nyears+proyears", call. = FALSE)
  #     }
  #   }
  # }

  nms <- c("Catch", "BInd", "SBInd", "VInd", "CAA", "CAL")
  for (nm in nms) {
    dd <- dim(Sample_Area[[nm]])
    if (length(dd)!=4) { # Sample_area from Hist
      temp <- replicate(n_age, Sample_Area[[nm]])
      Sample_Area[[nm]] <- aperm(temp, c(1,4,2,3))
    }
  }

  # --- Populate Data object with Historical Data ----
  CurrentYr <- MOM@Fleets[[1]][[1]]@CurrentYr
  DataList <- new('list')
  for (p in 1:np) {
    DataList[[p]] <- vector('list', nf)
    
    if (!is.null(control$skipdata)) {
      for (f in 1:nf) {
        Data <- new('Data')
        # ---- Add Stock & Fleet Dynamics to Data ----
        StockPars[[p]]$N <- N
        StockPars[[p]]$SSB <- SSB
        StockPars[[p]]$Biomass <- Biomass
        StockPars[[p]]$VBiomass <- VBiomass
        Data@Misc$StockPars <- StockPars[[p]]
        Data@Misc$FleetPars <- FleetPars[[p]][[f]]
        Data@Misc$ReferencePoints <- StockPars[[p]]$ReferencePoints
        DataList[[p]][[f]] <- Data
      }
    } else {
      if (!silent) cli::cli_alert('Generating historical data for Stock: {Snames[p]}')
      
      for (f in 1:nf) {
        # if (!silent) message('Generating historical data for Fleet: ', Fnames[f])
        ObsPars[[p]][[f]]$Sample_Area <- Sample_Area # add to Obs Pars
        
        Data <- makeData(Biomass=Biomass[,p,,,],
                         CBret=CBret[,p,f,,,],
                         Cret=Cret[,p,f,,,],
                         N=N[,p,,,],
                         SSB=SSB[,p,,,],
                         VBiomass=VBF[,p,f,,,],
                         StockPars=StockPars[[p]],
                         FleetPars=FleetPars[[p]][[f]],
                         ObsPars=ObsPars[[p]][[f]],
                         ImpPars=ImpPars[[p]][[f]],
                         RefPoints=StockPars[[p]]$ReferencePoints$ReferencePoints,
                         SampCpars=SampCpars[[p]][[f]],
                         StockPars[[p]]$initD,
                         Sample_Area,
                         Name=paste(Snames[p], Fnames[f, p]),
                         nyears,
                         proyears,
                         nsim,
                         nareas,
                         MOM@reps,
                         CurrentYr,
                         silent=TRUE,
                         control)
        
        
        # ---- Add Stock & Fleet Dynamics to Data ----
        StockPars[[p]]$N <- N
        StockPars[[p]]$SSB <- SSB
        StockPars[[p]]$Biomass <- Biomass
        StockPars[[p]]$VBiomass <- VBiomass
        Data@Misc$StockPars <- StockPars[[p]]
        Data@Misc$FleetPars <- FleetPars[[p]][[f]]
        Data@Misc$ReferencePoints <- StockPars[[p]]$ReferencePoints
        DataList[[p]][[f]] <- Data
      }
    }
  }

  # ---- Condition Simulated Data on input Data object (if it exists) & calculate error stats ----
  Real.Data.Map <- matrix(rep(1:np), nrow=nf, ncol=np, byrow=TRUE)
  if (!is.null(SampCpars[[1]][[1]]$Real.Data.Map)) {
    Real.Data.Map.t <- SampCpars[[1]][[1]]$Real.Data.Map
    # check dimensions
    if (any(dim(Real.Data.Map.t) != c(nf, np))) {
      message_warn('cpars$Real.Data.Map does not have dimensions n.fleet x n.stock. Not being used')
      Real.Data.Map.t <- Real.Data.Map
    }
    Real.Data.Map <- Real.Data.Map.t
  }
  
  # Aggregate comp effective and sample size
  # (only used if fleet/stock data are mapped to each other)
  CAA_ESS_array <- array(0, dim=c(nsim, np, nf))
  CAL_ESS_array <- CAA_nsamp_array <- CAL_nsamp_array <- CAA_ESS_array
  for (p in 1:np) {
    for (f in 1:nf) {
      CAA_ESS_array[,p,f] <- ObsPars[[p]][[f]]$CAA_ESS
      CAL_ESS_array[,p,f] <- ObsPars[[p]][[f]]$CAL_ESS
      CAA_nsamp_array[,p,f] <- ObsPars[[p]][[f]]$CAA_nsamp
      CAL_nsamp_array[,p,f] <- ObsPars[[p]][[f]]$CAL_nsamp
    }
  }

  for (p in 1:np) {
    for (f in 1:nf) {
      if (methods::is(SampCpars[[p]][[f]]$Data,"Data")) {
        # real data has been passed in cpars

        # -- check real data mirroring across stocks ---
        map.stocks <- which(Real.Data.Map[f,] ==p)
        if (length(map.stocks)==0) {
          # stock data have been mapped to another one
          mapped.to <- Real.Data.Map[f,p]
          om <- DataList[[p]][[f]]@OM
          DataList[[p]][[f]] <- new('Data')
          DataList[[p]][[f]]@OM <- om
          ObsPars[[p]][[f]] <- ObsPars[[mapped.to]][[f]]
        } else {
          
          ObsPars[[p]][[f]]$CAA_ESS <- apply(CAA_ESS_array[,map.stocks,, drop=FALSE], 1, sum)
          ObsPars[[p]][[f]]$CAL_ESS <- apply(CAL_ESS_array[,map.stocks, ,drop=FALSE], 1, sum)
          ObsPars[[p]][[f]]$CAA_nsamp <- apply(CAA_nsamp_array[,map.stocks,,drop=FALSE], 1, sum)
          ObsPars[[p]][[f]]$CAL_nsamp <- apply(CAL_nsamp_array[,map.stocks,,drop=FALSE], 1, sum)
          
          # update MPrec (map last catch across mapped stock)
          MPrec <- rep(0, nsim)
          for (i in map.stocks) {
            MPrec <- MPrec+ DataList[[i]][[f]]@MPrec
          }
          
          # update Data and calculate observation error
          updatedData <- AddRealData_MS(SimData=DataList[[p]][[f]],
                                        RealData=SampCpars[[p]][[f]]$Data,
                                        StockPars=StockPars,
                                        FleetPars=FleetPars,
                                        ObsPars=ObsPars,
                                        SampCpars=SampCpars,
                                        map.stocks,
                                        CBret,
                                        VBF,
                                        p,
                                        f,
                                        nsim,
                                        nyears,
                                        proyears,
                                        msg=FALSE,
                                        control)

          if (!is.null(MPrec)) {
            if (is.na(SampCpars[[p]][[f]]$Data@MPrec)) {
              # don't update if MPrec provided in real data
              updatedData$Data@MPrec <- MPrec
            }
          }
          
          DataList[[p]][[f]] <- updatedData$Data
          ObsPars <- updatedData$ObsPar
        }
      }
    }
  }

  multiHist <- vector('list', np)
  
  stock.names <- names(MOM@Stocks)
  fleet.names <- names(MOM@Fleets[[1]])
  
  if (is.null(stock.names)) stock.names <- paste('Stock', rep(1:length(MOM@Stocks)))
  if (is.null(fleet.names)) fleet.names <- paste('Fleet', rep(1:length(MOM@Fleets[[1]])))

  for (p in 1:np) {
    multiHist[[p]] <- vector('list', nf)
    names(multiHist) <- stock.names
    
    for (f in 1:nf) {
      Hist <- new("Hist")
      # Data@Misc <- list()
      Hist@Data <-  DataList[[p]][[f]]
      Hist@Data@Obs <- data.frame() # remove

      ind <- which(lapply(ObsPars[[p]][[f]], length) == nsim)
      obs <- data.frame(ObsPars[[p]][[f]][ind])
      ind <- which(lapply(ImpPars[[p]][[f]], length) == nsim)
      imp <- data.frame(ImpPars[[p]][[f]][ind])
      OMPars <- DataList[[p]][[f]]@OM
      if (nrow(OMPars)<1) {
        OMPars <- data.frame(obs, imp)
      } else {
        OMPars <- data.frame(OMPars, obs, imp)  
      }
      
      Hist@OMPars <- OMPars
      if (f==1) {
        Hist@AtAge <- list(Length=StockPars[[p]]$Len_age,
                           Weight=StockPars[[p]]$Wt_age,
                           Select=FleetPars[[p]][[f]]$V_real_2,
                           Retention=FleetPars[[p]][[f]]$retA_real,
                           Maturity=StockPars[[p]]$Mat_age,
                           N.Mortality=StockPars[[p]]$M_ageArray,
                           Z.Mortality=Z[,p,,,],
                           F.Mortality=FM[,p,f,,,],
                           Fret.Mortality=FMret[,p,f,,,],
                           Number=N[,p,,,],
                           Biomass=Biomass[,p,,,],
                           VBiomass=VBF[,p,f,,,],
                           SBiomass=SSB[,p,,,],
                           Removals=CB[,p,f,,,],
                           Landings=CBret[,p,f,,,],
                           Discards=CB[,p,f,,,]-CBret[,p,f,,,]
        )
      } else {
        Hist@AtAge <- list(Select=FleetPars[[p]][[f]]$V_real_2,
                           Retention=FleetPars[[p]][[f]]$retA_real,
                           Z.Mortality=Z[,p,,,],
                           F.Mortality=FM[,p,f,,,],
                           Fret.Mortality=FMret[,p,f,,,],
                           VBiomass=VBF[,p,f,,,],
                           Removals=CB[,p,f,,,],
                           Landings=CBret[,p,f,,,],
                           Discards=CB[,p,f,,,]-CBret[,p,f,,,]
        )
      }
      if (f==1) {
        Hist@TSdata <- list(
          Number=apply(N[,p,,,],c(1,3,4), sum),
          Biomass=apply(Biomass[,p,,,],c(1,3,4), sum),
          VBiomass=apply(VBF[,p,f,,,],c(1,3,4), sum),
          SBiomass=apply(SSB[,p,,,],c(1,3,4), sum),
          Removals=apply(CB[,p,f,,,], c(1,3,4), sum),
          Landings=apply(CBret[,p,f,,,],c(1,3,4), sum),
          Discards=apply(CB[,p,f,,,]-CBret[,p,f,,,],c(1,3,4), sum),
          Find=FleetPars[[p]][[f]]$Find,
          RecDev=StockPars[[p]]$Perr_y,
          SPR=SPR_hist[[p]],
          Unfished_Equilibrium=Unfished_Equilibrium[[p]]
        )
      } else {
        Hist@TSdata <- list(
          VBiomass=apply(VBF[,p,f,,,],c(1,3,4), sum),
          Removals=apply(CB[,p,f,,,], c(1,3,4), sum),
          Landings=apply(CBret[,p,f,,,],c(1,3,4), sum),
          Discards=apply(CB[,p,f,,,]-CBret[,p,f,,,],c(1,3,4), sum),
          Find=FleetPars[[p]][[f]]$Find
        )
      }
      
      if (f==1) {
        Hist@Ref <- StockPars[[p]]$ReferencePoints  
      }
      
      if (f==1) {
        Hist@SampPars <- list(
          Stock=StockPars[[p]],
          Fleet=FleetPars[[p]][[f]],
          Obs=ObsPars[[p]][[f]],
          Imp=ImpPars[[p]][[f]]
        )
      } else {
        Hist@SampPars <- list(
          Fleet=FleetPars[[p]][[f]],
          Obs=ObsPars[[p]][[f]],
          Imp=ImpPars[[p]][[f]]
        )
      }
      
      if (f==1) {
        temp <- MOM@cpars$Data
        MOM@cpars <- list()
        MOM@cpars$control <- control
        MOM@cpars$Data <- temp  
      } else {
        MOM@cpars <- list()
      }

      Hist@Misc <- list(
        MOM=MOM,
        Stock=stock.names[p],
        Fleet=fleet.names[f]
      )

      multiHist[[p]][[f]] <- Hist
     

    }
    names(multiHist[[p]]) <- fleet.names
  }

  class(multiHist) <- c('multiHist', 'list')

  attr(multiHist, "version") <- packageVersion("MSEtool")
  attr(multiHist, "date") <- date()
  attr(multiHist, "R.version") <- R.version
  attr(multiHist, "Stocks") <- stock.names
  attr(multiHist, "Fleets") <- fleet.names

  multiHist[[1]][[1]]@Misc$Real.Data.Map <- Real.Data.Map
  multiHist

}



#' @describeIn multiMSE Run Forward Projections for a `MOM` object
#' @param multiHist An Historical Simulation object (class `multiHist`)
#' @export
ProjectMOM <- function (multiHist=NULL, MPs=NA, parallel=FALSE, silent=FALSE,
                        checkMPs=FALSE, dropHist=FALSE, extended=FALSE) {

  # ---- Setup ----
  if (! 'multiHist' %in% class(multiHist))
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
  
  Real.Data.Map <- multiHist[[1]][[1]]@Misc$Real.Data.Map
  if (is.null(Real.Data.Map)) 
    Real.Data.Map <- matrix(rep(1:np), nrow=nf, ncol=np, byrow=TRUE)
  
  # ---- Detect MP Specification ----
  MPcond <- "unknown"
  if (!length(Rel) && np == 1 && nf == 1) {
    if (!silent)
      cli::cli_inform(c('!'="runMSE checking: you have specified a single stock and fleet with no MICE relationships.", 
                        "i"="For analysis you should be using {.fun MSEtool::runMSE}. Use this only for debugging  against `runMSE`."))
    if (!methods::is(MPs,"character")) {
      stop('`MPs` must be specified as a vector of character names if there is only',
           ' 1 stock and 1 fleet. `MPs` is currently a ', class(MPs))
    }
    MPcond <- "complex"
    nMP <- length(MPs)
    MPrefs <- array(NA,c(nMP,nf,np))
    MPrefs[] <- unlist(MPs)

    # check class of MPs
    tt <- try(sapply(MPs, getMP), silent=TRUE)
    if (methods::is(tt,'try-error'))
      stop('Error in the MPs -', strsplit(tt,':')[[1]][2])
    MP_classes <- sapply(tt, class)
    if (!all(MP_classes == MP_classes[1]))
      stop('All MPs must be same class (`MP`)')
  }

  if (methods::is(MPs,'character') && MPcond == 'unknown') {
    # check class of MPs
    tt <- try(sapply(MPs, getMP), silent=TRUE)
    if (methods::is(tt,'try-error'))
      stop('Error in the MPs -', strsplit(tt,':')[[1]][2])
    MP_classes <- sapply(tt, class)

    if (any(!MP_classes %in% c('MP', 'MMP'))) {
      inval <- unique(MP_classes[!MP_classes %in% c('MP', 'MMP')])
      stop('Cannot mix MPs of class ', paste0(inval, collapse=","), ' with MPs of class `MP` and `MMP`')
    }

    MP_class <- unique(MP_classes)
    MPcond <- rep(NA, length(MPs))

    if ('MMP' %in% MP_class) {
      if (!silent) 
        cli::cli_inform(c(
          "i"="MMP mode: ",
          "You have specified multi-fleet, multi-stock MPs of class `MMP`. \n\nThis class of MP accepts all data objects (stocks x fleets) to simultaneously make a recommendation specific to each stock and fleet\n"
        ))

      MPcond[which(MP_classes == 'MMP')] <- "MMP"
      nMP <- length(MPs)
      MPrefs <- array(NA,c(nMP,nf,np))
      MPrefs[] <- MPs
    }
    if ('MP' %in% MP_class) {
      if (!silent) 
        cli::cli_inform(c(
          "i"="Complex mode: ",
          "You have specified a vector of MPs rather than a list of MPs, one list position for MP type. \n\nThe same MP will be applied to the aggregate data for all stocks and fleets. The MP will, for example, be used to set a single TAC for all stocks and fleets combined. This will be allocated among fleets according to recent catches and among stocks according to available, vulnerable biomass\n"
        ))
      MPcond[which(MP_classes == 'MP')] <- "complex"
      MPtemp <- MPs
      nMP <- length(MPs)
      MPrefs <- array(NA,c(nMP,nf,np))
      MPrefs[] <- unlist(MPs)
    }
  }

  if (methods::is(MPs,'list') && 'unknown' %in% MPcond) {

    if(identical(ldim(MPs), ldim(Fleets))){
      if (!silent) 
        cli::cli_inform(c(
          "i"="Byfleet mode: ",
          "You have specified an MP for each stock and fleet. Only fleet-specific data (e.g. catches and indices) will be used to set advice for each fleet for each stock\n"
        ))

      MPcond <- "byfleet"
      nMP <- length(MPs[[1]][[1]])
      MPrefs <- array(NA,c(nMP,nf,np))
      MPrefs[]<-unlist(MPs)

    } else if (ldim(MPs)==ldim(Fleets)[1]) { # not a two-tier list
      if (!silent) 
        cli::cli_inform(c(
          "i"="Bystock mode: ",
          "You have specified a vector of MPs for each stock but not a vector of MPs for each stock and fleet. The catch data for these fleets will be combined, a single MP will be used to set a single TAC for all fleets combined that will be allocated between the fleets according to recent catches\n"
        ))
        
      MPcond<-"bystock"
      checkN <- unlist(lapply(MPs, length))
      if (!all(checkN == checkN[1]))
        stop('Must have the same number of MPs for each stock')
      nMP<-length(MPs[[1]])
      MPrefs<-array(NA,c(nMP,nf,np))
      for(p in 1:np)MPrefs[,,p]<-MPs[[p]]
    }

    tt <- try(sapply(unlist(MPs), getMP), silent=TRUE)
    if (methods::is(tt,'try-error'))
      stop('Error in the MPs -', strsplit(tt,':')[[1]][2])
    MP_classes <- sapply(tt, class)
  }

  if ('unknown' %in% MPcond)
    stop('`MPs` is not a vector or list with correct dimensions. See `?multiMSE`')

  if (length(MPcond) != nMP) MPcond <- rep(MPcond, nMP)

  if (methods::is(MPs,"list")) {
    allMPs<-unlist(MPs)
  }else{
    allMPs<-MPs
  }
  if (nMP < 1) stop("No valid MPs found", call.=FALSE)

  # ---- Set up parallel processing of MPs ---
  runparallel <- FALSE 
  if (any(parallel==TRUE)) runparallel <- TRUE
  if (methods::is(parallel, 'list')) runparallel <- TRUE
  
  isrunning <- snowfall::sfIsRunning()
  if (!runparallel & isrunning) snowfall::sfStop()
  
  # Don't run MPs in parallel unless specified
  parallel_MPs <-  rep(FALSE, length(allMPs)) 
  names(parallel_MPs) <- allMPs
  if (methods::is(parallel, 'list')) {
    parallel <- parallel[parallel==TRUE]
    ind <- match(names(parallel), allMPs)
    ind <- ind[!is.na(ind)]
    parallel_MPs[ind] <- TRUE
    names(parallel_MPs) <- allMPs
  }
  
  if (runparallel & any(parallel_MPs)) {
    if (!isrunning) setup()
    Export_customMPs(allMPs)
    if (!silent) message("MPs running in parallel: ", paste0(names(parallel), collapse = ", "))
  }
  
  ncpus <- set_parallel(runparallel, msg=!silent)

  # ---- Check MPs ----
  if (checkMPs && all(MP_classes=='MP')) {
    CheckMPs(MPs=allMPs, silent=silent)
  }
  
  # ---- Set Management Interval for each MP ----
  # TODO - make same structure as MPs argument
  if (length(interval) != nMP) interval <- rep(interval, nMP)[1:nMP]
  if (!all(interval == interval[1])) {
    if (!silent) cli::cli_inform("Variable management intervals:")
    df <- data.frame(MP=MPs,interval=interval)
    for (i in 1:nrow(df)) {
      if (!silent) message(df$MP[i], 'has management interval:', df$interval[i], ifelse(i == nrow(df), "\n", ""))
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
  for (p in 1:np) {
    MSElist[[p]] <- lapply(1:nf, function(f) list(multiHist[[p]][[f]]@Data)[rep(1, nMP)]) # Historical data for this stock and fleet
  }
  
  # TODO - update names of stored values
  SB_SBMSYa <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected SB_SBMSY
  Ba <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected Biomass
  SSBa <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected SSB
  VBa <- array(NA, dim = c(nsim, np, nMP, proyears))  # store the projected vulnerable biomass

  FMa <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected fishing mortality rate
  F_FMSYa <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected F_FMSY
  Ca <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected removed catch
  CaRet <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected retained catch
  TACa <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected TAC recommendation
  TAE_out <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the projected TAE recommendation
  Effort <- array(NA, dim = c(nsim, np, nf, nMP, proyears))  # store the Effort
  
  SPReqa <- array(NA, dim = c(nsim, np, nMP, proyears)) # store the equilibrium Spawning Potential Ratio
  SPRdyna <- array(NA, dim = c(nsim, np, nMP, proyears)) # store the dynamic Spawning Potential Ratio
  
  # ---- Grab Stock, Fleet, Obs and Imp values from Hist ----
  StockPars <- FleetPars <- ObsPars <- ImpPars <- list()
  for(p in 1:np) {
    StockPars[[p]] <- multiHist[[p]][[1]]@SampPars$Stock
    FleetPars[[p]] <- lapply(1:nf, function(f) multiHist[[p]][[f]]@SampPars$Fleet)
    ObsPars[[p]] <- lapply(1:nf, function(f) multiHist[[p]][[f]]@SampPars$Obs)
    ImpPars[[p]] <- lapply(1:nf, function(f) multiHist[[p]][[f]]@SampPars$Imp)
    
    # Subset historical SSB, N, Biomass by stock
    StockPars[[p]]$SSB <- StockPars[[p]]$SSB[, p, , , ]
    StockPars[[p]]$N <- StockPars[[p]]$N[, p, , , ]
    StockPars[[p]]$Biomass <- StockPars[[p]]$Biomass[, p, , , ]
    if (!is.null(StockPars[[p]]$VBiomass)) StockPars[[p]]$VBiomass <- StockPars[[p]]$VBiomass[, p, , , ]
  }

  nareas <- StockPars[[1]]$nareas
  maxage <- StockPars[[1]]$maxage
  n_age <- maxage + 1
  plusgroup <- sapply(1:np, function(p) StockPars[[p]]$plusgroup) # a vector

  # projection arrays for storing all info (by simulation, stock, age, MP, years, areas)
  N_P_mp <- array(NA, dim = c(nsim, np, n_age, nMP, proyears, nareas))
  FMage_mp <- array(NA, dim = c(nsim, np, n_age, nMP, proyears, nareas)) # overall F across fleets
  
  # store M, growth, length at age, rec dev by MP due to MICE rel.
  if (length(Rel)) {
    DV_MICE <- sapply(1:length(Rel), function(r) get_Dnam(Rel[[r]][["terms"]][1]))
    DVarray <- c("M" = "M_ageArray", "a" = "Wt_age", "b" = "Wt_age", #"R0", "hs", 
                 "K" = "Len_age", "Linf" = "Len_age", "t0" = "Len_age", "Perr_y" = "Perr_y")
    
    StockPars_MICE <- lapply(unique(DV_MICE), function(r) {
      if (r == "Perr_y") {
        array(NA_real_, dim = c(nsim, np, nMP, proyears))
      } else {
        array(NA_real_, dim = c(nsim, np, n_age, nMP, proyears))
      }
    }) %>%
      structure(names = DVarray[unique(DV_MICE)])
  }

  # ---- Grab Historical N-at-age etc ----
  N <- array(NA, dim=c(nsim, np, n_age, nyears, nareas))
  Biomass <- SSB <- VBiomass <- N

  FM <- FMret <- array(NA, dim=c(nsim, np, nf, n_age, nyears, nareas))
  VBF <-  array(NA, dim=c(nsim, np, nf, n_age, nyears, nareas))
  VF<-  array(NA, dim=c(nsim, np, nf, n_age, nyears+proyears))
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
      VF[,p,f,,] <- FleetPars[[p]][[f]]$V_real
      VBF[,p,f,,,] <- multiHist[[p]][[f]]@AtAge$VBiomass
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
      cli::cli_alert("{mm}/{nMP} MPs - Running MSE for: ")  # print a progress report
      for(p in 1:np){
        MPrep <- data.frame(MPrefs[mm,,p])
        # row.names(MPrep)<-Fnames[,p]
        # names(MPrep)=Snames[p]
       
        cli::cli_h2(Snames[p])
        for (ff in 1:nrow(Fnames)) {
          cli::cli_text("{Fnames[ff,p]}: {.val  {MPrep[ff,,p]}}")
        }
      }
    }

    checkNA <- array(0,c(np,nf,proyears)) # save number of NAs

    # reset StockPars (MICE), selectivity & retention parameters for projections
    for (p in 1:np) {
      for (f in 1:nf) {
        # reset selectivity parameters for projections
        FleetPars[[p]][[f]]$L5_P <- HistFleetPars[[p]][[f]]$L5
        FleetPars[[p]][[f]]$LFS_P <- HistFleetPars[[p]][[f]]$LFS
        FleetPars[[p]][[f]]$Vmaxlen_P <- HistFleetPars[[p]][[f]]$Vmaxlen
        # selectivity at length array - projections
        FleetPars[[p]][[f]]$SLarray_P <- HistFleetPars[[p]][[f]]$SLarray_real
        #  selectivity at age array - projections
        FleetPars[[p]][[f]]$V_P <- HistFleetPars[[p]][[f]]$V
        FleetPars[[p]][[f]]$V_P_real <- HistFleetPars[[p]][[f]]$V_real  #  selectivity at age array - realized selectivity (max may be less than 1)
        FleetPars[[p]][[f]]$V_P_real_2 <- HistFleetPars[[p]][[f]]$V_real_2  #  selectivity at age array - realized selectivity (max = 1)
        
        # reset retention parameters for projections
        FleetPars[[p]][[f]]$LR5_P <- HistFleetPars[[p]][[f]]$LR5
        FleetPars[[p]][[f]]$LFR_P <- HistFleetPars[[p]][[f]]$LFR
        FleetPars[[p]][[f]]$Rmaxlen_P <- HistFleetPars[[p]][[f]]$Rmaxlen
        # retention at age array - projections
        FleetPars[[p]][[f]]$retA_P <- HistFleetPars[[p]][[f]]$retA # retention at age array - projections
        FleetPars[[p]][[f]]$retA_P_real <- HistFleetPars[[p]][[f]]$retA_real # retention at age array -  realized selectivity (max may be less than 1) 
        FleetPars[[p]][[f]]$retA_P_real_2 <- HistFleetPars[[p]][[f]]$retA_real_2 # asymptote at 1
        
        # retention at length array - projections
        FleetPars[[p]][[f]]$retL_P <- HistFleetPars[[p]][[f]]$retL_real
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
      
      StockPars[[p]] <- multiHist[[p]][[1]]@SampPars$Stock
      
      # Subset historical SSB, N, Biomass by stock
      StockPars[[p]]$SSB <- StockPars[[p]]$SSB[, p, , , ]
      StockPars[[p]]$N <- StockPars[[p]]$N[, p, , , ]
      StockPars[[p]]$Biomass <- StockPars[[p]]$Biomass[, p, , , ]
      if (!is.null(StockPars[[p]]$VBiomass)) StockPars[[p]]$VBiomass <- StockPars[[p]]$VBiomass[, p, , , ]
      
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
    if (!silent) {
      if (requireNamespace("pbapply", quietly = TRUE)) {
        pb <- pbapply::timerProgressBar(min = 1, max = proyears, style = 3, width = min(getOption("width"), 50))
      } else {
        pb <- txtProgressBar(min = 1, max = proyears, style = 3, width = min(getOption("width"), 50))
      }
    }
    
    #### Time-invariant parameters to project one-time step forward. Potential time-varying parameters inside local() call 
    # Matrix nsim x np
    hs <- sapply(1:np, function(p) StockPars[[p]]$hs)
    SRrel <- sapply(1:np, function(p) StockPars[[p]]$SRrel)
    R0 <- sapply(1:np, function(p) StockPars[[p]]$R0)
    SSB0array <- sapply(1:np, function(p) StockPars[[p]]$SSB0)
    B0array <- sapply(1:np, function(p) StockPars[[p]]$B0)
    
    # Vector length p
    a_y <- sapply(1:np, function(p) StockPars[[p]]$a)
    b_y <- sapply(1:np, function(p) StockPars[[p]]$b)
    
    # Array nsim x np x nareas
    aR <- sapply(1:np, function(p) StockPars[[p]]$aR, simplify = "array") %>% aperm(c(1, 3, 2))
    bR <- sapply(1:np, function(p) StockPars[[p]]$bR, simplify = "array") %>% aperm(c(1, 3, 2))
    R0a <- sapply(1:np, function(p) StockPars[[p]]$R0a, simplify = "array") %>% aperm(c(1, 3, 2))
    SSBpR <- sapply(1:np, function(p) StockPars[[p]]$SSBpR, simplify = "array") %>% aperm(c(1, 3, 2))
    Asize <- sapply(1:np, function(p) StockPars[[p]]$Asize, simplify = "array") %>% aperm(c(1, 3, 2))
    
    # Array nsim x np x n_age x nareas x nareas x allyears
    mov <- sapply(1:np, function(p) StockPars[[p]]$mov, simplify = "array") %>% aperm(c(1, 6, 2:5))
    
    # Array nsim x np x nf
    Spat_targ <- sapply(1:np, function(p) {
      sapply(1:nf, function(f) HistFleetPars[[p]][[f]]$Spat_targ)
    }, simplify = "array") %>% aperm(c(1, 3, 2))
    
    # Predict abundance, spawning, recruitment at the beginning of y = nyears+1. 
    # F, M from y = nyears and growth, maturity, recdev, etc. in y = nyears+1
    # MICE rel updates growth, Perr_y for nyears+1, M for nyears
    #
    # note that Fcur is apical F but, in popdynOneMICE it is DIVIDED in future
    # years between the two areas depending on vulnerable biomass.
    # So to get Fcur you need to sum over areas (a bit weird)
    NextYrN <- local({
      # Matrix nsim x np
      Perr <- sapply(1:np, function(p) StockPars[[p]]$Perr_y[, nyears + maxage + 1])
      Knext <- sapply(1:np, function(p) StockPars[[p]]$Karray[, nyears + 1])
      Linfnext <- sapply(1:np, function(p) StockPars[[p]]$Linfarray[, nyears + 1])
      t0next <- sapply(1:np, function(p) StockPars[[p]]$t0array[, nyears + 1])
      
      M <- sapply(1:np, function(p) StockPars[[p]]$Marray[, nyears])
      
      # Array nsim x np x n_age
      M_agecur <- sapply(1:np, function(p) StockPars[[p]]$M_ageArray[, , nyears], simplify = "array") %>% aperm(c(1, 3, 2))
      Mat_agenext <- sapply(1:np, function(p) StockPars[[p]]$Mat_age[, , nyears + 1], simplify = "array") %>% aperm(c(1, 3, 2))
      Fec_agenext <- sapply(1:np, function(p) StockPars[[p]]$Fec_Age[, , nyears + 1], simplify = "array") %>% aperm(c(1, 3, 2))
      Len_agenext <- sapply(1:np, function(p) StockPars[[p]]$Len_age[, , nyears + 1], simplify = "array") %>% aperm(c(1, 3, 2))
      Wt_agenext <- sapply(1:np, function(p) StockPars[[p]]$Wt_age[, , nyears + 1], simplify = "array") %>% aperm(c(1, 3, 2))
      
      SRRfun_p <- lapply(1:np, function(p) StockPars[[p]]$SRRfun)
      
      sapply(1:nsim, function(x) {
        SRRpars_p <- lapply(1:np, function(p) StockPars[[p]]$SRRpars[[x]])
        SexPars_y <- SexPars
        SexPars_y$Herm <- subsetHerm(SexPars_y$Herm, y = nyears+1)
        popdynOneMICE(np = np, nf = nf, nareas = nareas, maxage = maxage,
                      Ncur = array(N[x,,,nyears,], c(np, n_age, nareas)),
                      Bcur = array(Biomass[x,,,nyears,], c(np, n_age, nareas)),
                      SSBcur = array(SSB[x,,,nyears,], c(np, n_age, nareas)),
                      Vcur = array(VF[x,,,,nyears],c(np, nf, n_age)),
                      FMretx = array(FMret[x,,,,nyears,],c(np,nf,n_age,nareas)),
                      FMx = array(FM[x,,,,nyears,], c(np, nf, n_age, nareas)),
                      PerrYrp = Perr[x, ], hsx = hs[x, ], 
                      aRx = array(aR[x, , ], c(np, nareas)),
                      bRx = array(bR[x, , ], c(np, nareas)),
                      movy = array(mov[x,,,,,nyears+1], c(np, n_age, nareas, nareas)),
                      Spat_targ = array(Spat_targ[x, , ], c(np, nf)), SRrelx = SRrel[x, ],
                      M_agecur = array(M_agecur[x, , ], c(np, n_age)),
                      Mat_agenext = array(Mat_agenext[x, , ], c(np, n_age)),
                      Fec_agenext = array(Fec_agenext[x, , ], c(np, n_age)),
                      Asizex = array(Asize[x, , ], c(np, nareas)), 
                      Kx = Knext[x, ], Linfx = Linfnext[x, ], t0x = t0next[x, ], Mx = M[x, ],
                      R0x = R0[x, ], R0ax = array(R0a[x, , ], c(np, nareas)),
                      SSBpRx = array(SSBpR[x, , ], c(np, nareas)), ax = a_y, bx = b_y,
                      Rel = list(), # Do not use MICE. Parameters were updated in last time step of SimulateMOM!
                      SexPars = SexPars_y, x = x,
                      plusgroup = plusgroup, SSB0x = SSB0array[x, ], B0x = B0array[x, ],
                      Len_agenext = array(Len_agenext[x, , ], c(np, n_age)),
                      Wt_agenext = array(Wt_agenext[x, , ], c(np, n_age)),
                      SRRfun=SRRfun_p, SRRpars=SRRpars_p, ycur = nyears)
      })
    })

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
    
    VBF_P[,,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[20,], use.names=FALSE)),
                                 dim=c(np, nf, n_age, nareas, nsim)), c(5,1,2,3,4))
  
    FML <- apply(array(FM[, ,,, nyears, ],c(nsim,np,nf,n_age,nareas)),
                 c(1, 3), max)
    
    for (p in 1:np) {
      StockPars[[p]]$N_P <- N_P[,p,,,]
      StockPars[[p]]$Biomass_P <- Biomass_P[,p,,,]
      StockPars[[p]]$SSN_P <- SSN_P[,p,,,]
      StockPars[[p]]$SSB_P <- SSB_P[,p,,,]
      StockPars[[p]]$VBiomass_P <- VBiomass_P[,p,,,]
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
        MSElist[[p]][[f]][[mm]]@Misc$StockPars <- StockPars[[p]]
        MSElist[[p]][[f]][[mm]]@Misc$FleetPars <- FleetPars[[p]][[f]]
      } # end fleets
    } # end stocks

    # --- Apply MP in initial projection year ----
    # - Combined MP -
    if(MPcond[mm]=="MMP"){
      # returns a hierarchical list object stock then fleet of Data objects
      # DataList<-getDataList(MSElist,mm)
      DataList<-getDataList(MSElist,mm)
      # returns a hierarchical list object stock then fleet then slot type of Rec
      MPRecs_A <- applyMMP(DataList, MP = MPs[mm], reps = 1, silent = TRUE, parallel = parallel_MPs[mm])
      Data_p_A <- MPrecs_A_blank
      for(p in 1:np)for(f in 1:nf){
        Data_p_A[[p]][[f]]<-MSElist[[p]][[f]][[mm]]
        Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC # record TAC rec in Data
        Data_p_A[[p]][[f]]@Misc <- MPRecs_A[[p]][[f]]$Misc
      }
    }else if(MPcond[mm]=="complex"){
      # A temporary blank hierarchical list object stock by fleet
      MPRecs_A <- Data_p_A <- MPrecs_A_blank
      # need this for aggregating data and distributing TACs over stocks
      realVB<-apply(VBiomass[,,,1:nyears,, drop=FALSE],c(1,2,4),sum,na.rm=T)
      curdat<-multiDataS(MSElist,Real.Data.Map,np,mm,nf,realVB)
      runMP <- applyMP(curdat, MPs = MPs[mm], parallel=parallel_MPs[mm],
                       reps = 1, silent=TRUE)  # Apply MP
      chk_run <- try(runMP[[1]][[1]], silent=TRUE)
      if (methods::is(chk_run, 'try-error')) {
        warning('Error applying MP. Returning Data object that failed')
        out <- list()
        out$runMP <- runMP
        out$Data <- curdat
        out$MP <- MPs[mm]
        return(out)
      }
      
      Stock_Alloc<-realVB[,,nyears, drop=FALSE]/apply(realVB[,,nyears, drop=FALSE],1,sum)

      for(p in 1:np)  {
        for(f in 1:nf){
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
      }
      

    }else{
      # A temporary blank hierarchical list object stock by fleet
      MPRecs_A <- Data_p_A <- MPrecs_A_blank
      for(p in 1:np){
        if(MPcond[mm]=="bystock"){
          if(nf>1){
            curdat<-multiData(MSElist,StockPars,p,mm,nf)
          }else{
            curdat<-MSElist[[p]][[f]][[mm]]
          }
          
          runMP <- applyMP(curdat, MPs = MPs[[p]][mm], reps = 1,
                           parallel = parallel_MPs[match(MPs[[p]][mm], names(parallel_MPs))],
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
        }else if(MPcond[mm]=="byfleet"){
          for(f in 1:nf){
            curdat<-MSElist[[p]][[f]][[mm]]
            runMP <- applyMP(curdat, MPs = MPrefs[mm,f,p], reps = 1, 
                                      parallel = parallel_MPs[match(MPrefs[mm,f,p], names(parallel_MPs))],
                                      silent=TRUE)  # Apply MP
            MPRecs_A[[p]][[f]]<-runMP[[1]][[1]]
            Data_p_A[[p]][[f]]<-runMP[[2]]
            Data_p_A[[p]][[f]]@TAC <- MPRecs_A[[p]][[f]]$TAC
          }
        }
      } # end of stocks
    }

    # Update Misc slot in Data
    for (p in 1:np) {
      for (f in 1:nf) {
        MSElist[[p]][[f]][[mm]]@Misc <- Data_p_A[[p]][[f]]@Misc
      }
    }
    
    # Update M if there is a MICE rel for CalcMPDynamics
    StockPars_MPCalc <- StockPars
    if (length(Rel) && any(DV_MICE == "M")) {
      M_MICE <- sapply(1:nsim, function(x) {
        Responses <- ResFromRel(Rel[DV_MICE == "M"], 
                                Bcur = array(Biomass_P[x, , , 1, ], c(np, n_age, nareas)),
                                SSBcur = array(SSB_P[x, , , 1, ], c(np, n_age, nareas)),
                                Ncur = array(N_P[x, , , 1, ], c(np, n_age, nareas)),
                                SSB0 = SSB0array[x, ], B0 = B0array[x, ],
                                seed = 1, x = x, y = nyears + 1)
        
        Dmodnam <- sapply(Responses, getElement, "modnam")
        Dp <- sapply(Responses, getElement, "Dp")
        Dval <- lapply(Responses, getElement, "value")
        Dage <- lapply(Responses, getElement, "age")
        Dmult <- sapply(Responses, getElement, "mult")
        
        oldM_ageArray <- M_ageArray <- sapply(1:np, function(p) StockPars[[p]]$M_ageArray[x, , nyears+1]) %>% t()
        oldMx <- Mx <- sapply(1:np, function(p) StockPars[[p]]$Marray[x, nyears+1])
        
        # Ensure this is consistent with code in popdynOneMICE
        Rel_txt <- sapply(1:length(Responses), function(r) {
          if (all(!is.na(Dage[[r]]))) { # Age-specific M
            Rel_var <- paste0("M_ageArray[", Dp[r], ", Dage[[r]] + 1]")
          } else {
            Rel_var <- paste0(Dmodnam[r], "[", Dp[r], "]")
          }
          if (Dmult[r]) {
            Rel_val <- paste("Dval[[r]] *", Rel_var)
          } else {
            Rel_val <- "Dval[[r]]"
          }
          paste(Rel_var, "<-", Rel_val)
        })
        
        for (r in 1:length(Responses)) {
          eval(parse(text = Rel_txt[r]))
        }
        
        if (all(oldM_ageArray == M_ageArray)) M_ageArray <- oldM_ageArray * Mx/oldMx
        return(M_ageArray)
      }, simplify = "array")
      
      for(p in 1:np) StockPars_MPCalc[[p]]$M_ageArray[, , nyears+1] <- t(M_MICE[p, , ])
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
      }
      
      MPCalcs_MF <- CalcMPDynamics_MF(
        MPRecs_f = MPRecs_A[[p]], y,
        nyears, proyears, nsim,
        LastTAE = array(LastTAE[, p, ], c(nsim, nf)),
        histTAE = array(histTAE[, p, ], c(nsim, nf)),
        LastSpatial = array(LastSpatial[, p, , ], c(nareas, nf, nsim)),
        LastAllocat = array(LastAllocat[, p, ], c(nsim, nf)),
        LastTAC = array(LastCatch[, p, ], c(nsim, nf)),
        TACused = array(TACused[, p, ], c(nsim, nf)),
        maxF = maxF,
        Effort_pot = array(Effort_pot[, p, ], c(nsim, nf)),
        StockPars = StockPars_MPCalc[[p]],
        FleetPars_f = FleetPars[[p]],
        ImpPars_f = ImpPars[[p]],
        control = control
      )
      
      for (f in 1:nf) {
        if(length(SexPars)>0) MPCalcs_MF[[f]] <- MPCalcsNAs(MPCalcs_MF[[f]]) # Zeros caused by SexPars

        TACa[,p,f, mm, y] <- TACused[,p,f]#MPCalcs$TACrec # recommended TAC
        LastSpatial[,p,f,] <- MPCalcs_MF[[f]]$Si
        LastAllocat[,p,f] <- MPCalcs_MF[[f]]$Ai

        LastTAE[,p,f] <- MPCalcs_MF[[f]]$TAE # TAE set by MP
        TAE_out[,p,f, mm, y] <- MPCalcs_MF[[f]]$TAE # TAE
        LastCatch[,p,f] <- MPCalcs_MF[[f]]$TACrec # TAC et by MP

        Effort[,p,f, mm, y] <- rep(MPCalcs_MF[[f]]$Effort,nsim)[1:nsim]
        FleetPars[[p]][[f]]$CB_P <- MPCalcs_MF[[f]]$CB_P # removals
        FleetPars[[p]][[f]]$CB_Pret <- MPCalcs_MF[[f]]$CB_Pret # retained catch
        FleetPars[[p]][[f]]$FM_P <- MPCalcs_MF[[f]]$FM_P # fishing mortality
        FM_P[,p,f,,,]<- MPCalcs_MF[[f]]$FM_P

        FleetPars[[p]][[f]]$FM_Pret <- MPCalcs_MF[[f]]$FM_Pret # retained fishing mortality
        FMret_P[,p,f,,,] <- MPCalcs_MF[[f]]$FM_Pret
        #FretA[,p,f,,] <- MPCalcs_MF[[f]]$FM_Pret
        
        FleetPars[[p]][[f]]$Z_P <- MPCalcs_MF[[f]]$Z_P # total mortality
        FleetPars[[p]][[f]]$retA_P <- MPCalcs_MF[[f]]$retA_P # retained-at-age
        FleetPars[[p]][[f]]$retA_P_real <- MPCalcs_MF[[f]]$retA_P_real 
        FleetPars[[p]][[f]]$retA_P_real_2 <- MPCalcs_MF[[f]]$retA_P_real_2 

        FleetPars[[p]][[f]]$retL_P <- MPCalcs_MF[[f]]$retL_P # retained-at-length
        
        FleetPars[[p]][[f]]$V_P <- MPCalcs_MF[[f]]$V_P  # vulnerable-at-age
        FleetPars[[p]][[f]]$V_P_real <- MPCalcs_MF[[f]]$V_P_real
        FleetPars[[p]][[f]]$V_P_real_2 <- MPCalcs_MF[[f]]$V_P_real_2
        
        VF[,p,f,,]<- MPCalcs_MF[[f]]$V_P
        FleetPars[[p]][[f]]$SLarray_P <- MPCalcs_MF[[f]]$SLarray_P # vulnerable-at-length
        FMa[,p,f,mm,y] <- MPCalcs_MF[[f]]$Ftot # Total fishing mortality (by stock & fleet)
      }
      MPCalcs_list[[p]] <- MPCalcs_MF
    }

    # ---- Account for timing of spawning (if spawn_time_frac >0) ----
    spawn_time_frac <- array(0, dim=c(nsim, np, n_age, nareas))
    for (p in 1:np) {
      spawn_time_frac[,p,,] <-StockPars[[p]]$spawn_time_frac
      F_age <- array(0, dim=c(nsim, n_age, nareas))
      for (f in 1:nf) {
        F_age <- F_age + FleetPars[[p]][[f]]$FM_P[,,y,]
      }
      M_age <- replicate(nareas,StockPars[[p]]$M_ageArray[,,nyears+y])
      Z_age <- M_age+F_age
      N_Psp <- StockPars[[p]]$N_P[,,y,]
      for (a in 1:n_age) {
        N_Psp[,a,] <- N_Psp[,a,] * exp(-(Z_age[,a,]*spawn_time_frac[,p,a,]))
      }
      SSB_P[,p,,y,] <- N_Psp * replicate(nareas,StockPars[[p]]$Fec_Age[,,nyears+y])
      SSN_P[,p,,y,] <- N_Psp * replicate(nareas,StockPars[[p]]$Mat_age[,,nyears+y])
    }

    # recruitment
    for (p in 1:np) {
      StockPars[[p]]$SSN_P <- SSN_P[,p,,,]
      StockPars[[p]]$SSB_P <- SSB_P[,p,,,]
      if (length(SexPars$SSBfrom)) {  # use SSB from another stock to predict recruitment
        
        SSBcurr <- local({
          SSBfrom <- array(SexPars$SSBfrom[p, ], c(np, nsim, n_age, nareas)) %>% 
            aperm(c(2, 1, 3, 4))
          apply(SSBfrom * SSB_P[,,,y,], c(1, 2), sum) # nsim x np; contribution of each stock p' to p
        })
      } else {
        SSBcurr <- apply(SSB_P[, p, , y, ], c(1, 3), sum) # nsim x nareas
      }
      recdev <- StockPars[[p]]$Perr_y[, y+nyears+n_age-1]
      rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars[[p]]$SRrel, 
                         SSBcurr=SSBcurr,
                         recdev=recdev, hs=StockPars[[p]]$hs,
                         aR= StockPars[[p]]$aR, bR=StockPars[[p]]$bR, R0a=StockPars[[p]]$R0a,
                         SSBpR=StockPars[[p]]$SSBpR,
                         SRRfun=StockPars[[p]]$SRRfun,
                         SRRpars=StockPars[[p]]$SRRpars)
      
      StockPars[[p]]$N_P[,1,y,] <- N_P[,p,1,y,] <- t(rec_area)
    }


    # the years in which there are updates
    upyrs <- 1 + (0:(floor(proyears/interval[mm]) - 1)) * interval[mm]
   
    # --- Begin projection years ----
    for (y in 2:proyears) {
      if (!silent) setTxtProgressBar(pb, y) # Works with pbapply

      # -- Calculate MSY stats for this year ----
      # if selectivity has changed
      for (p in 1:np) {
        for (f in 1:nf) {
          SelectChanged <- FALSE
          if (any(
            FleetPars[[p]][[f]]$retA_P[,,nyears+y] - FleetPars[[p]][[f]]$retA_P[,,nyears+y] !=0))  SelectChanged <- TRUE
          if (any(
            FleetPars[[p]][[f]]$V_P[,,nyears+y] - FleetPars[[p]][[f]]$V_P[,,nyears+y] !=0))  SelectChanged <- TRUE


          # recalculate MSY ref points because selectivity has changed
          V_Pt[,f,,]<-FleetPars[[p]][[f]]$V_P*
            apply(CB[,p,f,,nyears,], 1, sum) # Weighted by catch frac
        }
        if (SelectChanged) {
          #summed over fleets and normalized to 1
          V_P<-nlz(apply(V_Pt,c(1,3,4),sum),c(1,3),"max")
          y1 <- nyears + y
          MSYrefsYr <- sapply(1:nsim, optMSY_eq,         
                              yr.ind=y1,
                              StockPars=StockPars[[p]],
                              V=V_P)
          MSY_y[,p,mm,y1] <- MSYrefsYr[1,]
          FMSY_y[,p,mm,y1] <- MSYrefsYr[2,]
          SSBMSY_y[,p,mm,y1] <- MSYrefsYr[3,]
          BMSY_y[,p,mm,y1] <- MSYrefsYr[6,]
          VBMSY_y[,p,mm,y1] <- MSYrefsYr[7,]
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
      
      # Predict abundance at the beginning of y with F, M from y-1 and
      # growth, maturity, recdev, etc. in y
      #
      # note that Fcur is apical F but, in popdynOneMICE it is DIVIDED in future
      # years between the two areas depending on vulnerable biomass. So to get
      # Fcur you need to sum over areas (a bit weird)
      NextYrN <- local({ # Calculate SSB in year y from abundance and mortality in y-1
        # Matrix nsim x np
        Perr <- sapply(1:np, function(p) StockPars[[p]]$Perr_y[, nyears + maxage + y])
        Knext <- sapply(1:np, function(p) StockPars[[p]]$Karray[, nyears + y])
        Linfnext <- sapply(1:np, function(p) StockPars[[p]]$Linfarray[, nyears + y])
        t0next <- sapply(1:np, function(p) StockPars[[p]]$t0array[, nyears + y])
        
        M <- sapply(1:np, function(p) StockPars[[p]]$Marray[, nyears + y - 1])
        
        # Array nsim x np x n_age
        M_agecur <- sapply(1:np, function(p) StockPars[[p]]$M_ageArray[, , nyears + y - 1], simplify = "array") %>% aperm(c(1, 3, 2))
        Mat_agenext <- sapply(1:np, function(p) StockPars[[p]]$Mat_age[, , nyears + y], simplify = "array") %>% aperm(c(1, 3, 2))
        Fec_agenext <- sapply(1:np, function(p) StockPars[[p]]$Fec_Age[, , nyears + y], simplify = "array") %>% aperm(c(1, 3, 2))
        Len_agenext <- sapply(1:np, function(p) StockPars[[p]]$Len_age[, , nyears + y], simplify = "array") %>% aperm(c(1, 3, 2))
        Wt_agenext <- sapply(1:np, function(p) StockPars[[p]]$Wt_age[, , nyears + y], simplify = "array") %>% aperm(c(1, 3, 2))
        
        SRRfun_p <- lapply(1:np, function(p) StockPars[[p]]$SRRfun)
        
        sapply(1:nsim, function(x) {
          SRRpars_p <- lapply(1:np, function(p) StockPars[[p]]$SRRpars[[x]])
          SexPars_y <- SexPars
          SexPars_y$Herm <- subsetHerm(SexPars_y$Herm, y = nyears+y)
          popdynOneMICE(np = np, nf = nf, nareas = nareas, maxage = maxage,
                        Ncur = array(N_P[x,,,y-1,], c(np, n_age, nareas)),
                        Bcur = array(Biomass_P[x,,,y-1,], c(np, n_age, nareas)),
                        SSBcur = array(SSB_P[x,,,y-1,], c(np, n_age, nareas)),
                        Vcur = array(VF[x,,,,nyears+y-1],c(np, nf, n_age)),
                        FMretx = array(FMret_P[x,,,,y-1,],c(np,nf,n_age,nareas)),
                        FMx = array(FM_P[x,,,,y-1,], c(np, nf, n_age, nareas)),
                        PerrYrp = Perr[x, ], hsx = hs[x, ], 
                        aRx = array(aR[x, , ], c(np, nareas)),
                        bRx = array(bR[x, , ], c(np, nareas)),
                        movy = array(mov[x,,,,,nyears+y], c(np, n_age, nareas, nareas)),
                        Spat_targ = array(Spat_targ[x, , ], c(np, nf)), SRrelx = SRrel[x, ],
                        M_agecur = array(M_agecur[x, , ], c(np, n_age)),
                        Mat_agenext = array(Mat_agenext[x, , ], c(np, n_age)),
                        Fec_agenext = array(Fec_agenext[x, , ], c(np, n_age)),
                        Asizex = array(Asize[x, , ], c(np, nareas)), 
                        Kx = Knext[x, ], Linfx = Linfnext[x, ], t0x = t0next[x, ], Mx = M[x, ],
                        R0x = R0[x, ], R0ax = array(R0a[x, , ], c(np, nareas)),
                        SSBpRx = array(SSBpR[x, , ], c(np, nareas)), ax = a_y, bx = b_y,
                        Rel = Rel,
                        SexPars = SexPars_y, x = x,
                        plusgroup = plusgroup, SSB0x = SSB0array[x, ], B0x = B0array[x, ],
                        Len_agenext = array(Len_agenext[x, , ], c(np, n_age)),
                        Wt_agenext = array(Wt_agenext[x, , ], c(np, n_age)),
                        SRRfun=SRRfun_p, SRRpars=SRRpars_p, ycur = nyears+y-1)
        })
      })

      N_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[1,], use.names=FALSE)),
                              dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      Biomass_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[23,], use.names=FALSE)),
                                    dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      SSN_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[24,], use.names=FALSE)),
                                dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      SSB_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[25,], use.names=FALSE)),
                                dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))

      VBiomass_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[19,], use.names=FALSE)),
                                     dim=c(np,n_age, nareas, nsim)), c(4,1,2,3))
      
      VBF_P[,,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[20,], use.names=FALSE)),
                                 dim=c(np, nf, n_age, nareas, nsim)), c(5,1,2,3,4))
      
      FML <- apply(array(FM_P[, ,,, y-1, ],c(nsim,np,nf,n_age,nareas)),
                   c(1, 3), max)
      
      if (length(Rel)) {
        Linfarray <- aperm(array(as.numeric(unlist(NextYrN[8,], use.names=FALSE)),
                                 dim=c(np, nsim)), c(2,1))
        
        Karray <- aperm(array(as.numeric(unlist(NextYrN[9,], use.names=FALSE)),
                              dim=c(np, nsim)), c(2,1))
        
        t0array <- aperm(array(as.numeric(unlist(NextYrN[10,], use.names=FALSE)),
                               dim=c(np, nsim)), c(2,1))
        
        Len_age <- aperm(array(as.numeric(unlist(NextYrN[14,], use.names=FALSE)),
                               dim=c(np, n_age, nsim)), c(3,1,2))
        
        Wt_age <- aperm(array(as.numeric(unlist(NextYrN[15,], use.names=FALSE)),
                              dim=c(np, n_age, nsim)), c(3,1,2))
        
        Fec_Age <- aperm(array(as.numeric(unlist(NextYrN[26,], use.names=FALSE)),
                               dim=c(np, n_age, nsim)), c(3,1,2))
        
        M_ageArray <- aperm(array(as.numeric(unlist(NextYrN[2,], use.names=FALSE)),
                                  dim=c(np, n_age, nsim)), c(3,1,2))
        Marray <- aperm(array(as.numeric(unlist(NextYrN[11, ], use.names=FALSE)),
                              dim=c(np, nsim)), c(2,1))
        
        Perr_y <- aperm(array(as.numeric(unlist(NextYrN[27, ], use.names=FALSE)),
                              dim=c(np, nsim)), c(2,1))
      }

      for (p in 1:np) {
        StockPars[[p]]$N_P <- N_P[,p,,,]
        StockPars[[p]]$Biomass_P <- Biomass_P[,p,,,]
        StockPars[[p]]$SSN_P <- SSN_P[,p,,,]
        StockPars[[p]]$SSB_P <- SSB_P[,p,,,]
        StockPars[[p]]$VBiomass_P <- VBiomass_P[,p,,,]
        #for(f in 1:nf)FleetPars[[p]][[f]]$FML<-FML[]
        
        if (length(Rel)) {
          StockPars[[p]]$Linfarray[, nyears + y] <- Linfarray[, p]
          StockPars[[p]]$Karray[, nyears + y] <- Karray[, p]
          StockPars[[p]]$t0array[, nyears + y] <- t0array[, p]
          StockPars[[p]]$Len_age[, , nyears + y] <- Len_age[, p, ]
          StockPars[[p]]$Wt_age[, , nyears + y] <- Wt_age[, p, ]
          StockPars[[p]]$Fec_Age[, , nyears + y] <- Fec_Age[, p, ]
          StockPars[[p]]$M_ageArray[, , nyears + y - 1] <- M_ageArray[, p, ]
          StockPars[[p]]$Marray[, nyears + y - 1] <- Marray[, p]
          StockPars[[p]]$Perr_y[, nyears + maxage + y] <- Perr_y[, p]
        }
      }
      
      # Update M in year y (one step ahead) if there is a MICE rel for CalcMPDynamics (not used in updateData)
      StockPars_MPCalc <- StockPars
      if (length(Rel) && any(DV_MICE == "M")) {
        M_MICE <- sapply(1:nsim, function(x) {
          Responses <- ResFromRel(Rel[DV_MICE == "M"], 
                                  Bcur = array(Biomass_P[x, , , 1, ], c(np, n_age, nareas)),
                                  SSBcur = array(SSB_P[x, , , 1, ], c(np, n_age, nareas)),
                                  Ncur = array(N_P[x, , , 1, ], c(np, n_age, nareas)),
                                  SSB0 = SSB0array[x, ], B0 = B0array[x, ],
                                  seed = 1, x = x, y = nyears+y)
          
          Dmodnam <- sapply(Responses, getElement, "modnam")
          Dp <- sapply(Responses, getElement, "Dp")
          Dval <- lapply(Responses, getElement, "value")
          Dage <- lapply(Responses, getElement, "age")
          Dmult <- sapply(Responses, getElement, "mult")
          
          oldM_ageArray <- M_ageArray <- sapply(1:np, function(p) StockPars[[p]]$M_ageArray[x, , nyears + y]) %>% t()
          oldMx <- Mx <- sapply(1:np, function(p) StockPars[[p]]$Marray[x, nyears + y])
          
          Rel_txt <- sapply(1:length(Responses), function(r) {
            if (all(!is.na(Dage[[r]]))) { # Age-specific M
              Rel_var <- paste0("M_ageArray[", Dp[r], ", Dage[[r]] + 1]")
            } else {
              Rel_var <- paste0(Dmodnam[r], "[", Dp[r], "]")
            }
            if (Dmult[r]) {
              Rel_val <- paste("Dval[[r]] *", Rel_var)
            } else {
              Rel_val <- "Dval[[r]]"
            }
            paste(Rel_var, "<-", Rel_val)
          })
          
          for (r in 1:length(Responses)) {
            eval(parse(text = Rel_txt[r]))
          }
          
          if (all(oldM_ageArray == M_ageArray)) M_ageArray <- oldM_ageArray * Mx/oldMx
          return(M_ageArray)
        }, simplify = "array")
        
        for(p in 1:np) StockPars_MPCalc[[p]]$M_ageArray[, , nyears + y] <- t(M_MICE[p, , ])
      }

      # 
      CBret_P <- array(0, dim=c(nsim, np, nf, n_age, proyears, nareas))
      for (p in 1:np) {
        for(f in 1:nf) {
          if (!is.null(control$TAC)) {
            if (control$TAC=="removals") {
              CBret_P[,p,f,,,] <-FleetPars[[p]][[f]]$CB_P    
            }
          } else {
            CBret_P[,p,f,,,] <-FleetPars[[p]][[f]]$CB_Pret  
          }
        }
      }
    
      # --- An update year ----
      if (y %in% upyrs) {
        # --- Update Data object ----
        for (p in 1:np) {
          for (f in 1:nf) {
            OM <- suppressMessages(new('OM', docheck=FALSE)) # temporary while MSEtool::makeData requires this
            OM@nyears <- nyears
            OM@hbiascv <- MOM@Obs[[p]][[f]]@hbiascv
            OM@maxF <- MOM@maxF
            OM@CurrentYr <- MSElist[[1]][[1]][[1]]@LHYear
            OM@reps <- MOM@reps
            OM@nsim <- nsim
            OM@BMSY_B0biascv <- MOM@Obs[[p]][[f]]@BMSY_B0biascv
            OM@proyears <- proyears
            
            # -- check real data mirroring across stocks ---
            map.stocks <- which(Real.Data.Map[f,] ==p)
            if (length(map.stocks)==0) {
              # stock data have been mapped to another one
              mapped.to <- Real.Data.Map[f,p]
              om <- MSElist[[p]][[f]][[mm]]@OM
              MSElist[[p]][[f]][[mm]] <- new('Data') 
              MSElist[[p]][[f]][[mm]]@OM <- om
            } else {
              # update MPrec (map last catch across mapped stock)
              MPrec <- rep(0, nsim)
              for (i in map.stocks) {
                MPrec <- MPrec+ MPCalcs_list[[i]][[f]]$TACrec
              }
        
              MSElist[[p]][[f]][[mm]] <- updateData_MS(Data=MSElist[[p]][[f]][[mm]],
                                                    OM,
                                                    MPCalcs=MPCalcs_list[[p]][[f]],
                                                    Effort=Effort[,p,f,, ,drop=FALSE],
                                                    Biomass=Biomass,
                                                    N=N,
                                                    Biomass_P=Biomass_P,
                                                    CB_Pret=CBret_P,
                                                    N_P=N_P,
                                                    SSB=SSB,
                                                    SSB_P=SSB_P,
                                                    VBiomass=VBF,
                                                    VBiomass_P=VBF_P,
                                                    StockPars,
                                                    FleetPars,
                                                    ObsPars,
                                                    ImpPars,
                                                    upyrs=upyrs,
                                                    interval=interval,
                                                    y=y,
                                                    mm=mm,
                                                    Misc=MSElist[[p]][[f]][[mm]]@Misc,
                                                    RealData=multiHist[[p]][[f]]@Data,
                                                    p, f,
                                                    map.stocks)

              
            }
            
            MSElist[[p]][[f]][[mm]]@Misc$StockPars <- StockPars[[p]]
            MSElist[[p]][[f]][[mm]]@Misc$FleetPars <- FleetPars[[p]][[f]]
            

            # ---- Update true abundance ----
            M_array <- array(0.5*StockPars[[p]]$M_ageArray[,,nyears+y],
                             dim=c(nsim, n_age, nareas))
            Atemp <- apply(StockPars[[p]]$VBiomass_P[, , y, ] *
                             exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
            MSElist[[p]][[f]][[mm]]@OM$A <- Atemp
            if (!is.null(MPrec)) MSElist[[p]][[f]][[mm]]@MPrec <- MPrec
          
          } # end of fleet
        } # end of stock
        
        if(MPcond[mm]=="MMP"){
          # returns a hierarchical list object stock then fleet of Data objects
          DataList <- getDataList(MSElist,mm)
          # # returns a hierarchical list object stock then fleet then slot type of Rec
          MPRecs_A <- applyMMP(DataList, MP = MPs[mm], reps = 1, silent = TRUE, parallel = parallel_MPs[mm])
          Data_p_A <- MPrecs_A_blank
          for(p in 1:np)for(f in 1:nf){
            Data_p_A[[p]][[f]]<-MSElist[[p]][[f]][[mm]]
            Data_p_A[[p]][[f]]@TAC<-MPRecs_A[[p]][[f]]$TAC # record TAC rec in Data
            Data_p_A[[p]][[f]]@Misc <- MPRecs_A[[p]][[f]]$Misc
          }
          
        }else if(MPcond[mm]=="complex"){
          # A temporary blank hierarchical list object stock by fleet
          MPRecs_A <- Data_p_A <- MPrecs_A_blank
          # need this for aggregating data and distributing TACs over stocks
          realVB<-abind::abind(apply(VBiomass[,,,1:nyears,, drop=FALSE],c(1,2,4),sum,na.rm=T),
                               apply(VBiomass_P[,,,1:(y-1), , drop=FALSE],c(1,2,4),sum,na.rm=T),
                               along=3)

          curdat <- multiDataS(MSElist,Real.Data.Map,np,mm,nf,realVB)
          runMP <- applyMP(curdat, MPs = MPs[mm], reps = 1, parallel=parallel_MPs[mm], silent=TRUE)  # Apply MP
          
          Stock_Alloc <- realVB[,,nyears, drop=FALSE]/
            apply(realVB[,,nyears, drop=FALSE],1,sum)

          chk_run <- try(runMP[[1]][[1]], silent=TRUE)
          if (methods::is(chk_run, 'try-error')) {
            warning('Error applying MP. Returning Data object that failed')
            out <- list()
            out$runMP <- runMP
            out$Data <- curdat
            out$MP <- MPs[mm]
            return(out)
          }
          
          for(p in 1:np) for(f in 1:nf){
            MPRecs_A[[p]][[f]] <- runMP[[1]][[1]]
            MPRecs_A[[p]][[f]]$TAC <- runMP[[1]][[1]]$TAC *
              MOM@Allocation[[p]][,f] * Stock_Alloc[,p,1]
            MPRecs_A[[p]][[f]]$Effort <- runMP[[1]][[1]]$Effort * MOM@Efactor[[p]][,f]

            if(length(MPRecs_A[[p]][[f]]$Effort)>0)
              if(all(is.na(MPRecs_A[[p]][[f]]$Effort[1,])))
                MPRecs_A[[p]][[f]]$Effort <- matrix(NA,
                                                    nrow=0,
                                                    ncol=ncol(MPRecs_A[[p]][[f]]$Effort))
            if(length(MPRecs_A[[p]][[f]]$TAC)>0)
              if(all(is.na(MPRecs_A[[p]][[f]]$TAC[1,])))
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

            if(MPcond[mm]=="bystock"){
              if(nf>1){
                curdat<-multiData(MSElist,StockPars,p,mm,nf)
              }else{
                curdat<-MSElist[[p]][[f]][[mm]]
              }
              runMP <- applyMP(curdat, MPs = MPs[[p]][mm], reps = MOM@reps, 
                               parallel = parallel_MPs[match(MPs[[p]][mm], names(parallel_MPs))],
                               silent=TRUE)  # Apply MP
              
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
            } else if(MPcond[mm]=="byfleet"){
              for(f in 1:nf){
                curdat<-MSElist[[p]][[f]][[mm]]
                runMP <- applyMP(curdat, MPs = MPrefs[mm,f,p],
                                 parallel = parallel_MPs[match(MPrefs[mm,f,p], names(parallel_MPs))],
                                 reps = MOM@reps, silent=TRUE)  # Apply MP
                MPRecs_A[[p]][[f]]<-runMP[[1]][[1]]
                Data_p_A[[p]][[f]]<-runMP[[2]]
                Data_p_A[[p]][[f]]@TAC <- MPRecs_A[[p]][[f]]$TAC
              }
            } # end of MPcond conditional
          } # end of stock loop
        } # end of MMP


        # Update Misc slot in Data
        for (p in 1:np) {
          for (f in 1:nf) {
            MSElist[[p]][[f]][[mm]]@Misc <- Data_p_A[[p]][[f]]@Misc
          }
        }
        
        for(p in 1:np){
          for(f in 1:nf){
            # calculate pstar quantile of TAC recommendation dist
            TACused[,p,f] <- apply(Data_p_A[[p]][[f]]@TAC, 2, quantile,
                                   p = MOM@pstar, na.rm = T)
            checkNA[p,f,y] <-checkNA[p,f,y] + sum(is.na(TACused[,p,f]))
          }
          
          MPCalcs_MF <- CalcMPDynamics_MF(
            MPRecs_f = MPRecs_A[[p]], y,
            nyears, proyears, nsim,
            LastTAE = array(LastTAE[, p, ], c(nsim, nf)),
            histTAE = array(histTAE[, p, ], c(nsim, nf)),
            LastSpatial = array(LastSpatial[, p, , ], c(nareas, nf, nsim)),
            LastAllocat = array(LastAllocat[, p, ], c(nsim, nf)),
            LastTAC = array(LastCatch[, p, ], c(nsim, nf)),
            TACused = array(TACused[, p, ], c(nsim, nf)),
            maxF = maxF,
            Effort_pot = array(Effort_pot[, p, ], c(nsim, nf)),
            StockPars = StockPars_MPCalc[[p]],
            FleetPars_f = FleetPars[[p]],
            ImpPars_f = ImpPars[[p]],
            control = control
          )
          
          for (f in 1:nf) {
            if(length(SexPars)>0) MPCalcs_MF[[f]] <- MPCalcsNAs(MPCalcs_MF[[f]]) # Zeros caused by SexPars
            
            TACa[,p,f, mm, y] <- MPCalcs_MF[[f]]$TACrec # recommended TAC
            LastSpatial[,p,f,] <- MPCalcs_MF[[f]]$Si
            LastAllocat[,p,f] <- MPCalcs_MF[[f]]$Ai
            LastTAE[,p,f] <- MPCalcs_MF[[f]]$TAE # adjustment to TAE
            TAE_out[,p,f, mm, y] <- MPCalcs_MF[[f]]$TAE # TAE
            LastCatch[,p,f] <- MPCalcs_MF[[f]]$TACrec # TAC et by MP
            
            Effort[,p,f, mm, y] <- rep(MPCalcs_MF[[f]]$Effort,nsim)[1:nsim]
            FleetPars[[p]][[f]]$CB_P <- MPCalcs_MF[[f]]$CB_P # removals
            FleetPars[[p]][[f]]$CB_Pret <- MPCalcs_MF[[f]]$CB_Pret # retained catch
            FleetPars[[p]][[f]]$FM_P <- MPCalcs_MF[[f]]$FM_P # fishing mortality
            FM_P[,p,f,,,] <- MPCalcs_MF[[f]]$FM_P
            
            FleetPars[[p]][[f]]$FM_Pret <- MPCalcs_MF[[f]]$FM_Pret # retained fishing mortality
            FMret_P[,p,f,,,] <- MPCalcs_MF[[f]]$FM_Pret
            #FretA[,p,f,,] <- MPCalcs_MF[[f]]$FM_Pret

            FleetPars[[p]][[f]]$Z_P <- MPCalcs_MF[[f]]$Z_P # total mortality
            FleetPars[[p]][[f]]$retA_P <- MPCalcs_MF[[f]]$retA_P # retained-at-age
            FleetPars[[p]][[f]]$retA_P_real <- MPCalcs_MF[[f]]$retA_P_real 
            FleetPars[[p]][[f]]$retA_P_real_2 <- MPCalcs_MF[[f]]$retA_P_real_2
            
            FleetPars[[p]][[f]]$retL_P <- MPCalcs_MF[[f]]$retL_P # retained-at-length
            
            FleetPars[[p]][[f]]$V_P <- MPCalcs_MF[[f]]$V_P  # vulnerable-at-age
            FleetPars[[p]][[f]]$V_P_real <- MPCalcs_MF[[f]]$V_P_real
            FleetPars[[p]][[f]]$V_P_real_2 <- MPCalcs_MF[[f]]$V_P_real_2
            
            VF[,p,f,,]<- MPCalcs_MF[[f]]$V_P
            FleetPars[[p]][[f]]$SLarray_P <- MPCalcs_MF[[f]]$SLarray_P # vulnerable-at-length
            FMa[,p,f,mm,y] <- MPCalcs_MF[[f]]$Ftot # Total fishing mortality (by stock & fleet)
            
            #MPCalcs_list[[p]][[f]] <- MPCalcs
          } # end of fleets
          
          MPCalcs_list[[p]] <- MPCalcs_MF
        } # end of stocks

        # end of update year
      } else {
        # ---- Not an update year ----
        for(p in 1:np){
          NoMPRecs <- MPRecs_A[[p]]
          for(f in 1:nf) NoMPRecs[[f]]$Spatial <- NA
          
          MPCalcs_MF <- CalcMPDynamics_MF(
            MPRecs_f = NoMPRecs, y,
            nyears, proyears, nsim,
            LastTAE = array(LastTAE[, p, ], c(nsim, nf)),
            histTAE = array(histTAE[, p, ], c(nsim, nf)),
            LastSpatial = array(LastSpatial[, p, , ], c(nareas, nf, nsim)),
            LastAllocat = array(LastAllocat[, p, ], c(nsim, nf)),
            LastTAC = array(LastCatch[, p, ], c(nsim, nf)),
            TACused = array(TACused[, p, ], c(nsim, nf)),
            maxF = maxF,
            Effort_pot = array(Effort_pot[, p, ], c(nsim, nf)),
            StockPars = StockPars_MPCalc[[p]],
            FleetPars_f = FleetPars[[p]],
            ImpPars_f = ImpPars[[p]],
            control = control
          )
          
          for (f in 1:nf) {

            if(length(SexPars)>0) MPCalcs_MF[[f]] <- MPCalcsNAs(MPCalcs_MF[[f]]) # Zeros caused by SexPars
            
            TACa[,p,f, mm, y] <- TACused[,p,f] # recommended TAC
            LastSpatial[,p,f,] <- MPCalcs_MF[[f]]$Si
            LastAllocat[,p,f] <- MPCalcs_MF[[f]]$Ai
            LastTAE[,p,f] <- MPCalcs_MF[[f]]$TAE # adjustment to TAE
            TAE_out[,p,f, mm, y] <- MPCalcs_MF[[f]]$TAE # TAE
            LastCatch[,p,f] <- MPCalcs_MF[[f]]$TACrec # TAC et by MP

            Effort[,p,f, mm, y] <- rep(MPCalcs_MF[[f]]$Effort,nsim)[1:nsim]
            FleetPars[[p]][[f]]$CB_P <- MPCalcs_MF[[f]]$CB_P # removals
            FleetPars[[p]][[f]]$CB_Pret <- MPCalcs_MF[[f]]$CB_Pret # retained catch
            
            FleetPars[[p]][[f]]$FM_P <- MPCalcs_MF[[f]]$FM_P # fishing mortality
            FM_P[,p,f,,,] <- MPCalcs_MF[[f]]$FM_P
            FleetPars[[p]][[f]]$FM_Pret <- MPCalcs_MF[[f]]$FM_Pret # retained fishing mortality
            FMret_P[,p,f,,,] <- MPCalcs_MF[[f]]$FM_Pret
            #FretA[,p,f,,] <- MPCalcs_MF[[f]]$FM_Pret
            
            FleetPars[[p]][[f]]$Z_P <- MPCalcs_MF[[f]]$Z_P # total mortality
            FleetPars[[p]][[f]]$retA_P <- MPCalcs_MF[[f]]$retA_P # retained-at-age
            FleetPars[[p]][[f]]$retA_P_real <- MPCalcs_MF[[f]]$retA_P_real 
            FleetPars[[p]][[f]]$retA_P_real_2 <- MPCalcs_MF[[f]]$retA_P_real_2
            
            FleetPars[[p]][[f]]$retL_P <- MPCalcs_MF[[f]]$retL_P # retained-at-length
            
            FleetPars[[p]][[f]]$V_P <- MPCalcs_MF[[f]]$V_P  # vulnerable-at-age
            FleetPars[[p]][[f]]$V_P_real <- MPCalcs_MF[[f]]$V_P_real
            FleetPars[[p]][[f]]$V_P_real_2 <- MPCalcs_MF[[f]]$V_P_real_2
            
            VF[,p,f,,]<- MPCalcs_MF[[f]]$V_P
            FleetPars[[p]][[f]]$SLarray_P <- MPCalcs_MF[[f]]$SLarray_P # vulnerable-at-length
            FMa[,p,f,mm,y] <- MPCalcs_MF[[f]]$Ftot # Total fishing mortality (by stock & fleet)

          } # end of fleets
        } # end of stocks
      } # end of not update year
      
      
      # ---- Account for timing of spawning (if spawn_time_frac >0) ----
      for (p in 1:np) {
        spawn_time_frac[,p,,] <-StockPars[[p]]$spawn_time_frac
        F_age <- array(0, dim=c(nsim, n_age, nareas))
        for (f in 1:nf) {
          F_age <- F_age + FleetPars[[p]][[f]]$FM_P[,,y,]
        }
        M_age <- replicate(nareas,StockPars[[p]]$M_ageArray[,,nyears+y])
        Z_age <- M_age+F_age
        N_Psp <- StockPars[[p]]$N_P[,,y,]
        for (a in 1:n_age) {
          N_Psp[,a,] <- N_Psp[,a,] * exp(-(Z_age[,a,]*spawn_time_frac[,p,a,]))
        }
        SSB_P[,p,,y,] <- N_Psp * replicate(nareas,StockPars[[p]]$Fec_Age[,,nyears+y])
        SSN_P[,p,,y,] <- N_Psp * replicate(nareas,StockPars[[p]]$Mat_age[,,nyears+y])
      }
      
      # recruitment
      for (p in 1:np) {
        StockPars[[p]]$SSN_P <- SSN_P[,p,,,]
        StockPars[[p]]$SSB_P <- SSB_P[,p,,,]
        if (length(SexPars$SSBfrom)) {  # use SSB from another stock to predict recruitment
          SSBcurr <- local({
            SSBfrom <- array(SexPars$SSBfrom[p, ], c(np, nsim, n_age, nareas)) %>% 
              aperm(c(2, 1, 3, 4))
            apply(SSBfrom * SSB_P[,,,y,], c(1, 2), sum) # nsim x np; contribution of each stock p' to p
          })
        } else {
          SSBcurr <- apply(SSB_P[, p, , y, ], c(1, 3), sum) # nsim x nareas
        }
        recdev <- StockPars[[p]]$Perr_y[, y+nyears+n_age-1]
        rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars[[p]]$SRrel, 
                           SSBcurr=SSBcurr,
                           recdev=recdev, hs=StockPars[[p]]$hs,
                           aR= StockPars[[p]]$aR, bR=StockPars[[p]]$bR, R0a=StockPars[[p]]$R0a,
                           SSBpR=StockPars[[p]]$SSBpR,
                           SRRfun=StockPars[[p]]$SRRfun,
                           SRRpars=StockPars[[p]]$SRRpars)
        
        StockPars[[p]]$N_P[,1,y,] <- N_P[,p,1,y,] <- t(rec_area)
      }
    
    } # end of projection years
    
    if(!silent) close(pb) # use pbapply::closepb(pb) for shiny related stuff
    
    if (max(upyrs) < proyears) { # One more call to complete Data object
      # --- Update Data object ----
      upyrs2 <- upyrs[upyrs>0] 
      upyrs2 <- c(upyrs2, proyears)
      
      if (length(upyrs2) < 3 && !silent) message("Updating the Data object for the final time...")
      
      interval[mm] <- proyears
      for (p in 1:np) {
        for (f in 1:nf) {
          OM <- suppressMessages(new('OM', docheck=FALSE)) 
          OM@nyears <- nyears
          OM@hbiascv <- MOM@Obs[[p]][[f]]@hbiascv
          OM@maxF <- MOM@maxF
          OM@CurrentYr <- MSElist[[1]][[1]][[1]]@LHYear
          OM@reps <- MOM@reps
          OM@nsim <- nsim
          OM@BMSY_B0biascv <- MOM@Obs[[p]][[f]]@BMSY_B0biascv
          OM@proyears <- proyears
          
          # -- check real data mirroring across stocks ---
          map.stocks <- which(Real.Data.Map[f,] ==p)
          if (length(map.stocks)==0) {
            # stock data have been mapped to another one
            mapped.to <- Real.Data.Map[f,p]
            om <- MSElist[[p]][[f]][[mm]]@OM
            MSElist[[p]][[f]][[mm]] <- new('Data') 
            MSElist[[p]][[f]][[mm]]@OM <- om
          } else {
        
            # update MPrec (map last catch across mapped stock)
            MPrec <- rep(0, nsim)
            for (i in map.stocks) {
              MPrec <- MPrec+ MPCalcs_list[[i]][[f]]$TACrec
            }
            
            MSElist[[p]][[f]][[mm]] <- updateData_MS(Data=MSElist[[p]][[f]][[mm]],
                                                     OM,
                                                     MPCalcs=MPCalcs_list[[p]][[f]],
                                                     Effort=Effort[,p,f,, ,drop=FALSE],
                                                     Biomass=Biomass,
                                                     N=N,
                                                     Biomass_P=Biomass_P,
                                                     CB_Pret=CBret_P,
                                                     N_P=N_P,
                                                     SSB=SSB,
                                                     SSB_P=SSB_P,
                                                     VBiomass=VBF,
                                                     VBiomass_P=VBF_P,
                                                     StockPars,
                                                     FleetPars,
                                                     ObsPars,
                                                     ImpPars,
                                                     upyrs=upyrs2,
                                                     interval=interval,
                                                     y=y,
                                                     mm=mm,
                                                     Misc=MSElist[[p]][[f]][[mm]]@Misc,
                                                     RealData=multiHist[[p]][[f]]@Data,
                                                     p, f,
                                                     map.stocks)
            
            
          }
          
          MSElist[[p]][[f]][[mm]]@Misc$StockPars <- StockPars[[p]]
          MSElist[[p]][[f]][[mm]]@Misc$FleetPars <- FleetPars[[p]][[f]]
            
            
            
          # ---- Update true abundance ----
          M_array <- array(0.5*StockPars[[p]]$M_ageArray[,,nyears+y],
                           dim=c(nsim, n_age, nareas))
          Atemp <- apply(StockPars[[p]]$VBiomass_P[, , y, ] *
                           exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
          MSElist[[p]][[f]][[mm]]@OM$A <- Atemp
        } # end of fleet
      } # end of stock
    }
  
    # SSB relative to SSBMSY
    SB_SBMSYa[, ,mm, ] <- apply(SSB_P, c(1,2, 4), sum, na.rm=TRUE)/array(SSBMSY_y[,,mm,(nyears+1):(nyears+proyears)],
                                                                       c(nsim,np,proyears))

    # for(p in 1:np) for(f in 1:nf)
    #   FMa[,p,f, mm, ] <- -log(1 - apply(FleetPars[[p]][[f]]$CB_P, c(1, 3), sum,
    #                                     na.rm=TRUE)/apply(VBiomass_P[,p,,,]+
    #                                                         FleetPars[[p]][[f]]$CB_P,
    #                                                       c(1, 3), sum, na.rm=TRUE))
    for(f in 1:nf)
      F_FMSYa[, ,f,mm, ] <- FMa[,,f, mm, ]/FMSY_y[,,mm,(nyears+1):(nyears+proyears)]

    Ba[, ,mm, ] <- apply(Biomass_P, c(1, 2,4), sum, na.rm=TRUE) # biomass
    SSBa[, ,mm, ] <- apply(SSB_P, c(1, 2,4), sum, na.rm=TRUE) # spawning stock biomass
    VBa[, ,mm, ] <- apply(VBiomass_P, c(1, 2, 4), sum, na.rm=TRUE) # vulnerable biomass

    N_P_mp[,,, mm,,] <- N_P

    for(p in 1:np) {
      for(f in 1:nf) {
        Ca[, p,f,mm, ] <- apply(FleetPars[[p]][[f]]$CB_P, c(1, 3), sum, na.rm=TRUE) # removed
        CaRet[, p,f,mm, ] <- apply(FleetPars[[p]][[f]]$CB_Pret, c(1, 3), sum, na.rm=TRUE) # retained catch
      }
      
      FMage_mp[, p, , mm, , ] <- FleetPars[[p]][[1]]$Z_P - replicate(nareas, StockPars_MPCalc[[p]]$M_ageArray[, , nyears + seq(1, proyears)])
      
      SPReqa[, p, mm, ] <- CalcSPReq(FM = FMage_mp[, p, , mm, , ],
                                     StockPars_MPCalc[[p]], n_age, nareas, nyears, proyears, nsim)
      
      SPRdyna[, p, mm, ] <- local({
        FMh <- array(FM[, p, , , , ], c(nsim, nf, n_age, nyears, nareas)) %>% apply(c(1, 3:5), sum)
        FMp <- FMage_mp[, p, , mm, , ]
        
        CalcSPRdyn(abind::abind(FMh, FMp, along = 3), 
                   StockPars_MPCalc[[p]], n_age, nareas, nyears, proyears, nsim)
      })
    }

    if (!silent) {
      if (all(checkNA != nsim) & !all(checkNA == 0)) {
        # print number of NAs
        # message(checkNA)
        # message(checkNA[upyrs])
        ntot <- sum(checkNA[,,upyrs])
        totyrs <- sum(checkNA[,,upyrs] >0)
        nfrac <- round(ntot/(length(upyrs)*nsim),2)*100
        
        cat("\n")
        message(totyrs, ' years had TAC = NA for some simulations (', nfrac, "% of total simulations)")
        message('Used TAC_y = TAC_y-1')
      }

      if("progress"%in%names(control))
        if(control$progress)
          shiny::incProgress(1/nMP, detail = round(mm*100/nMP))
    }
    
    if (length(Rel)) {
      for (r in names(StockPars_MICE)) {
        if (r == "Perr_y") {
          StockPars_MICE[[r]][, , mm, ] <- sapply(1:np, function(p) {
            StockPars_MPCalc[[p]][[r]][, maxage + nyears + 1:proyears]
          }, simplify = "array") %>% aperm(c(1, 3, 2)) 
        } else {
          StockPars_MICE[[r]][, , , mm, ] <- sapply(1:np, function(p) {
            StockPars_MPCalc[[p]][[r]][, , nyears + 1:proyears]
          }, simplify = "array") %>% aperm(c(1, 4, 2, 3)) 
        }
      }
    }

  } # end of MP loop

  OM<-Obsout<-list()
  for(p in 1:np) {
    OM[[p]]<-Obsout[[p]]<-list()

    for(f in 1:nf) {
      OM[[p]][[f]]<-MSElist[[p]][[f]][[1]]@OM
      Obsout[[p]][[f]]<-MSElist[[p]][[f]][[1]]@Obs

      # remove MSElist Misc
      if (!extended) {
        for (mm in 1:nMP) {
          MSElist[[p]][[f]][[mm]]@Misc$StockPars <- NULL
          MSElist[[p]][[f]][[mm]]@Misc$FleetPars <- NULL
          MSElist[[p]][[f]][[mm]]@Misc$ReferencePoints <- NULL
        }
      }
    }
  }
  Misc <- list()
  # Misc$Data <-MSElist
  
  if (length(Rel)) {
    if (!silent) message("Returning updated parameters from MICE relationship in 'Misc$MICE'")
    Misc[["MICE"]] <- StockPars_MICE # Update for potential values updated by MICE
  }
  
  if (extended) {
    
    if (!silent) message("Returning overall F in 'Misc$extended$FM'")
    Misc[["extended"]] <- list(
      FM = FMage_mp  # nsim x np x n_age x nMP x proyears x nareas
    )
    
    Misc[['MOM']] <- MOM
  }

  # need to reformat MMP and complex mode to work with MSEout slot
  if(methods::is(MPs,"character")) MPs<-list(MPs)

  # ---- Create MMSE Object ----

  # add all reference points from multiHist to MMSE@RefPoint
  # and remove from multiHist
  # this is done so MMSE@multiHist can be dropped to save a ton of memory
  # while still preserving ref points in the MMSE object

  RefPoint <- list()
  RefPoint$ByYear <- list(MSY=MSY_y,
                          FMSY=FMSY_y,
                          SSBMSY=SSBMSY_y,
                          BMSY=BMSY_y,
                          VBMSY=VBMSY_y)
  # add other ref points
  nms <- names(multiHist[[1]][[1]]@Ref$ByYear)
  nms <- nms[!nms %in% names( RefPoint$ByYear)]
  # drop SPR, Fcrash etc - not updated in projection and could change if selectivity changes
  nms <- c(nms[grepl('0', nms)], 'h')
  nms <- nms[!nms=="F01_YPR"]

  for (nm in nms) {
    RefPoint$ByYear[[nm]] <- array(NA, dim=c(nsim, np, nMP, proyears+nyears))
    for (p in 1:np) {
      for(mm in 1:nMP) {
        RefPoint$ByYear[[nm]][,p,mm,] <- multiHist[[p]][[1]]@Ref$ByYear[[nm]]
      }
    }
  }
  
  RefPoint$Dynamic_Unfished <- list()
  nms <- names(multiHist[[1]][[1]]@Ref$Dynamic_Unfished)
  # add Dynamic_Unfished
  ### NOTE: Should be updated by MICE rel, e.g., M/Perr_y/growth have changed in the projection !
  for (nm in nms) {
    RefPoint$Dynamic_Unfished[[nm]] <- array(NA, dim=c(nsim, np,  proyears+nyears))
    for (p in 1:np) {
      RefPoint$Dynamic_Unfished[[nm]][,p,] <- multiHist[[p]][[1]]@Ref$Dynamic_Unfished[[nm]]
    }
  }
  # remove Ref from MultiHist
  for (p in 1:np) {
    for(f in 1:nf) {
      multiHist[[p]][[f]]@Ref <- list('Now in MMSE@RefPoint')
    }
  }

  if (dropHist) {
    multiHist <- structure(
      'multiHist dropped (dropHist=TRUE). Reference points available in MMSE@RefPoint',
      class = 'multiHist'
    )
  }
  
  MSEout <- new("MMSE",
                Name = MOM@Name,
                nyears,
                proyears,
                nMPs=nMP,
                MPs=MPs,
                MPcond=MPcond,
                MPrefs=MPrefs,
                nsim,
                nstocks=np,
                nfleets=nf,
                Snames=Snames,
                Fnames=Fnames,
                Stocks=Stocks,
                Fleets=Fleets,
                Obss=Obs,
                Imps=Imps,
                OM=OM,
                Obs=Obsout,
                SB_SBMSY=SB_SBMSYa,
                F_FMSY=F_FMSYa,
                N=N_P_mp, # apply(N_P_mp, c(1,2,4,5), sum),
                B=Ba,
                SSB=SSBa,
                VB=VBa,
                FM=FMa,
                SPR=list(Equilibrium = SPReqa, Dynamic = SPRdyna),
                Catch=CaRet,
                Removals=Ca,
                Effort = Effort,
                TAC=TACa,
                TAE=TAE_out,
                BioEco=list(),
                RefPoint=RefPoint,
                multiHist=multiHist,
                PPD=MSElist,
                Misc=Misc)
  MSEout
}



#' Run a multi-fleet multi-stock Management Strategy Evaluation
#'
#' Functions for running a multi-stock and/or multi-fleet Management
#' Strategy Evaluation (closed-loop simulation) for a specified operating model
#'
#' @param MOM A multi-fleet multi-stock operating model (class [MSEtool::MOM-class])
#' @param MPs A matrix of methods (nstock x nfleet) (character string) of class MP
#' @param Hist Should model stop after historical simulations? Returns a list
#' containing all historical data
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical or a named list. Should MPs be run using parallel processing?  See Details for more information.  
#' @param checkMPs Logical. Check if the specified MPs exist and can be run on `SimulatedData`?
#' @param dropHist Logical. Drop the (very large) `multiHist` object from the returned `MMSE` object?
#' The `multiHist` object can be (re-)created using `SimulateMOM` or kept in `MMSE@multiHist` if
#' `dropHist=FALSE`
#' @param extended Logical. Return extended projection results?
#' if TRUE, `MMSE@Misc$extended` is a named list with extended data: FM for overall F across fleets `[nsim, nstock, n_age, nMP, proyears, nareas]`.
#' `Misc` slot in `MMSE@PPD` will also contain StockPars, FleetPars, and ReferencePoints
#' @describeIn multiMSE Run a multi-stock, multi-fleet MSE
#' 
#' @details 
#' ## Running MPs in parallel
#' 
#' For most MPs, running in parallel can actually lead to an increase in computation time, due to the overhead in sending the 
#' information over to the cores. Consequently, by default the MPs will not be run in parallel if `parallel=TRUE` 
#' (although other internal code will be run in parallel mode).
#' 
#' To run MPs in parallel, specify a named list with the name of the MP(s) assigned as TRUE. For example,`parallel=list(AvC=TRUE`)
#' will run the `AvC` MP in parallel mode.
#
#' @return  Functions return objects of class `MMSE` and `multiHist`
#' #' \itemize{
#'   \item SimulateMOM - An object of class `multiHist`
#'   \item ProjectMOM - An object of class `MMSE`
#'   \item multiMSE - An object of class `MMSE`
#' }
#' @author T. Carruthers and A. Hordyk
#' @export
multiMSE <- function(MOM=MSEtool::Albacore_TwoFleet,
                     MPs=list(list(c("AvC","DCAC"),c("FMSYref","curE"))),
                     Hist=FALSE,
                     silent=FALSE,
                     parallel=TRUE,
                     checkMPs=FALSE,
                     dropHist=TRUE,
                     extended=FALSE) {

  # ---- Initial Checks and Setup ----
  if (methods::is(MOM,'MOM')) {
    if (MOM@nsim <=1) stop("MOM@nsim must be > 1", call.=FALSE)

  } else if ('multiHist' %in% class(MOM)) {
    stop("You must specify an operating model of class `MOM`")

    # if (!silent) message("Using `multiHist` object to reproduce historical dynamics")
    #
    # # --- Extract cpars from Hist object ----
    # nstocks <- length(MOM)
    # nfleets <- length(MOM[[1]])
    #
    # cpars <- list()
    # cpars[[1]] <- list()
    #
    # for (s in 1:nstocks) {
    #   for (f in 1:nfleets) {
    #     cpars[[s]][[f]] <- c(MOM[[s]][[f]]@SampPars$Stock,
    #                          MOM[[s]][[f]]@SampPars$Fleet,
    #                          MOM[[s]][[f]]@SampPars$Obs,
    #                          MOM[[s]][[f]]@SampPars$Imp,
    #                          MOM[[s]][[f]]@OMPars,
    #                          MOM[[s]][[f]]@OM@cpars)
    #   }
    # }
    #
    # # --- Populate a new OM object ----
    # newMOM <- MOM[[1]][[1]]@Misc$MOM
    # newMOM@cpars <- cpars
    # MOM <- newMOM

  } else {
    stop("You must specify an operating model of class `MOM`")
  }

  if (checkMPs) {
    allMPs <- unique(unlist(MPs))
    CheckMPs(MPs=allMPs, silent=silent)
  }

  multiHist <- SimulateMOM(MOM, parallel, silent)

  if (Hist) {
    if(!silent) message("Returning historical simulations")
    return(multiHist)
  }

  if(!silent) message("Running forward projections")

  MSEout <- try(ProjectMOM(multiHist=multiHist, MPs, parallel, silent, checkMPs=FALSE,
                           dropHist=dropHist,extended=extended), silent=TRUE)

  if (methods::is(MSEout,'try-error')) {
    message('The following error occured when running the forward projections: ',
            crayon::red(attributes(MSEout)$condition))
    message('Returning the historical simulations (class `multiHist`). To avoid re-running spool up, ',
            'the forward projections can be run with ',
            '`ProjectMOM(multiHist, MPs, ...)`')
    return(multiHist)
  }
  MSEout
}



