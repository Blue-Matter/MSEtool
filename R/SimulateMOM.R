
#' Simulate historical dynamics for multi-OM
#'
#' @param MOM 
#' @param parallel 
#' @param silent 
#'
#' @return
#' @export
#'
SimulateMOM <- function(MOM=Albacore_TwoFleet, parallel=TRUE, silent=FALSE) {
  # ---- Initial Checks and Setup ----
  if (class(MOM) == 'MOM') {
    if (MOM@nsim <=1) stop("MOM@nsim must be > 1", call.=FALSE)
    
  } else if (class(MOM) == 'Hist') {
    
    stop('Not working yet')
    # if (!silent) message("Using `Hist` object to reproduce historical dynamics")
    # 
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
    stop("You must specify an operating model of class `MOM`")
  }
  
  # ---- Set up parallel processing ----
  if (parallel) {
    if (snowfall::sfIsRunning()) {
      ncpus <- snowfall::sfCpus()
    } else {
      setup()
      ncpus <- snowfall::sfCpus()
    }
  } else {
    ncpus <- 1
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
  
  if(np==1 & nf==1){
    message("You have specified only a single stock and fleet. ",
            "You should really be using the function OMtool::runMSE()")
  } else if(np>1 & length(MOM@Rel)==0 & length(MOM@SexPars)==0) {
    message("You have specified more than one stock but no MICE relationships ",
            "(slot MOM@Rel) or sex-specific relationships (slot MOM@SexPars) among these. ",
            "As they are independent, consider doing MSE for one stock at a time for ",
            "computational efficiency.")
  }
  
  maxF <- MOM@maxF
  Snames <- SIL(Stocks,"Name")
  Fnames <- matrix(make.unique(SIL(Fleets,"Name")),nrow=nf)
  cpars <- MOM@cpars
  
  # ---- Custom Parameters (cpars) Options ----
  control <- cpars$control; cpars$control <- NULL
  
  optVB <- FALSE
  if (!is.null(control$D) && control$D == "VB") optVB <- TRUE
  
  # Allocation
  if(length(MOM@Allocation)==0){
    MOM@Allocation <- CatchFrac
    message("Slot @Allocation of MOM object not specified. Setting slot ",
            "@Allocation equal to slot @CatchFrac - current catch fractions")
  }
  
  if(length(MOM@Efactor)==0){
    MOM@Efactor <- list()
    for(p in 1:np) MOM@Efactor[[p]]<- array(1,c(nsim,nf))
    message("Slot @Efactor of MOM object not specified. Setting slot @Efactor ",
            "to current effort for all fleets")
  }
  
  # All stocks and sampled parameters must have compatible array sizes (maxage)
  maxage_s <- unique(SIL(MOM@Stocks,"maxage"))
  if (length(maxage_s)>1)
    message(paste("Stocks of varying maximum ages have been specified,",
                  "all simulations will run to",max(maxage_s),"ages"))
  maxage <- max(maxage_s)
  for(p in 1:np) MOM@Stocks[[p]]@maxage<-maxage

  if(!silent) message("Loading operating model")
  StockPars<-FleetPars<-ObsPars<-ImpPars<- SampCpars<-new('list')
  
  plusgroup <- rep(1, np)
  
  # custom parameters exist - sample and write to list
  for(p in 1:np){
    SampCpars[[p]]<-list()
    if(!silent) message(Stocks[[p]]@Name)
    for(f in 1:nf){
      # --- Sample custom parameters ----
      if(length(cpars)>0 && length(cpars[[p]][[f]])>0){
        # TODO - skip non-stochastic ----
        # ncparsim <- cparscheck(cpars[[p]][[f]]) 
        SampCpars[[p]][[f]] <- SampleCpars(cpars[[p]][[f]], nsim,
                                           silent=silent)
      }else{
        SampCpars[[p]][[f]] <-list()
      }
    }
    
    # --- Sample Stock Parameters ----
    if(!is.null(SampCpars[[p]][[1]]$plusgroup) & all(SampCpars[[p]][[1]]$plusgroup==0))
      plusgroup[p] <- 0
    
    StockPars[[p]] <- SampleStockPars(MOM@Stocks[[p]], nsim, nyears,
                                      proyears, SampCpars[[p]][[1]],
                                      msg=silent)
    StockPars[[p]]$plusgroup <- plusgroup[1]
    
    # --- Sample Fleet Parameters ----
    FleetPars[[p]]<-ObsPars[[p]]<-ImpPars[[p]]<-list()
    for(f in 1:nf){
      FleetPars[[p]][[f]] <- SampleFleetPars(MOM@Fleets[[p]][[f]],
                                             Stock=StockPars[[p]],
                                             nsim, nyears, proyears,
                                             cpars=SampCpars[[p]][[f]])
    }
    
    # --- Sample Obs Parameters ----
    for(f in 1:nf) {
      ObsPars[[p]][[f]] <- SampleObsPars(MOM@Obs[[p]][[f]], nsim,
                                         cpars=SampCpars[[p]][[f]],
                                         Stock=StockPars[[p]],
                                         nyears, proyears)
    }
    
    # --- Sample Imp Parameters ----
    for(f in 1:nf) {
      ImpPars[[p]][[f]] <- SampleImpPars(MOM@Imps[[p]][[f]], nsim,
                                         cpars=SampCpars[[p]][[f]],
                                         nyears, proyears)
    }
  }
  
  # --- Update Parameters for two-sex stocks ----
  # Depletion, stock-recruit parameters, recdevs, Fleet, Obs, and Imp copied
  # from females to males
  if(length(SexPars)>0){
    sexmatches <- sapply(1:nrow(SexPars$SSBfrom), function(x,mat)
      paste(mat[x,],collapse="_"), mat=SexPars$SSBfrom)
    parcopy<-match(sexmatches,sexmatches)
    StockPars_t<-StockPars # need to store a temporary object for copying to/from
    FleetPars_t <- FleetPars
    
    for(p in 1:np){
      
      # copied parameters
      StockPars[[p]]$D<-StockPars_t[[parcopy[p]]]$D
      StockPars[[p]]$hs<-StockPars_t[[parcopy[p]]]$hs
      StockPars[[p]]$AC<-StockPars_t[[parcopy[p]]]$AC
      StockPars[[p]]$R0<-StockPars_t[[parcopy[p]]]$R0
      StockPars[[p]]$R0a<-StockPars_t[[parcopy[p]]]$R0a
      StockPars[[p]]$Perr_y<-StockPars_t[[parcopy[p]]]$Perr_y
      
      for (f in 1:nf) {
        # copy over Fleet, Obs and Imps pars
        
        FleetPars[[p]][[f]]$Esd <- FleetPars_t[[parcopy[p]]][[f]]$Esd
        FleetPars[[p]][[f]]$Find <- FleetPars_t[[parcopy[p]]][[f]]$Find
        FleetPars[[p]][[f]]$dFFinal <- FleetPars_t[[parcopy[p]]][[f]]$dFFinal
        FleetPars[[p]][[f]]$Spat_targ <- FleetPars_t[[parcopy[p]]][[f]]$Spat_targ
        FleetPars[[p]][[f]]$qinc <- FleetPars_t[[parcopy[p]]][[f]]$qinc
        FleetPars[[p]][[f]]$qcv <- FleetPars_t[[parcopy[p]]][[f]]$qcv
        FleetPars[[p]][[f]]$qvar <- FleetPars_t[[parcopy[p]]][[f]]$qvar
        FleetPars[[p]][[f]]$FinF <- FleetPars_t[[parcopy[p]]][[f]]$FinF
        
        ObsPars[[p]][[f]] <- ObsPars[[parcopy[p]]][[f]]
        ImpPars[[p]][[f]] <- ImpPars[[parcopy[p]]][[f]]
      }
    }
  } # end of sexpars
  
  nareas_s <- NIL(StockPars,"nareas",lev1=T)
  if(length(unique(nareas_s))!=1)
    stop("Stocks must have the same specified number of areas - check cpars$mov",
         " for each stock object")
  nareas <- as.numeric(nareas_s[1])
  
  
  # ---- Initialize arrays ----
  n_age <- maxage + 1 # number of age classes (starting at age-0)
  
  N <- Biomass <- Z<- VBiomass<- SSN <- SSB <- array(NA,
                                                     dim = c(nsim, np, n_age,
                                                             nyears, nareas))
  VF <- FretA <- array(NA, dim = c(nsim, np, nf, n_age, allyears))
  VBF <- FM <- FMret <- array(NA, dim = c(nsim, np, nf, n_age, nyears, nareas))  # stock numbers array
  SPR <- array(NA, dim = c(nsim, np, n_age, nyears)) # store the Spawning Potential Ratio
  MPA <- array(1,c(np,nf, nyears+proyears,nareas))
  Agearray <- array(rep(1:n_age, each = nsim), dim = c(nsim, n_age))  # Age array
  
  # ---- Hermaphroditism -----
  # (this is the fraction to be kept (after sex change))
  # E.g. protygynous (Female to male) is H_1_2 where 1 is female 2 is male
  # [sim, stock, maxage] Defaults to all 1s if length(SexPars$Herm)==0
  HermFrac <- expandHerm(SexPars$Herm,maxage=n_age,np=np,nsim=nsim)
  
  ## TODO - CHECK HERM ----
  
  Unfished_Equilibrium <- list()
  for(p in 1:np){ # loop over stocks
    #  --- Pre Equilibrium calcs ----
    surv <- matrix(1, nsim, n_age)
    surv[, 2:n_age] <- t(exp(-apply(StockPars[[p]]$M_ageArray[,,1], 1, cumsum)))[, 1:(n_age-1)]  # Survival array
    
    if (plusgroup[p]) {
      surv[,n_age] <- surv[,n_age]+surv[,n_age]*
        exp(-StockPars[[p]]$M_ageArray[,n_age,1])/(1-exp(-StockPars[[p]]$M_ageArray[,n_age,1])) # indefinite integral
    }
    
    # predicted Numbers of mature ages in first year
    Nfrac <- surv * StockPars[[p]]$Mat_age[,,1] * HermFrac[,p,]
    
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
    # TODO - check if this works correctly ----
    # - Previously an if statement for existence of initdist, see runMSE.R of DLMtool/R
    # add code if mov passed in cpars
    
    #*HermFrac[,p,1]  # !!!! INITDIST OF AGE 1. Unfished recruitment by area
    R0a <- matrix(StockPars[[p]]$R0, nrow=nsim, ncol=nareas, byrow=FALSE) * StockPars[[p]]$initdist[,1,]
    
    # ---- Unfished Equilibrium calcs ----
    surv <- array(1, dim=c(nsim, n_age, nyears+proyears)) # unfished survival for every year
    # Survival array
    surv[, 2:n_age, ] <- aperm(exp(-apply(StockPars[[p]]$M_ageArray, c(1,3), cumsum))[1:(n_age-1), ,], c(2,1,3))
    if (plusgroup[p]) {
      surv[,n_age, ] <- surv[,n_age,]+surv[,n_age,]*
        apply(-StockPars[[p]]$M_ageArray[,n_age,], 2, exp)/(1-apply(-StockPars[[p]]$M_ageArray[,n_age,], 2, exp))
    }
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
    SSB_a[SAYR_a] <- SSN_a[SAYR_a] * StockPars[[p]]$Wt_age[SAY_a]    # Calculate spawning stock biomass
    
    SSN0_a <- apply(SSN_a, c(1,3), sum) # unfished spawning numbers for each year
    N0_a <- apply(N_a, c(1,3), sum) # unfished numbers for each year)
    SSB0_a <- apply(SSB_a, c(1,3), sum) # unfished spawning biomass for each year
    SSB0a_a <- apply(SSB_a, c(1, 3,4), sum)  # Calculate unfished spawning stock biomass by area for each year
    B0_a <- apply(Biomass_a, c(1,3), sum) # unfished biomass for each year
    
    Vraw <- array(NIL(listy=FleetPars[[p]],namey="V"),c(nsim,n_age,allyears,nf))
    Vind <- as.matrix(expand.grid(1:nsim,p,1:nf,1:n_age,1:allyears))
    VF[Vind] <- Vraw[Vind[,c(1,4,5,3)]]
    
    Fretraw <- array(NIL(listy=FleetPars[[p]],namey="retA"),c(nsim,n_age,allyears,nf))
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
    VB0_a <- apply(apply(Biomass_a, c(1,2,3), sum) * V, c(1,3), sum) # unfished vulnerable biomass for each year
    
    
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
    # TODO - currently done in SS2MOM
    
    # initD <- SampCpars[[p]][[1]]$initD # 
    # if (!is.null(initD)) { # initial depletion is not unfished
    #   if (!silent) 
    #     message("Optimizing for user-specified depletion in first historical year for ", Snames[p])
    #   Perrmulti <- sapply(1:nsim, optDfunwrap, 
    #                       initD=initD, 
    #                       Nfrac=Nfrac, 
    #                       R0=R0,
    #                       Perr_y=StockPars[[p]]$Perr_y, 
    #                       surv=surv,
    #                       Wt_age=StockPars[[p]]$Wt_age, 
    #                       SSB0=SSB0,
    #                       n_age=n_age)
    #   
    #   StockPars[[p]]$Perr_y[,1:n_age] <- StockPars[[p]]$Perr_y[, 1:n_age] * Perrmulti
    # }
    # 
    
    
    #  --- Non-equilibrium calcs ----
    SSN[SPAYR] <- Nfrac[SAY] * StockPars[[p]]$R0[S] * StockPars[[p]]$initdist[SAR] *
      StockPars[[p]]$Perr_y[Sa]
    # Calculate initial stock numbers
    N[SPAYR] <- StockPars[[p]]$R0[S] * surv[SAY] * HermFrac[SPA] *
      StockPars[[p]]$initdist[SAR] * StockPars[[p]]$Perr_y[Sa]

    # Calculate initial stock biomass
    Biomass[SPAYR] <- N[SPAYR] * StockPars[[p]]$Wt_age[SAY]
    # Calculate spawning stock biomass
    SSB[SPAYR] <- SSN[SPAYR] * StockPars[[p]]$Wt_age[SAY]
    # Calculate vunerable biomass
    VBiomass[SPAYR] <- Biomass[SPAYR] * V[SAY]
    
    # Assign stock parameters to StockPars object
    StockPars[[p]]$SSBpR <- SSBpR
    StockPars[[p]]$aR <- aR
    StockPars[[p]]$bR <- bR
    StockPars[[p]]$SSB0 <- SSB0
    StockPars[[p]]$VB0 <- VB0
    StockPars[[p]]$R0a <- R0a
    StockPars[[p]]$surv <- surv
    StockPars[[p]]$B0 <- B0
    StockPars[[p]]$N0 <- N0
    
    # loop over fleets
    for(f in 1:nf) {
      FleetPars[[p]][[f]]$V<-VF[,p,f,,] # update fleet vulnerability for this stock

      # --- Historical Spatial closures ----
      
      MPA <- matrix(1, nrow=nyears+proyears, ncol=nareas)
      if (!is.na(FleetPars[[p]][[f]]$MPA) && all(FleetPars[[p]][[f]]$MPA==TRUE)) {
        MPA[,1] <- 0
      }
      FleetPars[[p]][[f]]$MPA <- MPA
    
    } # end of loop over fleets
  } # end of loop over stocks
  
  # ---- SexPars - Update SSB0 and SRR parameters for male stock ----
  ## TODO -- TEST ----
  if(length(SexPars)>0){
    message("You have specified sex-specific dynamics, unfished spawning biomass",
            " and specified stock depletion will be mirrored across sex types according ",
            "to SexPars$SSBfrom")
    
    SSB0s<-matrix(NIL(StockPars,"SSB0"),nrow=nsim) # sim, p
    sexmatches<-sapply(1:nrow(SexPars$SSBfrom),function(x,mat)paste(mat[x,],collapse="_"), mat=SexPars$SSBfrom)
    parcopy<-match(sexmatches,sexmatches)
    StockPars_t<-StockPars # need to store a temporary object for copying to/from
    
    for(p in 1:np){
      
      SSB0<-apply(matrix(rep(SexPars$SSBfrom[p,],each=nsim),nrow=nsim)*SSB0s,1,sum)
      StockPars[[p]]$SSB0 <- SSB0
      # !!!!!!!!!!! SSBpR hardwired to be the same among areas !!!!
      StockPars[[p]]$SSBpR <- array(SSB0/StockPars[[p]]$R0,c(nsim,nareas))
      
      idist<-StockPars[[p]]$R0a/apply(StockPars[[p]]$R0a,1,sum)
      SSB0a<-SSB0*idist
      
      # Ricker SR params
      StockPars[[p]]$bR <- matrix(log(5 * StockPars[[p]]$hs)/(0.8 * SSB0a),
                                  nrow=nsim)
      StockPars[[p]]$aR <- matrix(exp(StockPars[[p]]$bR * SSB0a)/StockPars[[p]]$SSBpR,
                                  nrow=nsim)
      
    }
    if(length(SexPars$Herm)>0){
      message("You have specified sequential hermaphroditism (SexPars$Herm).",
              "Unfished stock numbers will be calculated from this vector of fractions ",
              "at age. Population dynamics will move individuals from one sex to another.")
    }
  } # end of SexPars loop
  
  # --- Optimize catchability (q) to fit depletion ----

  bounds <- c(0.0001, 15) # q bounds for optimizer
  
  # TODO - and update getq_multi to match Calculate q ----

  if(snowfall::sfIsRunning() & parallel){
    exp.time <- (np * nf)/(9*ncpus) * nsim 
    exp.time <- round(exp.time,2)
    
    if(!silent)
      message("Optimizing for user-specified depletion ",
              'using parallel processing',
              "(takes approximately [(nstocks x nfleets)/(9 x number of cores in cluster)]",
              " minutes per simulation): about", exp.time, 'minutes')
    
    out<-snowfall::sfLapply(1:nsim, getq_multi_MICE,StockPars, FleetPars,
                            np,nf, nareas, maxage, nyears, N, VF, FretA,
                            maxF=MOM@maxF, MPA, CatchFrac, bounds=bounds,
                            tol=1E-6,Rel,SexPars, plusgroup=plusgroup, optVB=optVB)

  }else{
    exp.time <- (np * nf)/(9) * nsim 
    exp.time <- round(exp.time,2)
    
    if(!silent)
      message("Optimizing for user-specified depletion ",
              'using single core',
              "(takes approximately [(nstocks x nfleets)/9]",
              " minutes per simulation): about", exp.time, 'minutes')
    
    out<-lapply(1:nsim,getq_multi_MICE, StockPars, FleetPars, np, nf, nareas,
                maxage, nyears, N, VF, FretA, maxF=MOM@maxF,
                MPA,CatchFrac, bounds= bounds,tol=1E-6,Rel,SexPars, 
                plusgroup=plusgroup, optVB=optVB)
   
  }              
  
  qs <- t(matrix(NIL(out,"qtot"),nrow=np))
  qfrac <- aperm(array(NIL(out,"qfrac"),c(np,nf,nsim)),c(3,1,2))
  
  for(p in 1:np){
    for(f in 1:nf){
      FleetPars[[p]][[f]]$qs<-qs[,p]*qfrac[,p,f]
    }
  }
  
  # --- Check that q optimizer has converged ----
  # bounds for q (catchability). Flag if bounded optimizer hits the bounds
  LimBound <- c(1.1, 0.9)*range(bounds)
  probQ <- which(apply(qs > max(LimBound) | qs < min(LimBound),1,sum)>0)
  Nprob <- length(probQ)
  
  # If q has hit bound, re-sample depletion and try again.
  # Tries 'ntrials' times and then alerts user
  ntrials <- 50 
  fracD <- 0.05
  if (!is.null(control$ntrials)) ntrials <- control$ntrials
  if (!is.null(control$ntrials)) fracD <- control$fracD
  
  if (length(probQ) > 0) {
    Err <- TRUE
    if(!silent) message(Nprob,
                        ' simulations have final biomass that is not ',
                        'close to sampled depletion')
    if(!silent) message('Re-sampling depletion, recruitment error and fishing effort')
    
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
            SampCpars2[[f]] <- SampleCpars(cpars[[p]][[f]], Nprob, silent=silent)
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
                                             cpars=SampCpars2[[f]])
          FleetPars[[p]][[f]]$Esd[probQ] <- ResampFleetPars$Esd
          FleetPars[[p]][[f]]$Find[probQ, ] <- ResampFleetPars$Find
          FleetPars[[p]][[f]]$dFfinal[probQ] <- ResampFleetPars$dFfinal
        }
      }
      
      if(snowfall::sfIsRunning() & parallel){
        out2<-snowfall::sfLapply(probQ,getq_multi_MICE,StockPars, FleetPars,
                                 np,nf, nareas, maxage, nyears, N, VF, FretA,
                                 maxF=MOM@maxF, MPA,CatchFrac, bounds=bounds,
                                 tol=1E-6,Rel,SexPars,
                                 plusgroup=plusgroup, optVB=optVB)
      }else{
        out2<-lapply(probQ,getq_multi_MICE,StockPars, FleetPars, np,nf, nareas,
                     maxage, nyears, N, VF, FretA, maxF=MOM@maxF,
                     MPA,CatchFrac, bounds= bounds,tol=1E-6,Rel,SexPars,
                     plusgroup=plusgroup, optVB=optVB)
      }
      
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
          message(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0)
          message(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent)
          message("More than ", fracD*100, "% of simulations can't get to the ",
                  "specified level of depletion with these Operating Model parameters")
        stop("Change OM@seed and try again for a complete new sample, modify the ",
             "input parameters, or increase ntrials")
      } else {
        if (length(tooLow) > 0)
          message(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0)
          message(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent)
          message("More than ", 100-fracD*100, "% simulations can get to the ",
                  "sampled depletion.\nContinuing")
      }
    }
    for(p in 1:np)for(f in 1:nf) FleetPars[[p]][[f]]$qs<-qs[,p]*qfrac[,p,f]
  } # end of re-optimization conditional
    
  if(!silent)
    message("Calculating historical stock and fishing dynamics")
  
  # ---- Run Historical Simulations ----
  histYrs <- sapply(1:nsim, HistMICE, StockPars=StockPars,
                    FleetPars=FleetPars,np=np,nf=nf,nareas=nareas,
                    maxage=maxage,nyears=nyears,N=N,VF=VF,FretA=FretA,
                    maxF=MOM@maxF,MPA=MPA,Rel=Rel,SexPars=SexPars,qs=qs,qfrac=qfrac,
                    plusgroup=plusgroup)
  
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
  VBF <- aperm(array(as.numeric(unlist(histYrs[15,], use.names=FALSE)),
                     dim=c(np,nf,n_age, nyears, nareas, nsim)), c(6,1,2,3,4,5))
  Z <- aperm(array(as.numeric(unlist(histYrs[16,], use.names=FALSE)),
                   dim=c(np,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  FMt<-aperm(array(as.numeric(unlist(histYrs[17,], use.names=FALSE)),
                   dim=c(np,n_age, nyears, nareas, nsim)), c(5,1,2,3,4))
  
  # Depletion check
  SSB0_specified <- array(NIL(StockPars,'SSB0'),c(nsim,np))
  D_specified <- array(NIL(StockPars,'D'),c(nsim,np))
  if (optVB) {
    VB0_specified <- array(NIL(StockPars,'VB0'),c(nsim,np))
    Depletion <- apply(VBiomass[,,,nyears,,drop=F],1:2,sum)/ VB0_specified
  } else {
    Depletion <- apply(SSB[,,,nyears,,drop=F],1:2,sum)/ SSB0_specified  
  }
  
  if(length(SexPars)>0){ # need to copy over depletion for a sex-specific model
    sexmatches<-sapply(1:nrow(SexPars$SSBfrom), function(x,mat)
      paste(mat[x,],collapse="_"), mat=SexPars$SSBfrom)
    parcopy<-match(sexmatches,sexmatches)
    StockPars_t<-StockPars # need to store a temporary object for copying to/from
    Depletion[,1:np]<-Depletion[,parcopy]
  }
  
  for(p in 1:np) 
    StockPars[[p]]$Depletion<-Depletion[,p]  # add actual Depletion to StockPars
  
  if(!is.null(control$checks)){
    Cpred<-array(NA,c(nsim,np,nf,maxage,nareas))
    Cind<-as.matrix(expand.grid(1:nsim,1:np,1:nf,1:maxage,nyears,1:nareas))
    Cpred[Cind[,c(1:4,6)]]<-Biomass[Cind[,c(1,2,4,5,6)]]*(1-exp(-FM[Cind]))
    Cpred<-apply(Cpred,1:3,sum,na.rm=T)
    
    for(p in 1:np){
      Cp<-array(Cpred[,p,],c(nsim,nf))/apply(Cpred[,p,],1,sum)
      
      if(prod(round(CatchFrac[[p]],4)/round(Cp,4))!=1){
        print(Snames[p])
        print(cbind(CatchFrac[[p]],rep(NaN,nsim),round(Cp,4)))
        warning("Possible problem in catch fraction calculations")
      }
      
    }
  }
  if (!is.null(control$checks)) {
    if (prod(round(Depletion,2)/ round(D_specified,2)) != 1) {
      print(cbind(round(Depletion,4),rep(NaN,nsim), round(D_specified,4)))
      warning("Possible problem in depletion calculations")
    }
  }
  
  # --- Calculate MSY statistics for each year ----
  # ignores spatial closures
  # assumes all vulnerable fish are caught - ie no discarding
  if(!silent) message("Calculating MSY reference points for each year")
  # average life-history parameters over ageM years
  for(p in 1:np){
    MSY_y <- array(0, dim=c(nsim, nyears+proyears)) # store MSY for each sim and year
    StockPars[[p]]$MSY_y <- MSY_y # store MSY for each sim and year
    StockPars[[p]]$FMSY_y <- MSY_y # store FMSY for each sim, and year
    StockPars[[p]]$SSBMSY_y <- MSY_y # store SSBMSY for each sim, and year 
    StockPars[[p]]$BMSY_y <- MSY_y # store BMSY for each sim, and year
    StockPars[[p]]$VBMSY_y <- MSY_y # store VBMSY for each sim, and year
  
    FMt_future <- aperm(replicate(proyears, FMt[,,,nyears,, drop=FALSE]), c(1,2,3,4,6,5))
    FMt_all <- abind::abind(FMt[,p,,,], FMt_future[,p,,1,,], along=3)
    
    V <- apply(FMt_all,1:3,sum)
    V <- nlz(V,c(1,3),"max")
    
    for (y in 1:(nyears+proyears)) {
      MSYrefsYr <- sapply(1:nsim, optMSY_eq, 
                          StockPars[[p]]$M_ageArray, 
                          StockPars[[p]]$Wt_age, 
                          StockPars[[p]]$Mat_age, 
                          V,
                          StockPars[[p]]$maxage, 
                          StockPars[[p]]$R0, 
                          StockPars[[p]]$SRrel,
                          StockPars[[p]]$hs, 
                          yr.ind=y,
                          plusgroup=plusgroup[p])
      
      
      StockPars[[p]]$MSY_y[,y] <- MSYrefsYr[1, ]
      StockPars[[p]]$FMSY_y[,y] <- MSYrefsYr[2,]
      StockPars[[p]]$SSBMSY_y[,y] <- MSYrefsYr[3,]
      StockPars[[p]]$BMSY_y[,y] <- MSYrefsYr[6,]
      StockPars[[p]]$VBMSY_y[,y] <- MSYrefsYr[7,] 
    }
  }
  
  # --- MSY reference points ----
  for (p in 1:np) {
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
    # TODO add equilbrium unfished reference points for each population
    
    
  }
  
  # ---- Calculate Mean Generation Time ----
  for (p in 1:np) {
    MarrayArea <- replicate(nareas, StockPars[[p]]$M_ageArray[,,1:nyears])
    Mnow<-apply(MarrayArea[,,nyears,]*N[,p,,nyears,],1:2,sum)/apply(N[,p,,nyears,],1:2,sum)
    MGTsurv<-t(exp(-apply(Mnow,1,cumsum)))
    StockPars[[p]]$MGT<-apply(Agearray*(StockPars[[p]]$Mat_age[,,nyears]*MGTsurv),1,sum)/apply(StockPars[[p]]$Mat_age[,,nyears]*MGTsurv,1,sum)
  }
  
  # --- Dynamic Unfished Reference Points (SSB0) ---- 
  Dynamic_SSB0 <- lapply(1:np, function(p) 
    CalcDynamicSSB0(StockPars[[p]], nsim, nareas, nyears, proyears, maxF, 
                    Mhist = Z[, p, , , ] - FMt[, p, , , ], Nhist = N[, p, , , ]))
  
  # ---- Calculate Reference Yield ----
  if(!silent) message("Calculating reference yield - best fixed F strategy")
  ## TODO - add RefY calcs
  for(p in 1:np) StockPars[[p]]$RefY <-StockPars[[p]]$MSY
  
  
  # ---- Store Reference Points ----
  for (p in 1:np) {
    StockPars[[p]]$ReferencePoints <- list(
      ByYear=list(
        MSY=StockPars[[p]]$MSY_y,
        FMSY=StockPars[[p]]$FMSY_y,
        SSBMSY=StockPars[[p]]$SSBMSY_y,
        BMSY=StockPars[[p]]$BMSY_y,
        VBMSY=StockPars[[p]]$VBMSY_y,
        Dynamic_SSB0=Dynamic_SSB0[[p]]
      ),
      ReferencePoints=data.frame(
        N0=StockPars[[p]]$N0,
        B0=StockPars[[p]]$B0,
        SSB0=StockPars[[p]]$SSB0,
        # SSN0=StockPars[[p]]$SSN0,
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
  Ctemp <- array(NA,c(nsim,np,nf,n_age,nyears,nareas))
  CNind <- TEG(dim(Ctemp))
  Nind<-CNind[,c(1,2,4,5,6)]  # sim, stock, n_age, nyears, nareas
  Ctemp[CNind]<-Biomass[Nind]*(1-exp(-Z[Nind]))*(FM[CNind]/Z[Nind])
  CB <- Ctemp
  
  # Calculate retained-at-age
  Ctemp[CNind] <- N[Nind] * (1-exp(-Z[Nind])) * (FMret[CNind]/Z[Nind])
  Cret <- Ctemp # apply(Ctemp,1:5,sum)
  Cret[is.na(Cret)] <- 0
  
  Ctemp[CNind] <- Biomass[Nind] * (1-exp(-Z[Nind])) * (FMret[CNind]/Z[Nind])
  CBret <- Ctemp
  
  # Add to FleetPars 
  for (p in 1:np) {
    for (f in 1:nf){
      FleetPars[[p]][[f]]$CBret <- CBret[,p,f,,,]
      FleetPars[[p]][[f]]$CB <- CB[,p,f,,,]
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
  
  CurrentYr <- nyears
  DataList <- new('list')
  for (p in 1:np) {
    StockPars[[p]]$maxF <- MOM@maxF
    DataList[[p]] <- vector('list', nf)
    message('Generating historical data for ', Snames[p])
    for (f in 1:nf) {
      ObsPars[[p]][[f]]$Sample_Area <- Sample_Area # add to Obs Pars
      Data <- makeData(Biomass[,p,,,], 
                       CBret[,p,f,,,], 
                       Cret[,p,f,,,],
                       N[,p,,,], 
                       SSB[,p,,,], 
                       VBiomass[,p,,,],
                       StockPars=StockPars[[p]], 
                       FleetPars=FleetPars[[p]][[f]], 
                       ObsPars[[p]][[f]], 
                       ImpPars[[p]][[f]],
                       RefPoints=StockPars[[p]]$ReferencePoints$ReferencePoints,
                       SampCpars[[p]][[f]], 
                       StockPars[[p]]$initD, 
                       Sample_Area,
                       Name=MOM@Name,
                       nyears,
                       proyears,
                       nsim, 
                       nareas,
                       MOM@reps,
                       CurrentYr,
                       silent=TRUE)
      
      DataList[[p]][[f]] <- Data
      
    }
  }
  
  # ---- Condition Simulated Data on input Data object (if it exists) & calculate error stats ----
  # TODO - cpars$Data should be be stock and fleet - currently it is combined when using SS2MOM 
  
  # for (p in 1:np) {
  #   for (f in 1:nf) {
  #     if (class(SampCpars[[p]][[f]]$Data)=="Data") {
  #       
  #       # real data has been passed in cpars
  #       updatedData <- AddRealData(SimData= DataList[[p]][[f]], 
  #                                  RealData=SampCpars[[p]][[f]]$Data, 
  #                                  ObsPars[[p]][[f]], 
  #                                  StockPars[[p]],
  #                                  FleetPars[[p]][[f]],
  #                                  nsim,
  #                                  nyears,
  #                                  proyears,
  #                                  SampCpars,
  #                                  msg=!silent)
  #       DataList[[p]][[f]] <- updatedData$Data
  #       ObsPars[[p]][[f]] <- updatedData$ObsPars
  #       
  #     }
  #     
  #   }
  # }
  
  HistList <- vector('list', np)
  
  for (p in 1:np) {
    HistList[[p]] <- vector('list', nf)
    for (f in 1:nf) {
      Hist <- new("Hist")
      Data@Misc <- list()
      Hist@Data <-  DataList[[p]][[f]]
      Hist@Data@Obs <- data.frame() # remove
      
      ind <- which(lapply(ObsPars[[p]][[f]], length) == nsim)
      obs <- data.frame(ObsPars[[p]][[f]][ind])
      ind <- which(lapply(ImpPars[[p]][[f]], length) == nsim)
      imp <- data.frame(ImpPars[[p]][[f]][ind])
      OMPars <- DataList[[p]][[f]]@OM
      OMPars <- data.frame(OMPars, obs, imp)
      Hist@OMPars <- OMPars
      Hist@AtAge <- list(Length=StockPars[[p]]$Len_age, 
                         Weight=StockPars[[p]]$Wt_age, 
                         Select=FleetPars[[p]][[f]]$V,
                         Retention=FleetPars[[p]][[f]]$retA,
                         Maturity=StockPars[[p]]$Mat_age, 
                         N.Mortality=StockPars[[p]]$M_ageArray,
                         Z.Mortality=Z[,p,,,],
                         F.Mortality=FM[,p,f,,,],
                         Fret.Mortality=FMret[,p,f,,,],
                         Number=N[,p,,,],
                         Biomass=Biomass[,p,,,],
                         VBiomass=VBiomass[,p,,,],
                         SBiomass=SSB[,p,,,],
                         Removals=CB[,p,f,,,],
                         Landings=CBret[,p,f,,,],
                         Discards=CB[,p,f,,,]-CBret[,p,f,,,]
      )
      
      Hist@TSdata <- list(
        Number=apply(N[,p,,,],c(1,3,4), sum),
        Biomass=apply(Biomass[,p,,,],c(1,3,4), sum),
        VBiomass=apply(VBiomass[,p,,,],c(1,3,4), sum),
        SBiomass=apply(SSB[,p,,,],c(1,3,4), sum),
        Removals=apply(CB[,p,f,,,], c(1,3,4), sum),
        Landings=apply(CBret[,p,f,,,],c(1,3,4), sum),
        Discards=apply(CB[,p,f,,,]-CBret[,p,f,,,],c(1,3,4), sum),
        Find=FleetPars[[p]][[f]]$Find,
        RecDev=StockPars[[p]]$Perr_y,
        Unfished_Equilibrium=Unfished_Equilibrium[[p]]
      )
      
      Hist@Ref <- StockPars[[p]]$ReferencePoints
      
      Hist@SampPars <- list(
        Stock=StockPars[[p]],
        Fleet=FleetPars[[p]][[f]],
        Obs=ObsPars[[p]][[f]],
        Imp=ImpPars[[p]][[f]]
      )
      
      temp <- MOM@cpars$Data
      MOM@cpars <- list()
      MOM@cpars$control <- control
      MOM@cpars$Data <- temp
      
      Hist@Misc <- list(
        MOM=MOM
      )
      
      HistList[[p]][[f]] <- Hist
    }
  }
  
  class(HistList) <- 'multiHist'
  HistList
  
}

