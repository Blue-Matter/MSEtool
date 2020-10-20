
#' Run a multi-fleet multi-stock Management Strategy Evaluation
#'
#' A function that runs a Management Strategy Evaluation (closed-loop
#' simulation) for a specified operating model
#'
#' @param MOM A multi-fleet multi-stock operating model (class 'MOM')
#' @param MPs A matrix of methods (nstock x nfleet) (character string) of class MP
#' @param CheckMPs Logical to indicate if \link{Can} function should be used to check
#' if MPs can be run.
#' @param timelimit Maximum time taken for a method to carry out 10 reps
#' (methods are ignored that take longer)
#' @param Hist Should model stop after historical simulations? Returns a list
#' containing all historical data
#' @param ntrials Maximum of times depletion and recruitment deviations are
#' resampled to optimize for depletion. After this the model stops if more than
#' percent of simulations are not close to the required depletion
#' @param fracD Maximum allowed proportion of simulations where depletion is not
#' close to sampled depletion from OM before model stops with error
#' @param CalcBlow Should low biomass be calculated where this is the spawning
#' biomass at which it takes HZN mean generation times of zero fishing to reach
#' Bfrac fraction of SSBMSY
#' @param HZN The number of mean generation times required to reach Bfrac SSBMSY
#' in the Blow calculation
#' @param Bfrac The target fraction of SSBMSY for calculating Blow
#' @param AnnualMSY Logical. Should MSY statistics be calculated for each projection year?
#' May differ from MSY statistics from last historical year if there are changes in productivity
#' @param silent Should messages be printed out to the console?
#' @param PPD Logical. Should posterior predicted data be included in the MSE object Misc slot?
#' @param parallel Logical. Should the MSE be run using parallel processing?
#' @param save_name Character. Optional name to save parallel MSE list
#' @param checks Logical. Run tests?
#' @param control control options for testing and debugging
#' @return A hierarchical list (by stock then fleet) of objects of class \linkS4class{MSE}
#' @author T. Carruthers and A. Hordyk

multiMSE <- function(MOM, MPs=list(c("AvC","DCAC"),c("FMSYref","curE")),
                     CheckMPs = FALSE, timelimit = 1, Hist=FALSE,
                     ntrials=50, fracD=0.05, CalcBlow=FALSE,
                     HZN=2, Bfrac=0.5, AnnualMSY=TRUE, silent=FALSE,
                     PPD=FALSE, checks=FALSE,
                     control=NULL, parallel=FALSE) {

  stop('Function under development. Use SimulateMOM and ProjectMOM instead')
  
  if (class(MOM) != "MOM")
    stop("You must specify a valid operating model of class MOM (multi operating model)")

  set.seed(MOM@seed) # set seed for reproducibility
  if(any(is.na(MPs)))
    stop("You must specify valid management procedures - argument MPs")

  Misc<-new('list') # Blank miscellaneous slot created

  tiny <- 1e-15  # define tiny variable
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
    "You should really be using the function DLMtool::runMSE()")
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

  if (proyears < 2) stop('MOM@proyears must be > 1', call.=FALSE)
  if(!silent) message("Loading operating model")

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

  # --- Sample custom parameters ----
  # Initial N-at-age (temp workaround)
  N_at_age_initial <- matrix(NA, nrow=maxage, ncol=np)
  SampCpars <- list() # empty list
  # custom parameters exist - sample and write to list
  for(p in 1:np){
    SampCpars[[p]]<-list()
    for(f in 1:nf){
      if(length(cpars[[p]][[f]])>0){
        if (length(cpars[[p]][[f]]$N_at_age_initial)>0) {
          N_at_age_initial[,p] <- cpars[[p]][[f]]$N_at_age_initial
          cpars[[p]][[f]]$N_at_age_initial <- NULL
        }


        message(paste(Stocks[[p]]@Name," - ",Fleets[[p]][[f]]@Name))
        ncparsim <- DLMtool::cparscheck(cpars[[p]][[f]])
        SampCpars[[p]][[f]] <- DLMtool::SampleCpars(cpars[[p]][[f]], nsim,
                                                    msg=!silent)
      }else{
        SampCpars[[p]][[f]] <-list()
      }
    }
  }

  # --- Sample Stock Parameters ----
  StockPars<-FleetPars<-ObsPars<-ImpPars<-new('list')
  for(p in 1:np){
    StockPars[[p]] <- DLMtool::SampleStockPars(MOM@Stocks[[p]], nsim, nyears,
                                               proyears, SampCpars[[p]][[1]],
                                               msg=silent)
  }

  plusgroup <- rep(0, np)
  for (p in 1:np) {
    if(!is.null(cpars[[p]][[1]]$plusgroup) & all(cpars[[p]][[1]]$plusgroup==1)) plusgroup[p] <- 1
  }

  # --- Sample Fleet Parameters ----
  for(p in 1:np){
    FleetPars[[p]]<-ObsPars[[p]]<-ImpPars[[p]]<-list()
    for(f in 1:nf){
      FleetPars[[p]][[f]] <- DLMtool::SampleFleetPars(MOM@Fleets[[p]][[f]],
                                             Stock=StockPars[[p]],
                                             nsim, nyears, proyears,
                                             cpars=SampCpars[[p]][[f]])
    }

  }

  # --- Sample Obs Parameters ----
  for(p in 1:np) {
    for(f in 1:nf) {
      ObsPars[[p]][[f]] <- DLMtool::SampleObsPars(MOM@Obs[[p]][[f]], nsim,
                                                  cpars=SampCpars[[p]][[f]])
    }
  }

  # --- Sample Imp Parameters ----
  for(p in 1:np) {
    for(f in 1:nf) {
      ImpPars[[p]][[f]] <- DLMtool::SampleImpPars(MOM@Imps[[p]][[f]], nsim,
                                         cpars=SampCpars[[p]][[f]])
    }
  }

  # --- Update Parameters for two-sex stocks ----
  # Depletion, stock-recruit parameters, recdevs, Fleet, Obs, and Imp copied
  # from females to males
  ## TODO - TEST AND FINISH ----
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
  }

  nareas_s <- NIL(StockPars,"nareas",lev1=T)
  if(length(unique(nareas_s))!=1)
    stop("Stocks must have the same specified number of areas - check cpars$mov",
    " for each stock object")
  nareas <- nareas_s[1]

  # ---- Initialize arrays ----
  N <- Biomass <- Z<- VBiomass<- SSN <- SSB <- array(NA,
                                                     dim = c(nsim, np, maxage,
                                                             nyears, nareas))
  VF <- FretA <- array(NA, dim = c(nsim, np, nf, maxage, allyears))
  VBF <- FM <- FMret <- array(NA, dim = c(nsim, np, nf, maxage, nyears, nareas))  # stock numbers array
  SPR <- array(NA, dim = c(nsim, np, maxage, nyears)) # store the Spawning Potential Ratio
  MPA <- array(1,c(np,nf, nyears+proyears,nareas))
  Agearray <- array(rep(1:maxage, each = nsim), dim = c(nsim, maxage))  # Age array

  # ---- Hermaphroditism -----
  # (this is the fraction to be kept (after sex change))
  # E.g. protygynous (Female to male) is H_1_2 where 1 is female 2 is male
  # [sim, stock, maxage] Defaults to all 1s if length(SexPars$Herm)==0
  HermFrac <- expandHerm(SexPars$Herm,maxage=StockPars[[1]]$maxage,np=np,nsim=nsim)

  ## TODO - CHECK HERM ----
  for(p in 1:np){ # loop over stocks
    #  --- Pre Equilibrium calcs ----
    surv <- matrix(1, nsim, maxage)
    surv[, 2:maxage] <- t(exp(-apply(StockPars[[p]]$M_ageArray[,,1], 1, cumsum)))[, 1:(maxage-1)]  # Survival array

    if (plusgroup[p]) {
      surv[,maxage] <- surv[,maxage]+surv[,maxage]*exp(-StockPars[[p]]$M_ageArray[,maxage,1])/(1-exp(-StockPars[[p]]$M_ageArray[,maxage,1])) # indefinite integral
    }

    # predicted Numbers of mature ages in first year
    Nfrac <- surv * StockPars[[p]]$Mat_age[,,1] * HermFrac[,p,]
    # Set up some array indexes sim (S) age (A) year (Y) region/area (R)
    SPAYR <- as.matrix(expand.grid(1:nareas, 1, 1:maxage, p, 1:nsim)[5:1])
    SPA <- SPAYR[,1:3]
    SAY <- SPAYR[, c(1,3,4)]
    SAR <- SPAYR[, c(1,3,5)]
    SA <- Sa <- SPAYR[, c(1,3)]
    SR <- SPAYR[, c(1,5)]
    S <- SPAYR[, 1]
    SY <- SPAYR[, c(1, 4)]
    Sa[,2] <- maxage-Sa[,2]+1 # This is the process error index for initial year

    # TODO - Previously an if statement for existence of initdist, see runMSE.R of DLMtool/R ----

    #*HermFrac[,p,1]  # !!!! INITDIST OF AGE 1. Unfished recruitment by area
    R0a <- matrix(StockPars[[p]]$R0, nrow=nsim, ncol=nareas, byrow=FALSE) * StockPars[[p]]$initdist[,1,]
    # Calculate initial spawning stock numbers
    SSN[SPAYR] <- Nfrac[SA] * StockPars[[p]]$R0[S] * StockPars[[p]]$initdist[SAR]
    # Calculate initial stock numbers
    N[SPAYR] <- StockPars[[p]]$R0[S] * surv[SA] *HermFrac[SPA] * StockPars[[p]]$initdist[SAR]

    Neq <- N
    Biomass[SPAYR] <- N[SPAYR] * StockPars[[p]]$Wt_age[SAY]  # Calculate initial stock biomass
    SSB[SPAYR] <- SSN[SPAYR] * StockPars[[p]]$Wt_age[SAY]    # Calculate spawning stock biomass

    Vraw <- array(NIL(listy=FleetPars[[p]],namey="V"),c(nsim,maxage,allyears,nf))
    Vind <- as.matrix(expand.grid(1:nsim,p,1:nf,1:maxage,1:allyears))
    VF[Vind] <- Vraw[Vind[,c(1,4,5,3)]]

    if(nf==1){
      V <- VF[,p,1,,] #<-SOL(FleetPars[[p]],"V")
    }else{
      #Weight by catch fraction
      V <- array(0,c(nsim,maxage,allyears))
      for(f in 1:nf){
        V <- V+VF[,p,f,,]*CatchFrac[[p]][,f]
      }
      #V<-nlz(V,c(1,3),"max") # currently assume unfished vulnerability is equally weighted among fleets
      # V includes discards
    }

    VBiomass[SPAYR] <- Biomass[SPAYR] * V[SAY]  # Calculate vunerable biomass
    Fretraw <- array(NIL(listy=FleetPars[[p]],namey="retA"),c(nsim,maxage,allyears,nf))
    FretA[Vind] <- Fretraw[Vind[,c(1,4,5,3)]]

    if (nsim > 1) {
      SSN0 <- apply(SSN[,p , , 1, ], c(1, 3), sum)  # Calculate unfished spawning stock numbers
      SSB0 <- apply(SSB[,p , , 1, ], 1, sum)  # Calculate unfished spawning stock biomass
      SSBpR <- matrix(SSB0/StockPars[[p]]$R0, nrow=nsim, ncol=nareas)  # Spawning stock biomass per recruit
      SSB0a <- apply(SSB[,p, , 1, ], c(1, 3), sum)  # Calculate unfished spawning stock numbers
      B0 <- apply(Biomass[,p, , 1, ], 1, sum)
      N0 <- apply(N[,p, , 1, ], 1, sum)
    } else {
      SSN0 <- apply(SSN[,p, , 1, ], 2, sum)  # Calculate unfished spawning stock numbers
      SSB0 <-  sum(SSB[,p, , 1, ])  # Calculate unfished spawning stock biomass
      SSBpR <- SSB0/R0  # Spawning stock biomass per recruit
      SSB0a <- apply(SSB[,p, , 1, ], 2, sum)  # Calculate unfished spawning stock numbers
      B0 <- apply(Biomass[,p, , 1, ], 2, sum)
      N0 <- apply(N[,p, , 1, ], 2, sum)
    }

    bR <- matrix(log(5 * StockPars[[p]]$hs)/(0.8 * SSB0a), nrow=nsim)  # Ricker SR params
    aR <- matrix(exp(bR * SSB0a)/SSBpR, nrow=nsim)  # Ricker SR params

    #  --- Non-equilibrium calcs ----
    # Calculate initial spawning stock numbers
    if (!all(is.na(N_at_age_initial))) {
      N_init <- replicate(nsim,  N_at_age_initial[,p]) %>% t()
      N[SPAYR] <- N_init[SA] *  StockPars[[p]]$initdist[SAR]
      SSN[SPAYR] <- Nfrac[SA] * N[SPAYR]

    } else {
      SSN[SPAYR] <- Nfrac[SA] * StockPars[[p]]$R0[S] * StockPars[[p]]$initdist[SAR] *
        StockPars[[p]]$Perr_y[Sa]
      # Calculate initial stock numbers
      N[SPAYR] <- StockPars[[p]]$R0[S] * surv[SA] * HermFrac[SPA] *
        StockPars[[p]]$initdist[SAR] * StockPars[[p]]$Perr_y[Sa]
    }
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
    StockPars[[p]]$R0a <- R0a
    StockPars[[p]]$surv <- surv
    StockPars[[p]]$B0 <- B0
    StockPars[[p]]$N0 <- N0

    # loop over fleets
    for(f in 1:nf) {
      FleetPars[[p]][[f]]$V<-VF[,p,f,,] # update fleet vulnerability for this stock
      ## TODO - Need to do the same for retention ----

      # --- Historical Spatial closures ----
      ## TODO - Fix this for DLMtool V6.0 ----
      if (all(!is.na(Fleets[[p]][[f]]@MPA)) && sum(Fleets[[p]][[f]]@MPA) != 0) {
        yrindex <- Fleets[[p]][[f]]@MPA[,1]
        if (max(yrindex)>nyears)
          stop("Invalid year index for spatial closures: must be <= nyears")
        if (min(yrindex)<1)
          stop("Invalid year index for spatial closures: must be > 1")
        if (ncol(Fleets[[p]][[f]]@MPA)-1 != nareas)
          stop("OM@MPA must be nareas + 1")
        for (xx in seq_along(yrindex)) {
          MPA[p,f,yrindex[xx]:nrow(MPA),] <- matrix(
            Fleets[[p]][[f]]@MPA[xx, 2:ncol(Fleets[[p]][[f]]@MPA)],
            nrow=length(yrindex[xx]:nrow(MPA)), ncol=nareas, byrow = TRUE)
        }
      }
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
  if(!silent)
    message("Optimizing for user-specified depletion ",
    "(takes approximately [(nstocks x nfleets)/(9 x number of cores in cluster)]",
    " minutes per simulation)")

  bounds <- c(0.0001, 15) # q bounds for optimizer

  if(snowfall::sfIsRunning()){
    out<-snowfall::sfLapply(1:nsim,getq_multi_MICE,StockPars, FleetPars,
                            np,nf, nareas, maxage, nyears, N, VF, FretA,
                            maxF=MOM@maxF, MPA, CatchFrac, bounds=bounds,
                            tol=1E-6,Rel,SexPars, plusgroup=plusgroup)
  }else{
    out<-lapply(1:nsim,getq_multi_MICE, StockPars, FleetPars, np, nf, nareas,
                maxage, nyears, N, VF, FretA, maxF=MOM@maxF,
                MPA,CatchFrac, bounds= bounds,tol=1E-6,Rel,SexPars,
                plusgroup=plusgroup)
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
  if (length(probQ) > 0) {

    Err <- TRUE
    if(!silent) message(Nprob,
                        ' simulations have final biomass that is not ',
                        'close to sampled depletion')
    if(!silent) message('Re-sampling depletion, recruitment error and fishing effort')

    count <- 0
    MOM2 <- MOM
    while (Err & count < ntrials) {
      # Re-sample Stock Parameters
      Nprob <- length(probQ)
      MOM2@nsim <- Nprob

      SampCpars2 <- vector("list", nf)
      for(p in 1:np){
        for(f in 1:nf){
          if(length(cpars[[p]][[f]])>0){
            # check each list object has the same length and if not stop and error report
            ncparsim <- DLMtool::cparscheck(cpars[[p]][[f]])
            SampCpars2[[f]] <- DLMtool::SampleCpars(cpars[[p]][[f]], Nprob, msg=!silent)
          }
        }

        ResampStockPars <- DLMtool::SampleStockPars(MOM2@Stocks[[p]],
                                                    nsim=Nprob,nyears=nyears,
                                                    proyears=proyears,
                                                    cpars=SampCpars2[[1]],
                                                    msg=FALSE)
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
          ResampFleetPars <- DLMtool::SampleFleetPars(MOM2@Fleets[[p]][[f]],
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

      if(snowfall::sfIsRunning()){
        out2<-snowfall::sfLapply(probQ,getq_multi_MICE,StockPars, FleetPars,
                                 np,nf, nareas, maxage, nyears, N, VF, FretA,
                                 maxF=MOM@maxF, MPA,CatchFrac, bounds=bounds,
                                 tol=1E-6,Rel,SexPars,
                                 plusgroup=plusgroup)
      }else{
        out2<-lapply(probQ,getq_multi_MICE,StockPars, FleetPars, np,nf, nareas,
                     maxage, nyears, N, VF, FretA, maxF=MOM@maxF,
                     MPA,CatchFrac, bounds= bounds,tol=1E-6,Rel,SexPars,
                     plusgroup=plusgroup)
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

    }
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
  }  # end of q estimation steps

  if(!silent)
    message("Calculating historical stock and fishing dynamics")

  histYrs <- sapply(1:nsim,HistMICE, StockPars=StockPars,
                    FleetPars=FleetPars,np=np,nf=nf,nareas=nareas,
                    maxage=maxage,nyears=nyears,N=N,VF=VF,FretA=FretA,
                    maxF=MOM@maxF,MPA=MPA,Rel=Rel,SexPars=SexPars,qs=qs,qfrac=qfrac,
                    plusgroup=plusgroup)

  N <- aperm(array(as.numeric(unlist(histYrs[1,], use.names=FALSE)),
                   dim=c(np ,maxage, nyears, nareas, nsim)), c(5,1,2,3,4))
  Biomass <- aperm(array(as.numeric(unlist(histYrs[2,], use.names=FALSE)),
                         dim=c(np ,maxage, nyears, nareas, nsim)), c(5,1,2,3,4))
  SSN <- aperm(array(as.numeric(unlist(histYrs[3,], use.names=FALSE)),
                     dim=c(np ,maxage, nyears, nareas, nsim)), c(5,1,2,3,4))
  SSB <- aperm(array(as.numeric(unlist(histYrs[4,], use.names=FALSE)),
                     dim=c(np ,maxage, nyears, nareas, nsim)), c(5,1,2,3,4))
  # first year VBiomass not returned - quick workaroud
  temp <- VBiomass
  VBiomass <- aperm(array(as.numeric(unlist(histYrs[5,], use.names=FALSE)),
                          dim=c(np ,maxage, nyears, nareas, nsim)), c(5,1,2,3,4))
  VBiomass[,,,1,] <- temp[,,,1,]

  FM <- aperm(array(as.numeric(unlist(histYrs[6,], use.names=FALSE)),
                    dim=c(np ,nf,maxage, nyears, nareas, nsim)), c(6,1,2,3,4,5))
  FMret <- aperm(array(as.numeric(unlist(histYrs[7,], use.names=FALSE)),
                       dim=c(np ,nf,maxage, nyears, nareas, nsim)), c(6,1,2,3,4,5))
  VBF <- aperm(array(as.numeric(unlist(histYrs[15,], use.names=FALSE)),
                     dim=c(np ,nf,maxage, nyears, nareas, nsim)), c(6,1,2,3,4,5))
  Z <- aperm(array(as.numeric(unlist(histYrs[16,], use.names=FALSE)),
                   dim=c(np ,maxage, nyears, nareas, nsim)), c(5,1,2,3,4))
  FMt<-aperm(array(as.numeric(unlist(histYrs[17,], use.names=FALSE)),
                   dim=c(np ,maxage, nyears, nareas, nsim)), c(5,1,2,3,4))


  # Depletion check
  SSB0_specified <- array(NIL(StockPars,'SSB0'),c(nsim,np))
  D_specified <- array(NIL(StockPars,'D'),c(nsim,np))
  Depletion <- apply(SSB[,,,nyears,,drop=F],1:2,sum)/ SSB0_specified

  if(length(SexPars)>0){ # need to copy over depletion for a sex-specific model
    sexmatches<-sapply(1:nrow(SexPars$SSBfrom), function(x,mat)
      paste(mat[x,],collapse="_"), mat=SexPars$SSBfrom)
    parcopy<-match(sexmatches,sexmatches)
    StockPars_t<-StockPars # need to store a temporary object for copying to/from
    Depletion[,1:np]<-Depletion[,parcopy]
  }

  for(p in 1:np) StockPars[[p]]$Depletion<-Depletion[,p]

  if(checks){
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
  if (checks) {
    if (prod(round(Depletion,2)/ round(D_specified,2)) != 1) {
      print(cbind(round(Depletion,4),rep(NaN,nsim), round(D_specified,4)))
      warning("Possible problem in depletion calculations")
    }
  }

  # --- Calculate MSY references ----
  if(!silent) message("Calculating MSY reference points")
  for(p in 1:np){
    V <- apply(FMt[,p,,,],1:3,sum)
    V <- nlz(V,c(1,3),"max")
    MSYrefs <- sapply(1:nsim, optMSY_eq, StockPars[[p]]$M_ageArray,
                      StockPars[[p]]$Wt_age, StockPars[[p]]$Mat_age,
                      V=V, maxage=n_age,
                      R0=StockPars[[p]]$R0, SRrel=StockPars[[p]]$SRrel,
                      hs=StockPars[[p]]$hs, yr.ind=(nyears-1):nyears,
                      plusgroup=1)

    StockPars[[p]]$MSY <- MSYrefs[1, ]  # record the MSY results (Vulnerable)
    StockPars[[p]]$FMSY <- MSYrefs[2, ]  # instantaneous FMSY (Vulnerable)
    StockPars[[p]]$SSBMSY <- MSYrefs[3, ]  # Spawning Stock Biomass at MSY
    StockPars[[p]]$SSBMSY_SSB0 <- MSYrefs[4, ] # SSBMSY relative to unfished (SSB)
    StockPars[[p]]$BMSY_B0 <- MSYrefs[5, ] # Biomass relative to unfished (B0)
    StockPars[[p]]$BMSY <- MSYrefs[6,] # total biomass at MSY
    # Biomass at MSY (Vulnerable)
    StockPars[[p]]$VBMSY <- (MSYrefs[1, ]/(1 - exp(-MSYrefs[2, ])))
    # exploitation rate [equivalent to 1-exp(-FMSY)]
    StockPars[[p]]$UMSY <- MSYrefs[1, ]/StockPars[[p]]$VBMSY
    # ratio of true FMSY to natural mortality rate M
    StockPars[[p]]$FMSY_M <- StockPars[[p]]$FMSY/StockPars[[p]]$M

    StockPars[[p]]$VBMSY_VB0 <- rep(NA, nsim)
    StockPars[[p]]$VB0 <- rep(NA, nsim)

  } # end of stocks


  ## TODO - add B-Low calcs
  # --- B-low Calculations
  if(CalcBlow){
    message("B-low calculations are not yet available for multiMSE")
    for(p in 1:np) StockPars[[p]]$Blow<-StockPars[[p]]$MGT<-rep(NA,nsim) #
  }else{
    for(p in 1:np) StockPars[[p]]$Blow<-StockPars[[p]]$MGT<-rep(NA,nsim) #
  }

  # --- Calculate Reference Yield ----
  if(!silent) message("Calculating reference yield - best fixed F strategy")
  ## TODO - add RefY calcs
  for(p in 1:np) StockPars[[p]]$RefY <-StockPars[[p]]$MSY

  # --- Calculate catch-at-age ----
  Ctemp <- array(NA,c(nsim,np,nf,maxage,nyears,nareas))
  CNind <- TEG(dim(Ctemp))
  Nind<-CNind[,c(1,2,4,5,6)]  # sim, stock, maxage, nyears, nareas
  Ctemp[CNind]<-Biomass[Nind]*(1-exp(-Z[Nind]))*(FM[CNind]/Z[Nind])
  CB <- Ctemp

  # --- Calculate retained-at-age ----
  Ctemp[CNind] <- N[Nind] * (1-exp(-Z[Nind])) * (FMret[CNind]/Z[Nind])
  Cret <- Ctemp # apply(Ctemp,1:5,sum)
  Cret[is.na(Cret)] <- 0

  Ctemp[CNind] <- Biomass[Nind] * (1-exp(-Z[Nind])) * (FMret[CNind]/Z[Nind])
  CBret <- Ctemp

  # --- Calculate dead discarded-at-age ----
  CBdisc <- CB - CBret # discarded biomass

  # --- Observation errors for all years ----
  ErrList_P_F <- vector('list', np)
  for (p in 1:np) {
    ErrList_P_F[[p]] <- vector('list', nf)
    for (f in 1:nf) {
      ErrList <- list()
      ErrList$Cbiasa <- array(ObsPars[[p]][[f]]$Cbias, c(nsim, nyears + proyears))
      if (!is.null(control$Cbias_yr)) { # catch bias specified with control argument
        Cbiasa <- matrix(1, nsim, nyears+proyears)
        Cbiasa[,control$yrs] <- control$Cbias_yr
        ErrList$Cbiasa <- Cbiasa
      }
      ErrList$Cerr <- array(rlnorm((nyears + proyears) * nsim,
                                   DLMtool::mconv(1, rep(ObsPars[[p]][[f]]$Csd,
                                                         (nyears + proyears))),
                                   DLMtool::sdconv(1, rep(ObsPars[[p]][[f]]$Csd,
                                                          nyears + proyears))),
                            c(nsim, nyears + proyears))
      ErrList$Ierr <- array(rlnorm((nyears + proyears) * nsim,
                                   DLMtool::mconv(1, rep(ObsPars[[p]][[f]]$Isd,
                                                         nyears + proyears)),
                                   DLMtool::sdconv(1, rep(ObsPars[[p]][[f]]$Isd,
                                                          nyears + proyears))),
                            c(nsim, nyears + proyears))
      ErrList$SpIerr <- array(rlnorm((nyears + proyears) * nsim,
                                     DLMtool::mconv(1, rep(ObsPars[[p]][[f]]$Isd,
                                                           nyears + proyears)),
                                     DLMtool::sdconv(1, rep(ObsPars[[p]][[f]]$Isd,
                                                            nyears + proyears))),
                              c(nsim, nyears + proyears))

      ErrList$VIerr <- array(rlnorm((nyears + proyears) * nsim,
                                    DLMtool::mconv(1, rep(ObsPars[[p]][[f]]$Isd,
                                                          nyears + proyears)),
                                    DLMtool::sdconv(1, rep(ObsPars[[p]][[f]]$Isd,
                                                           nyears + proyears))),
                             c(nsim, nyears + proyears))
      # Simulate error in observed recruitment index
      ErrList$Recerr <- array(rlnorm((nyears + proyears) * nsim,
                                     DLMtool::mconv(1, rep(ObsPars[[p]][[f]]$Recsd,
                                                           (nyears + proyears))),
                                     DLMtool::sdconv(1, rep(ObsPars[[p]][[f]]$Recsd,
                                                            nyears + proyears))),
                              c(nsim, nyears + proyears))

      ErrList_P_F[[p]][[f]] <- ErrList
    }
  }

  # --- Implementation error time series ----
  for (p in 1:np) {
    for (f in 1:nf) {
      # composite of TAC fraction and error
      ImpPars[[p]][[f]]$TAC_f <- array(rlnorm(proyears * nsim,
                                           DLMtool::mconv(ImpPars[[p]][[f]]$TACFrac,
                                                          ImpPars[[p]][[f]]$TACSD),
                        DLMtool::sdconv(ImpPars[[p]][[f]]$TACFrac,
                                        ImpPars[[p]][[f]]$TACSD)), c(nsim, proyears))
      # composite of TAE fraction and error
      ImpPars[[p]][[f]]$E_f <- array(rlnorm(proyears * nsim,
                                         DLMtool::mconv(ImpPars[[p]][[f]]$TAEFrac,
                                                        ImpPars[[p]][[f]]$TAESD),
                                         DLMtool::sdconv(ImpPars[[p]][[f]]$TAEFrac,
                                                         ImpPars[[p]][[f]]$TAESD)),
                                  c(nsim, proyears))
      # composite of size limit fraction and error
      ImpPars[[p]][[f]]$SizeLim_f<-array(rlnorm(proyears * nsim,
                                             DLMtool::mconv(ImpPars[[p]][[f]]$SizeLimFrac,
                                                            ImpPars[[p]][[f]]$SizeLimSD),
                                             DLMtool::sdconv(ImpPars[[p]][[f]]$SizeLimFrac,
                                                             ImpPars[[p]][[f]]$SizeLimSD)),
                                      c(nsim, proyears))
    }
  }

  # --- Sampling by area ----
  if (!is.null(control$Sample_Area)) {
    message('Sampling by area is not yet available in multiMSE')
  }
  valNames <- c("Catch", 'BInd', 'SBInd', 'VInd', 'RecInd',
                'CAA', 'CAL')
  Sample_Area_array <- array(1, dim=c(nsim, nyears+proyears, nareas))
  Sample_Area <- rep(list(Sample_Area_array), length(valNames))
  names(Sample_Area) <- valNames
  n_age <- maxage # +1 for new version of model starting at age-0
  nms <- c("Catch", "BInd", "SBInd", "VInd", "CAA", "CAL")
  for (nm in nms) {
    temp <- replicate(n_age, Sample_Area[[nm]])
    Sample_Area[[nm]] <- aperm(temp, c(1,4,2,3))
  }
  ## TODO - add sample by area for stock and fleet

  # --- Populate Data object with Historical Data ----
  DataList<-new('list')
  RefPoints <- list()
  for (p in 1:np) {
    DataList[[p]] <- vector('list', nf)
    RefPoints[[p]] <- vector('list', nf)
    message('Generating historical data for ', Snames[p])
    for (f in 1:nf) {
      RefPoints[[p]][[f]] <- data.frame(MSY=StockPars[[p]]$MSY,
                              FMSY=StockPars[[p]]$FMSY,
                              SSBMSY=StockPars[[p]]$SSBMSY,
                              SSBMSY_SSB0=StockPars[[p]]$SSBMSY_SSB0,
                              BMSY_B0=StockPars[[p]]$BMSY_B0,
                              BMSY=StockPars[[p]]$BMSY,
                              VBMSY=StockPars[[p]]$VBMSY,
                              UMSY=StockPars[[p]]$UMSY,
                              VBMSY_VB0=StockPars[[p]]$VBMSY_VB0,
                              FMSY_M=StockPars[[p]]$FMSY_M,
                              N0=StockPars[[p]]$N0,
                              SSB0=StockPars[[p]]$SSB0,
                              B0=StockPars[[p]]$B0,
                              VB0=StockPars[[p]]$VB0,
                              RefY=StockPars[[p]]$RefY,
                              Blow=StockPars[[p]]$Blow,
                              MGT=StockPars[[p]]$MGT,
                              R0=StockPars[[p]]$R0)

      OM <- suppressMessages(new('OM')) # temporary while DLMtool::makeData requires this
      OM@nyears <- nyears
      OM@hbiascv <- MOM@Obs[[p]][[f]]@hbiascv
      OM@maxF <- MOM@maxF
      OM@CurrentYr <- nyears ## TODO - add currentyear to MOM
      OM@reps <- MOM@reps
      OM@nsim <- nsim
      OM@BMSY_B0biascv <- MOM@Obs[[p]][[f]]@BMSY_B0biascv

      Data <- DLMtool:::makeData(Biomass[,p,,,],
                       CBret[,p,f,,,],
                       Cret[,p,f,,,],
                       N[,p,,,],
                       SSB[,p,,,],
                       VBiomass[,p,,,],
                       StockPars[[p]],
                       FleetPars[[p]][[f]],
                       ObsPars[[p]][[f]],
                       ImpPars[[p]][[f]],
                       RefPoints[[p]][[f]],
                       ErrList_P_F[[p]][[f]],
                       OM=OM,
                       SampCpars[[p]][[f]],
                       initD=NULL, Sample_Area,
                       control=control,
                       silent=TRUE)

      DataList[[p]][[f]] <- Data
    }
  }

  # ---- Condition Simulated Data on input Data object (if it exists) & calculate error stats ----
  # TODO ---- add conditioning on real data


  # ---- Return Historical Object ----
  if (Hist) {
    if(!silent) message("Returning historical simulations")
    HistList <- vector('list', np)
    for (p in 1:np) {
      HistList[[p]] <- vector('list', nf)
      for (f in 1:nf) {
        HistObj <- new('Hist')

        Misc$initdist <- StockPars[[p]]$initdist
        Misc$N <- N[,p,,,]
        Misc$B <- Biomass[,p,,,]
        Data@Misc <- list()
        HistObj@Data <- DataList[[p]][[f]]
        OMPars <- DataList[[p]][[f]]@OM
        HistObj@Obs <- as.data.frame(ObsPars[[p]][[f]])
        om <- OMPars[,order(colnames(OMPars))]

        ind <- which(!colnames(om) %in% colnames(RefPoints[[p]][[f]]))
        HistObj@OM <- om[,ind]
        HistObj@AtAge <- list(Length=StockPars[[p]]$Len_age,
                              Weight=StockPars[[p]]$Wt_age,
                              Select=FleetPars[[p]][[f]]$V,
                              Retention=FleetPars[[p]][[f]]$retA,
                              Maturity=StockPars[[p]]$Mat_age,
                              N.Mortality=StockPars[[p]]$M_ageArray,
                              Nage=apply(N[,p,,,], 1:3, sum),
                              SSBage=apply(SSB[,p,,,], 1:3, sum),
                              FM=FM[,p,f,,,]
                              )
        nout <- t(apply(N[,p,,,], c(1, 3), sum))
        vb <- t(apply(VBiomass[,p,,,], c(1, 3), sum))
        b <- t(apply(Biomass[,p,,,], c(1, 3), sum))
        ssb <- t(apply(SSB[,p,,,], c(1, 3), sum))
        Cc <- t(apply(CB[,p,f,,,], c(1,3), sum))
        Ccret <- t(apply(CBret[,p,f,,,], c(1,3), sum))
        rec <- t(apply(N[, p,1, , ], c(1,2), sum))
        TSdata <- list(VB=t(vb), SSB=t(ssb), B=t(b), Removals=t(Cc), Catch=t(Ccret),
                       Rec=t(rec), N=t(nout),
                       Find=FleetPars[[p]][[f]]$Find,
                       Marray=StockPars[[p]]$Marray, RecDev=StockPars[[p]]$Perr_y)
        HistObj@TSdata <- TSdata
        HistObj@Ref <- RefPoints[[p]][[f]]
        HistObj@SampPars <- c(StockPars[[p]],
                              FleetPars[[p]][[f]],
                              ObsPars[[p]][[f]],
                              ImpPars[[p]][[f]])
        HistObj@Misc <- Misc
        HistObj@Misc$CurrentYr <- OM@CurrentYr
        HistObj@Misc$ErrList <- ErrList_P_F[[p]][[f]]

        HistList[[p]][[f]] <- HistObj
      } # end fleets
    } # end stocks

    return(HistList)
  }

  
  # ---- Detecting MP specification ----
  if(identical(ldim(MPs),ldim(Fleets))){
    message("Byfleet mode: you have specified an MP for each stock and fleet. ",
    "Only fleet-specific data (e.g. catches and indices) will be used to set ",
    "advice for each fleet for each stock")
    MPcond <- "byfleet"
    nMP <- length(MPs[[1]][[1]])
    MPrefs <- array(NA,c(nMP,nf,np))
    MPrefs[]<-unlist(MPs)
  } else if(np==1&nf==1){
    nMP <- length(MPs[[1]][[1]])
    MPcond <- "bystock"
    message("runMSE checking: you have specified a single stock and fleet. ",
    "For analysis you should be using runMSE(). Use this only for debugging ",
    "against runMSE.")
    MPrefs <- array(NA,c(nMP,nf,np))
    MPrefs[] <- unlist(MPs)
  }else{
    if(ldim(MPs)==ldim(Fleets)[1]){ # not a two-tier list
      message("Bystock mode: you have specified a vector of MPs for each stock, ",
      "but not a vector of MPs for each stock and fleet. The catch data for these",
      " fleets will be combined, a single MP will be used to set a single TAC ",
      "for all fleets combined that will be allocated between the fleets ",
      "according to recent catches")
      MPcond<-"bystock"
      nMP<-length(MPs[[1]])
      MPrefs<-array(NA,c(nMP,nf,np))
      for(p in 1:np)MPrefs[,,p]<-MPs[[p]]
    }
    if(class(MPs)!="list"){
      if(class(get(MPs[1]))=="MMP"){
        message("MMP mode: you have specified multi-fleet, multi-stock MPs of ",
        "class MMP. This class of MP accepts all data objects (stocks x fleets) ",
        "to simultaneously make a recommendation specific to each stock and fleet")
        MPcond <- "MMP"
        nMP <- length(MPs)
        MPrefs <- array(NA,c(nMP,nf,np))
        MPrefs[] <- MPs
      }else if(class(get(MPs[1]))=="MP"){
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
    } # not a list
  } # end of two

  if(class(MPs)=="list"){
    allMPs<-unlist(MPs)
  }else{
    allMPs<-MPs
  }

  # --- Check MPs ----
  if (CheckMPs & MPcond != "MMP") {
    if(!silent) message("Determining available methods")
    PosMPs <- Can( Data, timelimit = timelimit)
    if (!is.na(allMPs[1])) {
      cant <- allMPs[!allMPs %in% PosMPs]
      if (length(cant) > 0) {
        if(!silent) stop(paste0("Cannot run some MPs:",
                                DLMtool::DLMdiag(Data, "not available",
                                                 funcs1=cant, timelimit = timelimit)))
      }
    }
  }

  # --- Create a data object for each method ----
  # (they have identical historical data and branch in projected years)
  # also create the CALout (true catch at length) by stock and fleet as a list
  # since nCALbins varies among stocks (ragged)
  MSElist<-CALout<-list('list')
  for(p in 1:np){
    MSElist[[p]] <- CALout[[p]] <- new('list')
    for(f in 1:nf){
      MSElist[[p]][[f]] <- list(DataList[[p]][[f]])[rep(1, nMP)]
      CALout[[p]][[f]] <- list()
    }
  }
  
  B_BMSYa <- Ba <- SSBa <- VBa <- array(NA, dim = c(nsim,np,nMP, proyears))
  FMa <-F_FMSYa<- Ca <- CaRet <- TACa <- Effort <- array(NA, dim = c(nsim,np,nf, nMP, proyears))  #
  CAAout <- array(NA, dim = c(nsim,np,nf, nMP, maxage))
  PAAout <-  array(NA, dim = c(nsim,np,nMP,maxage))

  # --- Calculate MSY statistics for each projection year ----
  MSY_P <- FMSY_P <- SSBMSY_P <- array(NA, dim=c(nsim,np, nMP, proyears))
  for(p in 1:np){
    MSY_P[,p,,] <- StockPars[[p]]$MSY
    FMSY_P[,p,,] <- StockPars[[p]]$FMSY
    SSBMSY_P[,p,,] <- StockPars[[p]]$SSBMSY
  }

  interval<-MOM@interval
  if (length(MOM@interval) != nMP) interval <- rep(interval, nMP)[1:nMP]
  if (!all(interval == interval[1])) {
    message("Variable management intervals:")
    df <- data.frame(MP=MPs,interval=interval)
    message(paste(capture.output(print(df)), collapse = "\n"))
  }

  # --- Begin loop over MPs ----
  mm <- 1 # for debugging

  TAC_A <- array(NA,c(nsim,np,nf)) # Temporary store of the TAC
  TAE_A <- array(NA,c(nsim,np,nf)) # Temporary store of the TAE
  MPrecs_A_blank<-list() # Temporary Hierarchical list of MPrec objects
  for(p in 1:np) MPrecs_A_blank[[p]]<-list()
  LastTAE <- histTAE <- Effort_pot <-LastAllocat <-LastCatch <-TACused <-
    array(NA,c(nsim,np,nf))
  LastSpatial <- array(NA,c(nareas,np,nf,nsim))
  # temporary vulnerability for MSY calcs combined over fleets
  V_Pt <- array(NA,c(nsim,nf,maxage,nyears+proyears))

  for (mm in 1:nMP) {  # MSE Loop over methods

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

    for(p in 1:np){
      for(f in 1:nf){
        # reset selectivity parameters for projections
        FleetPars[[p]][[f]]$L5_P <- FleetPars[[p]][[f]]$L5
        FleetPars[[p]][[f]]$LFS_P <- FleetPars[[p]][[f]]$LFS
        FleetPars[[p]][[f]]$Vmaxlen_P <- FleetPars[[p]][[f]]$Vmaxlen
        # selectivity at length array - projections
        FleetPars[[p]][[f]]$SLarray_P <- FleetPars[[p]][[f]]$SLarray
        #  selectivity at age array - projections
        FleetPars[[p]][[f]]$V_P <- FleetPars[[p]][[f]]$V

        # reset retention parameters for projections
        FleetPars[[p]][[f]]$LR5_P <- FleetPars[[p]][[f]]$LR5
        FleetPars[[p]][[f]]$LFR_P <- FleetPars[[p]][[f]]$LFR
        FleetPars[[p]][[f]]$Rmaxlen_P <- FleetPars[[p]][[f]]$Rmaxlen
        # retention at age array - projections
        FleetPars[[p]][[f]]$retA_P <- FleetPars[[p]][[f]]$retA
        # retention at length array - projections
        FleetPars[[p]][[f]]$retL_P <- FleetPars[[p]][[f]]$retL
        # Discard ratio for projections
        FleetPars[[p]][[f]]$DR_P <- FleetPars[[p]][[f]]$DR

        FleetPars[[p]][[f]]$FM_P <- array(NA,
                                          dim = c(nsim, maxage, proyears, nareas))
        FleetPars[[p]][[f]]$FM_Pret <- array(NA,
                                             dim = c(nsim, maxage, proyears, nareas))
        # stores prospective F before reallocation to new areas
        FleetPars[[p]][[f]]$FM_nospace <- array(NA,
                                                dim = c(nsim, maxage,
                                                        proyears, nareas))
        # last apical F
        FleetPars[[p]][[f]]$FML <- array(NA, dim = c(nsim, nareas))
        FleetPars[[p]][[f]]$Z_P <- array(NA,
                                         dim = c(nsim, maxage, proyears, nareas))
        FleetPars[[p]][[f]]$CB_P <- array(NA,
                                          dim = c(nsim,maxage, proyears, nareas))
        # retained catch
        FleetPars[[p]][[f]]$CB_Pret <- array(NA,
                                             dim = c(nsim,maxage, proyears, nareas))

      }
      # Discard mortality for projections
      StockPars[[p]]$Fdisc_P <- StockPars[[p]]$Fdisc
      StockPars[[p]]$N_P <- array(NA, dim = c(nsim, maxage, proyears, nareas))
      StockPars[[p]]$Biomass_P <- array(NA, dim = c(nsim, maxage, proyears, nareas))
      StockPars[[p]]$VBiomass_P <- array(NA, dim = c(nsim, maxage, proyears, nareas))
      StockPars[[p]]$SSN_P <-array(NA, dim = c(nsim,maxage, proyears, nareas))
      StockPars[[p]]$SSB_P <- array(NA, dim = c(nsim, maxage, proyears, nareas))
    }

    N_P <- array(NA, dim = c(nsim, np, maxage, proyears, nareas))
    Biomass_P <- array(NA, dim = c(nsim,np, maxage, proyears, nareas))
    VBiomass_P <- array(NA, dim = c(nsim,np, maxage, proyears, nareas))
    SSN_P <-array(NA, dim = c(nsim,np, maxage, proyears, nareas))
    SSB_P <- array(NA, dim = c(nsim,np, maxage, proyears, nareas))
    FMt_P <- array(NA, dim = c(nsim, np, maxage, proyears, nareas))
    Z_P <- array(NA, dim = c(nsim, np, maxage, proyears, nareas))
    FM_P <- array(NA, dim = c(nsim, np,nf,maxage, proyears, nareas))
    FMret_P <- array(NA, dim = c(nsim,np,nf, maxage, proyears, nareas))
    VBF_P<-array(NA, dim = c(nsim,np,nf, maxage, proyears, nareas))

    # indexes
    SAYRL <- as.matrix(expand.grid(1:nsim, 1:maxage, nyears, 1:nareas))  # Final historical year
    SAYRt <- as.matrix(expand.grid(1:nsim, 1:maxage, 1 + nyears, 1:nareas))  # Trajectory year
    SAYR <- as.matrix(expand.grid(1:nsim, 1:maxage, 1, 1:nareas))
    SYt <- SAYRt[, c(1, 3)]
    SAYt <- SAYRt[, 1:3]
    SR <- SAYR[, c(1, 4)]
    SA1 <- SAYR[, 1:2]
    S1 <- SAYR[, 1]
    SY1 <- SAYR[, c(1, 3)]
    SAY1 <- SAYRt[, 1:3]
    SYA <- as.matrix(expand.grid(1:nsim, 1, 1:maxage))  # Projection year
    SY <- SYA[, 1:2]
    SA <- SYA[, c(1, 3)]
    SAY <- SYA[, c(1, 3, 2)]
    S <- SYA[, 1]

    # -- First projection year ----
    y <- 1
    Perr<-hs<-R0<-SRrel<-K<-Linf<-t0<-M<-array(NA,c(nsim,np))
    aR<-bR<-R0a<-SSBpR<-Asize<-array(NA,c(nsim,np,nareas))
    mov<-array(NA,c(nsim,np,maxage,nareas,nareas,nyears+proyears))
    Spat_targ_y<-array(NA,c(nsim,np,nf))
    M_agecur_y<-Mat_agecur_y<-array(NA,c(nsim,np,maxage))
    a_y<-b_y<-rep(NA,np)

    for(p in 1:np){
      Perr[,p]<-StockPars[[p]]$Perr_y[,nyears+maxage-1]
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
    # years between the two areas depending on vulnerabile biomass.
    # So to get Fcur you need to sum over areas (a bit weird)
    NextYrN <- sapply(1:nsim, function(x)
      popdynOneMICE(np,nf,nareas, maxage,
                    Ncur=array(N[x,,,nyears,],
                               c(np,maxage,nareas)),
                    Vcur=array(VF[x,,,,nyears],c(np,nf,maxage)),
                    FMretx=array(FMret[x,,,,nyears,],c(np,nf,maxage,nareas)),
                    FMx=array(FM[x,,,,nyears,],c(np,nf,maxage,nareas)),
                    PerrYrp=Perr[x,], hsx=hs[x,], aRx=matrix(aR[x,,],nrow=np),
                    bRx=matrix(bR[x,,],nrow=np),
                    movy=array(mov[x,,,,,nyears],c(np,maxage,nareas,nareas)),
                    Spat_targ=array(Spat_targ_y[x,,],c(np,nf)), SRrelx=SRrel[x,],
                    M_agecur=matrix(M_agecur_y[x,,],nrow=np),
                    Mat_agecur=matrix(Mat_agecur_y[x,,],nrow=np),
                    Asizex=matrix(Asize[x,,],ncol=nareas),Kx =K[x,],
                    Linfx=Linf[x,],t0x=t0[x,],Mx=M[x,],
                    R0x=R0[x,],R0ax=matrix(R0a[x,,],nrow=np),
                    SSBpRx=matrix(SSBpR[x,,],nrow=np),ax=a_y,
                    bx=b_y,Rel=Rel,SexPars=SexPars,x=x,
                    plusgroup=plusgroup))

    N_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[1,], use.names=FALSE)),
                            dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))

    Biomass_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[23,],
                                                    use.names=FALSE)),
                                  dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
    SSN_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[24,],
                                                use.names=FALSE)),
                              dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
    SSB_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[25,], use.names=FALSE)),
                              dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
    VBiomass_P[,,,1,] <- aperm(array(as.numeric(unlist(NextYrN[19,],
                                                     use.names=FALSE)),
                                   dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
    FML <- apply(array(FM[, ,,, nyears, ],c(nsim,np,nf,maxage,nareas)),
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
                         dim=c(nsim, maxage, nareas))
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
        MPCalcs <-  DLMtool::CalcMPDynamics(MPRecs=MPRecs_A[[p]][[f]], y=y,
                                            nyears=nyears, proyears=proyears,
                                            nsim=nsim, LastTAE=LastTAE[,p,f],
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
                                            M_ageArray=StockPars[[p]]$M_ageArray,
                                            FM_P=FleetPars[[p]][[f]]$FM_P,
                                            FM_Pret=FleetPars[[p]][[f]]$FM_Pret,
                                            Z_P=FleetPars[[p]][[f]]$Z_P,
                                            CB_P=FleetPars[[p]][[f]]$CB_P,
                                            CB_Pret=FleetPars[[p]][[f]]$CB_Pret,
                                            TAC_f=ImpPars[[p]][[f]]$TAC_f,
                                            E_f=ImpPars[[p]][[f]]$E_f,
                                            SizeLim_f=ImpPars[[p]][[f]]$SizeLim_f,
                                            VBiomass_P=StockPars[[p]]$VBiomass_P,
                                            Biomass_P=StockPars[[p]]$Biomass_P,
                                            FinF=FleetPars[[p]][[f]]$FinF,
                                            Spat_targ=FleetPars[[p]][[f]]$Spat_targ,
                                            CAL_binsmid=StockPars[[p]]$CAL_binsmid,
                                            Linf=StockPars[[p]]$Linf,
                                            Len_age=StockPars[[p]]$Len_age,
                                            maxage=StockPars[[p]]$maxage,
                                            nareas=StockPars[[p]]$nareas,
                                            Asize=StockPars[[p]]$Asize,
                                            nCALbins=StockPars[[p]]$nCALbins,
                                            qs=FleetPars[[p]][[f]]$qs,
                                            qvar=FleetPars[[p]][[f]]$qvar,
                                            qinc=FleetPars[[p]][[f]]$qinc,
                                            Effort_pot=Effort_pot[,p,f])

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
    #upyrs <- 1 + (0:(floor(proyears/2) - 1)) * 2
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
      if (AnnualMSY) { #
        for(p in 1:np){
          for(f in 1:nf){
            V_Pt[,f,,]<-FleetPars[[p]][[f]]$V_P*
              apply(CB[,p,f,,nyears,], 1, sum) # Weighted by catch frac
          }
          #summed over fleets and normalized to 1
          V_P<-nlz(apply(V_Pt,c(1,3,4),sum),c(1,3),"max")
          MSYrefsYr <- sapply(1:nsim, DLMtool::optMSY_eq,
                              StockPars[[p]]$M_ageArray,
                              StockPars[[p]]$Wt_age,
                              StockPars[[p]]$Mat_age,
                              V_P, StockPars[[p]]$maxage,
                              StockPars[[p]]$R0,
                              StockPars[[p]]$SRrel,
                              StockPars[[p]]$hs,
                              yr.ind=(nyears+y)+(-1:0))
          MSY_P[,p,mm,y] <- MSYrefsYr[1, ]
          FMSY_P[,p,mm,y] <- MSYrefsYr[2,]
          SSBMSY_P[,p,mm,y] <- MSYrefsYr[3,]
        }

      } # end of annual MSY

      TACa[,,, mm, y] <- TACa[,,, mm, y-1] # TAC same as last year unless changed

      SAYRt <- as.matrix(expand.grid(1:nsim, 1:maxage, y + nyears, 1:nareas))  # Trajectory year
      SAYt <- SAYRt[, 1:3]
      SAYtMP <- cbind(SAYt, mm)
      SYt <- SAYRt[, c(1, 3)]
      SAY1R <- as.matrix(expand.grid(1:nsim, 1:maxage, y - 1, 1:nareas))
      SAYR <- as.matrix(expand.grid(1:nsim, 1:maxage, y, 1:nareas))
      SY <- SAYR[, c(1, 3)]
      SA <- SAYR[, 1:2]
      S1 <- SAYR[, 1]

      SAY <- SAYR[, 1:3]
      S <- SAYR[, 1]
      SR <- SAYR[, c(1, 4)]
      SA2YR <- as.matrix(expand.grid(1:nsim, 2:maxage, y, 1:nareas))
      SA1YR <- as.matrix(expand.grid(1:nsim, 1:(maxage - 1), y -1, 1:nareas))

      for(p in 1:np){
        Perr[,p]<-StockPars[[p]]$Perr_y[,y+nyears+maxage-1]
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
                      Ncur=array(N_P[x,,,y-1,],c(np,maxage,nareas)),
                      Vcur=array(VF[x,,,,nyears+y-1],c(np,nf,maxage)),

                      FMretx=array(FMret_P[x,,,,y-1,],c(np,nf,maxage,nareas)),
                      FMx=array(FM_P[x,,,,y-1,],c(np,nf,maxage,nareas)),
                      PerrYrp=Perr[x,], hsx=hs[x,], aRx=matrix(aR[x,,],nrow=np),
                      bRx=matrix(bR[x,,],nrow=np),
                      movy=array(mov[x,,,,,nyears+y],c(np,maxage,nareas,nareas)),
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
                              dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
      Biomass_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[23,],
                                                      use.names=FALSE)),
                                    dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
      SSN_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[24,],
                                                  use.names=FALSE)),
                                dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
      SSB_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[25,],
                                                  use.names=FALSE)),
                                dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))

      VBiomass_P[,,,y,]<-aperm(array(as.numeric(unlist(NextYrN[19,],
                                                       use.names=FALSE)),
                                     dim=c(np,maxage, nareas, nsim)), c(4,1,2,3))
      FML <- apply(array(FM_P[, ,,, y-1, ],c(nsim,np,nf,maxage,nareas)),
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

            OM <- suppressMessages(new('OM')) # temporary while DLMtool::makeData requires this
            OM@nyears <- nyears
            OM@hbiascv <- MOM@Obs[[p]][[f]]@hbiascv
            OM@maxF <- MOM@maxF
            OM@CurrentYr <- nyears ## TODO - add currentyear to MOM
            OM@reps <- MOM@reps
            OM@nsim <- nsim
            OM@BMSY_B0biascv <- MOM@Obs[[p]][[f]]@BMSY_B0biascv
            OM@proyears <- proyears

            RefPoints <- data.frame(MSY=StockPars[[p]]$MSY,
                                    FMSY=StockPars[[p]]$FMSY,
                                    SSBMSY=StockPars[[p]]$SSBMSY,
                                    SSBMSY_SSB0=StockPars[[p]]$SSBMSY_SSB0,
                                    BMSY_B0=StockPars[[p]]$BMSY_B0,
                                    BMSY=StockPars[[p]]$BMSY,
                                    VBMSY=StockPars[[p]]$VBMSY,
                                    UMSY=StockPars[[p]]$UMSY,
                                    VBMSY_VB0=StockPars[[p]]$VBMSY_VB0,
                                    FMSY_M=StockPars[[p]]$FMSY_M,
                                    N0=StockPars[[p]]$N0,
                                    SSB0=StockPars[[p]]$SSB0,
                                    B0=StockPars[[p]]$B0,
                                    VB0=StockPars[[p]]$VB0,
                                    RefY=StockPars[[p]]$RefY,
                                    Blow=StockPars[[p]]$Blow,
                                    MGT=StockPars[[p]]$MGT,
                                    R0=StockPars[[p]]$R0)

            MSElist[[p]][[f]][[mm]] <- DLMtool:::updateData(Data=MSElist[[p]][[f]][[mm]],
                                                  OM=OM,
                                                  MPCalcs=MPCalcs_list[[p]][[f]],
                                                  Effort=Effort[,p,f, , ],
                                                  Biomass=Biomass[,p,,,],
                                                  N=N[,p,,,],
                                                  Biomass_P=Biomass_P[,p,,,],
                                                  CB_Pret=FleetPars[[p]][[f]]$CB_Pret,
                                                  N_P=N_P[,p,,,],
                                                  SSB=SSB[,p,,,],
                                                  SSB_P=SSB_P[,p,,,],
                                                  VBiomass=VBiomass[,p,,,],
                                                  VBiomass_P=VBiomass_P[,p,,,],
                                                  RefPoints,
                                                  ErrList=ErrList_P_F[[p]][[f]],
                                                  FMSY_P=FMSY_P[,p,,],
                                                  retA_P=FleetPars[[p]][[f]]$retA_P,
                                                  retL_P=FleetPars[[p]][[f]]$retL_P ,
                                                  StockPars=StockPars[[p]],
                                                  FleetPars=FleetPars[[p]][[f]],
                                                  ObsPars=ObsPars[[p]][[f]],
                                                  V_P=FleetPars[[p]][[f]]$V_P,
                                                  Mat_age=StockPars[[p]]$Mat_age,
                                                  upyrs=upyrs,
                                                  interval=interval,
                                                  y=y,
                                                  mm=mm,
                                                  Misc=MSElist[[p]][[f]][[mm]]@Misc,
                                                  SampCpars=SampCpars[[p]][[f]],
                                                  Sample_Area,
                                                  AddIunits=NA,
                                                  AddIndType=NA)

            # ---- Update true abundance ----
            M_array <- array(0.5*StockPars[[p]]$M_ageArray[,,nyears+y],
                             dim=c(nsim, maxage, nareas))
            Atemp <- apply(StockPars[[p]]$VBiomass_P[, , y, ] *
                             exp(-M_array), 1, sum) # Abundance (mid-year before fishing)
            MSElist[[p]][[f]][[mm]]@OM$A <- Atemp

          } # end of fleet
        } # end of stock


        if(MPcond=="MMP"){
          ## TODO -- Test ----
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

            MPCalcs <- DLMtool::CalcMPDynamics(MPRecs=MPRecs_A[[p]][[f]],
                                               y=y, nyears=nyears,
                                               proyears=proyears, nsim=nsim,
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
                                               M_ageArray=StockPars[[p]]$M_ageArray,
                                               FM_P=FleetPars[[p]][[f]]$FM_P,
                                               FM_Pret=FleetPars[[p]][[f]]$FM_Pret,
                                               Z_P=FleetPars[[p]][[f]]$Z_P,
                                               CB_P=FleetPars[[p]][[f]]$CB_P,
                                               CB_Pret=FleetPars[[p]][[f]]$CB_Pret,
                                               TAC_f=ImpPars[[p]][[f]]$TAC_f,
                                               E_f=ImpPars[[p]][[f]]$E_f,
                                               SizeLim_f=ImpPars[[p]][[f]]$SizeLim_f,
                                               VBiomass_P=StockPars[[p]]$VBiomass_P,
                                               Biomass_P=StockPars[[p]]$Biomass_P,
                                               FinF=FleetPars[[p]][[f]]$FinF,
                                               Spat_targ=FleetPars[[p]][[f]]$Spat_targ,
                                               CAL_binsmid=StockPars[[p]]$CAL_binsmid,
                                               Linf=StockPars[[p]]$Linf,
                                               Len_age=StockPars[[p]]$Len_age,
                                               maxage=StockPars[[p]]$maxage,
                                               nareas=StockPars[[p]]$nareas,
                                               Asize=StockPars[[p]]$Asize,
                                               nCALbins=StockPars[[p]]$nCALbins,
                                               qs=FleetPars[[p]][[f]]$qs,
                                               qvar=FleetPars[[p]][[f]]$qvar,
                                               qinc=FleetPars[[p]][[f]]$qinc,
                                               Effort_pot=Effort_pot[,p,f])
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


      } else { # end of update year
        # ---- Not an update year ----

        for(p in 1:np){
          for(f in 1:nf){
            NoMPRecs <- MPRecs_A[[p]][[f]]
            NoMPRecs$Spatial <- NA
            MPCalcs <- DLMtool::CalcMPDynamics(MPRecs=NoMPRecs,
                                               y=y, nyears=nyears,
                                               proyears=proyears,
                                               nsim=nsim,
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
                                               M_ageArray=StockPars[[p]]$M_ageArray,
                                               FM_P=FleetPars[[p]][[f]]$FM_P,
                                               FM_Pret=FleetPars[[p]][[f]]$FM_Pret,
                                               Z_P=FleetPars[[p]][[f]]$Z_P,
                                               CB_P=FleetPars[[p]][[f]]$CB_P,
                                               CB_Pret=FleetPars[[p]][[f]]$CB_Pret,
                                               TAC_f=ImpPars[[p]][[f]]$TAC_f,
                                               E_f=ImpPars[[p]][[f]]$E_f,
                                               SizeLim_f=ImpPars[[p]][[f]]$SizeLim_f,
                                               VBiomass_P=StockPars[[p]]$VBiomass_P,
                                               Biomass_P=StockPars[[p]]$Biomass_P,
                                               FinF=FleetPars[[p]][[f]]$FinF,
                                               Spat_targ=FleetPars[[p]][[f]]$Spat_targ,
                                               CAL_binsmid=StockPars[[p]]$CAL_binsmid,
                                               Linf=StockPars[[p]]$Linf,
                                               Len_age=StockPars[[p]]$Len_age,
                                               maxage=StockPars[[p]]$maxage,
                                               nareas=StockPars[[p]]$nareas,
                                               Asize=StockPars[[p]]$Asize,
                                               nCALbins=StockPars[[p]]$nCALbins,
                                               qs=FleetPars[[p]][[f]]$qs,
                                               qvar=FleetPars[[p]][[f]]$qvar,
                                               qinc=FleetPars[[p]][[f]]$qinc,
                                               Effort_pot=Effort_pot[,p,f])
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

      } # end of not an update year
    } # end of projection years

    # SSB relative to SSBMSY
    B_BMSYa[, ,mm, ] <- apply(SSB_P, c(1,2, 4), sum, na.rm=TRUE)/array(SSBMSY_P[,,mm,],
                                                                       c(nsim,np,proyears))

    for(p in 1:np) for(f in 1:nf)
      FMa[,p,f, mm, ] <- -log(1 - apply(FleetPars[[p]][[f]]$CB_P, c(1, 3), sum,
                                        na.rm=TRUE)/apply(VBiomass_P[,p,,,]+
                                                            FleetPars[[p]][[f]]$CB_P,
                                                          c(1, 3), sum, na.rm=TRUE))
    for(f in 1:nf)
      F_FMSYa[, ,f,mm, ] <- FMa[,,f, mm, ]/FMSY_P[,,mm,]

    Ba[, ,mm, ] <- apply(Biomass_P, c(1, 2,4), sum, na.rm=TRUE) # biomass
    SSBa[, ,mm, ] <- apply(SSB_P, c(1, 2,4), sum, na.rm=TRUE) # spawning stock biomass
    VBa[, ,mm, ] <- apply(VBiomass_P, c(1, 2, 4), sum, na.rm=TRUE) # vulnerable biomass

    for(p in 1:np) for(f in 1:nf)
      Ca[, p,f,mm, ] <- apply(FleetPars[[p]][[f]]$CB_P, c(1, 3),
                              sum, na.rm=TRUE) # removed
    for(p in 1:np) for(f in 1:nf)
      CaRet[, p,f,mm, ] <- apply(FleetPars[[p]][[f]]$CB_Pret, c(1, 3),
                                 sum, na.rm=TRUE) # retained catch

    # Store Pop and Catch-at-age and at-length for last projection year
    PAAout[ ,, mm, ] <- apply(array(N_P[ , , , proyears, ],
                                    c(nsim,np,maxage,nareas)), c(1,2,3), sum) # population-at-age

    for(p in 1:np){
      for(f in 1:nf) {
        CNtemp <- apply(FleetPars[[p]][[f]]$CB_Pret, c(1,2,3), sum)/
        StockPars[[p]]$Wt_age[(nyears+1):nyears+proyears]
        CAAout[ ,p,f, mm, ] <- CNtemp[,,proyears] # nsim, maxage # catch-at-age
        lastyr<-dim(MSElist[[p]][[f]][[mm]]@CAL)[2]
        CALout[[p]][[f]][[mm]] <- MSElist[[p]][[f]][[mm]]@CAL[,lastyr,] # catch-at-length in last year
        }
      }
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

      if (!parallel)
        if("progress"%in%names(control))
          if(control$progress)
            shiny::incProgress(1/nMP, detail = round(mm*100/nMP))
    }

  } # end of MP loop

  if(PPD)Misc<-MSElist

  # rescale effort to today
  # AH - don't think this is neccessary - Effort is already in today's units
  # ie 1 = current (last historical year) effort
  # for(p in 1:np) {
  #   for(f in 1:nf) {
  #     Effort[,p,f,,] <- array(Effort[,p,f,,],c(nsim,nMP,proyears))/
  #       array(FleetPars[[p]][[f]]$FinF, dim=c(nsim, nMP, proyears))
  #   }
  # }
  #
  OM<-Obsout<-CALbins<-list()
  for(p in 1:np) {
    OM[[p]]<-Obsout[[p]]<-list()
    CALbins[[p]]<-StockPars[[p]]$CAL_binsmid
    for(f in 1:nf) {
      OM[[p]][[f]]<-DataList[[p]][[f]]@OM
      Obsout[[p]][[f]]<-DataList[[p]][[f]]@Obs
    }
  }
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
                PAA=PAAout, CAA=CAAout, CAL=CALout, CALbins=CALbins,
                MSY_P = MSY_P, FMSY_P = FMSY_P, SSBMSY_P = SSBMSY_P,
                Misc = Misc)


  # Store MSE info
  attr(MSEout, "version") <- paste0("OMtool: ",
                                    packageVersion("OMtool"))
  attr(MSEout, "date") <- date()
  attr(MSEout, "R.version") <- R.version

  MSEout

}







