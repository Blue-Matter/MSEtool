#' Run a Management Strategy Evaluation
#' 
#' A function that runs a Management Strategy Evaluation (closed-loop
#' simulation) for a specified operating model
#' 
#' @param OM An operating model object (class 'OM')
#' @param MPs A vector of methods (character string) of class MP
#' @param CheckMPs Logical to indicate if \link{Can} function should be used to check
#' if MPs can be run.
#' @param timelimit Maximum time taken for a method to carry out 10 reps
#' (methods are ignored that take longer)
#' @param Hist Should model stop after historical simulations? Returns an object of 
#' class 'Hist' containing all historical data
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
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical. Should the MSE be run using parallel processing?
#' @param save_name Character. Optional name to save parallel MSE list
#' @param checks Logical. Run tests?
#' 
#' @templateVar url running-the-mse
#' @templateVar ref NULL 
# #' @template userguide_link
#' 
#' @return An object of class \linkS4class{MSE}
#' @author T. Carruthers and A. Hordyk
#' @describeIn runMSE Default function to use.
#' @seealso \link{joinMSE} \link{checkMSE} \link{updateMSE}
#' @export
runMSE <- function(OM =NULL, 
                   MPs = NA, 
                   CheckMPs = FALSE, 
                   timelimit = 1, 
                   Hist=FALSE, 
                   ntrials=100, 
                   fracD=0.05, 
                   CalcBlow=TRUE, 
                   HZN=2, 
                   Bfrac=0.5, 
                   silent=FALSE, 
                   PPD=TRUE, 
                   parallel=FALSE, 
                   save_name=NULL, 
                   checks=FALSE) {
  
  
  if (class(OM) == 'OM') {
    runMSE_single()
  } else if (class(OM) == 'MOM') {
    runMSE_multi()
  } else {
    stop('OM must be class `OM` or `MOM`')
  }
  
  
  
  
}

runMSE_single <- function() {
  
  # ---- Initial Checks and Setup ----
  if  (class(OM) != "OM") stop("You must specify an operating model")
  if (OM@nsim <=1) stop("OM@nsim must be > 1", call.=FALSE)
  if (!Hist & OM@proyears < 2) stop('OM@proyears must be > 1', call.=FALSE)
  
  set.seed(OM@seed) # set seed for reproducibility 
  tiny <- 1e-15  # define tiny variable
  
  nsim <- OM@nsim # number of simulations
  nyears <- OM@nyears # number of historical years
  proyears <- OM@proyears # number of projection years
  interval <- OM@interval # management interval (annual)
  maxF <- OM@maxF # maximum apical F
  pstar <- OM@pstar # Percentile of the sample of the TAC for each MP 
  reps <- OM@reps # Number of samples of the management recommendation for each MP
  control <- OM@cpars$control
  OM@cpars$control <- NULL
  
  # ---- Plus-Group ----
  plusgroup <- 1 # default now is to use plusgroup  
  if(!is.null(OM@cpars$plusgroup) && !OM@cpars$plusgroup) {
    plusgroup <- 0
    OM@cpars$plusgroup <- NULL 
  }
  
  # Option to optimize depletion for vulnerable biomass instead of spawning biomass
  optVB <- FALSE
  if (!is.null(control$D) && control$D == "VB") optVB <- TRUE 
  
  # --- Sample OM parameters ----
  if(!silent) message("Loading operating model")
  # TO DO - check all validcpars are included here 
  
  # custom parameters exist - sample and write to list
  SampCpars <- list()
  SampCpars <- if(length(OM@cpars)>0) SampleCpars(OM@cpars, nsim, msg=!silent)
  
  # Stock Parameters & assign to function environment
  StockPars <- SampleStockPars(OM, nsim, nyears, proyears, SampCpars, msg=!silent)
  
  # Fleet Parameters & assign to function environment
  FleetPars <- SampleFleetPars(Fleet=SubOM(OM, "Fleet"), Stock=StockPars, nsim, 
                               nyears, proyears, cpars=SampCpars)

  # Obs Parameters & assign to function environment
  ObsPars <- SampleObsPars(OM, nsim, cpars=SampCpars)

  # Imp Parameters & assign to function environment
  ImpPars <- SampleImpPars(OM, nsim, cpars=SampCpars)

  # Bio-Economic Parameters
  # TODO - add to Fleet object
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
  
  # --- Initialize Arrays ----
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
  SPR <- array(NA, dim = c(nsim, n_age, nyears)) # store the Spawning Potential Ratio
  Agearray <- array(rep(0:StockPars$maxag, each = nsim), dim = c(nsim, n_age))  # Age array
  
  #  --- Pre Equilibrium calcs ----
  surv <- matrix(1, nsim, n_age)
  surv[, 2:n_age] <- t(exp(-apply(M_ageArray[,,1], 1, cumsum)))[, 1:(n_age-1)]  # Survival array
  
  if (plusgroup) {
    surv[,n_age] <- surv[,n_age]+surv[,n_age]*exp(-M_ageArray[,n_age,1])/(1-exp(-M_ageArray[,n_age,1])) # indefinite integral
  }
  Nfrac <- surv * Mat_age[,,1]  # predicted Numbers of mature ages in first year
  
  # Set up array indexes sim (S) age (A) year (Y) region/area (R)
  SAYR <- as.matrix(expand.grid(1:nareas, 1, 1:n_age, 1:nsim)[4:1])  
  SAY <- SAYR[, 1:3]
  SAR <- SAYR[, c(1,2,4)]
  SA <- Sa <- SAYR[, 1:2]
  SR <- SAYR[, c(1, 4)]
  S <- SAYR[, 1]
  SY <- SAYR[, c(1, 3)]
  Sa[,2]<- n_age-Sa[,2] + 1 # This is the process error index for initial year
  
  # Calculate initial distribution if mov provided in cpars
  # TODO - check if this works correctly
  # add code if mov passed in cpars
  
  # Unfished recruitment by area - INITDIST OF AGE 1.
  StockPars$R0a <- matrix(StockPars$R0, nrow=nsim, ncol=nareas, byrow=FALSE) * StockPars$initdist[,1,] # 
  
  # ---- Unfished Equilibrium calcs ----
  surv <- array(1, dim=c(nsim, n_age, nyears+proyears)) # unfished survival for every year
  surv[, 2:n_age, ] <- aperm(exp(-apply(StockPars$M_ageArray, c(1,3), cumsum))[1:(n_age-1), ,], c(2,1,3)) # Survival array
  if (plusgroup) {
    surv[,n_age, ] <- surv[,n_age,]+surv[,n_age,]*apply(-StockPars$M_ageArray[,n_age,], 2, exp)/(1-apply(-StockPars$M_ageArray[,n_age,], 2, exp))
  }
  Nfrac <- surv * StockPars$Mat_age  # predicted numbers of mature ages in all years
  
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
  N_a[SAYR_a] <- StockPars$R0[S_a] * surv[SAY_a] * StockPars$initdist[SAR_a] # Calculate initial stock numbers for all years
  Biomass_a[SAYR_a] <- N_a[SAYR_a] * StockPars$Wt_age[SAY_a]  # Calculate initial stock biomass
  SSB_a[SAYR_a] <- SSN_a[SAYR_a] * StockPars$Wt_age[SAY_a]    # Calculate spawning stock biomass
  
  SSN0_a <- apply(SSN_a, c(1,3), sum) # unfished spawning numbers for each year
  N0_a <- apply(N_a, c(1,3), sum) # unfished numbers for each year)
  SSB0_a <- apply(SSB_a, c(1,3), sum) # unfished spawning biomass for each year
  SSB0a_a <- apply(SSB_a, c(1, 3,4), sum)  # Calculate unfished spawning stock biomass by area for each year
  B0_a <- apply(Biomass_a, c(1,3), sum) # unfished biomass for each year
  VB0_a <- apply(apply(Biomass_a, c(1,2,3), sum) * FleetPars$V, c(1,3), sum) # unfished vulnerable biomass for each year
  
  # TODO ---- create Unfished data to return
  
  # ---- Unfished Reference Points ----
  SSBpRa <- array(SSB0_a/matrix(StockPars$R0, nrow=nsim, ncol=nyears+proyears), 
                  dim = c(nsim, nyears+proyears))
  
  UnfishedRefs <- sapply(1:nsim, CalcUnfishedRefs, ageM=ageM, N0_a=N0_a, SSN0_a=SSN0_a,
                         SSB0_a=SSB0_a, B0_a=B0_a, VB0_a=VB0_a, SSBpRa=SSBpRa, SSB0a_a=SSB0a_a) 
  
  N0 <- UnfishedRefs[1,] %>% unlist() # average unfished numbers
  SSN0 <- UnfishedRefs[2,] %>% unlist() # average unfished spawning numbers
  SSB0 <- UnfishedRefs[3,] %>% unlist() # average unfished spawning biomass
  B0 <- UnfishedRefs[4,] %>% unlist() # average unfished biomass
  VB0 <- UnfishedRefs[5,] %>% unlist() # average unfished vulnerable biomass
  
  # TODO ---- create Reference Points data to return
  
}

runMSE_multi <- function() {
  
}

