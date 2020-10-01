#' Run Historical Simulations
#' 
#' 
#' @param OM An operating model object (class `OM` or class `MOM`)
#' @param silent Should messages be printed out to the console?
#' @param parallel Logical. Should the MSE be run using parallel processing?
#' 
#' @return An object of class \linkS4class{Hist}
#' @export
#
Simulate <- function(OM=testOM, parallel=FALSE, silent=FALSE) {
  # ---- Initial Checks and Setup ----
  if (class(OM) == 'OM') {
    if (OM@nsim <=1) stop("OM@nsim must be > 1", call.=FALSE)
    
  } else if (class(OM) == 'Hist') {
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
  
  set.seed(OM@seed) # set seed for reproducibility 
  nsim <- OM@nsim # number of simulations
  nyears <- OM@nyears # number of historical years
  proyears <- OM@proyears # number of projection years
  interval <- OM@interval # management interval (annual)
  maxF <- OM@maxF # maximum apical F
  pstar <- OM@pstar # Percentile of the sample of the TAC for each MP 
  reps <- OM@reps # Number of samples of the management recommendation for each MP
  
  # ---- Custom Parameters (cpars) Options ----
  control <- OM@cpars$control; OM@cpars$control <- NULL
  
  # plusgroup
  plusgroup <- 1 # default now is to use plusgroup  
  if(!is.null(OM@cpars$plusgroup) && !OM@cpars$plusgroup) {
    plusgroup <- 0; OM@cpars$plusgroup <- NULL 
  }
  
  # Option to optimize depletion for vulnerable biomass instead of spawning biomass
  control$optVB <- FALSE
  if (!is.null(control$D) && control$D == "VB") control$optVB <- TRUE 
  
  # --- Sample OM parameters ----
  if(!silent) message("Loading operating model")
  # TO DO - check all validcpars are included here 
  
  # custom parameters exist - sample and write to list
  SampCpars <- list()
  if(length(OM@cpars)>0) 
    SampCpars <- SampleCpars(OM@cpars, nsim, silent=silent)
  
  # Stock Parameters
  StockPars <- SampleStockPars(Stock=OM, 
                               nsim, 
                               nyears,
                               proyears, 
                               cpars=SampCpars, 
                               msg=!silent)
  
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
  surv[, 2:n_age] <- t(exp(-apply(StockPars$M_ageArray[,,1], 1, cumsum)))[, 1:(n_age-1)]  # Survival array
  
  if (plusgroup) {
    surv[,n_age] <- surv[,n_age]+surv[,n_age]*exp(-StockPars$M_ageArray[,n_age,1])/(1-exp(-StockPars$M_ageArray[,n_age,1])) # indefinite integral
  }
  Nfrac <- surv * StockPars$Mat_age[,,1]  # predicted Numbers of mature ages in first year
  
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
    surv[,n_age, ] <- surv[,n_age,]+surv[,n_age,]*
      apply(-StockPars$M_ageArray[,n_age,], 2, exp)/(1-apply(-StockPars$M_ageArray[,n_age,], 2, exp))
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
    VB_at_age=Biomass_a * replicate(nareas, FleetPars$V)
  )
  
  # average spawning stock biomass per recruit 
  SSBpR <- matrix(UnfishedRefs[6,] %>% unlist(), nrow=nsim, ncol=nareas) 
  # average unfished biomass
  SSB0a <- UnfishedRefs[7,] %>% unlist() %>% matrix(nrow=nsim, ncol=nareas, byrow = TRUE)
  bR <- matrix(log(5 * StockPars$hs)/(0.8 * SSB0a), nrow=nsim)  # Ricker SR params
  aR <- matrix(exp(bR * SSB0a)/SSBpR, nrow=nsim)  # Ricker SR params
  
  # --- Optimize for Initial Depletion ----
  # TODO
  
  # --- Non-equilibrium Initial Year ----
  SSN[SAYR] <- Nfrac[SAY] * StockPars$R0[S] * 
    StockPars$initdist[SAR]*StockPars$Perr_y[Sa]  # Calculate initial spawning stock numbers
  N[SAYR] <- StockPars$R0[S] * surv[SAY] * StockPars$initdist[SAR]*StockPars$Perr_y[Sa]  # Calculate initial stock numbers
  Biomass[SAYR] <- N[SAYR] * StockPars$Wt_age[SAY]  # Calculate initial stock biomass
  SSB[SAYR] <- SSN[SAYR] * StockPars$Wt_age[SAY]    # Calculate spawning stock biomass
  VBiomass[SAYR] <- Biomass[SAYR] * FleetPars$V[SAY]  # Calculate vulnerable biomass
  
  StockPars$aR <- aR
  StockPars$bR <- bR
  StockPars$SSB0 <- SSB0
  StockPars$VB0 <- VB0 
  StockPars$N <- N
  StockPars$plusgroup <- plusgroup
  StockPars$maxF <- OM@maxF
  StockPars$SSBpR <- SSBpR
  
  if (!is.null(control$checks) && !is.null(initD)) { # check initial depletion 
    plot(apply(SSB[,,1,], 1, sum)/SSB0, initD)
    if (!any(round(apply(SSB[,,1,], 1, sum)/SSB0, 2) == round(initD,2))) 
      warning('problem with initial depletion')
  }
  
  
  # --- Historical Spatial closures ----
  MPA <- matrix(1, nrow=nyears+proyears, ncol=nareas)
  if (FleetPars$MPA) {
    MPA[,1] <- 0
  }
  FleetPars$MPA <- MPA
  
  # --- Optimize catchability (q) to fit depletion ----
  bounds <- c(0.0001, 15) # q bounds for optimizer
  # find the q that gives current stock depletion - optVB = depletion for vulnerable biomass else SB
  if (!is.null(SampCpars$qs)) {
    if(!silent) 
      message("Skipping optimization for depletion - using catchability (q) from OM@cpars.")
    qs <- SampCpars$qs
  } else {
    if(!silent) 
      message("Optimizing for user-specified depletion in last historical year")
    
    if (!snowfall::sfIsRunning()) {
      qs <- sapply(1:nsim, CalculateQ, StockPars, FleetPars,
                   pyears=nyears, bounds, control=control)
    } else {
      qs <- snowfall::sfSapply(1:nsim, CalculateQ, StockPars, FleetPars,
                               pyears=nyears, bounds, control=control)
    }
  }
  
  # --- Check that q optimizer has converged ---- 
  LimBound <- c(1.1, 0.9)*range(bounds)  # bounds for q (catchability). Flag if bounded optimizer hits the bounds
  probQ <- which(qs > max(LimBound) | qs < min(LimBound))
  Nprob <- length(probQ)
  
  ntrials <- 50 
  if (!is.null(control$ntrials)) ntrials <- control$ntrials
  
  # If q has hit bound, re-sample depletion and try again. Tries 'ntrials' times and then alerts user
  if (length(probQ) > 0) {
    Err <- TRUE
    if(!silent) message(Nprob,' simulations have final biomass that is not close to sampled depletion') 
    if(!silent) message('Re-sampling depletion, recruitment error, and fishing effort')
    
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
        if (length(tooLow) > 0) message(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0) message(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent) message("More than ", fracD*100, "% of simulations can't get to the specified level of depletion with these Operating Model parameters")
        stop("Change OM@seed and try again for a complete new sample, modify the input parameters, or increase OM@cpars$control$ntrials (default 50)")
      } else {
        if (length(tooLow) > 0) message(tooLow, " sims can't get down to the lower bound on depletion")
        if (length(tooHigh) > 0) message(tooHigh, " sims can't get to the upper bound on depletion")
        if(!silent) message("More than ", 100-fracD*100, "% simulations can get to the sampled depletion.\nContinuing")
      }
    }
  }
  
  # --- Simulate historical years ----
  if(!silent) message("Calculating historical stock and fishing dynamics")  # Print a progress update
  
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
              Vuln=FleetPars$V[x,,], 
              Retc=FleetPars$retA[x,,], 
              Prec=StockPars$Perr_y[x,], 
              movc=split.along.dim(StockPars$mov[x,,,,],4), 
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
              MPA=FleetPars$MPA, 
              maxF=StockPars$maxF, 
              control=1, 
              SSB0c=StockPars$SSB0[x], 
              plusgroup=StockPars$plusgroup))
  
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
  
  # Number at the end of the last projection year (after fishing)
  N_at_end <- aperm(array(as.numeric(unlist(histYrs[9,], use.names=FALSE)), 
                          dim=c(n_age, nareas, nsim)), c(3,1,2))
  N_at_end <- apply(N_at_end, c(1,2), sum)
  
  if (control$optVB) {
    if (!is.null(control$Depletion) && control$Depletion == 'end') {
      Depletion <- apply(N_at_end * FleetPars$V[,,nyears] * StockPars$Wt_age[,,nyears],1,sum)/VB0  
    } else {
      Depletion <- apply(VBiomass[,,nyears,],1,sum)/VB0 
    }
  } else {
    if (!is.null(control$Depletion) && control$Depletion == 'end') {
      Depletion <- apply(N_at_end * StockPars$Mat_age[,,nyears] * StockPars$Wt_age[,,nyears],1,sum)/SSB0  
    } else {
      Depletion <- apply(SSB[,,nyears,],1,sum)/SSB0
    }
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
  
  # --- Calculate MSY statistics for each year ----
  # ignores spatial closures
  # assumes all vulnerable fish are caught - ie no discarding
  MSY_y <- array(0, dim=c(nsim, nyears+proyears)) # store MSY for each sim and year
  FMSY_y <- MSY_y # store FMSY for each sim, and year
  SSBMSY_y <- MSY_y # store SSBMSY for each sim, and year 
  BMSY_y <- MSY_y # store BMSY for each sim, and year
  VBMSY_y <- MSY_y # store VBMSY for each sim, and year 
  
  if(!silent) message("Calculating MSY reference points for each year")
  # average life-history parameters over ageM years
  
  for (y in 1:(nyears+proyears)) {
    MSYrefsYr <- sapply(1:nsim, optMSY_eq, 
                        StockPars$M_ageArray, 
                        StockPars$Wt_age, 
                        StockPars$Mat_age, 
                        FleetPars$V,
                        StockPars$maxage, 
                        StockPars$R0, 
                        StockPars$SRrel,
                        StockPars$hs, 
                        yr.ind=y,
                        plusgroup=StockPars$plusgroup)
    MSY_y[,y] <- MSYrefsYr[1, ]
    FMSY_y[,y] <- MSYrefsYr[2,]
    SSBMSY_y[,y] <- MSYrefsYr[3,]
    BMSY_y[,y] <- MSYrefsYr[6,]
    VBMSY_y[,y] <- MSYrefsYr[7,] 
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
  
  if (!is.null(control$checks)) {
    Btemp <- apply(StockPars$SSB, c(1,3), sum)
    x <- Btemp[,nyears]/StockPars$SSBMSY
    y <- StockPars$D/StockPars$SSBMSY_SSB0
    plot(x,y, xlim=c(0,max(x)), ylim=c(0,max(y)), xlab="SSB/SSBMSY", ylab="D/SSBMSY_SSB0")
    lines(c(-10,10),c(-10,10))
  }
  
  # --- Calculate Reference Yield ----
  if(!silent) message("Calculating reference yield - best fixed F strategy")  
  RefY <- sapply(1:nsim, calcRefYield, StockPars, FleetPars, proyears, 
                 Ncurr=StockPars$N[,,nyears,])
  
  # ---- Store Reference Points ----
  ReferencePoints <- list(
    ByYear=list(
      MSY=MSY_y,
      FMSY=FMSY_y,
      SSBMSY=SSBMSY_y,
      BMSY=BMSY_y,
      VBMSY=VBMSY_y
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
      RefY=RefY
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
  
  # TODO -- 
  # add to ObsPars 
  ObsPars$Sample_Area <- Sample_Area
  
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
                   StockPars$nareas,
                   reps,
                   OM@CurrentYr,
                   silent=silent)
  
  # --- Condition Simulated Data on input Data object (if it exists) & calculate error stats ----
  # TODO 
  
  OMPars <- Data@OM
  
  # --- Return Historical Simulations and Data from last historical year ----
  
  if(!silent) 
    message("Returning historical simulations")
  Hist <- new("Hist")
  Data@Misc <- list()
  Hist@Data <- Data
  Hist@Data@Obs <- data.frame() # remove
  
  ind <- which(lapply(ObsPars, length) == nsim)
  obs <- data.frame(ObsPars[ind])
  ind <- which(lapply(ImpPars, length) == nsim)
  imp <- data.frame(ImpPars[ind])
  OMPars <- data.frame(OMPars, obs, imp)
  
  Hist@OMPars <- OMPars
  Hist@AtAge <- list(Length=StockPars$Len_age, 
                     Weight=StockPars$Wt_age, 
                     Select=FleetPars$V,
                     Retention=FleetPars$retA,
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
    Unfished_Equilibrium=Unfished_Equilibrium
  )
  
  StockPars$N <- StockPars$Biomass <- StockPars$SSN <- StockPars$SSB <- 
    StockPars$VBiomass <- StockPars$FM <- StockPars$FMret <- StockPars$Z <- NULL
  
  Hist@Ref <- ReferencePoints
  
  Hist@SampPars <- list()
  
  # cpars_Stock <- StockPars[which(lapply(StockPars, length) != nsim)]
  # cpars_Fleet <- FleetPars[which(lapply(FleetPars, length) != nsim)]
  # cpars_Obs <- ObsPars[which(lapply(ObsPars, length) != nsim)]
  # cpars_Imp <- ImpPars[which(lapply(ImpPars, length) != nsim)]
  
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
  
  Hist
}