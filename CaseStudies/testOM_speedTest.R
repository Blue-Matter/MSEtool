
library(MSEtool)

la <- devtools::load_all

la()

testOM@nsim <- 50

OM <- testOM


parallel=FALSE; silent=FALSE; nsim=NULL


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


if (inc.progress)
  shiny::incProgress(0.2, detail = 'Simulating Historical Dynamics')

if(!is.null(control$unfished)) { # generate unfished historical simulations
  if(!silent) message("Simulating unfished historical period")
  Hist <- TRUE
  qs <- rep(0, nsim) # no fishing
}
FleetPars$qs <- qs

tictoc::tic()
for (i in 1:100) {
  TEMP <-
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
              spawn_time_frac = StockPars$spawn_time_frac[x])
}
tictoc::toc()



