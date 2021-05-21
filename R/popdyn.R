#' Optimize for catchability (q)
#'
#' Function optimizes catchability (q, where F=qE) required to get to user-specified stock
#' depletion
#'
#' @param x Integer, the simulation number
#' @param StockPars List of Stock Parameters
#' @param FleetPars List of Fleet Parameters
#' @param pyears The number of years to project forward. Equal to 'nyears' for optimizing for q.
#' @param bounds A numeric vector of length 2 with bounds for the optimizer
#' @param control List. Control parameters including `optVB=TRUE` to optimize
#' for vulnerable biomass instead of SSB?
#'
#' @author A. Hordyk
#' @keywords internal
CalculateQ <- function(x, StockPars, FleetPars, pyears,
                  bounds = c(1e-05, 15), control) {

  opt <- optimize(optQ, log(bounds), depc=StockPars$D[x], SSB0c=StockPars$SSB0[x],
                  StockPars$nareas, StockPars$maxage, Ncurr=StockPars$N[x,,1,],
                  pyears, M_age=StockPars$M_ageArray[x,,],
                  MatAge=StockPars$Mat_age[x,,], Asize_c=StockPars$Asize[x,],
                  WtAge=StockPars$Wt_age[x,,], Vuln=FleetPars$V[x,,],
                  Retc=FleetPars$retA[x,,], Prec=StockPars$Perr_y[x,],
                  movc=split.along.dim(StockPars$mov[x,,,,],4),
                  SRrelc=StockPars$SRrel[x], Effind=FleetPars$Find[x,],
                  Spat_targc=FleetPars$Spat_targ[x], hc=StockPars$hs[x],
                  R0c=StockPars$R0a[x,], SSBpRc=StockPars$SSBpR[x,],
                  aRc=StockPars$aR[x,], bRc=StockPars$bR[x,],
                  maxF=StockPars$maxF, MPA=FleetPars$MPA,
                  plusgroup=StockPars$plusgroup,
                  StockPars$VB0[x],
                  SBMSYc=StockPars$SSBMSY[x],
                  control)

  return(exp(opt$minimum))
}

#' Optimize q for a single simulation
#'
#' @param logQ log q
#' @param depc Depletion value
#' @param SSB0c Unfished spawning biomass
#' @param nareas Number of areas
#' @param maxage Maximum age
#' @param Ncurr Current N-at-age
#' @param pyears Number of years to project population dynamics
#' @param M_age M-at-age
#' @param Asize_c Numeric vector (length nareas) with size of each area
#' @param MatAge Maturity-at-age
#' @param WtAge Weight-at-age
#' @param Vuln Vulnerability-at-age
#' @param Retc Retention-at-age
#' @param Prec Recruitment error by year
#' @param movc movement matrix
#' @param SRrelc SR parameter
#' @param Effind Historical fishing effort
#' @param Spat_targc Spatial targeting
#' @param hc Steepness
#' @param R0c Unfished recruitment by area
#' @param SSBpRc Unfished spawning biomass per recruit by area
#' @param aRc Ricker aR
#' @param bRc Ricker bR
#' @param maxF maximum F
#' @param MPA A matrix of spatial closures by year
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
#' @param VB0c  Unfished vulnerable biomass
#' @param SBMSYc  Spawning biomass at MSY for simulation x
#' @param control List. Control parameters including `optVB=TRUE` to optimize
#' for vulnerable biomass instead of SSB?
#' @author A. Hordyk
#' @keywords internal
optQ <- function(logQ, depc, SSB0c, nareas, maxage, Ncurr, pyears, M_age, Asize_c,
                 MatAge, WtAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc,
                 R0c, SSBpRc, aRc, bRc, maxF, MPA, plusgroup, VB0c, SBMSYc, control) {

  simpop <- popdynCPP(nareas, maxage, Ncurr, pyears, M_age, Asize_c,
                      MatAge, WtAge, Vuln, Retc, Prec, movc, SRrelc, Effind,
                      Spat_targc, hc, R0c=R0c, SSBpRc=SSBpRc, aRc=aRc, bRc=bRc,
                      Qc=exp(logQ), Fapic=0, maxF=maxF, MPA=MPA, control=1,
                      SSB0c=SSB0c, plusgroup=plusgroup)

  if (!is.null(control$Depletion) && control$Depletion == 'end') {
    # Calculate depletion using biomass at the end of the last projection year
    N_at_end_yr <- rowSums(simpop[[9]])
    SB_end <- sum(N_at_end_yr * WtAge[,pyears] * MatAge[,pyears])
    VB_end <- sum(N_at_end_yr * WtAge[,pyears] * Vuln[,pyears])

    if (control$optSBMSY) {
      return((log(depc) - log(ssb/SBMSYc))^2)
    }
    if (control$optVB) {
      return((log(depc) - log(VB_end/VB0c))^2)
    }
    else {
      return((log(depc) - log(SB_end/SSB0c))^2)
    }
  } else {
    # Calculate depletion using biomass at the beginning of last projection year
    ssb <- sum(simpop[[4]][,pyears,])
    vb <- sum(simpop[[5]][,pyears,])

    if (control$optSBMSY) {
      return((log(depc) - log(ssb/SBMSYc))^2)
    }

    if (control$optVB) {
      return((log(depc) - log(vb/VB0c))^2)
    }
    else {
      return((log(depc) - log(ssb/SSB0c))^2)
    }
  }
}

split.along.dim <- function(a, n) {
  stats::setNames(lapply(split(a, arrayInd(seq_along(a), dim(a))[, n]),
                         array, dim = dim(a)[-n], dimnames(a)[-n]),
                  dimnames(a)[[n]])
}


#' Internal wrapper function to calculate MSY reference points
#'
#' @param x Simulation number
#' @param M_ageArray Array of M-at-age
#' @param Wt_age Array of weight-at-age
#' @param Mat_age Array of maturity-at-age
#' @param V Array of selectivity-at-age
#' @param maxage Vector of maximum age
#' @param R0 Vector of R0s
#' @param SRrel SRR type
#' @param hs Vector of steepness
#' @param yr.ind Year index used in calculations
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
#' @return Results from `MSYCalcs`
#' @export
#'
#' @keywords internal
optMSY_eq <- function(x, M_ageArray, Wt_age, Mat_age, V, maxage, R0, SRrel, hs,
                      yr.ind=1, plusgroup=0) {
  if (length(yr.ind)==1) {
    M_at_Age <- M_ageArray[x,,yr.ind]
    Wt_at_Age <- Wt_age[x,, yr.ind]
    Mat_at_Age <- Mat_age[x,, yr.ind]
    V_at_Age <- V[x,, yr.ind]
  } else {
    M_at_Age <- apply(M_ageArray[x,,yr.ind], 1, mean)
    Wt_at_Age <- apply(Wt_age[x,, yr.ind], 1, mean)
    Mat_at_Age <- apply(Mat_age[x,, yr.ind], 1, mean)
    V_at_Age <- apply(V[x,, yr.ind], 1, mean)
  }

  boundsF <- c(1E-8, 3)

  doopt <- optimise(MSYCalcs, log(boundsF), M_at_Age, Wt_at_Age, Mat_at_Age,
                    V_at_Age, maxage, R0x=R0[x], SRrelx=SRrel[x], hx=hs[x], opt=1,
                    plusgroup=plusgroup)

  MSYs <- MSYCalcs(doopt$minimum, M_at_Age, Wt_at_Age, Mat_at_Age,
                   V_at_Age, maxage, R0x=R0[x], SRrelx=SRrel[x], hx=hs[x], opt=2,
                   plusgroup=plusgroup)

  return(MSYs)
}


#' Internal function to calculate MSY Reference Points
#'
#' @param logF log fishing mortality
#' @param M_at_Age Vector of M-at-age
#' @param Wt_at_Age Vector of weight-at-age
#' @param Mat_at_Age Vector of maturity-at-age
#' @param V_at_Age Vector of selectivity-at-age
#' @param maxage Maximum age
#' @param R0x R0 for this simulation
#' @param SRrelx SRR type for this simulation. Use NA for per-recruit calculations, i.e. constant recruitment.
#' @param hx numeric. Steepness value for this simulation
#' @param opt Option. 1 = return -Yield, 2= return all MSY calcs
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
#' @return See `opt`
#' @export
#'
#' @keywords internal
MSYCalcs <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age,
                     maxage, R0x, SRrelx, hx, opt=1, plusgroup=0) {
  # Box 3.1 Walters & Martell 2004
  n_age <- maxage + 1
  FF <- exp(logF)
  lx <- rep(1, n_age)
  l0 <- c(1, exp(cumsum(-M_at_Age[1:(n_age-1)]))) # unfished survival

  surv <- exp(-M_at_Age - FF * V_at_Age)
  for (a in 2:n_age) {
    lx[a] <- lx[a-1] * surv[a-1] # fished survival
  }

  if (plusgroup == 1) {
    l0[length(l0)] <- l0[length(l0)]+l0[length(l0)]*exp(-M_at_Age[length(l0)])/(1-exp(-M_at_Age[length(l0)]))
    lx[length(lx)] <- lx[length(lx)]+lx[length(lx)]*exp(-M_at_Age[length(lx)])/(1-exp(-M_at_Age[length(lx)]))
  }

  Egg0 <- sum(l0 * Wt_at_Age * Mat_at_Age) # unfished egg-per-recruit (assuming fecundity proportional to weight)
  EggF <- sum(lx * Wt_at_Age * Mat_at_Age) # fished egg-per-recruit (assuming fecundity proportional to weight)

  vB0 <- sum(l0 * Wt_at_Age * V_at_Age) # unfished and fished vuln. biomass per-recruit
  vBF <- sum(lx * Wt_at_Age * V_at_Age)

  SB0 <- sum(l0 * Wt_at_Age * Mat_at_Age) # spawning biomas per-recruit - same as eggs atm
  SBF <- sum(lx * Wt_at_Age * Mat_at_Age)

  B0 <- sum(l0 * Wt_at_Age) # biomass-per-recruit
  BF <- sum(lx * Wt_at_Age)

  SPR <- EggF/Egg0
  # Calculate equilibrium recruitment at this SPR
  if (is.na(SRrelx)) {
    RelRec <- R0x
  } else {
    hx[hx>0.999] <- 0.999
    if (SRrelx ==1) { # BH SRR
      recK <- (4*hx)/(1-hx) # Goodyear compensation ratio
      reca <- recK/Egg0

      recb <- (reca * Egg0 - 1)/(R0x*Egg0)
      RelRec <- (reca * EggF-1)/(recb*EggF)
    }
    if (SRrelx ==2) { # Ricker
      bR <- (log(5*hx)/(0.8*SB0))
      aR <- exp(bR*SB0)/(SB0/R0x)
      RelRec <- (log(aR*EggF/R0x))/(bR*EggF/R0x)
    }
  }
  RelRec[RelRec<0] <- 0

  Z_at_Age <- FF * V_at_Age + M_at_Age
  YPR <- sum(lx * Wt_at_Age * FF * V_at_Age * (1 - exp(-Z_at_Age))/Z_at_Age)
  Yield <- YPR * RelRec

  if (opt == 1)  return(-Yield)
  if (opt == 2) {
    out <- c(Yield=Yield,
             F= FF,
             SB = SBF * RelRec,
             SB_SB0 = (SBF * RelRec)/(SB0 * R0x),
             B_B0 = (BF * RelRec)/(B0 * R0x),
             B = BF * RelRec,
             VB = vBF * RelRec,
             VB_VB0 = (vBF * RelRec)/(vB0 * R0x),
             RelRec=RelRec,
             SB0 = SB0 * R0x,
             B0=B0 * R0x)
    return(out)
  }
}

#' @importFrom numDeriv grad
YPRCalc <- function(M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age, maxage, plusgroup, boundsF) {
  dYPR_logF0 <- numDeriv::grad(YPR_int, -10, M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age,
                               Mat_at_Age = Mat_at_Age, V_at_Age = V_at_Age, maxage = maxage,
                               plusgroup = plusgroup)
  dYPR_F0 <- dYPR_logF0/exp(-10) # Chain rule to calculate derivative near origin, F = exp(-10)

  log_F01 <- optimize(F01_solve, interval = log(boundsF),
                      M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
                      V_at_Age = V_at_Age, maxage = maxage, plusgroup = plusgroup, dYPR_F0 = dYPR_F0)

  log_Fmax <- optimize(YPR_int, interval = log(boundsF), maximum = TRUE,
                       M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
                       V_at_Age = V_at_Age, maxage = maxage, plusgroup = plusgroup)

  c(YPR_F01 = exp(log_F01$minimum), YPR_Fmax = exp(log_Fmax$maximum))
}

YPR_int <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age, maxage, plusgroup) {
  out <- MSYCalcs(logF, M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
                  V_at_Age = V_at_Age, maxage = maxage, R0x = 1, SRrelx = NA, hx = 1, opt = 2, plusgroup = plusgroup)
  out["Yield"]
}

F01_solve <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age, maxage, plusgroup, dYPR_F0) {
  dYPR_logF <- numDeriv::grad(YPR_int, logF, M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
                              V_at_Age = V_at_Age, maxage = maxage, plusgroup = plusgroup)
  dYPR_F <- dYPR_logF/exp(logF) # Chain rule
  (dYPR_F - 0.1 * dYPR_F0)^2
}

SPR_int <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age, maxage, plusgroup, SPR_target) {
  out <- MSYCalcs(logF, M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
                  V_at_Age = V_at_Age, maxage = maxage, R0x = 1, SRrelx = NA, hx = 1, opt = 2, plusgroup = plusgroup)
  (out["SB_SB0"] - SPR_target)^2
}

per_recruit_F_calc <- function(x, M_ageArray, Wt_age, Mat_age, V, maxage,
                               yr.ind=1, plusgroup=0, SPR_target = seq(0.2, 0.6, 0.05)) {
  if (length(yr.ind)==1) {
    M_at_Age <- M_ageArray[x,,yr.ind]
    Wt_at_Age <- Wt_age[x,, yr.ind]
    Mat_at_Age <- Mat_age[x,, yr.ind]
    V_at_Age <- V[x,, yr.ind]
  } else {
    M_at_Age <- apply(M_ageArray[x,,yr.ind], 1, mean)
    Wt_at_Age <- apply(Wt_age[x,, yr.ind], 1, mean)
    Mat_at_Age <- apply(Mat_age[x,, yr.ind], 1, mean)
    V_at_Age <- apply(V[x,, yr.ind], 1, mean)
  }

  boundsF <- c(1E-8, 3)

  FSPR <- vapply(SPR_target, function(x) {
    r <- optimize(SPR_int, interval = log(boundsF),
                  M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
                  V_at_Age = V_at_Age, maxage = maxage, plusgroup = plusgroup, SPR_target = x)
    exp(r$minimum)
  }, numeric(1))

  FYPR <- YPRCalc(M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age, maxage, plusgroup, boundsF)
  return(list(FSPR, FYPR))
}

FcrashCalc <- function(x, StockPars, FleetPars, y) {
  boundsF <- c(1e-8, 5)

  if(StockPars$SRrel[x] == 1) {
    alpha <- 4 * StockPars$hs[x]/(1 - StockPars$hs[x])/StockPars$SSBpR[x, 1]
  } else {
    alpha <- StockPars$aR[x, 1]
  }
  opt <- optimize(optFreplacement, interval = log(boundsF),
                  M_at_Age = StockPars$M_ageArray[x,,y],
                  Wt_at_Age = StockPars$Wt_age[x,,y],
                  Mat_at_Age = StockPars$Mat_age[x,,y],
                  V_at_Age = FleetPars$V[x,,y],
                  maxage = StockPars$maxage,
                  RpS_slope = alpha,
                  plusgroup = StockPars$plusgroup)
  exp(opt$minimum)
}

FmedCalc <- function(x, StockPars, FleetPars, y) {
  SSB <- apply(StockPars$SSB[x,,,], 2, sum)
  R <- apply(StockPars$N[x, 1, , ], 1, sum)

  RpS <- median(R/SSB)

  boundsF <- c(1e-8, 5)
  opt <- optimize(optFreplacement, interval = log(boundsF),
                  M_at_Age = StockPars$M_ageArray[x,,y],
                  Wt_at_Age = StockPars$Wt_age[x,,y],
                  Mat_at_Age = StockPars$Mat_age[x,,y],
                  V_at_Age = FleetPars$V[x,,y],
                  maxage = StockPars$maxage,
                  RpS_slope = RpS,
                  plusgroup = StockPars$plusgroup)
  exp(opt$minimum)
}

optFreplacement <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age,
                            maxage, RpS_slope, opt=1, plusgroup=0) {
  # Box 3.1 Walters & Martell 2004
  n_age <- maxage + 1
  FF <- exp(logF)
  lx <- rep(1, n_age)
  l0 <- c(1, exp(cumsum(-M_at_Age[1:(n_age-1)]))) # unfished survival

  surv <- exp(-M_at_Age - FF * V_at_Age)
  for (a in 2:n_age) {
    lx[a] <- lx[a-1] * surv[a-1] # fished survival
  }

  if (plusgroup == 1) {
    l0[length(l0)] <- l0[length(l0)]+l0[length(l0)]*exp(-M_at_Age[length(l0)])/(1-exp(-M_at_Age[length(l0)]))
    lx[length(lx)] <- lx[length(lx)]+lx[length(lx)]*exp(-M_at_Age[length(lx)])/(1-exp(-M_at_Age[length(lx)]))
  }

  Egg0 <- sum(l0 * Wt_at_Age * Mat_at_Age) # unfished egg-per-recruit (assuming fecundity proportional to weight)
  EggF <- sum(lx * Wt_at_Age * Mat_at_Age) # fished egg-per-recruit (assuming fecundity proportional to weight)

  vB0 <- sum(l0 * Wt_at_Age * V_at_Age) # unfished and fished vuln. biomass per-recruit
  vBF <- sum(lx * Wt_at_Age * V_at_Age)

  SB0 <- sum(l0 * Wt_at_Age * Mat_at_Age) # spawning biomas per-recruit - same as eggs atm
  SBF <- sum(lx * Wt_at_Age * Mat_at_Age)

  B0 <- sum(l0 * Wt_at_Age) # biomass-per-recruit
  BF <- sum(lx * Wt_at_Age)

  SPR <- EggF/Egg0
  if(opt==1) {
    return((1/EggF - RpS_slope)^2)
  } else {
    return(SPR)
  }
}

#' Calculate Reference Yield
#'
#' @param x Integer, the simulation number
#' @param StockPars List of Stock Parameters
#' @param FleetPars List of Fleet Parameters
#' @param pyears The number of years to project forward. Equal to 'nyears' for optimizing for q.
#' @param Ncurr Array with current numbers-at-age (dim=c(nsim, maxage+1, nareas))
#' @param nyears Number of historical years
#' @param proyears Number of projection years
#' @author A. Hordyk
calcRefYield <- function(x, StockPars, FleetPars, pyears, Ncurr, nyears, proyears) {

  opt <- optimize(optYield, log(c(0.001, 10)),
                  Asize_c=StockPars$Asize[x,],
                  StockPars$nareas,
                  StockPars$maxage,
                  Ncurr=Ncurr[x,,],
                  pyears=pyears,
                  M_age=StockPars$M_ageArray[x,,(nyears):(nyears+proyears)],
                  MatAge=StockPars$Mat_age[x,,(nyears):(nyears+proyears)],
                  WtAge=StockPars$Wt_age[x,,(nyears):(nyears+proyears)],
                  Vuln=FleetPars$V[x,,(nyears):(nyears+proyears)],
                  Retc=FleetPars$retA[x,,(nyears):(nyears+proyears)],
                  Prec=StockPars$Perr_y[x,(nyears):(nyears+proyears+StockPars$maxage)],
                  movc=split.along.dim(StockPars$mov[x,,,,(nyears):(nyears+proyears)],4),
                  SRrelc=StockPars$SRrel[x],
                  Effind=FleetPars$Find[x,],
                  Spat_targc=FleetPars$Spat_targ[x],
                  hc=StockPars$hs[x],
                  R0c=StockPars$R0a[x,],
                  SSBpRc=StockPars$SSBpR[x,],
                  aRc=StockPars$aR[x,],
                  bRc=StockPars$bR[x,],
                  MPA=FleetPars$MPA,
                  maxF=StockPars$maxF,
                  SSB0c=StockPars$SSB0[x],
                  plusgroup=StockPars$plusgroup)

  -opt$objective

}

#' Optimize yield for a single simulation
#'
#' @param logFa log apical fishing mortality
#' @param Asize_c A vector of length areas with relative size of areas
#' @param nareas Number of area
#' @param maxage Maximum age
#' @param Ncurr Current N-at-age
#' @param pyears Number of projection years
#' @param M_age M-at-age
#' @param MatAge Maturity-at-age
#' @param WtAge Weight-at-age
#' @param Vuln Vulnerability-at-age
#' @param Retc Retention-at-age
#' @param Prec Recruitment error
#' @param movc Movement matrix
#' @param SRrelc SR Relationship
#' @param Effind Historical effort
#' @param Spat_targc Spatial targeting
#' @param hc Steepness
#' @param R0c Unfished recruitment by area
#' @param SSBpRc Unfished spawning stock per recruit by area
#' @param aRc Ricker aR
#' @param bRc Ricker bR
#' @param Qc Catchability
#' @param MPA A matrix of spatial closures by year
#' @param maxF A numeric value specifying the maximum fishing mortality for any single age class
#' @param SSB0c SSB0
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
#' @keywords internal
#'
#' @author A. Hordyk
#'
optYield <- function(logFa, Asize_c, nareas, maxage, Ncurr, pyears, M_age,
                   MatAge, WtAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc,
                   R0c, SSBpRc, aRc, bRc, Qc, MPA, maxF, SSB0c,
                   plusgroup=0) {

  FMSYc <- exp(logFa)

  simpop <- popdynCPP(nareas, maxage, Ncurr, pyears, M_age, Asize_c,
                      MatAge, WtAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc,
                      R0c, SSBpRc, aRc, bRc, Qc=0, Fapic=FMSYc, MPA=MPA, maxF=maxF, control=2,
                      SSB0c=SSB0c, plusgroup = plusgroup)

  # Yield
  # Cn <- simpop[[7]]/simpop[[8]] * simpop[[1]] * (1-exp(-simpop[[8]])) # retained catch
  Cn <- simpop[[6]]/simpop[[8]] * simpop[[1]] * (1-exp(-simpop[[8]])) # removals
  # Cb <- Cn[,pyears,] * WtAge[,pyears]
  # -sum(Cb)
  Cb <- Cn[,(pyears-4):pyears,] * array(WtAge[,(pyears-4):pyears], dim=dim(Cn[,(pyears-4):pyears,]))

  -mean(apply(Cb,2,sum))


}



#' Calculate population dynamics from MP recommendation
#'
#' An internal function to calculate the population dynamics for the next time
#' step based on the recent MP recommendation
#'
#' @param MPRecs A named list of MP recommendations. The names are the same as `slotNames('Rec')`, except
#' for `Misc`. Each element in the list is a matrix. With the exception of `Spatial`, all elements in list
#' have `nrow=1` and `ncol=nsim`. `Spatial` has `nrow=nareas`. Matrices can be empty matrix, populated with all NAs
#' (both mean no change in management with respect to this element (e.g. `Effort`)), or populated with a recommendation.
#' MPs must either return a recommendation or no recommendation for every simulation for a particular slot (i.e. cannot have some NA and some values).
#' @param y The projection year
#' @param nyears The number of historical years
#' @param proyears The number of projection years
#' @param nsim The number of simulations
#' @param Biomass_P An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas` with total biomass in the projection years
#' @param VBiomass_P An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas` with vulnerable biomass in the projection years
#' @param LastTAE A vector of length `nsim` with the most recent TAE
#' @param LastSpatial A matrix of `nrow=nareas` and `ncol=nsim` with the most recent spatial management arrangements
#' @param LastAllocat A vector of length `nsim` with the most recent allocation
#' @param LastTAC A vector of length `nsim` with the most recent TAC
#' @param TACused A vector of length `nsim` with the most recent TAC
#' @param maxF A numeric value with maximum allowed F. From `OM@maxF`
#' @param LR5_P A matrix with `nyears+proyears` rows and `nsim` columns with the first length at 5 percent retention.
#' @param LFR_P A matrix with `nyears+proyears` rows and `nsim` columns with the first length at full retention.
#' @param Rmaxlen_P A matrix with `nyears+proyears` rows and `nsim` columns with the retention at maximum length.
#' @param retL_P An array with dimensions `nsim`, `nCALbins` and `nyears+proyears` with retention at length
#' @param retA_P An array with dimensions `nsim`, `maxage` and `nyears+proyears` with retention at age
#' @param L5_P A matrix with `nyears+proyears` rows and `nsim` columns with the first length at 5 percent selectivity
#' @param LFS_P A matrix with `nyears+proyears` rows and `nsim` columns with the first length at full selectivity
#' @param Vmaxlen_P A matrix with `nyears+proyears` rows and `nsim` columns with the selectivity at maximum length.
#' @param SLarray_P An array with dimensions `nsim`, `nCALbins` and `nyears+proyears` with selectivity at length
#' @param V_P An array with dimensions `nsim`, `maxage` and `nyears+proyears` with selectivity at age
#' @param Fdisc_P  vector of length `nsim` with discard mortality. From `OM@Fdisc` but can be updated by MP (`Rec@Fdisc`)
#' @param DR_P A matrix with `nyears+proyears` rows and `nsim` columns with the fraction discarded.
#' @param M_ageArray An array with dimensions `nsim`, `maxage` and `nyears+proyears` with natural mortality at age
#' @param FM_P An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas` with total fishing mortality
#' @param FM_Pret An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas` with fishing mortality of the retained fish
#' @param Z_P An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas` with total mortality
#' @param CB_P An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas` with total catch
#' @param CB_Pret An array with dimensions `nsim`, `maxage`, `proyears`, and `nareas` with retained catch
#' @param TAC_f A matrix with `nsim` rows and `proyears` columns with the TAC implementation error
#' @param E_f A matrix with `nsim` rows and `proyears` columns with the effort implementation error
#' @param SizeLim_f A matrix with `nsim` rows and `proyears` columns with the size limit implementation error
#' @param FinF A numeric vector of length `nsim` with fishing mortality in the last historical year
#' @param Spat_targ A numeric vector of length `nsim` with spatial targeting
#' @param CAL_binsmid A numeric vector of length `nCALbins` with mid-points of the CAL bins
#' @param Linf A numeric vector of length `nsim` with Linf (from `Stock@Linf`)
#' @param Len_age An array with dimensions `nsim`, `maxage`, and `nyears+proyears` with length-at-age
#' @param maxage A numeric value with maximum age from `Stock@maxage`
#' @param nareas A numeric value with number of areas
#' @param Asize A matrix with `nsim` rows and `nareas` columns with the relative size of each area
#' @param nCALbins The number of CAL bins. Should be the same as `length(CAL_binsmid)`
#' @param qs A numeric vector of length `nsim` with catchability coefficient
#' @param qvar A matrix with `nsim` rows and `proyears` columns with catchability variability
#' @param qinc A numeric vector of length `nsim` with average annual change in catchability
#' @param Effort_pot A numeric vector of potential effort
#' @param checks Logical. Run internal checks? Currently not used.
#'
#' @return A named list with updated population dynamics
#' @author A. Hordyk
#'
#' @keywords internal
CalcMPDynamics <- function(MPRecs, y, nyears, proyears, nsim, Biomass_P,
                           VBiomass_P,
                           LastTAE, histTAE, LastSpatial, LastAllocat, LastTAC,
                           TACused, maxF,
                           LR5_P, LFR_P, Rmaxlen_P, retL_P, retA_P,
                           L5_P, LFS_P, Vmaxlen_P, SLarray_P, V_P,
                           Fdisc_P, DR_P,
                           FM_P, FM_Pret, Z_P, CB_P, CB_Pret, Effort_pot,
                           StockPars, FleetPars, ImpPars,
                           checks=FALSE) {

  n_age <- StockPars$maxage + 1 # include age-0

  # Implementation Error
  Effort_Imp_Error <- ImpPars$E_y
  SL_Imp_Error <- ImpPars$SizeLim_y
  TAC_Imp_Error <- ImpPars$TAC_y

  if (!is.null(MPRecs$type) && MPRecs$type == 'reference') {
    Effort_Imp_Error[Effort_Imp_Error!=1] <- 1
    SL_Imp_Error[SL_Imp_Error!=1] <- 1
    TAC_Imp_Error[TAC_Imp_Error!=1] <- 1
  }

  # Effort
  if (length(MPRecs$Effort) == 0) { # no max effort recommendation
    if (y==1) TAE_err <- LastTAE  *  Effort_Imp_Error[,y] # max effort is unchanged but has implementation error
    if (y>1) TAE_err <- LastTAE  * Effort_Imp_Error[,y] # max effort is unchanged but has implementation error
  } else if (length(MPRecs$Effort) != nsim) {
    stop("Effort recommmendation is not 'nsim' long.\n Does MP return Effort recommendation under all conditions?")
  } else {
    # a maximum effort recommendation
    if (!all(is.na(histTAE))) {
      TAE_err <- histTAE * MPRecs$Effort * Effort_Imp_Error[,y] # adjust existing TAE adjustment with implementation error
    } else {
      TAE_err <- MPRecs$Effort * Effort_Imp_Error[,y] # adjust existing TAE adjustment with implementation error
    }
  }

  # Spatial
  if (all(is.na(MPRecs$Spatial))) { # no spatial recommendation
    Si <- LastSpatial # spatial is unchanged
  } else if (any(is.na(MPRecs$Spatial))) {
    stop("Spatial recommmendation has some NAs.\n Does MP return Spatial recommendation under all conditions?")
  } else {
    Si <- MPRecs$Spatial # change spatial fishing
  }
  if (all(dim(Si) != c(StockPars$nareas, nsim))) stop("Spatial recommmendation not nareas long")

  # Allocation
  if (length(MPRecs$Allocate) == 0) { # no allocation recommendation
    Ai <- LastAllocat # allocation is unchanged
  } else if (length(MPRecs$Allocate) != nsim) {
    stop("Allocate recommmendation is not 'nsim' long.\n Does MP return Allocate recommendation under all conditions?")
  } else {
    Ai <- MPRecs$Allocate # change in spatial allocation
  }
  Ai <- as.numeric(Ai)

  # Retention Curve
  RetentFlag <- FALSE # should retention curve be updated for future years?
  # LR5
  if (length(MPRecs$LR5) == 0) { # no  recommendation
    LR5_P[,(y + nyears):(nyears+proyears)] <- matrix(LR5_P[,y + nyears-1],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # unchanged

  } else if (length(MPRecs$LR5) != nsim) {
    stop("LR5 recommmendation is not 'nsim' long.\n Does MP return LR5 recommendation under all conditions?")
  } else {
    LR5_P[,(y + nyears):(nyears+proyears)] <- matrix(MPRecs$LR5 *  SL_Imp_Error[,y],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # recommendation with implementation error
    RetentFlag <- TRUE
  }
  # LFR
  if (length(MPRecs$LFR) == 0) { # no  recommendation
    LFR_P[,(y + nyears):(nyears+proyears)] <- matrix(LFR_P[,y + nyears-1],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # unchanged
  } else if (length(MPRecs$LFR) != nsim) {
    stop("LFR recommmendation is not 'nsim' long.\n Does MP return LFR recommendation under all conditions?")
  } else {
    LFR_P[,(y + nyears):(nyears+proyears)] <- matrix(MPRecs$LFR *  SL_Imp_Error[,y],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # recommendation with implementation error
    RetentFlag <- TRUE
  }
  # Rmaxlen
  if (length(MPRecs$Rmaxlen) == 0) { # no  recommendation
    Rmaxlen_P[,(y + nyears):(nyears+proyears)] <- matrix(Rmaxlen_P[,y + nyears-1],
                                                         ncol=(length((y + nyears):(nyears+proyears))),
                                                         nrow=nsim, byrow=FALSE)   # unchanged

  } else if (length(MPRecs$Rmaxlen) != nsim) {
    stop("Rmaxlen recommmendation is not 'nsim' long.\n Does MP return Rmaxlen recommendation under all conditions?")
  } else {
    Rmaxlen_P[,(y + nyears):(nyears+proyears)] <- matrix(MPRecs$Rmaxlen,
                                                         ncol=(length((y + nyears):(nyears+proyears))),
                                                         nrow=nsim, byrow=FALSE) # recommendation
    RetentFlag <- TRUE
  }

  # HS - harvest slot
  if (length(MPRecs$HS) == 0) { # no  recommendation
    HS <- rep(1E5, nsim) # no harvest slot
  } else if (length(MPRecs$HS) != nsim) {
    stop("HS recommmendation is not 'nsim' long.\n Does MP return HS recommendation under all conditions?")
  } else {
    HS <- MPRecs$HS  *  SL_Imp_Error[,y] # recommendation
    RetentFlag <- TRUE
  }

  # Selectivity Curve
  SelectFlag <- FALSE # has selectivity been updated?
  # L5
  if (length(MPRecs$L5) == 0) { # no  recommendation
    L5_P[,(y + nyears):(nyears+proyears)] <- matrix(L5_P[,y + nyears-1],
                                                    ncol=(length((y + nyears):(nyears+proyears))),
                                                    nrow=nsim, byrow=FALSE) # unchanged

  } else if (length(MPRecs$L5) != nsim) {
    stop("L5 recommmendation is not 'nsim' long.\n Does MP return L5 recommendation under all conditions?")
  } else {
    L5_P[,(y + nyears):(nyears+proyears)] <- matrix(MPRecs$L5 *  SL_Imp_Error[,y],
                                                    ncol=(length((y + nyears):(nyears+proyears))),
                                                    nrow=nsim, byrow=FALSE) # recommendation with implementation error
    SelectFlag <- TRUE
  }
  # LFS
  if (length(MPRecs$LFS) == 0) { # no  recommendation
    LFS_P[,(y + nyears):(nyears+proyears)] <- matrix(LFS_P[,y + nyears-1],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # unchanged
  } else if (length(MPRecs$LFS) != nsim) {
    stop("LFS recommmendation is not 'nsim' long.\n Does MP return LFS recommendation under all conditions?")
  } else {
    LFS_P[,(y + nyears):(nyears+proyears)] <- matrix(MPRecs$LFS *  SL_Imp_Error[,y],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # recommendation with implementation error
    SelectFlag <- TRUE
  }
  # Vmaxlen
  if (length(MPRecs$Vmaxlen) == 0) { # no  recommendation
    Vmaxlen_P[,(y + nyears):(nyears+proyears)] <- matrix(Vmaxlen_P[,y + nyears-1],
                                                         ncol=(length((y + nyears):(nyears+proyears))),
                                                         nrow=nsim, byrow=FALSE)   # unchanged

  } else if (length(MPRecs$Vmaxlen) != nsim) {
    stop("Vmaxlen recommmendation is not 'nsim' long.\n Does MP return Vmaxlen recommendation under all conditions?")
  } else {
    Vmaxlen_P[,(y + nyears):(nyears+proyears)] <- matrix(MPRecs$Vmaxlen,
                                                         ncol=(length((y + nyears):(nyears+proyears))),
                                                         nrow=nsim, byrow=FALSE) # recommendation
    SelectFlag <- TRUE
  }

  # Discard Mortality
  if (length(MPRecs$Fdisc) >0) { # Fdisc has changed
    if (length(MPRecs$Fdisc) != nsim) stop("Fdisc recommmendation is not 'nsim' long.\n Does MP return Fdisc recommendation under all conditions?")
    Fdisc_P <- MPRecs$Fdisc
    RetentFlag <- TRUE
  }

  # Discard Ratio
  if (length(MPRecs$DR)>0) { # DR has changed
    if (length(MPRecs$DR) != nsim) stop("DR recommmendation is not 'nsim' long.\n Does MP return DR recommendation under all conditions?")
    DR_P[,(y+nyears):(nyears+proyears)] <- matrix(MPRecs$DR, ncol=length((y+nyears):(nyears+proyears)), nrow=nsim, byrow=FALSE)
    RetentFlag <- TRUE
  }

  # Update Selectivity and Retention Curve
  if (SelectFlag | RetentFlag) {
    yr <- y+nyears
    allyrs <- (y+nyears):(nyears+proyears)  # update vulnerability for all future years

    srs <- (StockPars$Linf - LFS_P[,yr]) / ((-log(Vmaxlen_P[,yr],2))^0.5) # descending limb
    srs[!is.finite(srs)] <- Inf
    sls <- (LFS_P[,yr] - L5_P[,yr]) / ((-log(0.05,2))^0.5) # ascending limb

    CAL_binsmidMat <- matrix(StockPars$CAL_binsmid, nrow=nsim, ncol=length(StockPars$CAL_binsmid), byrow=TRUE)
    selLen <- t(sapply(1:nsim, getsel, lens=CAL_binsmidMat, lfs=LFS_P[,yr], sls=sls, srs=srs))

    for (yy in allyrs) {
      # calculate new selectivity at age curve
      V_P[ , , yy] <- t(sapply(1:nsim, getsel, lens=StockPars$Len_age[,,yy], lfs=LFS_P[,yy], sls=sls, srs=srs))
      SLarray_P[,, yy] <- selLen # calculate new selectivity at length curve
    }

    # sim <- 158
    # plot(CAL_binsmid, selLen[sim,], type="b")
    # lines(c(L5_P[yr,sim], L5_P[yr,sim]), c(0, 0.05), lty=2)
    # lines(c(LFS_P[yr,sim], LFS_P[yr,sim]), c(0, 1), lty=2)
    # lines(c(StockPars$Linf[sim], StockPars$Linf[sim]), c(0, Vmaxlen_P[yr,sim]), lty=2)

    # calculate new retention curve
    yr <- y+nyears
    allyrs <- (y+nyears):(nyears+proyears)  # update vulnerability for all future years

    srs <- (StockPars$Linf - LFR_P[,yr]) / ((-log(Rmaxlen_P[,yr],2))^0.5) # retention parameters are constant for all years
    srs[!is.finite(srs)] <- Inf
    sls <- (LFR_P[,yr] - LR5_P[,yr]) / ((-log(0.05,2))^0.5)

    CAL_binsmidMat <- matrix(StockPars$CAL_binsmid, nrow=nsim, ncol=length(StockPars$CAL_binsmid), byrow=TRUE)
    relLen <- t(sapply(1:nsim, getsel, lens=CAL_binsmidMat, lfs=LFR_P[,yr], sls=sls, srs=srs))

    for (yy in allyrs) {
      # calculate new retention at age curve
      retA_P[ , , yy] <- t(sapply(1:nsim, getsel, lens=StockPars$Len_age[,,yy], lfs=LFR_P[,yy], sls=sls, srs=srs))
      retL_P[,, yy] <- relLen  # calculate new retention at length curve
    }

    # upper harvest slot
    aboveHS <- StockPars$Len_age[,,allyrs, drop=FALSE]>array(HS, dim=c(nsim, n_age, length(allyrs)))
    tretA_P <- retA_P[,,allyrs]
    tretA_P[aboveHS] <- 0
    retA_P[,,allyrs] <- tretA_P
    for (ss in 1:nsim) {
      index <- which(StockPars$CAL_binsmid >= HS[ss])
      retL_P[ss, index, allyrs] <- 0
    }

    dr <- aperm(abind::abind(rep(list(DR_P), n_age), along=3), c(1,3,2))
    retA_P[,,allyrs] <- (1-dr[,,yr]) * retA_P[,,yr]
    dr <- aperm(abind::abind(rep(list(DR_P), StockPars$nCALbins), along=3), c(1,3,2))
    retL_P[,,allyrs] <- (1-dr[,,yr]) * retL_P[,,yr]

    # update realized vulnerablity curve with retention and dead discarded fish
    Fdisc_array1 <- array(Fdisc_P, dim=c(nsim, n_age, length(allyrs)))

    V_P2 <- V_P
    V_P2[,,allyrs] <- V_P[,,allyrs, drop=FALSE] * (retA_P[,,allyrs, drop=FALSE] + (1-retA_P[,,allyrs, drop=FALSE])*Fdisc_array1)

    Fdisc_array2 <- array(Fdisc_P, dim=c(nsim, StockPars$nCALbins, length(allyrs)))
    SLarray_P[,,allyrs]  <- SLarray_P[,,allyrs, drop=FALSE] * (retL_P[,,allyrs, drop=FALSE]+ (1-retL_P[,,allyrs, drop=FALSE])*Fdisc_array2)

    # Realised Retention curves
    retA_P[,,allyrs] <- retA_P[,,allyrs] * V_P[,,allyrs]
    retL_P[,,allyrs] <- retL_P[,,allyrs] * SLarray_P[,,allyrs]

    V_P <- V_P2
  }

  CurrentB <- Biomass_P[,,y,] # biomass at the beginning of year
  CurrentVB <- array(NA, dim=dim(CurrentB))
  Catch_tot <- Catch_retain <- array(NA, dim=dim(CurrentB)) # catch this year arrays
  FMc <- Zc <- array(NA, dim=dim(CurrentB)) # fishing and total mortality this year

  # indices
  SAYRL <- as.matrix(expand.grid(1:nsim, 1:n_age, nyears, 1:StockPars$nareas))  # Final historical year
  SAYRt <- as.matrix(expand.grid(1:nsim, 1:n_age, y + nyears, 1:StockPars$nareas))  # Trajectory year
  SAYR <- as.matrix(expand.grid(1:nsim, 1:n_age, y, 1:StockPars$nareas))
  SAR <- SAYR[, c(1,2,4)]
  SAY <- SAYR[,c(1:3)]

  SYt <- SAYRt[, c(1, 3)]
  SAYt <- SAYRt[, 1:3]
  SR <- SAYR[, c(1, 4)]
  SA1 <- SAYR[, 1:2]
  S1 <- SAYR[, 1]
  SY1 <- SAYR[, c(1, 3)]
  SAY1 <- SAYR[, 1:3]
  SYA <- as.matrix(expand.grid(1:nsim, 1, 1:n_age))  # Projection year
  SY <- SYA[, 1:2]
  SA <- SYA[, c(1, 3)]
  S <- SYA[, 1]

  CurrentVB[SAR] <- CurrentB[SAR] * V_P[SAYt] # update available biomass if selectivity has changed

  # Calculate fishing distribution if all areas were open
  newVB <- apply(CurrentVB, c(1,3), sum) # calculate total vuln biomass by area
  fishdist <- (newVB^FleetPars$Spat_targ)/apply(newVB^FleetPars$Spat_targ, 1, sum)  # spatial preference according to spatial vulnerable biomass

  d1 <- t(Si) * fishdist  # distribution of fishing effort
  fracE <- apply(d1, 1, sum) # fraction of current effort in open areas
  fracE2 <- d1 * (fracE + (1-fracE) * Ai)/fracE # re-distribution of fishing effort accounting for re-allocation of effort
  fishdist <- fracE2 # fishing effort by area

  # ---- no TAC - calculate F with bio-economic effort ----
  if (all(is.na(TACused))) {
    if (all(is.na(Effort_pot)) & all(is.na(TAE_err))) Effort_pot <- rep(1, nsim) # historical effort
    if (all(is.na(Effort_pot))) Effort_pot <- TAE_err[1,]
    # fishing mortality with bio-economic effort
    FM_P[SAYR] <- (FleetPars$FinF[S1] * Effort_pot[S1] * V_P[SAYt] * t(Si)[SR] * fishdist[SR] *
                     FleetPars$qvar[SY1] * (FleetPars$qs[S1]*(1 + FleetPars$qinc[S1]/100)^y))/StockPars$Asize[SR]

    # retained fishing mortality with bio-economic effort
    FM_Pret[SAYR] <- (FleetPars$FinF[S1] * Effort_pot[S1] * retA_P[SAYt] * t(Si)[SR] * fishdist[SR] *
                        FleetPars$qvar[SY1] * FleetPars$qs[S1]*(1 + FleetPars$qinc[S1]/100)^y)/StockPars$Asize[SR]

    Effort_req <- Effort_pot

  }

  # ---- calculate required F and effort for TAC recommendation ----
  if (!all(is.na(TACused))) { # a TAC has been set
    # if MP returns NA - TAC is set to TAC from last year
    TACused[is.na(TACused)] <- LastTAC[is.na(TACused)]
    TACusedE <-  TAC_Imp_Error[,y]*TACused   # TAC taken after implementation error

    # Calculate total vulnerable biomass available mid-year accounting for any changes in selectivity &/or spatial closures
    M_array <- array(0.5*StockPars$M_ageArray[,,nyears+y], dim=c(nsim, n_age, StockPars$nareas))
    Atemp <- apply(CurrentVB * exp(-M_array), c(1,3), sum) # mid-year before fishing
    availB <- apply(Atemp * t(Si), 1, sum) # adjust for spatial closures

    # Calculate total F (using Steve Martell's approach http://api.admb-project.org/baranov_8cpp_source.html)
    expC <- TACusedE
    expC[TACusedE> availB] <- availB[TACusedE> availB] * 0.99

    Ftot <- sapply(1:nsim, calcF, expC, V_P, Biomass_P, fishdist,
                   StockPars$Asize,
                   StockPars$maxage,
                   StockPars$nareas,
                   StockPars$M_ageArray,nyears, y)

    # apply max F constraint
    Ftot[Ftot<0] <- maxF
    Ftot[!is.finite(Ftot)] <- maxF
    Ftot[Ftot>maxF] <- maxF

    # Calculate F & Z by age class
    FM_P[SAYR] <- Ftot[S] * V_P[SAYt] * fishdist[SR]/StockPars$Asize[SR]
    FM_Pret[SAYR] <- Ftot[S] * retA_P[SAYt] * fishdist[SR]/StockPars$Asize[SR]
    Z_P[SAYR] <- FM_P[SAYR] + StockPars$M_ageArray[SAYt] # calculate total mortality

    # Calculate total and retained catch
    CB_P[SAYR] <- FM_P[SAYR]/Z_P[SAYR] * (1-exp(-Z_P[SAYR])) * Biomass_P[SAYR]
    CB_Pret[SAYR] <- FM_Pret[SAYR]/Z_P[SAYR] * (1-exp(-Z_P[SAYR])) * Biomass_P[SAYR]

    # Calculate total removals when CB_Pret == TAC - total removal > retained when discarding
    actualremovals <- apply(CB_P[,,y,], 1, sum)
    retained <- apply(CB_Pret[,,y,], 1, sum)
    ratio <- actualremovals/retained # ratio of actual removals to retained catch
    ratio[!is.finite(ratio)] <- 0
    ratio[ratio>1E5] <- 1E5

    temp <- CB_Pret[,,y,]/apply(CB_Pret[,,y,], 1, sum) # distribution by age & area of retained fish
    Catch_retain <- TACusedE * temp  # retained catch
    Catch_tot <- CB_P[,,y,]/apply(CB_P[,,y,], 1, sum) # distribution by age & area of caught fish
    temp <- Catch_tot/apply(Catch_tot, 1, sum) # distribution of removals
    Catch_tot <- TACusedE * ratio * temp # scale up total removals (if applicable)

    # total removals can't be more than available biomass
    chk <- apply(Catch_tot, 1, sum) > availB
    if (sum(chk)>0) {
      c_temp <- apply(Catch_tot[chk,,, drop=FALSE], 1, sum)
      ratio_temp <- (availB[chk]/c_temp) * 0.99
      # scale total catches to 0.99 available biomass
      if (sum(chk)>1) Catch_tot[chk,, ] <- Catch_tot[chk,,] * array(ratio_temp, dim=c(sum(chk), n_age, StockPars$nareas))
      if (sum(chk)==1) Catch_tot[chk,, ] <- Catch_tot[chk,,] * array(ratio_temp, dim=c(n_age, StockPars$nareas))
    }

    # check where actual catches are higher than TAC due to discarding (with imp error)
    ind <- which(apply(Catch_tot, 1, sum) > TACusedE)
    if (length(ind)>0) {
      # update Ftot calcs
      Ftot[ind] <- sapply(ind, calcF, TACusedE, V_P, Biomass_P, fishdist, StockPars$Asize,
                          StockPars$maxage, StockPars$nareas, StockPars$M_ageArray,nyears, y)
    }

    # Effort relative to last historical with this catch
    Effort_req <- Ftot/(FleetPars$FinF * FleetPars$qs*FleetPars$qvar[,y]*
                          (1 + FleetPars$qinc/100)^y) * apply(fracE2, 1, sum) # effort required

    Effort_req[Ftot<=1E-3] <- tiny

    # Calculate F & Z by age class
    FM_P[SAYR] <- Ftot[S] * V_P[SAYt] * fishdist[SR]/StockPars$Asize[SR]
    FM_Pret[SAYR] <- Ftot[S] * retA_P[SAYt] * fishdist[SR]/StockPars$Asize[SR]
    Z_P[SAYR] <- FM_P[SAYR] + StockPars$M_ageArray[SAYt] # calculate total mortality
  }

  # Effort_req - effort required to catch TAC
  # Effort_pot - potential effort this year (active fishers) from bio-economic model
  # Effort_act - actual effort this year
  # TAE_err - maximum actual effort limit - with imp error
  # Effort_act < Effort_pot if Effort_req < Effort_pot

  # Limit effort to potential effort from bio-economic model
  Effort_act <- Effort_req
  if (!all(is.na(Effort_pot))) {
    excessEff <- Effort_req>Effort_pot # simulations where required effort > potential effort
    Effort_act[excessEff] <- Effort_pot[excessEff] # actual effort can't be more than bio-economic effort
  }

  # Limit actual effort <= TAE
  if (!all(is.na(TAE_err))) { # a TAE exists
    Effort_act[Effort_act>TAE_err] <- TAE_err[Effort_act>TAE_err]
  }
  Effort_act[Effort_act<=0] <- tiny

  # --- Re-calculate catch given actual effort ----
  # TODO should really only do this for the sims where Effort_act is different than Effort_req
  # fishing mortality with actual effort
  FM_P[SAYR] <- (FleetPars$FinF[S1] * Effort_act[S1] * V_P[SAYt] * t(Si)[SR] * fishdist[SR] *
                 FleetPars$qvar[SY1] * (FleetPars$qs[S1]*(1 + FleetPars$qinc[S1]/100)^y))/StockPars$Asize[SR]

  # retained fishing mortality with actual effort
  FM_Pret[SAYR] <- (FleetPars$FinF[S1] * Effort_act[S1] * retA_P[SAYt] * t(Si)[SR] * fishdist[SR] *
                    FleetPars$qvar[SY1] * FleetPars$qs[S1]*(1 + FleetPars$qinc[S1]/100)^y)/StockPars$Asize[SR]

  # Apply maxF constraint
  FM_P[SAYR][FM_P[SAYR] > maxF] <- maxF
  FM_Pret[SAYR][FM_Pret[SAYR] > maxF] <- maxF
  Z_P[SAYR] <- FM_P[SAYR] + StockPars$M_ageArray[SAYt] # calculate total mortality

  # Update catches after maxF constraint
  CB_P[SAYR] <- FM_P[SAYR]/Z_P[SAYR] * (1-exp(-Z_P[SAYR])) * Biomass_P[SAYR]
  CB_Pret[SAYR] <- FM_Pret[SAYR]/Z_P[SAYR] * (1-exp(-Z_P[SAYR])) * Biomass_P[SAYR]

  # Calculate total F (using Steve Martell's approach http://api.admb-project.org/baranov_8cpp_source.html)
  totalCatch <- apply(CB_P[,,y,], 1, sum)

  Ftot <- sapply(1:nsim, calcF, totalCatch, V_P, Biomass_P, fishdist,
                 Asize=StockPars$Asize, maxage=StockPars$maxage, StockPars$nareas,
                 M_ageArray=StockPars$M_ageArray,nyears, y) # update if effort has changed

  # Effort relative to last historical with this catch
  Effort_act <- Ftot/(FleetPars$FinF * FleetPars$qs*FleetPars$qvar[,y]*
                        (1 + FleetPars$qinc/100)^y)  # effort required - already accounts for effort-by-area

  Effort_act[Ftot<=1E-3] <- tiny

  # Returns
  TAE <- MPRecs$Effort
  if (length(TAE)==0) TAE <- rep(NA, nsim)
  out <- list()
  out$TACrec <- TACused
  out$V_P <- V_P
  out$SLarray_P <- SLarray_P
  out$retA_P <- retA_P
  out$retL_P <- retL_P
  out$Fdisc_P <- Fdisc_P
  out$VBiomass_ <- VBiomass_P
  out$Z_P <- Z_P
  out$FM_P <- FM_P
  out$FM_Pret <- FM_Pret
  out$CB_P <- CB_P
  out$CB_Pret <- CB_Pret
  out$Si <- Si
  out$Ai <- Ai
  out$TAE <- TAE
  out$Effort <- Effort_act # actual effort this year
  out$Ftot <- Ftot

  out$LR5_P <- LR5_P
  out$LFR_P <- LFR_P
  out$Rmaxlen_P <- Rmaxlen_P
  out$L5_P <- L5_P
  out$LFS_P <- LFS_P
  out$Vmaxlen_P <- Vmaxlen_P
  out$Fdisc_P <- Fdisc_P
  out$DR_P <- DR_P

  out
}



calcF <- function(x, TACusedE, V_P, Biomass_P, fishdist, Asize, maxage, nareas,
                  M_ageArray, nyears, y) {
  ct <- TACusedE[x]
  ft <- ct/sum(Biomass_P[x,,y,] * V_P[x,,y+nyears]) # initial guess

  fishdist[x,] <- fishdist[x,]/sum(fishdist[x,])

  if (ft <= 1E-3) return(tiny)
  for (i in 1:50) {
    Fmat <- ft * matrix(V_P[x,,y+nyears], nrow=maxage+1, ncol=nareas) *
      matrix(fishdist[x,], maxage+1, nareas, byrow=TRUE)/
      matrix(Asize[x,], maxage+1, nareas, byrow=TRUE) # distribute F over age and areas
    Zmat <- Fmat + matrix(M_ageArray[x,,y+nyears], nrow=maxage+1, ncol=nareas, byrow=FALSE)
    predC <- Fmat/Zmat * (1-exp(-Zmat)) * Biomass_P[x,,y,] # predicted catch
    pct <- sum(predC)

    Omat <- (1-exp(-Zmat)) * Biomass_P[x,,y,]
    # derivative of catch wrt ft
    dct <- sum(Omat/Zmat - ((Fmat * Omat)/Zmat^2) + Fmat/Zmat * exp(-Zmat) * Biomass_P[x,,y,])
    ft <-  ft - (pct - ct)/dct

    if (abs(pct - ct)<1E-6) break
  }

  ft
}


optDfun <- function(Perrmulti, x, initD, Nfrac, R0, Perr_y, surv,
                    Wt_age, SSB0, n_age) {

  initRecs <- rev(Perr_y[x,1:n_age]) * exp(Perrmulti)

  SSN <- Nfrac[x,] * R0[x] *  initRecs # Calculate initial spawning stock numbers
  SSB <- SSN * Wt_age[x,,1]    # Calculate spawning stock biomass

  (sum(SSB)/SSB0[x] - initD[x])^2
}

optDfunwrap <- function(x, initD, Nfrac, R0, initdist, Perr_y, surv,
                        Wt_age, SSB0, n_age) {
  interval <- log(c(0.01, 10))

  optD <- optimise(optDfun, interval=interval, x=x, initD=initD, Nfrac=Nfrac,
                   R0=R0, Perr_y=Perr_y, surv=surv,
                   Wt_age=Wt_age, SSB0=SSB0, n_age=n_age)
  exp(optD$minimum)
}


# Input Control Functions Wrapper function for input control methods


#' Runs input control MPs on a Data object.
#'
#' Function runs a MP (or MPs) of class 'Input' and returns a list: input
#' control recommendation(s) in element 1 and Data object in element 2.
#'
#' @param Data A object of class Data
#' @param MPs A vector of MPs of class 'Input'
#' @param reps Number of stochastic repetitions - often not used in input
#' control MPs.
#' @author A. Hordyk
#' @export
runInMP <- function(Data, MPs = NA, reps = 100) {

  nsims <- length(Data@Mort)
  if (.hasSlot(Data, "nareas")) {
    nareas <- Data@nareas
  } else {
    nareas <- 2
  }

  nMPs <- length(MPs)

  returnList <- list() # a list nMPs long containing MPs recommendations
  recList <- list() # a list containing nsim recommendations from a single MP

  if (!sfIsRunning() | (nMPs < 8 & nsims < 8)) {
    for (ff in 1:nMPs) {
      temp <- sapply(1:nsims, MPs[ff], Data = Data, reps = reps)
      slots <- slotNames(temp[[1]])
      for (X in slots) { # sequence along recommendation slots
        if (X == "Misc") { # convert to a list nsim by nareas
          rec <- lapply(temp, slot, name=X)
        } else {
          rec <- unlist(lapply(temp, slot, name=X))
        }
        if (X == "Spatial") { # convert to a matrix nsim by nareas
          rec <- matrix(rec, nsims, nareas, byrow=TRUE)
        }

        recList[[X]] <- rec
        for (x in 1:nsims) Data@Misc[[x]] <- recList$Misc[[x]]
        recList$Misc <- NULL
      }
      returnList[[ff]] <- recList
    }
  } else {
    sfExport(list = "Data")
    for (ff in 1:nMPs) {
      temp <- sfSapply(1:nsims, MPs[ff], Data = Data, reps = reps)
      slots <- slotNames(temp[[1]])
      for (X in slots) { # sequence along recommendation slots
        if (X == "Misc") { # convert to a list nsim by nareas
          rec <- lapply(temp, slot, name=X)
        } else {
          rec <- unlist(lapply(temp, slot, name=X))
        }
        if (X == "Spatial") { # convert to a matrix nsim by nareas
          rec <- matrix(rec, nsims, nareas, byrow=TRUE)
        }

        recList[[X]] <- rec
        for (x in 1:nsims) Data@Misc[[x]] <- recList$Misc[[x]]
        recList$Misc <- NULL
      }
      returnList[[ff]] <- recList
    }
  }

  return(list(returnList, Data))
}


projectEq <- function(x, Asize, nareas, maxage, N, pyears, M_ageArray, Mat_age, Wt_age,
                      V, retA, Perr, mov, SRrel, Find, Spat_targ, hs, R0a, SSBpR, aR, bR,
                      SSB0, MPA, maxF, Nyrs, plusgroup, Pinitdist) {

  simpop <- popdynCPP(nareas, maxage, Ncurr=N[x,,1,],
                      pyears, M_age=M_ageArray[x,,], Asize_c=Asize[x,],
                      MatAge=Mat_age[x,,],
                      WtAge=Wt_age[x,,], Vuln=V[x,,], Retc=retA[x,,], Prec=Perr[x,],
                      movc=split.along.dim(mov[x,,,,],4), SRrelc=SRrel[x],
                      Effind=Find[x,],  Spat_targc=Spat_targ[x], hc=hs[x], R0c=R0a[x,],
                      SSBpRc=SSBpR[x,], aRc=aR[x,], bRc=bR[x,], Qc=0, Fapic=0,
                      MPA=MPA,
                      maxF=maxF, control=2, SSB0c=SSB0[x], plusgroup = plusgroup)

  simpop[[1]][,Nyrs,]

}
