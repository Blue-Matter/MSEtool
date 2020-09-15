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
#' @param optVB Logical. Optimize for vulnerable biomass instead of SSB?
#' 
#' @author A. Hordyk
#' @keywords internal
CalculateQ <- function(x, StockPars, FleetPars, pyears, 
                  bounds = c(1e-05, 15), optVB) {
  
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
                  StockPars$VB0[x], optVB)
  
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
#' @param Spat_targc Spatial targetting
#' @param hc Steepness
#' @param R0c Unfished recruitment by area
#' @param SSBpRc Unfished spawning biomass per recruit by area
#' @param aRc Ricker aR
#' @param bRc Ricker bR
#' @param maxF maximum F
#' @param MPA A matrix of spatial closures by year
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group 
#' @param VB0c  Unfished vulnerable biomass
#' @param optVB Logical. Optimize for vulnerable biomass? 
#' @author A. Hordyk
#' @keywords internal
optQ <- function(logQ, depc, SSB0c, nareas, maxage, Ncurr, pyears, M_age, Asize_c,
                 MatAge, WtAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc, 
                 R0c, SSBpRc, aRc, bRc, maxF, MPA, plusgroup, VB0c, optVB) {
  
  simpop <- popdynCPP(nareas, maxage, Ncurr, pyears, M_age, Asize_c,
                      MatAge, WtAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc, 
                      R0c=R0c, SSBpRc=SSBpRc, aRc=aRc, bRc=bRc, Qc=exp(logQ), Fapic=0, 
                      maxF=maxF, MPA=MPA, control=1, SSB0c=SSB0c, 
                      plusgroup=plusgroup) 
  
  ssb <- sum(simpop[[4]][,pyears,])
  vb <- sum(simpop[[5]][,pyears,])
  if (optVB) {
    return((log(depc) - log(vb/VB0c))^2)
  }
  else {
    return((log(depc) - log(ssb/SSB0c))^2)
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
  
  #UMSY <- exp(doopt$minimum)
  
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
#' @param SRrelx SRR type for this simulation
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
  
  hx[hx>0.999] <- 0.999
  recK <- (4*hx)/(1-hx) # Goodyear compensation ratio
  reca <- recK/Egg0
  
  SPR <- EggF/Egg0
  # Calculate equilibrium recruitment at this SPR
  if (SRrelx ==1) { # BH SRR
    recb <- (reca * Egg0 - 1)/(R0x*Egg0) 
    RelRec <- (reca * EggF-1)/(recb*EggF)
  }
  if (SRrelx ==2) { # Ricker
    bR <- (log(5*hx)/(0.8*SB0))
    aR <- exp(bR*SB0)/(SB0/R0x)
    RelRec <- (log(aR*EggF/R0x))/(bR*EggF/R0x)
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

#' Calculate Reference Yield 
#'
#' @param x Integer, the simulation number
#' @param StockPars List of Stock Parameters
#' @param FleetPars List of Fleet Parameters
#' @param pyears The number of years to project forward. Equal to 'nyears' for optimizing for q.
#' @param Ncurr Array with current numbers-at-age (dim=c(nsim, maxage+1, nareas))
#' @author A. Hordyk
#' @export
#' @keywords internal
calcRefYield <- function(x, StockPars, FleetPars, pyears, Ncurr) {
  
  opt <- optimize(optYield, log(c(0.001, 10)), 
                  Asize_c=StockPars$Asize[x,], 
                  StockPars$nareas, 
                  StockPars$maxage, 
                  Ncurr=Ncurr[x,,], 
                  pyears=pyears, 
                  M_age=StockPars$M_ageArray[x,,], 
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
#' @param Vuln Vulnerablity-at-age
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