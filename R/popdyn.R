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
                  WtAge=StockPars$Wt_age[x,,], 
                  FecAge=StockPars$Fec_Age[x,,], 
                  Vuln=FleetPars$V_real[x,,],
                  Retc=FleetPars$retA_real[x,,], Prec=StockPars$Perr_y[x,],
                  movc=split.along.dim(StockPars$mov[x,,,,],4),
                  SRrelc=StockPars$SRrel[x], Effind=FleetPars$Find[x,],
                  Spat_targc=FleetPars$Spat_targ[x], hc=StockPars$hs[x],
                  R0c=StockPars$R0a[x,], SSBpRc=StockPars$SSBpR[x,],
                  aRc=StockPars$aR[x,], bRc=StockPars$bR[x,],
                  maxF=StockPars$maxF, MPA=FleetPars$MPA,
                  plusgroup=StockPars$plusgroup,
                  StockPars$VB0[x],
                  SBMSYc=StockPars$SSBMSY[x],
                  SRRfun=StockPars$SRRfun,
                  SRRpars=StockPars$SRRpars[[x]],
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
#' @param FecAge Mature weight-at-age (relative fecundity)
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
                 MatAge, WtAge, FecAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc,
                 R0c, SSBpRc, aRc, bRc, maxF, MPA, plusgroup, VB0c, SBMSYc, SRRfun, SRRpars,
                 control) {

  simpop <- popdynCPP(nareas, maxage, Ncurr, pyears, M_age, Asize_c,
                      MatAge, WtAge, FecAge, Vuln, Retc, Prec, movc, SRrelc, Effind,
                      Spat_targc, hc, R0c=R0c, SSBpRc=SSBpRc, aRc=aRc, bRc=bRc,
                      Qc=exp(logQ), Fapic=0, maxF=maxF, MPA=MPA, control=1,
                      SSB0c=SSB0c,
                      SRRfun=SRRfun,
                      SRRpars =SRRpars,
                      plusgroup=plusgroup)

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


#' Internal wrapper function to calculate MSY reference points (now using MSYCalcs)
#'
#' @param x Simulation number
#' @param M_ageArray Array of M-at-age
#' @param Wt_age Array of weight-at-age
#' @param Mat_age Array of maturity-at-age
#' @param Fec_age Array of relative fecundity-at-age (spawning biomass weight)
#' @param V Array of selectivity-at-age
#' @param maxage Vector of maximum age
#' @param R0 Vector of R0s
#' @param SRrel SRR type
#' @param hs Vector of steepness
#' @param SSBpR Vector of unfished spawners per recruit
#' @param yr.ind Year index used in calculations
#' @param plusgroup Integer. Default = 0 = no plus-group. Use 1 to include a plus-group
#' @param StockPars A list of stock parameters
#' @return Results from `MSYCalcs`
#' @export
#'
#' @keywords internal
optMSY_eq <- function(x, M_ageArray, Wt_age, Mat_age, Fec_age, V, maxage, R0, SRrel, hs,
                      SSBpR, yr.ind=1, plusgroup=0, StockPars=NULL) {
  if (length(yr.ind)==1) {
    M_at_Age <- M_ageArray[x,,yr.ind]
    Wt_at_Age <- Wt_age[x,, yr.ind]
    Fec_at_Age <- Fec_age[x,, yr.ind]
    Mat_at_Age <- Mat_age[x,, yr.ind]
    V_at_Age <- V[x,, yr.ind]
  } else {
    M_at_Age <- apply(M_ageArray[x,,yr.ind], 1, mean)
    Wt_at_Age <- apply(Wt_age[x,, yr.ind], 1, mean)
    Fec_at_Age <- apply(Fec_age[x,, yr.ind], 1, mean)
    Mat_at_Age <- apply(Mat_age[x,, yr.ind], 1, mean)
    V_at_Age <- apply(V[x,, yr.ind], 1, mean)
  }
  
  # check for M = 0 in MOMs where maxage isn't the same for each stock
  if (max(which(M_at_Age!=0)) != (maxage+1)) {
    ind <- which(M_at_Age>0)
    M_at_Age <- M_at_Age[ind]
    Wt_at_Age <- Wt_at_Age[ind]
    Mat_at_Age <- Mat_at_Age[ind]
    Fec_at_Age <- Fec_at_Age[ind]
    V_at_Age <- V_at_Age[ind]
    maxage <- length(ind)-1
  }
  
  boundsF <- c(1E-4, 3)
  
  do_eq_per_recruit <- FALSE
  if (SRrel[x] <3) do_eq_per_recruit <- TRUE
  if (SRrel[x] == 3) {
    # Check for function
    if (!is.null(formals(StockPars$relRfun)))
      do_eq_per_recruit <- TRUE
  }
  
  if (do_eq_per_recruit) {
    doopt <- optimise(MSYCalcs, log(boundsF), M_at_Age, Wt_at_Age, Mat_at_Age,
                      Fec_at_Age, V_at_Age, maxage,
                      relRfun = StockPars$relRfun, 
                      SRRpars=StockPars$SRRpars[[x]],
                      R0[x], SRrel[x], hs[x], SSBpR[x, 1], opt=1,
                      plusgroup=plusgroup)
    
    MSYs <- MSYCalcs(doopt$minimum, M_at_Age, Wt_at_Age, Mat_at_Age, Fec_at_Age,
                     V_at_Age, maxage, 
                     relRfun = StockPars$relRfun, 
                     SRRpars=StockPars$SRRpars[[x]],
                     R0[x], SRrel[x], hs[x], SSBpR[x, 1], opt=2,
                     plusgroup=plusgroup)
                     
    
    if(!doopt$objective) MSYs[] <- 0 # Assume stock crashes regardless of F
  } else {
    # Optimize for maximum yield
    nareas <- StockPars$nareas
    n_age <- StockPars$n_age
    pyears <- n_age*4
    Asize <- rep(1/nareas, nareas)
    
    surv <- rep(1, n_age) 
    surv[2:n_age] <- exp(-cumsum(M_at_Age[1:(n_age-1)]))
    if (plusgroup) {
      surv[n_age] <- surv[n_age]/(1-exp(-M_at_Age[n_age]))
    }
    # Unfished N
    R0 <- StockPars$R0[x]
    R0a <- rep(R0/nareas, nareas)
    Ninit <- R0 * surv
    Ncurr <- matrix(Ninit*1/nareas, n_age, nareas)
    M_age <- replicate(pyears,M_at_Age)
    MatAge <- replicate(pyears,Mat_at_Age)
    WtAge <- replicate(pyears,Wt_at_Age)
    FecAge <- replicate(pyears,Fec_at_Age)
    WtAgeC <- replicate(pyears,Wt_at_Age)
    Vuln <- replicate(pyears, V_at_Age)
    Retc <- array(1, dim=dim(Vuln))
    Perr_y <- rep(1, pyears+n_age)
    
    mov <- array(1/nareas, dim=c(n_age, nareas, nareas))
    movl <- list()
    movl[[1]] <- mov
    movl <- rep(movl, pyears)
    MPA <- matrix(1, pyears, nareas)
    
    opt <- optimize(optYield, log(c(0.001, 10)),
                    Asize_c=Asize,
                    StockPars$nareas,
                    StockPars$maxage,
                    Ncurr=Ncurr,
                    pyears=pyears,
                    M_age=M_age,
                    MatAge=MatAge,
                    WtAge=WtAge,
                    FecAge=FecAge,
                    WtAgeC=WtAgeC,
                    Vuln=Vuln,
                    Retc=Retc,
                    Prec=Perr_y,
                    movc=movl,
                    SRrelc=StockPars$SRrel[x],
                    Effind=rep(1, pyears),
                    Spat_targc=1,
                    hc=StockPars$hs[x],
                    R0c=R0a,
                    SSBpRc=StockPars$SSBpR[x,],
                    aRc=StockPars$aR[x,],
                    bRc=StockPars$bR[x,],
                    MPA=MPA,
                    maxF=StockPars$maxF,
                    SSB0c=StockPars$SSB0[x],
                    plusgroup=StockPars$plusgroup,
                    SRRfun=StockPars$SRRfun,
                    SRRpars=StockPars$SRRpars[[x]])

    FMSYc <- exp(opt$minimum)
    
    simpop <- popdynCPP(nareas, maxage, Ncurr, pyears, M_age, Asize,
                        MatAge, WtAge, FecAge, Vuln, Retc, Perr_y, movl, 3, 
                        rep(1, pyears), 1, StockPars$hs[x],
                        R0a, StockPars$SSBpR[x,],
                        StockPars$aR[x,], StockPars$bR[x,], Qc=0,
                        Fapic=FMSYc, MPA=MPA, maxF=StockPars$maxF, control=2,
                        SSB0c=StockPars$SSB0[x], 
                        SRRfun=StockPars$SRRfun,
                        SRRpars=StockPars$SRRpars[[x]],
                        plusgroup = StockPars$plusgroup)
    
    Yield <- -opt$objective
    F <- FMSYc
    SB <- sum(simpop[[4]][,pyears,])
    SB0 <- sum(simpop[[4]][,1,])
    SB_SB0 <- SB/SB0
    B <- sum(simpop[[2]][,pyears,])
    B0 <- sum(simpop[[2]][,1,])
    B_B0 <- B/B0
    VB <- sum(simpop[[5]][,pyears,])
    VB0 <- sum(simpop[[5]][,1,])
    VB_VB0 <- VB/VB0
    RelRec <- sum(simpop[[1]][1,pyears,])
    N0 <- sum(simpop[[1]][,1,])
    SN0 <- sum(simpop[[3]][,1,])
    
    MSYs <- c(Yield, F, SB, SB_SB0, B_B0, B, VB, VB_VB0, RelRec, SB0, B0, R0,
              NA, N0, SN0)
    names(MSYs) <- c("Yield", "F", "SB", "SB_SB0", "B_B0", "B", "VB", "VB_VB0",
                      "RelRec", "SB0", "B0", "R0", "h", "N0", "SN0")
 
    if(!opt$objective) MSYs[] <- 0 # Assume stock crashes regardless of F
    }
  
  return(MSYs)
}


# Ref_int <- function(logF, M_at_Age, Wt_at_Age, Mat_at_Age, V_at_Age, maxage, plusgroup) {
#   out <- MSYCalcs(logF, M_at_Age = M_at_Age, Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
#                   V_at_Age = V_at_Age, maxage = maxage, opt = 2, plusgroup = plusgroup)
#   out[c(1,4)]
# }

per_recruit_F_calc <- function(x, M_ageArray, Wt_age, Mat_age, Fec_age, V, maxage,
                               yr.ind=1, plusgroup=0, SPR_target = seq(0.2, 0.6, 0.05),
                               StockPars) {
  if (length(yr.ind)==1) {
    M_at_Age <- M_ageArray[x,,yr.ind]
    Wt_at_Age <- Wt_age[x,, yr.ind]
    Mat_at_Age <- Mat_age[x,, yr.ind]
    Fec_at_Age <- Fec_age[x,, yr.ind]
    V_at_Age <- V[x,, yr.ind]
  } else {
    M_at_Age <- apply(M_ageArray[x,,yr.ind], 1, mean)
    Wt_at_Age <- apply(Wt_age[x,, yr.ind], 1, mean)
    Mat_at_Age <- apply(Mat_age[x,, yr.ind], 1, mean)
    Fec_at_Age <- apply(Fec_age[x,, yr.ind], 1, mean)
    V_at_Age <- apply(V[x,, yr.ind], 1, mean)
  }

  # check for M = 0 in MOMs where maxage isn't the same for each stock
  if (max(which(M_at_Age!=0)) != (maxage+1)) {
    ind <- which(M_at_Age>0)
    M_at_Age <- M_at_Age[ind]
    Wt_at_Age <- Wt_at_Age[ind]
    Mat_at_Age <- Mat_at_Age[ind]
    Fec_at_Age <- Fec_at_Age[ind]
    V_at_Age <- V_at_Age[ind]
    maxage <- length(ind)-1
  }
  
  boundsF <- c(1E-3, 3)

  F_search <- exp(seq(log(min(boundsF)), log(max(boundsF)), length.out = 50))

  Ref_search <- Ref_int_cpp(F_search, M_at_Age = M_at_Age,
                            Wt_at_Age = Wt_at_Age, Mat_at_Age = Mat_at_Age,
                            Fec_at_Age=Fec_at_Age,
                            V_at_Age = V_at_Age,
                            StockPars$relRfun,
                            StockPars$SRRpars[[x]],
                            maxage = maxage,
                            plusgroup = plusgroup)

  YPR_search <- Ref_search[1,]
  SPR_search <- Ref_search[2,]
  RPS <- Ref_search[3,]

  FSPR <- vapply(SPR_target, function(xx)
    LinInterp_cpp(SPR_search, F_search, xlev = xx), numeric(1))

  dYPR_dF <- (YPR_search[-1] - YPR_search[-length(YPR_search)])/(F_search[-1] - F_search[-length(F_search)])
  F01 <- LinInterp_cpp(dYPR_dF, F_search[-length(YPR_search)], xlev = 0.1 * dYPR_dF[1])
  Fmax <- F_search[which.max(YPR_search)]
  
  if (StockPars$SRrel[x] == 3) {
    
    # Calculate 
    
    return(list(FSPR, FYPR = c(YPR_F01 = F01, YPR_Fmax = Fmax,
                               SPRcrash=NA, Fcrash=NA,
                               Fmed=NA)))
  }
  if(StockPars$SRrel[x] == 1) {
    CR <- 4 * StockPars$hs[x]/(1 - StockPars$hs[x])
  } else if(StockPars$SRrel[x] == 2) {
    CR <- (5 * StockPars$hs[x])^1.25
  }
  alpha <- CR/StockPars$SSBpR[x, 1]
  if(min(RPS) >= alpha) { # Unfished RPS is steeper than alpha
    SPRcrash <- min(1, RPS[1]/alpha) # Should be 1
    Fcrash <- 0
  } else if(max(RPS) <= alpha) { # Extrapolate
    SPRcrash <- local({ 
      slope <- (SPR_search[length(SPR_search)] - SPR_search[length(SPR_search) - 1])/
        (RPS[length(SPR_search)] - RPS[length(SPR_search) - 1])
      out <- SPR_search[length(SPR_search)] - slope * (RPS[length(SPR_search)] - alpha)
      max(out, 0.01)
    })
    Fcrash <- max(F_search)
  } else {  
    SPRcrash <- LinInterp_cpp(RPS, SPR_search, xlev = alpha)
    Fcrash <- LinInterp_cpp(RPS, F_search, xlev = alpha)
  }

  SSB <- apply(StockPars$SSB[x,,,], 2, sum)
  R <- apply(StockPars$N[x, 1, , ], 1, sum)
  Fmed <- LinInterp_cpp(RPS, F_search, xlev = median(R/SSB))

  return(list(FSPR, FYPR = c(YPR_F01 = F01, YPR_Fmax = Fmax,
                             SPRcrash=SPRcrash, Fcrash=Fcrash,
                             Fmed=Fmed)))
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
                  FecAge=StockPars$Fec_Age[x,,(nyears):(nyears+proyears)],
                  WtAgeC=FleetPars$Wt_age_C[x,,(nyears):(nyears+proyears)],
                  Vuln=FleetPars$V_real[x,,(nyears):(nyears+proyears)],
                  Retc=FleetPars$retA_real[x,,(nyears):(nyears+proyears)],
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
                  plusgroup=StockPars$plusgroup,
                  SRRfun=StockPars$SRRfun,
                  SRRpars=StockPars$SRRpars[[x]])

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
#' @param WtAge Weight-at-age (stock)
#' @param FecAge Mature-weight-at-age
#' @param WtAgeC Weight-at-age (fishery)
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
                   MatAge, WtAge, FecAge, WtAgeC, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc,
                   R0c, SSBpRc, aRc, bRc, Qc, MPA, maxF, SSB0c,
                   plusgroup=0, SRRfun, SRRpars) {

  FMSYc <- exp(logFa)

  simpop <- popdynCPP(nareas, maxage, Ncurr, pyears, M_age, Asize_c,
                      MatAge, WtAge, FecAge, Vuln, Retc, Prec, movc, SRrelc, Effind, Spat_targc, hc,
                      R0c, SSBpRc, aRc, bRc, Qc=0, Fapic=FMSYc, MPA=MPA, maxF=maxF, control=2,
                      SSB0c=SSB0c, 
                      SRRfun=SRRfun,
                      SRRpars=SRRpars,
                      plusgroup = plusgroup)

  # Yield
  # Cn <- simpop[[7]]/simpop[[8]] * simpop[[1]] * (1-exp(-simpop[[8]])) # retained catch
  Cn <- simpop[[6]]/simpop[[8]] * simpop[[1]] * (1-exp(-simpop[[8]])) # removals
  # Cb <- Cn[,pyears,] * WtAge[,pyears]
  # -sum(Cb)
  Cb <- Cn[,(pyears-4):pyears,] * array(WtAgeC[,(pyears-4):pyears], dim=dim(Cn[,(pyears-4):pyears,]))

  -mean(apply(Cb,2,sum, na.rm = TRUE))


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
                           checks=FALSE, control) {

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
  RetParsFlag <- FALSE
  # LR5
  if (length(MPRecs$LR5) == 0) { # no  recommendation
    LR5_P[,(y + nyears):(nyears+proyears)] <- matrix(LR5_P[,y + nyears-1],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # unchanged

  } else if (length(MPRecs$LR5) != nsim) {
    stop("LR5 recommmendation is not 'nsim' long.\n Does MP return LR5 recommendation under all conditions?")
  } else {
    lr5 <- replicate(length(y:proyears), MPRecs$LR5[1,])
    if (!prod(LR5_P[,(y + nyears):(nyears+proyears)]==lr5 *  SL_Imp_Error[,y:proyears])) {
      # LR5 has changed 
      LR5_P[,(y + nyears):(nyears+proyears)] <- matrix(lr5 *  SL_Imp_Error[,y:proyears],
                                                       ncol=(length((y + nyears):(nyears+proyears))),
                                                       nrow=nsim, byrow=FALSE) # recommendation with implementation error
      RetentFlag <- TRUE; RetParsFlag <- TRUE
    }
  }
  # LFR
  if (length(MPRecs$LFR) == 0) { # no  recommendation
    LFR_P[,(y + nyears):(nyears+proyears)] <- matrix(LFR_P[,y + nyears-1],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # unchanged
  } else if (length(MPRecs$LFR) != nsim) {
    stop("LFR recommmendation is not 'nsim' long.\n Does MP return LFR recommendation under all conditions?")
  } else {
    lrr <- replicate(length(y:proyears), MPRecs$LFR[1,])
    if (!prod(LFR_P[,(y + nyears):(nyears+proyears)]==lrr *  SL_Imp_Error[,y:proyears])) {
      # LFR has changed 
      LFR_P[,(y + nyears):(nyears+proyears)] <- matrix(lrr *  SL_Imp_Error[,y],
                                                       ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # recommendation with implementation error
      RetentFlag <- TRUE; RetParsFlag <- TRUE
    }
  }
  # Rmaxlen
  if (length(MPRecs$Rmaxlen) == 0) { # no  recommendation
    Rmaxlen_P[,(y + nyears):(nyears+proyears)] <- matrix(Rmaxlen_P[,y + nyears-1],
                                                         ncol=(length((y + nyears):(nyears+proyears))),
                                                         nrow=nsim, byrow=FALSE)   # unchanged

  } else if (length(MPRecs$Rmaxlen) != nsim) {
    stop("Rmaxlen recommmendation is not 'nsim' long.\n Does MP return Rmaxlen recommendation under all conditions?")
  } else {
    if (!all(Rmaxlen_P[,(y + nyears):(nyears+proyears)]  ==MPRecs$Rmaxlen[1,])) {
      Rmaxlen_P[,(y + nyears):(nyears+proyears)] <- matrix(MPRecs$Rmaxlen,
                                                           ncol=(length((y + nyears):(nyears+proyears))),
                                                           nrow=nsim, byrow=FALSE) # recommendation
      RetentFlag <- TRUE; RetParsFlag <- TRUE  
    }
    
  }

  # HS - harvest slot
  if (length(MPRecs$HS) == 0) { # no  recommendation
    HS <- rep(1E5, nsim) # no harvest slot
  } else if (length(MPRecs$HS) != nsim) {
    stop("HS recommmendation is not 'nsim' long.\n Does MP return HS recommendation under all conditions?")
  } else {
    HS <- MPRecs$HS  *  SL_Imp_Error[,y] # recommendation
    RetentFlag <- TRUE; RetParsFlag <- TRUE
  }

  # Selectivity Curve
  SelectFlag <- FALSE # has selectivity been updated?
  SelectParsFlag <- FALSE
  # L5
  if (length(MPRecs$L5) == 0) { # no  recommendation
    L5_P[,(y + nyears):(nyears+proyears)] <- matrix(L5_P[,y + nyears-1],
                                                    ncol=(length((y + nyears):(nyears+proyears))),
                                                    nrow=nsim, byrow=FALSE) # unchanged

  } else if (length(MPRecs$L5) != nsim) {
    stop("L5 recommmendation is not 'nsim' long.\n Does MP return L5 recommendation under all conditions?")
  } else {
    l5 <- replicate(length(y:proyears), MPRecs$L5[1,])
    if (!prod(L5_P[,(y + nyears):(nyears+proyears)]==l5 *  SL_Imp_Error[,y:proyears])) {
     #L5 has changed 
      L5_P[,(y + nyears):(nyears+proyears)] <- matrix(l5 *  SL_Imp_Error[,y:proyears],
                                                      ncol=(length((y + nyears):(nyears+proyears))),
                                                      nrow=nsim, byrow=FALSE) # recommendation with implementation error
      SelectFlag <- TRUE; SelectParsFlag <- TRUE
    }
    
    
  }
  # LFS
  if (length(MPRecs$LFS) == 0) { # no  recommendation
    LFS_P[,(y + nyears):(nyears+proyears)] <- matrix(LFS_P[,y + nyears-1],
                                                     ncol=(length((y + nyears):(nyears+proyears))),
                                                     nrow=nsim, byrow=FALSE) # unchanged
  } else if (length(MPRecs$LFS) != nsim) {
    stop("LFS recommmendation is not 'nsim' long.\n Does MP return LFS recommendation under all conditions?")
  } else {
    lfs <- replicate(length(y:proyears), MPRecs$LFS[1,])
    if (!prod(LFS_P[,(y + nyears):(nyears+proyears)]==lfs *  SL_Imp_Error[,y:proyears])) {
     # LFS has changed 
      LFS_P[,(y + nyears):(nyears+proyears)] <- matrix(lfs *  SL_Imp_Error[,y:proyears],
                                                       ncol=(length((y + nyears):(nyears+proyears))),
                                                       nrow=nsim, byrow=FALSE) # recommendation with implementation error
      SelectFlag <- TRUE; SelectParsFlag <- TRUE
    }
   
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
    SelectFlag <- TRUE; SelectParsFlag <- TRUE
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
      # update new selectivity at length curve
      SLarray_P[,, yy] <- selLen # calculate new selectivity at length curve
    } 
  
    # calculate selectivity-at-age from selectivity-at-length
    if (snowfall::sfIsRunning()) {
      VList <- snowfall::sfLapply(1:nsim, calcV, Len_age=StockPars$Len_age[,,allyrs, drop=FALSE],
                                  LatASD=StockPars$LatASD[,,allyrs, drop=FALSE], SLarray=SLarray_P[,,allyrs, drop=FALSE],
                                  n_age=n_age, nyears=0, proyears=length(allyrs),
                                  CAL_binsmid=StockPars$CAL_binsmid)
    } else {
      VList <- lapply(1:nsim, calcV, Len_age=StockPars$Len_age[,,allyrs, drop=FALSE],
                      LatASD=StockPars$LatASD[,,allyrs, drop=FALSE], SLarray=SLarray_P[,,allyrs, drop=FALSE],
                      n_age=n_age, nyears=0, proyears=length(allyrs),
                      CAL_binsmid=StockPars$CAL_binsmid)
      
    }
    newV <- aperm(array(as.numeric(unlist(VList, use.names=FALSE)), dim=c(n_age, length(allyrs), nsim)), c(3,1,2))
    V_P[ , , allyrs] <- newV
    
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
      retL_P[,, yy] <- relLen  # calculate new retention at length curve
    }

    # calculate retention-at-age from retention-at-length
    if (snowfall::sfIsRunning()) {
      VList <- snowfall::sfLapply(1:nsim, calcV, Len_age=StockPars$Len_age[,,allyrs, drop=FALSE],
                                  LatASD=StockPars$LatASD[,,allyrs, drop=FALSE], SLarray=retL_P[,,allyrs, drop=FALSE],
                                  n_age=n_age, nyears=0, proyears=length(allyrs),
                                  CAL_binsmid=StockPars$CAL_binsmid)
    } else {
      VList <- lapply(1:nsim, calcV, Len_age=StockPars$Len_age[,,allyrs, drop=FALSE],
                      LatASD=StockPars$LatASD[,,allyrs, drop=FALSE], SLarray=retL_P[,,allyrs, drop=FALSE],
                      n_age=n_age, nyears=0, proyears=length(allyrs),
                      CAL_binsmid=StockPars$CAL_binsmid)
    }
    
    newretA <- aperm(array(as.numeric(unlist(VList, use.names=FALSE)), dim=c(n_age, length(allyrs), nsim)), c(3,1,2))
    retA_P[ , , allyrs] <- newretA

    
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
    Fdisc_array2 <- array(Fdisc_P, dim=c(nsim, StockPars$nCALbins, length(allyrs)))

    retA_P[,,allyrs] <- retA_P[,,allyrs, drop=FALSE] * V_P[,,allyrs, drop=FALSE] # realized retention curve (prob of retention x prob of selection)
    V_P[,,allyrs] <- retA_P[,,allyrs, drop=FALSE] + ((V_P[,,allyrs, drop=FALSE]-retA_P[,,allyrs, drop=FALSE]) * Fdisc_array1)
    
    retL_P[,,allyrs] <- retL_P[,,allyrs, drop=FALSE] * SLarray_P[,,allyrs, drop=FALSE]
    SLarray_P[,,allyrs]  <- retL_P[,,allyrs, drop=FALSE]  + ((SLarray_P[,,allyrs, drop=FALSE]-retL_P[,,allyrs, drop=FALSE] ) * Fdisc_array2)
  }

  # ---- over-write biomass - with fleet-specific weight-at-age
  Biomass_P <- StockPars$N_P * replicate(StockPars$nareas, FleetPars$Wt_age_C[,,(nyears+1):((nyears)+proyears)])
  
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

    # Calculate total biomass available accounting for any changes in selectivity &/or spatial closures
    # Note vulnerable biomass is not used. With Baranov equation, catch can exceed VB with high F
    Atemp <- apply(CurrentB, c(1,3), sum)
    availB <- apply(Atemp * t(Si), 1, sum) # adjust for spatial closures

    # Calculate total F (using Steve Martell's approach http://api.admb-project.org/baranov_8cpp_source.html)
    expC <- TACusedE
    expC[TACusedE> availB] <- availB[TACusedE> availB] * 0.999

    Ftot <- sapply(1:nsim, calcF, expC, V_P, retA_P, Biomass_P, fishdist,
                   StockPars$Asize,
                   StockPars$maxage,
                   StockPars$nareas,
                   StockPars$M_ageArray,nyears, y,
                   control)
    
    # apply max F constraint
    Ftot[Ftot<0] <- maxF
    Ftot[!is.finite(Ftot)] <- maxF
    Ftot[Ftot>maxF] <- maxF

    # Calculate F & Z by age class
    FM_P[SAYR] <- Ftot[S] * V_P[SAYt] * fishdist[SR]/StockPars$Asize[SR]
    FM_Pret[SAYR] <- Ftot[S] * retA_P[SAYt] * fishdist[SR]/StockPars$Asize[SR]
    Z_P[SAYR] <- FM_P[SAYR] + StockPars$M_ageArray[SAYt] # calculate total mortality

    # Calculate total and retained catch
    CB_P[SAYR] <- FM_P[SAYR]/Z_P[SAYR] * (1-exp(-Z_P[SAYR])) * Biomass_P[SAYR] # removals
    CB_Pret[SAYR] <- FM_Pret[SAYR]/Z_P[SAYR] * (1-exp(-Z_P[SAYR])) * Biomass_P[SAYR] # retained

    # for debugging info:
    # actualremovals <- apply(CB_P[,,y,], 1, sum)
    # retained <- apply(CB_Pret[,,y,], 1, sum)
    # ratio <- actualremovals/retained # ratio of actual removals to retained catch

    # Effort relative to last historical with this catch
    Effort_req <- Ftot/(FleetPars$FinF * FleetPars$qs*FleetPars$qvar[,y]*
                          (1 + FleetPars$qinc/100)^y) * apply(fracE2, 1, sum) # effort required
    Effort_req[Ftot<=tiny] <- tiny
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
  CB_P[!is.finite(CB_P)] <- 0 
  CB_Pret[!is.finite(CB_Pret)] <- 0 

  # Calculate total F (using Steve Martell's approach http://api.admb-project.org/baranov_8cpp_source.html)
  retainedCatch <- apply(CB_Pret[,,y,], 1, sum)

  Ftot <- sapply(1:nsim, calcF, retainedCatch, V_P, retA_P, Biomass_P, fishdist,
                 Asize=StockPars$Asize, maxage=StockPars$maxage, StockPars$nareas,
                 M_ageArray=StockPars$M_ageArray,nyears, y, control) # update if effort has changed  

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


calcF <- function(x, TACusedE, V_P, retA_P, Biomass_P, fishdist, Asize, maxage, nareas,
                  M_ageArray, nyears, y, control) {
  
  maxiterF <- 300
  if(!is.null(control$maxiterF) && is.numeric(control$maxiterF)) maxiterF <- as.integer(control$maxiterF)
  
  tolF <- 1e-4
  if(!is.null(control$tolF) && is.numeric(control$tolF)) tolF <- control$tolF
  
  ct <- TACusedE[x]
  ft <- ct/sum(Biomass_P[x,,y,] * V_P[x,,y+nyears]) # initial guess
  fishdist[x,] <- fishdist[x,]/sum(fishdist[x,])

  if (ft <= 1E-9) return(tiny)
  for (i in 1:maxiterF) {
    Fmat <- ft * matrix(V_P[x,,y+nyears], nrow=maxage+1, ncol=nareas) *
      matrix(fishdist[x,], maxage+1, nareas, byrow=TRUE)/
      matrix(Asize[x,], maxage+1, nareas, byrow=TRUE) # distribute F over age and areas

    Fmat_ret <- ft * matrix(retA_P[x,,y+nyears], nrow=maxage+1, ncol=nareas) *
      matrix(fishdist[x,], maxage+1, nareas, byrow=TRUE)/
      matrix(Asize[x,], maxage+1, nareas, byrow=TRUE) # distribute F over age and areas

    Zmat <- Fmat + matrix(M_ageArray[x,,y+nyears], nrow=maxage+1, ncol=nareas, byrow=FALSE) # total mortality
    if (is.null(control$TAC)) {
      predC <- Fmat_ret/Zmat * (1-exp(-Zmat)) * Biomass_P[x,,y,] # predicted retained catch
    } else {
      if (control$TAC == 'removals') {
        predC <- Fmat/Zmat * (1-exp(-Zmat)) * Biomass_P[x,,y,] # TAC applied to predicted removals
      }
      else {
        stop('invalid entry for `OM@cpars$control$TAC`. Must be `OM@cpars$control$TAC="removals"`')
      }
    }
    predC[!is.finite(predC)] <- 0 
    pct <- sum(predC)
    
    Omat <- (1-exp(-Zmat)) * Biomass_P[x,,y,]
    Zmat[Zmat==0] <- tiny
    # derivative of catch wrt ft
    dct <- sum(Omat/Zmat - ((Fmat * Omat)/Zmat^2) + Fmat/Zmat * exp(-Zmat) * Biomass_P[x,,y,])
    
    if (dct<1E-15) break
    
    ft <-  ft - (pct - ct)/(0.5*dct)
    if (abs(pct - ct)/ct < tolF) break
  }
  ft
}






optDfun <- function(Perrmulti, x, initD, R0, Perr_y, surv,
                    Fec_age, SSB0, n_age) {

  initRecs <- rev(Perr_y[x,1:n_age]) * exp(Perrmulti)

  N <- R0[x] *  initRecs # Calculate initial spawning stock numbers
  SSB <- N * Fec_age[x,,1]    # Calculate spawning stock biomass

  (sum(SSB)/SSB0[x] - initD[x])^2
}

optDfunwrap <- function(x, initD, R0, initdist, Perr_y, surv,
                        Fec_age, SSB0, n_age) {
  interval <- log(c(0.01, 10))

  optD <- optimise(optDfun, interval=interval, x=x, initD=initD,
                   R0=R0, Perr_y=Perr_y, surv=surv,
                   Fec_age, SSB0=SSB0, n_age=n_age)
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


projectEq <- function(x, Asize, nareas, maxage, N, pyears, M_ageArray, Mat_age, Wt_age, Fec_Age,
                      V, retA, Perr, mov, SRrel, Find, Spat_targ, hs, R0a, SSBpR, aR, bR,
                      SSB0, MPA, maxF, Nyrs, plusgroup, Pinitdist) {

  simpop <- popdynCPP(nareas, maxage, Ncurr=N[x,,1,],
                      pyears, M_age=M_ageArray[x,,], Asize_c=Asize[x,],
                      MatAge=Mat_age[x,,],
                      WtAge=Wt_age[x,,], FecAge=Fec_Age[x,,], Vuln=V[x,,], Retc=retA[x,,], Prec=Perr[x,],
                      movc=split.along.dim(mov[x,,,,],4), SRrelc=SRrel[x],
                      Effind=Find[x,],  Spat_targc=Spat_targ[x], hc=hs[x], R0c=R0a[x,],
                      SSBpRc=SSBpR[x,], aRc=aR[x,], bRc=bR[x,], Qc=0, Fapic=0,
                      MPA=MPA,
                      maxF=maxF, control=2, SSB0c=SSB0[x], plusgroup = plusgroup)

  simpop[[1]][,Nyrs,]

}



CalcSPReq <- function(FM, StockPars, n_age, nareas, nyears, proyears, nsim, Hist = FALSE) {
  if(Hist) {
    yind <- 1:nyears
  } else {
    yind <- 1:proyears + nyears
  }
  n_y <- length(yind)
  M <- replicate(nareas, StockPars$M_ageArray[, , yind])
  Wt_age <- replicate(nareas, StockPars$Wt_age[, , yind])
  Mat_age <- replicate(nareas, StockPars$Mat_age[, , yind])
  Fec_age <- replicate(nareas, StockPars$Fec_Age[, , yind])

  # check for M == 0 in MOMs with different maxage 
  ind <- which(M[1,,1,1]==0)
  if (length(ind)>0) {
    ind2 <- which(M[1,,1,1]!=0)
    M <- M[,ind2,,]
    Wt_age <- Wt_age[,ind2,,]
    Mat_age <- Mat_age[,ind2,,]
    Fec_age <-Fec_age[,ind2,,]
    FM <- FM[,ind2,,]
    n_age <- dim(M)[2]
  }
  Z <- FM + M
  
  initdist <- replicate(n_y, StockPars$initdist[, 1, ]) %>% aperm(c(1, 3, 2))
  NPR_M <- NPR_F <- array(NA_real_, c(nsim, n_age, n_y, nareas))
  NPR_M[, 1, , ] <- NPR_F[, 1, , ] <- initdist
  
  for(a in 2:n_age) {
    NPR_M[, a, , ] <- NPR_M[, a-1, , ] * exp(-M[, a-1, , ])
    NPR_F[, a, , ] <- NPR_F[, a-1, , ] * exp(-Z[, a-1, , ])
  }
  if(StockPars$plusgroup) {
    NPR_M[, n_age, , ] <- NPR_M[, n_age, , ]/(1 - exp(-M[, n_age, , ]))
    NPR_F[, n_age, , ] <- NPR_F[, n_age, , ]/(1 - exp(-Z[, n_age, , ]))
  }
  SPReq <- apply(NPR_F * Fec_age, c(1, 3), sum)/apply(NPR_M * Fec_age, c(1, 3), sum)
  return(SPReq)
}

CalcSPRdyn <- function(FM, StockPars, n_age, nareas, nyears, proyears, nsim, Hist = FALSE) {
  if(Hist) {
    yind <- 1:nyears
  } else {
    yind <- 1:(proyears + nyears)
  }
  n_y <- length(yind)
  M <- replicate(nareas, StockPars$M_ageArray[, , yind])
  Wt_age <- replicate(nareas, StockPars$Wt_age[, , yind])
  Mat_age <- replicate(nareas, StockPars$Mat_age[, , yind])
  Fec_age <- replicate(nareas, StockPars$Fec_Age[, , yind])
  
  # check for M == 0 in MOMs with different maxage 
  ind <- which(M[1,,1,1]==0)
  if (length(ind)>0) {
    ind2 <- which(M[1,,1,1]!=0)
    M <- M[,ind2,,]
    Wt_age <- Wt_age[,ind2,,]
    Mat_age <- Mat_age[,ind2,,]
    Fec_age <-Fec_age[,ind2,,]
    FM <- FM[,ind2,,]
    n_age <- dim(M)[2]
  }
  Z <- FM + M
  
  initdist <- replicate(n_y, StockPars$initdist[, 1, ]) %>% aperm(c(1, 3, 2))
  cumsurv_F <- cumsurv_M <- array(NA_real_, c(nsim, n_age, n_y, nareas))
  cumsurv_F[, 1, , ] <- cumsurv_M[, 1, , ] <- initdist
  
  #### Dynamic SPR
  # Boundary condition (first historical year) - assume unfished in cumsurv_F in first year
  for(a in 2:n_age) {
    Msum <- replicate(nareas, apply(StockPars$M_ageArray[, 2:a - 1, 1, drop = FALSE], 1, sum))
    cumsurv_M[, a, 1, ] <- StockPars$initdist[, 1, ] * exp(-Msum)
  }
  if(StockPars$plusgroup) {
    cumsurv_M[, n_age, 1, ] <- cumsurv_M[, n_age, 1, ]/(1 - exp(-StockPars$M_ageArray[, n_age, 1]))
  }
  cumsurv_F[, , 1, ] <- cumsurv_M[, , 1, ]
  
  for(y in 2:n_y) {
    for(a in 2:n_age) {
      cumsurv_M[, a, y, ] <- cumsurv_M[, a-1, y-1, ] * exp(-M[, a-1, y-1, ])
      cumsurv_F[, a, y, ] <- cumsurv_F[, a-1, y-1, ] * exp(-Z[, a-1, y-1, ])
      if(a == n_age && StockPars$plusgroup) {
        cumsurv_M[, a, y, ] <- cumsurv_M[, a, y, ] + cumsurv_M[, a, y-1, ] * exp(-M[, a, y-1, ])
        cumsurv_F[, a, y, ] <- cumsurv_F[, a, y, ] + cumsurv_F[, a, y-1, ] * exp(-Z[, a, y-1, ])
      }
    }
    stockmovM <- lapply(1:nsim, function(x) movestockCPP(nareas, n_age-1, StockPars$mov[x, , , , y], cumsurv_M[x, , y, ]))
    stockmovF <- lapply(1:nsim, function(x) movestockCPP(nareas, n_age-1, StockPars$mov[x, , , , y], cumsurv_F[x, , y, ]))
    
    cumsurv_M[, , y, ] <- simplify2array(stockmovM) %>% aperm(c(3, 1, 2))
    cumsurv_F[, , y, ] <- simplify2array(stockmovF) %>% aperm(c(3, 1, 2))
  }
  SPRdyn <- apply(cumsurv_F * Fec_age, c(1, 3), sum)/apply(cumsurv_M * Fec_age, c(1, 3), sum)
  if(!Hist) SPRdyn <- SPRdyn[, 1:proyears + nyears]
  return(SPRdyn)
}
