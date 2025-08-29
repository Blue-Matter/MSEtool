
#' Optimize for catchability (q) and fishing dist for a MICE model
#'
#' Function optimizes catchability (q, where F=qE) required to get to user-specified stock
#' depletion across stocks and fleets if there are relationships among stocks
#'
#' @param x Integer, the simulation number
#' @param StockPars A list of sampled stock parameters, one list element per stock
#' @param FleetPars A hierarchical list of sampled fleet parameters,
#' first list level is stock, second is fleet
#' @param np The number of stocks
#' @param nf The number of fleets
#' @param nareas The number of areas
#' @param maxage The maximum number of modeled ages
#' @param nyears The number of historical 'spool-up' years (from unfished to now)
#' @param N An array of stock numbers `[nsim,np,maxage,nyears,nareas]` -
#' only the values from the first year are used
#' @param VF An array of vulnerability `[nsim,np,nf,maxage,nyears+proyears]`
#' @param FretA An array of retention `[nsim,np,nf,maxage,nyears+proyears]`
#' @param maxF A numeric value specifying the maximum fishing mortality for any
#' single age class
#' @param MPA An array of spatial closures by year `[np,nf,nyears+proyears,nareas]`
#' @param CatchFrac A list of stock-specific fleet fractions of current catch
#' list`[[stock]][nsim, nf]`
#' @param bounds Bounds for total q estimation
#' @param tol A numeric value that is the fraction of machine tolerance
#' (once reduction in objective function steps below this, optimization ends)
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific dynamics SSBfrom stock_age
#' @param optVB Logical, whether to optimize to vulnerable biomass (or spawning biomass otherwise)
#' @param silent Logical, whether to report the objective function for each simulation
#' @author T.Carruthers
#' @keywords internal
getq_multi_MICE <- function(x, StockPars, FleetPars, np, nf, nareas, maxage,
                            nyears, N, VF, FretA, maxF = 0.9, MPA, CatchFrac,
                            bounds= c(1e-05, 15), tol = 1E-6, Rel, SexPars, plusgroup,
                            optVB = FALSE, silent = FALSE) {

  # Ensure this code matches HistMICE
  n_age <- maxage + 1 # include age-0
  Nx <- array(N[x,,,,], c(np, n_age, nyears, nareas))
  VFx <- array(VF[x,,,,], c(np, nf, n_age, nyears))
  FretAx <- array(FretA[x,,,,] , c(np, nf, n_age, nyears))
  
  # Vectors of length np
  Kx <- sapply(StockPars, getElement, "K")[x, ]
  Linfx <- sapply(StockPars, getElement, "Linf")[x, ]
  t0x <- sapply(StockPars, getElement, "t0")[x, ]
  Mx <- sapply(StockPars, getElement, "M")[x, ]
  R0x <- sapply(StockPars, getElement, "R0")[x, ]
  SSB0x <- sapply(StockPars, getElement, "SSB0")[x, ]
  B0x <- sapply(StockPars, getElement, "B0")[x, ]
  
  hsx <- sapply(StockPars, getElement, "hs")[x, ]
  ax <- sapply(StockPars, getElement, "a")
  bx <- sapply(StockPars, getElement, "b")
  SRrelx <- sapply(StockPars, getElement, "SRrel")[x, ]
  
  # Matrix np x nyears + nage
  Perrx <- sapply(1:np, function(p) StockPars[[p]]$Perr_y[x, 1:(nyears + n_age)]) %>% t()
  
  # Matrix np x nage x areas x areas x nyears+1
  movx <- sapply(1:np, function(p) StockPars[[p]]$mov[x, , , , 0:nyears + 1], simplify = "array") %>% aperm(c(5, 1:4))
  
  # Matrix np x nareas
  distx <- sapply(1:np, function(p) StockPars[[p]]$R0a[x, ]/sum(StockPars[[p]]$R0a[x, ])) %>% t()
  SSBpRx <- sapply(1:np, function(p) StockPars[[p]]$SSBpR[x, ]) %>% t()
  R0ax <- sapply(1:np, function(p) StockPars[[p]]$R0a[x, ]) %>% t()
  aRx <- sapply(1:np, function(p) StockPars[[p]]$aR[x, ]) %>% t()
  bRx <- sapply(1:np, function(p) StockPars[[p]]$bR[x, ]) %>% t()
  Asizex <- sapply(1:np, function(p) StockPars[[p]]$Asize[x, ]) %>% t()
  
  # Arrays np x nage x nyears + 1
  Mat_agex <- sapply(1:np, function(p) StockPars[[p]]$Mat_age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  Fec_agex <- sapply(1:np, function(p) StockPars[[p]]$Fec_Age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  M_ageArrayx <- sapply(1:np, function(p) StockPars[[p]]$M_ageArray[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  WatAgex <- sapply(1:np, function(p) StockPars[[p]]$Wt_age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  Len_agex <- sapply(1:np, function(p) StockPars[[p]]$Len_age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  
  # Array np x nf x nyears
  Effind <- sapply(1:np, function(p) {
    sapply(1:nf, function(f) FleetPars[[p]][[f]][["Find"]][x, ]) %>% matrix(ncol = nf)
  }, simplify = "array") %>% aperm(3:1)
  
  # Matrix np x nf
  Spat_targ <- sapply(1:np, function(p) {
    sapply(1:nf, function(f) FleetPars[[p]][[f]][["Spat_targ"]][x])
  }) %>% matrix(nf, np) %>% t()
  
  # Matrix np x nyears + 1 
  Karrayx <- getLHpars(x, 'Karray', StockPars, nyears + 1)
  Linfarrayx <- getLHpars(x, 'Linfarray', StockPars, nyears + 1) 
  t0arrayx <- getLHpars(x, 't0array', StockPars, nyears + 1) 
  Marrayx <- getLHpars(x, 'Marray', StockPars, nyears + 1) 
  
  # Variables needed for q optim
  CF <- sapply(CatchFrac, function(xx) xx[x, ]) %>% matrix(nrow = nf) %>% t() # np x nf
  Fdist <- CF/Effind[,,nyears] # Catch divided by effort (q proxy)
  Fdist[!is.finite(Fdist)] <- tiny
  Fdist <- Fdist/apply(Fdist[, , drop = FALSE], 1, sum)    # q ratio proxy (real space)
  
  VB0x <- sapply(StockPars, getElement, "VB0")[x, ] # vector length np
  
  WtCx <- sapply(1:np, function(p) { # array np x nf x n_age
    sapply(1:nf, function(f) FleetPars[[p]][[f]]$Wt_age_C[x, , nyears])
  }, simplify = "array") %>% aperm(3:1)
  
  if (nf == 1) {
    par <- rep(-5, np)
  } else {
    # low initial F followed by logit guess at fraction based on Fdist
    # according to catch fraction in recent year
    par <- c(rep(-5,np), logit(Fdist[, 2:nf]))
  }
  
  depc <- sapply(1:np, function(p) StockPars[[p]][["D"]])[x, ]
  CFc <- sapply(1:np, function(p) CatchFrac[[p]][x, ]) %>% matrix(nf, np) %>% t()
  
  spawn_time_frac <- rep(0, np)
  for (p in 1:np) {
    spawn_time_frac[p] <- StockPars[[p]]$spawn_time_frac[x]
  }
  
  SRRfun_p <- lapply(1:np, function(p) StockPars[[p]]$SRRfun)
  SRRpars_p <- lapply(1:np, function(p) StockPars[[p]]$SRRpars[[x]])
  
  if (!silent) message_info("\n\nSimulation", x, "depletion objective function (should be zero):\n")
  opt <- optim(par, qestMICE,
               method = "L-BFGS-B",
               lower = c(rep(log(bounds[1]), np), rep(-5, np * (nf-1))),
               upper = c(rep(log(bounds[2]), np), rep(5, np*(nf-1))),
               depc = depc, CFc = CFc, mode = "opt", np = np, nf = nf, nyears = nyears,
               nareas = nareas, maxage = maxage, Nx = Nx, VFx = VFx, FretAx = FretAx,
               Effind = Effind, distx = distx, movx = movx, Spat_targ = Spat_targ,
               M_ageArrayx = M_ageArrayx, Mat_agex = Mat_agex, 
               Fec_agex = Fec_agex,
               Asizex = Asizex,
               WatAgex = WatAgex, Len_agex = Len_agex,
               Karrayx = Karrayx, Linfarrayx = Linfarrayx, t0arrayx = t0arrayx, Marrayx = Marrayx,
               R0x = R0x, R0ax = R0ax, SSBpRx = SSBpRx,
               SSB0x = SSB0x, hsx = hsx, ax = ax, bx = bx, aRx = aRx, bRx = bRx, Perrx = Perrx,
               SRrelx = SRrelx, Rel = Rel, SexPars = SexPars, x = x, plusgroup = plusgroup,
               optVB = optVB, VB0x = VB0x, WtCx = WtCx, maxF = maxF,
               MPA=MPA,
               SRRfun=SRRfun_p, SRRpars=SRRpars_p,
               control = list(trace = ifelse(silent, 0, 1), factr = tol/.Machine$double.eps),
               spawn_time_frac=spawn_time_frac)
  if (!silent) cat("\n")

  out <- qestMICE(par = opt$par, depc = depc,CFc = CFc, mode = 'calc', np = np, nf = nf,
                  nyears = nyears, nareas = nareas, maxage = maxage, Nx = Nx, VFx = VFx,
                  FretAx = FretAx, Effind = Effind, distx = distx, movx = movx,
                  Spat_targ = Spat_targ, M_ageArrayx = M_ageArrayx, Mat_agex = Mat_agex,
                  Fec_agex = Fec_agex,
                  Asizex = Asizex, 
                  WatAgex = WatAgex, Len_agex = Len_agex,
                  Karrayx = Karrayx, Linfarrayx = Linfarrayx, t0arrayx = t0arrayx, Marrayx = Marrayx,
                  R0x = R0x,
                  R0ax = R0ax, SSBpRx = SSBpRx, SSB0x = SSB0x, hsx = hsx, aRx = aRx, bRx = bRx,
                  ax = ax, bx = bx, Perrx = Perrx, SRrelx = SRrelx,
                  Rel = Rel,SexPars = SexPars, x = x, plusgroup = plusgroup,
                  optVB = optVB, VB0x = VB0x, B0x = B0x, WtCx = WtCx, maxF = maxF,
                  MPA=MPA,
                  SRRfun=SRRfun_p, SRRpars=SRRpars_p,
                  spawn_time_frac=spawn_time_frac)

  out
}


#' Internal function for optimizing catchability (q) for a MICE model
#'
#' Function returns objective function that fits both stock depletion and catch fraction among fleets
#'
#' @param par Integer, the simulation number
#' @param depc Numeric vector, nstock long of specified stock depletion (SSB now / SSB0)
#' @param CFc Matrix `[nstock, nfleet]`, a catch fraction among fleets (sums to 1 for each stock (row))
#' @param mode Character if 'opt' qestMICE returns the objective function otherwise the fitted values in a list
#' @param nf Integer, number of stocks
#' @param nf Integer, number of fleets
#' @param nyears Integer, number of historical years (unfished til today)
#' @param nareas Integer, number of areas (default is 2)
#' @param maxage Integer, maximum number of age classes for calculation
#' @param Nx Array `[stock, age, year, area]` of stock numbers
#' @param VFx Array `[fleet, age, year, area]` of the vulnerability curve
#' @param FretAx Array `[fleet, age, year, area]` of the retention curve
#' @param Effind Array `[fleet, year]` of effort
#' @param movx Array `[stock,age,area,area]` of movement transitions
#' @param Spat_targ Matrix `[stock, fleet]` of spatial targeting parameter (0 evenly spatial distributed, 1 proportional to vulnerable biomass)
#' @param M_ageArrayx Array `[stock, age,year]` of Natural mortality rate at age
#' @param Mat_agex Array `[stock, age, year]` of maturity (spawning fraction) at age
#' @param Fec_agex Array `[stock, age, year]` of mature spawning weight at age
#' @param Asizex Matrix `[stock, area]` Area size
#' @param Karrayx Array of von B growth parameter K
#' @param Linfarrayx Array of von B asymptotic length parameter Linf
#' @param t0arrayx Array ofvon B theoretical age at zero length (t0)
#' @param Marrayx Array of mature natural mortality rate 
#' @param R0x Vector `[stock]` unfished recruitment
#' @param R0ax Matrix `[stock, area]` unfished recruitment by area
#' @param SSBpRx Matrix `[stock, area]` spawning biomass per recruit by area
#' @param SSB0x Vector `[stock]` Unfished spawning stock biomass
#' @param hsx Vector `[stock]` steepness of the stock recruitment curve
#' @param aRx Vector `[stock]` stock recruitment parameter alpha (for Ricker curve)
#' @param bRx Vector `[stock]` stock recruitment parameter beta (for Ricker curve)
#' @param ax Vector `[stock]` weight-length parameter a W=aL^b
#' @param bx Vector `[stock]` weight-length parameter b W=aL^b
#' @param Perrx Matrix `[stock, year]` process error - the lognormal factor for recruitment strength
#' @param SRrelx Integer vector `[stock]` the form of the stock recruitment relationship (1 = Beverton-Holt, 2= Ricker)
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific dynamics (SSBfrom, stcck_age)
#' @param x Integer. The simulation number
#' @param plusgroup Integer vector `[stock]` indicating if a plus group is used
#' @param optVB Logical, whether to optimize to vulnerable biomass (or spawning biomass otherwise)
#' @param VB0x Vector `[stock]` unfished vulnerable biomass
#' @param B0x Vector `[stock]` unfished total biomass
#' @param WtCx Array `[stock, fleet, n_age]` of weight at age in catch in the last historical year
#' @param maxF A numeric value specifying the maximum fishing mortality for any
#' single age class
#' @param MPA An array of spatial closures by year `[np,nf,nyears+proyears,nareas]`
#' @param SRRfun  Optional. Stock-recruit functions used if `SRrelc =3`. List of length `np` 
#' @param SRRpars Optional. A named list of arguments for `SRRfun`. List of length `np`
#' @param spawn_time_frac Numeric. Fraction of the year when spawning occurs. Default = 0.
#' @author T.Carruthers
#' @keywords internal
qestMICE <- function(par, depc, CFc, mode='opt', np, nf, nyears, nareas, maxage, Nx, VFx,
                     FretAx, Effind, distx, movx, Spat_targ, M_ageArrayx, Mat_agex,
                     Fec_agex,
                     Asizex,
                     WatAgex, Len_agex,
                     Karrayx, Linfarrayx, t0arrayx, Marrayx,
                     R0x, R0ax, SSBpRx, SSB0x, hsx, aRx, bRx,
                     ax, bx, Perrx, SRrelx, Rel, SexPars, x, plusgroup, optVB, VB0x, B0x, WtCx,
                     maxF, MPA,
                     SRRfun, SRRpars,
                     spawn_time_frac=rep(0, np)) {

  n_age <- maxage + 1 # include age-0
  qsx <- exp(par[1:np])
  if (nf == 1) {
    qfracx <- matrix(1, nrow = np, 1)
  } else {
    qlogit <- matrix(0, np, nf)
    qlogit[, 2:nf] <- par[(np+1):length(par)]
    qfracx <- ilogitm(qlogit)
  }

  HistVars <- popdynMICE(qsx, qfracx, np, nf, nyears, nareas, maxage, Nx, VFx, FretAx,
                         Effind, movx, Spat_targ, M_ageArrayx, Mat_agex, Fec_agex, 
                         Asizex,
                         WatAgex, Len_agex,
                         Karrayx, Linfarrayx, t0arrayx, Marrayx,
                         R0x, R0ax, SSBpRx, hsx, aRx, bRx, ax, bx, Perrx,
                         SRrelx, Rel, SexPars, x, plusgroup, maxF, SSB0x, B0x,
                         MPA,
                         SRRfun=SRRfun, SRRpars=SRRpars,
                         spawn_time_frac=spawn_time_frac)

  if (optVB) {
    VBest <- apply(HistVars$VBx, c(1, 3), sum)
    deppred <- VBest[, nyears]/VB0x
  } else {
    SSBest <- apply(HistVars$SSBx, c(1, 3), sum)
    deppred <- SSBest[, nyears]/SSB0x
  }

  if (length(SexPars)) { # you need to make depletion just one variable for all components of a sex-specific model
    sexmatches <- sapply(1:nrow(SexPars$SSBfrom), function(x) paste(SexPars$SSBfrom[x, ], collapse = "_"))
    parcopy <- match(sexmatches, sexmatches)
    deppred <- deppred[parcopy]
    qsx <- qsx[parcopy] # copy female q to males
  }

  Cfracpred <- local({
    Cpred <- CBpred <- array(NA, c(np, nf, n_age, nareas))
    Cind <- TEG(dim(Cpred))
    Find <- cbind(Cind[, 1:3], nyears, Cind[, 4]) # p f age y area
    Nind <- Find[, c(1, 3:5)]
    
    Cpred[Cind] <- HistVars$Nx[Nind] * (1 - exp(-HistVars$Zx[Nind])) * HistVars$FMy[Find] / HistVars$Zx[Nind]
    CBpred[Cind] <- Cpred[Cind] * WtCx[Cind[, 1:3]]
    Ctot <- apply(CBpred, 1:2, sum)
    Ctot/apply(Ctot, 1, sum)
  })

  depOBJ <- sum(log(depc/deppred)^2)
  CFc[CFc == 0] <- tiny
  Cfracpred[Cfracpred == 0] <- tiny
  cOBJ <- sum(log(CFc/Cfracpred)^2) # Lazy - should be: sum(log(CFc[,2:nf]/Cpred[,2:nf])^2) but this doesn't work for single fleets and it makes no difference anyway

  if(mode=='opt'){
    return(depOBJ+cOBJ)
  }else{
    return(list(qtot=qsx,
                qfrac=qfracx,
                CFc=CFc,
                Cfracpred=Cfracpred,
                depc=depc,
                deppred=deppred)) #,Vulnf=Vulnf,Retf=Retf,MPAf=MPAf))
  }

}



