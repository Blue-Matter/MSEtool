#' Population dynamics for a MICE model (multiyear)
#'
#' Calls popdynOneMICE iteratively to reconstruct a time series given MICE model inputs
#'
#' @param qsx Total catchability
#' @param qfracx Vector `[fleet]`, the fraction of total qs by fleet
#' @param np Integer, the number of stocks
#' @param nf Integer, number of fleets
#' @param nyears Integer, number of historical years (unfished til today)
#' @param nareas Integer, the number of spatial areas
#' @param maxage Integer, maximum modeled age
#' @param Nx Array `[stock, age, year, area]` of stock numbers
#' @param VFx Array `[fleet, age, year, area]` of the vulnerability curve
#' @param FretAx Array `[fleet, age, year, area]` of the retention curve
#' @param Effind Array `[fleet, year]` of effort
#' @param movx Array `[stock,age,area,area]` of movement transitions
#' @param Spat_targ Matrix `[stock, fleet]` of spatial targeting parameter
#' (0 evenly spatial distributed, 1 proportional to vulnerable biomass)
#' @param M_ageArrayx Array `[stock, age,year]` of Natural mortality rate at age
#' @param Mat_agex Array `[stock, age, year]` of maturity (spawning fraction) at age
#' @param Fec_agex Array `[stock, age, year]` of female spawning weight (fecundity) at age
#' @param Asizex  Array `[stock, area]` Area size
#' @param WatAgex Array of weight-at-age
#' @param Len_agex Array of length-at-age
#' @param Karrayx Array of von B growth parameter K
#' @param Linfarrayx Array of von B asymptotic length parameter Linf
#' @param t0arrayx Array ofvon B theoretical age at zero length (t0)
#' @param Marrayx Array of mature natural mortality rate 
#' @param R0x Vector `[stock]` unfished recruitment
#' @param R0ax Matrix `[stock, area]` unfished recruitment by area
#' @param SSBpRx Matrix `[stock, area]` spawning biomass per recruit by area
#' @param hsx Vector `[stock]` steepness of the stock recruitment curve
#' @param aRx Vector `[stock]` stock recruitment parameter alpha (for Ricker curve)
#' @param bRx Vector `[stock]` stock recruitment parameter beta (for Ricker curve)
#' @param ax Vector `[stock]` weight-length parameter a W=aL^b
#' @param bx Vector `[stock]` weight-length parameter b W=aL^b
#' @param Perrx Matrix `[stock, year]` process error - the lognormal factor for recruitment strength
#' @param SRrelx Integer vector `[stock]` the form of the stock recruitment
#' relationship (1 = Beverton-Holt, 2= Ricker)
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific relationships (SSBfrom, stock_age)
#' @param x Integer the simulation number
#' @param plusgroup Numeric vector. Use plus-group (1) or not (0)
#' @param maxF maximum F
#' @param SSB0x SSB0 for this simulation
#' @param B0x B0 for this simulation
#' @param MPA An array of spatial closures by year `[np,nf,nyears+proyears,nareas]`
#' @param SRRfun A list of custom stock-recruit functions. List of length `np`
#' @param SRRpars A list of custom stock-recruit parameters for simulation `x`. List of length `np`
#' @param spawn_time_frac vector of relative spawn timing. Length nstock
#' @author T.Carruthers
#' @keywords internal
popdynMICE <- function(qsx, qfracx, np, nf, nyears, nareas, maxage, Nx, VFx, FretAx, Effind,
                       movx, Spat_targ, M_ageArrayx, Mat_agex, Fec_agex, 
                       Asizex, WatAgex, Len_agex,
                       Karrayx, Linfarrayx, t0arrayx, Marrayx,
                       R0x, R0ax, SSBpRx, hsx, aRx,
                       bRx, ax, bx, Perrx, SRrelx, Rel, SexPars, x, plusgroup, maxF, SSB0x, B0x,
                       MPA,
                       SRRfun, SRRpars,
                       spawn_time_frac=rep(0, np)) {

  n_age <- maxage + 1 # include age-0
  Bx <- SSNx <- SSBx <- VBx <- Zx <- array(NA_real_, c(np, n_age, nyears, nareas))
  
  Fy <- array(NA_real_, c(np, nf, nyears))
  Fty <- array(NA_real_, c(np, n_age, nyears, nareas))
  FMy <- FMrety <- VBfx <- array(NA_real_, c(np, nf, n_age, nyears, nareas))
  
  hsy <- ay <- by <- array(NA_real_, c(np, nyears+1))
  hsy[] <- hsx
  ay[] <- ax
  by[] <- bx
  
  # Array and array indices for first year
  VBfind <- TEG(c(np, nf, n_age, 1, nareas))
  Nind <- TEG(c(np, n_age, 1, nareas))
  
  FMx <- FMretx <- array(NA_real_, c(np, nf, n_age, nareas))
  Fdist <- FMx
  
  Find <- TEG(dim(FMretx))
  
  VBcur <- array(NA, dim(VBfx)[-4]) # np x nfleet x nage x nareas
  Ecur <- array(NA, c(np, nf))
  Vcur <- Retcur <- array(NA_real_, c(np, nf, n_age))

  # Year 1 calcs
  Bx[Nind] <- Nx[Nind] * WatAgex[Nind[, 1:3]]
  
  for(y in 1:nyears + 1) { # Start loop at y = 2
    MPAthisyr <- MPA[,,y-1,, drop=FALSE]
    MPAthisyr <- abind::adrop(MPAthisyr, 3) # drop year dimension
    Nind[, 3] <- VBfind[, 4] <- y-1
    
    #    p f a r                  p a y r                 p f a y
    VBfx[VBfind] <- Bx[VBfind[, c(1,3,4,5)]] * VFx[VBfind[, 1:4]] * qfracx[VBfind[, c(1,2)]]
    
    VBcur[] <- VBfx[, , , y-1, ] # array(VBfx[,,,y-1,], c(np, nf, n_age, nareas))
    
    VBcur_a <- apply(VBcur, c(1,2,4), sum)
   
    Ecur[] <- Effind[, , y-1] #matrix(Effind[, , y-1], np, nf)
    Vcur[] <- VFx[, , , y-1] #array(VFx[, , , y-1], c(np, nf, n_age))
    
    # Fdist[Find] <- VBcur[Find]*MPAthisyr[Find[,c(1,2,4)]]^Spat_targ[Find[, 1:2]]
    
    # optimize Spat_targ if neccesary - make sure overall F is correct
    # for (p in 1:np) {
    #   # check if biomass equally distributed across areas
    #   chk <- apply(VBcur_a[p,,, drop=FALSE], 3, mean)
    #   if (chk[1] == mean(chk)) next()
    #   
    #   # optimize spat_targ 
    #   par <- rep(0, nf) 
    #   f_zero_fleet <- which(qfracx[p,]==0)
    #   if (length(f_zero_fleet)>0)
    #     par <- par[-f_zero_fleet] 
    #   
    #   N_area <- Nx[p,,y-1,]
    #   M_age <- M_ageArrayx[p,,y-1]
    #   
    #   
    #   if (length(par)==1) {
    #     opt_spat_targ <- optimize(solve_spat_targ, 
    #                            c(-3,3),
    #                            pop=p, qsx=qsx, 
    #                            qfracx=qfracx,
    #                            Ecur=Ecur, n_age=n_age, nareas=nareas, 
    #                            Fdist=Fdist, 
    #                            Vcur=Vcur, 
    #                            N_area=N_area, M_age=M_age,
    #                            VBcur_a=VBcur_a,
    #                            f_zero_fleet=f_zero_fleet,
    #                            Asizex=Asizex,
    #                            MPAthisyr=MPAthisyr)
    #     
    #     par <- opt_spat_targ$minimum
    #     if (length(f_zero_fleet)>0) {
    #       dummy <- rep(0, length(f_zero_fleet))
    #       
    #       temp <- rep(NA, length(c(par, f_zero_fleet)))
    #       temp[f_zero_fleet] <- 0
    #       temp[is.na(temp)] <- par
    #       Spat_targ[p] <- temp
    #     } else {
    #       Spat_targ[p] <- opt_spat_targ$minimum
    #     }
    #   } else {
    #     opt_spat_targ <- optim(par, 
    #                            fn=solve_spat_targ,
    #                            pop=p, qsx=qsx,  qfracx=qfracx,
    #                            Ecur=Ecur, n_age=n_age, nareas=nareas, 
    #                            Fdist=Fdist, 
    #                            Vcur=Vcur, 
    #                            N_area=N_area, M_age=M_age,
    #                            VBcur_a=VBcur_a,
    #                            f_zero_fleet=f_zero_fleet,
    #                            Asizex=Asizex,
    #                            MPAthisyr=MPAthisyr)
    #     
    #     par <- opt_spat_targ$par
    #     if (length(f_zero_fleet)>0) {
    #       dummy <- rep(0, length(f_zero_fleet))
    #       
    #       temp <- rep(NA, length(c(par, f_zero_fleet)))
    #       temp[f_zero_fleet] <- 0
    #       temp[is.na(temp)] <- par
    #       Spat_targ[p,] <- temp
    #     } else {
    #       Spat_targ[p,] <- opt_spat_targ$par
    #     }
    #   }
    # }

    Fdist[Find] <- (VBcur_a[Find[,c(1,2,4)]]*MPAthisyr[Find[,c(1,2,4)]])^Spat_targ[Find[, 1:2]]
    Fdist[Find] <- Fdist[Find]/apply(Fdist,1:3,sum)[Find[,1:3]]
    Fdist[is.na(Fdist)] <- 0 # This is an NA catch for hermaphroditism
    
    FMx[Find] <- qsx[Find[, 1]] * qfracx[Find[, 1:2]] * Ecur[Find[, 1:2]] * Fdist[Find] *
      Vcur[Find[, 1:3]]  / Asizex[Find[, c(1, 4)]]
    FMx[FMx > maxF] <- maxF # apply maxF

    Retcur[] <- FretAx[, , , y-1] #array(FretAx[, , , 1], c(np, nf, n_age))
    FMretx[Find] <- qsx[Find[,1]] * qfracx[Find[,1:2]] * Ecur[Find[,1:2]] * Fdist[Find] *
      Retcur[Find[, 1:3]]  / Asizex[Find[, c(1, 4)]]
    FMretx[FMretx > maxF] <- maxF # apply maxF

    # Update spawning biomass and recruitment for last year (calculated mid year if spawn_time_frac>0)
    ZMx_all <- array(NA, dim=c(np, n_age, nareas))
    Foverall <- apply(FMx, c(1,3,4), sum)
    M_agecur <- array(M_ageArrayx[, , y-1], c(np, n_age))
    M_agecur <- replicate(nareas, M_agecur)
    ZMx_all <- Foverall + M_agecur
    
    SSBx[Nind] <- Nx[Nind] * exp(-ZMx_all[Nind[,c(1,2,4)]] * spawn_time_frac[Nind[,1]]) * Fec_agex[Nind[, 1:3]]
    SSNx[Nind] <- Nx[Nind] * exp(-ZMx_all[Nind[,c(1,2,4)]] * spawn_time_frac[Nind[,1]]) * Mat_agex[Nind[, 1:3]]
 
    # Recruitment for last year
    if (y>2) {
      # Calculate SSB for S-R relationship
      SSB_SR <- local({
        SSBtemp <- array(NA_real_, dim(SSBx[,,y-1,])) # np x n_age x nareas
        SSBtemp[] <- SSBx[,,y-1,, drop=FALSE]
        if (length(SexPars$SSBfrom)) { # use SSB from another stock to predict recruitment
          sapply(1:np, function(p) apply(SexPars$SSBfrom[p, ] * SSBtemp, 2:3, sum), simplify = "array") %>%
            aperm(c(3, 1, 2))
        } else {
          SSBtemp
        }
      })
      if (length(dim(SSB_SR))==2) {
        Nx[, 1, y-1, ] <- sapply(1:np, function(p) {
          calcRecruitment_int(SRrel = SRrelx[p], SSBcurr = SSB_SR, recdev = Perrx[p, y-1+n_age-1], hs = hsx[p], 
                              aR = aRx[p, 1], bR = 1/sum(1/bRx[p, ]), R0a = R0ax[p, ], SSBpR = SSBpRx[p, 1],
                              SRRfun[[p]], SRRpars[[p]])
        }) %>% t()
      } else {
        Nx[, 1, y-1, ] <- sapply(1:np, function(p) {
          calcRecruitment_int(SRrel = SRrelx[p], SSBcurr = SSB_SR[p, , ], recdev = Perrx[p, y-1+n_age-1], hs = hsx[p], 
                              aR = aRx[p, 1], bR = 1/sum(1/bRx[p, ]), R0a = R0ax[p, ], SSBpR = SSBpRx[p, 1],
                              SRRfun[[p]], SRRpars[[p]])
        }) %>% t()
      }
   
      # update biomass with recruitment
      Bx[, , y-1, ] <- Nx[, , y-1, ] * replicate(nareas,WatAgex[,,y-1])
    }
    
    SexPars_y <- SexPars
    SexPars_y$Herm <- subsetHerm(SexPars_y$Herm, y = y)
    
    out <- popdynOneMICE(np, nf, nareas, maxage,
                         Ncur = array(Nx[, , y-1, ], dim(Nx)[c(1:2, 4)]),
                         Bcur = array(Bx[, , y-1, ], dim(Nx)[c(1:2, 4)]),
                         SSBcur = array(SSBx[, , y-1, ], dim(Nx)[c(1:2, 4)]),
                         Vcur = Vcur,
                         FMretx = FMretx,
                         FMx = FMx,
                         PerrYrp = Perrx[, y+n_age-1],
                         hsx = hsy[, y], aRx = aRx, bRx = bRx,
                         movy = array(movx[, , , , y], c(np, n_age, nareas, nareas)),
                         Spat_targ = Spat_targ, SRrelx = SRrelx,
                         M_agecur = array(M_ageArrayx[, , y-1], c(np, n_age)),
                         Mat_agenext = array(Mat_agex[, , y], c(np, n_age)),
                         Fec_agenext = array(Fec_agex[, , y], c(np, n_age)),
                         Asizex = Asizex,
                         Kx = Karrayx[, y], 
                         Linfx = Linfarrayx[, y], 
                         t0x = t0arrayx[, y],
                         Mx = Marrayx[, y-1],
                         R0x = R0x, R0ax = R0ax, SSBpRx = SSBpRx, ax = ay[, y],
                         bx = by[, y], Rel = Rel, SexPars = SexPars_y, x = x,
                         plusgroup = plusgroup, SSB0x = SSB0x, B0x = B0x,
                         Len_agenext = array(Len_agex[, , y], c(np, n_age)),
                         Wt_agenext = array(WatAgex[, , y], c(np, n_age)),
                         SRRfun=SRRfun, SRRpars=SRRpars, ycur = y-1)
    
    # update arrays 
    if (y <= nyears) {
      if (length(Rel) && y <= nyears) { # Update Len_age, Wt_age, Fec_age when MICE relationship exists
        gc <- FALSE # growth changed?
        if (any(Linfarrayx[, y] != out$Linfx)) Linfarrayx[, y] <- out$Linfx; gc <- TRUE
        if (any(Karrayx[, y] != out$Kx)) Karrayx[, y] <- out$Kx; gc <- TRUE
        if (any(t0arrayx[, y] != out$t0x)) t0arrayx[, y] <- out$t0x; gc <- TRUE
        if (any(ay[, y] != out$ax)) ay[, y] <- out$ax; gc <- TRUE
        if (any(by[, y] != out$bx)) by[, y] <- out$bx; gc <- TRUE
        #hsy[,y]<-out$hsx; ay[,y]<-out$ax; by[,y]<-out$bx
        
        if (gc) {
          Len_agex[, , y] <- out$Len_agenext
          WatAgex[, , y] <- out$Wt_agenext
          Fec_agex[, , y] <- out$Fec_agenext
        }
      }
    
      Nx[, , y, ] <- out$Nnext
      Bx[, , y, ] <- out$Bnext
      # SSNx[, , y, ] <- out$SSNnext
      # SSBx[, , y, ] <- out$SSBnext
    }
    
    M_ageArrayx[, , y-1] <- out$M_agecur
    Marrayx[, y-1] <- out$Mx
      
    VBx[, , y-1, ] <- out$VBt
    VBfx[, , , y-1, ] <- out$VBft
      
    Zx[, , y-1, ] <- out$Zt
    Fty[, , y-1, ] <- out$Ft
    FMy[, , , y-1, ] <- out$FMx
    FMrety[, , , y-1, ] <- out$FMretx
  }
  
  list(Nx=Nx, #1
       Bx=Bx, #2
       SSNx=SSNx,#3
       SSBx=SSBx, #4
       VBx=VBx, #5
       FMy=FMy, #6
       FMrety=FMrety, #7
       Linfarrayx=Linfarrayx, #8
       Karrayx=Karrayx, #9
       t0array=t0arrayx, #10
       Len_age=Len_agex, #11
       Wt_age=WatAgex, #12
       My=Marrayx, #13
       hsy=hsy, #14
       ay=ay, #15
       by=by, #16
       VBfx=VBfx, #17
       Zx=Zx, #18
       Fty=Fty, #19
       M_ageArrayx=M_ageArrayx, #20
       Fec_Agex=Fec_agex, # 21
       Spat_targ=Spat_targ) #22
}



#' Population dynamics for a MICE model (single year)
#'
#' Completes a single iteration of recruitment, mortality, fishing and
#' movement given MICE model inputs
#'
#' @param np Integer, the number of stocks
#' @param nf Integer, number of fleets
#' @param nareas Integer, number of areas
#' @param maxage Integer, maximum modelled age
#' @param Ncur Array `[stock, age, area]` of stock numbers
#' @param Bcur Array `[stock, age, area]` of stock biomass
#' @param SSBcur Array `[stock, age, area]` of spawning biomass
#' @param Vcur Array `[fleet, age, area]` of the vulnerability curve
#' @param FMretx Array `[stock, fleet, age, area]` of the retention curve
#' @param FMx Array `[stock, fleet, age, area]` fishing mortality rate
#' @param PerrYrp Vector `[stock]` process error - the lognormal factor for
#' recruitment strength (for next year)
#' @param hsx Vector `[stock]` steepness of the stock recruitment curve
#' @param aRx Vector `[stock]` stock recruitment parameter alpha (for Ricker curve)
#' @param bRx Vector `[stock]` stock recruitment parameter beta (for Ricker curve)
#' @param movy Array `[stock,age,area,area]` of movement transitions (for next year)
#' @param Spat_targ Matrix `[stock, fleet]` of spatial targeting parameter
#' (0 evenly spatial distributed, 1 proportional to vulnerable biomass)
#' @param SRrelx Integer vector `[stock]` the form of the stock recruitment
#'  relationship (1 = Beverton-Holt, 2= Ricker)
#' @param M_agecur Matrix `[stock, age]` of Natural mortality rate at age
#' @param Mat_agecur Matrix `[stock, age]` of maturity (spawning fraction) at age (current year)
#' @param Mat_agenext Matrix `[stock, age]` of maturity (spawning fraction) at age (next year)
#' @param Fec_agenext Matrix `[stock, age]` of spawning weight (fecundity) at age (next year)
#' @param Asizex Matrix `[stock, area]` of relative area sizes
#' @param Kx Vector `[stock]` of von B growth parameter K (next year)
#' @param Linfx Vector `[stock]` of von B asymptotic length parameter Linf (next year)
#' @param t0x Vector `[stock]` of von B theoretical age at zero length (t0) (next year)
#' @param Mx Vector `[stock]` mature natural mortality rate (this year)
#' @param R0x Vector `[stock]` unfished recruitment
#' @param R0ax Matrix `[stock, area]` unfished recruitment by area
#' @param SSBpRx Matrix `[stock, area]` spawning biomass per recruit by area
#' @param ax Vector `[stock]` weight-length parameter a W=aL^b
#' @param bx Vector `[stock]` weight-length parameter b W=aL^b
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific relationships (SSBfrom, stock_age)
#' @param x Integer. The simulation number
#' @param plusgroup Numeric vector. Use plus-group (1) or not (0)
#' @param SSB0x Unfished SSB0, Vector `[stock]` length.
#' @param B0x Unfished B0, Vector `[stock]` length.
#' @param Len_agenext Matrix `[stock, age]` of next year's length-at-age
#' @param Wt_agenext Matrix `[stock, age]` of next year's weight-at-age
#' @param SRRfun  Optional. A stock-recruit function used if `SRrelc =3` 
#' @param SRRpars Optional. A named list of arguments for `SRRfun`
#' @param ycur Integer. The current year of the simulation
#' @author T.Carruthers
#' @keywords internal
popdynOneMICE <- function(np, nf, nareas, maxage, Ncur, Bcur, SSBcur, Vcur, FMretx, FMx, PerrYrp,
                          hsx, aRx, bRx, movy, Spat_targ,
                          SRrelx, M_agecur, Mat_agecur, Mat_agenext, Fec_agenext,
                          Asizex,
                          Kx, Linfx, t0x, Mx, R0x, R0ax, SSBpRx, ax, bx, Rel, SexPars, x,
                          plusgroup, SSB0x, B0x,
                          Len_agenext, Wt_agenext,
                          SRRfun, SRRpars, ycur) {
  
  n_age <- maxage + 1
  Nind <- TEG(dim(Ncur)) # p, age, area
  
  # These could change, these are values previous to MICE rel
  oldMx <- Mx # M of mature animals
  oldM_agecur <- M_agecur
  
  oldLen_agenext <- Len_agenext
  oldWt_agenext <- Wt_agenext
  oldFec_agenext <- Fec_agenext
  
  oldPerrYrp <- PerrYrp
  
  if (length(Rel)) { # MICE relationships, parameters that could change: M, K, Linf, t0, a, b, hs, Perr_y
    Dlag_all <- sapply(Rel, get_Dlag)
    
    if (any(Dlag_all == "current")) {  # Proceed except if Rel[[r]]$Dlag = "next"
      Responses <- ResFromRel(Rel[Dlag_all == "current"], Bcur, SSBcur, Ncur, SSB0x, B0x, seed = 1, x, y = ycur)
      
      Dmodnam <- sapply(Responses, getElement, "modnam")
      Dp <- sapply(Responses, getElement, "Dp")
      Dval <- lapply(Responses, getElement, "value")
      Dage <- lapply(Responses, getElement, "age")
      Dmult <- sapply(Responses, getElement, "mult")
      
      Rel_txt <- sapply(1:length(Responses), function(r) {
        if (Dmodnam[r] == "Mx" && all(!is.na(Dage[[r]]))) { # Age-specific M
          Rel_var <- paste0("M_agecur[", Dp[r], ", Dage[[r]] + 1]")
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
      
      for (r in 1:length(Responses)) { # e.g., Mx[1] <- 0.4 - operations are sequential
        eval(parse(text = Rel_txt[r]))
      }
      
      if (any(Dmodnam %in% c("Linfx", "Kx", "t0x"))) { # only update Len_age for next year if MICE response is used
        Len_agenext <- matrix(Linfx * (1 - exp(-Kx * (rep(0:maxage, each = np) - t0x))), nrow = np)
        Len_agenext[Len_agenext < 0] <- tiny
      } 
      if (any(Dmodnam %in% c("Linfx", "Kx", "t0x", "ax", "bx"))) { # only update Len_age/Wt_age for next year if MICE response
        # update relative fecundity-at-age for SSB
        Fec_per_weight <- array(NA, dim = dim(Fec_agenext))
        Fec_per_weight[Nind[, 1:2]] <- Fec_agenext[Nind[, 1:2]]/Wt_agenext[Nind[ ,1:2]]
        
        Wt_agenext <- ax * Len_agenext ^ bx # New weight-at-age
        Fec_agenext[Nind[, 1:2]] <- Fec_per_weight[Nind[, 1:2]] * Wt_agenext[Nind[, 1:2]]
      }
      
      # Recalc M_age for this year (only if age-invariant M was updated) ------------------
      if (any(Dmodnam == "Mx") && all(M_agecur == oldM_agecur)) {
        M_agecur <- oldM_agecur * Mx/oldMx
      }
      
      # --- This is redundant code for updating parameters when R0 changes -----
      # surv <- cbind(rep(1,np),t(exp(-apply(M_agecur, 1, cumsum)))[, 1:(maxage-1)])  # Survival array
      # SSB0x<-apply(R0x*surv*Mat_agecur*Wt_age,1,sum)
      #SSBpRx<-SSB0x/R0x
      #SSBpRax<-SSBpRx*distx
      #SSB0ax<-distx*SSB0x
      #R0ax<-distx*R0x
      #R0recalc thus aR bR recalc ---------------
      #bRx <- matrix(log(5 * hsx)/(0.8 * SSB0ax), nrow=np)  # Ricker SR params
      #aRx <- matrix(exp(bRx * SSB0ax)/SSBpRx, nrow=np)  # Ricker SR params
    }
  } # end of MICE
  
  # Survival this year
  surv <- array(c(rep(1,np), t(exp(-apply(M_agecur, 1, cumsum)))[, 1:(n_age-1)]), c(np, n_age))  # Survival array
  surv[plusgroup, n_age] <- surv[plusgroup, n_age]/(1 - exp(-M_agecur[plusgroup, n_age])) # plusgroup
  
  # Vulnerable biomass calculation (current year) -------------
  VBft <- Fdist <- array(NA, c(np, nf, n_age, nareas))
  VBind <- TEG(dim(VBft))
  VBft[VBind] <- Vcur[VBind[, 1:3]] * Bcur[VBind[, c(1,3:4)]]
  Ft <- apply(FMx, c(1, 3, 4), sum) %>% array(c(np, n_age, nareas))
  Zcur <- Ft + replicate(nareas, M_agecur)
  
  SumF <- apply(FMx, c(1, 3, 4), sum, na.rm = TRUE)
  Fapic <- apply(SumF, c(1, 3), max)     # get apical F
  Fapic[Fapic < tiny] <- tiny
  Selx <- array(NA, dim(SumF))
  Selx[Nind] <- SumF[Nind]/Fapic[Nind[, c(1, 3)]]
  VBt <- Bcur * Selx
  
  # ----------- Next year's abundance -----------
  # Mortality
  Nnext <- sapply(1:np, function(p) {
    popdynOneTScpp(nareas, maxage, Ncurr = Ncur[p, , ], Zcurr = Zcur[p, , ], plusgroup = plusgroup[p])
  }, simplify = "array") %>% aperm(c(3, 1, 2)) # np x n_age x nareas

  Nnext[, 1, ] <- 0
  
  # hack for 0-filled M-at-age
  for (p in 1:np) {
    zero.m.ind <- which(M_agecur[p,]==0)
    if (length(zero.m.ind)>0) {
      # set N above maxage for this stock to zero
      Nnext[p, min(zero.m.ind)-1, ] <- Nnext[p, min(zero.m.ind)-1, ] + apply(Nnext[p, zero.m.ind, , drop = FALSE], 3, sum)
      Nnext[p, zero.m.ind, ] <- 0 
    }
  }
  
  # Re-assign abundance due to hermaphroditism
  if (length(SexPars$Herm)) {
    Nnext[is.na(Nnext)] <- 0 # catch for NAs
    for (i in 1:length(SexPars$Herm)) {
      ps <- as.numeric(strsplit(names(SexPars$Herm)[i], "_")[[1]][2:3])
      pfrom <- ps[2]
      pto <- ps[1]
      frac <- rep(1, n_age)
      frac[1:length(SexPars$Herm[[i]][x, ])] <- SexPars$Herm[[i]][x, ]
      h_rate <- hrate(frac)
      Nnext[pto, , ] <- Nnext[pto, , ] * (frac > 0) # remove any recruitment
      Nmov <- Nnext[pfrom, , ] * h_rate
      Nnext[pto, , ] <- Nnext[pto,,] + Nmov
      Nnext[pfrom, , ] <- Nnext[pfrom, , ] - Nmov  # subtract fish
    }
  }
  
  # Calculate SSB for S-R relationship
  SSBtemp <- array(NA_real_, dim(Nnext)) # np x n_age x nareas
  SSBtemp[Nind] <- Nnext[Nind] * Fec_agenext[Nind[, 1:2]]
  SSB_SR <- local({
    if (length(SexPars$SSBfrom)) { # use SSB from another stock to predict recruitment
      sapply(1:np, function(p) apply(SexPars$SSBfrom[p, ] * SSBtemp, 2:3, sum), simplify = "array") %>%
        aperm(c(3, 1, 2))
    } else {
      SSBtemp
    }
  })
  
  # Re-run MICE if any of them use recruitment deviations with next year's abundance/biomass
  if (length(Rel) && any(Dlag_all == "next")) {
    Responses2 <- local({
      Bnext <- SSBnext <- array(NA_real_, dim(Nnext)) # np x n_age x nareas
      Bnext[Nind] <- Nnext[Nind] * Wt_agenext[Nind[, 1:2]]
      ResFromRel(Rel[Dlag_all == "next"], Bcur = Bnext, SSBcur = SSBtemp, Ncur = Nnext, SSB0x, B0x, seed = 1, x, y = ycur)
    })
    
    Dmodnam2 <- sapply(Responses2, getElement, "modnam")
    Dp2 <- sapply(Responses2, getElement, "Dp")
    Dval2 <- lapply(Responses2, getElement, "value")
    Dmult2 <- sapply(Responses2, getElement, "mult")
    
    Rel_txt2 <- sapply(1:length(Responses2), function(r) {
      Rel_var2 <- paste0(Dmodnam2[r], "[", Dp2[r], "]")
      if (Dmult2[r]) {
        Rel_val2 <- paste("Dval2[r] *", Rel_var2)
      } else {
        Rel_val2 <- "Dval2[[r]]"
      }
      paste(Rel_var2, "<-", Rel_val2)
    })
    
    for (r in 1:length(Responses2)) { # e.g., PerrYrp[1] <- 1.5 - operations are sequential
      eval(parse(text = Rel_txt2[r]))
    }
  }

  # Generate next year's recruitment (this will get updated if spawn_time_frac > 0 )
  Nnext[, 1, ] <- sapply(1:np, function(p) {
    calcRecruitment_int(SRrel = SRrelx[p], SSBcurr = SSB_SR[p, , ], recdev = PerrYrp[p], hs = hsx[p],
                        aR = aRx[p, 1], bR = 1/sum(1/bRx[p, ]), R0a = R0ax[p, ], SSBpR = SSBpRx[p, 1],
                        SRRfun[[p]], SRRpars[[p]])
  }) %>% t()

  # Movement
  for (p in 1:np) {
    Nnext[p,,] <- movestockCPP(nareas, maxage, mov=movy[p,,,], Nnext[p,,])
  }
  
  # Calculate biomass at beginning of next year
  Bnext <- SSBnext <- SSNnext <- array(NA_real_, dim(Nnext)) # np x n_age x nareas
  
  Bnext[Nind] <- Nnext[Nind] * Wt_agenext[Nind[, 1:2]]
  # SSBnext[Nind] <- Nnext[Nind] * Fec_agenext[Nind[, 1:2]]
  # SSNnext[Nind] <- Nnext[Nind] * Mat_agenext[Nind[, 1:2]]
  # 
  # returns new N and any updated parameters:
  list(Nnext=Nnext, #1
       M_agecur=M_agecur, #2
       R0x=R0x, #3
       R0ax=R0ax, #4
       hsx=hsx, #5
       aRx=aRx, #6
       bRx=bRx, #7
       Linfx=Linfx, #8
       Kx=Kx, #9
       t0x=t0x, #10
       Mx=Mx, #11
       ax=ax, #12
       bx=bx, #13
       Len_agenext=Len_agenext, #14
       Wt_agenext=Wt_agenext, #15
       surv=surv, #16
       FMx=FMx, #17
       FMretx=FMretx, #18
       VBt=VBt, #19
       VBft=VBft, #20
       Zt=Zcur, #21
       Ft=Ft, #22
       Bnext=Bnext, #23
       SSNnext=SSNnext, #24
       SSBnext=SSBnext,#25
       Fec_agenext=Fec_agenext,#26
       PerrYrp=PerrYrp)#27
  
}



#' Returns Results of a set of MICE relationships
#'
#' Predicts stock-specific parameters from another stocks biomass, spawning biomass or numbers
#'
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param Bcur An array of current stock biomass `[stock, age, area]`
#' @param SSBcur An array of current spawning stock biomass `[stock, age, area]`
#' @param Ncur An array of current stock numbers `[stock, age, area]`
#' @param SSB0 A vector of unfished spawning biomass `[stock]`
#' @param B0 A vector of unfished biomass `[stock]`
#' @param seed Seed for sampling.
#' @param x The simulation number.
#' @param y The current year of the simulation.
#' @author T. Carruthers, Q. Huynh
#' @keywords internal
ResFromRel <- function(Rel, Bcur, SSBcur, Ncur, SSB0, B0, seed, x, y) {

  IVnams <- c("B", "SSB", "N", "Nage", "SSB0", "B0", "x", "y")           # Variable in MICE function
  IVcode <- c("Bcur", "SSBcur", "Ncur", "Nage", "SSB0", "B0", "x", "y")  # Variable in OM
  B <- apply(Bcur, 1, sum)
  SSB <- apply(SSBcur, 1, sum)
  N <- apply(Ncur, 1, sum)
  Nage <- apply(Ncur, 1:2, sum)

  DVnam <- c("M", "a", "b", "R0", "hs", "K", "Linf", "t0", "Perr_y")            # Variable in MICE function
  modnam <- c("Mx", "ax", "bx", "R0x", "hsx", "Kx", "Linfx", "t0x", "PerrYrp")  # Variable in OM

  nRel <- length(Rel)
  
  out <- lapply(1:nRel, function(r) {
    fnams <- names(Rel[[r]]$model)
    DV <- fnams[1]
    Dp <- get_Dp(DV)
    Dnam <- get_Dnam(DV)
    IV <- fnams[-1]
    nIV <- length(IV)
    
    newdata <- lapply(IV, function(iv, B, SSB, N, Nage, SSB0, B0, x, y)  {
      IVs <- unlist(strsplit(iv, "_"))
      p <- ifelse(length(IVs) == 1, 1, as.numeric(IVs[2]))
      
      # Get independent variables from OM
      OMvar <- get(IVs[1], inherits = FALSE)
      if (is.matrix(OMvar)) {
        OMvar[p, ]
      } else {
        OMvar[p]
      }
    }, B = B, SSB = SSB, N = N, Nage = Nage, SSB0 = SSB0, B0 = B0, x = x, y = y) %>% 
      structure(names = IV) %>%
      unlist()
    
    # If IV = Nage_p, then name will be Nage_pa (population, age)
    newdata2 <- matrix(newdata, nrow = 1) %>%
      as.data.frame() %>% 
      structure(names = names(newdata))
    
    ys <- predict(Rel[[r]], newdata = newdata2)
    templm <- Rel[[r]]
    templm$fitted.values <- ys
    ysamp <- stats::simulate(templm, nsim = 1, seed = seed) %>% unlist()
    
    rel_r <- list(
      value = ysamp,
      DV = DV,
      Dp = Dp,
      modnam = modnam[match(Dnam, DVnam)]
    )
    
    if (Dnam == "Perr_y") {
      if (!is.null(Rel[[r]]$lag)) {
        lag <- Rel[[r]]$lag # choices are "current" or "next"
      } else {
        lag <- "current"
      }
      if (lag != "next") lag <- "current"
    } else {
      lag <- NA_character_
    }
    rel_r[["lag"]] <- lag
    
    if (Dnam == "M") {
      if (!is.null(Rel[[r]]$age)) {
        age <- Rel[[r]]$age
      } else {
        age <- NA_integer_
      }
    } else {
      age <- NA_integer_
    }
    rel_r[["age"]] <- age
    
    rel_r[["mult"]] <- FALSE
    if (!is.null(Rel[[r]]$mult) && is.logical(Rel[[r]]$mult)) rel_r[["mult"]] <- Rel[[r]]$mult
    
    return(rel_r)
  })
  
  return(out)
}

get_Dlag <- function(Rel) {
  lag <- Rel$lag
  if (is.null(lag) || lag != "next") lag <- "current"
  return(lag)
}


get_Dp <- function(DV) {
  x <- unlist(strsplit(DV, "_"))
  x[length(x)]
}

get_Dnam <- function(DV) {
  x <- unlist(strsplit(DV, "_"))
  paste0(x[-length(x)], collapse = "_")
}
  
#' Derives the rate of exchange from one sex to another based on asymptotic fraction
#'
#' @param frac A vector of asymptotic sex fraction (must start with zero and end with 1)
#' @author T.Carruthers
#' @keywords internal
hrate<-function(frac){

  m1frac<-1-frac
  ind1<-(1:(length(frac)-1))
  ind2<-ind1+1
  hrate<-rep(0,length(frac))
  hrate[ind2]<-1-(m1frac[ind2]/m1frac[ind1])
  hrate[is.na(hrate)]<-1
  hrate[hrate<0]<-0
  #cbind(frac,m1frac,hrate)
  hrate

}


