popdynOneMICEDEV <- function(np, nf, nareas, maxage, Ncur, Bcur, SSBcur, Vcur, FMretx, FMx, PerrYrp,
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
