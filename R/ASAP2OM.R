

#' @title Convert ASAP 3 assessments into an operating model
#
#' @description Reads a fitted ASAP model and uses the MLE estimates with identical reconstruction among simulations. Future recruitment is
#' sampled from a lognormal distribution with autocorrelation. \code{ASAP2Data} imports a Data object.
#' 
#' @param asap A list returned by ASAP, e.g., \code{asap <- dget("asap3.rdat")}.
#' @param nsim The number of simulations in the operating model
#' @param proyears The number of MSE projection years
#' @param mcmc Logical, whether to use mcmc samples. Currently unsupported. 
#' @param Name The name of the operating model
#' @param Source Reference to assessment documentation e.g. a url
#' @param nyr_par_mu integer, the number of recent years to estimate vulnerability over for future projections
#' @param Author Who did the assessment
#' @param report Logical, should a comparison of biomass reconstruction be produced?
#' @param silent Logical, should progress reporting be printed to the console?
#' @return An operating model \linkS4class{OM} class.
#' 
#' @details 
#' Length at age is not used in ASAP so arbitrary placeholder values are used for length-based parameters. Update
#' these parameters to model length in the operating model. 
#' 
#' @author Q. Huynh
#' @seealso \link{Assess2OM}
#' @export
ASAP2OM <- function(asap, nsim = 48, proyears = 50, mcmc = FALSE, Name = "ASAP Model", Source = "No source provided",
                    nyr_par_mu = 3, Author = "No author provided", report = FALSE, silent = FALSE) {
  
  # get dimensions
  nyears <- asap$parms$nyears
  maxage <- asap$parms$nages
  CurrentYr <- asap$parms$endyr
  sage <- 1
  n_age <- maxage + 1
  nfleet <- asap$nfleets
  
  M <- N <- array(NA_real_, c(nsim, n_age, nyears))
  M[, sage:maxage + 1, ] <- replicate(nsim, asap$M.age) %>% aperm(3:1)
  N[, sage:maxage + 1, ] <- replicate(nsim, asap$N.age) %>% aperm(3:1) # Back calculate age-0
  
  FM <- Mat_age <- Fec_age <- Wt_age <- array(0, c(nsim, n_age, nyears))
  FM[, sage:maxage + 1, ] <- replicate(nsim, asap$F.age) %>% aperm(3:1) # Set age-0 F to zero
  
  Mat_age[, sage:maxage + 1, ] <- replicate(nsim, asap$maturity) %>% aperm(3:1) # Set age-0 mat to zero
  Fec_age[, sage:maxage + 1, ] <- replicate(nsim, asap$fecundity) %>% aperm(3:1) # Set age-0 fec to zero
  Wt_age[, sage:maxage + 1, ] <- replicate(nsim, asap$WAA.mats$WAA.jan1) %>% aperm(3:1) # Set age-0 wt to zero
  
  if(sage > 0) {
    M[, 1:sage, ] <- tiny + .Machine$double.eps
    FM[, 1:sage, ] <- Mat_age[, 1:sage, ] <- Fec_age[, 1:sage, ] <- Wt_age[, 1:sage, ] <- 0
    
    aind_missing <- sage:1 # Missing cohorts to be filled in
    for(i in 1:length(aind_missing)) {
      N[, aind_missing[i], 2:nyears - 1] <- N[, aind_missing[i] + 1, 2:nyears] * exp(M[, aind_missing[i], 2:nyears - 1])
    }
  }
  
  Len_age <- Wt_age^(1/3) # Placeholder
  
  # SPR
  h <- min(0.999, asap$SR.parms$SR.steepness)
  R0 <- asap$SR.parms$SR.R0
  phi0 <- asap$SR.parms$SR.SPR0
  
  OM <- Assess2OM(Name = Name,
                  proyears = proyears, interval = 2, CurrentYr = CurrentYr, h = h,
                  Obs = MSEtool::Imprecise_Unbiased, Imp = MSEtool::Perfect_Imp,
                  naa = N, faa = FM, waa = Wt_age, Mataa = Mat_age, Maa = M, laa = Len_age,
                  nyr_par_mu = nyr_par_mu, LowerTri = sage,
                  recind = 0, plusgroup = TRUE, altinit = 0, fixq1 = TRUE,
                  report = FALSE, silent = silent, R0 = R0, phi0 = phi0, 
                  Perr = sdconv(1, max(asap$control.parms$recruit.cv)))
  
  # Wt_age C
  OM@cpars$Wt_age_C <- local({
    Wt <- array(0, c(nsim, n_age, nyears + proyears))
    Whist <- replicate(nsim, asap$WAA.mats$WAA.catch.all) %>% aperm(3:1)
    Wpro <- replicate(proyears, Whist[, , nyears])
    Wt[, sage:maxage + 1, ] <- abind::abind(Whist, Wpro, along = 3)
    Wt
  })
  
  # growth parameters - placeholders to avoid fitting in MSEtool::SampleStockPars
  Lestpars <- local({
    L <- apply(Wt_age[1, , ]^(1/3), 1, mean)
    starts  <- c(log(max(L)), log(0.2), 0)
    optim(starts, fitVB, LatAge = L, ages=0:maxage)$par
  })
  OM@cpars$Linf <- rep(exp(Lestpars[1]), nsim)
  OM@cpars$K <- rep(exp(Lestpars[2]), nsim)
  OM@cpars$t0 <- rep(Lestpars[3], nsim)
  
  OM@CurrentYr <- CurrentYr
  
  # Observation model parameters ==============================================================================
  OM@cpars$Data <- ASAP2Data(asap, Name = Name)
  if(!all(is.na(length(OM@cpars$Data@AddInd)))) {
    OM@cpars$AddIbeta <- matrix(1, nsim, dim(OM@cpars$Data@AddInd)[2])
  }
  
  # REPORT
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  if(!mcmc && report) {
    Hist <- runMSE(suppressMessages(SubCpars(OM, 1:2)), Hist = TRUE, silent = TRUE)
    
    # jan 1 biomass
    par(mfrow = c(2, 2), mar = c(5, 4, 1, 1))
    Year <- OM@cpars$Data@Year
    plot(Year, asap$tot.jan1.B, ylim = c(0, 1.1 * max(asap$tot.jan1.B)), pch = 16, typ = 'o', xlab = "Year", 
         ylab = "Total biomass (start of year)")
    lines(Year, rowSums(Hist@TSdata$Biomass[1, , ]), pch = 16, typ = "o", col = "red")
    legend("topleft", c("ASAP", "OM"), pch = 16, col = c("black", "red"))
    abline(h = 0, col = "grey")
    
    # SSB
    plot(Year, asap$SSB, ylim = c(0, 1.1 * max(asap$SSB)), pch = 16, typ = 'o', xlab = "Year", ylab = "Spawning biomass")
    lines(Year, rowSums(Hist@TSdata$SBiomass[1, , ]), pch = 16, typ = "o", col = "red")
    #legend("topleft", c("ASAP", "OM"), pch = 16, col = c("black", "red"))
    abline(h = 0, col = "grey")
    
    # VB
    plot(Year, asap$exploitable.B, ylim = c(0, 1.1 * max(asap$exploitable.B)), pch = 16, typ = 'o', xlab = "Year", 
         ylab = "Vulnerable biomass")
    lines(Year, rowSums(Hist@TSdata$VBiomass[1, , ]), pch = 16, typ = "o", col = "red")
    #legend("topleft", c("ASAP", "OM"), pch = 16, col = c("black", "red"))
    abline(h = 0, col = "grey")
    
    # Removals
    Rem <- colSums(asap$catch.pred + asap$discard.pred)
    plot(Year, Rem, ylim = c(0, 1.1 * max(Rem)), pch = 16, typ = 'o', xlab = "Year", 
         ylab = "Total Removals")
    lines(Year, rowSums(Hist@TSdata$Removals[1, , ]), pch = 16, typ = "o", col = "red")
    #legend("topleft", c("ASAP", "OM"), pch = 16, col = c("black", "red"))
    abline(h = 0, col = "grey")
  }
  
  return(OM)
}

#' @rdname ASAP2OM
#' @export  
ASAP2Data <- function(asap, Name = "ASAP assessment") {
  
  Data <- new("Data")
  Data@Name <- Name
  Data@Year <- seq(asap$parms$styr, asap$parms$endyr)
  Data@MaxAge <- asap$parms$nages
  
  ny <- length(Data@Year)
  final3y <- -2:0 + ny
  
  Catch <- colSums(asap$catch.obs + asap$discard.obs)
  Catch[Catch < 1e-8] <- 1e-8
  Data@Cat <- matrix(Catch, 1, ny)
  Data@CV_Cat <- local({
    num <- colSums(asap$catch.obs * t(asap$control.parms$catch.tot.cv) + asap$discard.obs * t(asap$control.parms$discard.tot.cv))
    den <- Data@Cat[1, ]
    matrix(num/den, 1, ny)
  })
  
  # AddInd, CV_AddInd, AddIndV, AddIndType, AddIunits
  n_index <- asap$parms$nindices
  Data@AddInd <- vapply(1:n_index, function(x) {
    out <- rep(NA_real_, ny)
    out[asap$index.year.counter[[x]]] <- asap$index.obs[[x]]
    return(out)
  }, numeric(ny)) %>% array(c(1, ny, n_index)) %>% aperm(c(1, 3, 2))
  
  Data@CV_AddInd <- vapply(1:n_index, function(x) {
    out <- rep(NA_real_, ny)
    out[asap$index.year.counter[[x]]] <- asap$index.cv[[x]]
    return(out)
  }, numeric(ny)) %>% array(c(1, ny, n_index)) %>% aperm(c(1, 3, 2))
  
  Data@AddIunits <- ifelse(asap$control.parms$index.units.aggregate == 1, 1, 0) # ASAP units 1 = Biomass at age, 2 = Abundance at age
  Data@AddIndType <- rep(1, n_index)
  
  Data@AddIndV <- local({
    out <- array(0, c(1, n_index, Data@MaxAge + 1))
    a <- asap$index.sel %>% colnames() %>% as.numeric()
    out[1, 1:n_index, a + 1] <- asap$index.sel
    out
  })
  
  Data@t <- length(Data@Year)
  Data@AvC <- mean(Data@Cat)
  
  Data@Dt <- asap$SSB[ny]/asap$SR.parms$SR.S0
  Data@Mort <- asap$M.age[ny, ] %>% mean()
  
  #A50 <- LinInterp(asap$maturity[ny, ], 1:Data@MaxAge, 0.5)
  #A95 <- LinInterp(asap$maturity[ny, ], 1:Data@MaxAge, 0.95)
  #Data@L50 <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), A50)
  #Data@L95 <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), A95)
  
  #FM <- asap$F.age[ny, ]
  #V <- FM/max(FM)
  #AFC <- LinInterp(V, 1:Data@MaxAge, 0.05, ascending = TRUE)
  #AFS <- LinInterp(V, 1:Data@MaxAge, 0.95, ascending = TRUE)
  #Data@LFC <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), AFC)
  #Data@LFS <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), AFS)
  
  Data@CAA <- local({
    a <- array(0, c(1, ny, Data@MaxAge + 1))
    a[1, , 1:Data@MaxAge + 1] <- asap$catch.comp.mats$catch.fleet1.ob
    zeroind <- rowSums(a[1, , ], na.rm = TRUE) == 0
    a[1, zeroind, ] <- NA_real_
    a
  })
  
  Data@Dep <- asap$SSB[ny]/asap$SR.parms$SR.S0
  Data@SpAbun <- asap$SSB[ny]
  
  Data@steep <- asap$SR.parms$SR.steepness
  Data@Ref <- Data@Cat[1, ny]
  Data@Ref_type <- paste(max(Data@Year), "Catch")
  Data@MPrec <- Data@Cat[1, ny]
  Data@MPeff <- 1
  Data@LHYear <- max(Data@Year)
  
  Data
  
}
