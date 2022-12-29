
SS_import <- function(SSdir, silent = FALSE, ...) {
  if(!requireNamespace("r4ss", quietly = TRUE)) {
    stop("Download the r4ss package to use this function. It is recommended to install the Github version with: remotes::install_github(\"r4ss/r4ss\")", call. = FALSE)
  }

  dots <- list(dir = SSdir, ...)
  if(!any(names(dots) == "covar")) dots$covar <- FALSE
  if(!any(names(dots) == "forecast")) dots$forecast <- FALSE
  #if(!any(names(dots) == "ncols")) dots$ncols <- 1e3
  if(!any(names(dots) == "printstats")) dots$printstats <- FALSE
  if(!any(names(dots) == "verbose")) dots$verbose <- FALSE
  if(!any(names(dots) == "warn")) dots$warn <- FALSE

  if(!silent) {
    message(paste("-- Using function SS_output of package r4ss version", packageVersion("r4ss"), "to extract data from SS file structure --"))
    message(paste("Reading directory:", SSdir))
  }
  replist <- try(do.call(r4ss::SS_output, dots), silent = TRUE)
  if(is.character(replist)) stop("r4ss::SS_output function returned an error -\n", replist, call. = FALSE)
  if(!silent) message("-- End of r4ss operations --\n")

  return(replist)
}

SS_steepness <- function(replist, mainyrs, mean_h = TRUE, nsim, seed = 1) {

  if(replist$SRRtype == 3 || replist$SRRtype == 6) { # Beverton-Holt SR
    SRrel <- 1L
    par <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]

    h <- par$Value
    hsd <- ifelse(is.na(par$Parm_StDev), 0, par$Parm_StDev)
    if(mean_h) {
      h_out <- rep(h, nsim)
    } else{
      set.seed(seed)
      h_out <- rnorm(nsim, h, hsd)
    }
  } else if(replist$SRRtype == 2) {
    SRrel <- 2L
    par <- replist$parameters[grepl("SR_Ricker", rownames(replist$parameters)), ]

    h <- par$Value
    hsd <- ifelse(is.na(par$Parm_StDev), 0, par$Parm_StDev)
    if(mean_h) {
      h_out <- rep(h, nsim)
    } else{
      set.seed(seed)
      h_out <- rnorm(nsim, h, hsd)
    }
  } else if(replist$SRRtype == 7) {
    SRrel <- 1L

    s_frac <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Sfrac"]
    Beta <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Beta"]

    s0 <- 1/SpR0
    z0 <- -log(s0)
    z_min <- z0 * (1 - s_frac)

    h <- 0.2 * exp(z0 * s_frac * (1 - 0.2 ^ Beta))
    hsd <- 0

    h_out <- rep(h, nsim)
  } else {
    if(packageVersion("r4ss") == 1.24) {
      SR_ind <- match(mainyrs, replist$recruit$year)
      SSB <- replist$recruit$spawn_bio[SR_ind]
      SSB0 <- replist$derived_quants[replist$derived_quants$LABEL == "SPB_Virgin", 2]
    } else {
      SR_ind <- match(mainyrs, replist$recruit$Yr)
      SSB <- replist$recruit$SpawnBio[SR_ind]
      SSB0 <- replist$derived_quants[replist$derived_quants$Label == "SSB_Virgin", 2]
    }
    SRrel <- 1L

    rec <- replist$recruit$pred_recr[SR_ind] # recruits to age 0
    SpR0 <- SSB0/(R0 * ifelse(season_as_years, nseas, 1))

    if(mean_h) {
      h_out <- SRopt(1, SSB, rec, SpR0, plot = FALSE, type = ifelse(SRrel == 1, "BH", "Ricker")) %>% rep(nsim)
    } else {
      h_out <- SRopt(1:nsim, SSB, rec, SpR0, plot = FALSE, type = ifelse(SRrel == 1, "BH", "Ricker"))
    }
  }

  h_out[h_out < 0.2] <- 0.2
  if(SRrel == 1) h_out[h_out > 0.999] <- 0.999

  return(list(SRrel = SRrel, h = h_out))
}


logit <- function(p) log(p/(1-p))
ilogit <- function(x) 1/(1 + exp(-x))
ilogitm <- function(x) {
  if(inherits(x, "matrix")) {
    return(exp(x)/apply(exp(x), 1, sum))
  } else {
    return(exp(x)/sum(exp(x)))
  }
}

# #' Predict recruitment and return fit to S-R observations
# #'
# #' @description Internal function to \link{optSR}
# #' @param pars an initial guess at model parameters steepness and R0
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param mode should fit (= 1) or recruitment deviations (not 1) be returned
# #' @param plot should a plot of the model fit be produced?#'
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @author T. Carruthers
# #' @export
getSR <- function(pars, SSB, rec, SSBpR, mode = 1, plot = FALSE, type = c("BH", "Ricker")){
  R0 <- exp(pars[2])
  if(type == "BH") {
    h <- 0.2 + 0.8 * ilogit(pars[1])
    recpred<-((0.8*R0*h*SSB)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB))
  }
  if(type == "Ricker") {
    h <- 0.2 + exp(pars[1])
    recpred <- SSB * (1/SSBpR) * (5*h)^(1.25*(1 - SSB/(R0*SSBpR)))
  }

  if(plot){
    ord <- order(SSB)
    plot(SSB[ord], rec[ord], ylim=c(0, max(rec, R0)), xlim=c(0, max(SSB, R0*SSBpR)), xlab="", ylab="")
    SSB2 <- seq(0, R0*SSBpR, length.out=500)
    if(type == "BH") recpred2 <- ((0.8*R0*h*SSB2)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB2))
    if(type == "Ricker") recpred2 <- SSB2 * (1/SSBpR) * (5*h)^(1.25*(1 - SSB2/(R0*SSBpR)))
    lines(SSB2, recpred2, col='blue')
    abline(v=c(0.2*R0*SSBpR, R0*SSBpR), lty=2, col='red')
    abline(h=c(R0, R0*h), lty=2, col='red')
    legend('topright', legend=c(paste0("h = ", round(h,3)), paste0("ln(R0) = ", round(log(R0),3))), bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    sigmaR <- sqrt(sum((log(rec/recpred))^2)/length(recpred))
    return(-sum(dnorm(log(rec)-log(recpred),0,sigmaR,log=T)))
    #-dnorm(pars[1],0,6,log=T)) # add a vague prior on h = 0.6
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }
}

# #' Wrapper for estimating stock recruitment parameters from resampled stock-recruitment data
# #'
# #' @param x position to accommodate lapply-type functions
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param pars an initial guess at model parameters steepness and R0
# #' @param frac the fraction of observations for resampling
# #' @param plot should a plot of model fit be produced?
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @return Estimated value of steepness.
# #' @author T. Carruthers
# #' @export
optSR<-function(x, SSB, rec, SSBpR, pars, frac = 0.5, plot = FALSE, type = c("BH", "Ricker")) {
  type <- match.arg(type)
  samp <- sample(1:length(SSB), size = ceiling(length(SSB) * frac), replace = FALSE)
  opt <- optim(pars, getSR, method = "BFGS", #lower = c(-6, pars[2]/50), upper = c(6, pars[2] * 50),
               SSB = SSB[samp], rec = rec[samp], SSBpR = SSBpR, mode = 1, plot = FALSE, type = type)
  if(plot) getSR(opt$par, SSB, rec, SSBpR, mode = 2, plot = plot, type = type)
  if(type == "BH") h <- 0.2 + 0.8 * ilogit(opt$par[1])
  if(type == "Ricker") h <- 0.2 + exp(opt$par[1])
  return(h)
}

# #' Function that returns a stochastic estimate of steepness given observed stock recruitment data
# #'
# #' @param nsim number of samples of steepness to generate
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param plot should plots of model fit be produced?
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @return Vector of length nsim with steepness values.
# #' @author T. Carruthers
# #' @export
SRopt <- function(nsim, SSB, rec, SSBpR, plot = FALSE, type = c("BH", "Ricker")) {
  type <- match.arg(type)
  R0temp <- rec[1] # have a guess at R0 for initializing nlm
  pars <- c(0, log(R0temp))
  #SSBpR=SSB[1]/rec[1]
  vapply(1:nsim, optSR, numeric(1), SSB = SSB, rec = rec, SSBpR = SSBpR, pars = pars, frac = 0.8,
         plot = plot, type = type)
}


# i = 1 (female)
# i = 2 (male)
SS_stock <- function(i, replist, mainyrs, nyears, proyears, nsim, single_sex = TRUE, partition = 2,
                     age_M = NULL, mean_h = TRUE, seed = 1) {

  Morph <- Morph2 <- Seas <- BirthSeas <- Settlement <- Age_Beg <- Mat_F_wtatage <- NULL # checks

  allyears <- nyears + proyears

  Stock <- new("Stock")
  Stock@Name <- ifelse(i == 1, "Female", "Male")
  if(!is.null(replist$movement) && nrow(replist$movement) > 0) warning("Movement detected in SS model but not imported right now.")
  Stock@Size_area_1 <- Stock@Frac_area_1 <- Stock@Prob_staying <- rep(0.5, 2)

  cpars_bio <- list()

  ###### R0
  if(replist$nsexes > 1) {
    # For the 2 sex model with multiple seasons used to develop this code: 
    # the i-th morph recruits appear in the i-th season Males are indexed morphs 5-8 in a 4-season model. 
    # Morph2 re-maps males to 1-4
    N_at_age <- dplyr::filter(replist$natage, Sex == i, `Beg/Mid` == "B", Era == "VIRG") %>%
      dplyr::mutate(Morph2 = Morph - (i - 1) * replist$nseasons) %>% dplyr::filter(Morph2 == Seas)
  } else {
    # For the single-sex model with multiple seasons used to develop this code: 
    # the annual recruits entering the population is found by matching BirthSeas to Seas
    N_at_age <- dplyr::filter(replist$natage, Sex == i, `Beg/Mid` == "B", Era == "VIRG") %>%
      dplyr::filter(BirthSeas == Seas)
  }
  Stock@R0 <- N_at_age$`0` %>% sum(na.rm = TRUE)

  ###### maxage
  Stock@maxage <- suppressWarnings(colnames(N_at_age) %>% as.numeric()) %>% max(na.rm = TRUE)

  n_age <- Stock@maxage + 1 # include age-0

  ###### Biological parameters
  endgrowth <- dplyr::filter(replist$endgrowth, Sex == i, Seas == 1)
  if(!is.null(endgrowth$BirthSeas)) endgrowth <- dplyr::filter(endgrowth, BirthSeas == 1)
  if(!is.null(endgrowth$Settlement)) endgrowth <- dplyr::filter(endgrowth, Settlement == 1)

  # M
  M_at_age <- replist$M_at_age[replist$M_at_age$Sex == i & replist$M_at_age$Year %in% mainyrs, ]
  if(!nrow(M_at_age)) {
    M_at_age <- replist$M_at_age[replist$M_at_age$Sex == i & replist$M_at_age$Yr %in% mainyrs, ]
  }
  if(!nrow(M_at_age)) {
    M_at_age <- replist$M_at_age[replist$M_at_age$Gender == i & replist$M_at_age$Year %in% mainyrs, ]
  }
  M_age <- suppressWarnings(lapply(0:Stock@maxage, function(x) parse(text = paste0("M_at_age$`", x, "`")) %>% eval() %>% as.numeric()))
  M_age <- do.call(rbind, M_age)
  if(all(is.na(M_age[nrow(M_age), ]))) M_age[nrow(M_age), ] <- endgrowth$M[Stock@maxage]
  if(ncol(M_age) == (nyears - 1)) M_age <- cbind(M_age, endgrowth$M)
  if(proyears) {
    M_age_pro <- matrix(M_age[, nyears], n_age, proyears)
    M_age <- cbind(M_age, M_age_pro)
  }
  if(all(M_age[1, ] < 0)) M_age[1, ] <- M_age[2, ]
  cpars_bio$M_ageArray <- array(M_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  Stock@M <- mean(M_age[, nyears]) %>% rep(2)

  # Steepness
  SR_par <- SS_steepness(replist, mainyrs, mean_h, nsim, seed)
  Stock@SRrel <- SR_par[[1]]
  h_out <- SR_par[[2]]
  Stock@h <- SR_par[[2]]

  Stock@Perr <- rep(replist$sigma_R_in, 2)

  # Perr_y
  Rec_main <- replist$recruit[replist$recruit$Yr %in% mainyrs, ]
  Rdev <- Rec_main$pred_recr/Rec_main$exp_recr
  Rec_early <- replist$recruit[vapply(c((min(mainyrs-1)-(Stock@maxage-1)):(min(mainyrs-1))), match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ]
  Rdev_early <- Rec_early$pred_recr/Rec_early$exp_recr
  Rdev_early[is.na(Rdev_early)] <- 1

  cpars_bio$Perr_y <- c(Rdev_early, Rdev) %>% matrix(nrow = nsim, ncol = Stock@maxage + nyears, byrow = TRUE)

  # AC
  Stock@AC <- log(cpars_bio$Perr_y[1, ]) %>% acf(lag.max = 1, plot = FALSE) %>% getElement("acf") %>%
    getElement(2) %>% rep(2)
  if (any(!is.finite(Stock@AC))) Stock@AC <- c(0,0)

  # Length at age
  Len_age_df <- dplyr::filter(replist$growthseries, Morph == i, Yr %in% mainyrs)
  if(nrow(Len_age_df)>0) { # Would do time-varying
    Len_age <- do.call(rbind, lapply(0:Stock@maxage, function(x) parse(text = paste0("Len_age_df$`", x, "`")) %>% eval()))
    if(ncol(Len_age) == (nyears - 1)) Len_age <- cbind(Len_age, endgrowth$Len_Beg)
  } else {
    Len_age <- endgrowth$Len_Beg %>% matrix(n_age, nyears) # No time-varying
  }

  if(proyears) {
    Len_age_pro <- matrix(Len_age[, nyears], n_age, proyears)
    Len_age <- cbind(Len_age, Len_age_pro)
  }
  cpars_bio$Len_age <- array(Len_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  Stock@LenCV <- mean(endgrowth$SD_Beg[-1]/endgrowth$Len_Beg[-1]) %>% rep(2)
  cpars_bio$LatASD <- array(endgrowth$SD_Beg, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  if(!all(is.na(replist$Growth_Parameters[i, ]))) {
    # Length parameters to avoid re-fitting vb curve in SampleStockPars
    Stock@Linf <- replist$Growth_Parameters[i, ]$Linf %>% rep(2)
    Stock@K <- replist$Growth_Parameters[i, ]$K %>% rep(2)
    Stock@t0 <- replist$Growth_Parameters[i, ]$A_a_L0 %>% rep(2)
    cpars_bio$Linf <- rep(Stock@Linf[1], nsim)
    cpars_bio$K <- rep(Stock@K[1], nsim)
    cpars_bio$t0 <- rep(Stock@t0[1], nsim)

    # Weight at age
    Stock@a <- cpars_bio$Wa <- replist$Growth_Parameters[i, ]$WtLen1
    Stock@b <- cpars_bio$Wb <- replist$Growth_Parameters[i, ]$WtLen2
  }

  if(!is.null(replist$mean_body_wt)) {
    Wt_age_df <- replist$mean_body_wt[replist$mean_body_wt$Morph == i, ]
    Wt_age_df <- Wt_age_df[findInterval(mainyrs, Wt_age_df$Yr), ]
    Wt_age <- do.call(rbind, lapply(0:n_age, function(x) parse(text = paste0("Wt_age_df$`", x, "`")) %>% eval()))
    if(ncol(Wt_age) == nyears - 1) Wt_age <- cbind(Wt_age, endgrowth$Wt_Beg[-1])
  } else {
    Wt_age <- endgrowth$Wt_Beg %>% matrix(n_age, nyears)
  }
  if(proyears) {
    Wt_age_pro <- matrix(Wt_age[, nyears], n_age, proyears)
    Wt_age <- cbind(Wt_age, Wt_age_pro)
  }
  cpars_bio$Wt_age <- array(Wt_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  # Maturity at age - technically fecundity = 0 for males in SS.
  # We will set male maturity equal to female maturity
  # but MOM@SexPars will also effectively set male maturity = 0.
  if(any(endgrowth$Age_Mat < 0)) endgrowth$Age_Mat <- abs(endgrowth$Age_Mat) # Should all be 1's
  if(any(endgrowth$Len_Mat < 0)) endgrowth$Len_Mat <- abs(endgrowth$Len_Mat)
  Mat_age <- endgrowth$Len_Mat * endgrowth$Age_Mat
  cpars_bio$Mat_age <- array(Mat_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  # initial Depletion
  sb0 <- replist$timeseries %>% dplyr::filter(Era == "VIRG") %>% getElement("SpawnBio") %>% sum(na.rm = TRUE)
  sb1 <- replist$timeseries %>% dplyr::filter(Yr == mainyrs[1]) %>% getElement("SpawnBio") %>% sum(na.rm = TRUE)

  if (sb1 != sb0) {
    # initD <- sb1$SpawnBio/sb0$SpawnBio
    # cpars_bio$initD <- rep(initD, nsim)
    # Modify rec devs so N-at-age 1 is correct

    # numbers-at-age in initial year and unfished conditions
    n_init <- replist$natage %>% dplyr::filter(Sex == i, Yr == mainyrs[1], `Beg/Mid`=='B') 
    # If multiple season model, age-0 recruits may enter over the course of each season
    # Get the initial age-0 abundance by summing across seasons
    if(nrow(n_init) > 1) {
      n_age0 <- dplyr::filter(n_init, BirthSeas == Seas) %>% select("0") %>% sum()
      n_init <- dplyr::filter(n_init, Seas == 1)
      n_init$`0` <- n_age0
    }
    cols <- which(colnames(n_init) %in% 0:Stock@maxage)
    n_init <- n_init %>% dplyr::select(dplyr::all_of(cols)) %>% colSums()
    
    n_virg <- replist$natage %>% dplyr::filter(Sex == i, Era == "VIRG", `Beg/Mid` == 'B')
    if(nrow(n_virg) > 1) {
      n_age0 <- dplyr::filter(n_virg, BirthSeas == Seas) %>% select("0") %>% sum()
      n_virg <- dplyr::filter(n_virg, Seas == 1)
      n_virg$`0` <- n_age0
    }
    n_virg <- n_virg %>% dplyr::select(dplyr::all_of(cols)) %>% colSums()

    adjust <- as.numeric(n_init/n_virg)# *  cpars_bio$Perr_y[1,n_age:1])
    cpars_bio$Perr_y[, n_age:1] <- matrix(adjust, nrow = nsim, ncol = n_age, byrow = TRUE)
  }
  
  # Fecundity-at-age (weight used to calculate SB0 (females))
  if(!is.null(replist$endgrowth$`Mat*Fecund`)) {
    fec_age <- replist$endgrowth %>% dplyr::filter(Morph == 1, Seas == 1, Sex==i) %>%
      dplyr::select(Age_Beg, `Mat*Fecund`)
    if (nrow(fec_age)>0) {
      Fec_age = replicate(nsim, fec_age$`Mat*Fecund`)
      Fec_age = replicate(nyears+proyears,Fec_age)
      Fec_age <- aperm(Fec_age, c(2,1,3))
      cpars_bio$Fec_age <- Fec_age # only for females
    }
  } else {
    if(!is.null(replist$endgrowth$Mat_F_wtatage)) {
      fec_age <- replist$endgrowth %>% dplyr::filter(Morph == 1, Seas == 1, Sex==i) %>%
        dplyr::select(Age_Beg, Mat_F_wtatage)
      if (nrow(fec_age)>0) {
        Fec_age = replicate(nsim, fec_age$Mat_F_wtatage)
        Fec_age = replicate(nyears+proyears,Fec_age)
        Fec_age <- aperm(Fec_age, c(2,1,3))
        cpars_bio$Fec_age <- Fec_age # only for females
      }
    }
  }


  # Depletion
  if(i == 1) { # In 3.24, SSB = NA in seasons 1-3 out of 4, so I chose to take the mean
    sb_curr <- replist$timeseries %>% dplyr::filter(Yr == max(mainyrs)) %>% getElement("SpawnBio") %>%
      mean(na.rm = TRUE)
    Stock@D <- rep(sb_curr/sb0, 2)
  } else {
    # Calculate 'SSB' for males because it is always 0 for males in SS
    N_virg <- dplyr::filter(replist$natage, Sex == i, Era == "VIRG", `Beg/Mid` == "B", Seas == 1)
    N_virg2 <- vapply(0:Stock@maxage, function(x)
      N_virg[, parse(text = paste0("\"", x, "\"")) %>% eval()] %>% sum(), numeric(1))

    N_now <- dplyr::filter(replist$natage, Sex == i, Yr == max(mainyrs), `Beg/Mid` == "B", Seas == 1)
    N_now2 <- vapply(0:Stock@maxage, function(x)
      N_now[, parse(text = paste0("\"", x, "\"")) %>% eval()] %>% sum(), numeric(1))

    Stock@D <- rep(sum(N_now2 * Mat_age * Wt_age[, nyears])/sum(N_virg2 * Mat_age * Wt_age[, 1]), 2)
  }

  # Placeholders, Fdisc is in Fleet cpars
  Stock@Ksd <- Stock@Linfsd <- Stock@Msd <- c(0, 0)
  Stock@L50 <- Stock@L50_95 <- c(0, 0)
  Stock@Fdisc <- c(0, 0)

  fleet_output <- lapply(seq_len(replist$nfleets)[replist$IsFishFleet], SS_fleet, i = i, replist = replist,
                         Stock = Stock, mainyrs = mainyrs, nyears = nyears, proyears = proyears, nsim = nsim,
                         single_sex = single_sex, partition = partition, cpars_bio = cpars_bio, age_M = age_M)
  
  Fleet <- lapply(fleet_output, getElement, "Fleet") %>% structure(names = replist$FleetNames[replist$IsFishFleet])
  cpars <- lapply(fleet_output, function(x) c(cpars_bio, x$cpars_fleet)) %>% structure(names = replist$FleetNames[replist$IsFishFleet])

  return(list(Stock = Stock, Fleet = Fleet, cpars = cpars))
}

SS_fleet <- function(ff, i, replist, Stock, mainyrs, nyears, proyears, nsim, single_sex = TRUE,
                     partition = 2, cpars_bio, age_M = NULL) {

  if(!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package `reshape2` is required for this function. Install with `install.packages('reshape2')`", call. = FALSE)
  }

  ret_num <- NULL # dummy for CRAN
  Factor <- Seas <- Morph <- Use <- SE <- Obs <- Nsamp_adj <- NULL # variable declaration for binding check

  allyears <- nyears + proyears
  n_age <- Stock@maxage + 1

  #### Selectivity (Asel2 incorporates time-varying length selectivity, Asel age-based assumed constant)
  V <- get_V_from_Asel2(ff, i, replist, mainyrs, Stock@maxage)
  V_proj <- array(V[,ncol(V)], dim=c(Stock@maxage+1, proyears))
  Vout <- cbind(V, V_proj) %>% array(c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  #### Retention and selectivity-at-length - loop over years for time-varying quantities

  loop_over_change_points <- function(yy, df) {
    yy <- ifelse(yy < mainyrs[1], min(df$Yr), yy) # Check to avoid infinite loop
    sched <- df[findInterval(yy, df$Yr), ]
    if(nrow(sched) == 1) {
      return(sched[1, -c(1:5)] %>% unlist())
    } else {
      stop("Could not find selectivity/retention function for year: ", yy)
    }
  }
  retL <- vapply(mainyrs, loop_over_change_points, numeric(replist$nlbinspop),
                 df = replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                          replist$sizeselex$Factor == "Keep", ])
  if(proyears) {
    retLpro <- retL[, nyears] %>% matrix(nrow(retL), proyears)
    retL <- cbind(retL, retLpro)
  }

  SLarray <- vapply(mainyrs, loop_over_change_points, numeric(replist$nlbinspop),
                    df = replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                             replist$sizeselex$Factor == "Dead", ])
  if(proyears) {
    SLarraypro <- SLarray[, nyears] %>% matrix(nrow(SLarray), proyears)
    SLarray <- cbind(SLarray, SLarraypro)
  }

  #### Discard mortality - Fdisc_array2
  Fdisc2 <- vapply(mainyrs, loop_over_change_points, numeric(replist$nlbinspop),
                   df = replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                            replist$sizeselex$Factor == "Mort", ])
  
  if(proyears) {
    Fdisc2pro <- Fdisc2[, nyears] %>% matrix(nrow(Fdisc2), proyears)
    Fdisc2 <- cbind(Fdisc2, Fdisc2pro)
  }
  
  #### Apical F
  FF <- replist$exploitation[, match(replist$FleetNames[ff], colnames(replist$exploitation))] %>%
    aggregate(by = list(Yr = replist$exploitation$Yr), mean) 
  Find <- FF$x[FF$Yr %in% mainyrs]

  if (length(Find)<1 || all(is.na(Find))) {
    # above method more accurate (for swordfish at least)
    FF <- FF[match(mainyrs, FF$Yr), ]
    F2 <- vapply(0:Stock@maxage, function(x) FF[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))
    Find <- apply(F2, 1, max)
  }

  #### Sex-specific catches: predicted retained catch for fleet ff for stock (sex) i
  # Also retention and discard mortality at age
  ALK_dim_match <- paste0("Seas: 1 Sub_Seas: 2 Morph: ", i) %in% dimnames(replist$ALK)[[3]] %>% any()
  if(ALK_dim_match) {
    ALK <- replist$ALK[, , paste0("Seas: 1 Sub_Seas: 2 Morph: ", i)]
  } else {
    ALK <- replist$ALK[, , paste0("Seas: 1 Morph: ", (i - 1) * replist$nseasons + 1)]
  }
  if (all(!is.na(replist$lbinspop))) {
    ALK <- ALK[match(replist$lbinspop, dimnames(ALK)$Length), match(0:Stock@maxage, dimnames(ALK)$TrueAge)]
  } else {
    lbinspop <- sort(as.numeric(dimnames(ALK)$Length))
    ALK <- ALK[match(lbinspop, dimnames(ALK)$Length), match(0:Stock@maxage, dimnames(ALK)$TrueAge)]
  }

  wt <- dplyr::filter(replist$ageselex, Fleet == ff, Sex == i, Factor == "bodywt", Seas == 1)
  wt_morphs <- wt$Morph %>% unique()
  if(length(wt_morphs) > 1) wt <- dplyr::filter(wt, Morph == wt_morphs[1])
  wt <- vapply(0:Stock@maxage, function(x) wt[match(mainyrs, wt$Yr), parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))

  meanN <- dplyr::filter(replist$natage, Sex == i, `Beg/Mid` == "M", Seas == 1)
  meanN <- meanN[match(mainyrs, meanN$Yr), ]
  meanN <- vapply(0:Stock@maxage, function(x) meanN[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))

  retA <- Fdisc1 <- matrix(NA, nrow=n_age, ncol=nyears)
  Cat <- numeric(nyears)
  for(yy in 1:nyears) {
    retA[,yy] <- colSums(retL[, yy] * ALK) # realized retention at-age
    if(!is.numeric(retA[n_age,yy])) retA[n_age,yy] <- retA[n_age-1]
    Cat[yy] <- sum(meanN[yy, ] * Find[yy] * wt[yy, ] * retA[,yy] * V[, yy])
    
    Fdisc1[,yy] <- colSums(Fdisc2[,yy] * ALK) # realized discard mortality at age
    if(!is.numeric(Fdisc1[n_age,yy])) Fdisc1[n_age,yy] <- Fdisc1[n_age-1]
    
    # back-calculate retention probability curve
    retA[,yy] <- retA[,yy]/V[,yy]
    isZero <- which(V[,yy]==0)
    if (length(isZero)>0) retA[isZero,yy] <- 0
  }

  retA_proj <- array(retA[,ncol(retA)], dim=c(n_age, proyears))
  retAout <- cbind(retA, retA_proj) %>% array(c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  retAout[retAout < 0] <- 0
  retAout[retAout > 1] <- 1
  
  Fdisc1[Fdisc1 < 0] <- 0
  Fdisc1[Fdisc1 > 1] <- 1
  Fdisc1_proj <- array(Fdisc1[, ncol(Fdisc1)], dim = c(n_age, proyears))
  Fdisc1 <- cbind(Fdisc1, Fdisc1_proj) %>% array(c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  
  # no discard mortality if fleet no longer in operation
  retN <- replist$catch %>% dplyr::filter(Fleet==ff, Yr==max(mainyrs)) %>% dplyr::select(N=ret_num)
  if (!ncol(retN) || all(retN$N <= 0) || !all(is.finite(Fdisc2))) {
    Fdisc1[, , nyears + 1:proyears] <- Fdisc2[, nyears + 1:proyears] <- 0
  } 
  Fdisc <- mean(Fdisc2[, 1:nyears], na.rm = TRUE)
 
  # ---- empirical weight-at-age for catches ----
  if (!methods::is(replist$wtatage, 'logical')) {
    wt_at_age_c_df <- replist$wtatage %>% dplyr::filter(abs(Yr) %in% mainyrs, Sex==i, Fleet==ff)
    
    if(nrow(wt_at_age_c_df)) {
      
      wt_at_age_c <- local({
        sel_cols <- which(colnames(wt_at_age_c_df)=='0'):ncol(wt_at_age_c_df)
        wt_at_age_c <- wt_at_age_c_df[,sel_cols]
        wt_at_age_c <- aggregate(wt_at_age_c, by = list(Yr = wt_at_age_c_df$Yr), mean) # Mean over seasons and morphs
        wt_at_age_c <- wt_at_age_c[, -1] %>% t()
        lst <- wt_at_age_c[, ncol(wt_at_age_c)]
        wt_at_age_c <- cbind(wt_at_age_c, replicate(proyears, lst))
        wt_at_age_c <- replicate(nsim, wt_at_age_c)
        aperm(wt_at_age_c, c(3,1,2))
      })
      
    } else {
      wt_at_age_c <- NULL
    }
  } else {
    wt_at_age_c <- NULL
  }

  #### Fleet object
  Fleet <- new("Fleet")
  Fleet@Name <- replist$FleetNames[ff]
  Fleet@nyears <- length(mainyrs)
  Fleet@DR <- rep(0, 2) # Should be zero since we have retention in cpars$retA and cpars$retL
  Fleet@Spat_targ <- rep(1, 2)
  Fleet@EffYears <- 1:nyears
  Fleet@EffLower <- Fleet@EffUpper <- Find
  Fleet@Esd <- Fleet@qinc <- Fleet@qcv <- rep(0, 2)
  Fleet@L5 <- Fleet@LFS <- Fleet@Vmaxlen <- rep(0, 2)
  Fleet@LR5 <- Fleet@LFR <- Fleet@Rmaxlen <- rep(0, 2)
  Fleet@isRel <- "FALSE"
  Fleet@MPA <- FALSE
  Fleet@CurrentYr <- max(mainyrs)

  #### cpars
  cpars_fleet <- list()
  
  # CAL_bins
  upper_boundary_last_bin <- max(replist$lbinspop) +
    2 * (suppressWarnings(max(as.numeric(colnames(replist$sizeselex)), na.rm = TRUE)) - max(replist$lbinspop))
  cpars_fleet$CAL_bins <- c(replist$lbinspop, upper_boundary_last_bin)
  if(!all(is.finite(cpars_fleet$CAL_bins))) {
    # alternative method to get length bins
    cpars_fleet$CAL_binsmid <- colnames(replist$sizeselex[,6:ncol(replist$sizeselex)]) %>% as.numeric()
    by <- cpars_fleet$CAL_binsmid[2] - cpars_fleet$CAL_binsmid[1]
    cpars_fleet$CAL_bins <- seq(cpars_fleet$CAL_binsmid[1]-0.5*by, by=by, length.out=length(cpars_fleet$CAL_binsmid)+1)
  } else {
    by <- cpars_fleet$CAL_bins[2] - cpars_fleet$CAL_bins[1]
    cpars_fleet$CAL_binsmid <- seq(cpars_fleet$CAL_bins[1]+0.5*by, by=by, length.out=length(cpars_fleet$CAL_bins)-1)
  }
  
  cpars_fleet$Fdisc <- rep(Fdisc, nsim)
  cpars_fleet$Fdisc_array1 <- Fdisc1
  cpars_fleet$Fdisc_array2 <- replicate(nsim, Fdisc2) %>% aperm(c(3, 1, 2))
  cpars_fleet$qs <- rep(1, nsim)
  cpars_fleet$V <- Vout
  cpars_fleet$retA <- retAout
  cpars_fleet$retL <- replicate(nsim, retL) %>% aperm(c(3, 1, 2))
  cpars_fleet$SLarray <- replicate(nsim, SLarray) %>% aperm(c(3, 1, 2))
  cpars_fleet$Find <- Find %>% matrix(nsim, length(mainyrs), byrow = TRUE)

  cpars_fleet$Wt_age_C <- wt_at_age_c

  #### Data object
  cpars_fleet$Data <- new("Data")
  cpars_fleet$Data@Year <- mainyrs
  cpars_fleet$Data@Cat <- matrix(Cat, nrow = 1)
  cpars_fleet$Data@CV_Cat <- matrix(0.2, nrow = 1, ncol = ncol(cpars_fleet$Data@Cat)) # Default value

  cpars_fleet$Data@Name <- paste0(Stock@Name, ", ", replist$FleetNames[ff])
  cpars_fleet$Data@LHYear <- max(mainyrs)
  cpars_fleet$Data@nareas <- replist$nareas

  # Biological and fleet parameters
  cpars_fleet$Data@MaxAge <- Stock@maxage
  if(is.null(age_M)) age_M <- 0:Stock@maxage
  cpars_fleet$Data@Mort <- cpars_bio$M_ageArray[1, age_M + 1, nyears] %>% mean()

  GP <- replist$Growth_Parameters[i, ]
  if(all(is.na(GP))) GP <- replist$Growth_Parameters[1, ]
  cpars_fleet$Data@vbLinf <- GP$Linf
  cpars_fleet$Data@vbK <- GP$K
  cpars_fleet$Data@vbt0 <- GP$A_a_L0
  cpars_fleet$Data@wla <- GP$WtLen1
  cpars_fleet$Data@wlb <- GP$WtLen2

  cpars_fleet$Data@steep <- unique(Stock@h)
  cpars_fleet$Data@sigmaR <- unique(Stock@Perr)

  if (max(cpars_bio$Mat_age[1,,nyears])>0.5 && !all(cpars_bio$Mat_age[1,,nyears]==1)) {
    cpars_fleet$Data@L50 <- LinInterp(cpars_bio$Mat_age[1,,nyears], cpars_bio$Len_age[1,,nyears], 0.5 + 1e-4)
    if (max(cpars_bio$Mat_age[1,,nyears])>=0.95) {
      cpars_fleet$Data@L95 <- LinInterp(cpars_bio$Mat_age[1,,nyears], cpars_bio$Len_age[1,,nyears], 0.95)  
    } else {
      cpars_fleet$Data@L95 <- max(cpars_bio$Mat_age[1,,nyears])
    }  
  }
  
  cpars_fleet$Data@LenCV <- GP$CVmax

  vrel <- V[,nyears]/max(V[,nyears], na.rm=T)
  
  if (min(vrel[1:which.max(vrel)])<0.05) {
    cpars_fleet$Data@LFC <- LinInterp(vrel, cpars_bio$Len_age[1,,nyears], 0.05, ascending=TRUE)  
  } 
  if (max(vrel)>0.999) {
    cpars_fleet$Data@LFS <- LinInterp(vrel, cpars_bio$Len_age[1,,nyears], 0.999, ascending=TRUE)
  } 
  
  cpars_fleet$Data@Vmaxlen <- V[n_age, nyears]

  cpars_fleet$Data@Dep <- unique(Stock@D)

  # In 3.24, SpawnBio = NA for seasons 1 - 3 (out of 4). I decide to take the mean
  ts_Yr <- which(replist$timeseries$Yr == max(mainyrs))
  cpars_fleet$Data@SpAbun <- replist$timeseries$SpawnBio[ts_Yr] %>% mean(na.rm = TRUE)
  cpars_fleet$Data@Abun <- parse(text = paste0("replist$timeseries$`sel(B):_", ff, "`[ts_Yr]")) %>% eval() %>% mean(na.rm = TRUE)

  cpars_fleet$Data@FMSY_M <- replist$derived_quants$Value[replist$derived_quants$Label == "Fstd_MSY"]/cpars_fleet$Data@Mort

  BMSY_B0 <- replist$derived_quants$Value[replist$derived_quants$Label == "B_MSY/SSB_unfished"]
  if(!length(BMSY_B0)) {
    BMSY_B0 <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"]/
      replist$derived_quants$Value[replist$derived_quants$Label == "SSB_Unfished"]
  }
  cpars_fleet$Data@BMSY_B0 <- BMSY_B0

  Cref <- replist$derived_quants$Value[replist$derived_quants$Label == "Ret_Catch_MSY"]
  if(!length(Cref)) replist$derived_quants$Value[replist$derived_quants$Label == "RetYield_MSY"]
  cpars_fleet$Data@Cref <- Cref

  cpars_fleet$Data@t <- length(mainyrs)
  cpars_fleet$Data@AvC <- mean(Cat, na.rm = TRUE)
  cpars_fleet$Data@Dt <- replist$derived_quants$Value[replist$derived_quants$Label == paste0("SSB_", mainyrs[nyears])]/
    replist$derived_quants$Value[replist$derived_quants$Label == paste0("SSB_", mainyrs[1])]

  if(single_sex) { # Add Index if model is a single sex population

    get_index <- function(ff) { # Get index function

      Ind <- replist$cpue %>% dplyr::filter(Fleet == ff)
      if(!is.null(Ind$Use)) Ind <- dplyr::filter(Ind, Use == 1)
      Ind <- Ind %>% group_by(Yr) %>%
        summarise(Obs = mean(Obs, na.rm = TRUE), SE = mean(SE, na.rm = TRUE))
      if(nrow(Ind) > 0) {
        Obs <- Ind$Obs[match(mainyrs, Ind$Yr)] %>% matrix(1)
        CV <- sqrt(exp(Ind$SE[match(mainyrs, Ind$Yr)]^2) - 1) %>% matrix(1)
      } else {
        Obs <- matrix(NA_real_, 1, 1)
        CV <- matrix(0.2, 1, 1)
      }
      return(list(Obs = Obs, CV = CV))
    }

    Index <- get_index(ff) # Add fleet CPUE
    cpars_fleet$Data@VInd <- Index$Obs
    cpars_fleet$Data@CV_VInd <- Index$CV

    if(ff == 1) { # Add extra surveys
      survey_ind <- which(!replist$IsFishFleet)
      if(length(survey_ind) > 0) {
        cpars_fleet$Data@AddInd <- lapply(survey_ind, get_index) %>% lapply(getElement, "Obs") %>% unlist() %>%
          array(c(nyears, length(survey_ind), 1)) %>% aperm(3:1)

        cpars_fleet$Data@CV_AddInd <- lapply(survey_ind, get_index) %>% lapply(getElement, "CV") %>% unlist() %>%
          array(c(nyears, length(survey_ind), 1)) %>% aperm(3:1)

        cpars_fleet$Data@AddIndV <- lapply(survey_ind, get_V_from_Asel2, i = 1, replist = replist, mainyrs = mainyrs,
                                           maxage = Stock@maxage,rescale = TRUE) %>% lapply(function(x) x[, nyears]) %>%
          unlist() %>% array(c(n_age, length(survey_ind), 1)) %>% aperm(3:1)

        cpars_fleet$Data@AddIndType <- rep(1, length(survey_ind))

        cpars_fleet$Data@AddIunits <- replist$survey_units[survey_ind]
        cpars_fleet$Data@AddIunits[cpars_fleet$Data@AddIunits > 1] <- 0
      }
    }
  }

  CAA <- lapply(partition, function(x) {
    replist$agedbase[replist$agedbase$Fleet == ff & replist$agedbase$Sex == i &
                       replist$agedbase$Used == "yes" & replist$agedbase$Part == x, ]
  })
  CAA <- do.call(rbind, CAA)

  if (!is.null(CAA)) {
    if(nrow(CAA) > 0) {
      CAA <- CAA %>% dplyr::mutate(Nout = Obs * Nsamp_adj) %>% reshape2::acast(list("Yr", "Bin"), value.var = "Nout", fun.aggregate = sum)

      CAAout <- matrix(NA, nyears, n_age)
      CAAout[match(as.numeric(rownames(CAA)), mainyrs), match(as.numeric(colnames(CAA)), 0:Stock@maxage)] <- CAA
      CAA2 <- apply(CAAout, 1, function(x) {x[is.na(x)] <- 0; return(x)})
      cpars_fleet$Data@CAA <- CAA %>% array(c(n_age, nyears, 1)) %>% aperm(3:1)
    }

  }

  CAL <- lapply(partition, function(x) {
    replist$lendbase[replist$lendbase$Fleet == ff & replist$lendbase$Sex == i &
                       replist$lendbase$Used == "yes" & replist$lendbase$Part == x, ] # Retained CAL
  })
  CAL <- do.call(rbind, CAL)
  if (!is.null(CAL)) {
    if(nrow(CAL) > 0) {
      cpars_fleet$Data@CAL_bins <- cpars_fleet$CAL_bins
      binWidth <- cpars_fleet$CAL_bins[2:length(cpars_fleet$CAL_bins)] - cpars_fleet$CAL_bins[2:length(cpars_fleet$CAL_bins) - 1]
      cpars_fleet$Data@CAL_mids <- cpars_fleet$CAL_bins[2:length(cpars_fleet$CAL_bins)] - 0.5 * binWidth

      CAL <- CAL %>% dplyr::mutate(Nout = Obs * Nsamp_adj) %>% reshape2::acast(list("Yr", "Bin"), value.var = "Nout", fun.aggregate = sum)

      CALout <- matrix(NA_real_, nyears, length(cpars_fleet$Data@CAL_mids))
      CALout[mainyrs %in% as.numeric(rownames(CAL)) %in% mainyrs,
             cpars_fleet$CAL_bins[-length(cpars_fleet$CAL_bins)] %in% as.numeric(colnames(CAL))] <- CAL
      CAL2 <- apply(CALout, 1, function(x) {
        if(!all(is.na(x))) x[is.na(x)] <- 0
        return(x)
      })
      cpars_fleet$Data@CAL <- CAL2 %>% array(c(length(cpars_fleet$Data@CAL_mids), nyears, 1)) %>% aperm(3:1)

      cpars_fleet$Data@ML <- apply(CAL2, 2, function(xx) weighted.mean(x = cpars_fleet$Data@CAL_mids, w = xx)) %>% matrix(1)
    }
  }

  return(list(Fleet = Fleet, cpars_fleet = cpars_fleet))
}

# #' Perr_hist A matrix with nsim years and nyear rows
# #' proyears Integer, number of projection years
# #' procsd Standard deviation, can be a vector of length 1 or nsim
# #' AC Autocorrelation, can be a vector of length 1 or nsim
# #' seed Integer for random number generator
sample_recruitment <- function(Perr_hist, proyears, procsd, AC, seed) {
  if(!missing(seed)) set.seed(seed)
  nsim <- nrow(Perr_hist)
  if(length(procsd) == 1) procsd <- rep(procsd, nsim)
  if(length(AC) == 1) AC <- rep(AC, nsim)
  procmu <- -0.5 * procsd^2 * (1 - AC)/sqrt(1 - AC^2) # adjusted log normal mean with AC
  Perr_delta <- rnorm(nsim * proyears, procmu, procsd) %>%
    matrix(nrow = nsim, ncol = proyears) # Sample recruitment for projection
  Perr_proj <- matrix(NA_real_, nsim, proyears)

  # Add autocorrelation to projection recruitment
  Perr_proj[, 1] <- AC * Perr_hist[, ncol(Perr_hist)] + Perr_delta[, 1] * sqrt(1 - AC^2)
  for(y in 2:ncol(Perr_proj)) Perr_proj[, y] <- AC * Perr_proj[, y-1] + Perr_delta[, y] * sqrt(1 - AC^2)

  return(Perr_proj)
}

get_V_from_Asel2 <- function(ff, i, replist, mainyrs, maxage, rescale = FALSE) {
  Fleet <- Factor <- NULL # checks
  # Note for surveys in 3.24 Asel2 does not exist
  Asel2 <- dplyr::filter(replist$ageselex, Fleet == ff, Sex %in% i, Factor == "Asel2", Yr %in% mainyrs)
  V2 <- vapply(0:maxage, function(x) {
    # Grab Asel2 for each age and average across morphs and seasons within year. Also sex if i is a vector
    out <- Asel2[, parse(text = paste0("\"", x, "\"")) %>% eval()]
    if(length(out) > length(mainyrs)) {
      return(aggregate(out, by = list(Yr = Asel2$Yr), mean) %>% getElement("x"))
    } else if(!nrow(Asel2)) {
      return(rep(1, length(mainyrs)))
    } else {
      return(out)
    }
  }, numeric(length(mainyrs))) %>% t()

  # # Assume Asel is time-invariant
  # don't think this is necessary
  # Asel <- dplyr::filter(replist$ageselex, Fleet == ff, Sex %in% i, Factor == "Asel")
  # V <- vapply(0:maxage, function(x) Asel[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
  # Vout <- V2 * V
  Vout <- V2

  if(rescale) {
    Vapical <- apply(Vout, 2, max) %>% matrix(nrow(Vout), ncol(Vout), byrow = TRUE)
    Vout <- Vout/Vapical
  }
  return(Vout)
}


SS_seasonalyears_to_annual <- function(OM, SSdir, ...) {
  age <- NULL # binding check
  if(is.list(SSdir)) {
    replist <- SSdir
  } else {
    replist <- SS_import(SSdir, silent=F, ...)
  }

  nseas <- 1/replist$seasdurations
  mainyrs <- replist$startyr:replist$endyr
  year_frac <- data.frame(mainyrs = mainyrs, seas = rep(1:nseas, length(mainyrs)/nseas),
                          true_year = rep(1:(length(mainyrs)/nseas), each = nseas))

  age_frac <- data.frame(age = 0:OM@maxage) %>% mutate(true_age = floor(age/nseas))

  OM2 <- OM
  OM2@nyears <- OM2@CurrentYr <- max(year_frac$true_year)
  OM2@maxage <- max(age_frac$true_age)

  avg <- c("M_ageArray", "Wt_age", "Len_age", "LatASD", "Mat_age", "V", "SLarray", "retL", "Fdisc")
  OM2@cpars[match(avg, names(OM2@cpars))] <-
    lapply(avg, function(xx) OM@cpars[[xx]] %>%
             cpars_season(FUN = mean, year_frac = year_frac, age_frac = age_frac, proyears = OM2@proyears))

  OM2@cpars$M_ageArray <- OM2@cpars$M_ageArray * nseas

  if(!is.null(OM2@cpars$V)) {
    OM2@cpars$V <- apply(OM2@cpars$V, c(1, 3), function(x) x/max(x)) %>% aperm(c(2, 1, 3))
  }
  if(!is.null(OM2@cpars$SLarray)) {
    OM2@cpars$SLarray <- apply(OM2@cpars$SLarray, c(1, 3), function(x) x/max(x)) %>% aperm(c(2, 1, 3))
  }
  OM2@cpars$Find <- cpars_season_Find(OM@cpars$Find, year_frac)
  OM2@EffYears <- year_frac$true_year
  OM2@EffLower <- OM2@EffUpper <- OM2@cpars$Find[1, ]

  OM2@cpars$Perr_y <- cpars_season_Perr(OM@cpars$Perr_y, year_frac, age_frac, OM2@proyears)
  if(!is.null(OM@cpars$mov)) {
    OM2@cpars$mov <- cpars_season_mov(OM@cpars$mov, age_frac)
  }
  OM2@R0 <- OM@R0 * nseas

  return(OM2)
}

cpars_season <- function(x, FUN, year_frac, age_frac, proyears) {
  nsim <- dim(x)[1]
  nyears <- max(year_frac$true_year)
  n_age <- length(unique(age_frac$true_age))

  hist_x <- x[1, , 1:nrow(year_frac)] %>%
    apply(2, function(xx) aggregate(xx, by = list(Age = age_frac$true_age), FUN)$x) %>%
    apply(1, function(xx) aggregate(xx, by = list(Yr = year_frac$true_year), FUN)$x)

  proj_x <- hist_x[nrow(hist_x), ] %>% matrix(proyears, n_age, byrow = TRUE)

  rbind(hist_x, proj_x) %>% array(c(nyears + proyears, n_age, nsim)) %>% aperm(3:1)
}

cpars_season_Find <- function(x, year_frac) {
  aggregate(x[1, ], by = list(Yr = year_frac$true_year), mean) %>% getElement("x") %>%
    matrix(nrow(x), max(year_frac$true_year), byrow = TRUE)
}

cpars_season_Perr <- function(x, year_frac, age_frac, proyears) {
  init_p <- x[, 2:nrow(age_frac)-1] %>%
    apply(1, function(xx) aggregate(xx, by = list(Age = age_frac$true_age[2:nrow(age_frac)-1]), mean)$x)

  hist_p <- x[, nrow(age_frac):(nrow(age_frac) + nrow(year_frac) - 1)]  %>%
    apply(1, function(xx) aggregate(xx, by = list(Yr = year_frac$true_year), mean)$x)

  rbind(init_p, hist_p) %>% t() %>% cbind(x[, (nrow(age_frac) + nrow(year_frac)):ncol(x)])
}

cpars_season_mov <- function(x, age_frac) {
  mov <- apply(x, c(1, 3, 4), function(xx) aggregate(xx, by = list(Yr = age_frac$true_age), mean)$x) %>%
    aperm(c(2, 1, 3, 4))
  #mov <- apply(mov, c(1, 2), function(xx) apply(xx, 1, function(xxx) xxx/sum(xxx)))
  return(mov)
}
