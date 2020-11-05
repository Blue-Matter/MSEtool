
SS_import <- function(SSdir, silent = FALSE, ...) {
  if(!requireNamespace("r4ss", quietly = TRUE)) {
    stop("Download the r4ss package to use this function. It is recommended to install the Github version with: devtools::install_github(\"r4ss/r4ss\")", call. = FALSE)
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

SS_steepness <- function(replist, mainyrs) {

  ###### Steepness
  if(replist$SRRtype == 3 || replist$SRRtype == 6) { # Beverton-Holt SR
    SRrel <- 1L
    h <- replist$parameters[grepl("steep", rownames(replist$parameters)), ]$Value
  } else if(replist$SRRtype == 2) {
    SRrel <- 2L
    h <- replist$parameters[grepl("SR_Ricker", rownames(replist$parameters)), ]$Value
  } else if(replist$SRRtype == 7) {
    OM@SRrel <- 1L

    s_frac <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Sfrac"]
    Beta <- replist$parameters$Value[replist$parameters$Label == "SR_surv_Beta"]

    s0 <- 1/SpR0
    z0 <- -log(s0)
    z_min <- z0 * (1 - s_frac)

    h <- 0.2 * exp(z0 * s_frac * (1 - 0.2 ^ Beta))

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

    h <- SRopt(1, SSB, rec, SpR0, plot = FALSE, type = ifelse(SR == 1, "BH", "Ricker"))
  }
  return(list(SRrel = SRrel, h = h))
}



ilogit <- function(x) 1/(1 + exp(-x))

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


# #' Extracts growth parameters from a SS3 r4ss replist
# #'
# #' @param replist the list output of the r4ss SS_output function (a list of assessment inputs / outputs)
# #' @param seas The reference season for the growth (not actually sure what this does yet)
# #' @author T. Carruthers
# #' @export getGpars
getGpars <- function(replist, seas = 1) { # This is a rip-off of SSPlotBiology

  if(packageVersion("r4ss") == 1.24) {
    res <- getGpars_r4ss_124(replist, seas)
  } else res <- getGpars_r4ss_134(replist, seas)
  #if(nrow(res) == 0) warning("No growth parameters were retrieved from r4ss output.")
  return(res)
}

getGpars_r4ss_124 <- function(replist, seas = 1) {

  nseasons <- replist$nseasons
  growdat <- replist$endgrowth[replist$endgrowth$Seas == seas, ]
  growdat$CV_Beg <- growdat$SD_Beg/growdat$Len_Beg
  growthCVtype <- replist$growthCVtype
  biology <- replist$biology
  startyr <- replist$startyr
  FecType <- replist$FecType
  FecPar1name <- replist$FecPar1name
  FecPar2name <- replist$FecPar2name
  FecPar1 <- replist$FecPar1
  FecPar2 <- replist$FecPar2
  parameters <- replist$parameters
  nsexes <- replist$nsexes
  mainmorphs <- replist$mainmorphs
  accuage <- replist$accuage
  startyr <- replist$startyr
  endyr <- replist$endyr
  growthvaries <- replist$growthvaries
  growthseries <- replist$growthseries
  ageselex <- replist$ageselex
  MGparmAdj <- replist$MGparmAdj
  wtatage <- replist$wtatage
  Growth_Parameters <- replist$Growth_Parameters
  Grow_std <- replist$derived_quants[grep("Grow_std_", replist$derived_quants$LABEL), ]
  if (nrow(Grow_std) == 0) {
    Grow_std <- NULL
  }  else {
    Grow_std$pattern <- NA
    Grow_std$sex_char <- NA
    Grow_std$sex <- NA
    Grow_std$age <- NA
    for (irow in 1:nrow(Grow_std)) {
      tmp <- strsplit(Grow_std$LABEL[irow], split = "_")[[1]]
      Grow_std$pattern[irow] <- as.numeric(tmp[3])
      Grow_std$sex_char[irow] <- tmp[4]
      Grow_std$age[irow] <- as.numeric(tmp[6])
    }
    Grow_std$sex[Grow_std$sex_char == "Fem"] <- 1
    Grow_std$sex[Grow_std$sex_char == "Mal"] <- 2
  }
  if (!is.null(replist$wtatage_switch)) {
    wtatage_switch <- replist$wtatage_switch
  } else{ stop("SSplotBiology function doesn't match SS_output function. Update one or both functions.")
  }
  if (wtatage_switch)
    cat("Note: this model uses the empirical weight-at-age input.\n",
        "     Therefore many of the parametric biology quantities which are plotted\n",
        "     are not used in the model.\n")
  if (!seas %in% 1:nseasons)
    stop("'seas' input should be within 1:nseasons")

  if (length(mainmorphs) > nsexes) {
    cat("!Error with morph indexing in SSplotBiology function.\n",
        " Code is not set up to handle multiple growth patterns or birth seasons.\n")
  }
  #if (FecType == 1) {
  #  fec_ylab <- "Eggs per kg"
  #  FecX <- biology$Wt_len_F
  #  FecY <- FecPar1 + FecPar2 * FecX
  #}

  growdatF <- growdat[growdat$Gender == 1 & growdat$Morph ==
                        mainmorphs[1], ]
  growdatF$Sd_Size <- growdatF$SD_Beg

  if (growthCVtype == "logSD=f(A)") {
    growdatF$high <- qlnorm(0.975, meanlog = log(growdatF$Len_Beg),
                            sdlog = growdatF$Sd_Size)
    growdatF$low <- qlnorm(0.025, meanlog = log(growdatF$Len_Beg),
                           sdlog = growdatF$Sd_Size)
  }  else {
    growdatF$high <- qnorm(0.975, mean = growdatF$Len_Beg,
                           sd = growdatF$Sd_Size)
    growdatF$low <- qnorm(0.025, mean = growdatF$Len_Beg,
                          sd = growdatF$Sd_Size)
  }
  if (nsexes > 1) {
    growdatM <- growdat[growdat$Gender == 2 & growdat$Morph ==
                          mainmorphs[2], ]
    xm <- growdatM$Age_Beg
    growdatM$Sd_Size <- growdatM$SD_Beg
    if (growthCVtype == "logSD=f(A)") {
      growdatM$high <- qlnorm(0.975, meanlog = log(growdatM$Len_Beg),
                              sdlog = growdatM$Sd_Size)
      growdatM$low <- qlnorm(0.025, meanlog = log(growdatM$Len_Beg),
                             sdlog = growdatM$Sd_Size)
    }    else {
      growdatM$high <- qnorm(0.975, mean = growdatM$Len_Beg,
                             sd = growdatM$Sd_Size)
      growdatM$low <- qnorm(0.025, mean = growdatM$Len_Beg,
                            sd = growdatM$Sd_Size)
    }
  } else growdatM <- NULL

  list(Female = growdatF, Male = growdatM)

}

getGpars_r4ss_134 <- function(replist, seas = 1) {
  nseasons <- replist$nseasons
  growdat <- replist$endgrowth[replist$endgrowth$Seas == seas,
  ]
  growdat$CV_Beg <- growdat$SD_Beg/growdat$Len_Beg
  growthCVtype <- replist$growthCVtype
  biology <- replist$biology
  startyr <- replist$startyr
  FecType <- replist$FecType
  FecPar1name <- replist$FecPar1name
  FecPar2name <- replist$FecPar2name
  FecPar1 <- replist$FecPar1
  FecPar2 <- replist$FecPar2
  parameters <- replist$parameters
  nsexes <- replist$nsexes
  accuage <- replist$accuage
  startyr <- replist$startyr
  endyr <- replist$endyr
  growthvaries <- replist$growthvaries
  growthseries <- replist$growthseries
  ageselex <- replist$ageselex
  MGparmAdj <- replist$MGparmAdj
  wtatage <- replist$wtatage
  if ("comment" %in% names(wtatage)) {
    wtatage <- wtatage[, -grep("comment", names(wtatage))]
  }
  M_at_age <- replist$M_at_age
  Growth_Parameters <- replist$Growth_Parameters
  #if (is.null(morphs)) {
  morphs <- replist$mainmorphs
  #}
  Grow_std <- replist$derived_quants[grep("Grow_std_", replist$derived_quants$Label), ]
  if (nrow(Grow_std) == 0) {
    Grow_std <- NULL
  } else {
    Grow_std$pattern <- NA
    Grow_std$sex_char <- NA
    Grow_std$sex <- NA
    Grow_std$age <- NA
    for (irow in 1:nrow(Grow_std)) {
      tmp <- strsplit(Grow_std$Label[irow], split = "_")[[1]]
      Grow_std$pattern[irow] <- as.numeric(tmp[3])
      Grow_std$sex_char[irow] <- tmp[4]
      Grow_std$age[irow] <- as.numeric(tmp[6])
    }
    Grow_std$sex[Grow_std$sex_char == "Fem"] <- 1
    Grow_std$sex[Grow_std$sex_char == "Mal"] <- 2
  }
  if (!is.null(replist$wtatage_switch)) {
    wtatage_switch <- replist$wtatage_switch
  } else {
    stop("SSplotBiology function doesn't match SS_output function.",
         "Update one or both functions.")
  }
  #if (wtatage_switch) {
  #  cat("Note: this model uses the empirical weight-at-age input.\n",
  #      "      Plots of many quantities related to growth are skipped.\n")
  #}
  if (!seas %in% 1:nseasons)
    stop("'seas' input should be within 1:nseasons")
  #if (nseasons > 1) {
  #  labels[6] <- gsub("beginning of the year", paste("beginning of season",
  #                                                   seas), labels[6])
  #}

  if (length(morphs) > nsexes) {
    cat("!Error with morph indexing in SSplotBiology function.\n",
        " Code is not set up to handle multiple growth patterns or birth seasons.\n")
  }
  #if (FecType == 1) {
  #  fec_ylab <- "Eggs per kg"
  #  fec_xlab <- labels[8]
  #  FecX <- biology$Wt_len_F
  #  FecY <- FecPar1 + FecPar2 * FecX
  #}
  #if (labels[11] != "Default fecundity label")
  #  fec_ylab <- labels[11]
  growdatF <- growdat[growdat$Sex == 1 & growdat$Morph == morphs[1], ]
  growdatF$Sd_Size <- growdatF$SD_Beg
  if (growthCVtype == "logSD=f(A)") {
    growdatF$high <- qlnorm(0.975, meanlog = log(growdatF$Len_Beg),
                            sdlog = growdatF$Sd_Size)
    growdatF$low <- qlnorm(0.025, meanlog = log(growdatF$Len_Beg),
                           sdlog = growdatF$Sd_Size)
  } else {
    growdatF$high <- qnorm(0.975, mean = growdatF$Len_Beg,
                           sd = growdatF$Sd_Size)
    growdatF$low <- qnorm(0.025, mean = growdatF$Len_Beg,
                          sd = growdatF$Sd_Size)
  }
  if (nsexes > 1) {
    growdatM <- growdat[growdat$Sex == 2 & growdat$Morph == morphs[2], ]
    xm <- growdatM$Age_Beg
    growdatM$Sd_Size <- growdatM$SD_Beg
    if (growthCVtype == "logSD=f(A)") {
      growdatM$high <- qlnorm(0.975, meanlog = log(growdatM$Len_Beg),
                              sdlog = growdatM$Sd_Size)
      growdatM$low <- qlnorm(0.025, meanlog = log(growdatM$Len_Beg),
                             sdlog = growdatM$Sd_Size)
    } else {
      growdatM$high <- qnorm(0.975, mean = growdatM$Len_Beg,
                             sd = growdatM$Sd_Size)
      growdatM$low <- qnorm(0.025, mean = growdatM$Len_Beg,
                            sd = growdatM$Sd_Size)
    }
  } else growdatM <- NULL

  list(Female = growdatF, Male = growdatM)

}

# i = 1 (female)
# i = 2 (male)
SS_stock <- function(i, replist, mainyrs, nyears, MOM = NULL, single_sex = TRUE) {

  if(!is.null(MOM)) {
    proyears <- MOM@proyears
    nsim <- MOM@nsim
  } else {
    proyears <- 0
    nsim <- 1
  }
  allyears <- nyears + proyears

  Stock <- new("Stock")
  Stock@Name <- ifelse(i == 1, "Female", "Male")
  if(nrow(replist$movement) > 0) warning("Movement detected in SS model but not imported right now.")
  Stock@Size_area_1 <- Stock@Frac_area_1 <- Stock@Prob_staying <- rep(0.5, 2)

  cpars_bio <- list()

  ###### maxage
  N_at_age <- replist$natage[replist$natage$Sex == i, ]
  Stock@maxage <- suppressWarnings(colnames(N_at_age) %>% as.numeric()) %>% max(na.rm = TRUE)

  ###### R0
  R0_row <- N_at_age$`Beg/Mid` == "B" & N_at_age$Era == "VIRG"
  R0_col <- parse(text = paste0("N_at_age$`", 0, "`")) %>% eval() # recruit to age-0
  Stock@R0 <- R0_col[R0_row]

  ###### Biological parameters
  endgrowth <- replist$endgrowth[replist$endgrowth$Sex == i, ]

  # M
  cpars_bio$M_ageArray <- array(endgrowth$M, c(Stock@maxage + 1, allyears, nsim)) %>% aperm(c(3, 1, 2))

  # Steepness
  SR_par <- SS_steepness(replist, mainyrs)
  Stock@SRrel <- SR_par[[1]]
  Stock@h <- rep(SR_par[[2]], 2)

  Stock@Perr <- rep(replist$sigma_R_in, 2)

  # Perr_y
  Rec_main <- replist$recruit[vapply(mainyrs, match, numeric(1), table = replist$recruit$Yr, nomatch = 0), ]
  Rdev <- Rec_main$pred_recr/Rec_main$exp_recr

  # Rec_early <- replist$recruit[vapply(c((min(mainyrs)-(Stock@maxage-1)):(min(mainyrs)-1)), match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ] # original

  Rec_early <- replist$recruit[vapply(c((min(mainyrs-1)-(Stock@maxage-1)):(min(mainyrs-1))), match, numeric(1), table = replist$recruit$Yr, nomatch = NA), ]

  Rdev_early <- Rec_early$pred_recr/Rec_early$exp_recr
  Rdev_early[is.na(Rdev_early)] <- 1

  cpars_bio$Perr_y <- c(Rdev_early, Rdev) %>% matrix(nrow = nsim, ncol = Stock@maxage + nyears, byrow = TRUE)

  # AC
  Stock@AC <- log(cpars_bio$Perr_y[1, ]) %>% acf(lag.max = 1, plot = FALSE) %>% getElement("acf") %>%
    getElement(2) %>% rep(2)

  n_age <- Stock@maxage + 1 # include age-0
  
  # Length at age - not found for terminal year
  Len_age_df <- replist$growthseries[replist$growthseries$Morph == i, ]
  Len_age_df <- Len_age_df[vapply(mainyrs, match, numeric(1), table = Len_age_df$Yr, nomatch = 0), ]
  Len_age <- do.call(rbind, lapply(0:Stock@maxage, function(x) parse(text = paste0("Len_age_df$`", x, "`")) %>% eval()))
  if(ncol(Len_age) == (nyears - 1)) Len_age <- cbind(Len_age, endgrowth$Len_Beg)
  if(proyears > 0) {
    Len_age_pro <- matrix(Len_age[, nyears], n_age, proyears)
    Len_age <- cbind(Len_age, Len_age_pro)
  }
  cpars_bio$Len_age <- array(Len_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  
  Stock@LenCV <- mean(endgrowth$SD_Beg[-1]/endgrowth$Len_Beg[-1]) %>% rep(2)
  cpars_bio$LatASD <- array(endgrowth$SD_Beg, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  # Weight at age
  growdat <- getGpars(replist)      # Age-specific parameters in endyr
  
  # Weight at age
  Wt_age_df <- replist$mean_body_wt[replist$mean_body_wt$Morph == i, ]
  Wt_age_df <- Wt_age_df[vapply(mainyrs, match, numeric(1), table = Wt_age_df$Yr, nomatch = 0), ]
  Wt_age <- do.call(rbind, lapply(0:n_age, function(x) parse(text = paste0("Wt_age_df$`", x, "`")) %>% eval()))
  if(ncol(Wt_age) == nyears - 1) Wt_age <- cbind(Wt_age, endgrowth$Wt_Beg[-1])
  if(proyears > 0) {
    Wt_age_pro <- matrix(Wt_age[, nyears], n_age, proyears)
    Wt_age <- cbind(Wt_age, Wt_age_pro)
  }  
  cpars_bio$Wt_age <- array(Wt_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  
  # cpars_bio$plusgroup <- rep(1, nsim) not needed anymore

  # Maturity at age - technically fecundity = 0 for males
  Mat_age <- endgrowth$Len_Mat * endgrowth$Age_Mat
  cpars_bio$Mat_age <- array(Mat_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  # initial Depletion
  sb0 <- replist$timeseries %>% dplyr::filter(Era=="VIRG") %>% dplyr::select(SpawnBio)
  sb1 <- replist$timeseries %>% dplyr::filter(Yr==mainyrs[1]) %>% dplyr::select(SpawnBio)
  
  if (sb1$SpawnBio!=sb0$SpawnBio) {
    # initD <- sb1$SpawnBio/sb0$SpawnBio
    # cpars_bio$initD <- rep(initD, nsim)
    # Modify rec devs so N-at-age 1 is correct
    
    # numbers-at-age in initial year
    n_init <- replist$natage %>% dplyr::filter(Sex==i, Yr==mainyrs[1], `Beg/Mid`=='B')
    cols <- which(colnames(n_init) %in% 0:Stock@maxage)
    n_init <- n_init %>% dplyr::select(dplyr::all_of(cols))
    
    n_virg <- replist$natage %>% 
      dplyr::filter(Sex==i, Era=="VIRG", `Beg/Mid`=='B') %>% 
      dplyr::select(dplyr::all_of(cols)) %>%
      as.numeric()

    adjust <- n_init / (n_virg)# *  cpars_bio$Perr_y[1,n_age:1])
    adjust <- as.numeric(adjust)
    mat <-  matrix(adjust, nrow = nsim, ncol = n_age, byrow = TRUE)
    cpars_bio$Perr_y[, n_age:1] <-  mat
  }
  
  # Depletion
  if(i == 1) {
    sb_curr <- replist$timeseries %>% dplyr::filter(Yr==max(mainyrs)) %>% dplyr::select(SpawnBio)
    Stock@D <- rep(sb_curr$SpawnBio/sb0$SpawnBio, 2)
  } else {
    # Calculate 'SSB' for males, in SS, SSB is always 0 for males
    N_virg <- N_at_age[N_at_age$Era == "VIRG" & N_at_age$`Beg/Mid` == "B", ]
    N_virg2 <- vapply(0:Stock@maxage, function(x)
      N_virg[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))

    N_now <- N_at_age[N_at_age$Yr == max(mainyrs) & N_at_age$`Beg/Mid` == "B", ]
    N_now2 <- vapply(0:Stock@maxage, function(x) 
      N_now[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))

    Stock@D <- rep(sum(N_now2 * Mat_age * Wt_age[, nyears])/sum(N_virg2 * Mat_age * Wt_age[, 1]), 2)
  }

  # Get Fdisc from fleets

  # not used - vB pars estimated from Len_age internally
  Stock@Ksd <- Stock@Linfsd <- Stock@Msd <- c(0, 0)
  Stock@K <- Stock@Linf <- Stock@t0 <- c(0, 0)
  Stock@L50 <- Stock@L50_95 <- c(0, 0)

  fleet_output <- lapply(seq_len(replist$nfleets)[replist$IsFishFleet], SS_fleet, i = i, replist = replist,
                         Stock = Stock, mainyrs = mainyrs, nyears = nyears, MOM = MOM, single_sex = single_sex)

  Fleet <- lapply(fleet_output, getElement, "Fleet")
  cpars <- lapply(fleet_output, function(x) c(cpars_bio, x$cpars_fleet))

  return(list(Stock = Stock, Fleet = Fleet, cpars = cpars))
}

SS_fleet <- function(ff, i, replist, Stock, mainyrs, nyears, MOM = NULL, single_sex = TRUE) {
  
  if(!is.null(MOM)) {
    proyears <- MOM@proyears
    nsim <- MOM@nsim
  } else {
    proyears <- 0
    nsim <- 1
  }
  allyears <- nyears + proyears
  n_age <- Stock@maxage + 1

  #### Selectivity (Asel2 incorporates time-varying length selectivity, Asel age-based assumed constant)
  get_V <- function(ff, rescale = FALSE) {
    #V <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
    #                        replist$ageselex$Factor == "sel_nums", ]
    #V2 <- vapply(1:Stock@maxage, function(x) V[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
    Asel2 <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                                replist$ageselex$Factor == "Asel2", ]
    V2 <- vapply(0:Stock@maxage, function(x) Asel2[match(mainyrs, Asel2$Yr), parse(text = paste0("\"", x, "\"")) %>% eval()],
                 numeric(length(mainyrs))) %>% t()
    Asel <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                               replist$ageselex$Factor == "Asel", ]
    V <- vapply(0:Stock@maxage, function(x) Asel[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
    Vout <- V2 * V
    if(rescale) {
      Vapical <- apply(Vout, 2, max) %>% matrix(nrow(Vout), ncol(Vout), byrow = TRUE)
      Vout <- Vout/Vapical
    }
    return(Vout)
  }
  V <- get_V(ff)
  
   
  # #### Retention-at-age (not currently used in SampleFleetPars)
  # VR <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
  #                         replist$ageselex$Factor == "sel*ret_nums", ]
  # VR2 <- vapply(0:Stock@maxage, function(x) VR[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
  # retA <- VR2/V2

  #### Retention-at-length - assumed to be constant over time (need to update for time-varying retention)
  retL <- replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                              replist$sizeselex$Factor == "Keep" & replist$sizeselex$Yr == max(mainyrs), -c(1:5)] %>% unlist()

  SLarray <- replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                              replist$sizeselex$Factor == "Dead" & replist$sizeselex$Yr == max(mainyrs), -c(1:5)] %>% unlist()
  
  #### Discard mortality
  disc_mort <- replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                   replist$sizeselex$Factor == "Mort" & replist$sizeselex$Yr == max(mainyrs), -c(1:5)] %>% unlist() %>%
    as.numeric() %>% unique()
  if(length(disc_mort) > 1) warning("Discard mortality at age not supported.")

  #### Apical F
  FF <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                           replist$ageselex$Factor == "F", ]
  FF <- FF[match(mainyrs, FF$Yr), ]
  F2 <- vapply(0:Stock@maxage, function(x) FF[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))
  Find <- apply(F2, 1, max)
  # These F's are slighty lower than in replist$exploitation?

  #### Sex-specific catches: predicted retained catch for fleet ff for stock (sex) i
  ALK <- replist$ALK[, , paste0("Seas: 1 Sub_Seas: 2 Morph: ", i)]
  ALK <- ALK[match(replist$lbins, dimnames(ALK)$Length), match(0:Stock@maxage, dimnames(ALK)$TrueAge)]
  retA <- colSums(retL * ALK)
  retA[n_age] <- retA[n_age-1] # TODO - retention missing for terminal age class

  wt <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                           replist$ageselex$Factor == "bodywt", ]
  wt2 <- vapply(0:Stock@maxage, function(x) wt[match(mainyrs, wt$Yr), parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))

  meanN <- replist$natage[replist$natage$Sex == i & replist$natage$`Beg/Mid` == "M", ]
  meanN <- meanN[match(mainyrs, meanN$Yr), ]
  meanN2 <- vapply(0:Stock@maxage, function(x) meanN[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))
  Cat <- colSums(t(meanN2 * Find * wt2) * retA * V)

  #### Fleet object
  Fleet <- new("Fleet")
  Fleet@Name <- replist$FleetNames[ff]
  Fleet@nyears <- length(mainyrs)
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
  cpars_fleet$binWidth <- replist$lbins[2] - replist$lbins[1]
  cpars_fleet$CAL_bins <- replist$lbins %>% c(max(replist$lbins) + cpars_fleet$binWidth)
  cpars_fleet$CAL_binsmid <- replist$lbins + 0.5 * cpars_fleet$binWidth
  cpars_fleet$Fdisc <- rep(mean(disc_mort), nsim)
  # cpars_fleet$V <- cbind(V2, V2_proj) %>% array(c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  # cpars_fleet$retA <- retA %>% array(c(Stock@maxage, allyears, nsim)) %>% aperm(c(3, 1, 2))
  cpars_fleet$retL <- retL %>% array(c(length(retL), allyears, nsim)) %>% aperm(c(3, 1, 2))
  cpars_fleet$SLarray <- SLarray %>% array(c(length(SLarray), allyears, nsim)) %>% aperm(c(3, 1, 2))
  cpars_fleet$DR <- rep(0, nsim) # Should be zero since we have retention in cpars (retL)
  cpars_fleet$Find <- Find %>% matrix(nsim, length(mainyrs), byrow = TRUE)
  cpars_fleet$Data <- new("Data")
  cpars_fleet$Data@Year <- mainyrs
  cpars_fleet$Data@Cat <- matrix(Cat, nrow = 1)
  cpars_fleet$Data@CV_Cat <- matrix(0.2, nrow = 1, ncol = ncol(cpars_fleet$Data@Cat)) # Default value
  
  if(single_sex) { # Add Index if model is a single sex population
    
    get_index <- function(ff) { # Get 
      Ind <- replist$cpue[replist$cpue$Fleet == ff & replist$Use, ]
      if(nrow(Ind) > 0) {
        Obs <- Ind$Obs[match(mainyrs, Ind$Yr)] %>% matrix(1)
        CV <- sqrt(exp(Ind$SE[match(mainyrs, Ind$Yr)]^2) - 1) %>% matrix(1)
      } else {
        Obs <- matrix(NA_real_, 1, 1)
        CV <- matrix(0.2, 1, 1)
      }
      return(list(Obs = Obs, CV = CV))
    }
    Index <- get_index(ff)
    cpars_fleet$Data@VInd <- Index$Obs
    cpars_fleet$Data@CV_VInd <- Index$CV
    
    if(nrow(replist$agedbase) > 0) {
      CAA <- replist$agedbase[replist$agedbase$Fleet == ff & replist$agedbase$sex == i & 
                                replist$agedbase$Used == "yes" & replist$agedbase$Part == 2, ] # Retained CAA 
      
      if(nrow(CAA) > 0) {
        CAA <- CAA %>% dplyr::mutate(Nout = Obs * Nsamp_adj) %>% reshape2::acast(list("Yr", "Bin"), value.var = "Nout")
        
        CAAout <- matrix(NA, nyears, n_age)
        CAAout[match(as.numeric(rownames(CAA)), mainyrs), match(as.numeric(colnames(CAA)), 0:Stock@maxage)] <- CAA
        CAA2 <- apply(CAAout, 1, function(x) {x[is.na(x)] <- 0; return(x)})
        cpars_fleet$Data@CAA <- CAA %>% array(c(n_age, nyears, 1)) %>% aperm(3:1)
      }
    }
    if(nrow(replist$lendbase) > 0) {
      CAL <- replist$lendbase[replist$lendbase$Fleet == ff & replist$lendbase$sex == i & 
                                replist$lendbase$Used == "yes" & replist$lendbase$Part == 2, ] # Retained CAL 
      if(nrow(CAL) > 0) {
        cpars_fleet$Data@CAL_mids <- cpars_fleet$CAL_binsmid
        
        CAL <- CAL %>% dplyr::mutate(Nout = Obs * Nsamp_adj) %>% reshape2::acast(list("Yr", "Bin"), value.var = "Nout")
        
        CALout <- matrix(NA, nyears, length(cpars_fleet$CAL_bins))
        CALout[match(as.numeric(rownames(CAL)), mainyrs), match(as.numeric(colnames(CAL)), cpars_fleet$CAL_bins)] <- CAL
        CAL2 <- apply(CALout, 1, function(x) {x[is.na(x)] <- 0; return(x)})
        cpars_fleet$Data@CAL <- CAL2 %>% array(c(length(cpars_fleet$CAL_bins), nyears, 1)) %>% aperm(3:1)
      }
    }
    
    if(ff == 1) {
      survey_ind <- which(!replist$IsFishFleet)
      if(length(survey_ind) > 0) {
        cpars_fleet$Data@AddInd <- lapply(survey_ind, get_index) %>% lapply(getElement, "Obs") %>% unlist() %>%
          array(c(nyears, length(survey_ind), 1)) %>% aperm(3:1)
        
        cpars_fleet$Data@CV_AddInd <- lapply(survey_ind, get_index) %>% lapply(getElement, "CV") %>% unlist() %>%
          array(c(nyears, length(survey_ind), 1)) %>% aperm(3:1)
        
        cpars_fleet$Data@AddIndV <- lapply(survey_ind, get_V, rescale = TRUE) %>% lapply(function(x) x[, nyears]) %>% 
          unlist() %>% array(c(n_age, length(survey_ind), 1)) %>% aperm(3:1)
        
        cpars_fleet$Data@AddIndType <- rep(1, length(survey_ind))
        
        cpars_fleet$Data@AddIunits <- replist$survey_units[survey_ind]
        cpars_fleet$Data@AddIunits[cpars_fleet$Data@AddIunits > 1] <- 0 
      }
    }
  }
  
  return(list(Fleet = Fleet, cpars_fleet = cpars_fleet))
}

sample_recruitment <- function(Perr_hist, proyears, procsd, AC, seed) {
  set.seed(seed)
  nsim <- nrow(Perr_hist)
  procmu <- -0.5 * procsd^2 * (1 - AC/sqrt(1 - AC^2)) # adjusted log normal mean with AC
  Perr_proj <- rnorm(proyears * nsim, rep(procmu, each = nsim), rep(procsd, each = nsim)) %>%
    matrix(nrow = nsim, ncol = proyears) # Sample recruitment for projection

  # Add autocorrelation to projection recruitment
  Perr_proj[, 1] <- AC * Perr_hist[, ncol(Perr_hist)] + Perr_proj[, 1] * sqrt(1 - AC^2)
  for(y in 2:ncol(Perr_proj)) Perr_proj[, y] <- AC * Perr_proj[, y-1] + Perr_proj[, y] * sqrt(1 - AC^2)

  return(Perr_proj)
}

#Asel <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
#                           replist$ageselex$Factor == "Asel" & replist$ageselex$Yr == max(mainyrs), ]
#VA <- vapply(1:Stock@maxage, function(x) Asel[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
#Asel2 <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
#                            replist$ageselex$Factor == "Asel2"  & replist$ageselex$Yr == max(mainyrs), ]
#VA2 <- vapply(1:Stock@maxage, function(x) Asel2[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
#Vprod <- VA * VA2
