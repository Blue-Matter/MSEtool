
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
SS_stock <- function(i, replist, mainyrs, nyears, proyears, nsim, single_sex = TRUE, partition = 2, 
                     age_M = NULL, mean_h = TRUE, seed = 1) {

  allyears <- nyears + proyears

  Stock <- new("Stock")
  Stock@Name <- ifelse(i == 1, "Female", "Male")
  if(nrow(replist$movement) > 0) warning("Movement detected in SS model but not imported right now.")
  Stock@Size_area_1 <- Stock@Frac_area_1 <- Stock@Prob_staying <- rep(0.5, 2)

  cpars_bio <- list()

  ###### R0 - in multiple season models, the i-th morph recruits appear in the i-th season
  ###### Males are indexed morphs 5-8 in a 4-season model. Morph2 re-maps to 1-4
  N_at_age <- dplyr::filter(replist$natage, Sex == i, `Beg/Mid` == "B", Era == "VIRG") %>%
    dplyr::mutate(Morph2 = Morph - (i - 1) * replist$nseasons) %>% dplyr::filter(Morph2 == Seas) 
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
  if(proyears > 0) {
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

  # Length at age
  if(replist$SS_versionNumeric == 3.30) { # Would do time-varying
    Len_age_df <- dplyr::filter(replist$growthseries, Morph == i, Yr %in% mainyrs)
    Len_age <- do.call(rbind, lapply(0:Stock@maxage, function(x) parse(text = paste0("Len_age_df$`", x, "`")) %>% eval()))
    if(ncol(Len_age) == (nyears - 1)) Len_age <- cbind(Len_age, endgrowth$Len_Beg)
  } else {
    Len_age <- endgrowth$Len_Beg %>% matrix(n_age, nyears) # No time-varying
  }
  if(proyears > 0) {
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
    Stock@a <- replist$Growth_Parameters[i, ]$WtLen1
    Stock@b <- replist$Growth_Parameters[i, ]$WtLen2
  }
  
  if(replist$SS_versionNumeric == 3.30) {
    Wt_age_df <- replist$mean_body_wt[replist$mean_body_wt$Morph == i, ]
    Wt_age_df <- Wt_age_df[vapply(mainyrs, match, numeric(1), table = Wt_age_df$Yr, nomatch = 0), ]
    Wt_age <- do.call(rbind, lapply(0:n_age, function(x) parse(text = paste0("Wt_age_df$`", x, "`")) %>% eval()))
    if(ncol(Wt_age) == nyears - 1) Wt_age <- cbind(Wt_age, endgrowth$Wt_Beg[-1])
  } else {
    Wt_age <- endgrowth$Wt_Beg %>% matrix(n_age, nyears)
  }
  if(proyears > 0) {
    Wt_age_pro <- matrix(Wt_age[, nyears], n_age, proyears)
    Wt_age <- cbind(Wt_age, Wt_age_pro)
  }  
  cpars_bio$Wt_age <- array(Wt_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  
  # cpars_bio$plusgroup <- rep(1, nsim) not needed anymore

  # Maturity at age - technically fecundity = 0 for males in SS.
  # We will set male maturity equal to female maturity
  # but MOM@SexPars will also effectively set male maturity = 0.
  Mat_age <- endgrowth$Len_Mat * endgrowth$Age_Mat
  cpars_bio$Mat_age <- array(Mat_age, c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))

  # initial Depletion
  sb0 <- replist$timeseries %>% dplyr::filter(Era == "VIRG") %>% getElement("SpawnBio") %>% sum(na.rm = TRUE)
  sb1 <- replist$timeseries %>% dplyr::filter(Yr == mainyrs[1]) %>% getElement("SpawnBio") %>% sum(na.rm = TRUE)
  
  if (sb1 != sb0) {
    # initD <- sb1$SpawnBio/sb0$SpawnBio
    # cpars_bio$initD <- rep(initD, nsim)
    # Modify rec devs so N-at-age 1 is correct
    
    # numbers-at-age in initial year
    n_init <- replist$natage %>% dplyr::filter(Sex==i, Yr==mainyrs[1], `Beg/Mid`=='B', Seas == 1)
    cols <- which(colnames(n_init) %in% 0:Stock@maxage)
    n_init <- n_init %>% dplyr::select(dplyr::all_of(cols)) %>% colSums()
    
    n_virg <- replist$natage %>% 
      dplyr::filter(Sex == i, Era == "VIRG", `Beg/Mid` == 'B', Seas == 1) %>% 
      dplyr::select(dplyr::all_of(cols)) %>% colSums()

    adjust <- as.numeric(n_init/n_virg)# *  cpars_bio$Perr_y[1,n_age:1])
    cpars_bio$Perr_y[, n_age:1] <- matrix(adjust, nrow = nsim, ncol = n_age, byrow = TRUE)
  }
  
  # Depletion
  if(i == 1) { # In 3.24, SSB = NA in seasons 1-3 out of 4, so I chose to take the mean
    sb_curr <- replist$timeseries %>% dplyr::filter(Yr == max(mainyrs)) %>% getElement("SpawnBio") %>% 
      mean(na.rm = TRUE)
    Stock@D <- rep(sb_curr/sb0, 2)
  } else {
    # Calculate 'SSB' for males because it is always 0 for males in SS
    N_virg <- N_at_age %>% dplyr::filter(Era == "VIRG", `Beg/Mid` == "B", BirthSeas == Seas)
    N_virg2 <- vapply(0:Stock@maxage, function(x)
      N_virg[, parse(text = paste0("\"", x, "\"")) %>% eval()] %>% sum(), numeric(1))

    N_now <- dplyr::filter(replist$natage, Sex == i, Yr == max(mainyrs), `Beg/Mid` == "B", BirthSeas == Seas)
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
  
  allyears <- nyears + proyears
  n_age <- Stock@maxage + 1

  #### Selectivity (Asel2 incorporates time-varying length selectivity, Asel age-based assumed constant)
  V <- get_V_from_Asel2(ff, i, replist, mainyrs, Stock@maxage)
  
  #### Retention and selectivity-at-length - loop over years for time-varying quantities
  loop_over_change_points <- function(yy, df) {
    sched <- df[df$Yr == yy, ]
    if(nrow(sched) == 1) {
      return(sched[1, -c(1:5)] %>% unlist())
    } else {
      Recall(yy - 1, df)
    }
  }
  retL <- vapply(mainyrs, loop_over_change_points, numeric(replist$nlbinspop),
                 df = replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                          replist$sizeselex$Factor == "Keep", ])
  retLpro <- retL[, nyears] %>% matrix(nrow(retL), proyears)
  
  SLarray <- vapply(mainyrs, loop_over_change_points, numeric(replist$nlbinspop),
                    df = replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                             replist$sizeselex$Factor == "Dead", ])
  SLarraypro <- SLarray[, nyears] %>% matrix(nrow(SLarray), proyears)
  
  #### Discard mortality
  disc_mort <- replist$sizeselex[replist$sizeselex$Fleet == ff & replist$sizeselex$Sex == i &
                                   replist$sizeselex$Factor == "Mort" & replist$sizeselex$Yr == max(mainyrs), -c(1:5)] %>% unlist() %>%
    as.numeric() %>% unique()
  if(length(disc_mort) > 1) warning("Discard mortality at age not supported.")

  #### Apical F
  FF <- replist$ageselex[replist$ageselex$Fleet == ff & replist$ageselex$Sex == i &
                           replist$ageselex$Factor == "F", ]
  if(nrow(FF)) {
    FF <- FF[match(mainyrs, FF$Yr), ]
    F2 <- vapply(0:Stock@maxage, function(x) FF[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))
    Find <- apply(F2, 1, max)
  } else {
    FF <- replist$exploitation[, match(replist$FleetNames[ff], colnames(replist$exploitation))] %>%
      aggregate(by = list(Yr = replist$exploitation$Yr), sum)
    Find <- FF$x[FF$Yr %in% mainyrs]
    #Find <- parse(text = paste0("replist$timeseries$`F:_", ff, "`")) %>% eval()
    #Find <- Find[match(mainyrs, replist$timeseries$Yr)]
  }
  

  #### Sex-specific catches: predicted retained catch for fleet ff for stock (sex) i
  ALK_dim_match <- paste0("Seas: 1 Sub_Seas: 2 Morph: ", i) %in% dimnames(replist$ALK)[[3]] %>% any()
  if(ALK_dim_match) {
    ALK <- replist$ALK[, , paste0("Seas: 1 Sub_Seas: 2 Morph: ", i)]
  } else {
    ALK <- replist$ALK[, , paste0("Seas: 1 Morph: ", (i - 1) * replist$nseasons + 1)]
  }
  ALK <- ALK[match(replist$lbinspop, dimnames(ALK)$Length), match(0:Stock@maxage, dimnames(ALK)$TrueAge)]
  
  wt <- dplyr::filter(replist$ageselex, Fleet == ff, Sex == i, Factor == "bodywt", Seas == 1, Morph == 1)
  wt <- vapply(0:Stock@maxage, function(x) wt[match(mainyrs, wt$Yr), parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))
  
  meanN <- dplyr::filter(replist$natage, Sex == i, `Beg/Mid` == "M", Seas == 1)
  meanN <- meanN[match(mainyrs, meanN$Yr), ]
  meanN <- vapply(0:Stock@maxage, function(x) meanN[, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(length(mainyrs)))

  Cat <- numeric(nyears)
  for(yy in 1:nyears) {
    retA <- colSums(retL[, yy] * ALK)
    if(!is.numeric(retA[n_age])) retA[n_age] <- retA[n_age-1]
    Cat[yy] <- sum(meanN[yy, ] * Find[yy] * wt[yy, ] * retA * V[, yy])
  }

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
  cpars_fleet$binWidth <- replist$lbinspop[2] - replist$lbinspop[1]
  cpars_fleet$CAL_bins <- replist$lbinspop %>% c(max(replist$lbinspop) + cpars_fleet$binWidth)
  cpars_fleet$CAL_binsmid <- replist$lbinspop + 0.5 * cpars_fleet$binWidth
  cpars_fleet$Fdisc <- rep(mean(disc_mort), nsim)
  #cpars_fleet$V <- cbind(V2, V2_proj) %>% array(c(n_age, allyears, nsim)) %>% aperm(c(3, 1, 2))
  cpars_fleet$retL <- replicate(nsim, cbind(retL, retLpro)) %>% aperm(c(3, 1, 2))
  cpars_fleet$SLarray <- replicate(nsim, cbind(SLarray, SLarraypro)) %>% aperm(c(3, 1, 2))
  cpars_fleet$DR <- rep(0, nsim) # Should be zero since we have retention in cpars$retL
  cpars_fleet$Find <- Find %>% matrix(nsim, length(mainyrs), byrow = TRUE)
  
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
  
  cpars_fleet$Data@L50 <- LinInterp(cpars_bio$Mat_age[1,,nyears], cpars_bio$Len_age[1,,nyears], 0.5 + 1e-4)
  cpars_fleet$Data@L95 <- LinInterp(cpars_bio$Mat_age[1,,nyears], cpars_bio$Len_age[1,,nyears], 0.95)
  cpars_fleet$Data@LenCV <- GP$CVmax
  
  cpars_fleet$Data@LFC <- LinInterp(V[, nyears], cpars_bio$Len_age[1,,nyears], 0.05)
  cpars_fleet$Data@LFS <- LinInterp(V[, nyears], cpars_bio$Len_age[1,,nyears], 0.95)
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
      Ind <- replist$cpue %>% dplyr::filter(Fleet == ff, Use == 1) %>% group_by(Yr) %>% 
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
  
  if(nrow(CAA) > 0) {
    CAA <- CAA %>% dplyr::mutate(Nout = Obs * Nsamp_adj) %>% reshape2::acast(list("Yr", "Bin"), value.var = "Nout", fun.aggregate = sum)
    
    CAAout <- matrix(NA, nyears, n_age)
    CAAout[match(as.numeric(rownames(CAA)), mainyrs), match(as.numeric(colnames(CAA)), 0:Stock@maxage)] <- CAA
    CAA2 <- apply(CAAout, 1, function(x) {x[is.na(x)] <- 0; return(x)})
    cpars_fleet$Data@CAA <- CAA %>% array(c(n_age, nyears, 1)) %>% aperm(3:1)
  }
  
  CAL <- lapply(partition, function(x) {
    replist$lendbase[replist$lendbase$Fleet == ff & replist$lendbase$Sex == i & 
                       replist$lendbase$Used == "yes" & replist$lendbase$Part == x, ] # Retained CAL 
  }) 
  CAL <- do.call(rbind, CAL)
  
  if(nrow(CAL) > 0) {
    cpars_fleet$Data@CAL_bins <- cpars_fleet$CAL_bins
    cpars_fleet$Data@CAL_mids <- cpars_fleet$CAL_binsmid # Optional
    
    CAL <- CAL %>% dplyr::mutate(Nout = Obs * Nsamp_adj) %>% reshape2::acast(list("Yr", "Bin"), value.var = "Nout", fun.aggregate = sum)
    
    CALout <- matrix(NA, nyears, length(cpars_fleet$CAL_binsmid))
    CALout[match(as.numeric(rownames(CAL)), mainyrs), 
           match(as.numeric(colnames(CAL)), cpars_fleet$CAL_bins[-length(cpars_fleet$CAL_bins)])] <- CAL
    CAL2 <- apply(CALout, 1, function(x) {x[is.na(x)] <- 0; return(x)})
    cpars_fleet$Data@CAL <- CAL2 %>% array(c(length(cpars_fleet$CAL_binsmid), nyears, 1)) %>% aperm(3:1)
    
    cpars_fleet$Data@ML <- apply(CAL2, 2, function(xx) weighted.mean(x = cpars_fleet$CAL_binsmid, w = xx)) %>% matrix(1)
  }
  
  return(list(Fleet = Fleet, cpars_fleet = cpars_fleet))
}


sample_recruitment <- function(Perr_hist, proyears, procsd, AC, seed) {
  set.seed(seed)
  nsim <- nrow(Perr_hist)
  procmu <- -0.5 * procsd^2 * (1 - AC/sqrt(1 - AC^2)) # adjusted log normal mean with AC
  Perr_delta <- rnorm(proyears * nsim, rep(procmu, each = nsim), rep(procsd, each = nsim)) %>%
    matrix(nrow = nsim, ncol = proyears) # Sample recruitment for projection
  Perr_proj <- matrix(NA_real_, nsim, proyears)

  # Add autocorrelation to projection recruitment
  Perr_proj[, 1] <- AC * Perr_hist[, ncol(Perr_hist)] + Perr_delta[, 1] * sqrt(1 - AC^2)
  for(y in 2:ncol(Perr_proj)) Perr_proj[, y] <- AC * Perr_proj[, y-1] + Perr_delta[, y] * sqrt(1 - AC^2)

  return(Perr_proj)
}

get_V_from_Asel2 <- function(ff, i, replist, mainyrs, maxage, rescale = FALSE) {
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
  
  # Assume Asel is time-invariant
  Asel <- dplyr::filter(replist$ageselex, Fleet == ff, Sex %in% i, Factor == "Asel")
  V <- vapply(0:maxage, function(x) Asel[1, parse(text = paste0("\"", x, "\"")) %>% eval()], numeric(1))
  Vout <- V2 * V
  
  if(rescale) {
    Vapical <- apply(Vout, 2, max) %>% matrix(nrow(Vout), ncol(Vout), byrow = TRUE)
    Vout <- Vout/Vapical
  }
  return(Vout)
}


SS_seasonalyears_to_annual <- function(OM, SSdir) {
  
  if(is.list(SSdir)) {
    replist <- SSdir
  } else {
    replist <- SS_import(SSdir, silent, ...)
  }
  
  nseas <- 1/replist$seasdurations
  mainyrs <- replist$startyr:replist$endyr
  year_frac <- data.frame(mainyrs = mainyrs, seas = rep(1:nseas, length(mainyrs)/nseas), 
                          true_year = rep(1:(length(mainyrs)/nseas), each = nseas))
  
  age_frac <- data.frame(age = 0:OM@maxage) %>% mutate(true_age = floor(age/nseas))
  
  OM2 <- OM
  OM2@nyears <- OM2@CurrentYr <- max(year_frac$true_year)
  OM2@maxage <- max(age_frac$true_age)
  
  avg <- c("M_ageArray", "Wt_age", "Len_age", "LatASD", "Mat_age", "V")
  OM2@cpars[match(avg, names(OM2@cpars))] <- 
    lapply(avg, function(xx) parse(text = paste0("OM@cpars$", xx)) %>% eval() %>%
             cpars_season(FUN = mean, year_frac = year_frac, age_frac = age_frac, proyears = OM2@proyears))
  
  OM2@cpars$M_ageArray <- OM2@cpars$M_ageArray * nseas
  
  OM2@cpars$V <- apply(OM2@cpars$V, c(1, 3), function(x) x/max(x)) %>% aperm(c(2, 1, 3))
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
