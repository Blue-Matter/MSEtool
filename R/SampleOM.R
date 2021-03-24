myrunif <- function(n, val1, val2) {
  min <- min(c(val1, val2))
  max <- max(c(val1, val2))

  if (is.na(n)) stop("First argument is NA")
  # if (is.na(val1)) stop('Second argument is NA')
  # if (is.na(val2)) stop('Third argument is NA')

  if (all(is.na(c(min, max)))) return(rep(NA,n))
  if (all(min == max)) {
    tt <- runif(n)
    return(rep(min, n))
  } else {
    return(runif(n, min, max))
  }
}


#' Internal function for checking that the `OM@cpars` is formatted correctly
#'
#' @param cpars a list of model parameters to be sampled (single parameters are
#' a vector `nsim` long, first dimension of matrices and arrays must be `nsim`)
#' @return either an error and the length of the first dimension of the various
#' cpars list items or passes and returns the number of simulations in `cpars`
#' @author T. Carruthers
cparscheck<-function(cpars){

  if (length(cpars)<1) return(length(cpars))
  dim1check<-function(x){
    if (inherits(x, 'numeric') | inherits(x, 'integer')) return(length(x))
    else return(dim(x)[1])
  }

  dims <- sapply(cpars,dim1check)

  # drop invalid names
  dims <- dims[!unlist(lapply(dims, is.null))]

  # Check Stock names - must be vectors
  slts <- slotNames("Stock")
  ind <- which(names(cpars) %in% slts)
  if (length(ind)>0) {
    chkdim <- !unlist(lapply(lapply(cpars[ind], dim), is.null))
    if (any(chkdim)) {
      err <- names(chkdim[chkdim])
      stop("Incorrect dimensions in Stock parameter slots in OM@cpars:", paste(err, collapse=", "))
    }
  }

  # check if EffYears etc are in cpars
  effNames <- c("EffYears", "EffLower", "EffUpper")
  temp <- effNames %in%  names(dims)
  # all 3?
  if (any(temp) & !all(temp)) stop(paste(effNames[!temp], collapse=", "), " missing")
  # can't have Find as well
  if (any(temp & 'Find' %in% names(dims))) stop("Can't provide both Find and EffYears etc in cpars")
  # same length
  if (all(temp)) {
    if (!all(dims[effNames]==dims[effNames][1])) stop(paste(effNames, collapse=", "), " are not equal length")
  }

  if (length(dims) > 0) {
    if(length(unique(dims))!=1){
      print(dims)
      stop("The custom parameters in your operating model @cpars have varying
           number of simulations. For each simulation each parameter / variable
           should correspond with one another")
    }else{
      return(as.integer(dims[1]))
    }
  }



}


#' Sample custom pars
#'
#' @param cpars A named list containing custom parameters for the OM
#' @param nsim number of simulations
#' @param silent logical - print the names of the cpars? Turn off when using the function in a loop
#' @return A named list of sampled custom parameters
#' @keywords internal
#' @export
#'
SampleCpars <- function(cpars, nsim=48, silent=FALSE) {

  sampCpars <- list()

  # ---- Non-stochastic parameters ----
  Names <- c('CAL_bins', 'CAL_binsmid', 'binWidth', 'nCALbins',
             'maxage', 'n_age', 'CurrentYr',
             'plusgroup', 'control', 'AddIUnits', 'Data', 'MPA',
             'nareas', 'a', 'b', 'maxF', 'Sample_Area', 'Asize')

  cpars2 <- cpars
  cpars2[Names] <- NULL
  # --- Check custom parameters dimensions ----
  ncparsim <- cparscheck(cpars2)

  # --- Check for Invalid Cpars ----

  # CparsInfo <- MSEtool:::cpars_info # get internal data from sysdata
  CparsInfo <- cpars_info # get internal data from sysdata - above for debugging

  CNames <- names(cpars)
  ValNames <- CparsInfo$Var

  invalid <- CNames[!CNames %in% ValNames]
  if (length(invalid)>0) {
    invdf <- data.frame(not_used_cpars=invalid, stringsAsFactors = FALSE)

    if(!silent) {
      message("Invalid names found in custom parameters:")
      base::message(paste0(capture.output(invdf), collapse = "\n"))
    }
  }
  # # report found names
  valid <- which(CNames %in% ValNames)
  cpars <- cpars[valid]
  if (length(valid) == 0) {
    message("No valid names found in custompars. Ignoring `OM@cpars`")
    return(list())
  }
  CNames <- names(cpars)
  outNames <- paste(CNames, "")
  if(!silent) message("Valid custom parameters found: \n", paste0(outNames, collapse="\n"))

  # ---- Sample custom pars ----

  if (!is.null(ncparsim) && ncparsim>0) {
    if (ncparsim < nsim) {
      ind <- sample(1:ncparsim, nsim, replace=TRUE) # index for cpars
    } else {
      if (ncparsim == nsim) {
        ind <- 1:nsim
      } else {
        ind <- sample(1:ncparsim, nsim, replace=FALSE)
      }
    }
  }

  if (length(cpars2)>0) {
    for (i in 1:length(cpars2)) {
      samps <- cpars2[[i]]
      name <- names(cpars2)[i]
      # sample vectors
      if ("numeric" %in% class(samps) | "integer" %in% class(samps))
        sampCpars[[name]] <- samps[ind]
      # sample matrices

      if (inherits(samps, "matrix") | inherits(samps, 'array')) {
        if (length(dim(samps)) == 2) {
          sampCpars[[name]] <- samps[ind,, drop=FALSE] # sample matrix
        } else {
          # sample arrays
          dims <- dim(samps)
          tout <- array(NA, dim=c(nsim, dims[2:length(dims)]))
          tlist <- c(list(ind), lapply(dims[2:length(dims)], seq))
          tlist2 <- c(list(1:nsim), lapply(dims[2:length(dims)], seq))
          varind <- expand.grid(tlist) %>% as.matrix()
          varind2 <- expand.grid(tlist2) %>% as.matrix()
          tout[varind2] <- samps[varind]
          sampCpars[[name]] <- tout
        }
      }
      if (inherits(samps, 'data.frame')) {
        sampCpars[[name]] <- samps
      }
    }
  }

  sampCpars <- sampCpars[lengths(sampCpars) != 0] # remove NULL


  # --- Add non-stochastic parameters ----
  sampCpars[Names] <- cpars[Names]
  ind <- which(unlist(lapply(sampCpars, length)) ==0)
  sampCpars[ind] <- NULL

  sampCpars
}

sample_unif <- function(par, cpars, Obj, nsim, altpar=NULL) {
  if (!is.null(cpars[[par]])) {
    tt <- myrunif(nsim, 0,1) # call to runif to increment RNG
    return(cpars[[par]])
  }
  if (!is.null(altpar)) par <- altpar
  vals <- slot(Obj, par)
  if (length(vals)<1) stop('slot ', par, ' in object class ', class(Obj), ' is missing values')
  myrunif(nsim, vals[1], vals[2])
}

vB <- function(pars, ages) {
  pars[1] * (1-exp(-pars[2]*(ages-pars[3])))
}
fitVB <- function(pars, LatAge, ages) {
  pars[1] <- exp(pars[1])
  pars[2] <- exp(pars[2])
  sum((vB(pars, ages) - LatAge)^2)
}

#' Sample Stock parameters
#'
#' @param Stock An object of class 'Stock' or class 'OM'
#' @param nsim Number of simulations. Ignored if 'Stock' is class 'OM'
#' @param nyears Number of historical years. Ignored if 'Stock' is class 'OM'
#' @param proyears Number of projection years. Ignored if 'Stock' is class 'OM'
#' @param cpars Optional named list of custom parameters. Ignored if 'Stock' is class 'OM'
#' @param msg logical. Warning message for M values?
#'
#' @return A named list of sampled Stock parameters
#' @keywords internal
#' @export
#'
SampleStockPars <- function(Stock, nsim=48, nyears=80, proyears=50, cpars=NULL, msg=TRUE) {
  if (class(Stock) != "Stock" & class(Stock) != "OM")
    stop("First argument must be class 'Stock' or 'OM'")

  if (class(Stock) == "OM") {
    nsim <- Stock@nsim
    nyears <- Stock@nyears
    proyears <- Stock@proyears
  }
  # Get custom pars if they exist
  if (class(Stock) == "OM" && length(Stock@cpars) > 0 && is.null(cpars))
    cpars <- SampleCpars(Stock@cpars, nsim)  # custom parameters exist in Stock/OM object

  # ---- Maximum age ----
  if(!is.null(cpars$maxage)) {
    maxage <- cpars$maxage
  } else {
    maxage <- Stock@maxage
  }
  maxage  <- maxage[1] # check if maxage has been passed in custompars

  # ---- Virgin Recruitment ----
  if(!is.null(cpars$R0)) {
    R0 <- cpars$R0  # Initial recruitment
  } else {
    R0 <- Stock@R0  # Initial recruitment
  }
  R0 <- rep(R0, nsim)[1:nsim] # allow for different R0 per sim

  # ---- Natural Mortality ----
  n_age <- maxage + 1 # number of age classes (including age-0)
  M <- sample_unif('M', cpars, Stock, nsim)
  Msd <- sample_unif('Msd', cpars, Stock, nsim)

  # ---- Stock-Recruitment Relationship ----
  # type of Stock-recruit relationship. 1=Beverton; 2=Ricker
  if (!is.null(cpars$SRrel)) {
    SRrel <- cpars$SRrel
  } else {
    SRrel <- rep(Stock@SRrel, nsim)
  }

  hs <- sample_unif('hs', cpars, Stock, nsim, 'h')
  if (any(hs > 1 | hs < 0.2))
    stop("Steepness (OM@h) must be between 0.2 and 1", call.=FALSE)

  # ---- Depletion ----
  D <- sample_unif('D', cpars, Stock, nsim)

  # ---- Recruitment Deviations ----
  # have rec devs been passed in cpars?
  if (is.null(cpars$Perr_y)) { # no - sample recruitment deviations

    if (!is.null(cpars[['Perr']])) cpars$procsd <- cpars[['Perr']]
    procsd <-  sample_unif('procsd', cpars, Stock, nsim, 'Perr')

    AC <- sample_unif('AC', cpars, Stock, nsim)
    # adjusted log normal mean http://dx.doi.org/10.1139/cjfas-2016-0167
    procmu <- -0.5 * procsd^2  * (1 - AC)/sqrt(1 - AC^2)

    procsd[procsd==0] <- tiny # for reproducibility in rnorm
    Perr_y <- array(rnorm((nyears + proyears+maxage) * nsim,
                          rep(procmu, nyears + proyears+maxage),
                          rep(procsd, nyears + proyears+maxage)),
                    c(nsim, nyears + proyears+maxage))

    # add auto-correlation
    for (y in 2:(nyears + proyears+maxage))
      Perr_y[, y] <- AC * Perr_y[, y - 1] + Perr_y[, y] * (1 - AC * AC)^0.5
    Perr_y <- exp(Perr_y) # normal space (mean 1 on average)
  } else { # Perr_y passed in cpars
    Perr_y <- cpars$Perr_y # normal space
    log_Perr_y <- log(Perr_y)
    if (!is.null(cpars$procsd)) {
      procsd <- cpars$procsd
    } else {
      procsd <- apply(log_Perr_y, 1, sd)
    }
    if (!is.null(cpars$AC)) {
      AC <- cpars$AC
    } else {
      AC <- apply(log_Perr_y,1,function(x)acf(x, plot=FALSE)$acf[2,1,1])
    }
    if (!is.null(cpars$procmu)) {
      procmu <- cpars$procmu
    } else {
      procmu <- -0.5 * procsd^2  * (1 - AC)/sqrt(1 - AC^2)
    }
  }

  # ---- Growth Parameters ----
  Linf <- sample_unif('Linf', cpars, Stock, nsim)
  Linfsd <- sample_unif('Linfsd', cpars, Stock, nsim)
  K <- sample_unif('K', cpars, Stock, nsim)
  Ksd <- sample_unif('Ksd', cpars, Stock, nsim)
  t0 <- sample_unif('t0', cpars, Stock, nsim)

  # Generate random numbers for random walk
  if (!is.null(cpars$Mrand)) {
    Mrand <- cpars$Mrand
  } else {
    Mrand <- matrix(exp(rnorm(nsim*(proyears+nyears), -0.5 * Msd^2, Msd)),
                    nrow=nsim, ncol=proyears+nyears)
  }
  if (!is.null(cpars$Linfrand)) {
    Linfrand <- cpars$Linfrand
  } else {
    Linfrand <- matrix(exp(rnorm(nsim*(proyears+nyears), -0.5 * Linfsd^2, Linfsd)),
                    nrow=nsim, ncol=proyears+nyears)
  }
  if (!is.null(cpars$Krand)) {
    Krand <- cpars$Krand
  } else {
    Krand <- matrix(exp(rnorm(nsim*(proyears+nyears), -0.5 * Ksd^2, Ksd)),
                       nrow=nsim, ncol=proyears+nyears)
  }
  if (!is.null(cpars$Linfarray)) {
    Linfarray <- cpars$Linfarray
  } else {
    Linfarray <- GenerateRandomWalk(Linf, Linfsd, nyears + proyears,
                                    nsim, Linfrand)
  }
  if (!is.null(cpars$Karray)) {
    Karray <- cpars$Karray
  } else {
    Karray <- GenerateRandomWalk(K, Ksd, nyears + proyears,
                                 nsim, Krand)
  }
  if (!is.null(cpars$Agearray)) {
    Agearray <- cpars$Agearray
  } else {
    Agearray <- array(rep(0:maxage, each = nsim), dim = c(nsim, n_age))
  }

  # Check dimensions of Linfarray and Karray
  if (all(dim(Linfarray) != c(nsim, nyears+proyears)))
    stop("Linfarray must be dimensions: nsim, proyears+nyears (",
         nsim, ", ", proyears+nyears, ")")
  if (all(dim(Karray) != c(nsim, nyears+proyears)))
    stop("Karray must be dimensions: nsim, proyears+nyears (",
         nsim, ", ", proyears+nyears, ")")

  if (!is.null(cpars$t0array)) {
    t0array <- cpars$t0array
  } else {
    t0array <- matrix(t0, nrow=nsim, ncol=proyears+nyears)
  }

  LenCV <- sample_unif('LenCV', cpars, Stock, nsim)

  if (msg && any(LenCV < 0.05))
    warning('Stock@LenCV is very low for at least some simulations (<0.05).\nLength composition data may not be generated successfully and MPs using length data may crash or be unreliable. \nLenCV is the variation in length-at-age. Very low values implies all individuals exactly follow the average growth curve')

  # --- Mean Length-at-Age array ----
  if (is.null(cpars$Len_age)) { # Len_age not in cpars - generate it
    Len_age <- array(NA, dim = c(nsim, n_age, nyears + proyears))  # Length at age array
    ind <- as.matrix(expand.grid(1:nsim, 1:n_age, 1:(nyears + proyears)))
    Len_age[ind] <- Linfarray[ind[, c(1, 3)]] * (1 - exp(-Karray[ind[, c(1, 3)]] *
                                                           (Agearray[ind[, 1:2]] -
                                                              t0[ind[, 1]])))
  } else { # Len_age passed in cpars
    Len_age <- cpars$Len_age
    # check dimensions
    if (any(dim(Len_age) != c(nsim, n_age, nyears + proyears)))
      stop("'Len_age' must be array with dimensions: nsim, maxage+1, nyears + proyears. \nShould be: ",
           paste0(c(nsim, n_age, nyears + proyears), collapse=","),
           '; Currently is: ', paste0(dim(Len_age), collapse = ','))

    # Estimate vB parameters for each year and each sim if growth parameters are not in cpars
    if (!all(c("Linf", "K", "t0") %in% names(cpars))) {
      if (msg)
        message('Mean length-at-age array are in cpars, but von Bert. growth parameters are not.',
                'Estimating von Bert. growth parameters for each simulation and year from ',
                'cpars$Len_age')

      # loop over simulations
      cnt <- 0
      for (ss in 1:nsim) {
        if(msg) { # print status update
          cnt <- cnt + 1
          cat(".")
          flush.console()
          if (cnt >80) {
            cat("\n")
            flush.console()
            cnt <- 0
          }
        }
        estpars <- sapply(1:(nyears + proyears), function(X) {
          starts  <- c(log(max(Len_age[ss,,X])), log(0.2), 0)
          optim(starts, fitVB, LatAge=Len_age[ss,,X], ages=0:maxage)$par
        })

        pars <- estpars
        Linfarray[ss,] <- round(exp(pars[1,]),3)
        Karray[ss,] <- round(exp(pars[2,]),3)
        t0[ss] <- mean(pars[3,])
        t0array <- matrix(t0, nrow=nsim, ncol=proyears+nyears)
      } # end loop over sims for vB estimation
      if(msg) cat("\n")
      # Add Linf, K, t0 to StockPars (current yr)
      Linf <- Linfarray[,nyears]
      K <- Karray[,nyears]
      t0 <- t0array[,nyears]

    } # end estimate vB conditional
  } # end generate Len_age array

  Len_age[Len_age<0] <- tiny
  maxlen <- apply(Linfarray, 1, mean) # reference length for Vmaxlen

  # ---- Generate Catch-at-Length Classes ----
  if (!is.null(cpars$LatASD)) {
    LatASD <- cpars$LatASD
  } else {
    LatASD <- Len_age * array(LenCV, dim=dim(Len_age)) # SD of length-at-age
  }
  if (any(dim(LatASD) != dim(Len_age)))
    stop("Dimensions of 'LatASD' must match dimensions of 'Len_age'", .call=FALSE)

  if (!is.null(cpars$CAL_bins)) {
    CAL_bins <- cpars$CAL_bins
    binWidth <- CAL_bins[2:length(CAL_bins)] - CAL_bins[2:length(CAL_bins) - 1]
  }
  #if (!is.null(cpars$CAL_binsmid)) {
  #  binWidth <- cpars$CAL_binsmid[2] - cpars$CAL_binsmid[1]
  #  CAL_binsmid <- cpars$CAL_binsmid
  #}

  MaxBin <- ceiling(max(Linfarray) + 2 * max(Linfarray) * max(LenCV))
  if (!exists("CAL_bins", inherits=FALSE)) {
    binWidth <- ceiling(0.03 * MaxBin)
    CAL_bins <- seq(from = 0, to = MaxBin + binWidth, by = binWidth)
    binWidth <- rep(binWidth, length(CAL_bins) - 1)
  }
  CAL_binsmid <- CAL_bins[2:length(CAL_bins)] - 0.5 * binWidth
  if (length(CAL_bins) != length(CAL_binsmid)+1)
    stop("Length of 'CAL_bins' must be length(CAL_binsmid)+1", .call=FALSE)

  #binWidth <- CAL_binsmid[2] - CAL_binsmid[1] # should be unchanged if passed in cpars anyway

  # Check bin width - in case both CAL_bins or CAL_binsmid AND binWidth have been passed in with cpars
  #if (!all(diff(CAL_bins) == binWidth))
  #  stop("width of CAL_bins != binWidth", call.=FALSE)
  #if (!all(diff(CAL_binsmid) == binWidth))
  #  stop("width of CAL_binsmid != binWidth", call.=FALSE)
  nCALbins <- length(CAL_binsmid)

  if (max(Linfarray) > max(CAL_bins))
    stop("`max(CAL_bins)` must be larger than `max(Linfarray)`")

  # ---- Weight-at-Age array ----
  if (is.null(cpars$Wt_age)) { # Wt_age not passed in cpars
    Wt_age <- array(NA, dim = c(nsim, n_age, nyears + proyears))  # Weight at age array
    ind <- as.matrix(expand.grid(1:nsim, 1:n_age, 1:(nyears + proyears)))  # an index for calculating Weight at age
    Wt_age[ind] <- Stock@a * Len_age[ind]^Stock@b  # Calculation of weight array
    Wa <- Stock@a
    Wb <- Stock@b
  } else { # Wt_age in cpars
    Wt_age <- cpars$Wt_age
    if (any(dim(Wt_age) != c(nsim, n_age, nyears + proyears)))
      stop("'Wt_age' must be array with dimensions: nsim, maxage+1, nyears + proyears: ",
           paste0(c(nsim, maxage+1, nyears + proyears), collapse=","), "; but has dimensions: ",
           paste0(dim(Wt_age), collapse=","))
    # check if length-weight pars in cpars
    if (all(c('a', 'b') %in% names(cpars))) {
      Wa <- cpars$a
      Wb <- cpars$b
    } else {
      # Estimate length-weight parameters from the Wt_age data
      logL <- log(as.numeric(Len_age)+tiny)
      logW <- log(as.numeric(Wt_age)+tiny)
      mod  <- lm(logW ~ logL)
      EstVar <- summary(mod)$sigma^2
      Wa <- as.numeric(exp(coef(mod)[1]) * exp((EstVar)/2))
      Wb <- as.numeric(coef(mod)[2])
    }
  } # end Wt_age calculation

  # ---- Sample Maturity Parameters ----
  if (is.null(cpars$Mat_age)) { # Maturity-at-age not passed in cpars
    if (!is.null(cpars$L50)) {
      L50 <- cpars$L50
    } else {
      sL50 <- array(myrunif(nsim * 50, Stock@L50[1], Stock@L50[2]), c(nsim, 50))  # length at 50% maturity
      sL50[sL50/Linf > 0.95] <- NA
      L50 <- apply(sL50, 1, function(x) x[!is.na(x)][1])
      L50[is.na(L50)] <- 0.95 * Linf[is.na(L50)]
    }
    if (!is.null(cpars$L50_95)) {
      L50_95 <- cpars$L50_95
    } else {
      L50_95 <- array(myrunif(nsim * 50, Stock@L50_95[1], Stock@L50_95[2]), c(nsim, 50))  # length at 95% maturity
      if (!exists("sL50", inherits=FALSE)) sL50 <- matrix(L50, nsim, 50)
      L50_95[((sL50+L50_95)/matrix(Linf, nsim, 50)) > 0.99] <- NA
      L50_95 <- apply(L50_95, 1, function(x) x[!is.na(x)][1])
      L50_95[is.na(L50_95)] <- 2
    }
    L95 <- cpars$L95
    if (is.null(L95)) L95 <- L50 + L50_95
    if (any(L95> Linf)) {
      if (msg) message("Note: Some samples of L95 are above Linf. Defaulting to 0.99*Linf")
      L95[L95> Linf] <- 0.99* Linf[L95> Linf]
      L50_95 <- L95 - L50
    }
    # Generate L50 & L95 for each year
    relL50 <- matrix(L50/Linf, nrow=nsim, ncol=nyears + proyears, byrow=FALSE) # assume L50/Linf stays constant
    L50array <- relL50 * Linfarray
    delLm <- L95 - L50
    L95array <- L50array + matrix(delLm, nrow=nsim, ncol=nyears + proyears, byrow=FALSE)
    L95array[L95array>Linfarray] <- 0.99 *  Linfarray[L95array>Linfarray]
  } else { # Maturity-at-age in cpars
    Mat_age <- cpars$Mat_age
    if (any(dim(Mat_age) != c(nsim, n_age, nyears+proyears)))
      stop("'cpars$Mat_age' must be array with dimensions: nsim, maxage+1, nyears+proyears")
    # Calculate L50, L95, ageM and age95array
    if (all(c('L50array', 'L95array', 'ageMarray', 'age95array') %in% names(cpars))) {  # no need to calculate
      L50array <- cpars$L50array
      L50 <- apply(L50array, 1, mean)
      L95array <- cpars$L95array
      L95 <- apply(L95array, 1, mean)
      ageMarray <- cpars$ageMarray
      age95array <- cpars$age95array

    } else { # calculate maturity-at-length and -age parameters from  cpars$Mat_age
      ageMarray <- age95array <- L50array <- L95array <- matrix(NA, nsim, nyears+proyears)
      for (yr in 1:(nyears+proyears)) { # loop over years
        # check that Mat_age < 0.5 values exist
        if (nsim == 1) {
          oksims <- which(min(Mat_age[1,,yr]) < 0.5)
        } else {
          oksims <- which(apply(Mat_age[,,yr], 1, min) < 0.5)
        }
        if (length(oksims)<1) {
          ageMarray[,yr] <- 1 # set to 1 if < 1
          L50array[,yr] <- 1 # set to 1 if < 1
        } else {
          noksims <- (1:nsim)[-oksims]
          ageMarray[oksims,yr] <- unlist(sapply(oksims, function(x)
            LinInterp(Mat_age[x,, yr], y=1:n_age, 0.5)))
          ageMarray[noksims,yr] <- 1 # set to 1
          L50array[oksims,yr] <- unlist(sapply(oksims, function(x)
            LinInterp(Mat_age[x,,yr], y=Len_age[x, , nyears], 0.5)))
          L50array[noksims,yr] <- 1 # set to 1
        }
        age95array[,yr] <- unlist(sapply(1:nsim, function(x)
          LinInterp(Mat_age[x,, yr], y=1:n_age, 0.95)))
        L95array[,yr]<- unlist(sapply(1:nsim, function(x)
          LinInterp(Mat_age[x,,yr], y=Len_age[x, , nyears], 0.95)))
      } # end loop over years

      L50array[!is.finite(L50array)] <- 0.8*Linfarray[!is.finite(L50array)]
      L95array[!is.finite(L95array)] <- 0.99*Linfarray[!is.finite(L95array)]
      L95array[L50array >= L95array] <- L50array[L50array >= L95array] * 1.01
      L50 <- L50array[,nyears]
      L95 <- L95array[,nyears]
      L50[!is.finite(L50)] <- 0.8*Linf[!is.finite(L50)]
      L95[!is.finite(L95)] <- 0.99*Linf[!is.finite(L95)]

    } # end calculate Maturity parameters

  } # end generate Mat_age
  if (any(L50>= Linf)) {
    if (msg) message("Note: Some samples of L50 are above Linf. Defaulting to 0.8*Linf")
    L50[L50>=Linf] <- 0.8* Linf[L50>=Linf]
  }
  if (any(L95> Linf)) {
    if (msg)  message("Note: Some samples of L95 are above Linf. Defaulting to 0.99*Linf")
    L95[L95> Linf] <- 0.99* Linf[L95> Linf]
  }
  L50_95 <- L95 - L50

  # --- Calculate Age-at-Maturity ----
  if (exists('ageMarray', inherits=FALSE)) { # check dimensions
    if (!all(dim(ageMarray) == c(nsim, proyears+nyears))) stop('"ageMarray" must be dimensions: nsim, nyears+proyers')
  }
  if (exists('age95array', inherits=FALSE)) { # check dimensions
    if (!all(dim(age95array) == c(nsim, proyears+nyears))) stop('"age95array" must be dimensions: nsim, nyears+proyers')
  }

  if (!exists("ageMarray", inherits=FALSE))
    ageMarray <- -((log(1 - L50array/Linfarray))/Karray) + t0array
  ageMarray[ageMarray < 0] <- 0  # age at maturity must be at least 0
  if (!exists("age95array", inherits=FALSE))
    age95array <- -((log(1 - L95array/Linfarray))/Karray) + t0array
  age95array[age95array < 1] <- 1  # must be greater than 0 and ageMarray
  if (any(ageMarray >= maxage-1)) {
    if (msg) message("Note: Some samples of age of maturity are above 'maxage'-1. Defaulting to maxage-1")
    ageMarray[ageMarray >= (maxage-1)] <- maxage - 1
  }

  if (any(age95array >= maxage)) {
    if (msg) message("Note: Some samples of age of 95 per cent maturity are above 'maxage'. Defaulting to maxage")
    age95array[age95array >= maxage] <- maxage
  }

  # ---- Generate Maturity-at-Age array ----
  if (!exists("Mat_age", inherits=FALSE)) {
    Mat_age <- array(NA, dim=c(nsim, n_age, nyears+proyears))
    for (yr in 1:(nyears+proyears)) {
      # Maturity at age array by year
      Mat_age[,,yr] <- 1/(1 + exp(-log(19) * ((Agearray - ageMarray[,yr])/(age95array[,yr] - ageMarray[,yr]))))
    }
  }

  # # ---- Calculate M-at-Age from M-at-Length (if provided in cpars) ----
  # if (exists("M_at_Length", inherits=FALSE)) {  # M-at-length data.frame has been provided in cpars
  #   MatLen <- matrix(NA, nsim, nrow(M_at_Length))
  #   MatLen[,1] <- runif(nsim, min(M_at_Length[1,2:3]), max(M_at_Length[1,2:3]))
  #   for (k in 1:nsim) {
  #     for (X in 2:nrow(M_at_Length)) {
  #       val <- (MatLen[k,1] - min(M_at_Length[1,2:3]))/ diff(t(M_at_Length[1,2:3]))
  #       MatLen[k,X] <- min(M_at_Length[X,2:3]) + diff(t(M_at_Length[X,2:3]))*val
  #     }
  #   }
  #
  #   # Calculate M at age
  #   Mage <- matrix(NA, nsim, n_age)
  #   for (sim in 1:nsim) {
  #     ind <- findInterval(Len_age[sim,,nyears], M_at_Length[,1])
  #     Mage[sim, ] <- MatLen[sim, ind]
  #   }
  #   stop()
  # }

  # ---- M-at-age in cpars -----
  if (!is.null(cpars$M_ageArray)) {
    M_ageArray <- cpars$M_ageArray
    if (!all(dim(M_ageArray) == c(nsim, n_age, proyears+nyears)))
      stop("'M_ageArray' must be array with dimensions: nsim, maxage+1, nyears + proyears but has dimensions: ",
           paste(dim(M_ageArray), collapse=" "))
    if(msg & is.null(cpars$M)) message("M_ageArray has been provided in OM@cpars. Ignoring OM@M and OM@Msd")
    Msd <- rep(0, nsim)

    # set M to mean M for mature age-classes in year=nyears
    ageatMaturity <- ceiling(ageMarray[,nyears] + 1 )
    M <- apply(M_ageArray[,ageatMaturity,nyears], 1, mean)
  }

  # ---- Mean Natural mortality by simulation and year ----
  if (is.null(cpars$Marray) & !is.null(cpars$M_ageArray)) {
    # Mean M-at-age needs to be calculated
    Marray <- matrix(NA, nsim, nyears+proyears)
    for (yr in 1:(nyears+proyears)) {
      for (sim in 1:nsim) {
        Marray[sim, yr] <- mean(M_ageArray[sim, (ageMarray[sim,yr]+1):n_age,yr])
      }
    }
  } else {
    Marray <- cpars$Marray
  }
  if (is.null(Marray)) {
    # M by sim and year according to gradient and inter annual variability
    Marray <- GenerateRandomWalk(M, Msd, nyears + proyears, nsim, Mrand)
  }

  # ---- Natural mortality by simulation, age and year ----
  if (!exists("M_ageArray", inherits=FALSE)) { # only calculate M_ageArray if it hasn't been specified in cpars
    M_ageArray <- array(NA, dim=c(nsim, n_age, nyears + proyears))
    ind <- as.matrix(expand.grid(1:nsim, 1:n_age, 1:(nyears+proyears)))
    M_ageArray[ind] <- Marray[ind[,c(1,3)]]

  }

  # ---- Sample Discard Mortality ----
  Fdisc <- sample_unif('Fdisc', cpars, Stock, nsim)

  # Check if M-at-age is constant that Maxage makes sense
  if (all(M_ageArray[1,,1] == mean(M_ageArray[1,,1])) & all(M !=0)) { # constant M at age
    calcMax <- ceiling(-log(0.01)/(min(M)))        # Age at which 1% of cohort survives
    if (maxage < 0.95*calcMax && msg) {
      message("Note: Maximum age (",
              maxage,
              ") is lower than assuming 1% of cohort survives to maximum age (",
              calcMax,
              ")")
    }
  }

  # ---- Sample Spatial Parameters ----
  Frac_area_1 <- sample_unif('Frac_area_1', cpars, Stock, nsim)
  Prob_staying <- sample_unif('Prob_staying', cpars, Stock, nsim)
  Size_area_1 <- sample_unif('Size_area_1', cpars, Stock, nsim)

  if (is.null(cpars$mov)) {
    # Check spatial 1 isn't zero, unless movement matrix passed in cpars
    if (max(Size_area_1) == 0) stop("Size_area_1 must be > 0", call. = FALSE)
    if (max(Frac_area_1) == 0) stop("Frac_area_1 must be > 0", call. = FALSE)
    if (max(Prob_staying) == 0) stop("Prob_staying must be > 0", call. = FALSE)

    if (max(Size_area_1) >= 1) stop("Size_area_1 must be < 1", call. = FALSE)
    if (max(Frac_area_1) >= 1) stop("Frac_area_1 must be < 1", call. = FALSE)
    if (max(Prob_staying) >= 1) stop("Prob_staying must be < 1", call. = FALSE)
  } else {
    if (!all(c('Frac_area_1', 'Size_area_1', 'Prob_staying') %in% names(cpars)))
      Size_area_1 <- Frac_area_1 <- Prob_staying <- rep(0,nsim)
  }
  # --- Calculate movement ----
  initdist <- Pinitdist <- NULL
  if (is.null(cpars$mov)) { # movement matrix has not been passed in cpars
    if(msg) message("Optimizing for user-specified movement")
    nareas <- 2 # default is a 2 area model
    mov1 <- array(t(sapply(1:nsim, getmov2, Frac_area_1 = Frac_area_1,
                           Prob_staying = Prob_staying)), dim = c(nsim, nareas, nareas))
    mov <- array(NA,c(nsim,n_age,nareas,nareas))
    mind <- as.matrix(expand.grid(1:nsim,1:n_age,1:nareas,1:nareas))
    mov[mind] <- mov1[mind[,c(1,3,4)]]

    initdist <- array(0,c(nsim,n_age,nareas))
    initdist[,,1]<- Frac_area_1
    initdist[,,2]<- 1- Frac_area_1 # spatial distribution of age classes in initial year
    Asize <- cbind(Size_area_1, 1 - Size_area_1)
  } else {
    mov <- cpars$mov
    nareas <- dim(mov)[3]
    if (!is.null(cpars$initdist)) {
      initdist <- cpars$initdist
    } else {
      # if mov is specified need to calculate age-based spatial distribution (Pinitdist to initdist)
      if(msg) message("Custom movement matrix detected in cpars: simulating movement among ", nareas," areas")
      if(is.na(dim(mov)[5])) {
        # movement constant across years
        mind <- as.matrix(expand.grid(1:nsim,1,1:nareas,1:nareas))
      } else {
        # variable movement across years
        mind<-as.matrix(expand.grid(1:nsim,1,1:nareas,1:nareas, 1)) # movement for 1st year
      }
      movedarray<-array(0,c(nsim,nareas,nareas))
      Pinitdist<-array(1/nareas,c(nsim,nareas)) # population distribution by area
      for(i in 1:100){ # convergence in initial distribution is assumed to occur in 100 iterations (generally overkill)
        # distribution in from areas multiplied by movement array
        movedarray[mind[,c(1,3,4)]]<-Pinitdist[mind[,c(1,3)]]*mov[mind]
        Pinitdist<-apply(movedarray,c(1,3),sum) # add over to areas
      }

    }
    if(!is.null(cpars$Asize)) {
      Asize <- cpars$Asize
      if (is.null(dim(Asize))) {
        Asize <- t(replicate(nsim, Asize) )
      }
    } else {
      if(msg) message('cpars$Asize is not specified, assuming all areas equal size')
      Asize <- matrix(1/nareas, nrow=nsim, ncol=nareas)
    }

    if (dim(Asize)[2]!=nareas) {
      if(msg) message('Asize is not length "nareas", assuming all areas equal size')
      Asize <- matrix(1/nareas, nrow=nsim, ncol=nareas)
    }
    colnames(Asize) <- NULL
  }

  if(is.na(dim(mov)[5])) { # movement matrix only specified for one year
    mov <- array(mov, dim=c(dim(mov), nyears+proyears))
  }
  # check dimensions
  if (any(dim(mov) != c(nsim,n_age,nareas,nareas, nyears+proyears)))
    stop('cpars$mov must be array with dimensions: \nc(nsim, maxage+1, nareas, nareas) \nOR \nc(nsim, maxage+1, nareas, nareas, nyears+proyears)', call.=FALSE)

  StockOut <- list()
  StockOut$maxage <- maxage
  StockOut$R0 <- R0
  StockOut$M <- M
  StockOut$Msd <- Msd
  StockOut$SRrel <- SRrel
  StockOut$procsd <- procsd
  StockOut$AC <- AC
  StockOut$procmu <- procmu
  StockOut$Perr_y <- Perr_y
  StockOut$hs <- hs
  StockOut$D <- D
  StockOut$Mrand <- Mrand
  StockOut$Linfrand <- Linfrand
  StockOut$Krand <- Krand
  StockOut$Linf <- Linf
  StockOut$Linfsd <- Linfsd
  StockOut$K <- K
  StockOut$Ksd <- Ksd
  StockOut$Len_age <- Len_age
  StockOut$maxlen <- maxlen
  StockOut$t0 <- t0
  StockOut$Linfarray <- Linfarray
  StockOut$Karray <- Karray
  StockOut$Agearray <- Agearray
  StockOut$Frac_area_1 <- Frac_area_1
  StockOut$Prob_staying <- Prob_staying
  StockOut$Size_area_1 <- Size_area_1
  StockOut$Fdisc <- Fdisc
  StockOut$ageMarray <- ageMarray
  StockOut$age95array <- age95array
  StockOut$Marray <- Marray
  StockOut$M_ageArray <- M_ageArray
  StockOut$t0array <- t0array
  StockOut$Len_age <- Len_age
  StockOut$a <- Wa
  StockOut$b <- Wb
  StockOut$Wt_age <- Wt_age
  StockOut$L50 <- L50
  StockOut$L50array <- L50array
  StockOut$L95 <- L95
  StockOut$L50_95 <- L50_95
  StockOut$L95array <- L95array
  Mat_age[,1,] <- 0 # age-0 must not be mature
  StockOut$Mat_age <- Mat_age
  StockOut$LenCV <- LenCV
  StockOut$LatASD <- LatASD
  StockOut$CAL_binsmid <- CAL_binsmid
  StockOut$CAL_bins <- CAL_bins
  StockOut$binWidth <- binWidth
  StockOut$nCALbins <- nCALbins
  StockOut$initdist <- initdist
  StockOut$mov <- mov
  StockOut$Pinitdist <- Pinitdist
  StockOut$Asize <- Asize
  StockOut$nareas <- nareas
  StockOut
}



#' Sample Fleet Parameters
#'
#' @param Fleet An object of class 'Fleet' or class 'OM'
#' @param Stock An object of class 'Stock' or a list of sampled Stock parameters.
#' Ignored if 'Fleet' is class 'OM'
#' @param nsim Number of simulations. Ignored if 'Fleet' is class 'OM'
#' @param nyears Number of historical years. Ignored if 'Fleet' is class 'OM'
#' @param proyears Number of projection years. Ignored if 'Fleet' is class 'OM'
#' @param cpars Optional named list of custom parameters. Ignored if 'Fleet' is class 'OM'
#' @param msg Logical. Print messages?
#'
#' @keywords internal
#'
#' @return A named list of sampled Fleet parameters
#' @export
#'
SampleFleetPars <- function(Fleet, Stock=NULL, nsim=NULL, nyears=NULL,
                            proyears=NULL, cpars=NULL, msg=TRUE) {
  if (class(Fleet) != "Fleet" & class(Fleet) != "OM")
    stop("First argument must be class 'Fleet' or 'OM'")

  if (class(Fleet) != "OM" & class(Stock) != "Stock" & class(Stock) != "list")
    stop("Must provide 'Stock' object", call.=FALSE)

  # Get custom pars if they exist
  if (class(Fleet) == "OM" && length(Fleet@cpars) > 0 && is.null(cpars))
    cpars <- SampleCpars(Fleet@cpars, Fleet@nsim, silent=!msg)  # custom parameters exist in Stock/OM object

  if (class(Fleet) == "OM") {
    nsim <- Fleet@nsim
    nyears <- Fleet@nyears
    proyears <- Fleet@proyears
    StockPars <- SampleStockPars(Fleet, nsim, nyears, proyears, cpars, msg=msg)
  }

  if (class(Stock) == "Stock") {
    # Sample Stock Pars - need some to calculate selectivity at age and length
    StockPars <- SampleStockPars(Stock, nsim, nyears, proyears, cpars, msg=msg)
  }
  if (class(Stock) == 'list') StockPars <- Stock

  maxage <- StockPars$maxage
  Fleetout <- list()
  n_age <- maxage + 1
  # --- Sample Historical Fishing Effort ----
  Esd <- sample_unif('Esd', cpars, Fleet, nsim)
  EffLower <- Fleet@EffLower
  EffUpper <- Fleet@EffUpper
  EffYears <- Fleet@EffYears

  Find <- cpars$Find
  if (is.null(Find)) { # not in cpars
    Deriv <- getEffhist(Esd, nyears,
                        EffYears = EffYears,
                        EffLower = EffLower,
                        EffUpper = EffUpper)  # Historical fishing effort
    Find <- Deriv[[1]]  # Calculate fishing effort rate
  } else { # Find in cpars
    EffLower <- EffUpper <- EffYears <- 0
  }

  dFfinal <- cpars$dFfinal
  if (is.null(dFfinal)) {
    if (exists("Deriv", inherits = FALSE)) {
      dFfinal <- Deriv[[2]]  # Final gradient in fishing effort yr-1
    } else {
      dFfinal <- rep(NA, nsim)
    }
  }

  if (any(dim(Find) != c(nsim, nyears)))
    stop("Find must be matrix with dimensions: nsim (", nsim, "), nyears (", nyears, ") but is: ", paste(dim(Find), ""))

  Fleetout$Esd <- Esd
  Fleetout$Find <- Find
  Fleetout$dFfinal <- dFfinal

  # ---- Spatial Targetting ----
  # spatial targeting Ba^targeting param
  Spat_targ <- sample_unif('Spat_targ', cpars, Fleet, nsim)
  Fleetout$Spat_targ <- Spat_targ

  # ---- Sample fishing efficiency parameters ----
  # interannual variability in catchability
  qinc <- sample_unif('qinc', cpars, Fleet, nsim)
  qcv <- sample_unif('qcv', cpars, Fleet, nsim)

  # ---- Simulate future variability in fishing efficiency ----
  qmu <- -0.5 * qcv^2  # Mean
  # Variations in interannual variation for future projections
  qvar <- cpars$qvar
  if (is.null(qvar))
    qvar <- array(exp(rnorm(proyears * nsim, rep(qmu, proyears), rep(qcv, proyears))), c(nsim, proyears))

  FinF <- Find[, nyears]  # Effort in final historical year
  Fleetout$qinc <- qinc
  Fleetout$qcv <- qcv
  Fleetout$qvar <- qvar
  Fleetout$FinF <- FinF

  # ---- Selectivity Curve ----
  if (any(c('V', 'V2', 'SLarray', 'retA', 'retL')%in% names(cpars)))
    Fleet@isRel <- FALSE

  # are selectivity parameters relative to size at maturity?
  chk <- class(Fleet@isRel)
  if (length(Fleet@isRel) < 1) Fleet@isRel <- "true"
  if (chk == "character") {
    chkRel <- substr(tolower(Fleet@isRel), 1,1)
    if (chkRel == "t" | Fleet@isRel == "1") multi <- StockPars$L50
    if (chkRel == "f" | Fleet@isRel == "0") multi <- 1
  }
  if (chk == "numeric") {
    if (Fleet@isRel == 1) multi <- StockPars$L50
    if (Fleet@isRel == 0) multi <- 1
  }
  if (chk == "logical") {
    if (Fleet@isRel) multi <- StockPars$L50
    if (!Fleet@isRel) multi <- 1
  }

  if (!any(is.null(c(cpars[['L5']],
                     cpars[['LFS']],
                     cpars[['Vmaxlen']],
                     cpars[['LR5']],
                     cpars[['LFR']],
                     cpars[['Rmaxlen']]))) & all(multi!=1))
    stop("Selectivity parameters provided in cpars must be absolute values. Is Fleet@isRel == 'FALSE'?")

  L5 <- sample_unif('L5', cpars, Fleet, nsim) * multi
  LFS <- sample_unif('LFS', cpars, Fleet, nsim) * multi
  Vmaxlen <- sample_unif('Vmaxlen', cpars, Fleet, nsim)

  L5_y <- cpars$L5_y
  LFS_y <- cpars$LFS_y
  Vmaxlen_y <- cpars$Vmaxlen_y

  if (is.null(L5_y)) L5_y <- matrix(L5, nrow =nsim , ncol = nyears + proyears, byrow = FALSE)
  if (is.null(LFS_y)) LFS_y <- matrix(LFS, nrow = nsim, ncol = nyears + proyears, byrow = FALSE)
  if (is.null(Vmaxlen_y)) Vmaxlen_y <- matrix(Vmaxlen, nrow = nsim, ncol = nyears + proyears, byrow = FALSE)

  if (!is.null(cpars$SLarray) & !is.null(cpars[['V']])) {# both selectivity-at-length and -at-age in cpars
    SLarray <- cpars$SLarray
    V <- cpars[['V']]
  } else if (!is.null(cpars$SLarray) & is.null(cpars[['V']])) { # SLarray in cpars
    # Calculate selectivity parameters
    SLarray <- cpars$SLarray
    nbins <- length(StockPars$CAL_binsmid)
    if (any(dim(SLarray) != c(nsim, nbins, nyears+proyears)))
      stop("SLarray must be dimensions c(nsim, length(CAL_binsmid), nyears+proyears)")
    if (!all(c('L5_y', 'LFS_y', 'Vmaxlen_y') %in% names(cpars))) {
      # need to calculate these
      for (yr in 1:(nyears+proyears)) {
        b_ind <- rep(NA, nsim)
        for (i in 1:nsim) {
          temp <- min(which(SLarray[i,,yr]>=0.05))
          if (length(temp)<1) temp <- 1
          if (is.na(temp)) temp <- 1
          b_ind[i] <- temp
        }

        L5_y[,yr] <- StockPars$CAL_binsmid[b_ind]
        b_ind <- apply(SLarray[,,yr], 1, which.max)
        LFS_y[,yr] <- StockPars$CAL_binsmid[b_ind]
        temp <- abs(replicate(nsim, StockPars$CAL_binsmid) - StockPars$Linf)
        b_ind <- apply(temp, 2, which.min)
        Vmaxlen_y[,yr] <- SLarray[,b_ind,yr][1,]
      }
    } # end calculate L5_y etc
  } else if (is.null(cpars$SLarray) & !is.null(cpars[['V']])) {
    # update selectivity parameters
    V <- cpars[['V']]
    if (dim(V)[3] == nyears) {
      Dims <- dim(V)
      v2 <- array(V[,,nyears], dim=c(Dims[1], Dims[2], proyears))
      V <- abind::abind(V, v2, along=3)
    }
    if(any(dim(V)!= c(nsim, n_age, nyears + proyears)))
      stop('V must be dimensions: nsim, n_age, nyears + proyears')
    if (!all(c('L5_y', 'LFS_y', 'Vmaxlen_y') %in% names(cpars))) {
      # need to calculate these from V
      VB <- function(Linf, K, t0, age) Linf * (1-exp(-K*(age-t0)))
      for (yr in 1:(nyears+proyears)) {
        for (s in 1:nsim) {
          xout <- seq(1, n_age, by=0.1)
          tt <- approx(V[s,,yr], xout=xout)
          age5 <- tt$x[min(which(tt$y >=0.05))]-1
          L5_y[s,yr] <- VB(StockPars$Linfarray[s,yr],
                           StockPars$Karray[s,yr],
                           StockPars$t0array[s,yr], age5)
          ageFS <- tt$x[which.max(tt$y)]-1
          if (ageFS == age5) ageFS <- age5 + 1
          LFS_y[s, yr] <- VB(StockPars$Linfarray[s,yr],
                             StockPars$Karray[s,yr],
                             StockPars$t0array[s,yr], ageFS)
          Vmaxlen_y[s, yr] <- V[s, n_age, yr]
        }
      }
    }
  }

  if (!exists("SLarray", inherits = FALSE)) { # selectivity-at-length hasn't been defined yet
    # calculate SLarray
    nCALbins <- length(StockPars$CAL_binsmid)
    CAL_binsmidMat <- matrix(StockPars$CAL_binsmid, nrow=nsim, ncol=length(StockPars$CAL_binsmid), byrow=TRUE)
    SLarray <- array(NA, dim=c(nsim, nCALbins, nyears+proyears)) # Selectivity-at-length
    Vmaxlen_y[Vmaxlen_y<=0] <- tiny
    for (yr in 1:(nyears+proyears)) {

      srs <- (StockPars$Linf - LFS_y[,yr]) / ((-log(Vmaxlen_y[,yr],2))^0.5)

      srs[!is.finite(srs)] <- Inf
      sls <- (LFS_y[,yr] - L5_y[, yr]) /((-log(0.05,2))^0.5)
      SLarray[,, yr] <- t(sapply(1:nsim, getsel, lens=CAL_binsmidMat, lfs=LFS_y[,yr], sls=sls, srs=srs))
    }
  }

  # Check LFS is greater than L5
  chk <- sum(apply(L5_y > LFS_y, 2, prod) != 0)
  if (chk > 0) stop("L5 is greater than LFS in ", chk, ' simulations')

  if (!exists("V", inherits = FALSE)) {
    # calculate selectivity-at-age from selectivity-at-length
    VList <- lapply(1:nsim, calcV, Len_age=StockPars$Len_age,
                    LatASD=StockPars$LatASD, SLarray=SLarray,
                    n_age=n_age, nyears=nyears, proyears=proyears,
                    CAL_binsmid=StockPars$CAL_binsmid)
    V <- aperm(array(as.numeric(unlist(VList, use.names=FALSE)), dim=c(n_age, nyears+proyears, nsim)), c(3,1,2))
  }

  # ---- Retention Curve ----
  LR5 <- sample_unif('LR5', cpars, Fleet, nsim) * multi
  LFR <- sample_unif('LFR', cpars, Fleet, nsim) * multi
  Rmaxlen <- sample_unif('Rmaxlen', cpars, Fleet, nsim)
  DR <- sample_unif('DR', cpars, Fleet, nsim)
  if (all(is.na(DR))) DR <- rep(0, nsim)

  LR5_y <- matrix(LR5, nrow = nsim, ncol = nyears + proyears, byrow = FALSE)
  LFR_y <- matrix(LFR, nrow = nsim, ncol = nyears + proyears, byrow = FALSE)
  Rmaxlen_y <- matrix(Rmaxlen, nrow = nsim, ncol = nyears + proyears, byrow = FALSE)

  DR_y <- cpars$DR_y
  if (is.null(DR_y))
    DR_y <- matrix(DR, nrow = nsim, ncol = nyears + proyears, byrow = FALSE)

  if (!is.null(cpars$retL) & !is.null(cpars$retA)) {# both retention-at-length and -at-age in cpars
    retL <- cpars$retL
    retA <- cpars$retA
  } else if (!is.null(cpars$retL) & is.null(cpars$retA)) { # retL in cpars
    # Calculate retention parameters
    retL <- cpars$retL
    nbins <- length(StockPars$CAL_binsmid)
    if (any(dim(retL) != c(nsim, nbins, nyears+proyears)))
      stop("retL must be dimensions c(nsim, length(CAL_binsmid), nyears+proyears)")
    if (!all(c('LR5_y', 'LFR_y', 'Rmaxlen_y') %in% names(cpars))) {
      # need to calculate these
      for (yr in 1:(nyears+proyears)) {
        b_ind <- rep(NA, nsim)
        for (i in 1:nsim) {
          temp <- min(which(SLarray[i,,yr]>=0.05))
          if (length(temp)<1) temp <- 1
          if (is.na(temp)) temp <- 1
          b_ind[i] <- temp
        }
        LR5_y[,yr] <- StockPars$CAL_binsmid[b_ind]
        b_ind <- apply(retL[,,yr], 1, which.max)
        LFR_y[,yr] <- StockPars$CAL_binsmid[b_ind]
        temp <- abs(replicate(nsim, StockPars$CAL_binsmid) - StockPars$Linf)
        b_ind <- apply(temp, 2, which.min)
        Rmaxlen_y[,yr] <- retL[,b_ind,yr][1,]
      }
      LFR_y[LFR_y<=LR5_y] <- LR5_y[LFR_y<=LR5_y]  +1
    } # end calculate LR5_y etc
  } else if (is.null(cpars$retL) & !is.null(cpars$retA)) {
    # update retention parameters
    retA <- cpars$retA
    if (dim(retA)[3] == nyears) {
      Dims <- dim(V)
      retA2 <- array(retA[,,nyears], dim=c(Dims[1], Dims[2], proyears))
      retA <- abind::abind(retA, retA2, along=3)
    }
    if(any(dim(retA)!= c(nsim, n_age, nyears + proyears)))
      stop('retA must be dimensions: nsim, n_age, nyears + proyears')
    if (!all(c('LR5_y', 'LFR_y', 'Rmaxlen_y') %in% names(cpars))) {
      # need to calculate these from retA
      VB <- function(Linf, K, t0, age) Linf * (1-exp(-K*(age-t0)))
      for (yr in 1:(nyears+proyears)) {
        for (s in 1:nsim) {
          xout <- seq(1, n_age, by=0.1)
          tt <- approx(retA[s,,yr], xout=xout)
          age5 <- tt$x[min(which(tt$y >=0.05))]-1
          LR5_y[s,yr] <- VB(StockPars$Linfarray[s,yr],
                            StockPars$Karray[s,yr],
                            StockPars$t0array[s,yr], age5)
          ageFS <- tt$x[which.max(tt$y)]-1
          if (ageFS == age5) ageFS <- age5 + 1
          LFR_y[s, yr] <- VB(StockPars$Linfarray[s,yr],
                             StockPars$Karray[s,yr],
                             StockPars$t0array[s,yr], ageFS)
          Rmaxlen_y[s, yr] <- retA[s, n_age, yr]
        }
      }
    }
  }

  if (!exists("retL", inherits = FALSE)) { # retention-at-length hasn't been defined yet
    # calculate retL
    nCALbins <- length(StockPars$CAL_binsmid)
    CAL_binsmidMat <- matrix(StockPars$CAL_binsmid, nrow=nsim, ncol=length(StockPars$CAL_binsmid), byrow=TRUE)
    retL <- array(NA, dim=c(nsim, nCALbins, nyears+proyears)) # Selectivity-at-length
    Rmaxlen_y[Rmaxlen_y<=0] <- tiny
    for (yr in 1:(nyears+proyears)) {
      srs <- (StockPars$Linf - LFR_y[,yr]) / ((-log(Rmaxlen_y[,yr],2))^0.5)
      srs[!is.finite(srs)] <- Inf
      sls <- (LFR_y[,yr] - LR5_y[, yr]) /((-log(0.05,2))^0.5)
      retL[,, yr] <- t(sapply(1:nsim, getsel, lens=CAL_binsmidMat, lfs=LFR_y[,yr], sls=sls, srs=srs))
    }
  }

  # Check LFR is greater than LR5
  chk <- sum(apply(LR5_y > LFR_y, 2, prod) != 0)
  if (chk > 0) stop("LR5 is greater than LFR in ", chk, ' simulations')

  if (!exists("retA", inherits = FALSE)) {
    # calculate retention-at-age from retention-at-length
    VList <- lapply(1:nsim, calcV, Len_age=StockPars$Len_age,
                    LatASD=StockPars$LatASD, SLarray=retL,
                    n_age=n_age, nyears=nyears, proyears=proyears,
                    CAL_binsmid=StockPars$CAL_binsmid)
    retA <- aperm(array(as.numeric(unlist(VList, use.names=FALSE)), dim=c(n_age, nyears+proyears, nsim)), c(3,1,2))
  }

  V2 <- cpars$V2
  SLarray2 <- cpars$SLarray2
  if (is.null(V2)) V2 <- V
  if (is.null(SLarray2)) SLarray2 <- SLarray

  # Apply general discard rate
  if (is.null(cpars$retA)) {
    dr <- aperm(abind::abind(rep(list(DR_y), n_age), along=3), c(1,3,2))
    retA <- (1-dr) * retA
  }
  if (is.null(cpars$retL)) {
    dr <- aperm(abind::abind(rep(list(DR_y), StockPars$nCALbins), along=3), c(1,3,2))
    retL <- (1-dr) * retL
  }

  Fdisc <- StockPars$Fdisc

  Fdisc_array1 <- cpars$Fdisc_array1
  if (is.null(Fdisc_array1)) Fdisc_array1 <- array(Fdisc, dim=c(nsim, n_age, nyears+proyears))
  if (is.null(cpars$V) & is.null(cpars$SLarray)) {
    # update realized vulnerability curve with retention and dead discarded fish
    V <- V * (retA + (1-retA)*Fdisc_array1) # Realised selection at age
  }

  Fdisc_array2 <- cpars$Fdisc_array2
  if (is.null(Fdisc_array2)) Fdisc_array2 <- array(Fdisc, dim=c(nsim, StockPars$nCALbins, nyears+proyears))
  if (is.null(cpars$SLarray)) {
    SLarray <- SLarray2 * (retL + (1-retL)*Fdisc_array2) # Realised selection at length
  }

  # Realised Retention curves
  if (is.null(cpars$retA) & is.null(cpars$retL)) {
    retA <- retA * V2
  }
  if (is.null(cpars$retL)) {
    retL <- retL * SLarray2
  }

  # ---- Existing MPA ----
  if (inherits(Fleet@MPA, 'matrix')) {
    warning('This OM is from a previous version. OM@MPA is now a logical instead of matrix. Assuming no existing MPA')
    MPA <- FALSE
  } else {
    if (length(Fleet@MPA)<1) Fleet@MPA <- FALSE
    MPA <- as.logical(Fleet@MPA)

  }

  Fleetout$Fdisc <- Fdisc
  Fleetout$Fdisc_array1 <- Fdisc_array1
  Fleetout$Fdisc_array2 <- Fdisc_array2
  Fleetout$LR5_y <- LR5_y
  Fleetout$LFR_y <- LFR_y
  Fleetout$Rmaxlen_y <- Rmaxlen_y
  Fleetout$DR_y <- DR_y
  Fleetout$retA <- retA  # retention-at-age array - nsim, maxage, nyears+proyears
  Fleetout$retL <- retL  # retention-at-length array - nsim, nCALbins, nyears+proyears
  Fleetout$L5_y <- L5_y
  Fleetout$LFS_y <- LFS_y
  Fleetout$Vmaxlen_y <- Vmaxlen_y
  Fleetout$V <- V  # realized vulnerability-at-age
  Fleetout$SLarray <- SLarray # realized vulnerability-at-length
  Fleetout$V2 <- V2 # original vulnerablity-at-age curve
  Fleetout$SLarray2 <- SLarray2 # original vulnerablity-at-length curve
  Fleetout$MPA <- MPA

  # check V
  if (sum(apply(V, c(1,3), max) <0.01)) {
    maxV <- apply(V, c(1,3), max)
    fails <- which(maxV < 0.01, arr.ind = TRUE)
    sims <- unique(fails[,1])
    yrs <- unique(fails[,2])
    warning("Vulnerability (V) is <0.01 for all ages in:\nsims:", sims, "\nyears:", yrs, "\n", call. = FALSE)
    # warning('Check selectivity parameters. Is Fleet@isRel set correctly?', call.=FALSE)
  }
  Fleetout
}


sample_lnorm <- function(par, cpars, Obj, nsim, altpar=NULL) {
  if (!is.null(cpars[[par]])) return(cpars[[par]])
  if (!is.null(altpar)) par <- altpar
  val <- slot(Obj, par)
  rlnorm(nsim, mconv(1, val), sdconv(1, val))

}

#' Sample Observation Parameters
#'
#' @param Obs An object of class 'Obs' or class 'OM'
#' @param nsim Number of simulations. Ignored if 'Obs' is class 'OM'
#' @param cpars Optional named list of custom parameters. Ignored if 'OM' is class 'OM'
#' @param Stock An object of class 'Stock' or a list of sampled Stock parameters. Not essential.
#' @param nyears Number of historical years.
#' @param proyears Number of projection years. Not essential.
#' @keywords internal
#' @return A named list of sampled Observation parameters
#' @export
#'
SampleObsPars <- function(Obs, nsim=NULL, cpars=NULL, Stock=NULL,
                          nyears=NULL, proyears=NULL){
  if (class(Obs) != "Obs" & class(Obs) != "OM")
    stop("First argument must be class 'Obs' or 'OM'")
  if (class(Obs) == "OM") {
    nsim <- Obs@nsim
    nyears <- Obs@nyears
    proyears <- Obs@proyears
  }

  # Get custom pars if they exist
  if (class(Obs) == "OM" && length(Obs@cpars) > 0 && is.null(cpars))
    cpars <- SampleCpars(Obs@cpars, Obs@nsim)  # custom parameters exist in OM object

  if (class(Stock) == "Stock") {
    # Sample Stock Pars - need some to calculate selectivity at age and length
    StockPars <- SampleStockPars(Stock, nsim, nyears, proyears, cpars, msg=FALSE)
  } else if (is.null(Stock)) StockPars <- NULL
  if (class(Stock) == 'list') StockPars <- Stock

  ObsOut <- list()

  # ---- Catch Error ----
  # Sampled catch observation error (lognormal sd)
  Csd <- sample_unif('Csd', cpars, Obs, nsim, 'Cobs')
  ObsOut$Csd <- Csd

  # Sampled catch bias (log normal sd)
  Cbias <- sample_lnorm('Cbias', cpars, Obs, nsim, 'Cbiascv')
  ObsOut$Cbias <- Cbias

  # Generate catch obs error by year
  Cerr_y <- cpars$Cerr_y
  if (is.null(Cerr_y)) {
    Cerr_y <- array(rlnorm((nyears + proyears) * nsim,
                           mconv(1, rep(ObsOut$Csd, (nyears + proyears))),
                           sdconv(1, rep(ObsOut$Csd, nyears + proyears))),
                    c(nsim, nyears + proyears))
  }
  ObsOut$Cerr_y <- Cerr_y

  Cobs_y <- cpars$Cobs_y
  if (is.null(Cobs_y)) {
    Cobs_y <- ObsOut$Cbias * ObsOut$Cerr_y
  }
  ObsOut$Cobs_y <- Cobs_y

  # ---- Catch-at-Age Observations ----
  CAA_nsamp <- sample_unif('CAA_nsamp', cpars, Obs, nsim)
  CAA_ESS <- sample_unif('CAA_ESS', cpars, Obs, nsim)
  ObsOut$CAA_nsamp <- CAA_nsamp
  ObsOut$CAA_ESS <- CAA_ESS

  # ---- Catch-at-Length Observations ----
  CAL_nsamp <- sample_unif('CAL_nsamp', cpars, Obs, nsim)
  CAL_ESS <- sample_unif('CAL_ESS', cpars, Obs, nsim)
  ObsOut$CAL_nsamp <- CAL_nsamp
  ObsOut$CAL_ESS <- CAL_ESS

  # ---- Index Observation Error -----
  betas <- cpars$betas
  if (is.null(betas)) betas <- cpars$beta

  if (is.null(betas))
    betas <- exp(myrunif(nsim, log(Obs@beta[1]), log(Obs@beta[2])))  # the sampled hyperstability /

  if (!is.null(cpars$I_beta)) {
    ObsOut$I_beta <- cpars$I_beta
  } else {
    ObsOut$I_beta <- betas
  }

  if (!is.null(cpars$SpI_beta)) {
    ObsOut$SpI_beta <- cpars$SpI_beta
  } else {
    ObsOut$SpI_beta <- betas
  }

  if (!is.null(cpars$VI_beta)) {
    ObsOut$VI_beta <- cpars$VI_beta
  } else {
    ObsOut$VI_beta <- betas
  }


  # Sampled index observation error (lognormal sd)
  Isd <- sample_unif('Isd', cpars, Obs, nsim, 'Iobs')
  ObsOut$Isd <- Isd

  Ierr_y <- cpars$Ierr_y
  if (is.null(Ierr_y)) {
    Ierr_y <- array(rlnorm((nyears + proyears) * nsim,
                                  mconv(1, rep(ObsOut$Isd, nyears + proyears)),
                                  sdconv(1, rep(ObsOut$Isd, nyears + proyears))),
                           c(nsim, nyears + proyears))
  }
  SpIerr_y <- cpars$SpIerr_y
  if (is.null(SpIerr_y)) {
    SpIerr_y <- array(rlnorm((nyears + proyears) * nsim,
                                    mconv(1, rep(ObsOut$Isd, nyears + proyears)),
                                    sdconv(1, rep(ObsOut$Isd, nyears + proyears))),
                             c(nsim, nyears + proyears))
  }
  VIerr_y <- cpars$VIerr_y
  if (is.null(VIerr_y)) {
    VIerr_y <- array(rlnorm((nyears + proyears) * nsim,
                            mconv(1, rep(ObsOut$Isd, nyears + proyears)),
                            sdconv(1, rep(ObsOut$Isd, nyears + proyears))),
                     c(nsim, nyears + proyears))
  }

  ObsOut$Ierr_y <- Ierr_y # total index
  ObsOut$SpIerr_y <- SpIerr_y # spawning biomass index
  ObsOut$VIerr_y <- VIerr_y # vulnerable index

  # ----- Depletion Observation ----
  # Depletion obs error
  Derr <- sample_unif('Derr', cpars, Obs, nsim, 'Dobs')
  ObsOut$Derr <- Derr

  # Depletion bias
  Dbias <- sample_lnorm('Dbias', cpars, Obs, nsim, 'Dbiascv')
  ObsOut$Dbias <- Dbias

  Derr_y <- cpars$Derr_y
  if (is.null(Derr_y)) {
    Derr_y <- array(rlnorm((nyears + proyears) * nsim,
                           mconv(1, rep(ObsOut$Derr, nyears + proyears)),
                           sdconv(1, rep(ObsOut$Derr, nyears + proyears))),
                    c(nsim, nyears + proyears)) * Dbias
  }

  ObsOut$Derr_y <- Derr_y

  # ---- M bias ----
  Mbias <- sample_lnorm('Mbias', cpars, Obs, nsim, 'Mbiascv')
  ObsOut$Mbias <- Mbias

  # ---- FMSY_Mbias ----
  FMSY_Mbias <- sample_lnorm('FMSY_Mbias', cpars, Obs, nsim, 'FMSY_Mbiascv')
  ObsOut$FMSY_Mbias <- FMSY_Mbias

  # ---- BMSY_B0bias ----
  BMSY_B0bias <- cpars$BMSY_B0bias
  if (is.null(BMSY_B0bias)) {
    ntest <- 20
    BMSY_B0bias <- array(rlnorm(nsim * ntest,
                                mconv(1, Obs@BMSY_B0biascv), sdconv(1, Obs@BMSY_B0biascv)),
                         dim = c(nsim, ntest))  # trial samples of BMSY relative to unfished
  }
  ObsOut$BMSY_B0bias <- BMSY_B0bias

  # --- Length-at-maturity bias ----
  lenMbias <- sample_lnorm('lenMbias', cpars, Obs, nsim, 'LenMbiascv')
  ObsOut$lenMbias <- lenMbias

  # ---- Length-at-capture bias ----
  LFCbias <- sample_lnorm('LFCbias', cpars, Obs, nsim, 'LFCbiascv')
  ObsOut$LFCbias <- LFCbias

  LFSbias <- sample_lnorm('LFSbias', cpars, Obs, nsim, 'LFSbiascv')
  ObsOut$LFSbias <- LFSbias

  # ---- Abundance Error ----
  # Obs error
  Aerr <- sample_unif('Aerr', cpars, Obs, nsim, 'Btobs')
  ObsOut$Aerr <- Aerr
  # Abundance bias
  Abias <- cpars$Abias
  if (is.null(Abias))
    Abias <-  sample_lnorm('Btbiascv', cpars, Obs, nsim, 'Btbiascv')
  # exp(myrunif(nsim, log(Obs@Btbiascv[1]), log(Obs@Btbiascv[2])))

  ObsOut$Abias <- Abias

  Aerr_y <- cpars$Aerr_y
  if (is.null(Aerr_y))
    Aerr_y <- ObsOut$Abias * array(rlnorm((nyears + proyears) * nsim,
                                          mconv(1, rep(ObsOut$Aerr, nyears + proyears)),
                                          sdconv(1, rep(ObsOut$Aerr, nyears + proyears))),
                                   c(nsim, nyears + proyears))

  ObsOut$Aerr_y <- Aerr_y

  # --- Growth parameters obs error ----
  Kbias <- sample_lnorm('Kbias', cpars, Obs, nsim, 'Kbiascv')
  ObsOut$Kbias <- Kbias

  t0bias <- sample_lnorm('t0bias', cpars, Obs, nsim, 't0biascv')
  ObsOut$t0bias <- t0bias

  Linfbias <- sample_lnorm('Linfbias', cpars, Obs, nsim, 'Linfbiascv')
  ObsOut$Linfbias <- Linfbias

  # --- Reference Point Bias ----
  Irefbias <- sample_lnorm('Irefbias', cpars, Obs, nsim, 'Irefbiascv')
  ObsOut$Irefbias <- Irefbias

  Crefbias <- sample_lnorm('Crefbias', cpars, Obs, nsim, 'Crefbiascv')
  ObsOut$Crefbias <- Crefbias

  Brefbias <- sample_lnorm('Brefbias', cpars, Obs, nsim, 'Brefbiascv')
  ObsOut$Brefbias <- Brefbias

  # ---- Recruitment Obs error ----
  Recsd <- sample_lnorm('Recsd', cpars, Obs, nsim, 'Recbiascv')
  ObsOut$Recsd <- Recsd

  # Simulate error in observed recruitment index
  Recerr_y <- cpars$Recerr_y
  if (is.null(Recerr_y)) {
    Recerr_y <- array(rlnorm((nyears + proyears) * nsim,
                             mconv(1, rep(ObsOut$Recsd, (nyears + proyears))),
                             sdconv(1, rep(ObsOut$Recsd, nyears + proyears))),
                      c(nsim, nyears + proyears))

  }
  ObsOut$Recerr_y <- Recerr_y

  ObsOut$sigmaRbias <- sample_lnorm('sigmaRbias', cpars, Obs, nsim, 'sigmaRbiascv')

  # ---- Steepness obs bias ----
  hsim <- cpars$hsim
  if (is.null(hsim)) {
    hsim <- rep(NA, nsim)
    cond <- StockPars$hs > 0.6
    hsim[cond] <- 0.2 + rbeta(sum(StockPars$hs > 0.6),
                              alphaconv((StockPars$hs[cond] - 0.2)/0.8,
                                        (1 - (StockPars$hs[cond] - 0.2)/0.8) * Obs@hbiascv),
                              betaconv((StockPars$hs[cond] - 0.2)/0.8,
                                       (1 - (StockPars$hs[cond] - 0.2)/0.8) * Obs@hbiascv)) * 0.8

    hsim[!cond] <- 0.2 + rbeta(sum(StockPars$hs <= 0.6),
                               alphaconv((StockPars$hs[!cond] - 0.2)/0.8,
                                         (StockPars$hs[!cond] - 0.2)/0.8 * Obs@hbiascv),
                               betaconv((StockPars$hs[!cond] - 0.2)/0.8,
                                        (StockPars$hs[!cond] - 0.2)/0.8 * Obs@hbiascv)) * 0.8
    hbias <- hsim/StockPars$hs  # back calculate the simulated bias
  } else {
    hbias <- hsim/StockPars$hs  # back calculate the simulated bias
  }
  ObsOut$hbias <- hbias

  # ---- Effort Obs Error -----
  # Sampled effort observation error (lognormal sd)
  if (length(Obs@Eobs)<2 | all(is.na(Obs@Eobs)) && is.null(cpars$Eerr_y)) {
    message('OM@Eobs not specified. Assuming no observation error on Effort')
    Obs@Eobs <- rep(0,2)
    Obs@Ebiascv <- 0
  }
  Evar <- sample_unif('Eobs', cpars, Obs, nsim)

  ObsOut$Evar <- Evar

  # Sampled effort bias (log normal sd)
  Ebias <- sample_lnorm('Ebias', cpars, Obs, nsim, 'Ebiascv')
  ObsOut$Ebias <- Ebias

  # Generate effort obs error by year
  Eerr_y <- cpars$Eerr_y
  if (is.null(Eerr_y)) {
    Eerr_y <- array(rlnorm((nyears + proyears) * nsim,
                           mconv(1, rep(ObsOut$Evar, (nyears + proyears))),
                           sdconv(1, rep(ObsOut$Evar, nyears + proyears))),
                    c(nsim, nyears + proyears))
  }
  ObsOut$Eerr_y <- Eerr_y

  Eobs_y <- cpars$Eobs_y
  if (is.null(Eobs_y)) {
    Eobs_y <- ObsOut$Ebias * ObsOut$Eerr_y
  }
  ObsOut$Eobs_y <- Eobs_y

  ObsOut
}

#' Sample Implementation Error Parameters
#'
#' @param Imp An object of class 'Imp' or class 'OM'
#' @param nsim Number of simulations. Ignored if 'Imp' is class 'OM'
#' @param cpars Optional named list of custom parameters. Ignored if 'OM' is class 'OM'
#' @param nyears Number of historical years.
#' @param proyears Number of projection years. Not essential.
#' @return A named list of sampled Implementation Error parameters
#' @keywords internal
#' @export
#'
SampleImpPars <- function(Imp, nsim=NULL, cpars=NULL, nyears=NULL, proyears=NULL) {
  if (class(Imp) != "Imp" & class(Imp) != "OM")
    stop("First argument must be class 'Imp' or 'OM'")
  if (class(Imp) == "OM") nsim <- Imp@nsim

  # Get custom pars if they exist
  if (class(Imp) == "OM" && length(Imp@cpars) > 0 && is.null(cpars))
    cpars <- SampleCpars(Imp@cpars, Imp@nsim)  # custom parameters exist in OM object
  if (length(cpars) > 0) { # custom pars exist - assign to function environment
    Names <- names(cpars)
    for (X in 1:length(Names)) assign(names(cpars)[X], cpars[[X]])
  }

  ImpOut <- list()

  # ---- TAC Implementation Error -----
  TACSD <- sample_unif('TACSD', cpars, Imp, nsim)
  ImpOut$TACSD <- TACSD

  TACFrac <- sample_unif('TACFrac', cpars, Imp, nsim)
  ImpOut$TACFrac <- TACFrac

  TAC_y <- cpars$TAC_y
  if (is.null(TAC_y)) {
    TAC_y <- array(rlnorm(proyears * nsim,
                          mconv(ImpOut$TACFrac, ImpOut$TACSD),
                          sdconv(ImpOut$TACFrac, ImpOut$TACSD)),
                   c(nsim, proyears))  # composite of TAC fraction and error
  }
  ImpOut$TAC_y <- TAC_y

  # ---- TAE Implementation Error ----
  TAESD <- sample_unif('TAESD', cpars, Imp, nsim)
  ImpOut$TAESD <- TAESD

  TAEFrac <- sample_unif('TAEFrac', cpars, Imp, nsim)
  ImpOut$TAEFrac <- TAEFrac

  E_y <- cpars$E_y
  if (is.null(E_y)) {
    E_y <- array(rlnorm(proyears * nsim,
                        mconv(ImpOut$TAEFrac, ImpOut$TAESD),
                        sdconv(ImpOut$TAEFrac, ImpOut$TAESD)),
                 c(nsim, proyears))
  }
  ImpOut$E_y <- E_y


  # ---- Size Limit Implementation Error ----
  SizeLimSD <- sample_unif('SizeLimSD', cpars, Imp, nsim)
  ImpOut$SizeLimSD <- SizeLimSD

  SizeLimFrac <- sample_unif('SizeLimFrac', cpars, Imp, nsim)
  ImpOut$SizeLimFrac <- SizeLimFrac

  SizeLim_y <- cpars$SizeLim_y
  if (is.null(SizeLim_y)) {
    SizeLim_y <- array(rlnorm(proyears * nsim,
                        mconv(ImpOut$SizeLimFrac, ImpOut$SizeLimSD),
                        sdconv(ImpOut$SizeLimFrac, ImpOut$SizeLimSD)),
                 c(nsim, proyears))
  }
  ImpOut$SizeLim_y <- SizeLim_y

  ImpOut
}

#' Valid custom parameters (cpars)
#'
#' @param type What cpars to show? 'all', 'Stock', 'Fleet', 'Obs', 'Imp', or 'internal'
#' @param valid Logical. Show valid cpars?
#'
#' @return a HTML datatable with variable name, description and type of valid cpars
#' @export
#'
#' @examples
#' \dontrun{
#' validcpars() # all valid cpars
#'
#' validcpars("Obs", FALSE) # invalid Obs cpars
#' }
#'
validcpars <- function(type=c("all", "Stock", "Fleet", "Obs", "Imp", "internal"),
                       valid=TRUE) {

  Var <- Desc <- NULL # checks
  type <- match.arg(type, choices=c("all", "Stock", "Fleet", "Obs", "Imp", "internal"),
                    several.ok = TRUE )
  if ('all' %in% type) type <- c("Stock", "Fleet", "Obs", "Imp", "internal")

  Valid <- Slot <- Dim <- Description <- NULL

  # cpars_info <- MSEtool:::cpars_info
  # cpars_info <- cpars_info[!duplicated(cpars_info$Slot),] # remove duplicated 'Name'

  cpars_info$type <- NA
  stock_ind <- match(slotNames("Stock"), cpars_info$Var)
  fleet_ind <- match(slotNames("Fleet"), cpars_info$Var)
  obs_ind <- match(slotNames("Obs"), cpars_info$Var)
  imp_ind <- match(slotNames("Imp"), cpars_info$Var)
  int_ind <- (1:nrow(cpars_info))[!1:nrow(cpars_info) %in%
                                    c(stock_ind, fleet_ind, obs_ind, imp_ind)]

  cpars_info$type[stock_ind] <- "Stock"
  cpars_info$type[fleet_ind] <- "Fleet"
  cpars_info$type[obs_ind] <- "Obs"
  cpars_info$type[imp_ind] <- "Imp"
  cpars_info$type[int_ind] <- "internal"

  dflist <- list(); count <- 0
  for (ss in type) {
    count <- count + 1
    df <- cpars_info %>% dplyr::filter(cpars_info$type %in% ss) %>%
      dplyr::select(Var, Dim, Desc, type)
    names(df) <- c("Var.", "Dim.", "Desc.", "Type")
    if (nrow(df)> 0) {
      if (nrow(df)>1) {
        dflist[[count]] <- df[,as.logical(apply(!apply(df, 2, is.na), 2, prod))]
      } else {
        dflist[[count]] <- df[,!is.na(df)]
      }
    }
  }

  dfout <- do.call("rbind", dflist)
  if (is.null(dfout)) {
    if (valid) message("No valid  parameters")
    if (!valid) message("No invalid parameters")
  }

  dfout$Type <- as.factor(dfout$Type)
  dfout$Var. <- as.factor(dfout$Var.)
  if (requireNamespace("DT", quietly = TRUE)) {
    return(DT::datatable(dfout, filter = 'top', options = list(
      columnDefs = list(list(searchable = FALSE, targets = c(2,3))),
      pageLength = 25, autoWidth = TRUE)))
  } else {
    message("Install package `DT` to display dataframe as HTML table")
    return(dfout)
  }
}


