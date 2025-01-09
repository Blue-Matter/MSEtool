
# Transformations -----

logit <- function(p) {
  log(p/(1-p))
}

ilogit <- function(x)  {
  1/(1 + exp(-x))
}


# Distributions -----

# truncated normal distribution
ptnorm <- function(q, mean, sd, truncsd) {
  a <- (-truncsd*sd) + mean
  b <- (truncsd*sd) + mean
  out <- vector('numeric', length(a))
  for (i in seq_along(a)) {
    if (q<a[i]) {
      out[i] <- 0
    } else if (q>b[i]) {
      out[i] <- 1
    } else {
      p1 <- stats::pnorm(q, mean[i], sd[i], TRUE, FALSE)
      p2 <- stats::pnorm(a[i], mean[i], sd[i], TRUE, FALSE)
      p3 <- stats::pnorm(b[i], mean[i], sd[i], TRUE, FALSE)
      out[i] <- (p1-p2)/(p3-p2)
    }
  }
  out
}

rtnorm <- function(n, mu, sigma, lower, upper) {
  qnorm(
    runif(n,
          pnorm(lower, mu, sigma),
          pnorm(upper, mu, sigma)),
    mu,
    sigma)
}


# ---- Text ----
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# ---- Names ---- 

StockNames <- function(OM) {
  if (!methods::is(OM, 'om'))
    cli::cli_abort('`OM` must be class `om`')
  names(OM@Stock)
}

FleetNames <- function(OM) {
  if (!methods::is(OM, 'om'))
    cli::cli_abort('`OM` must be class `om`')
  names(OM@Fleet[[1]])
}

# ---- Messages ----

# default: info, progress, warnings
# FALSE: no messages or warnings
# minimal: 

SetMessages <- function(messages='default') {
  msg <- list()
  if (isFALSE(messages)) 
    return(msg)
  
  msg$info <- TRUE
  msg$alert <- TRUE
  msg$progress <- TRUE
  msg$warning <- TRUE
  
  msg
}

StartMessages <- function(OM, messages='default') {
  msg <- SetMessages(messages)
  
  # Allocation
  if (nFleet(OM)>1) {
    if (!length(OM@Allocation)) {
      OM@Allocation <- OM@CatchFrac
      if (isTRUE(msg$alert)) 
        cli::cli(c(
          cli::cli_alert_info('`Allocation(OM)` not specified'),
          cli::cli_alert('Setting `Allocation` equal to `CatchFrac` (`Allocate(OM) <- CatchFrac(OM)`)')
        ))  
      }
    
    if(!length(OM@Efactor)) {
      OM@Efactor <- lapply(1:nStock(OM), function(x) 
        matrix(1, nSim(OM), nFleet(OM)))
      if (isTRUE(msg$alert)) 
        cli::cli(c(
          cli::cli_alert_info("`Efactor(OM)` not specified"),
          cli::cli_alert("Setting `Efactor(OM)` to current effort for all fleets")
        ))
    }
  }
  
  if (nStock(OM)>1 && !length(OM@Relations) && !length(MOM@SexPars)) {
    if (isTRUE(msg$alert)) {
      cli::cli_alert_info("You have specified more than one stock but no MICE relationships (`Relations(OM)`) or sex-specific relationships (`SexPars(OM)`) among these. \nAs they are independent, consider doing MSE for one stock at a time for computational efficiency\n")
    }
  }
  Populate(OM, messages=FALSE)
}



# ----------------------------------

GetnTS <- function(TimeSteps) {
  nTS <- length(TimeSteps)
  if (nTS==0)
    nTS <- NULL
  nTS
}

TimeStepAttributes <- function(object, TimeSteps) {
  if (!is.null(attributes(object)$TimeSteps))
    TimeSteps <- attributes(object)$TimeSteps
  TimeSteps
}




getnleet <- function(Fleets) {
  if (inherits(Fleets, 'list')) {
    nfleet <- length(Fleets)
  } else {
    nfleet <- 1
  }
}

getModelClass <- function(Model=NULL) {
  if (is.null(Model))
    return(NULL)
  class(get(Model))
}

getFleetInfo <- function(Fleets) {
  nFleet <- getnleet(Fleets)

  if (nFleet>1) {
    nTS <- max(sapply(1:nFleet, function(f)
      Fleets[[f]]@FishingMortality@nYear
    ))
  } else {
    if (!inherits(Fleets, 'list')) {
      nyear <- Fleets@FishingMortality@nYear
    } else {
      nyear <- Fleets[[1]]@FishingMortality@nYear
    }
  }
  if (length(nyear)<1) {
    cli::cli_alert_danger("`nyear` not set in `FishingMortality`. Setting to `1`")
    nyear <- 1
  }


  list(nFleet=nFleet, nyear=nyear)
}

TSperYear <- function(Units) {
  Units <- tolower(Units)
  switch(Units,
         'year'=1,
         'half-year'=2,
         'quarter'=4,
         'month'=12,
         'week'=52,
         'day'=365)
}

CalcTimeSteps <- function(nYear, pYear, CurrentYear, TimeUnits='year', Period=NULL) {
  nTimeStepsPerYear <- TSperYear(TimeUnits)

  hist <- seq(CurrentYear-(nYear-1), by=1/nTimeStepsPerYear,
      length.out=nYear*nTimeStepsPerYear)
  proj <- seq(CurrentYear+1,
              by=1/nTimeStepsPerYear,
              length.out=pYear*nTimeStepsPerYear)
  if (is.null(Period))
    return(c(hist, proj))

  if (Period=='Historical')
    return(hist)

  if (Period=='Projection')
    return(proj)
}


GenerateStochasticValues <- function(object, nsim=NULL) {
  if (!is.array(object) & length(object)==2) {
    if (is.null(nsim))
      cli::cli_abort('`nsim` required to generate stochastic values')
    object <- StructurePars(list(object), nsim)[[1]]
  }
  object
}

AddAgeTimeStepDimensions <- function(object, outdim=4) {
  if (is.null(object))
    return(object)
  dd <- dim(object)

  if (outdim==4) {
    if (all(dd==1)) {
      object <- array(object, dim=c(1,1,1,1))
    } else {
      if (length(dd)==2) {
        # add age and time-step dimension
        object <- replicate(1, replicate(1, object))
      } else if (length(dd)==3) {
        # add time-step dimension
        object <- replicate(1, object)
      }
    }
  }
  if (outdim==5) {
    if (all(dd==1)) {
      object <- array(object, dim=c(1,1,1,1,1))
    } else {
      if (length(dd)==3) {
        # add age and time-step dimension
        object <- replicate(1, replicate(1, object))
      } else if (length(dd)==4) {
        # add time-step dimension
        object <- replicate(1, object)
      }
    }
  }



  object
}

SetSeed <- function(object, seed=NULL) {
  if ('Misc' %in% slotNames(object))
    object@Misc <- list()

  val <- digest::digest2int(digest::digest(object))

  if (!is.null(seed))
    val <- val + seed

  set.seed(val)
}









get_in_dim <- function(req,  n_sim, p, n_age, n_ts, n_areas) {
  switch(req,
         'SPAYR' = c(n_sim, p, n_age, n_ts, n_areas),
         'SPA' = c(n_sim, p, n_age),
         'SAY' = c(n_sim, n_age, n_ts),
         'SAR' = c(n_sim, n_age, n_areas),
         'SA' = c(n_sim, n_age),
         'SR' = c(n_sim, n_areas),
         'S' = n_sim,
         'SY' = c(n_sim, n_ts)
  )
}

get_out_index <- function(req, SPAYR) {
  switch(req,
         'SPAYR' = SPAYR,
         'SPA' = SPAYR[,1:3],
         'SAY' = SPAYR[, c(1,3,4)],
         'SAR' = SPAYR[, c(1,3,5)],
         'SA' = SPAYR[, c(1,3)],
         'SR' = SPAYR[, c(1,5)],
         'S' = SPAYR[, 1],
         'SY' = SPAYR[, c(1, 4)])

}

update_index <- function(array, exp_dim, out) {
  dd <- dim(array)
  if (is.null(dd)) {
    if (length(array)==1) {
      out[] <- 1
      return(out)
    }
  }

  match_dims <- dd==exp_dim

  if (all(match_dims)) return(out)

  if (!all(match_dims)) {
    not_match <- which(!match_dims)
    if (is.null(dim(out))) {
      out[] <- 1
    } else {
      out[,!match_dims] <- 1
    }

    return(out)
  }

  if (all(dd==1)) {
    out[] <- 1
    return(out)
  }
}

calc_index_dim_SPAYR <- function(array, req='S', n_sim, p, n_age, n_ts, n_areas) {
  SPAYR <- as.matrix(expand.grid(1:n_areas, 1:n_ts, 1:n_age, p, 1:n_sim)[5:1])
  colnames(SPAYR) <- c('n_sim', 'stock', 'n_age', 'n_ts', 'n_areas')
  out <- get_out_index(req, SPAYR)
  if (req=='Sa') {
    out <- get_out_index('SA', SPAYR)
    out[,2]<- n_age-out[,2] + 1  # This is the process error index for initial year
  }

  exp_dim <- get_in_dim(req, n_sim, p, n_age, n_ts, n_areas)
  out <- update_index(array, exp_dim, out)
  out
}

ParsEmpty <- function(Pars) {
  !ParsNotEmpty(Pars)
}

ParsNotEmpty <- function(Pars) {
  if (length(Pars)==0) return(FALSE)
  !prod(unlist(lapply(Pars, is.na)))
}

GetIndex <- function(i, max_i) {
  if (i>=max_i)
    return(rep(1:max_i, i)[i])
  i
}


SetDigest <- function(argList, object) {
  object@Created <- NULL
  object@Modified <- NULL

  if (is.list(argList)) {
    for (i in seq_along(argList)) {
      if (isS4(argList[[i]])) {
        argList[[i]]@Created <- NULL
        argList[[i]]@Modified <- NULL
      }
    }
  }

  attributes(object)$digest <- NULL
  attributes(object)$digest <- digest::digest(list(argList,object), algo='spookyhash')
  object
}


CheckDigest <- function(argList, object) {
  if (is.null(attributes(object)$digest))
    return(FALSE)
  SetDigest <- SetDigest(argList, object)

  if (attributes(SetDigest)$digest == attributes(object)$digest)
    return(TRUE)
  FALSE
}

isNewObject <- function(object) {
  thisobject <- object
  newobject <- new(class(object))
  
  if ('Created' %in% slotNames(thisobject)) {
    thisobject@Created <- NULL
    thisobject@Modified <- NULL
    newobject@Created <- NULL
    newobject@Modified <- NULL
  }
  if (identical(thisobject, newobject))
    return(TRUE)
  FALSE
}

EmptyObject <- function(object) {
  if (isS4(object)) {

    if (isNewObject(object))
      return(TRUE)

    sltnms <- slotNames(object)
    empty <- rep(TRUE, length(sltnms))
    for (i in seq_along(sltnms)) {
      sl <- sltnms[i]
      val <- slot(object, sl)
      if (isS4(val)) {
        empty[i] <- Recall(val)
      } else {
        empty[i] <- is.null(val) || length(val)==0 || all(is.na(val))
      }


    }
    return(prod(empty))
  }
  length(object) < 1 | all(is.na(object))

}

PopulatedObject <- function(object) {
  !is.null(attributes(object)$digest)
}

AddDimNames <- function(array, names=c('Sim', 'Age', 'Time Step'), TimeSteps=NULL) {

  if (is.null(array))
    return(array)
  d <- dim(array)
  l <- list()
  for (i in seq_along(names)) {
    if (names[i]=='Age') {
      l[[i]] <- 0:(d[i]-1)
    } else if (names[i]=='Time Step' & !is.null(TimeSteps)) {
      l[[i]] <- TimeSteps[1:d[i]]
    } else {
      l[[i]] <- 1:d[i]
    }
  }

  names(l) <- names
  dimnames(array) <- l
  array
}

AddMeanAtAgeAttributes <- function(object, TimeSteps=NULL, Ages=NULL) {

  object@MeanAtAge <- Structure(value=object@MeanAtAge,
                                out=c('nsim', 'nage', 'nTS')) |>
    AddDimNames(TimeSteps=TimeSteps)


  if ('Units' %in% slotNames(object))
    attributes(object@MeanAtAge)$Units <- object@Units

  attributes(object@MeanAtAge)$TimeSteps <- TimeSteps
  if (methods::is(Ages, 'ages')) {
    attributes(object@MeanAtAge)$Ages <- Ages@Classes
    attributes(object@MeanAtAge)$UnitsAge <- Ages@Units
  }
  object
}



SubtractArrays <- function(array1, array2) {
  d1 <- dim(array1)
  d2 <- dim(array1)
  if (length(d1)!=length(d1))
    cli::cli_abort('`array1` and `array2` must have number of dimensions')

  nm1 <- names(dimnames(array1))
  nm2 <- names(dimnames(array2))
  if (!(all(nm1==nm2)))
    cli::cli_abort('`array1` and `array2` must have same named dimensions')

  if (all(d1==d2))
    return(array1-array2)
}

AddArrays <- function(array1, array2) {

}

MultiplyArrays <- function(array1, array2, structure=FALSE,
                           names=c('Sim', 'Age', 'Time Step')) {
  if (structure) {
    array1  <- Structure(array1) |> AddDimNames(name=names)
  }

  d1 <- dim(array1)
  d2 <- dim(array2)
  if (length(d1)!=length(d2))
    cli::cli_abort('`array1` and `array2` must have number of dimensions')

  nm1 <- names(dimnames(array1))
  nm2 <- names(dimnames(array2))
  if (!(all(nm1==nm2)))
    cli::cli_abort('`array1` and `array2` must have same named dimensions')

  if (all(d1==d2))
    return(array1*array2)

  alldims <- rbind(d1, d2)
  outdims <- apply(alldims, 2, max)
  out <- array(NA, dim=outdims)

  if (length(outdims)==2) {
    for (s in 1:outdims[1]) {
      for (age in 1:outdims[2]) {
        out[s,age,ts] <- array1[GetIndex(s, d1[1]), 
                                GetIndex(age, d1[2])]  *
          array2[GetIndex(s, d2[1]),
                 GetIndex(age, d2[2])]
      }
    }
  }
  
  if (length(outdims)==3) {
    for (s in 1:outdims[1]) {
      for (age in 1:outdims[2]) {
        for (ts in 1:outdims[3]) {
          out[s,age,ts] <- array1[GetIndex(s, d1[1]), 
                                  GetIndex(age, d1[2]),
                                  GetIndex(ts, d1[3])]  *
            array2[GetIndex(s, d2[1]),
                   GetIndex(age, d2[2]),
                   GetIndex(ts, d2[3])]
        }
      }
    }
  }
  AddDimNames(out, nm1)

}

GetLengthClass <- function(object, RefValue=0.5) {
  array <- object@MeanAtLength
  
  d1 <- dim(RefValue)
  if (is.null(d1)) {
    dd <- dim(array)
  } 
  
  out <- array(0, dim=dd[c(1,3)])
  # silly loop for now
  for (s in 1:dd[1]) {
    for (ts in 1:dd[3]) {
      ind <- which.min(abs(array[s,,ts]-RefValue))
      out[s,ts] <- object@Classes[ind]
    }
  }
  out
}

DivideArrays <- function(array1, array2) {
  d1 <- dim(array1)
  d2 <- dim(array1)
  if (length(d1)!=length(d1))
    cli::cli_abort('`array1` and `array2` must have number of dimensions')

  nm1 <- names(dimnames(array1))
  nm2 <- names(dimnames(array2))
  if (!(all(nm1==nm2)))
    cli::cli_abort('`array1` and `array2` must have same named dimensions')

  if (all(d1==d2))
    return(array1/array2)
  alldims <- rbind(d1, d2)
  outdims <- apply(alldims, 2, max)
  out <- array(NA, dim=outdims)

  for (s in 1:outdims[1]) {
    for (ts in 1:outdims[2]) {
      out[s,ts] <- array1[GetIndex(s, d1[1]), GetIndex(ts, d1[2])]  /
        array2[GetIndex(s, d2[1]), GetIndex(ts, d2[2])]
    }
  }
  out
}


AddSimDimension <- function(array, names=c('Sim', 'Age', 'Time Step'), TimeSteps=NULL) {
  dd <- dim(array)
  if (length(dd)==length(names))
    return(AddDimNames(array, TimeSteps=TimeSteps))
  if (length(dd)==2) {
    array <- replicate(1, array) |> aperm(c(3,1,2))
  }
  AddDimNames(array, names, TimeSteps=TimeSteps)
}


range01 <- function (x) {
  (x - min(x))/(max(x) - min(x))
}



SolveForVmaxlen <- function(om) {
  # calculates new value for Vmaxlen to correspond with maximum
  # length bin rather than Linf, as previously defined
  Linf <- om@Stock@Length@Pars$Linf
  dd <- dim(Linf)
  L5 <- om@Fleet@Selectivity@Pars$L5
  LFS <- om@Fleet@Selectivity@Pars$LFS
  Vmaxlen <- om@Fleet@Selectivity@Pars$Vmaxlen
  
  VmaxlenOut <- array(0, dim=dd)
  cli::cli_progress_bar('Calculating `Vmaxlen`', total=prod(dd))
  
  for (s in 1:dd[1]) {
    for (ts in 1:dd[2]) {
      l5 <- L5[GetIndex(s, nrow(L5)), GetIndex(ts, ncol(L5))]
      lfs <- LFS[GetIndex(s, nrow(LFS)), GetIndex(ts, ncol(LFS))]
      linf <- Linf[GetIndex(s, nrow(Linf)), GetIndex(ts, ncol(Linf))]
      vmaxlen <- Vmaxlen[GetIndex(s, nrow(Vmaxlen)), GetIndex(ts, ncol(Vmaxlen))]
      if (vmaxlen==1)
        next()
      opt <- uniroot(optForVmaxLen,
                      interval=logit(c(0.001, 0.999)),
                      l5=l5,
                      lfs=lfs,
                      linf=linf,
                      vmaxlen=vmaxlen)
      VmaxlenOut[s,ts] <- ilogit(opt$root)
      cli::cli_progress_update()
    }
  }
  cli::cli_progress_done()
  om@Fleet@Selectivity@Pars$Vmaxlen <- VmaxlenOut
  om
}

SolveForRmaxlen <- function(om) {
  # calculates new value for Rmaxlen  to correspond with maximum
  # length bin rather than Linf, as previously defined
  Linf <- om@Stock@Length@Pars$Linf
  dd <- dim(Linf)
  L5 <- om@Fleet@Retention@Pars$LR5
  LFS <- om@Fleet@Retention@Pars$LFR
  Rmaxlen <- om@Fleet@Retention@Pars$Rmaxlen
  
  RmaxlenOut <- array(0, dim=dd)
  cli::cli_progress_bar('Calculating `Rmaxlen`', total=prod(dd))
  for (s in 1:dd[1]) {
    for (ts in 1:dd[2]) {
      l5 <- L5[GetIndex(s, nrow(L5)), GetIndex(ts, ncol(L5))]
      lfs <- LFS[GetIndex(s, nrow(LFS)), GetIndex(ts, ncol(LFS))]
      linf <- Linf[GetIndex(s, nrow(Linf)), GetIndex(ts, ncol(Linf))]
      rmaxlen <- Rmaxlen[GetIndex(s, nrow(Rmaxlen)), GetIndex(ts, ncol(Rmaxlen))]
      
      if (l5==0)
        next()
      
      
      if (rmaxlen==1)
        next()
      opt <- uniroot(optForVmaxLen,
                     interval=logit(c(0.001, 0.999)),
                     l5=l5,
                     lfs=lfs,
                     linf=linf,
                     vmaxlen=rmaxlen)
      RmaxlenOut[s,ts] <- ilogit(opt$root)
      cli::cli_progress_update()
    }
  }
  cli::cli_progress_done()
  om@Fleet@Retention@Pars$Rmaxlen <- RmaxlenOut
  om
}

optForVmaxLen <- function(logitTrial, l5, lfs, linf, vmaxlen) {
  trial <- ilogit(logitTrial)
  lens <- seq(0, linf,length.out=100)
  sel <- DoubleNormal(lens,l5, lfs, trial)
  sel[length(sel)] - vmaxlen
}
