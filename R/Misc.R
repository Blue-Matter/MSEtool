
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
  switch(Units,
         'year'=1,
         'half-year'=2,
         'quarter'=4,
         'month'=12,
         'week'=52,
         'day'=365)
}

CalcTimeSteps <- function(nYears, pYears, CurrentYear, TimeUnits='year', Period=NULL) {
  nTimeStepsPerYear <- TSperYear(TimeUnits)

  hist <- seq(CurrentYear-(nYears-1), by=1/nTimeStepsPerYear,
      length.out=nYears*nTimeStepsPerYear)
  proj <- seq(CurrentYear+1,
              by=1/nTimeStepsPerYear,
              length.out=pYears*nTimeStepsPerYear)
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
    return(max_i)
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
  thisobject@Created <- NULL
  thisobject@Modified <- NULL
  newobject <- new(class(object))
  newobject@Created <- NULL
  newobject@Modified <- NULL

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

MultiplyArrays <- function(array1, array2) {
  d1 <- dim(array1)
  d2 <- dim(array2)
  if (length(d1)!=length(d1))
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

  for (s in 1:outdims[1]) {
    for (ts in 1:outdims[2]) {
      out[s,ts] <- array1[GetIndex(s, d1[1]), GetIndex(ts, d1[2])]  *
                            array2[GetIndex(s, d2[1]), GetIndex(ts, d2[2])]
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
