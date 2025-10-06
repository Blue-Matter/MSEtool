OnExit <- function() {
  on.exit(cli::stop_app())
}


#' Miscellaneous Helper Functions 
#' @name miscellanous
NULL

#' @describeIn miscellanous Reduce the number of simulations in an `om` or `hist` object
#' @export
ReduceNSim <- function(object, nSim=NULL) {
  if (is.null(nSim))
    return(object)
  
  CheckClass(nSim, c('numeric', 'integer'), 'nSim')
  
  if (length(nSim)>1)
    cli::cli_abort("`nSim` ({.val {nSim}}) must be numeric/integer length 1")
  
  if (nSim<1)
    cli::cli_abort("`nSim` ({.val {nSim}}) must be >= 1")
  
  if (nSim>nSim(object)) {
    cli::cli_alert_warning("Argument `nSim` ({.val {nSim}}) is greater than {.run nSim(OM)} ({.val {nSim(object)}}). Ignoring argument `nSim`  ")
    nSim <- nSim(object)
  }
    
  if (nSim(object) == nSim)
    return(object)
  
  SubsetSim(object, Sim=1:nSim)
}

#' Update missing slots in an S4 object 
#' @export
UpdateObject <- function(object) {
  if (!isS4(object))
    return(object)
  slots <- slotNames(object)
  for (sl in slots) {
    chk <- try(slot(object,sl), silent=TRUE)
    if (inherits(chk, 'try-error')) {
      newobject <- new(class(object))
      slot(object,sl) <- slot(newobject,sl)
    }
  }
  object
}


not <- function(val) !val

ReplaceTiny <- function(Array, value=1, default=tiny/2) {
  Array[Array==default] <- value
  Array
}

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
firstup <- function(x, n=1) {
  substr(x, 1, n) <- toupper(substr(x, 1, n))
  x
}

# ---- Names ---- 

#' @export
StockNames <- function(OM) {
  if (!methods::is(OM, 'om'))
    cli::cli_abort('`OM` must be class `om`')
  names(OM@Stock)
}

#' @export
FleetNames <- function(OM) {
  if (!methods::is(OM, 'om'))
    cli::cli_abort('`OM` must be class `om`')
  names(OM@Fleet[[1]])
}

# 
# `FleetNames<-` <- function(x, value) {
#   if (inherits(x, 'om'))
#     AssignFleetNamesOM(x, value)
# }
# 
# AssignFleetNamesOM <- function(OM, value) {
#   
# 
# }

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
 
  # if (!length(OM@Allocation)) {
  #   OM@Allocation <- OM@CatchFrac
  #   if (nFleet(OM)>1) {
  #     if (isTRUE(msg$alert)) 
  #       cli::cli(c(
  #         cli::cli_alert_info('`Allocation(OM)` not specified'),
  #         cli::cli_alert('Setting `Allocation` equal to `CatchFrac` (`Allocate(OM) <- CatchFrac(OM)`)')
  #       ))  
  #   }
  # }
  
  if(!length(OM@Efactor)) {
    OM@Efactor <- lapply(1:nStock(OM), function(x) 
      matrix(1, nSim(OM), nFleet(OM)))
    if (nFleet(OM)>1) {
      if (isTRUE(msg$alert)) 
        cli::cli(c(
          cli::cli_alert_info("`Efactor(OM)` not specified"),
          cli::cli_alert("Setting `Efactor(OM)` to current effort for all fleets")
        ))
    }
  }
    
  
  if (nStock(OM)>1 && !length(OM@Relations) && !length(OM@SexPars)) {
    if (isTRUE(msg$alert)) {
      cli::cli_alert_info("You have specified more than one stock but no MICE relationships (`Relations(OM)`) or sex-specific relationships (`SexPars(OM)`) among these. \nAs they are independent, consider doing MSE for one stock at a time for computational efficiency\n")
    }
  }
  OM
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
  if (inherits(Model, 'function'))
    return('function')
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
  
  TimeUnits <- tolower(TimeUnits)
  
  if (CurrentYear<1900) {
    # not in year units
    hist <- seq(CurrentYear, by=-1, length.out=nYear) |> rev()
    proj <- seq(CurrentYear+1, by=1, length.out=pYear)
    
    if (is.null(Period))
      return(c(hist, proj))
    
    if (Period=='Historical')
      return(hist)
    
    if (Period=='Projection')
      return(proj)
  }
  
  FirstHistYear <- lubridate::ymd(paste0(CurrentYear-nYear+1, '-01-01'))
  LastHistYear <- lubridate::ymd(paste0(CurrentYear, '-12-31'))
  
  FirstProjYear <- lubridate::ymd(paste0(CurrentYear+1, '-01-01'))
  LastProjYear <- lubridate::ymd(paste0(CurrentYear+pYear, '-12-31'))
  
  validTimeUnits <- c('year', 'half-year', 'quarter', 'month', 'week', 'day')
  
  if (TimeUnits=='year') {
    hist <- seq(FirstHistYear, LastHistYear, by='year') |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by='year') |> lubridate::decimal_date()
  } else if (TimeUnits=='half-year') {
    hist <- seq(FirstHistYear, LastHistYear, by='6 months') |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by='6 months') |> lubridate::decimal_date()
  } else if (TimeUnits=='quarter') {
    hist <- seq(FirstHistYear, LastHistYear, by='4 months') |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by='4 months') |> lubridate::decimal_date()
  } else if (TimeUnits=='month') {
    hist <- seq(FirstHistYear, LastHistYear, by='1 month') |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by='1 month') |> lubridate::decimal_date()
  } else if (TimeUnits=='week') {
    hist <- seq(FirstHistYear, LastHistYear, by='1 week') |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by='1 week') |> lubridate::decimal_date()
  } else if (TimeUnits=='day') {
    hist <- seq(FirstHistYear, LastHistYear, by='1 day') |> lubridate::decimal_date()
    proj <- seq(FirstProjYear, LastProjYear, by='1 day') |> lubridate::decimal_date()
  } else {
    cli::cli_abort('`TimeUnits` must be one of: {.val {validTimeUnits}}')
  }
  
  hist <- hist |> round(4)
  proj <- proj |> round(4)
 
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

# ---- Add Dimensions ----


SumOverDimension <- function(array, name='Area') {
  d <- dim(array)
  nms <- names(dimnames(array))
  ind <- which(nms==name)
  if (d[ind] == 1) {
    return(DropDimension(array))
  }
  apply(array, nms[-ind], sum) 
}

DropDimension <- function(array, name='Area', warn=TRUE) {
  # drops the named dimension only if it is length one
  d <- dim(array)
  nms <- names(dimnames(array))
  ind <- which(nms==name)
  if (length(ind)<1)
    cli::cli_abort('Dimension {.var {name}} not found ', .internal=TRUE)
  
  if (d[ind]>1 & warn) 
    cli::cli_alert_warning('Note: Dropping dimension {.val {name}} but dimension length is > 1. Use `warn=FALSE` to suppress')
  
  dNames <- dimnames(array)[-ind]
  
  array <- abind::asub(array, 1, ind, drop=FALSE) |>
    abind::adrop(ind)
  
  if (!inherits(array, 'array'))
    array <- array(array, dimnames=dNames)
  array
}

AddDimension <- function(array, name=NULL, val=1) {
  if (inherits(array, 'list'))
    array <- unlist(array)
  if (is.null(array))
    return(NULL)
  d <- dim(array)
  nms <- names(dimnames(array))
  if (name %in% nms)
    return(array)
  
  if (all(d==1)) {
    outarray <- array(array, dim=c(d, 1))
  } else {
    outarray <- replicate(1, array) 
  }
  
  # set dimnames
  l <- dimnames(array)
  if (!is.null(l)) {
    l[name] <- val
    dimnames(outarray) <- l  
  }
  outarray
}

AddSimDimension <- function(array, names=c('Sim', 'Age', 'TimeStep'), TimeSteps=NULL) {
  dd <- dim(array)
  if (length(dd)==length(names))
    return(AddDimNames(array, names, TimeSteps=TimeSteps))
  
  if (length(dd)==2) {
    array <- replicate(1, array) |> aperm(c(3,1,2))
  }
  AddDimNames(array, names, TimeSteps=TimeSteps)
}


AddAreaDimension <- function(array) {
  l <- dimnames(array)
  dd <- dim(array)
  if (all(dd==1)) {
    array <- array(array, dim=c(dd, 1))
  } else {
    array <- replicate(1, array) 
  }
  
  l$Area <- 1
  dimnames(array) <- l
  array
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

#' @export
List2Array <- function(List, dimname="Fleet", dim1="Sim", ListDimNames=NULL, pos=NULL) {
  if (inherits(List, 'array'))
    return(List)
  if (!length(List))
    return(NULL)
  
  
  UnList <- unlist(List)
  if (length(UnList)<1)
    return(UnList)
  
  if (is.null(dim(List[[1]]))) {
    array <- array(UnList, 
                   dim=c(length(List[[1]]), length(List)))
    dimnames(array) <- list(dim1=1:nrow(array),
                            temp=names(List))
    names(dimnames(array))[1] <- dim1
    names(dimnames(array))[2] <- dimname
    return(array)
    
  } 
  
  array <- array(UnList, 
                 dim=c(dim(List[[1]]), length(List)))

  if (!is.null(dimname)) {
    if (!is.null(ListDimNames)) {
      dimnames(List[[1]]) <- ListDimNames
    } 
    d <- dimnames(List[[1]])
    
    if (!is.null(d)) {
      d[[dimname]] <- names(List)
      dimnames(array) <- d 
    }
 
  }
  
  if (is.null(pos))
    return(array)
  
  dnames <- array |> dimnames() |> names()
  dnamesDrop <- dnames[!dnames==dimname]
  dnamesNew <- rep(NA, length(dnames))
  dnamesNew[pos] <- dimname
  
  cnt <- 1
  for (i in seq_along(dnamesNew)) {
    if (is.na(dnamesNew[i])) {
      dnamesNew[i] <- dnamesDrop[cnt]
      cnt <- cnt + 1
    }
  }
  
  array |> aperm(dnamesNew)
}

Array2List <- function(array, pos=3, sim=NULL) {
  if (is.null(array))
    return(array)
  dnames <- names(dimnames(array))
  if (is.character(pos)) {
    pos <- match(pos, dnames)
  }
  
  dd <- dim(array)
  
  if (!is.null(sim)) {
    list <- vector('list', 1)
    indexvalue <- sim
  } else {
    list <- vector('list', dd[pos])  
    indexvalue <- 1:length(list)
  }
  
  
  dimnames <- dimnames(array)
  
  listnames <- dimnames[[pos]][indexvalue]
  names(list) <- listnames
  keepnames <- dimnames[-pos]
  keepnames[[names(dimnames)[pos]]] <- 1
  
  for (i in seq_along(indexvalue)) {
    tdimnames <- dimnames
    tdimnames[pos] <-  dimnames[[pos]][indexvalue[i]]
    val <- abind::adrop(abind::asub(array, tdimnames, drop=FALSE), pos)
    if (is.null(dimnames(val))) {
      tdimnames[pos] <- NULL
      val <- array(val, dim=length(val), dimnames=tdimnames)
    }
    list[[i]] <- val
    
  }
  list
}

# Makes sure each seed is unique for the same object
SetSeed <- function(object, seed=NULL) {
  if ('Misc' %in% slotNames(object))
    object@Misc <- list()
  
  if ('Created' %in% slotNames(object))
    object@Created <- NULL
  
  if ('Modified' %in% slotNames(object))
    object@Modified <- NULL
  
  if ('Model' %in% slotNames(object))
    object@Model <- NULL
  
  if ('RelRecFun' %in% slotNames(object))
    object@RelRecFun <- NULL
  
  if ('Catchability' %in% slotNames(object))
    object@Catchability <- NULL

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

IdenticalS4 <- function(object1, object2) {
  digest::digest(object1, algo='spookyhash') == digest::digest(object2, algo='spookyhash')
}

SetDigest <- function(object, argList=list()) {
  # object@Created <- NULL
  # object@Modified <- NULL

  if (is.list(argList)) {
    for (i in seq_along(argList)) {
      if (isS4(argList[[i]])) {
        # argList[[i]]@Created <- NULL
        # argList[[i]]@Modified <- NULL
      }
    }
  }

  attributes(object)$digest <- NULL
  attributes(object)$digest <- digest::digest(list(argList,object), algo='spookyhash')
  object
}


MakeNamedList <- function(names, values=NULL) {
  l <- vector('list', length(names))
  names(l) <- names
  if (!is.null(values)) {
    for (i in 1:length(l)) {
      l[[i]] <- values
    }
  }
  l
}

CheckDigest <- function(object, argList=list()) {
  if (is.null(attributes(object)$digest))
    return(FALSE)
  SetDigest <- SetDigest(object, argList)

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
      if (inherits(val, 'function'))
        next()
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

AddDimNames <- function(array, names=c('Sim', 'Age', 'TimeStep'), 
                        TimeSteps=NULL, Ages=NULL, Fleets=NULL,
                        values=NULL) {
  
  if (inherits(array,'list'))
    array <- unlist(array)
  if (is.null(array))
    return(array)
  d <- dim(array)
  l <- list()
  for (i in seq_along(names)) {
    if (names[i]=='Age') {
      if (!is.null(Ages)) {
        l[[i]] <- Ages
      } else {
        l[[i]] <- 0:(d[i]-1)
      }
        
    } else if (names[i]=='Fleet' && !is.null(Fleets)) {
      l[[i]] <- Fleets
    } else if (names[i]=='TimeStep' && !is.null(TimeSteps)) {
      l[[i]] <- TimeSteps[1:d[i]]
    } else {
      if (is.null(values)) {
        l[[i]] <- 1:d[i]  
      } else {
        if (!is.null(values[[i]]) && !any(is.na(values[[i]]))) {
          l[[i]] <- values[[i]]
        } else {
          l[[i]] <- 1:d[i]  
        }
          
      }
      
    }
  }

  names(l) <- names
  dimnames(array) <- l
  array
}

AddMeanAtAgeAttributes <- function(object, TimeSteps=NULL, Ages=NULL) {

  object@MeanAtAge <- Structure(value=object@MeanAtAge,
                                out=c('nsim', 'nage', 'nTS'))
  
  if (is.null(dimnames(object@MeanAtAge)))  
    object@MeanAtAge <- object@MeanAtAge |> AddDimNames(TimeSteps=TimeSteps)

  if ('Units' %in% slotNames(object))
    attributes(object@MeanAtAge)$Units <- object@Units

  # if (is.null(attributes(object@MeanAtAge)$TimeSteps))
  #   attributes(object@MeanAtAge)$TimeSteps <- TimeSteps
  # 
  if (methods::is(Ages, 'ages')) {
    # attributes(object@MeanAtAge)$Ages <- Ages@Classes
    attributes(object@MeanAtAge)$UnitsAge <- Ages@Units
  }
  object
}











range01 <- function (x) {
  (x - min(x))/(max(x) - min(x))
}





aperm <- function(a, perm, ...) {
  if (is.null(a))
    return(a)
  base::aperm(a, perm, ...)
}



# EditSlotsForSimCheck <- function(object, TimeSteps) {
#   # TODO subset time dimension to check for identical historical
#   nms <- slotNames(object)
#   
#   for (nm in nms) {
#     object2 <- slot(object, nm)
#     if (!isS4(object2))
#       next()
#     
#     if (inherits(object2, 'srr')) {
#       dimnames(object2@RecDevInit) <- NULL
#       names(object2@RecDevInit) <- NULL
#       object2@RecDevInit <- array(object2@RecDevInit)
#       object2@RecDevProj <- array()
#     }
#     
#     slots <- slotNames(object2)
# 
#     # if ('MeanAtAge' %in% slots) 
#     #   object2@MeanAtAge <- ArraySubsetTimeStep(object2@MeanAtAge, TimeSteps)
#     #   
#   
#     if (!'Pars' %in% slots)
#       next()
#     object2@Pars <- list()
#     slot(object, nm) <- object2
#   }
#   object
# }



# slots <- slotNames(HistSimList[[1]]@OM@Fleet$Female)
# for (sl in slots) {
#   dig1 <- digest::digest(slot(HistSimList[[1]]@OM@Fleet$Female, sl), algo='spookyhash')
#   dig2 <- digest::digest(slot(HistSimList[[i]]@OM@Fleet$Female,sl), algo='spookyhash')
#   if (dig1 != dig2)
#     stop(sl)
# }





