#' Calculate an Age-Size Key
#'
#' Generates an Age-Size key given mean and standard deviation of size-at-age. The size-at-age
#' can be normally or log-normally distributed. By default the distribution is truncated at 2
#' standard deviations.
#'
#' @param MeanAtAge The mean size at age. Either a numeric vector of length `nage`, a 2D array with
#' dimensions `c(nsim, nage)`, or a 3D array with dimensions `c(nsim, nage, nTS)`.
#' @param CVatAge The coefficient of variation (CV) at age. Same structure as `MeanAtAge`.
#' @param Classes A numeric vector with the size classes for the age-size key.
#' @param TruncSD Numeric value indicating the number of standard deviations
#' where the distribution is truncated. Use high values to approximate a non-truncated distribution
#' @param Dist The distribution of `MeanAtAge`. Character, either `normal` or `lognormal`
#' @param Ages Optional. Numeric vector of values for the age classes. Defaults to `0:(nage-1)`.
#'
#' @return A 4D array with dimensions `nsim`, `nage`, `nClasses`, and `nTS`
#'
#' @export
CalcASK <- function(MeanAtAge, CVatAge, Classes,
                    TruncSD=2, Dist=c('normal', 'lognormal'),
                    Ages=NULL, silent=FALSE,
                    type='Length') {
  Dist <- match.arg(Dist)

  # Checks
  if (any(Classes<0))
    cli::cli_abort('Some `Classes` < 0 ')
  if (length(Classes)<3)
    cli::cli_abort('`length(Classes)<3`')
  if (length(TruncSD)>1)
    TruncSD <- TruncSD[1]
  if (TruncSD<0)
    cli::cli_abort('`TruncSD` < 0 ')

  MeanAtAge <- Structure(MeanAtAge)
  SDatAge <- CalcSDatAge(MeanAtAge, CVatAge)

  dim_MeanAtAge <- dim(MeanAtAge)
  nage <- dim_MeanAtAge[2]

  dim_SDatAge <- dim(SDatAge)

  if (dim_MeanAtAge[2] != dim_SDatAge[2]) {
    if (dim_SDatAge[2]==1) {
      SDatAge <- Structure(replicate(nage, SDatAge))
    } else {
      cli::cli_abort('`dim(MeanAtAge)[2] != dim(SDatAge)[2]`')
    }
  }

  nsim_MeanAtAge <- dim_MeanAtAge[1]
  nTS_MeanAtAge <- dim_MeanAtAge[3]

  nsim_SDatAge <- dim_SDatAge[1]
  nTS_SDatAge <- dim_SDatAge[3]

  nsim <- max(nsim_MeanAtAge, nsim_SDatAge) # maximum number of simulations
  nTS <- max(nTS_MeanAtAge, nTS_SDatAge) # maximum number of time-steps

  ASK_list <- vector('list', nsim)

  by <- Classes[2]-Classes[1]
  ClassLower <- seq(Classes[1]-0.5*by, by=by, length.out=length(Classes))
  nClasses <- length(Classes)

  if (is.null(Ages))
    Ages <- 0:(nage-1)

  text <- paste('Age', type, sep='-')
  if (!silent)
    cli::cli_progress_bar(paste('Calculating', text, 'Key'),
                          total=nsim, type='tasks')
  for (s in 1:nsim) {
    if (!silent)
      cli::cli_progress_update()
    ASK <- array(0, dim=c(nage, nClasses, nTS))
    for (t in 1:nTS) {
      sd <- SDatAge[GetIndex(s, nsim_SDatAge),,GetIndex(t, nTS_SDatAge)]
      if (Dist=='normal') {
        mu <- MeanAtAge[GetIndex(s, nsim_MeanAtAge),,GetIndex(t, nTS_MeanAtAge)]
        classlower <- ClassLower
      }
      if (Dist =='lognormal') {
        sd <- SDatAge[GetIndex(s, nsim_SDatAge),,GetIndex(t, nTS_SDatAge)]/
          MeanAtAge[GetIndex(s, nsim_MeanAtAge),,GetIndex(t, nTS_MeanAtAge)]

        sd[!is.finite(sd)] <- 0.05
        mu <- log(MeanAtAge[GetIndex(s, nsim_MeanAtAge),,GetIndex(t, nTS_MeanAtAge)]) -
          0.5 * sd^2
        mu[!is.finite(mu)] <- log(1E-6)
        classlower <- log(ClassLower)
      }
      ASK[,1,t] <- ptnorm(classlower[2], mean=mu, sd=sd, truncsd=TruncSD)
      for (l in 2:(nClasses-1)) {
        ASK[,l,t] <- ptnorm(classlower[l+1], mean=mu, sd=sd, TruncSD) -
          ptnorm(classlower[l], mean=mu, sd=sd, TruncSD)
      }
      ASK[,nClasses,t] <- 1 - ptnorm(classlower[nClasses], mean=mu, sd, TruncSD)
      # ASK[,,t] <- ASK[,,t]/matrix(apply(ASK[,,t], 1, sum), nrow=nage, ncol=nClasses)
    }
    ASK_list[[s]] <- ASK
  }
  out <- abind::abind(ASK_list, along=4) |> aperm(c(4,1,2,3))
  attributes(out)$Classes <- Classes
  attributes(out)$Ages <- Ages
  out
}


RequireALK <- function(stock) {
  stock_slots <- c('Length', 'Weight', 'NaturalMortality', 'Maturity', 'Fecundity')

  fleet_slots <- c('Selectivity', 'Retention')

  reqALK <- rep(FALSE, length(stock_slots))
  for (i in seq_along(stock_slots)) {
    model <- slot(slot(stock, stock_slots[i]), 'Model')
    if (is.null(model))
      next()
    cl <- model|> get() |> class()
    if (grepl('at-Length-Model', cl))
      reqALK[i] <- TRUE
  }
  sum(reqALK)
}

RequireAWK <- function(stock) {
  TRUE

}




PopulateModel <- function(object, ignore=NULL) {
  if (EmptyObject(object@Pars))
    return(object)
  if (!is.null(ignore)) {
    object@Model <- FindModel(object, ignore)
  } else {
    object@Model <- FindModel(object)
  }
  object
}


MaxTSValue <- function(Units) {
  switch(Units,
         "year" =1,
         "half-year"=2,
         "quarter"=4,
         "month"=12,
         "week"=52,
         "day"=365 )
}



PopulateRandom <- function(object) {
  if (!is.null(object@Random)) {
    cl <- class(object)
    cli::cli_alert(paste0('`', cl, '@Random` populated but not currently used'))
  }
  object
}


CheckRequiredObject <- function(object, class, argName=NULL) {

  if (!(methods::is(object, class))) {
    if (is.null(argName)) {
      argName <- firstup(class)
    }
    obj <- paste0('MSEtool::', argName)
    cli::stop_app()
    cli::cli_abort(c("{.code {argName}}  must be an {.help {.fun {obj}}} object",
                   'i'="Provide {.val {class}} object to the {.val {argName}} argument"))

  }

  tt <- methods('Check')
  if (!any(grepl(paste0(class, '-method'), tt))) {
    cli::cli_alert_warning("`Check` method doesn't exist for class {.code {class}}")
    chk <- NULL
  } else {
    chk <- Check(object)
    if(!chk@complete)
      cli::cli_abort("{.code {argName}} is not complete. See {.code Check({.run {class}})}")
  }
  invisible(chk)
}

PopulateMeanAtAge <- function(object, Ages=NULL, TimeSteps=NULL, Length=NULL) {

  if (!is.null(object@MeanAtAge))
    return(object)

  if (ParsNotEmpty(object@Pars)) {
    if (is.null(object@Model))
      object@Model <- FindModel(object)

    args <- names(formals(object@Model))

    if ('Length' %in% args) {
      CheckRequiredObject(Length, 'length', 'Length')
      chk <- Check(Length)
      if(!chk@populated) {
        CheckRequiredObject(Ages, 'ages', 'Ages')
        Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
      }
      object@MeanAtAge <- GenerateMeanatLength(Model=object@Model,
                                               Pars=object@Pars,
                                               Length=Length@MeanAtAge)

    } else {
      if ('Ages' %in% args) {
        CheckRequiredObject(Ages, 'ages', 'Ages')
        if ('Timing' %in% slotNames(object)) {
          Ages@Classes <- Ages@Classes+object@Timing
        }
      }
      object@MeanAtAge <- GenerateMeanAtAge(Model=object@Model,
                                            Pars=object@Pars,
                                            Ages=Ages@Classes)
      dd <- dim(object@MeanAtAge)
      dimnames(object@MeanAtAge) <- list(Sim=1:dd[1],
                                         Age=Ages@Classes[1:dd[2]],
                                         `Time Step`=TimeSteps[1:dd[3]])
    }
  }
  object
}

PopulateMeanAtLength <- function(object, 
                                 Length=NULL, 
                                 TimeSteps=NULL, 
                                 Ages=NULL, 
                                 nsim=NULL,
                                 seed=NULL, silent) {

  if (is.null(object@Model))
    return(object)

  if (!is.null(object@MeanAtLength))
    return(object)

  if (ParsNotEmpty(object@Pars)) {

    if (is.null(object@Model))
      object@Model <- FindModel(object)

    args <- names(formals(object@Model))

    if ('Ages' %in% args)
      return(object)

    if ('Length' %in% args) {
      CheckRequiredObject(Length, 'length', 'Length')
      chk <- Check(Length)
      if(!chk@populated) {
        CheckRequiredObject(Ages, 'ages', 'Ages')
        Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
      }
    }
    object@MeanAtLength <- GenerateMeanatLength(Model=object@Model,
                                                Pars=object@Pars,
                                                Length=Length@Classes)

    if ('Units' %in% slotNames(object))
      attributes(object@MeanAtLength)$Units <- object@Units
    attributes(object@MeanAtLength)$TimeSteps <- TimeSteps
    if (methods::is(Length, 'length')) {
      attributes(object@MeanAtLength)$LengthClasses <- Length@Classes
      attributes(object@MeanAtLength)$UnitsLength <- Length@Units
    }
    object@MeanAtLength <- AddDimNames(object@MeanAtLength, c('sim', 'class', 'TS'))
  }
  object
}


CalcSDatAge <- function(MeanAtAge, CVatAge) {

  dims <- rbind(dim(MeanAtAge),
                dim(CVatAge))
  dim <- apply(dims, 2, max)

  d1 <- dim(MeanAtAge)
  ind1 <- expand.grid(1:d1[1], 1:d1[2], 1:d1[3]) |> as.matrix()
  d2 <- dim(CVatAge)
  ind2 <- expand.grid(1:d2[1], 1:d2[2], 1:d2[3]) |> as.matrix()

  nsim <- dim[1]
  nage <- dim[2]
  nTS <- dim[3]

  SDatAge <- array(NA, dim=c(nsim, nage, nTS))

  ind3 <- expand.grid(1:dim[1], 1:dim[2], 1:dim[3]) |> as.matrix()
  SDatAge[ind3] <-  MeanAtAge[ind1] * CVatAge[ind2]

  SDatAge
}


CalcMaxBin <- function(MeanAtAge, CVatAge, TruncSD=2, dist='normal') {
  d1 <- dim(MeanAtAge)
  ind1 <- expand.grid(1:d1[1], 1:d1[2], 1:d1[3]) |> as.matrix()

  if (dist =='normal') {
    SDatAge <- CalcSDatAge(MeanAtAge, CVatAge)
    d2 <- dim(SDatAge)
    ind2 <- expand.grid(1:d2[1], 1:d2[2], 1:d2[3]) |> as.matrix()
    MaxBin <- max(TruncSD * SDatAge[ind2] + MeanAtAge[ind1]) |> ceiling()

  } else if (dist =='lognormal') {
    CVatAge[!is.finite(CVatAge)] <- 0.01
    d2 <- dim(CVatAge)
    ind2 <- expand.grid(1:d2[1], 1:d2[2], 1:d2[3]) |> as.matrix()
    MeanAtAge[MeanAtAge<=0] <- 1E-6
    MeanAtAge <- log(MeanAtAge[ind1]) - -0.5*CVatAge[ind2]^2
    MaxBin <- max(exp(MeanAtAge[ind1] + TruncSD * CVatAge[ind2])) |> ceiling()
  } else {
    cli::cli_abort('{.code {dist}} is not valid for `Dist` slot. Options are `normal` or `lognormal`')
  }
  MaxBin
}

PopulateClasses <- function(object) {
  if (!EmptyObject(object@Classes))
    return(object)

  MaxBin <- CalcMaxBin(object@MeanAtAge, object@CVatAge, object@TruncSD, object@Dist)
  bins <- seq(0, to=MaxBin, length.out=40) |> round(2)
  by <- bins[2] - bins[1]

  object@Classes <- seq(bins[1]+0.5*by, by=by, length.out=length(bins)-1)
  object
}


PopulateASK <- function(object, Ages=NULL, silent=FALSE, type='Length') {


  CheckRequiredObject(Ages, 'ages', 'Ages')
  if ('Timing' %in% slotNames(object)) {
    Ages@Classes <- Ages@Classes+object@Timing
  }

  MeanAtAge <- object@MeanAtAge
  CVatAge <- object@CVatAge
  Classes <- object@Classes
  Dist <- object@Dist
  TruncSD <- object@TruncSD

  object@ASK <- CalcASK(MeanAtAge, CVatAge, Classes, TruncSD, Dist, Ages,
                        silent=silent, type=type)
  attributes(object@ASK)$timesteps <- attributes(object@MeanAtAge)$timesteps
  object
}


StructureCV <- function(CVatAge, nsim) {
  if (is.null(dim(CVatAge))) {
    if (length(CVatAge)==1)
      return(Structure(CVatAge))
    if (length(CVatAge)==2) {
      return(Structure(StructurePars_(CVatAge, nsim)))
    }
  } 
  Structure(CVatAge)
} 

UpdateSPFrom <- function(OM) {
  stocknames <- StockNames(OM) 
  if (length(stocknames) != nStock(OM)) {
    cli::cli_abort(c('{.var Name} must be unique for each Stock.',
                     'i'='Current Stock Names are {.val {stocknames}}'))
  }
  if (length(stocknames)==1)
    return(OM)
  for (st in 1:nStock(OM)) {
    if (!is.null(OM@Stock[[st]]@SRR@SPFrom))
      OM@Stock[[st]]@SRR@SPFrom <- stocknames[OM@Stock[[st]]@SRR@SPFrom]
  }
  OM
}

ShareParameters <- function(OM) {
  
  if (length(OM@SexPars@Herm)) {
    stop('Herm not done yet!')
    # SexPars$Herm <- checkHerm(SexPars$Herm, maxage, nsim, nyears, proyears)
  }
  
  # TODO - remove SPFrom if it remains in SRR
  if (!length(OM@SexPars@SPFrom))
    return(OM)
  
  if (isFALSE(OM@SexPars@SharePar))
    return(OM)
  
  sexmatches <- sapply(1:nrow(OM@SexPars@SPFrom), function(x) 
    paste(OM@SexPars@SPFrom[x, ], collapse = "_"))
  
  parcopy <- match(sexmatches, sexmatches)

 
  # if (!silent)  {
 
  cli::cli_alert_info("You have specified sex-specific dynamics, these parameters will be mirrored across sex types according to `SPFrom(OM)`:")
  cli::cli_ul()
  cli::cli_li(OM@SexPars@Misc$Stock)
  cli::cli_li(OM@SexPars@Misc$Fleet)
  cli::cli_li('Obs: All parameters')
  cli::cli_li('Imp: All parameters')
  cli::cli_end()
    # }
  
  
  for (s in 1:nStock(OM)) {
    # Stock
    for (sl in OM@SexPars@Misc$Stock) 
      slot(OM@Stock[[s]], sl) <- slot(OM@Stock[[parcopy[s]]], sl)
    
    for (fl in 1:nFleet(OM)) {
      # Fleet
      for (sl in OM@SexPars@Misc$Fleet) 
        slot(OM@Fleet[[s]][[fl]], sl) <- slot(OM@Fleet[[parcopy[s]]][[fl]], sl)
      
      # Obs
      if (OM@SexPars@Misc$Obs) {
        for (sl in slotNames(OM@Obs[[s]][[fl]]))
          slot(OM@Obs[[s]][[fl]], sl) <- slot(OM@Obs[[parcopy[s]]][[fl]], sl)
      }
      
      # Imp
      if (OM@SexPars@Misc$Imp) {
        for (sl in slotNames(OM@Imp[[s]][[fl]]))
          slot(OM@Imp[[s]][[fl]], sl) <- slot(OM@Imp[[parcopy[s]]][[fl]], sl)
      }
    }
  }
  OM
}


CalcUnfishedDist <- function(Spatial,
                             TimeSteps=NULL,
                             plot=FALSE,
                             nits=100) {
  dims <- dim(Spatial@Movement)
  if (is.null(dims))
    return(Spatial)
  UnfishedDist <- AddDimNames(array(NA, dim=c(dims[1],
                                              dims[2],
                                              dims[4],
                                              dims[5])),
                              c('Sim', 'Area', 'Age', 'Time Step'),
                              TimeSteps=TimeSteps)
  for (s in 1:dims[1]) {
    for (ts in 1:dims[5]) {
      for (age in 1:dims[4]) {
        UnfishedDist[s,,age,ts] <- CalcAsymptoticDist(Movement=Spatial@Movement[s,,,age,ts],
                                                      plot=plot, nits=nits)
      }
    }
  }
  Spatial@UnfishedDist <- UnfishedDist
  Spatial
}

#

#' Populate an object
#'
#' This function takes a valid S4 object and ...
#'
setGeneric("Populate", function(object, ...) standardGeneric("Populate"))

## ---- OM -----
#' @describeIn Populate Populate an [om-class()] object
#' @param x A Operating Model object or sub-object
#' @param messages Logical or character. 
#' FALSE to suppress all messages, 
#' TRUE to print all messages,
#' 'progress' to print key progress updates 
#' @export
setMethod("Populate", "om", function(object, messages='progress') {
  
  if (CheckDigest(list(), object) | EmptyObject(object))
    return(object)
  
  chk <- Check(object)
  
  if (methods::is(object@Stock, 'list'))
    class(object@Stock) <- 'StockList'

  if (methods::is(object@Stock, 'stock')) {
    nStocks <- 1
  } else if (methods::is(object@Stock, 'StockList')) {
    nStocks <- length(object@Stock)
  } else {
    cli::cli_abort('`Stock` must be a {.fun {"Stock"}} object or a list of {.fun {"Stock"}} objects')
  }
  
  if (methods::is(object@Fleet, 'list')) 
    class(object@Fleet) <- 'StockFleetList'
  
  if (methods::is(object@Fleet, 'fleet')) {
    nFleets <- 1
  } else if (methods::is(object@Fleet, 'StockFleetList')) {
    nFleets <- length(object@Fleet[[1]])
  } else {
    cli::cli_abort('`Fleet` must be a {.fun {"Fleet"}} object or a nested list of {.fun {"Fleet"}} objects for each {.fun {"Sleet"}} object')
  }

  # Populate Stock
  stockList <- vector('list', nStocks)
  names(stockList) <- paste('Stock', 1:nStocks)
  class(stockList) <- 'StockList'
  
  fleetList <- vector('list', nStocks)
  class(fleetList) <- 'StockFleetList'
  names(fleetList) <- paste('Stock', 1:nStocks)
  
  for (st in 1:nStocks) {
    if (isS4(object@Stock)) {
      stock <- object@Stock 
    } else {
      stock <- object@Stock[[st]]
    }
    
    if (!isFALSE(messages)) {
      if (nStocks>1) {
        cli::cli_alert('Populating Stock {.val {st}/{nStocks}}')
      } else {
        cli::cli_alert('Populating Stock')
      }
    }
 
    stock@nSim <- object@nSim
    stock@nYear <- object@nYear
    stock@pYear <- object@pYear
    stock@CurrentYear <- object@CurrentYear
   
    stockList[[st]] <- Populate(stock, seed=object@Seed, messages=messages)

    names(stockList)[st] <- stock@Name
    names(fleetList)[st] <- names(stockList)[st]
    fleetList[[st]] <- list()
    class(fleetList[[st]]) <- 'FleetList'
    
    for (fl in 1:nFleets) {
      if (isS4(object@Fleet)) {
        fleet <- object@Fleet
      } else if (inherits(object@Fleet, 'FleetList')) {
        fleet <- object@Fleet[[fl]]
      } else {
        fleet <- object@Fleet[[st]][[fl]]
      }
     

      fleetList[[st]][[fl]] <- Populate(fleet, 
                                        Ages=Ages(stockList[[st]]),
                                        Length=Length(stockList[[st]]),
                                        nsim=nSim(object),
                                        TimeSteps=TimeSteps(object),
                                        seed=object@Seed,
                                        messages=messages)
      
      names(fleetList[[st]])[fl] <- fleetList[[st]][[fl]]@Name
      
    }
  }
  
  if (methods::is(object@Stock, 'list') | methods::is(object@Stock, 'StockList')) {
    object@Stock <- stockList
  } 
  #else {
    #object@Stock <- stockList[[1]]
  #}
  
  if (methods::is(object@Fleet, 'list')| methods::is(object@Fleet, 'StockFleetList')) {
    object@Fleet <- fleetList
  }
  #else {
   # object@Fleet <- fleetList[[1]][[1]]    
  #}
  
  # share paramaters for two-sex stocks
  object <- object |> 
    UpdateSPFrom() |>
    ShareParameters() |> 
    StartMessages()
  
  # Obs and Imp
  
  # Update OM Timesteps
  # object@Stock[[1]]@Ages@Units
  # TimeUnits(object)
  # 
  # object@Stock
  # object@TimeUnits 
  
  SetDigest(list(), object)
  
  
})


## ---- Stock ----

#' @describeIn Populate Populate an [stock-class()] object
#' @param seed Seed for the random number generator
#' @export
setMethod("Populate", "stock", function(object,
                                        ALK=TRUE,
                                        AWK=FALSE,
                                        seed=NULL,
                                        messages='progress') {

  argList <- list(seed, ALK, AWK)
  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  stock <- object
  
  stock@TimeUnits <- stock@Ages@Units
  stock@TimeStepsPerYear <- TSperYear(stock@TimeUnits)
  stock@TimeSteps <- CalcTimeSteps(stock@nYear, 
                                   stock@pYear, 
                                   stock@CurrentYear, 
                                   stock@TimeUnits)
  


  # Require ALK and/or AWK?
  # ALK <- RequireALK(stock)
  # AWK <- RequireAWK(stock)

  stock@Length <- Populate(object=stock@Length,
                           Ages=stock@Ages,
                           nsim=nSim(stock),
                           TimeSteps=TimeSteps(stock),
                           ASK=ALK,
                           seed=seed,
                           messages=messages)
  

  stock@Weight <- Populate(stock@Weight,
                           Ages=stock@Ages,
                           Length=stock@Length,
                           nSim(stock),
                           TimeSteps=TimeSteps(stock),
                           ASK=AWK,
                           seed=seed,
                           messages=messages)

  stock@NaturalMortality <- Populate(stock@NaturalMortality,
                                     Ages=stock@Ages,
                                     Length=stock@Length,
                                     nsim=nSim(stock),
                                     TimeSteps=TimeSteps(stock),
                                     seed=seed,
                                     messages=messages)

  stock@Maturity <- Populate(stock@Maturity,
                             Ages=stock@Ages,
                             Length=stock@Length,
                             nsim=nSim(stock),
                             TimeSteps=TimeSteps(stock),
                             seed=seed,
                             messages=messages)
  
  stock@Fecundity <- Populate(object=stock@Fecundity,
                              Ages=stock@Ages,
                              Length=stock@Length,
                              Weight=stock@Weight,
                              Maturity=stock@Maturity,
                              nsim=nSim(stock),
                              TimeSteps=TimeSteps(stock),
                              seed=seed,
                              messages=messages)

  stock@SRR <- Populate(stock@SRR,
                        MaxAge=stock@Ages@MaxAge,
                        CurrentYear=stock@CurrentYear,
                        TimeSteps=stock@TimeSteps,
                        nsim=stock@nSim,
                        seed=seed,
                        messages=messages)

  stock@Spatial <- Populate(stock@Spatial,
                            Ages=stock@Ages,
                            TimeSteps=TimeSteps(stock),
                            nsim=stock@nSim,
                            seed=seed,
                            messages=messages)

  stock@Depletion <- Populate(stock@Depletion,
                            nsim=stock@nSim,
                            seed=seed,
                            messages=messages)
  
  
  SetDigest(argList, stock)
})



### ---- Length ----
#' @describeIn Populate Populate a [Length()] object
#' @param Ages A complete [Ages()] object
#'
#' @param TimeSteps Numeric vector of Time Steps
#' @param ASK Logical. Generate the age-length or age-weight key?
#' @export
setMethod("Populate", "length", function(object,
                                         Ages=NULL,
                                         nsim=NULL,
                                         TimeSteps=NULL,
                                         ASK=TRUE,
                                         seed=NULL,
                                         messages=TRUE) {

  TimeSteps <- TimeStepAttributes(object, TimeSteps)

  argList <- list(Ages, nsim, TimeSteps, ASK, seed)

  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  length <- object

  sb <- PrintPopulating(length, isTRUE(messages))
  
  length@Pars <- StructurePars(Pars=length@Pars, nsim, TimeSteps)
  length@Model <- FindModel(length)

  length <- PopulateMeanAtAge(length, Ages, TimeSteps)

  length <- PopulateRandom(length)

  length@CVatAge <- StructureCV(length@CVatAge, nsim)

  if (is.null(length@CVatAge))
    ASK <- FALSE

  if (ASK) {
    length <- PopulateClasses(length)
    length <- PopulateASK(length, Ages, isFALSE(messages))
  }

  length <- AddMeanAtAgeAttributes(length, TimeSteps, Ages)

  PrintDonePopulating(length, sb, print=isTRUE(messages))
  SetDigest(argList, length)
})


### ---- Weight ----
#' @describeIn Populate Populate a [Weight()] object
#' @param Length A complete [Length()] object
#' @export
setMethod("Populate", "weight", function(object,
                                         Ages=NULL,
                                         Length=NULL,
                                         nsim=NULL,
                                         TimeSteps=NULL,
                                         ASK=FALSE,
                                         CalcAtLength=FALSE,
                                         seed=NULL,
                                         messages=TRUE) {
  TimeSteps <- TimeStepAttributes(object, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, ASK, CalcAtLength, seed)

  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  sb <- PrintPopulating(length, isTRUE(messages))

  object@Pars <- StructurePars(Pars=object@Pars, nsim, TimeSteps)
  object@Model <- FindModel(object)

  ModelClass <- getModelClass(object@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(object@Model))) {
      CheckRequiredObject(Length, 'length', 'Length')
      chk <- Check(Length, silent=TRUE)
      if(!chk@populated) {
        CheckRequiredObject(Ages, 'ages', 'Ages')
        Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
      }
      object <- PopulateMeanAtLength(object, Length, TimeSteps, Ages, nsim,
                                     seed, silent)

    } else {
      object <- PopulateMeanAtAge(object, Ages, TimeSteps, Length)
    }
  }

  object <- MeanAtLength2MeanAtAge(object, Length, Ages, nsim, TimeSteps, seed, silent)
  if (CalcAtLength)
    object <- MeanAtAge2MeanAtLength(object, Length, Ages, nsim, TimeSteps, seed, silent)

  object <- PopulateRandom(object)
  object@CVatAge <- StructureCV(object@CVatAge, nsim)

  if (is.null(object@CVatAge))
    ASK <- FALSE

  if (ASK) {
    object <- PopulateClasses(object)
    object <- PopulateASK(object, Ages, !isFALSE(messages), type='Weight')
  }

  object <- AddMeanAtAgeAttributes(object, TimeSteps, Ages)

  PrintDonePopulating(object, sb, isTRUE(messages))
  SetDigest(argList, object)
})


### ---- NaturalMortality ----
#' @describeIn Populate Populate a [NaturalMortality()] object
#' @export
setMethod("Populate", "naturalmortality", function(object,
                                                   Ages=NULL,
                                                   Length=NULL,
                                                   nsim=NULL,
                                                   TimeSteps=NULL,
                                                   CalcAtLength=FALSE,
                                                   seed=NULL,
                                                   messages=TRUE) {

  TimeSteps <- TimeStepAttributes(object, TimeSteps)

  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)
  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  sb <- PrintPopulating(object, isTRUE(messages), name='NaturalMortality')

  object@Pars <- StructurePars(Pars=object@Pars, nsim, TimeSteps)
  object@Model <- FindModel(object)

  ModelClass <- getModelClass(object@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(object@Model))) {
      object <- PopulateMeanAtLength(object, Length, TimeSteps, Ages, nsim,
                                     seed, !isFALSE(messages))
    } else {
      object <- PopulateMeanAtAge(object, Ages, TimeSteps)
    }
  }

  object <- MeanAtLength2MeanAtAge(object, Length, Ages, nsim, TimeSteps, seed, !isFALSE(messages))
  if (CalcAtLength)
    object <- MeanAtAge2MeanAtLength(object, Length, Ages, nsim, TimeSteps, seed, !isFALSE(messages))

  object <- PopulateRandom(object)

  object <- AddMeanAtAgeAttributes(object, TimeSteps, Ages)

  PrintDonePopulating(object, sb, isTRUE(messages), name='NaturalMortality')
  SetDigest(argList, object)
})


### ---- Maturity ----
#' @describeIn Populate Populate a [Maturity()] object
#' @export
setMethod("Populate", "maturity", function(object,
                                           Ages=NULL,
                                           Length=NULL,
                                           nsim=NULL,
                                           TimeSteps=NULL,
                                           CalcAtLength=FALSE,
                                           seed=NULL,
                                           messages=TRUE) {
  TimeSteps <- TimeStepAttributes(object, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)
  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)
  sb <- PrintPopulating(object, isTRUE(messages))

  object@Pars <- StructurePars(Pars=object@Pars, nsim, TimeSteps)
  object@Model <- FindModel(object)

  ModelClass <- getModelClass(object@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(object@Model))) {
      object <- PopulateMeanAtLength(object, Length, TimeSteps, Ages, nsim,
                                     seed, silent)
    } else {
      object <- PopulateMeanAtAge(object, Ages, TimeSteps)
    }
  }

  object <- MeanAtLength2MeanAtAge(object, Length, Ages, nsim, TimeSteps, seed, silent)
  if (CalcAtLength)
    object <- MeanAtAge2MeanAtLength(object, Length, Ages, nsim, TimeSteps, seed, silent)

  object <- AddMeanAtAgeAttributes(object, TimeSteps, Ages)
  
  # Semelparous 
  if (inherits(object@Semelparous, 'array')) {
    
  } else {
    if (object@Semelparous) {
      object@Semelparous <- object@MeanAtAge 
    } else {
      object@Semelparous <- object@MeanAtAge 
      object@Semelparous[] <- 0
    }
  }



  PrintDonePopulating(object, sb, isTRUE(messages))
  SetDigest(argList, object)
})


### ---- Fecundity ----
#' @describeIn Populate Populate a [Fecundity()] object
#' @param Weight A complete [Weight()] object
#' @param Maturity A complete [Maturity()] object
#' @export
setMethod("Populate", "fecundity", function(object,
                                            Ages=NULL,
                                            Length=NULL,
                                            Weight=NULL,
                                            Maturity=NULL,
                                            nsim=NULL,
                                            TimeSteps=NULL,
                                            CalcAtLength=FALSE,
                                            seed=NULL,
                                            messages=TRUE) {
  TimeSteps <- TimeStepAttributes(object, TimeSteps)
  argList <- list(Ages, Length, Weight, Maturity, nsim, TimeSteps, CalcAtLength, seed)
  
  if (EmptyObject(object)) {
    if (!isFALSE(messages))
      cli::cli_alert_info('No `Fecundity` model found. Assuming Fecundity proportional to Spawning Biomass')
    
    CheckRequiredObject(Ages, 'ages', 'Ages')
    CheckRequiredObject(Weight, 'weight', 'Weight')
    # CheckRequiredObject(Length, 'length', 'Length')
    # CheckRequiredObject(Maturity, 'maturity', 'Maturity')
    
    Weight <- Populate(Weight, Ages, Length, nsim, TimeSteps, seed=seed, ASK=FALSE, messages=messages)
    # Maturity <- Populate(Maturity, Ages, Length, nsim, TimeSteps, seed=seed, messages=messages)
    # object@MeanAtAge <- MultiplyArrays(array1=Weight@MeanAtAge, array2=Maturity@MeanAtAge)
    object@MeanAtAge <- Weight@MeanAtAge # egg production is fecundity x maturity - calculated internally
    # fecundity is the egg production of a MATURE individual 
    
    PrintDonePopulating(object, sb=NULL, isTRUE(messages))
    return(SetDigest(argList, object))
  }

  if (CheckDigest(argList, object))
    return(object)

  SetSeed(object, seed)

  sb <- PrintPopulating(object, isTRUE(messages))

  object@Pars <- StructurePars(Pars=object@Pars, nsim, TimeSteps)
  object@Model <- FindModel(object)

  if (is.null(object@Model)| all(is.na(object@Pars))) {
    if (is.null(object@MeanAtAge)) {
      if (!isFALSE(messages))
        cli::cli_alert_info('No `Fecundity` model found. Assuming Fecundity proportional to Spawning Biomass')
      
      CheckRequiredObject(Ages, 'ages', 'Ages')
      CheckRequiredObject(Weight, 'weight', 'Weight')
      CheckRequiredObject(Length, 'length', 'Length')
      # CheckRequiredObject(Maturity, 'maturity', 'Maturity')
      
      Weight <- Populate(Weight, Ages, Length, nsim, TimeSteps, seed=seed, ASK=FALSE, messages=messages)
      # Maturity <- Populate(Maturity, Ages, Length, nsim, TimeSteps, seed=seed, messages=messages)
      # object@MeanAtAge <- MultiplyArrays(array1=Weight@MeanAtAge, array2=Maturity@MeanAtAge)
      object@MeanAtAge <- Weight@MeanAtAge # egg production is fecundity x maturity - calculated internally
      # fecundity is the egg production of a MATURE individual 
      
      PrintDonePopulating(object, sb, isTRUE(messages))
      return(SetDigest(argList, object))
    }
  }

  ModelClass <- getModelClass(object@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(object@Model))) {
      object <- PopulateMeanAtLength(object, Length, TimeSteps, Ages, nsim,
                                     seed, isTRUE(messages))
    } else {
      object <- PopulateMeanAtAge(object, Ages, TimeSteps)
    }
  }

  object <- MeanAtLength2MeanAtAge(object, Length, Ages, nsim, TimeSteps, seed, isTRUE(messages))
  if (CalcAtLength)
    object <- MeanAtAge2MeanAtLength(object, Length, Ages, nsim, TimeSteps, seed, isTRUE(messages))

  object <- AddMeanAtAgeAttributes(object, TimeSteps, Ages)

  PrintDonePopulating(object, sb, isTRUE(messages))
  SetDigest(argList, object)
})

### ---- SRR ----
#' @describeIn Populate Populate a [SRR()] object
#' @param nHistTS Number of historical time-steps
#' @param nProjTS Number of projection time-steps
#'
#' @export
setMethod("Populate", "srr", function(object,
                                      MaxAge=NULL,
                                      CurrentYear=NULL,
                                      TimeSteps=NULL,
                                      nsim=NULL,
                                      seed=NULL,
                                      messages=TRUE) {

  argList <- list(MaxAge, CurrentYear, TimeSteps, nsim, seed)

  if (is.null(MaxAge))
    cli::cli_abort('`MaxAge` cannot be NULL')

  if (is.null(CurrentYear))
    cli::cli_abort('`CurrentYear` cannot be NULL')

  if (is.null(TimeSteps))
    cli::cli_abort('`TimeSteps` cannot be NULL')

  if (is.null(nsim)) {
    cli::cli_alert_info('`nsim` not specified. Assuming `nsim=1` and no recruitment process error.')
    nsim <-1
  }
  
  tTimeSteps <- floor(TimeSteps)
  histTS <- TimeSteps[tTimeSteps<=CurrentYear]
  projTS <- TimeSteps[tTimeSteps>CurrentYear]
  nHistTS <- length(histTS)
  nProjTS <- length(projTS)

  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  sb <- PrintPopulating(object, isTRUE(messages), allup=TRUE)
  object@Pars <- StructurePars(Pars=object@Pars, nsim, TimeSteps)
  object@Model <- FindModel(object)


  pars <- StructurePars(list(object@R0, object@SD, object@AC), nsim, TimeSteps)
  object@R0 <- pars[[1]][,1, drop=FALSE] # only one time step for now
  object@SD <- pars[[2]][,1, drop=FALSE] # only one time step for now
  object@AC <- pars[[3]][,1, drop=FALSE] # only one time step for now
  object@SD[object@SD==0] <- 1E-6 # for reproducibility in rnorm

  EmptyObjects <- c(EmptyObject(object@RecDevInit),
                     EmptyObject(object@RecDevHist),
                     EmptyObject(object@RecDevProj))

  if (all(!EmptyObjects)) {
    PrintDonePopulating(object, sb, isTRUE(messages), allup=TRUE)
    return( SetDigest(argList, object))
  }

  RecDeviations <- GenerateRecruitmentDeviations(SD=object@SD,
                                   AC=object@AC,
                                   TruncSD=object@TruncSD,
                                   MaxAge,
                                   nHistTS,
                                   nProjTS,
                                   nsim,
                                   RecDevInit=object@RecDevInit,
                                   RecDevHist=object@RecDevHist,
                                   RecDevProj=object@RecDevProj)

  object@RecDevInit <- RecDeviations$RecDevInit
  dimnames(object@RecDevInit) <- list(
    Sim=1:nrow(object@RecDevInit),
    Age=1:ncol(object@RecDevInit)
  )
  
  object@RecDevHist <- RecDeviations$RecDevHist
  dimnames(object@RecDevHist) <- list(
    Sim=1:nrow(object@RecDevHist),
    `Time Step`=histTS
  )
  
  object@RecDevProj <- RecDeviations$RecDevProj
  dimnames(object@RecDevProj) <- list(
    Sim=1:nrow(object@RecDevProj),
    `Time Step`=projTS
  )
  

  PrintDonePopulating(object, sb, isTRUE(messages), allup=TRUE)
  SetDigest(argList, object)
})


### ---- Spatial ----


#' @describeIn Populate Populate a [Spatial()] object
#' @export
setMethod("Populate", "spatial", function(object,
                                          Ages=NULL,
                                          TimeSteps=NULL,
                                          nsim=NULL,
                                          seed=NULL,
                                          messages=TRUE,
                                          plot=FALSE,
                                          nits=100) {

  argList <- list(Ages, nsim, seed, nits)

  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  # empty object
  DimNames <- c('Sim', 'Area', 'Age', 'Time Step')
  if (EmptyObject(object)) {
    object@RelativeSize <- AddDimNames(array(1, dim=c(1,1)), 
                                       DimNames[1:2], TimeSteps=TimeSteps)
    object@ProbStaying <- AddDimNames(array(1, dim=c(1,1,1,1)), 
                                      DimNames, TimeSteps=TimeSteps)
    object@FracOther <- AddDimNames(array(1, dim=c(1,1,1,1)),
                                    DimNames, TimeSteps=TimeSteps)
    object@UnfishedDist  <- AddDimNames(array(1, dim=c(1,1,1,1)),
                                        DimNames, TimeSteps=TimeSteps)
    object@Movement  <- AddDimNames(array(1, dim=c(1,1,1,1,1)),
                                    DimNames[c(1,2,2,3,4)], TimeSteps=TimeSteps)
    return(SetDigest(argList, object))
  }


  sb <- PrintPopulating(object, isTRUE(messages))
  
  if (is.null(object@Movement))
    object <- CalcMovement(object, TimeSteps, nsim, seed, nits, plot, silent=isFALSE(messages))
  
  if (is.null(object@UnfishedDist)) {
    object <- CalcUnfishedDist(object, TimeSteps)
  } 


  if (is.null(object@UnfishedDist))
    cli::cli_abort('`UnfishedDist` must be populated for `Spatial` objects')

  # if (is.null(object@ProbStaying))
  #   cli::cli_abort('`ProbStaying` must be populated for `Spatial` objects')

  nareas <- dim(object@UnfishedDist)[2]

  if (!is.null(object@RelativeSize) & !methods::is(object@RelativeSize, 'character')) {
    object@RelativeSize <- StructurePars(list(object@RelativeSize),nsim)[[1]]
    dd <- dim(object@RelativeSize)
    if (dd[2]>nareas)
      cli::cli_abort('`RelativeSize` is longer than `nAreas` ({.val {nareas}})')

    if (dd[2]==1 & nareas==2) {
      RelativeSize <- array(0, dim=c(dd[1], 2))
      RelativeSize[,1] <- object@RelativeSize
      RelativeSize[,2] <- 1- RelativeSize[,1]
      object@RelativeSize <- RelativeSize
    }

    if (nareas>2) {
      if (dd[2]<nareas)
        cli::cli_abort('`RelativeSize` must have `nAreas` ({.val {nareas}}) columns')

      rowsums <- apply(object@RelativeSize, 1, sum)
      if (!all(rowsums==1))
        cli::cli_abort('`RelativeSize` must sum to 1 across columns')
    }

  } else if (methods::is(object@RelativeSize, 'character')) {
    if (object@RelativeSize=="EqualDensity") {
      object@RelativeSize <- apply(object@UnfishedDist, 1:2, mean)   
    } else {
      cli::cli_abort('If `Spatial@RelativeSize` is character, it can only be "EqualDensity"')
    }
  } else {
    cli::cli_alert_warning('`RelativeSize` is not specified. Assuming all areas are equal size')
    object@RelativeSize <- matrix(1/nareas, 1, nareas)
  }
  object@RelativeSize <- AddDimNames(object@RelativeSize, c('Sim', 'Area'))

  PrintDonePopulating(object, sb, isTRUE(messages))
  SetDigest(argList, object)
})



### ---- Depletion ----
#' @describeIn Populate Populate a [Depetion()] object
#' @export
setMethod("Populate", "depletion", function(object,
                                         nsim=NULL,
                                         seed=NULL,
                                         messages=TRUE) {

  argList <- list(nsim, seed)

  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  sb <- PrintPopulating(object, isTRUE(messages))

  if (!is.null(object@Initial)) {
    if (length(object@Initial)==2) {
      object@Initial <- StructurePars(list(object@Initial), nsim)[[1]][,1]
    }
  }

  if (!is.null(object@Final)) {
    if (length(object@Final)==2) {
      object@Final <- StructurePars(list(object@Final), nsim)[[1]][,1]
    }
  }

  if (!object@Reference %in% c('B0', 'BMSY'))
    cli::cli_abort('`Reference` must be either `B0` or `BMSY`')

  PrintDonePopulating(object, sb, isTRUE(messages))
  SetDigest(argList, object)
})



# ---- Fleet ----

#' @describeIn Populate Populate an [stock-class()] object
#' @param seed Seed for the random number generator
#' @export
setMethod("Populate", "fleet", function(object,
                                        Ages=NULL,
                                        Length=NULL,
                                        nsim=NULL,
                                        TimeSteps=NULL,
                                        seed=NULL,
                                        messages=TRUE) {
  
  argList <- list(Ages, Length, nsim, TimeSteps, seed)
  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)
  
  SetSeed(object, seed)
  
  fleet <- object
  
  fleet@FishingMortality <- Populate(fleet@FishingMortality,
                                      nsim,
                                      TimeSteps,
                                      seed,
                                      messages=messages
                                      )
  
  fleet@DiscardMortality <- Populate(fleet@DiscardMortality,
                                      Ages,
                                      Length,
                                      nsim,
                                      TimeSteps,
                                      seed=seed,
                                     messages=messages)

  fleet@Selectivity <- Populate(fleet@Selectivity,
                                FishingMortality=fleet@FishingMortality,
                                DiscardMortality=fleet@DiscardMortality,
                                Ages,
                                Length,
                                nsim,
                                TimeSteps,
                                CalcAtLength=FALSE,
                                seed,
                                messages=messages)

  fleet@Retention <- Populate(fleet@Retention,
                              FishingMortality=fleet@FishingMortality,
                              DiscardMortality=fleet@DiscardMortality,
                              Ages,
                              Length,
                              nsim,
                              TimeSteps,
                              CalcAtLength=FALSE,
                              seed,
                              messages=messages)
  
  fleet@Effort <- Populate(fleet@Effort,
                           FishingMortality=fleet@FishingMortality,
                           DiscardMortality=fleet@DiscardMortality,
                           Ages,
                           Length,
                           nsim,
                           TimeSteps,
                           seed,
                           messages)
  
  
  SetDigest(argList, fleet)


})

### ---- FishingMortality ----
#' @describeIn Populate Populate a [FishingMortality()] object
#' @export
setMethod("Populate", "fishingmortality", function(object,
                                                   nsim=NULL,
                                                   TimeSteps=NULL,
                                                   seed=NULL,
                                                   messages=TRUE) {


  TimeSteps <- TimeStepAttributes(object, TimeSteps)
  argList <- list(nsim, TimeSteps, seed)

  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)
  sb <- PrintPopulating(object, isTRUE(messages), name='FishingMortality')

  object@ApicalF <- AddSimDimension(object@ApicalF, c('Sim', 'Time Step'), TimeSteps=TimeSteps)
  object@DeadAtAge <- AddSimDimension(object@DeadAtAge, TimeSteps=TimeSteps)
  object@RetainAtAge <- AddSimDimension(object@RetainAtAge, TimeSteps=TimeSteps)

  if (EmptyObject(object@ApicalF)) # calculate from `DeadAtAge`
    object@ApicalF <- apply(object@DeadAtAge, c(1,3), max)

  PrintDonePopulating(object, sb, isTRUE(messages), name='FishingMortality')
  SetDigest(argList, object)
})



### ---- DiscardMortality ----
#' @describeIn Populate Populate a [DiscardMortality()] object
#' @export
setMethod("Populate", "discardmortality", function(object,
                                                   Ages=NULL,
                                                   Length=NULL,
                                                   nsim=NULL,
                                                   TimeSteps=NULL,
                                                   CalcAtLength=FALSE,
                                                   seed=NULL,
                                                   messages=TRUE) {

  TimeSteps <- TimeStepAttributes(object, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)

  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)

  SetSeed(object, seed)

  sb <- PrintPopulating(object, isTRUE(messages), name='DiscardMortality')

  object <- MeanAtLength2MeanAtAge(object, Length, Ages, nsim, TimeSteps, seed, isFALSE(messages))
  if (CalcAtLength)
    object <- MeanAtAge2MeanAtLength(object, Length, Ages, nsim, TimeSteps, seed, isFALSE(messages))

  object <- AddMeanAtAgeAttributes(object, TimeSteps, Ages)

  PrintDonePopulating(object, sb, isTRUE(messages), name='DiscardMortality')
  SetDigest(argList, object)
})




### ---- Selectivity ----
#' @describeIn Populate Populate a [Selectivity()] object
#' @export
setMethod("Populate", "selectivity", function(object,
                                              FishingMortality=NULL,
                                              DiscardMortality=NULL,
                                              Ages=NULL,
                                              Length=NULL,
                                              nsim=NULL,
                                              TimeSteps=NULL,
                                              CalcAtLength=FALSE,
                                              seed=NULL,
                                              messages=TRUE,
                                              CheckMaxValue=TRUE) {
  
  TimeSteps <- TimeStepAttributes(object, TimeSteps)
  argList <- list(FishingMortality, DiscardMortality, Ages, Length, 
                  TimeSteps, nsim, CalcAtLength, seed)

  if (CheckDigest(argList, object))
    return(object)

  SetSeed(object, seed)
  sb <- PrintPopulating(object, isTRUE(messages))
  
  selectivity <- object
  
  selectivity@Pars <- StructurePars(Pars=selectivity@Pars, nsim, TimeSteps)
  selectivity@Model <- FindModel(selectivity)
  
  ModelClass <- getModelClass(selectivity@Model)
 
  if (!is.null(ModelClass)) {
    
    if (grepl('at-Length',getModelClass(selectivity@Model))) {
      selectivity <- PopulateMeanAtLength(selectivity, 
                                          Length, 
                                          TimeSteps,
                                          Ages, 
                                          nsim,
                                          seed, 
                                          isFALSE(messages))
    } else {
      selectivity <- PopulateMeanAtAge(selectivity, Ages, TimeSteps)
    }
  } 
  
  selectivity <- MeanAtLength2MeanAtAge(selectivity, Length, Ages, nsim,
                                        TimeSteps, seed, isTRUE(messages))
  
  if (CalcAtLength)
    selectivity <- MeanAtAge2MeanAtLength(selectivity, Length, Ages, nsim, TimeSteps, seed, isTRUE(messages))
  
  if (is.null(selectivity@MeanAtAge)) {
    chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    if (!chk@populated)
      FishingMortality <- Populate(FishingMortality,
                                   nsim,
                                   TimeSteps,
                                   seed,
                                   messages)
    
    if (!EmptyObject(FishingMortality@DeadAtAge)) {
      selectivity@MeanAtAge <- FishingMortality2Selectivity(FishingMortality,
                                                       DiscardMortality,
                                                       Ages,
                                                       TimeSteps,
                                                       Length)
    } else {
      cli::cli_abort('`Selectivity` requires either `Pars`, `MeanAtAge` or a `FishingMortality` object')
    }
  }
  
  # Check selectivity has a max value of one across age classes
  if(CheckMaxValue) 
    selectivity@MeanAtAge <- CheckSelectivityMaximum(selectivity@MeanAtAge)

  selectivity <- AddMeanAtAgeAttributes(selectivity, TimeSteps, Ages)
  PrintDonePopulating(selectivity, sb, isTRUE(messages))
  SetDigest(argList, selectivity)

})

CheckSelectivityMaximum <- function(MeanAtAge) {
  MaxValues <- apply(MeanAtAge, c(1,3), max) |> round(3)
  
  ind <- MaxValues<1 & MaxValues!=0
  if (all(!ind))
    return(MeanAtAge)
  
  cli::cli_alert_warning("WARNING: Selectivity-at-Age does not have a maximum value of 1. F-at-Age won't correspond with Apical F")
  cli::cli_alert_warning('Standardizing to a max value of 1 but you probably want to fix this in the OM')
  
  sims <- which(apply(ind,1, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
  TSs <- which(apply(ind, 2, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))

  cli::cli_alert('Simulations {.val {sims}}; Time Steps {.val {TSs}}')

  for (i in 1:nrow(ind)) {
    for (j in 1:ncol(ind)) {
      if (ind[i,j]==FALSE)
        next()
      MeanAtAge[i, ,j] <- MeanAtAge[i, ,j]/max(MeanAtAge[i, ,j])
    }
  }
  
  MeanAtAge
}


### ---- Retention ----
#' @describeIn Populate Populate a [Retention()] object
#' @export
setMethod("Populate", "retention", function(object,
                                            FishingMortality=NULL,
                                            DiscardMortality=NULL,
                                            Ages=NULL,
                                            Length=NULL,
                                            nsim=NULL,
                                            TimeSteps=NULL,
                                            CalcAtLength=FALSE,
                                            seed=NULL,
                                            messages=TRUE) {

  TimeSteps <- TimeStepAttributes(object, TimeSteps)
  argList <- list(FishingMortality, DiscardMortality, Ages, Length, 
                  TimeSteps, nsim, CalcAtLength, seed)
  
  
  if (CheckDigest(argList, object) | EmptyObject(object))
    return(object)
  
  SetSeed(object, seed)
  sb <- PrintPopulating(object, isTRUE(messages))
  
  retention <- object
  
  retention@Pars <- StructurePars(Pars=retention@Pars, nsim, TimeSteps)
  
  ParsZero <- all((lapply(lapply(retention@Pars, `==`, 0), prod) |> unlist())==1)
  
  if (!ParsZero) {
    retention@Model <- FindModel(retention)
    ModelClass <- getModelClass(retention@Model)
    
    if (!is.null(ModelClass)) {
      if (grepl('at-Length',getModelClass(retention@Model))) {
        retention <- PopulateMeanAtLength(retention, Length, TimeSteps, Ages, nsim,
                                          seed, isTRUE(messages))
      } else {
        retention <- PopulateMeanAtAge(retention, Ages, TimeSteps)
      }
    } 
    
  }
  
  if (ParsZero & is.null(retention@MeanAtAge) & is.null(retention@MeanAtLength)) {
    
    retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> AddDimNames(TimeSteps=TimeSteps)
    retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> AddDimNames(c('Age', 'Length Class', 'Time Step'),
                                                                    TimeSteps=TimeSteps)
    
    PrintDonePopulating(retention, sb, isTRUE(messages))
    SetDigest(argList, retention)
  } 
  

  retention <- MeanAtLength2MeanAtAge(retention, Length, Ages, nsim, TimeSteps, seed, isFALSE(messages))
  if (CalcAtLength)
    retention <- MeanAtAge2MeanAtLength(retention, Length, Ages, nsim, TimeSteps, seed, isFALSE(messages))
  
  if (is.null(retention@MeanAtAge)) {
    chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    if (!chk@populated)
      FishingMortality <- Populate(FishingMortality,
                                   nsim,
                                   TimeSteps,
                                   seed,
                                   messages)
    
    if (!EmptyObject(FishingMortality@DeadAtAge)) {
      retention@MeanAtAge <- FishingMortality2Retention(FishingMortality,
                                                        DiscardMortality,
                                                        Ages,
                                                        TimeSteps,
                                                        Length)
    } else {
      cli::cli_abort('`Retention` requires either `Pars`, `MeanAtAge` or a `FishingMortality` object')
    }
  }
  
  
  retention <- AddMeanAtAgeAttributes(retention, TimeSteps, Ages)
  PrintDonePopulating(retention, sb, isTRUE(messages))
  SetDigest(argList, retention)
 
})



### ---- Effort ----
#' @describeIn Populate Populate a [Effort()] object
#' @export
setMethod("Populate", "effort", function(object,
                                         FishingMortality=NULL,
                                         DiscardMortality=NULL,
                                         Ages=NULL,
                                         Length=NULL,
                                         nsim=NULL,
                                         TimeSteps=NULL,
                                         seed=NULL,
                                         messages=TRUE) {

  argList <- list(FishingMortality,
                  DiscardMortality,
                  Ages,
                  Length,
                  TimeSteps,
                  seed)

  if (CheckDigest(argList, object))
    return(object)

  SetSeed(object, seed)

  sb <- PrintPopulating(object, isTRUE(messages))

  if (EmptyObject(object@Catchability)) {
    # if (!silent)
      # cli::cli_alert_info('`Catchability` (q) not populated. Assuming `q=1`')
    object@Catchability <- matrix(NA,1,1) |> AddDimNames(c('Sim', 'Time Step'),
                                                        TimeSteps=TimeSteps)
  }

  if (EmptyObject(object@Effort)) {
    FInteract <- CalculateFInteract(FishingMortality,
                                    DiscardMortality,
                                    Ages,
                                    TimeSteps,
                                    Length)
    dd <- dim(FInteract)
    object@Effort <- MultiplyArrays(array1=object@Catchability,
                                    array2=apply(FInteract,c(1,3), max)
                                    ) |>
      AddDimNames(c('Sim', 'Time Step'), TimeSteps=TimeSteps)


  } else {
    if (methods::is(object@Effort, 'data.frame')) {
      object@Effort <- GenerateHistoricalEffort(object@Effort, nsim, TimeSteps)
      
    }
  }



  PrintDonePopulating(object, sb, isTRUE(messages))
  SetDigest(argList, object)

})


GenerateHistoricalEffort <- function(Effort, nsim=NULL, TimeSteps=NULL) {
  if (!methods::is(Effort, 'data.frame'))
    cli::cli_abort('`Effort` must be a data.frame')
  
  if (!all(names(Effort) %in% c("TimeStep", "Lower", "Upper", "CV" )))
    cli::cli_abort('`Effort` must be a data.frame with columns: "TimeStep", "Lower", "Upper", "CV" ')
  
  if (is.null(nsim)) {
    cli::cli_warn('`nsim` not specified, assuming `nsim=100`')
    nsim <- 100
  }
    
    
  if (is.null(TimeSteps)) {
    cli::cli_warn('`TimeSteps` not specified, assuming last time step is: {.val {max(Effort$TimeStep)}}')
    TimeSteps <- 1:max(Effort$TimeStep)
  }
    
  if (all(Effort$TimeStep< 1000)) {
    chk <- max(Effort$TimeStep) %in% seq_along(TimeSteps)  
    if (!chk)
      cli::cli_abort('`max(Effort$TimeStep)` ({.val {max(Effort$TimeStep)}})')
  } else {
    chk <- max(Effort$TimeStep) %in% TimeSteps
    if (!chk)
      cli::cli_abort('`max(Effort$TimeStep)` ({.val {max(Effort$TimeStep)}})')
  }
  
  nTimeSteps <- length(TimeSteps)
  
  EffortPoints <- mapply(runif, n = nsim, min = Effort$Lower, max = Effort$Upper)  # sample Effort
  EffortTS <- t(sapply(1:nsim, function(x) 
    approx(x = Effort$TimeStep,
           y = EffortPoints[x, ], 
           method = "linear", 
           n = nTimeSteps)$y)
    ) 
  
  Esd <- Effort$CV[1]
  if (!is.null(Esd)) {
    Emu <- -0.5 * Esd^2
    EffortError <- array(exp(rnorm(nTimeSteps * nsim, rep(Emu, nTimeSteps), 
                                   rep(Esd, nTimeSteps))), 
                         c(nsim, nTimeSteps))  
    EffortTS <- EffortTS * EffortError
  }
  EffortTS <- EffortTS |> AddDimNames(names=c('Sim', 'Time Step'), 
                                      TimeSteps = round(TimeSteps,2))
  
  EffortTS/matrix(EffortTS[,nTimeSteps], nsim, nTimeSteps, byrow=FALSE)
}




# PopulateStock <- function(stock, silent=FALSE) {
#   if (CheckDigest(args=list(), stock)) return(stock)
#
#   # Timesteps
#   stock <- PopulateTimesteps(stock)
#
#
#   # Models
#   stock@Length@Model <- PopulateModel(stock@Length)
#   stock@Weight@Model <- PopulateModel(stock@Weight)
#   stock@NaturalMortality@Model <- PopulateModel(stock@NaturalMortality)
#   stock@Maturity@Model <- PopulateModel(stock@Maturity)
#   stock@Fecundity@Model <- PopulateModel(stock@Fecundity)
#   stock@SRR@Model <- PopulateModel(stock@SRR)
#
#   stock@Fleet@Selectivity@Model
#   stock@Fleet@Retention@Model
#
#
#
#   # Require ALK?
#   # Require AWK?
#
#   ALK <- RequireALK(stock)
#   AWK <- TRUE
#
#
#   stock@Length <- PopulateLength(Length=stock@Length,
#                                  Ages=stock@Ages,
#                                  nsim=stock@Meta$nsim,
#                                  nTS=stock@Meta$nTS,
#                                  ASK = ALK,
#                                  silent=silent)
#
#   stock@Weight <- PopulateWeight(Weight=stock@Weight,
#                                  Ages=stock@Ages,
#                                  Length=stock@Length,
#                                  nsim=stock@Meta$nsim,
#                                  nTS=stock@Meta$nTS,
#                                  AWK=AWK,
#                                  silent=silent)
#
#   stock@NaturalMortality <- PopulateNaturalMortality(NaturalMortality=stock@NaturalMortality,
#                                                      Ages=stock@Ages,
#                                                      Length=stock@Length,
#                                                      nsim=stock@Meta$nsim,
#                                                      nTS=stock@Meta$nTS,
#                                                      silent=silent)
#
#   stock@Maturity <- PopulateMaturity(Maturity=stock@Maturity,
#                                      Ages=stock@Ages,
#                                      Length=stock@Length,
#                                      nsim=stock@Meta$nsim,
#                                      nTS=stock@Meta$nTS,
#                                      silent=silent)
#
#   stock@Fecundity <- PopulateFecundity(Fecundity=stock@Fecundity,
#                                        Ages=stock@Ages,
#                                        Length=stock@Length,
#                                        Weight=stock@Weight,
#                                        Maturity=stock@Maturity,
#                                        nsim=stock@Meta$nsim,
#                                        nTS=stock@Meta$nTS,
#                                        silent=silent)
#
#   stock@SRR <- PopulateSRR(SRR=stock@SRR,
#                            Ages=stock@Ages,
#                            nsim=stock@Meta$nsim,
#                            nTS=stock@Meta$nTS,
#                            nHistTS=stock@Meta$nHistTS,
#                            nProjTS=stock@Meta$nProjTS,
#                            silent=silent)
#
#
#
#
#   # Spatial
#   Spatial <- PopulateSpatial(Spatial=stock@Spatial,
#                              Ages=stock@Ages,
#                              nsim=stock@Meta$nsim,
#                              nTS=stock@Meta$nTS,
#                              silent=silent)
#
# }
#



# PopulateLength <- function(Length, Ages, nsim, nTS, ASK=FALSE, silent=FALSE) {
#
#   argList <- list(Ages, nsim, nTS, ASK)
#   object <- Length
#
#   if (CheckDigest(args=argList, object))
#     return(object)
#
#   if (EmptyObject(object))
#     return(SetDigest(argList, object))
#
#   if (!silent)
#     cli::cli_alert_info('Populating `Length`')
#
#   object <- PopulateMeanAtAge(object, nsim, nTS, Ages, object)
#   object <- PopulateRandom(object)
#   object <- PopulateSDatAge(object, nsim, nage=length(Ages@Classes), nTS)
#   object <- PopulateClasses(object)
#   if (ASK)
#     object <- PopulateASK(object, silent)
#
#   SetDigest(argList, object)
# }


# PopulateWeight <- function(Weight, Ages, Length, nsim, nTS, AWK=FALSE, silent=FALSE) {
#
#   argList <- list(Ages, Length, nsim, nTS, AWK)
#   object <- Weight
#
#   if (CheckDigest(args=argList, object))
#     return(object)
#
#   if (EmptyObject(object))
#     return(SetDigest(argList, object))
#
#   if (!silent)
#     cli::cli_alert_info('Populating `Weight`')
#
#   object <- PopulateMeanAtAge(object, nsim, nTS, Ages, Length)
#   object <- PopulateRandom(object)
#   object <- PopulateSDatAge(object, nsim, nage=length(Ages@Classes), nTS)
#   object <- PopulateClasses(object)
#   if (AWK)
#     object <- PopulateASK(object, Ages, silent)
#
#   SetDigest(argList, object)
# }

# PopulateNaturalMortality <- function(NaturalMortality, Ages, Length, nsim, nTS, silent=FALSE) {
#
#   argList <- list(Ages, Length, nsim, nTS)
#   object <- NaturalMortality
#
#   if (CheckDigest(args=argList, object))
#     return(object)
#
#   if (EmptyObject(object))
#     return(SetDigest(argList, object))
#
#   if (!silent)
#     cli::cli_alert_info('Populating `NaturalMortality`')
#
#   object <- PopulateMeanAtAge(object=object, nsim, nTS, Ages, Length)
#   object <- PopulateRandom(object)
#
#
#   # populate mean at length from mean-at-age
#   # or reverse
#   # dim(NaturalMortality@MeanAtAge)
#   # dim(Length@ASK)
#   # Length@Classes
#   # NaturalMortality@MeanAtLength
#   # Length@Classes
#
#   SetDigest(argList, object)
# }

# PopulateMaturity <- function(Maturity, Ages, Length, nsim, nTS, silent=FALSE) {
#
#   argList <- list(Ages, Length, nsim, nTS)
#   object <- Maturity
#
#   if (CheckDigest(args=argList, object))
#     return(object)
#
#   if (EmptyObject(object))
#     return(SetDigest(argList, object))
#
#   if (!silent)
#     cli::cli_alert_info('Populating `Maturity`')
#
#   object <- PopulateMeanAtAge(object, nsim, nTS, Ages, Length)
#   object <- PopulateMeanAtLength(object, nsim, nTS, Length)
#   if (is.null(object@MeanAtAge))
#     object@MeanAtAge <- AtSize2AtAge(object@MeanAtLength, Length@ASK)
#
#   SetDigest(argList, object)
# }

# PopulateFecundity <- function(Fecundity, Ages, Length, Weight, Maturity, nsim, nTS, silent=FALSE) {
#
#   argList <- list(Ages, Length, Weight, Maturity, nsim, nTS)
#   object <- Fecundity
#
#   if (CheckDigest(args=argList, object))
#     return(object)
#
#   if (EmptyObject(object))
#     return(SetDigest(argList, object))
#
#   if (!silent)
#     cli::cli_alert_info('Populating `Fecundity`')
#
#   if (is.null(object@Model))
#     object@Model <- PopulateModel(object)
#
#
#   if (is.null(object@Model)) {
#     if (!silent)
#       cli::cli_alert_info('No `Fecundity` model found. \nAssuming Fecundity proportional to Spawning Biomass')
#
#     object@MeanAtAge <- MultiplyArrays(Weight@MeanAtAge, Maturity@MeanAtAge)
#     return(SetDigest(argList, object))
#   }
#
#   object <- PopulateMeanAtAge(object, nsim, nTS, Ages, Length)
#   object <- PopulateMeanAtLength(object, nsim, nTS, Length)
#   if (is.null(object@MeanAtAge))
#     object@MeanAtAge <- AtSize2AtAge(object@MeanAtLength, Length@ASK)
#
#   SetDigest(argList, object)
# }

# PopulateSRR <- function(SRR, Ages, nsim, nTS, nHistTS, nProjTS, silent=FALSE) {
#
#   argList <- list(Ages, nsim, nTS, nHistTS, nProjTS)
#   object <- SRR
#
#   if (CheckDigest(args=argList, object))
#     return(object)
#
#   if (EmptyObject(object))
#     return(SetDigest(argList, object))
#
#   if (!silent)
#     cli::cli_alert_info('Populating `SRR`')
#
#   object@Model <- FindModel(object)
#   maxage <- MaxAge(Ages)
#
#   pars <- StructurePars(list(SD=object@SD, AC=object@AC), nsim, nTS=0)
#   object@SD <- pars$SD[,1]
#   object@AC <- pars$AC[,1]
#   object@SD[object@SD==0] <- 1E-6 # for reproducibility in rnorm
#
#   EmptyObjects <- c(EmptyObject(object@RecDevInit),
#                      EmptyObject(object@RecDevHist),
#                      EmptyObject(object@RecDevProj))
#
#   if (any(!EmptyObjects) &&  (!all(!EmptyObjects))) {
#     slots <- c('RecDevInit', 'RecDevHist', 'RecDevProj')
#     populated <- paste(slots[!EmptyObjects], collapse=', ')
#     not_populated <- paste(slots[EmptyObjects], collapse=', ')
#
#     cli::cli_alert_warning(paste0("Slot(s): ", populated,
#                                   ' populated but: ',
#                                   not_populated,
#                                   ' are not. \nIgnoring and generating new recruitment deviations from `SD` and `AC`'))
#   } else if (all(!EmptyObjects)) {
#     return( SetDigest(argList, object))
#   }
#
#   RecDeviations <- GenerateRecDevs(SD=object@SD,
#                                    AC=object@AC,
#                                    nTS,
#                                    nHistTS,
#                                    nProjTS,
#                                    maxage,
#                                    nsim)
#
#   object@RecDevInit <- RecDeviations$RecDevInit
#   object@RecDevHist <- RecDeviations$RecDevHist
#   object@RecDevProj <- RecDeviations$RecDevProj
#
#   SetDigest(argList, object)
# }





# PopulateSpatial <- function(Spatial, Ages, nsim, nTS, silent=FALSE) {
#   if (CheckDigest(args=list(Ages, nsim, nTS), object=Spatial))
#     return(Spatial)
#
#   if (EmptyObject(Spatial)) {
#     # non-spatial model
#     Spatial@RelativeSize <- AddDimNames(array(1, dim=c(1,1)), c('sim', 'area'))
#     Spatial@ProbStaying <- AddDimNames(array(1, dim=c(1,1,1,1)), c('sim', 'area', 'age', 'ts'))
#     Spatial@FracArea <- AddDimNames(array(1, dim=c(1,1,1,1)), c('sim', 'area', 'age', 'ts'))
#     Spatial@UnfishedDist  <- AddDimNames(array(1, dim=c(1,1,1,1)), c('sim', 'area', 'age', 'ts'))
#     Spatial@Movement  <- AddDimNames(array(1, dim=c(1,1,1,1)), c('sim', 'area', 'age', 'ts'))
#     return(Spatial)
#   }
#
#   if (!silent)
#     cli::cli_alert_info('Populating `Spatial`')
#
#   Spatial@RelativeSize <- StructurePars(list(Spatial@RelativeSize), nsim)[[1]]
#   nareas <- max(2,ncol(Spatial@RelativeSize))
#
#   Spatial@ProbStaying <- StructurePars(list(Spatial@ProbStaying), nsim)[[1]]
#   dd <- dim(Spatial@ProbStaying)
#   if (length(dd)==2) {
#     # add age and time-step dimension
#     Spatial@ProbStaying <- replicate(1, replicate(1, Spatial@ProbStaying))
#   } else if (length(dd)==3) {
#     # add time-step dimension
#     Spatial@ProbStaying <- replicate(1, Spatial@ProbStaying)
#   }
#   Spatial@ProbStaying  <- AddDimNames(Spatial@ProbStaying, c('sim', 'area', 'age', 'ts'))
#
#
#   Spatial@FracArea <- StructurePars(list(Spatial@FracArea), nsim)[[1]]
#   dd <- dim(Spatial@FracArea)
#   if (length(dd)==2) {
#     # add age and time-step dimension
#     Spatial@FracArea <- replicate(1, replicate(1, Spatial@FracArea))
#   } else if (length(dd)==3) {
#     # add time-step dimension
#     Spatial@FracArea <- replicate(1, Spatial@FracArea)
#   }
#   Spatial@FracArea  <- AddDimNames(Spatial@FracArea, c('sim', 'area', 'age', 'ts'))
#
#   if (nareas==2) {
#     Spatial <- Populate2AreaModel(Spatial)
#   } else{
#     # multi-area
#     Spatial <- PopulateMultiAreaModel(Spatial)
#   }
#
#   SetDigest(list(Ages, nsim, nTS), Spatial)
# }














# PopulateMeanAtAge <- function(object, nsim, nTS, Ages=NULL, Length=NULL) {
#
#   if (ParsEmpty(object@Pars) & is.null(object@Model)) {
#     return(object)
#   }
#
#   if (!is.null(object@Model)) {
#     model_class <- class(get(object@Model))
#     if (grepl('-at-Length', model_class))
#       return(object)
#   }
#
#
#   if (ParsNotEmpty(object@Pars)) {
#     object@Pars <- StructurePars(Pars=object@Pars, nsim, nTS)
#     if (is.null(object@Model))
#       object@Model <- FindModel(object)
#     if (is.null(object@MeanAtAge))
#       object@MeanAtAge <- GenerateMeanAtAge(Model=object@Model,
#                                             Pars=object@Pars,
#                                             Ages@Classes+object@Timing,
#                                             Length@MeanAtAge)
#   }
#   object
# }






MeanAtLength2MeanAtAge <- function(object, Length, Ages, nsim, TimeSteps, seed, silent,
                                   max1=TRUE) {
  if (!is.null(object@MeanAtAge))
    return(object)

  CheckRequiredObject(Length, 'length')
  CheckRequiredObject(Ages, 'ages')

  if (is.null(Length@ASK)) {
    Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
  }

  object@MeanAtAge <- AtSize2AtAge(object, Length) |>
    AddDimNames(TimeSteps=TimeSteps)
  
  if ('Units' %in% slotNames(object))
    attributes(object@MeanAtAge)$Units <- object@Units
  attributes(object@MeanAtAge)$UnitsAge <- Ages@Units

  if (max1) {
    maxValue <- apply(object@MeanAtAge, c(1,3), max)
    ind <- maxValue<1
    # TODO speed up loop
    for (i in 1:nrow(ind)) {
      for (j in 1:ncol(ind)) {
        if (!ind[i,j])
          next()
        object@MeanAtAge[i,,j] <- object@MeanAtAge[i,,j]/max(object@MeanAtAge[i,,j], na.rm = TRUE)
      }
    }
  }
  
  object@Classes <- Length@Classes
  object
}

MeanAtAge2MeanAtLength <- function(object, Length, Ages, nsim, TimeSteps, seed, silent) {
  if (!is.null(object@MeanAtLength))
    return(object)

  CheckRequiredObject(Ages, 'ages')

  if (!methods::is(Length, 'length')) {
    cli::cli_alert_warning('Must supply populated `Length` object to calculate `MeanAtLength`')
    return(object)
  }

  if (is.null(Length@ASK)) {
    Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
  }

  object@MeanAtLength <- AtAge2AtSize(object, Length)

  if ('Units' %in% slotNames(object))
    attributes(object@MeanAtLength)$Units <- object@Units
  attributes(object@MeanAtLength)$TimeSteps <- TimeSteps

  attributes(object@MeanAtLength)$ClassesLength <- Length@Classes
  attributes(object@MeanAtLength)$UnitsLength <- Length@Units


  object@Classes <- Length@Classes
  object


}

#' @export
AtSize2AtAge <- function(object, Length) {

  MeanAtLength <- object@MeanAtLength
  ASK <- Length@ASK
  dim_MeanAtLength <- dim(MeanAtLength)
  dim_ASK <- dim(ASK)
  nage <- dim_ASK[2]
  nClasses <- dim_ASK[3]

  nsim_MeanAtLength <- dim_MeanAtLength[1]
  nTS_MeanAtLength <- dim_MeanAtLength[3]

  nsim_ASK <- dim_ASK[1]
  nTS_ASK <- dim_ASK[4]

  nsim <- max(nsim_MeanAtLength, nsim_ASK) # maximum number of simulations
  nTS <- max(nTS_MeanAtLength, nTS_ASK) # maximum number of time-steps

  AtAge <- array(0, dim=c(nsim, nage, nTS))
  for (s in 1:nsim) {
    for (t in 1:nTS) {
      MeanAtLength_ts <- MeanAtLength[GetIndex(s, nsim_MeanAtLength), ,GetIndex(t, nTS_MeanAtLength)]
      ASK_ts <- ASK[GetIndex(s, nsim_ASK),,,GetIndex(t, nTS_ASK)]
      ASK_ts_stand <- ASK_ts/ matrix(apply(ASK_ts, 1, sum), nage, nClasses, byrow=FALSE)
      AtAge[s,,t] <- MeanAtLength_ts %*%t(ASK_ts)
    }
  }
  AtAge
}

#' @export
AtAge2AtSize <- function(object, Length) {

  MeanAtAge <- object@MeanAtAge
  ASK <- Length@ASK

  if (dim(MeanAtAge)[2]<30) { # arbitrary number!
    # Generate higher resolution length-at-age
    # linear intepolate Mean length-at-age and CV length-at-age
    # for 11 time-steps in-between (e.g., months)
    dims  <- rbind(dim(Length@MeanAtAge),
                   dim(Length@CVatAge),
                   dim(MeanAtAge)
    )
    dd <- apply(dims, 2, max)

    objectMeanAtAge <- array(0, dim=c(dd[1], (dd[2]*12)-11, dd[3]))
    LengthMeanAtAge <- array(0, dim=c(dd[1], (dd[2]*12)-11, dd[3]))
    LengthCVatAge <- array(0, dim=c(dd[1], (dd[2]*12)-11, dd[3]))

    ind <- seq(from=1, by=12, to=dim(LengthMeanAtAge)[2])

    objectMeanAtAge[,ind,] <- MeanAtAge[]
    LengthMeanAtAge[,ind,] <- Length@MeanAtAge[]
    LengthCVatAge[,ind,] <- Length@CVatAge[]

    for (i in 1:(length(ind)-1)) {
      ind2 <- c(ind[i], ind[i]+12)
      ind3 <- (ind2[1]+1): (ind2[2]-1)

      # silly loop for now!
      for (s in 1:max(dims[,1])) {
        for (ts in 1:max(dims[,3])) {

          # Linear interpolate at-Age schedule
          temp <- approx(ind2, MeanAtAge[GetIndex(s, dims[1,1]),
                                                i:(i+1),
                                                GetIndex(s, dims[1,3])],
                         xout=ind3)
          objectMeanAtAge[s,ind3,ts] <- temp$y

          # Linear interpolate growth curve
          temp <- approx(ind2, Length@MeanAtAge[GetIndex(s, dims[1,1]),
                                                i:(i+1),
                                                GetIndex(s, dims[1,3])],
                         xout=ind3)

          LengthMeanAtAge[s,ind3,ts] <- temp$y



          if (dims[2,2]==1) {
            LengthCVatAge[s,,ts] <- Length@CVatAge[GetIndex(s, dims[2,1]),,
                                             GetIndex(s, dims[2,3])]
          } else {
            temp <- approx(ind2, Length@CVatAge[GetIndex(s, dims[2,1]),
                                                i:(i+1),
                                                GetIndex(s, dims[2,3])],
                           xout=ind3)
            LengthCVatAge[s,ind3,ts] <- temp$y
          }
        }
      }
    }
    # generate a new age-size key with finer temportal resolution
    ASK <- CalcASK(MeanAtAge=LengthMeanAtAge,
                   CVatAge=LengthCVatAge,
                   Classes=Length@Classes,
                   TruncSD=Length@TruncSD,
                   Dist=Length@Dist,
                   Ages=NULL,
                   silent=TRUE)

    MeanAtAge <- objectMeanAtAge

  }

  dim_MeanAtAge <- dim(MeanAtAge)
  dim_ASK <- dim(ASK)
  nage <- dim_ASK[2]
  nClasses <- dim_ASK[3]

  if (dim_MeanAtAge[2] != dim_ASK[2])
    cli::cli_abort('`dim(MeanAtAge)[2] != dim(ASK)[2]`')

  nsim_MeanAtAge <- dim_MeanAtAge[1]
  nTS_MeanAtAge <- dim_MeanAtAge[3]

  nsim_ASK <- dim_ASK[1]
  nTS_ASK <- dim_ASK[4]

  nsim <- max(nsim_MeanAtAge, nsim_ASK) # maximum number of simulations
  nTS <- max(nTS_MeanAtAge, nTS_ASK) # maximum number of time-steps

  AtSize <- array(0, dim=c(nsim, nClasses, nTS))

  for (s in 1:nsim) {
    for (t in 1:nTS) {
      MeanAtAge_ts <- MeanAtAge[GetIndex(s, nsim_MeanAtAge), ,GetIndex(t, nTS_MeanAtAge)]
      ASK_ts <- ASK[GetIndex(s, nsim_ASK),,,GetIndex(t, nTS_ASK)]
      sums <- matrix(apply(ASK_ts, 2, sum), nage, nClasses, byrow=TRUE)
      ASK_ts_stand <- ASK_ts/sums
      ASK_ts_stand[!is.finite(ASK_ts_stand)] <- 0
      AtSize[s,,t] <- MeanAtAge_ts %*% ASK_ts_stand
    }
  }
  AtSize
}

