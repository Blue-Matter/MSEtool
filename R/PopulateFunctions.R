

RequireALK <- function(stock) {
  stock_slots <- c('Length', 'Weight', 'NaturalMortality', 'Maturity', 'Fecundity')
  
  fleet_slots <- c('Selectivity', 'Retention')
  
  reqALK <- rep(FALSE, length(stock_slots))
  for (i in seq_along(stock_slots)) {
    model <- slot(slot(stock, stock_slots[i]), 'Model')
    if (is.null(model))
      next()
    if (inherits(model, 'function'))
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
  
  # tt <- methods('Check')
  # if (!any(grepl(paste0(class, '-method'), tt))) {
  #   cli::cli_alert_warning("`Check` method doesn't exist for class {.code {class}}")
  #   chk <- NULL
  # } else {
  #   # chk <- Check(object)
  #   # if(!chk@complete)
  #   #   cli::cli_abort("{.code {argName}} is not complete. See {.code Check({.run {class}})}")
  # }
  # invisible(chk)
  NULL
}

PopulateMeanAtAge <- function(object, Ages=NULL, TimeSteps=NULL, Length=NULL) {
  
  if (!is.null(object@MeanAtAge)) {
    object@MeanAtAge <- Structure(object@MeanAtAge)
    if (is.null(dimnames(object@MeanAtAge))) {
      dd <- dim(object@MeanAtAge)
      dimnames(object@MeanAtAge) <- list(Sim=1:dd[1],
                                         Age=Ages@Classes[1:dd[2]],
                                         TimeStep=TimeSteps[1:dd[3]])
    }
    
    return(object)
  }
  
  
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
      
    }
    dd <- dim(object@MeanAtAge)
    dimnames(object@MeanAtAge) <- list(Sim=1:dd[1],
                                       Age=Ages@Classes[1:dd[2]],
                                       TimeStep=TimeSteps[1:dd[3]])
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
      # chk <- Check(Length)
      # if(!chk@populated) {
      #   CheckRequiredObject(Ages, 'ages', 'Ages')
      #   Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
      # }
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
    object@MeanAtLength <- AddDimNames(object@MeanAtLength, c('Sim', 'Class', 'TimeStep'), TimeSteps)
  }
  object
}

PopulateMeanAtWeight <- function(object, 
                                 Weight=NULL, 
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
    
    if ('Weight' %in% args) {
      CheckRequiredObject(Weight, 'weight', 'Weight')
      # chk <- Check(Length)
      # if(!chk@populated) {
        # CheckRequiredObject(Ages, 'ages', 'Ages')
        # Weight <- Populate(Weight, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
      # }
    }
    object@MeanAtLength <- GenerateMeanatWeight(Model=object@Model,
                                                Pars=object@Pars,
                                                Weight=Weight@Classes)
    
    if ('Units' %in% slotNames(object))
      attributes(object@MeanAtLength)$Units <- object@Units
    attributes(object@MeanAtLength)$TimeSteps <- TimeSteps
    if (methods::is(Length, 'length')) {
      attributes(object@MeanAtLength)$LengthClasses <- Length@Classes
      attributes(object@MeanAtLength)$UnitsLength <- Length@Units
    }
    object@MeanAtLength <- AddDimNames(object@MeanAtLength, c('Sim', 'Class', 'TimeStep'), TimeSteps)
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
  
  
  if (all(d1 >= d2)) {
    dimnames(SDatAge) <- dimnames(MeanAtAge)
  } else { 
    dimnames(SDatAge) <- dimnames(CVatAge)
  }
  
  SDatAge
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
    logMeanAtAge <- ArraySubtract(MeanAtAge, -0.5*CVatAge^2) |> log()
    MaxBin <- max(exp(logMeanAtAge + TruncSD * CVatAge[ind2])) |> ceiling() 
  } else {
    cli::cli_abort('{.code {dist}} is not valid for `Dist` slot. Options are `normal` or `lognormal`')
  }
  MaxBin
}


PopulateClasses <- function(object) {
  if (!EmptyObject(object@Classes))
    return(object)
  
  MaxBin <- CalcMaxBin(MeanAtAge=object@MeanAtAge, 
                       CVatAge=object@CVatAge, 
                       TruncSD=object@TruncSD, 
                       dist=object@Dist)
  bins <- seq(0, to=MaxBin, length.out=40) |> round(2)
  by <- bins[2] - bins[1]
  
  object@Classes <- seq(bins[1]+0.5*by, by=by, length.out=length(bins)-1)
  object
}

PopulateASK <- function(object, Ages=NULL, TimeSteps=NULL, silent=FALSE, type='Length') {
  
  CheckRequiredObject(Ages, 'ages', 'Ages')
  if ('Timing' %in% slotNames(object)) {
    Ages@Classes <- Ages@Classes+object@Timing
  }
  
  MeanAtAge <- object@MeanAtAge
  CVatAge <- object@CVatAge
  Classes <- object@Classes
  Dist <- object@Dist
  TruncSD <- object@TruncSD
  
  object@ASK <- CalcAgeSizeKey(MeanAtAge, CVatAge, Classes, TruncSD, Dist, Ages@Classes,
                        silent=silent, type=type)
  

  timesteps <- c(dimnames(MeanAtAge)$TimeStep,
                 dimnames(CVatAge)$TimeStep) |> unique() |> sort()
  
  dd <- dim(object@ASK)
  if (is.null(  dimnames(object@ASK))) {
    dimnames(object@ASK) <- list(Sim=1:dd[1],
                                 Age=Ages@Classes,
                                 Class=Classes,
                                 TimeStep=timesteps)
  }
 
  object
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
                              c('Sim', 'Area', 'Age', 'TimeStep'),
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

CheckSelectivityMaximum <- function(MeanAtAge) {
  MaxValues <- apply(MeanAtAge, c(1,3), max) |> round(3)
  
  ind <- MaxValues<1 & MaxValues!=0
  if (all(!ind))
    return(MeanAtAge)
  
  cli::cli_alert_warning("WARNING: Selectivity-at-Age does not have a maximum value of 1. F-at-Age won't correspond with Apical F")
  cli::cli_alert_warning('Standardizing to a max value of 1 but you probably want to fix this in the OM')
  
  sims <- which(apply(ind,1, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
  TSs <- which(apply(ind, 2, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
  
  cli::cli_alert('Simulations {.val {sims}}; TimeSteps {.val {TSs}}')
  
  for (i in 1:nrow(ind)) {
    for (j in 1:ncol(ind)) {
      if (ind[i,j]==FALSE)
        next()
      MeanAtAge[i, ,j] <- MeanAtAge[i, ,j]/max(MeanAtAge[i, ,j])
    }
  }
  
  MeanAtAge
}




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
  if (nsim>1) {
    EffortTS <- t(sapply(1:nsim, function(x) 
      approx(x = Effort$TimeStep,
             y = EffortPoints[x, ], 
             method = "linear", 
             n = nTimeSteps)$y)
    )
  } else {
    EffortTS <- approx(x = Effort$TimeStep,
                       y = EffortPoints,
                       method = "linear", 
                       n = nTimeSteps)$y
  }
  
  Esd <- Effort$CV[1]
  if (!is.null(Esd)) {
    Emu <- -0.5 * Esd^2
    EffortError <- array(exp(rnorm(nTimeSteps * nsim, rep(Emu, nTimeSteps), 
                                   rep(Esd, nTimeSteps))), 
                         c(nsim, nTimeSteps))  
    EffortTS <- EffortTS * EffortError
  }
  EffortTS <- EffortTS |> AddDimNames(names=c('Sim', 'TimeStep'), 
                                      TimeSteps = TimeSteps)
  
  EffortTS/matrix(EffortTS[,nTimeSteps], nsim, nTimeSteps, byrow=FALSE)
}




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
  
  object
}

MeanAtWeight2MeanAtAge <- function(object, Weight, Ages, nsim, TimeSteps, seed, silent,
                                   max1=TRUE) {
  if (!is.null(object@MeanAtAge))
    return(object)
  
  CheckRequiredObject(Weight, 'weight')
  CheckRequiredObject(Ages, 'ages')
  
  if (is.null(Weight@ASK)) 
    return(object)
  
  object@MeanAtAge <- AtSize2AtAge(object, Weight) |>
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
  
  if (EmptyObject(Length))
    return(object)
  
  if (is.null(Length@ASK)) {
    Length <- PopulateLength(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
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

MeanAtAge2MeanAtWeight <- function(object, Weight, Ages, nsim, TimeSteps, seed, silent) {
  if (!is.null(object@MeanAtLength))
    return(object)
  
  CheckRequiredObject(Ages, 'ages')
  
  if (!methods::is(Weight, 'weight')) {
    cli::cli_alert_warning('Must supply populated `Weight` object to calculate `MeanAtWeight`')
    return(object)
  }
  
  if (EmptyObject(Weight))
    return(object)
  
  if (is.null(Weight@ASK)) 
    return(object)
  
  object@MeanAtLength <- AtAge2AtSize(object, Weight)
  
  if ('Units' %in% slotNames(object))
    attributes(object@MeanAtLength)$Units <- object@Units
  attributes(object@MeanAtLength)$TimeSteps <- TimeSteps
  
  # attributes(object@MeanAtLength)$ClassesLength <- Length@Classes
  # attributes(object@MeanAtLength)$UnitsLength <- Length@Units
  
  object@Classes <- Weight@Classes
  object
  
}


AtSize2AtAge <- function(object, Length) {
  # OBJ <<- object
  # LEN <<- Length
  
  MeanAtLength <- object@MeanAtLength
  ASK <- Length@ASK
  dim_MeanAtLength <- dim(MeanAtLength)
  dim_ASK <- dim(ASK)
 
  
  DNames <- names(dimnames(Length@MeanAtAge))
  bySim <- TRUE
  if ("Sim" %in% DNames) {
    nage <- dim_ASK[2]
    nClasses <- dim_ASK[3]
    
    nsim_MeanAtLength <- dim_MeanAtLength[1]
    nTS_MeanAtLength <- dim_MeanAtLength[3]
    
    nsim_ASK <- dim_ASK[1]
    nTS_ASK <- dim_ASK[4]
    
    nsim <- max(nsim_MeanAtLength, nsim_ASK) # maximum number of simulations
    nTS <- max(nTS_MeanAtLength, nTS_ASK) # maximum number of time-steps
  } else {
    bySim <- FALSE
    
    nage <- dim_ASK[1]
    nClasses <- dim_ASK[2]
    
    nsim_ASK <- 1
    nsim_MeanAtLength <- 1
    nsim <- 1
    
    nTS_ASK <- 1
    nTS_MeanAtLength <- 1
    nTS <- dim_MeanAtLength[3]
  }
  
  AtAge <- array(0, dim=c(nsim, nage, nTS))
  for (s in 1:nsim) {
    for (t in 1:nTS) {
      MeanAtLength_ts <- MeanAtLength[GetIndex(s, nsim_MeanAtLength), ,GetIndex(t, nTS_MeanAtLength)]
      if (bySim) {
        ASK_ts <- ASK[GetIndex(s, nsim_ASK),,,GetIndex(t, nTS_ASK)]
        AtAge[s,,t] <- MeanAtLength_ts %*%t(ASK_ts)
      } else {
        AtAge[s,,t] <- (MeanAtLength_ts %*%t(ASK))[1,]
      }
    }
  }
  AtAge
}

AtAge2AtSize <- function(object, Length, max1=TRUE) {
  
  MeanAtAge <- object@MeanAtAge
  ASK <- Length@ASK
  
  if (dim(MeanAtAge)[2]<30) { # arbitrary number!
    # Generate higher resolution length-at-age
    # linear interpolate Mean length-at-age and CV length-at-age
    # for 11 time-steps in-between (e.g., months)
    dims  <- rbind(dim(Length@MeanAtAge),
                   dim(Length@CVatAge),
                   dim(MeanAtAge)
    )
    dd <- apply(dims, 2, max)
    
    nSubAges <- 12
    SubAgeDim <- (dd[2]*nSubAges)-(nSubAges-1)
    
    objectMeanAtAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    LengthMeanAtAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    LengthCVatAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    
    dname1 <- dimnames(Length@MeanAtAge)
    ages <- as.numeric(dname1[["Age"]])
    dname1[["Age"]] <- seq(from=ages[1], to=ages[length(ages)], length.out=SubAgeDim)
    dimnames(LengthMeanAtAge) <- dname1
    
    dname1 <- dimnames(Length@CVatAge)
    dname1[["Age"]] <- seq(from=ages[1], to=ages[length(ages)], length.out=SubAgeDim)
    dimnames(LengthCVatAge) <- dname1
    
    
    ind <- seq(from=1, by=nSubAges, to=dim(LengthMeanAtAge)[2])
    
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
    
    # generate a new age-size key with finer temporal resolution
    ASK <- CalcAgeSizeKey(MeanAtAge=LengthMeanAtAge,
                          CVatAge=LengthCVatAge,
                          Classes=Length@Classes,
                          TruncSD=Length@TruncSD,
                          Dist=Length@Dist,
                          Ages=NULL,
                          silent=TRUE)
    
    MeanAtAge <- objectMeanAtAge
    dimnames(MeanAtAge) <- dname1
    
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
      ASK_tsvec <- apply(ASK_ts, 2, sum)
      ind <- max(which(ASK_tsvec>0))
      
      
      sums <- matrix(apply(ASK_ts, 2, sum), nage, nClasses, byrow=TRUE)
      ASK_ts_stand <- ASK_ts/sums
      ASK_ts_stand[!is.finite(ASK_ts_stand)] <- 0
      AtSize[s,,t] <- MeanAtAge_ts %*% ASK_ts_stand
      if (max1)
        AtSize[s,ind:ncol(ASK_ts),t] <- 1
    }
  }
  
  dimnames(AtSize) <- list(Sim=1:nsim,
                           Class=Length@Classes,
                           TimeStep=dimnames(MeanAtAge)[["TimeStep"]][1:nTS])
  AtSize 
}

# TODO - CatchFrac could be array nsim, nstock, nfleet - rather than list 
ProcessCatchFrac <- function(object) {
  if (length(object@CatchFrac)<1)
    return(object)
  
  nStock <- nStock(object)
  nFleet <- nFleet(object)
  nSim <- nSim(object)
  
  names(object@CatchFrac) <- StockNames(object)
  
  if (length(object@CatchFrac)!= nStock)
    cli::cli_abort('`OM@CatchFrac` must be a list length 0 or length `nStock(OM)` ')
  
  
  for (st in 1:nStock) {
    
    CatchFracFleet <- object@CatchFrac[[st]]
    if (!all(dim(CatchFracFleet) == c(nSim, nFleet)))
      cli::cli_abort('`OM@CatchFrac` must be a list length `nStock(OM)` with a `nSim` by `nFleet` matrix  for each stock')
    
    if (any(CatchFracFleet<0) || any(!is.finite(CatchFracFleet)))
      cli::cli_abort('Values in `OM@CatchFrac` must be positive')
    
    rsum <- rowSums(CatchFracFleet)
    if (any(rsum!=1))
      cli::cli_abort('Values in `OM@CatchFrac` sum to 1 across rows')
    
    
    dimnames(CatchFracFleet) <- list("Sim"=1:nSim,
                                     "Fleet"=FleetNames(object))
    
    object@CatchFrac[[st]] <- CatchFracFleet
  }
  
  object
  
}
