

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

PopulateMeanAtAge <- function(object, Ages=NULL, Years=NULL, Length=NULL) {
  
  if (!is.null(object@MeanAtAge)) {
    object@MeanAtAge <- Structure(object@MeanAtAge)
    if (is.null(dimnames(object@MeanAtAge))) {
      dd <- dim(object@MeanAtAge)
      dimnames(object@MeanAtAge) <- list(Sim=1:dd[1],
                                         Age=Ages@Classes[1:dd[2]],
                                         Year=Years[1:dd[3]])
    }
    
    return(object)
  }
  
  
  if (ParsNotEmpty(object@Pars)) {
    if (is.null(object@Model))
      object@Model <- FindModel(object)
    
    args <- names(formals(object@Model))
    
    if ('Length' %in% args) {
      CheckRequiredObject(Length, 'length', 'Length')
      # chk <- Check(Length)
      # if(!chk@populated) {
      #   CheckRequiredObject(Ages, 'ages', 'Ages')
      #   Length <- Populate(Length, Ages, nSim, Years, seed, ASK=TRUE, silent)
      # }
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
    yearnames <- purrr::map(object@Pars, \(par) {
      dimnames(par)$Year
    }) |> unlist() |> unique()
    
    if (is.null(yearnames)) {
      yearnames <- Years[1:dd[3]]
    }
    
    if (length(yearnames) != dd[3]) 
      cli::cli_abort('Names for `Year` dimension do not match length of `Year` dimension', .internal=TRUE)
    
    dimnames(object@MeanAtAge) <- list(Sim=1:dd[1],
                                       Age=Ages@Classes[1:dd[2]],
                                       Year=yearnames)
  }
  object
}

PopulateMeanAtLength <- function(object, 
                                 Length=NULL, 
                                 Years=NULL, 
                                 Ages=NULL, 
                                 nSim=NULL,
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
      #   Length <- Populate(Length, Ages, nSim, Years, seed, ASK=TRUE, silent)
      # }
    }
    object@MeanAtLength <- GenerateMeanatLength(Model=object@Model,
                                                Pars=object@Pars,
                                                Length=Length@Classes)
    
    object@Classes <- Length@Classes
    
    dd <- dim(object@MeanAtLength)
    dimnames(object@MeanAtLength) <- list(Sim=1:dd[1],
                                          Class=Length@Classes,
                                          Year=Years[1:dd[3]])
    
    
  #   if ('Units' %in% slotNames(object))
  #     attributes(object@MeanAtLength)$Units <- object@Units
  #   attributes(object@MeanAtLength)$Years <- Years
  #   if (methods::is(Length, 'length')) {
  #     attributes(object@MeanAtLength)$LengthClasses <- Length@Classes
  #     attributes(object@MeanAtLength)$UnitsLength <- Length@Units
  #   }
  #   object@MeanAtLength <- AddDimNames(object@MeanAtLength, c('Sim', 'Class', 'Year'), Years)
  }
  object
}

PopulateMeanAtWeight <- function(object, 
                                 Weight=NULL, 
                                 Years=NULL, 
                                 Ages=NULL, 
                                 nSim=NULL,
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
        # Weight <- Populate(Weight, Ages, nSim, Years, seed, ASK=TRUE, silent)
      # }
    }
    object@MeanAtWeight <- GenerateMeanatWeight(Model=object@Model,
                                                Pars=object@Pars,
                                                Weight=Weight@Classes)
    
    object@Classes <- Weight@Classes
    
    # if ('Units' %in% slotNames(object))
    #   attributes(object@MeanAtWeight)$Units <- object@Units
    # attributes(object@MeanAtWeight)$Years <- Years
    dd <- dim(object@MeanAtWeight)
    dimnames(object@MeanAtWeight) <- list(Sim=1:dd[1],
                                          Class=Weight@Classes,
                                          Year=Years[1:dd[3]])
      
    
    
  }
  object
}







StructureCV <- function(CVatAge, nSim) {
  
  
  if (is.null(dim(CVatAge))) {
    if (length(CVatAge)==1)
      return(Structure(CVatAge))
    if (length(CVatAge)==2) {
      return(Structure(StructurePars_(CVatAge, nSim)))
    }
    if (length(CVatAge)==nSim) {
      CVatAge <- array(CVatAge, dim=nSim)
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
    if (!is.null(OM@Stock[[st]]@SRR@SPFrom)) {
      if (is.numeric(OM@Stock[[st]]@SRR@SPFrom))
        OM@Stock[[st]]@SRR@SPFrom <- stocknames[OM@Stock[[st]]@SRR@SPFrom]
    }
  }
  OM
}

ShareParameters <- function(OM) {
  
  return(OM)
  
  # TODO
  
  
  if (length(OM@SexPars@Herm)) {
    stop('Herm not done yet!')
    # SexPars$Herm <- checkHerm(SexPars$Herm, maxage, nSim, nyears, proyears)
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
    SDatAge <- ArrayMultiply(MeanAtAge, CVatAge)
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
  
  if (is.null(object@MeanAtAge))
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

PopulateASK <- function(object, Ages=NULL, Years=NULL, silent=FALSE, type='Length') {
  
  CheckRequiredObject(Ages, 'ages', 'Ages')
  if ('Timing' %in% slotNames(object)) {
    Ages@Classes <- Ages@Classes+object@Timing
  }
  
  MeanAtAge <- object@MeanAtAge
  CVatAge <- object@CVatAge
  Classes <- object@Classes
  Dist <- object@Dist
  TruncSD <- object@TruncSD
  
  object@ASK <- CalcAgeSizeKey(MeanAtAge, 
                               CVatAge, 
                               Classes, 
                               TruncSD, 
                               Dist,
                               AgeClasses=Ages@Classes,
                               silent=silent, type=type)
  

  Years <- c(dimnames(MeanAtAge)$Year,
                 dimnames(CVatAge)$Year) |> unique() |> sort()
  
  dd <- dim(object@ASK)
  if (is.null(  dimnames(object@ASK))) {
    dimnames(object@ASK) <- list(Sim=1:dd[1],
                                 Age=Ages@Classes,
                                 Class=Classes,
                                 Year=Years)
  }
 
  object
}

CalculateRelativeSize <- function(Spatial, nSim) {
  nareas <- dim(Spatial@UnfishedDist)[2]
  
  if (!is.null(Spatial@RelativeSize) & !methods::is(Spatial@RelativeSize, 'character')) {
    Spatial@RelativeSize <- StructurePars(list(Spatial@RelativeSize),nSim)[[1]]
    dd <- dim(Spatial@RelativeSize)
    if (dd[2]>nareas)
      cli::cli_abort('`RelativeSize` is longer than `nAreas` ({.val {nareas}})')
    
    if (dd[2]==1 & nareas==2) {
      RelativeSize <- array(0, dim=c(dd[1], 2))
      RelativeSize[,1] <- Spatial@RelativeSize
      RelativeSize[,2] <- 1- RelativeSize[,1]
      Spatial@RelativeSize <- RelativeSize
    }
    
    if (nareas>2) {
      if (dd[2]<nareas)
        cli::cli_abort('`RelativeSize` must have `nAreas` ({.val {nareas}}) columns')
      
      rowsums <- apply(Spatial@RelativeSize, 1, sum) |> round(3)
      if (!all(rowsums==1))
        cli::cli_abort('`RelativeSize` must sum to 1 across columns')
    }
    
  } else if (methods::is(Spatial@RelativeSize, 'character')) {
    if (Spatial@RelativeSize=="EqualDensity") {
      Spatial@RelativeSize <- apply(Spatial@UnfishedDist, c('Sim', 'Area'), mean)   
    } else {
      cli::cli_abort('If `Spatial@RelativeSize` is character, it can only be "EqualDensity"')
    }
  } else {
    cli::cli_alert_warning('`RelativeSize` is not specified. Assuming all areas are equal size')
    Spatial@RelativeSize <- matrix(1/nareas, 1, nareas)
  }
  Spatial@RelativeSize <- AddDimNames(Spatial@RelativeSize, c('Sim', 'Area'))
  Spatial
}

CalcUnfishedDist <- function(Spatial,
                             Years=NULL,
                             plot=FALSE,
                             nits=100) {
  dims <- dim(Spatial@Movement)
  if (is.null(dims))
    return(Spatial)
  UnfishedDist <- AddDimNames(array(NA, dim=c(dims[1],
                                              dims[2],
                                              dims[4],
                                              dims[5])),
                              c('Sim', 'Area', 'Age', 'Year'),
                              Years=Years)
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
  
  
  ind <- MaxValues<0.99 & MaxValues!=0
  if (all(!ind))
    return(MeanAtAge)
  
  cli::cli_alert_warning("WARNING: Selectivity-at-Age does not have a maximum value of 1. F-at-Age won't correspond with Apical F")
  cli::cli_alert_warning('Standardizing to a max value of 1 but you probably want to fix this in the OM')
  
  sims <- which(apply(ind,1, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
  TSs <- which(apply(ind, 2, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
  
  cli::cli_alert('Simulations {.val {sims}}; Years {.val {TSs}}')
  
  for (i in 1:nrow(ind)) {
    for (j in 1:ncol(ind)) {
      if (ind[i,j]==FALSE)
        next()
      MeanAtAge[i, ,j] <- MeanAtAge[i, ,j]/max(MeanAtAge[i, ,j])
    }
  }
  
  MeanAtAge
}




GenerateHistoricalEffort <- function(Effort, nSim=NULL, Years=NULL) {
  if (!methods::is(Effort, 'data.frame'))
    cli::cli_abort('`Effort` must be a data.frame')
  
  if (!all(names(Effort) %in% c("Year", "Lower", "Upper", "CV" )))
    cli::cli_abort('`Effort` must be a data.frame with columns: "Year", "Lower", "Upper", "CV" ')
  
  if (is.null(nSim)) {
    cli::cli_warn('`nSim` not specified, assuming `nSim=100`')
    nSim <- 100
  }
  
  
  if (is.null(Years)) {
    cli::cli_warn('`Years` not specified, assuming last time step is: {.val {max(Effort$Year)}}')
    Years <- 1:max(Effort$Year)
  }
  
  if (all(Effort$Year< 1000)) {
    chk <- max(Effort$Year) %in% seq_along(Years)  
    if (!chk)
      cli::cli_abort('`max(Effort$Year)` ({.val {max(Effort$Year)}})')
  } else {
    chk <- max(Effort$Year) %in% Years
    if (!chk)
      cli::cli_abort('`max(Effort$Year)` ({.val {max(Effort$Year)}})')
  }
  
  nYears <- length(Years)
  
  EffortPoints <- mapply(runif, n = nSim, min = Effort$Lower, max = Effort$Upper)  # sample Effort
  if (nSim>1) {
    EffortTS <- t(sapply(1:nSim, function(x) 
      approx(x = Effort$Year,
             y = EffortPoints[x, ], 
             method = "linear", 
             n = nYears)$y)
    )
  } else {
    EffortTS <- approx(x = Effort$Year,
                       y = EffortPoints,
                       method = "linear", 
                       n = nYears)$y
  }
  
  Esd <- Effort$CV[1]
  if (!is.null(Esd)) {
    Emu <- -0.5 * Esd^2
    EffortError <- array(exp(rnorm(nYears * nSim, rep(Emu, nYears), 
                                   rep(Esd, nYears))), 
                         c(nSim, nYears))  
    EffortTS <- EffortTS * EffortError
  }
  EffortTS <- EffortTS |> AddDimNames(names=c('Sim', 'Year'), 
                                      Years = Years)
  
  EffortTS/matrix(EffortTS[,nYears], nSim, nYears, byrow=FALSE)
}




MeanAtLength2MeanAtAge <- function(object, Length, Ages, nSim, Years, seed, silent,
                                   max1=TRUE) {
  if (!is.null(object@MeanAtAge))
    return(object)
  
  CheckRequiredObject(Length, 'length')
  CheckRequiredObject(Ages, 'ages')
  
  if (is.null(Length@ASK)) 
    return(object)
  
  if (all(object@MeanAtLength>0.99)) {
    object@MeanAtAge <- array(1, dim=c(1, length(Ages@Classes), 1),
                              dimnames = list(
                                Sim=1,
                                Age=Ages@Classes,
                                Year=Years[1]
                              ))
    return(object)
  }
  
  if (all(object@MeanAtLength<0.01)) {
    object@MeanAtAge <- array(tiny, dim=c(1, length(Ages@Classes), 1),
                              dimnames = list(
                                Sim=1,
                                Age=Ages@Classes,
                                Year=Years[1]
                              ))
    return(object)
  }
  
  object@MeanAtAge <- AtSize2AtAge(object, Length) 
  
  
  # if ('Units' %in% slotNames(object))
  #   attributes(object@MeanAtAge)$Units <- object@Units
  # attributes(object@MeanAtAge)$UnitsAge <- Ages@Units
  
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
  object@MeanAtAge[!is.finite(object@MeanAtAge)] <- tiny
  
  object
}

MeanAtWeight2MeanAtAge <- function(object, Weight, Ages, nSim, Years, seed, silent,
                                   max1=TRUE) {
  if (!is.null(object@MeanAtAge))
    return(object)
  
  CheckRequiredObject(Weight, 'weight')
  CheckRequiredObject(Ages, 'ages')
  
  if (is.null(Weight@ASK)) 
    return(object)
  
  object@MeanAtAge <- AtSize2AtAge(object, Weight) |>
    AddDimNames(Years=Years, Ages=Ages@Classes)
  
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

MeanAtAge2MeanAtLength <- function(object, Length, Ages, nSim, Years, seed=NULL, silent=TRUE, replace=FALSE) {
  
  if (!is.null(object@MeanAtLength) & !replace)
    return(object)
  
  CheckRequiredObject(Ages, 'ages')
  
  if (!methods::is(Length, 'length')) {
    cli::cli_alert_warning('Must supply populated `Length` object to calculate `MeanAtLength`')
    return(object)
  }
  
  if (EmptyObject(Length))
    return(object)
  
  if (is.null(Length@ASK)) {
    Length <- PopulateLength(Length, Ages, Years, nSim, seed, ASK=TRUE, silent)
  }
  
  object@MeanAtLength <- AtAge2AtSize(object, Length)
  
  if ('Units' %in% slotNames(object))
    attributes(object@MeanAtLength)$Units <- object@Units
  attributes(object@MeanAtLength)$Years <- Years
  
  attributes(object@MeanAtLength)$ClassesLength <- Length@Classes
  attributes(object@MeanAtLength)$UnitsLength <- Length@Units
  
  
  object@Classes <- Length@Classes
  object
  
}

MeanAtAge2MeanAtWeight <- function(object, Weight, Ages, nSim, Years, seed, silent) {
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
  attributes(object@MeanAtLength)$Years <- Years
  
  # attributes(object@MeanAtLength)$ClassesLength <- Length@Classes
  # attributes(object@MeanAtLength)$UnitsLength <- Length@Units
  
  object@Classes <- Weight@Classes
  object
  
}

