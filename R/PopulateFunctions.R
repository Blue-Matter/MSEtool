

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





StructureCV <- function(CVatAge, nSim) {
  if (is.null(dim(CVatAge))) {
    if (length(CVatAge)==1)
      return(Structure(CVatAge))
    if (length(CVatAge)==2) {
      return(Structure(StructurePars_(CVatAge, nSim)))
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
  
  
  if (length(OM@Herm)) {
    stop('Herm not done yet!')
    # SexPars$Herm <- checkHerm(SexPars$Herm, maxage, nSim, nyears, proyears)
  }
  
  # TODO - remove SPFrom if it remains in SRR
  if (!length(OM@SPFrom))
    return(OM)
  
  if (isFALSE(OM@SharePar))
    return(OM)
  
  # sexmatches <- sapply(1:nrow(OM@SPFrom), function(x) 
  #   paste(OM@SPFrom[x, ], collapse = "_"))
  # 
  # parcopy <- match(sexmatches, sexmatches)
  
  
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
  
  ASK <- CalcAgeSizeKey(MeanAtAge, 
                        CVatAge, 
                        Classes, 
                        TruncSD, 
                        Dist,
                        silent=silent, 
                        type=type)
  

  Years <- c(dimnames(MeanAtAge)$Year,
                 dimnames(CVatAge)$Year) |> unique() |> sort()
  
  dd <- dim(ASK)
  if (is.null(dimnames(ASK))) {
    dimnames(ASK) <- list(Sim=1:dd[1],
                                 Age=Ages@Classes,
                                 Class=Classes,
                                 Year=Years)
  }
  
  if (type=='Length') {
    object@ALK <- ASK
  } else {
    object@AWK <- ASK
  } 
  
  object
}





CheckSelectivityMaximum <- function(MeanAtAge, alert=TRUE) {
  dnames <- dimnames(MeanAtAge)
  byArea <- ifelse(is.null(dnames[['Area']]), FALSE, TRUE)
  
  if (byArea) {
    MaxValues <- apply(MeanAtAge, c('Sim', 'Year', 'Area'), max) |> round(3)
  } else {
    MaxValues <- apply(MeanAtAge, c('Sim', 'Year'), max) |> round(3)
  }
  
  ind <- MaxValues<0.99 & MaxValues!=0
  if (all(!ind)) {
    return(MeanAtAge)
  }
    
  if (alert) {
    cli::cli_alert_warning("WARNING: Selectivity-at-Age does not have a maximum value of 1. F-at-Age won't correspond with Apical F")
    cli::cli_alert_warning('Standardizing to a max value of 1 but you probably want to fix this in the OM')
  }
  
  if (byArea) {
    if (alert) {
      sims <- which(apply(ind,1, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
      TSs <- which(apply(ind, 2, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
      areas <- which(apply(ind,3, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
      cli::cli_alert('Simulations {.val {sims}}; Years {.val {TSs}}; Areas: {.val{areas}}')
    }
   
    dd <- dim(MeanAtAge)
    nSim <- dd[1]
    nYear <- dd[3]
    nArea <- dd[4]
    
    for (sim in 1:nSim) {
      for (year in 1:nYear) {
        for (area in 1:nArea) {
          MeanAtAge[sim, ,year, area] <-  MeanAtAge[sim, ,year, area]/max(MeanAtAge[sim, ,year, area], na.rm=TRUE)
        }
      }
    }
 
    
  } else {
    if (alert) {
      sims <- which(apply(ind,1, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
      TSs <- which(apply(ind, 2, sum)>0) |> cli::cli_vec(list("vec-trunc" = 5))
      cli::cli_alert('Simulations {.val {sims}}; Years {.val {TSs}};')
    }
    
    dd <- dim(MeanAtAge)
    nSim <- dd[1]
    nYear <- dd[3]
    
    for (sim in 1:nSim) {
      for (year in 1:nYear) {
        MeanAtAge[sim, ,year] <-  MeanAtAge[sim, ,year]/max(MeanAtAge[sim, ,year], na.rm=TRUE)
        
      }
    }
    
  }
  MeanAtAge
}



MeanAtAge2MeanAtLength <- function(object, Length, replace=FALSE, max1=FALSE) {
  
  if (!is.null(object@MeanAtLength) & !replace)
    return(object)
  
  CheckRequiredObject(Length, 'length')
  AtAge2AtSize(object, Length, max1)
}

MeanAtAge2MeanAtWeight <- function(object, Weight, replace=FALSE, max1=FALSE) {
  if (!is.null(object@MeanAtWeight) & !replace)
    return(object)
  
  CheckRequiredObject(Weight, 'weight')
  AtAge2AtSize(object, Weight, max1)
}

