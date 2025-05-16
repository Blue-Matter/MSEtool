
# Populate an object
# 
# This function takes a valid S4 object and ...
# 
setGeneric("Populate", function(object, ...) standardGeneric("Populate"))

setMethod("Populate", "om", function(object, silent=FALSE) {
  PopulateOM(object, silent)
})

setMethod("Populate", "stock", function(object, 
                                        ALK=TRUE, 
                                        AWK=FALSE, 
                                        seed=NULL,
                                        silent=FALSE) {
  PopulateStock(object, ALK, AWK, seed, silent)
})



## ---- OM -----
#' @describeIn Populate Populate an [om-class()] object
#' @param x A Operating Model object or sub-object
#' @param silent Logical. TRUE to suppress all messages
#' @export
#' 
PopulateOM <- function(OM, silent=FALSE) {
  if (CheckDigest(list(), OM) | EmptyObject(OM))
    return(OM)
  
  # TODO - object check
  chk <- Check(OM)
  
  nStocks <- nStock(OM)
  nFleets <- nFleet(OM)
  
  # Populate Stock
  stockList <- vector('list', nStocks)
  names(stockList) <- paste('Stock', 1:nStocks)
  class(stockList) <- 'StockList'
  
  fleetList <- vector('list', nStocks)
  class(fleetList) <- 'StockFleetList'
  names(fleetList) <- paste('Stock', 1:nStocks)
  
  for (st in 1:nStocks) {
    if (isS4(OM@Stock)) {
      stock <- OM@Stock 
    } else {
      stock <- OM@Stock[[st]]
    }
    
    if (!silent) {
      cli::cli_alert('Populating: {.val {stock@Name}} ({st}/{nStocks})')
    }
    
    stock@nSim <- OM@nSim
    stock@nYear <- OM@nYear
    stock@pYear <- OM@pYear
    stock@CurrentYear <- OM@CurrentYear
    
    stockList[[st]] <- PopulateStock(stock, 
                                     seed=OM@Seed, 
                                     silent=silent)
    
    names(stockList)[st] <- stock@Name
    names(fleetList)[st] <- names(stockList)[st]
    fleetList[[st]] <- list()
    class(fleetList[[st]]) <- 'FleetList'
    
    for (fl in 1:nFleets) {
      if (isS4(OM@Fleet)) {
        fleet <- OM@Fleet
      } else if (inherits(OM@Fleet, 'FleetList')) {
        fleet <- OM@Fleet[[fl]]
      } else {
        fleet <- OM@Fleet[[st]][[fl]]
      }
      
      fleet@nSim <- OM@nSim
      fleet@nYear <- OM@nYear
      fleet@pYear <- OM@pYear
      fleet@CurrentYear <- OM@CurrentYear
      
      fleet@TimeUnits <- stock@Ages@Units
      fleet@TimeStepsPerYear <- TSperYear(stock@TimeUnits)
      fleet@TimeSteps <- CalcTimeSteps(stock@nYear, 
                                       stock@pYear, 
                                       stock@CurrentYear, 
                                       stock@TimeUnits)
      
      fleetList[[st]][[fl]] <- PopulateFleet(fleet, 
                                        Ages=Ages(stockList[[st]]),
                                        Length=Length(stockList[[st]]),
                                        nAreas=nArea(stockList[[st]]),
                                        seed=OM@Seed,
                                        silent=silent)
      
      names(fleetList[[st]])[fl] <- fleetList[[st]][[fl]]@Name
      
    }
  }
  
  if (methods::is(OM@Stock, 'list') | methods::is(OM@Stock, 'StockList')) {
    OM@Stock <- stockList
  }
 
  if (methods::is(OM@Fleet, 'list')| methods::is(OM@Fleet, 'StockFleetList')) {
    OM@Fleet <- fleetList
  }
 
  # share paramaters for two-sex stocks
  OM <- OM |>
    UpdateSPFrom() |>
    ShareParameters() |>
    StartMessages()
  
  # CatchFrac
  OM <- ProcessCatchFrac(OM)
  
  
  # Obs and Imp
  
  # Update OM Timesteps
  # object@Stock[[1]]@Ages@Units
  # TimeUnits(object)
  #
  # object@Stock
  # object@TimeUnits
  
  SetDigest(list(), OM)
  
}


# ---- Stock ----

#' @describeIn Populate Populate an [stock-class()] object
#' @param seed Seed for the random number generator
#' @export
PopulateStock <- function(stock, 
                          ALK=TRUE, 
                          AWK=FALSE, 
                          seed=NULL, 
                          silent=FALSE) {
  
  argList <- list(seed, ALK, AWK)
  if (CheckDigest(argList, stock) | EmptyObject(stock))
    return(stock)
  
  SetSeed(stock, seed)
  
  stock@TimeUnits <- stock@Ages@Units
  stock@TimeStepsPerYear <- TSperYear(stock@TimeUnits)
  stock@TimeSteps <- CalcTimeSteps(stock@nYear, 
                                   stock@pYear, 
                                   stock@CurrentYear, 
                                   stock@TimeUnits)
  # Require ALK and/or AWK?
  ALK <- RequireALK(stock)
  # AWK <- RequireAWK(stock)
  
  stock@Length <- PopulateLength(stock@Length,
                                 Ages=stock@Ages,
                                 nsim=nSim(stock),
                                 TimeSteps=TimeSteps(stock),
                                 ASK=ALK,
                                 seed=seed,
                                 silent=silent)
  
  stock@Weight <- PopulateWeight(stock@Weight,
                                 Ages=stock@Ages,
                                 Length=stock@Length,
                                 nSim(stock),
                                 TimeSteps=TimeSteps(stock),
                                 ASK=AWK,
                                 seed=seed,
                                 silent=silent)
  
  stock@NaturalMortality <- PopulateNaturalMortality(stock@NaturalMortality,
                                                     Ages=stock@Ages,
                                                     Length=stock@Length,
                                                     nsim=nSim(stock),
                                                     TimeSteps=TimeSteps(stock),
                                                     seed=seed,
                                                     silent=silent)
  
  stock@Maturity <- PopulateMaturity(stock@Maturity,
                                     Ages=stock@Ages,
                                     Length=stock@Length,
                                     nsim=nSim(stock),
                                     TimeSteps=TimeSteps(stock),
                                     seed=seed,
                                     silent=silent)
  
  stock@Fecundity <- PopulateFecundity(stock@Fecundity,
                                       Ages=stock@Ages,
                                       Length=stock@Length,
                                       Weight=stock@Weight,
                                       Maturity=stock@Maturity,
                                       nsim=nSim(stock),
                                       TimeSteps=TimeSteps(stock),
                                       seed=seed,
                                       silent=silent)
  
  stock@SRR <- PopulateSRR(stock@SRR,
                           MaxAge=stock@Ages@MaxAge,
                           CurrentYear=stock@CurrentYear,
                           TimeSteps=stock@TimeSteps,
                           nsim=stock@nSim,
                           seed=seed,
                           silent=silent)
  
  stock@Spatial <- PopulateSpatial(stock@Spatial,
                                   Ages=stock@Ages,
                                   TimeSteps=TimeSteps(stock),
                                   nsim=stock@nSim,
                                   seed=seed,
                                   silent=silent)
  
  stock@Depletion <- PopulateDepletion(stock@Depletion,
                                       nsim=stock@nSim,
                                       seed=seed)
  
  SetDigest(argList, stock)
}


PopulateLength <- function(Length,
                           Ages=NULL,
                           nsim=NULL,
                           TimeSteps=NULL,
                           ASK=TRUE,
                           seed=NULL,
                           silent=FALSE) {
  
  TimeSteps <- TimeStepAttributes(Length, TimeSteps)
  argList <- list(Ages, nsim, TimeSteps, ASK, seed)
  
  if (CheckDigest(argList, Length) | EmptyObject(Length))
    return(Length)
  
  SetSeed(Length, seed)
  
  Length@Pars <- StructurePars(Pars=Length@Pars, nsim, TimeSteps)
  Length@Model <- FindModel(Length)
  Length <- PopulateMeanAtAge(Length, Ages, TimeSteps)
  Length <- PopulateRandom(Length)
  Length@CVatAge <- StructureCV(Length@CVatAge, nsim)
  dd <- dim(Length@CVatAge)
  dimnames(Length@CVatAge) <- list(Sim=(1:nsim)[1:dd[1]],
                                   Age=Ages@Classes[1:dd[2]],
                                   TimeStep=TimeSteps[1:dd[3]])
  
  if (is.null(Length@CVatAge))
    ASK <- FALSE
  
  if (ASK) {
    Length <- PopulateClasses(Length)
    Length <- PopulateASK(Length, Ages, TimeSteps, silent=silent)
  }
  
  Length <- AddMeanAtAgeAttributes(Length, TimeSteps, Ages)
  SetDigest(argList, Length)
}


PopulateWeight <- function(Weight,
                           Ages=NULL,
                           Length=NULL,
                           nsim=NULL,
                           TimeSteps=NULL,
                           ASK=FALSE,
                           CalcAtLength=FALSE,
                           seed=NULL,
                           silent=FALSE) {
  TimeSteps <- TimeStepAttributes(Weight, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, ASK,
                  CalcAtLength, seed)
  
  if (CheckDigest(argList, Weight) | EmptyObject(Weight))
    return(Weight)
  
  SetSeed(Weight, seed)
  
  Weight@Pars <- StructurePars(Pars=Weight@Pars, nsim, TimeSteps)
  Weight@Model <- FindModel(Weight)
  
  ModelClass <- getModelClass(Weight@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Weight@Model))) {
      CheckRequiredObject(Length, 'length', 'Length')
      chk <- Check(Length, silent=TRUE)
      if (!chk@populated) {
        CheckRequiredObject(Ages, 'ages', 'Ages')
        Length <- Populate(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
      }
      Weight <- PopulateMeanAtLength(Weight, Length, TimeSteps, Ages,
                                     nsim,  seed, silent)
    } else {
      Weight <- PopulateMeanAtAge(Weight, Ages, TimeSteps, Length)
    }
  }
  
  Weight <- MeanAtLength2MeanAtAge(Weight, Length, Ages, nsim, TimeSteps, seed, silent)
  if (CalcAtLength)
    Weight <- MeanAtAge2MeanAtLength(Weight, Length, Ages, nsim, TimeSteps, seed, silent)
  
  Weight <- PopulateRandom(Weight)
  Weight@CVatAge <- StructureCV(Weight@CVatAge, nsim)
  dd <- dim(Weight@CVatAge)
  if (!is.null(dd))
    dimnames(Weight@CVatAge) <- list(Sim=(1:nsim)[1:dd[1]],
                                     Age=Ages@Classes[1:dd[2]],
                                     TimeStep=TimeSteps[1:dd[3]])
  if (is.null(Weight@CVatAge))
    ASK <- FALSE
  
  if (ASK) {
    Weight <- PopulateClasses(Weight)
    Weight <- PopulateASK(Weight, Ages, TimeSteps, silent, type='Weight')
  }
  
  SetDigest(argList, Weight)
}


PopulateNaturalMortality <- function(NaturalMortality,
                                     Ages=NULL,
                                     Length=NULL,
                                     nsim=NULL,
                                     TimeSteps=NULL,
                                     CalcAtLength=FALSE,
                                     seed=NULL,
                                     silent=FALSE) {
  
  TimeSteps <- TimeStepAttributes(NaturalMortality, TimeSteps)
  
  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)
  if (CheckDigest(argList, NaturalMortality) | EmptyObject(NaturalMortality))
    return(NaturalMortality)
  
  SetSeed(NaturalMortality, seed)
  
  NaturalMortality@Pars <- StructurePars(Pars=NaturalMortality@Pars, nsim, TimeSteps)
  NaturalMortality@Model <- FindModel(NaturalMortality)
  
  ModelClass <- getModelClass(NaturalMortality@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(NaturalMortality@Model))) {
      NaturalMortality <- PopulateMeanAtLength(NaturalMortality, Length,
                                               TimeSteps, Ages, nsim,
                                               seed,silent)
    } else {
      NaturalMortality <- PopulateMeanAtAge(NaturalMortality, Ages, TimeSteps)
    }
  }
  
  NaturalMortality <- MeanAtLength2MeanAtAge(NaturalMortality, Length, Ages,
                                             nsim, TimeSteps, seed, silent)
  if (CalcAtLength)
    NaturalMortality <- MeanAtAge2MeanAtLength(NaturalMortality, Length, 
                                               Ages, nsim, TimeSteps, seed,
                                               silent)
  
  NaturalMortality <- PopulateRandom(NaturalMortality)
  
  NaturalMortality <- AddMeanAtAgeAttributes(NaturalMortality, TimeSteps, Ages)
  SetDigest(argList, NaturalMortality)
}

PopulateMaturity <- function(Maturity,
                             Ages=NULL,
                             Length=NULL,
                             nsim=NULL,
                             TimeSteps=NULL,
                             CalcAtLength=FALSE,
                             seed=NULL,
                             silent=FALSE) {
  
  TimeSteps <- TimeStepAttributes(Maturity, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)
  if (CheckDigest(argList, Maturity) | EmptyObject(Maturity))
    return(Maturity)
  
  SetSeed(Maturity, seed)
 
  
  Maturity@Pars <- StructurePars(Pars=Maturity@Pars, nsim, TimeSteps)
  Maturity@Model <- FindModel(Maturity)
  
  ModelClass <- getModelClass(Maturity@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Maturity@Model))) {
      Maturity <- PopulateMeanAtLength(Maturity, Length, TimeSteps, Ages, nsim,
                                     seed, silent)
    } else {
      Maturity <- PopulateMeanAtAge(Maturity, Ages, TimeSteps)
    }
  }
  
  Maturity <- MeanAtLength2MeanAtAge(Maturity, Length, Ages, nsim, 
                                   TimeSteps, seed, silent)
  if (CalcAtLength)
    Maturity <- MeanAtAge2MeanAtLength(Maturity, Length, Ages, nsim, 
                                       TimeSteps, seed, silent)
  
  Maturity <- AddMeanAtAgeAttributes(Maturity, TimeSteps, Ages)
  
  # Semelparous 
  if (inherits(Maturity@Semelparous, 'array')) {
    
  } else {
    if (Maturity@Semelparous) {
      Maturity@Semelparous <- Maturity@MeanAtAge 
    } else {
      Maturity@Semelparous <- Maturity@MeanAtAge 
      Maturity@Semelparous[] <- 0
    }
  }
  
  SetDigest(argList, Maturity)
}

PopulateFecundity <- function(Fecundity,
                              Ages=NULL,
                              Length=NULL,
                              Weight=NULL,
                              Maturity=NULL,
                              nsim=NULL,
                              TimeSteps=NULL,
                              CalcAtLength=FALSE,
                              seed=NULL,
                              silent=FALSE) {
  TimeSteps <- TimeStepAttributes(Fecundity, TimeSteps)
  argList <- list(Ages, Length, Weight, Maturity, nsim, TimeSteps, CalcAtLength, seed)
  
  if (EmptyObject(Fecundity)) {
   
    CheckRequiredObject(Ages, 'ages', 'Ages')
    CheckRequiredObject(Weight, 'weight', 'Weight')
    CheckRequiredObject(Length, 'length', 'Length')
    CheckRequiredObject(Maturity, 'maturity', 'Maturity')
    
    Weight <- PopulateWeight(Weight, Ages, Length, nsim, TimeSteps,
                       seed=seed, ASK=FALSE)
    Maturity <- PopulateMaturity(Maturity, Ages, Length, nsim, TimeSteps,
                         seed=seed)
    
    Fecundity@MeanAtAge <- ArrayMultiply(array1=Weight@MeanAtAge,
                                         array2=Maturity@MeanAtAge)
    
    # object@MeanAtAge <- Weight@MeanAtAge # egg production is fecundity x maturity - calculated internally
    # fecundity is the egg production of a MATURE individual 
    
    return(SetDigest(argList, Fecundity))
  }
  
  if (CheckDigest(argList, Fecundity))
    return(Fecundity)
  
  SetSeed(Fecundity, seed)
  
  Fecundity@Pars <- StructurePars(Pars=Fecundity@Pars, nsim, TimeSteps)
  Fecundity@Model <- FindModel(Fecundity)
  
  if (is.null(Fecundity@Model)| all(is.na(Fecundity@Pars))) {
    if (is.null(Fecundity@MeanAtAge)) {

      CheckRequiredObject(Ages, 'ages', 'Ages')
      CheckRequiredObject(Weight, 'weight', 'Weight')
      CheckRequiredObject(Length, 'length', 'Length')
      CheckRequiredObject(Maturity, 'maturity', 'Maturity')
      
      Weight <- PopulateWeight(Weight, Ages, Length, nsim, TimeSteps,
                         seed=seed, ASK=FALSE)
      Maturity <- PopulateMaturity(Maturity, Ages, Length, nsim, TimeSteps, seed=seed)
      Fecundity@MeanAtAge <- ArrayMultiply(array1=Weight@MeanAtAge,
                                        array2=Maturity@MeanAtAge)
      
      return(SetDigest(argList, Fecundity))
    }
  }
  
  ModelClass <- getModelClass(Fecundity@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Fecundity@Model))) {
      Fecundity <- PopulateMeanAtLength(Fecundity, Length, TimeSteps, Ages, nsim,
                                     seed, silent)
    } else {
      Fecundity <- PopulateMeanAtAge(Fecundity, Ages, TimeSteps)
    }
  }
  
  Fecundity <- MeanAtLength2MeanAtAge(Fecundity, Length, Ages, nsim, 
                                      TimeSteps, seed, silent)
  if (CalcAtLength)
    Fecundity <- MeanAtAge2MeanAtLength(Fecundity, Length, Ages, nsim, 
                                     TimeSteps, seed, silent)
  
  Fecundity <- AddMeanAtAgeAttributes(Fecundity, TimeSteps, Ages)
  
  SetDigest(argList, Fecundity)
}

PopulateSRR <- function(SRR,
                        MaxAge=NULL,
                        CurrentYear=NULL,
                        TimeSteps=NULL,
                        nsim=NULL,
                        seed=NULL,
                        silent=FALSE) {
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
  
  if (CheckDigest(argList, SRR) | EmptyObject(SRR))
    return(SRR)
  
  SetSeed(SRR, seed)
  
  SRR@Pars <- StructurePars(Pars=SRR@Pars, nsim, TimeSteps)
  SRR@Model <- FindModel(SRR)
  
  
  pars <- StructurePars(list(SRR@R0, SRR@SD, SRR@AC), nsim, TimeSteps)
  SRR@R0 <- pars[[1]][,1, drop=FALSE] # only one time step for now
  SRR@SD <- pars[[2]][,1, drop=FALSE] # only one time step for now
  SRR@AC <- pars[[3]][,1, drop=FALSE] # only one time step for now
  SRR@AC[!is.finite(SRR@AC)] <- 0
  SRR@SD[SRR@SD==0] <- 1E-6 # for reproducibility in rnorm
  
  EmptyObjects <- c(EmptyObject(SRR@RecDevInit),
                    EmptyObject(SRR@RecDevHist),
                    EmptyObject(SRR@RecDevProj))
  
  if (all(!EmptyObjects)) {
    return( SetDigest(argList, SRR))
  }
  
  RecDeviations <- GenerateRecruitmentDeviations(SD=SRR@SD,
                                                 AC=SRR@AC,
                                                 TruncSD=SRR@TruncSD,
                                                 MaxAge,
                                                 nHistTS,
                                                 nProjTS,
                                                 nsim=nsim,
                                                 RecDevInit=SRR@RecDevInit,
                                                 RecDevHist=SRR@RecDevHist,
                                                 RecDevProj=SRR@RecDevProj)
  
  SRR@RecDevInit <- RecDeviations$RecDevInit
  dimnames(SRR@RecDevInit) <- list(
    Sim=1:nrow(SRR@RecDevInit),
    Age=1:ncol(SRR@RecDevInit)
  )
  
  SRR@RecDevHist <- RecDeviations$RecDevHist
  dimnames(SRR@RecDevHist) <- list(
    Sim=1:nrow(SRR@RecDevHist),
    TimeStep=histTS
  )
  
  SRR@RecDevProj <- RecDeviations$RecDevProj
  dimnames(SRR@RecDevProj) <- list(
    Sim=1:nrow(SRR@RecDevProj),
    TimeStep=projTS
  )
  SetDigest(argList, SRR)
}

PopulateSpatial <- function(Spatial,
                            Ages=NULL,
                            TimeSteps=NULL,
                            nsim=NULL,
                            seed=NULL,
                            silent=FALSE,
                            plot=FALSE,
                            nits=100) {
  argList <- list(Ages, nsim, seed, nits)
  
  if (CheckDigest(argList, Spatial))
    return(Spatial)
  
  SetSeed(Spatial, seed)
  
  # empty object
  DimNames <- c('Sim', 'Area', 'Age', 'TimeStep')
  if (EmptyObject(Spatial)) {
    Spatial@RelativeSize <- AddDimNames(array(1, dim=c(1,1)), 
                                       DimNames[1:2], TimeSteps=TimeSteps)
    Spatial@ProbStaying <- AddDimNames(array(1, dim=c(1,1,1,1)), 
                                      DimNames, TimeSteps=TimeSteps)
    Spatial@FracOther <- AddDimNames(array(1, dim=c(1,1,1,1)),
                                    DimNames, TimeSteps=TimeSteps)
    Spatial@UnfishedDist  <- AddDimNames(array(1, dim=c(1,1,1,1)),
                                        DimNames, TimeSteps=TimeSteps)
    Spatial@Movement  <- AddDimNames(array(1, dim=c(1,1,1,1,1)),
                                    c("Sim",'FromArea', 'ToArea','Age', 'TimeStep'),
                                    TimeSteps=TimeSteps)
    return(SetDigest(argList, Spatial))
  }
  

  if (is.null(Spatial@Movement)) {
    Spatial <- CalcMovement(Spatial, TimeSteps, nsim, seed, nits, plot, 
                            silent)
  } else {
    dnames <- dimnames(Spatial@Movement)
    if (!all(names(dnames)[1:3] == c('Sim', 'FromArea', 'ToArea'))) {
      cli::cli_abort("First three dimensions of `Spatial@Movement` must be: {.val {c('Sim', 'FromArea', 'ToArea')}}")
    }
    if (length(dnames)==3) {
      Spatial@Movement <- AddAgeTimeStepDimensions(Spatial@Movement, outdim=5) |>
        AddDimNames(c('Sim', 'FromArea', 'ToArea', 'Age', 'TimeStep'), TimeSteps=TimeSteps)
    } else if (length(dnames)==4) {
      cli::cli_abort("`Spatial@Movement` should either have dimensions: 
                     {.val {c('Sim', 'FromArea', 'ToArea')}} OR 
                     {.val {c('Sim', 'FromArea', 'ToArea', 'Age', 'TimeStep')}}")
    }
  }
  
  
  if (is.null(Spatial@UnfishedDist)) {
    Spatial <- CalcUnfishedDist(Spatial, TimeSteps)
  } 
  
  
  if (is.null(Spatial@UnfishedDist))
    cli::cli_abort('`UnfishedDist` must be populated for `Spatial` objects')
  
  # if (is.null(object@ProbStaying))
  #   cli::cli_abort('`ProbStaying` must be populated for `Spatial` objects')
  
  nareas <- dim(Spatial@UnfishedDist)[2]
  
  if (!is.null(Spatial@RelativeSize) & !methods::is(Spatial@RelativeSize, 'character')) {
    Spatial@RelativeSize <- StructurePars(list(Spatial@RelativeSize),nsim)[[1]]
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
      
      rowsums <- apply(Spatial@RelativeSize, 1, sum)
      if (!all(rowsums==1))
        cli::cli_abort('`RelativeSize` must sum to 1 across columns')
    }
    
  } else if (methods::is(Spatial@RelativeSize, 'character')) {
    if (Spatial@RelativeSize=="EqualDensity") {
      Spatial@RelativeSize <- apply(Spatial@UnfishedDist, 1:2, mean)   
    } else {
      cli::cli_abort('If `Spatial@RelativeSize` is character, it can only be "EqualDensity"')
    }
  } else {
    cli::cli_alert_warning('`RelativeSize` is not specified. Assuming all areas are equal size')
    Spatial@RelativeSize <- matrix(1/nareas, 1, nareas)
  }
  Spatial@RelativeSize <- AddDimNames(Spatial@RelativeSize, c('Sim', 'Area'))
  

  SetDigest(argList, Spatial)
  
}



PopulateInitial <- function(Initial, nsim=NA, name='Initial') {
  if (length(Initial)<1)
    return(Initial)
  if (all(is.na(Initial)))
    return(Initial)
  
  if (length(Initial)==1) 
    nsim <- 1
  
  if (length(Initial)==2) {
    # sample from uniform distribution
    if (is.na(nsim))
      cli::cli_abort(c('`nsim` required to generate stochastic values',
                       'i'='Provide number of simulations to `nsim` argument')
      )
    Initial <- sort(Initial)
    Initial <- stats::runif(nsim, Initial[1], Initial[2])
  }
  nsim <- length(Initial)
  array(Initial, dim=nsim, dimnames = list(Sim=1:nsim))
}

PopulateDepletion <- function(Depletion,  
                              nsim=NULL,
                              seed=NULL) {
  argList <- list(nsim, seed)
  
  if (CheckDigest(argList, Depletion) | EmptyObject(Depletion))
    return(Depletion)
  
  SetSeed(Depletion, seed)
  
  Depletion@Initial <- PopulateInitial(Depletion@Initial, nsim)
  Depletion@Final <- PopulateInitial(Depletion@Final, nsim, 'Final')

  validReference <- c('B0', 'BMSY', 'SB0', 'SBMSY')
  if (!Depletion@Reference %in% validReference)
    cli::cli_abort(c('Invalid value for `Reference`',
                     "x"="Currently {.val {Depletion@Reference}}. Must be one of: {.val {validReference}}")
                   )
  SetDigest(argList, Depletion)
}


# ---- Fleet ----
PopulateFleet <- function(Fleet, 
                          Ages=NULL,
                          Length=NULL,
                          nAreas=NULL,
                          seed=NULL,
                          silent=FALSE) {
  
  
  nsim <- nSim(Fleet)
  TimeSteps <- TimeSteps(Fleet)
  HistTimeSteps <- TimeSteps(Fleet, 'Historical')
  
  argList <- list(Ages, Length, nsim, TimeSteps, seed)
  if (CheckDigest(argList, Fleet) | EmptyObject(Fleet))
    return(Fleet)
  
  SetSeed(Fleet, seed)
  
  Fleet@FishingMortality <- PopulateFishingMortality(Fleet@FishingMortality,
                                                     nsim,
                                                     TimeSteps,
                                                     seed,
                                                     silent)
  
  Fleet@DiscardMortality <- PopulateDiscardMortality(Fleet@DiscardMortality,
                                     Ages,
                                     Length,
                                     nsim,
                                     TimeSteps,
                                     seed=seed,
                                     silent)
  
  Fleet@Selectivity <- PopulateSelectivity(Fleet@Selectivity,
                                           FishingMortality=Fleet@FishingMortality,
                                           DiscardMortality=Fleet@DiscardMortality,
                                           Ages,
                                           Length,
                                           nsim,
                                           TimeSteps,
                                           CalcAtLength=FALSE,
                                           seed,
                                           silent=silent)
  
  Fleet@Retention <- PopulateRetention(Fleet@Retention,
                                       FishingMortality=Fleet@FishingMortality,
                                       DiscardMortality=Fleet@DiscardMortality,
                                       Ages,
                                       Length,
                                       nsim,
                                       TimeSteps,
                                       CalcAtLength=FALSE,
                                       seed,
                                       silent=silent)
  
  Fleet@Effort <- PopulateEffort(Fleet@Effort,
                                 FishingMortality=Fleet@FishingMortality,
                                 DiscardMortality=Fleet@DiscardMortality,
                                 Ages,
                                 Length,
                                 nsim,
                                 HistTimeSteps,
                                 seed,
                                 silent)
  
  Fleet@Distribution <- PopulateDistribution(Fleet@Distribution, 
                                             nsim, 
                                             TimeSteps,
                                             nAreas)
  
  SetDigest(argList, Fleet)
}


PopulateFishingMortality <- function(FishingMortality,
                                     nsim=NULL,
                                     TimeSteps=NULL,
                                     seed=NULL,
                                     silent=FALSE) {
  
  TimeSteps <- TimeStepAttributes(FishingMortality, TimeSteps)
  argList <- list(nsim, TimeSteps, seed)
  
  if (CheckDigest(argList, FishingMortality) | EmptyObject(FishingMortality))
    return(FishingMortality)
  
  SetSeed(FishingMortality, seed)
 
  FishingMortality@ApicalF <- AddSimDimension(FishingMortality@ApicalF,
                                              c('Sim', 'TimeStep'), 
                                              TimeSteps=TimeSteps)
  
  FishingMortality@DeadAtAge <- AddSimDimension(FishingMortality@DeadAtAge,
                                                TimeSteps=TimeSteps)
  FishingMortality@RetainAtAge <- AddSimDimension(FishingMortality@RetainAtAge,
                                                  TimeSteps=TimeSteps)
  
  if (EmptyObject(FishingMortality@ApicalF)) # calculate from `DeadAtAge`
    FishingMortality@ApicalF <- apply(FishingMortality@DeadAtAge, c(1,3), max)
  
  SetDigest(argList, FishingMortality)
  
}
  

PopulateDiscardMortality <- function(DiscardMortality,
                                     Ages=NULL,
                                     Length=NULL,
                                     nsim=NULL,
                                     TimeSteps=NULL,
                                     CalcAtLength=FALSE,
                                     seed=NULL,
                                     silent=FALSE) {
  TimeSteps <- TimeStepAttributes(DiscardMortality, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)
  
  if (CheckDigest(argList, DiscardMortality) | EmptyObject(DiscardMortality))
    return(DiscardMortality)
  
  SetSeed(DiscardMortality, seed)
  
  DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, Length,
                                             Ages, nsim, TimeSteps, seed, silent)
  if (CalcAtLength)
    DiscardMortality <- MeanAtAge2MeanAtLength(DiscardMortality, Length, Ages,
                                               nsim, TimeSteps, seed, silent)
  
  DiscardMortality <- AddMeanAtAgeAttributes(DiscardMortality, TimeSteps, Ages)
  
  # Dimnames for at length
  if (!is.null(DiscardMortality@MeanAtLength)) {
    dd <- dim(DiscardMortality@MeanAtLength)
    dnames <- names(dimnames(DiscardMortality@MeanAtLength))
    if (is.null(dnames)) 
      dimnames(DiscardMortality@MeanAtLength) <- list(Sim=1:dd[1],
                                            Class=DiscardMortality@Classes,
                                            TimeStep=TimeSteps[1:dd[3]])
  }
  
  SetDigest(argList, DiscardMortality)
}
  
PopulateSelectivity <- function(Selectivity,
                                FishingMortality=NULL,
                                DiscardMortality=NULL,
                                Ages=NULL,
                                Length=NULL,
                                nsim=NULL,
                                TimeSteps=NULL,
                                CalcAtLength=FALSE,
                                seed=NULL,
                                silent=FALSE,
                                CheckMaxValue=TRUE) {
  
  TimeSteps <- TimeStepAttributes(Selectivity, TimeSteps)
  argList <- list(FishingMortality, DiscardMortality, Ages, Length, 
                  TimeSteps, nsim, CalcAtLength, seed)
  
  if (CheckDigest(argList, Selectivity))
    return(Selectivity)
  
  SetSeed(Selectivity, seed)
 
  
  Selectivity@Pars <- StructurePars(Pars=Selectivity@Pars, nsim, TimeSteps)
  Selectivity@Model <- FindModel(Selectivity)
  
  ModelClass <- getModelClass(Selectivity@Model)
  
  if (!is.null(ModelClass)) {
    
    if (grepl('at-Length',getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtLength(Selectivity, 
                                          Length, 
                                          TimeSteps,
                                          Ages, 
                                          nsim,
                                          seed, 
                                          silent)
    } else {
      Selectivity <- PopulateMeanAtAge(Selectivity, Ages, TimeSteps)
    }
  } 
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, Ages, nsim,
                                        TimeSteps, seed, silent)
  
  if (CalcAtLength)
    Selectivity <- MeanAtAge2MeanAtLength(Selectivity, Length, Ages, nsim, TimeSteps, seed, silent)
  
  if (is.null(Selectivity@MeanAtAge)) {
    chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    if (!chk@populated)
      FishingMortality <- PopulateFishingMortality(FishingMortality,
                                   nsim,
                                   TimeSteps,
                                   seed,
                                   silent)
    
    if (!EmptyObject(FishingMortality@DeadAtAge)) {
      Selectivity@MeanAtAge <- FishingMortality2Selectivity(FishingMortality,
                                                            DiscardMortality,
                                                            Ages,
                                                            TimeSteps,
                                                            Length)
    } else {
      cli::cli_abort('`Selectivity` requires either `Pars`, `MeanAtAge` or a `FishingMortality` object')
    }
  }
  
  # Check Selectivity has a max value of one across age classes
  if(CheckMaxValue) 
    Selectivity@MeanAtAge <- CheckSelectivityMaximum(Selectivity@MeanAtAge)
  
  
  # Dimnames for at length
  if (!is.null(Selectivity@MeanAtLength)) {
    dd <- dim(Selectivity@MeanAtLength)
    dnames <- names(dimnames(Selectivity@MeanAtLength))
    if (is.null(dnames)) 
      dimnames(Selectivity@MeanAtLength) <- list(Sim=1:dd[1],
                                                 Class=Selectivity@Classes,
                                                 TimeStep=TimeSteps[1:dd[3]])
  }
  
  
  # selectivity <- AddMeanAtAgeAttributes(selectivity, TimeSteps, Ages)

  SetDigest(argList, Selectivity)
}


PopulateRetention <- function(Retention, 
                              FishingMortality=NULL,
                              DiscardMortality=NULL,
                              Ages=NULL,
                              Length=NULL,
                              nsim=NULL,
                              TimeSteps=NULL,
                              CalcAtLength=FALSE,
                              seed=NULL,
                              silent=FALSE) {
  
  TimeSteps <- TimeStepAttributes(Retention, TimeSteps)
  argList <- list(FishingMortality, DiscardMortality, Ages, Length, 
                  TimeSteps, nsim, CalcAtLength, seed)
  
  
  if (CheckDigest(argList, Retention))
    return(Retention)
  
  if (EmptyObject(Retention)) {
    Retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(TimeSteps=TimeSteps)
    Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'TimeStep'), TimeSteps=TimeSteps)
    
    return(SetDigest(argList, Retention))
  }
  
  SetSeed(Retention, seed)


  Retention@Pars <- StructurePars(Pars=Retention@Pars, nsim, TimeSteps)
  
  ParsZero <- all((lapply(lapply(Retention@Pars, `==`, 0), prod) |> 
                     unlist())==1)
  
  if (!ParsZero) {
    Retention@Model <- FindModel(Retention)
    ModelClass <- getModelClass(Retention@Model)
    
    if (!is.null(ModelClass)) {
      if (grepl('at-Length',getModelClass(Retention@Model))) {
        Retention <- PopulateMeanAtLength(Retention, Length, TimeSteps,
                                          Ages, nsim,
                                          seed, silent)
      } else {
        Retention <- PopulateMeanAtAge(Retention, Ages, TimeSteps)
      }
    } 
    
  }
  
  if (ParsZero & is.null(Retention@MeanAtAge) & is.null(Retention@MeanAtLength)) {
    
    Retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(TimeSteps=TimeSteps)
    Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'TimeStep'),TimeSteps=TimeSteps)
    
    return(SetDigest(argList, Retention))
  } 
  
  Retention <- MeanAtLength2MeanAtAge(Retention, Length, Ages,
                                      nsim, TimeSteps, seed, silent)
  if (CalcAtLength)
    Retention <- MeanAtAge2MeanAtLength(Retention, Length, Ages, 
                                        nsim, TimeSteps, seed, silent)
  
  if (is.null(Retention@MeanAtAge)) {
    chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    if (!chk@populated)
      FishingMortality <- PopulateFishingMortality(FishingMortality,
                                   nsim,
                                   TimeSteps,
                                   seed,
                                   silent)
    
    if (!EmptyObject(FishingMortality@DeadAtAge)) {
      Retention@MeanAtAge <- FishingMortality2Retention(FishingMortality,
                                                        DiscardMortality,
                                                        Ages,
                                                        TimeSteps,
                                                        Length)
    } else {
      cli::cli_abort('`Retention` requires either `Pars`, `MeanAtAge` or a `FishingMortality` object')
    }
  }
  
  
  # Dimnames for at length
  if (!is.null(Retention@MeanAtLength)) {
    dd <- dim(Retention@MeanAtLength)
    dnames <- names(dimnames(Retention@MeanAtLength))
    if (is.null(dnames)) 
      dimnames(Retention@MeanAtLength) <- list(Sim=1:dd[1],
                                               Class=Retention@Classes,
                                               TimeStep=TimeSteps[1:dd[3]])
  }
  
  # Retention <- AddMeanAtAgeAttributes(Retention, TimeSteps, Ages)
  SetDigest(argList, Retention)
  
}

  

PopulateEffort <- function(Effort,
                           FishingMortality=NULL,
                           DiscardMortality=NULL,
                           Ages=NULL,
                           Length=NULL,
                           nsim=NULL,
                           TimeSteps=NULL,
                           seed=NULL,
                           silent=FALSE) {
  
  argList <- list(FishingMortality,
                  DiscardMortality,
                  Ages,
                  Length,
                  TimeSteps,
                  seed)
  
  if (CheckDigest(argList, Effort))
    return(Effort)
  
  SetSeed(Effort, seed)
  
  
  if (EmptyObject(Effort@Catchability)) {
    # if (!silent)
    # cli::cli_alert_info('`Catchability` (q) not populated. Assuming `q=1`')
    Effort@Catchability <- matrix(tiny/2,1,1) |> AddDimNames(c('Sim', 'TimeStep'),
                                                             TimeSteps=TimeSteps)
  } else {
    Effort@Catchability <- Structure(Effort@Catchability, c('nSim', 'nTS')) |>
      AddDimNames(c('Sim', 'TimeStep'), TimeSteps=TimeSteps)
  }
  
  if (EmptyObject(Effort@Effort)) {
    FInteract <- CalculateFInteract(FishingMortality,
                                    DiscardMortality,
                                    Ages,
                                    TimeSteps,
                                    Length)
    dd <- dim(FInteract)
    Effort@Effort <- ArrayMultiply(array1=Effort@Catchability,
                                   array2=apply(FInteract,c(1,3), max)
    ) |> AddDimNames(c('Sim', 'TimeStep'), TimeSteps=TimeSteps)
  } else {
    if (methods::is(Effort@Effort, 'data.frame')) {
      Effort@Effort <- GenerateHistoricalEffort(Effort@Effort, nsim, TimeSteps)
      
    } else{
      Effort@Effort <- Structure(Effort@Effort, c('nSim', 'nTS')) |>
        AddDimNames(c('Sim', 'TimeStep'), TimeSteps=TimeSteps)
    }
  }
  SetDigest(argList, Effort)
}
                            

PopulateDistribution <- function(Distribution, 
                                 nsim=NULL,
                                 TimeSteps=NULL,
                                 nAreas=NULL) {
  
  argList <- list(TimeSteps, nAreas)
  
  if (CheckDigest(argList, Distribution))
    return(Distribution)
  
  # Closure
  if (EmptyObject(Distribution@Closure)) {
    Distribution@Closure <- array(1, dim=c(nsim,
                                     length(TimeSteps),
                                     nAreas),
                            dimnames=list(Sim=1:nsim,
                                          TimeStep=TimeSteps,
                                          Area=1:nAreas)
    )
    
  } else {
    stop('`Closure` not done yet')
  }
  
  # Cost 
  if (!EmptyObject(Distribution@Cost))
    cli::cli_warn('`Distribution@Cost` not currently supported')
  
  SetDigest(argList, Distribution)
}













