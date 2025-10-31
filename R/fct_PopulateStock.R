
#' @describeIn Populate Populate a [Stock()] object
#' @export
PopulateStock <- function(stock, 
                          ALK=TRUE, 
                          AWK=TRUE, 
                          seed=NULL, 
                          silent=FALSE) {
  
  argList <- list(seed, ALK, AWK)
  if (CheckDigest(stock, argList) | EmptyObject(stock))
    return(stock)
  
  SetSeed(stock, seed)
  
  stock@TimeUnits <- stock@Ages@Units
  stock@TimeStepsPerYear <- TSperYear(stock@TimeUnits)
  stock@TimeSteps <- CalcTimeSteps(nYear=stock@nYear, 
                                   pYear=stock@pYear, 
                                   CurrentYear=stock@CurrentYear, 
                                   TimeUnits=stock@TimeUnits)
  # Require ALK and/or AWK?
  # ALK <- RequireALK(stock)
  # AWK <- RequireAWK(stock)
  
  stock@Length <- PopulateLength(Length=stock@Length,
                                 Ages=stock@Ages,
                                 nsim=nSim(stock),
                                 TimeSteps=TimeSteps(stock),
                                 ASK=ALK,
                                 seed=seed,
                                 silent=silent)
  
  stock@Weight <- PopulateWeight(Weight=stock@Weight,
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
  
  stock@Maturity <- PopulateMaturity(Maturity=stock@Maturity,
                                     Ages=stock@Ages,
                                     Length=stock@Length,
                                     Weight=stock@Weight,
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
  
  stock@SRR <- PopulateSRR(SRR=stock@SRR,
                           Ages=stock@Ages,
                           CurrentYear=stock@CurrentYear,
                           TimeSteps=stock@TimeSteps,
                           nsim=stock@nSim,
                           seed=seed,
                           silent=silent)
  
  stock@Spatial <- PopulateSpatial(Spatial=stock@Spatial,
                                   Ages=stock@Ages,
                                   TimeSteps=TimeSteps(stock),
                                   nsim=stock@nSim,
                                   seed=seed,
                                   silent=silent)
  
  stock@Depletion <- PopulateDepletion(stock@Depletion,
                                       nsim=stock@nSim,
                                       seed=seed)
  
  SetDigest(stock, argList)
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
  
  if (CheckDigest(Length, argList) | EmptyObject(Length))
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
  
  if (!is.null(Length@CVatAge))
    Length <- PopulateClasses(Length)
  
  
  if (ASK) {
    Length <- PopulateASK(Length, Ages, TimeSteps, silent=silent)
  }
  
  Length <- AddMeanAtAgeAttributes(Length, TimeSteps, Ages)
  SetDigest(Length, argList)
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
  
  if (CheckDigest(Weight, argList) | EmptyObject(Weight))
    return(Weight)
  
  SetSeed(Weight, seed)
  
  Weight@Pars <- StructurePars(Pars=Weight@Pars, nsim, TimeSteps)
  Weight@Model <- FindModel(Weight)
  
  ModelClass <- getModelClass(Weight@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Weight@Model))) {
      CheckRequiredObject(Length, 'length', 'Length')
      # chk <- Check(Length, silent=TRUE)
      # if (!chk@populated) {
      CheckRequiredObject(Ages, 'ages', 'Ages')
      Length <- PopulateLength(Length, Ages, nsim, TimeSteps, seed, ASK=TRUE, silent)
      # }
      Weight <- PopulateMeanAtLength(Weight, Length, TimeSteps, Ages,
                                     nsim,  seed, silent)
    } else {
      Weight <- PopulateMeanAtAge(Weight, Ages, TimeSteps, Length)
    }
  }
  
  Weight <- MeanAtLength2MeanAtAge(Weight, Length, Ages, nsim, TimeSteps, seed, silent)
  
  if (CalcAtLength) {
    Weight <- MeanAtAge2MeanAtLength(Weight, Length, Ages, nsim, TimeSteps, seed, silent)
  }
  
  Weight <- PopulateRandom(Weight)
  Weight@CVatAge <- StructureCV(Weight@CVatAge, nsim)
  dd <- dim(Weight@CVatAge)
  if (!is.null(dd))
    dimnames(Weight@CVatAge) <- list(Sim=(1:nsim)[1:dd[1]],
                                     Age=Ages@Classes[1:dd[2]],
                                     TimeStep=TimeSteps[1:dd[3]])
  if (is.null(Weight@CVatAge))
    ASK <- FALSE
  
  Weight@MeanAtAge <- AddDimNames(Weight@MeanAtAge, TimeSteps=TimeSteps, Ages=Ages@Classes)
  Weight@CVatAge <- AddDimNames(Weight@CVatAge, TimeSteps=TimeSteps, Ages=Ages@Classes)
  
  if (ASK) {
    Weight <- PopulateClasses(Weight)
    Weight <- PopulateASK(Weight, Ages, TimeSteps, silent, type='Weight')
  }
  
  SetDigest(Weight, argList)
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
  if (CheckDigest( NaturalMortality, argList) | EmptyObject(NaturalMortality))
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
  SetDigest(NaturalMortality, argList)
}

PopulateMaturity <- function(Maturity,
                             Ages=NULL,
                             Length=NULL,
                             Weight=NULL,
                             nsim=NULL,
                             TimeSteps=NULL,
                             CalcAtLength=FALSE,
                             seed=NULL,
                             silent=FALSE) {
  
  TimeSteps <- TimeStepAttributes(Maturity, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)
  
  if (CheckDigest(Maturity, argList) | EmptyObject(Maturity))
    return(Maturity)
  
  SetSeed(Maturity, seed)
  
  
  Maturity@Pars <- StructurePars(Pars=Maturity@Pars, nsim, TimeSteps)
  Maturity@Model <- FindModel(Maturity)
  ModelClass <- getModelClass(Maturity@Model)
  
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Maturity@Model))) {
      Maturity <- PopulateMeanAtLength(Maturity, Length, TimeSteps, Ages, nsim,
                                       seed, silent)
    } else if (grepl('at-Weight',getModelClass(Maturity@Model))) {
      Maturity <- PopulateMeanAtWeight(Maturity, Weight, TimeSteps, Ages, nsim,
                                       seed, silent)
    } else {
      Maturity <- PopulateMeanAtAge(Maturity, Ages, TimeSteps)
    }
  }
  
  Maturity <- MeanAtLength2MeanAtAge(Maturity, Length, Ages, nsim, 
                                     TimeSteps, seed, silent)
  
  Maturity <- MeanAtWeight2MeanAtAge(Maturity, Weight, Ages, nsim, TimeSteps,
                                     seed, silent)
  
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
  
  SetDigest(Maturity, argList)
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
    Maturity <- PopulateMaturity(Maturity, Ages, Length, Weight, nsim, TimeSteps,
                                 seed=seed)
    
    Fecundity@MeanAtAge <- ArrayMultiply(array1=Weight@MeanAtAge,
                                         array2=Maturity@MeanAtAge)
    
    # object@MeanAtAge <- Weight@MeanAtAge # egg production is fecundity x maturity - calculated internally
    # fecundity is the egg production of a MATURE individual 
    
    return(SetDigest(Fecundity, argList))
  }
  
  if (CheckDigest(Fecundity, argList))
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
      
      Weight <- PopulateWeight(Weight, Ages, Length, Weight, nsim, TimeSteps,
                               seed=seed, ASK=FALSE)
      Maturity <- PopulateMaturity(Maturity, Ages, Length, nsim, TimeSteps, seed=seed)
      Fecundity@MeanAtAge <- ArrayMultiply(array1=Weight@MeanAtAge,
                                           array2=Maturity@MeanAtAge)
      
      return(SetDigest(Fecundity, argList))
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
  
  SetDigest(Fecundity, argList)
}

PopulateSRR <- function(SRR,
                        Ages=NULL,
                        CurrentYear=NULL,
                        TimeSteps=NULL,
                        nsim=NULL,
                        seed=NULL,
                        silent=FALSE) {
  argList <- list(Ages, CurrentYear, TimeSteps, nsim, seed)
  
  MaxAge <- Ages@MaxAge
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
  
  if (CheckDigest(SRR, argList) | EmptyObject(SRR))
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
    return( SetDigest(SRR, argList))
  }
  
  RecDeviations <- GenerateRecruitmentDeviations(SD=SRR@SD,
                                                 AC=SRR@AC,
                                                 TruncSD=SRR@TruncSD,
                                                 Ages,
                                                 nHistTS,
                                                 nProjTS,
                                                 nsim=nsim,
                                                 RecDevInit=SRR@RecDevInit,
                                                 RecDevHist=SRR@RecDevHist,
                                                 RecDevProj=SRR@RecDevProj)
  
  SRR@RecDevInit <- RecDeviations$RecDevInit
  dimnames(SRR@RecDevInit) <- list(
    Sim=1:nrow(SRR@RecDevInit),
    Age=Ages@Classes[-1]
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
  SetDigest(SRR, argList)
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
  
  if (CheckDigest(Spatial, argList))
    return(Spatial)
  
  SetSeed(Spatial, seed)
  
  # empty object
  DimNames <- c('Sim', 'Area', 'Age', 'TimeStep')
  if (EmptyObject(Spatial)) {
    Spatial@RelativeSize <- AddDimNames(array(1, dim=c(1,1)), 
                                        DimNames[1:2], TimeSteps=TimeSteps)
    Spatial@ProbStaying <- AddDimNames(array(1, dim=c(1,1,1,1)), 
                                       DimNames, TimeSteps=TimeSteps)
    Spatial@FracOther <- AddDimNames(array(1, dim=c(1,1,1,1,1)),
                                     c("Sim",'FromArea', 'ToArea','Age', 'TimeStep'), 
                                     TimeSteps=TimeSteps)
    Spatial@UnfishedDist  <- AddDimNames(array(1, dim=c(1,1,1,1)),
                                         DimNames, TimeSteps=TimeSteps)
    Spatial@Movement  <- AddDimNames(array(1, dim=c(1,1,1,1,1)),
                                     c("Sim",'FromArea', 'ToArea','Age', 'TimeStep'),
                                     TimeSteps=TimeSteps)
    return(SetDigest(Spatial, argList))
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
      Spatial@RelativeSize <- apply(Spatial@UnfishedDist, c('Sim', 'Area'), mean)   
    } else {
      cli::cli_abort('If `Spatial@RelativeSize` is character, it can only be "EqualDensity"')
    }
  } else {
    cli::cli_alert_warning('`RelativeSize` is not specified. Assuming all areas are equal size')
    Spatial@RelativeSize <- matrix(1/nareas, 1, nareas)
  }
  Spatial@RelativeSize <- AddDimNames(Spatial@RelativeSize, c('Sim', 'Area'))
  
  
  SetDigest(Spatial, argList)
  
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
  
  if (CheckDigest(Depletion, argList) | EmptyObject(Depletion))
    return(Depletion)
  
  SetSeed(Depletion, seed)
  
  Depletion@Initial <- PopulateInitial(Depletion@Initial, nsim)
  Depletion@Final <- PopulateInitial(Depletion@Final, nsim, 'Final')
  
  validReference <- c('B0', 'BMSY', 'SB0', 'SBMSY')
  if (!Depletion@Reference %in% validReference)
    cli::cli_abort(c('Invalid value for `Reference`',
                     "x"="Currently {.val {Depletion@Reference}}. Must be one of: {.val {validReference}}")
    )
  SetDigest(Depletion, argList)
}
