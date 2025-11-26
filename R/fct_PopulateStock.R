
#' @describeIn Populate Populate a [Stock()] object
#' @export
PopulateStock <- function(Stock, 
                          ALK=TRUE, 
                          AWK=TRUE, 
                          seed=NULL, 
                          silent=FALSE) {
  
  argList <- list(seed, ALK, AWK)
  if (CheckDigest(Stock, argList) | EmptyObject(Stock))
    return(Stock)
  
  SetSeed(Stock, seed)
  
  Stock@Years <- CalcYears(nYear=Stock@nYear, 
                                   pYear=Stock@pYear, 
                                   CurrentYear=Stock@CurrentYear, 
                                   TSperYear=Stock@TSperYear )
  # Require ALK and/or AWK?
  # ALK <- RequireALK(Stock)
  # AWK <- RequireAWK(Stock)
  
  Stock@Ages@Classes <- CalcAgeClasses(Stock@Ages)
  
  Stock@Length <- PopulateLength(Length=Stock@Length,
                                 Ages=Stock@Ages,
                                 nsim=nSim(Stock),
                                 Years=Years(Stock),
                                 ASK=ALK,
                                 seed=seed,
                                 silent=silent)
  
  Stock@Weight <- PopulateWeight(Weight=Stock@Weight,
                                 Ages=Stock@Ages,
                                 Length=Stock@Length,
                                 nSim(Stock),
                                 Years=Years(Stock),
                                 ASK=AWK,
                                 seed=seed,
                                 silent=silent)
  
  Stock@NaturalMortality <- PopulateNaturalMortality(Stock@NaturalMortality,
                                                     Ages=Stock@Ages,
                                                     Length=Stock@Length,
                                                     nsim=nSim(Stock),
                                                     Years=Years(Stock),
                                                     seed=seed,
                                                     silent=silent)
  
  Stock@Maturity <- PopulateMaturity(Maturity=Stock@Maturity,
                                     Ages=Stock@Ages,
                                     Length=Stock@Length,
                                     Weight=Stock@Weight,
                                     nsim=nSim(Stock),
                                     Years=Years(Stock),
                                     seed=seed,
                                     silent=silent)
  
  Stock@Fecundity <- PopulateFecundity(Stock@Fecundity,
                                       Ages=Stock@Ages,
                                       Length=Stock@Length,
                                       Weight=Stock@Weight,
                                       Maturity=Stock@Maturity,
                                       nsim=nSim(Stock),
                                       Years=Years(Stock),
                                       seed=seed,
                                       silent=silent)
  
  Stock@SRR <- PopulateSRR(SRR=Stock@SRR,
                           Ages=Stock@Ages,
                           CurrentYear=Stock@CurrentYear,
                           Years=Stock@Years,
                           nsim=Stock@nSim,
                           seed=seed,
                           silent=silent)
  
  Stock@Spatial <- PopulateSpatial(Spatial=Stock@Spatial,
                                   Ages=Stock@Ages,
                                   Years=Years(Stock),
                                   nsim=Stock@nSim,
                                   seed=seed,
                                   silent=silent)
  
  Stock@Depletion <- PopulateDepletion(Stock@Depletion,
                                       nsim=Stock@nSim,
                                       seed=seed)
  
  SetDigest(Stock, argList)
}


PopulateLength <- function(Length,
                           Ages=NULL,
                           nsim=NULL,
                           Years=NULL,
                           ASK=TRUE,
                           seed=NULL,
                           silent=FALSE) {
  
  # Years <- YearAttributes(Length, Years)
  argList <- list(Ages, nsim, Years, ASK, seed)
  
  if (CheckDigest(Length, argList) | EmptyObject(Length))
    return(Length)
  
  SetSeed(Length, seed)
  
  Length@Pars <- StructurePars(Pars=Length@Pars, nsim, Years)
  Length@Model <- FindModel(Length)
  Length <- PopulateMeanAtAge(Length, Ages, Years)
  Length <- PopulateRandom(Length)
  Length@CVatAge <- StructureCV(Length@CVatAge, nsim)
  dd <- dim(Length@CVatAge)
  dimnames(Length@CVatAge) <- list(Sim=(1:nsim)[1:dd[1]],
                                   Age=Ages@Classes[1:dd[2]],
                                   Year=Years[1:dd[3]])
  
  if (is.null(Length@CVatAge))
    ASK <- FALSE
  
  if (!is.null(Length@CVatAge))
    Length <- PopulateClasses(Length)
  
  if (ASK) {
    Length <- PopulateASK(Length, Ages, Years, silent=silent)
  }
  
  # Length <- AddMeanAtAgeAttributes(Length, Years, Ages)
  SetDigest(Length, argList)
}


PopulateWeight <- function(Weight,
                           Ages=NULL,
                           Length=NULL,
                           nsim=NULL,
                           Years=NULL,
                           ASK=FALSE,
                           CalcAtLength=FALSE,
                           seed=NULL,
                           silent=FALSE) {
  # Years <- YearAttributes(Weight, Years)
  argList <- list(Ages, Length, nsim, Years, ASK,
                  CalcAtLength, seed)
  
  if (CheckDigest(Weight, argList) | EmptyObject(Weight))
    return(Weight)
  
  SetSeed(Weight, seed)
  
  Weight@Pars <- StructurePars(Pars=Weight@Pars, nsim, Years)
  Weight@Model <- FindModel(Weight)
  
  ModelClass <- getModelClass(Weight@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Weight@Model))) {
      CheckRequiredObject(Length, 'length', 'Length')
      # chk <- Check(Length, silent=TRUE)
      # if (!chk@populated) {
      CheckRequiredObject(Ages, 'ages', 'Ages')
      Length <- PopulateLength(Length, Ages, nsim, Years, seed, ASK=TRUE, silent)
      # }
      Weight <- PopulateMeanAtLength(Weight, Length, Years, Ages,
                                     nsim,  seed, silent)
    } else {
      Weight <- PopulateMeanAtAge(Weight, Ages, Years, Length)
    }
  }
  
  Weight <- MeanAtLength2MeanAtAge(Weight, Length, Ages, nsim, Years, seed, silent)
  
  if (CalcAtLength) {
    Weight <- MeanAtAge2MeanAtLength(Weight, Length, Ages, nsim, Years, seed, silent)
  }
  
  Weight <- PopulateRandom(Weight)
  Weight@CVatAge <- StructureCV(Weight@CVatAge, nsim)
  dd <- dim(Weight@CVatAge)
  if (!is.null(dd))
    dimnames(Weight@CVatAge) <- list(Sim=(1:nsim)[1:dd[1]],
                                     Age=Ages@Classes[1:dd[2]],
                                     Year=Years[1:dd[3]])
  if (is.null(Weight@CVatAge))
    ASK <- FALSE
  
  Weight@MeanAtAge <- AddDimNames(Weight@MeanAtAge, Years=Years, Ages=Ages@Classes)
  Weight@CVatAge <- AddDimNames(Weight@CVatAge, Years=Years, Ages=Ages@Classes)
  
  if (ASK) {
    Weight <- PopulateClasses(Weight)
    Weight <- PopulateASK(Weight, Ages, Years, silent, type='Weight')
  }
  
  SetDigest(Weight, argList)
}


PopulateNaturalMortality <- function(NaturalMortality,
                                     Ages=NULL,
                                     Length=NULL,
                                     nsim=NULL,
                                     Years=NULL,
                                     CalcAtLength=FALSE,
                                     seed=NULL,
                                     silent=FALSE) {
  
  # Years <- YearAttributes(NaturalMortality, Years)
  
  argList <- list(Ages, Length, nsim, Years, CalcAtLength, seed)
  if (CheckDigest( NaturalMortality, argList) | EmptyObject(NaturalMortality))
    return(NaturalMortality)
  
  SetSeed(NaturalMortality, seed)
  
  NaturalMortality@Pars <- StructurePars(Pars=NaturalMortality@Pars, nsim, Years)
  NaturalMortality@Model <- FindModel(NaturalMortality)
  
  ModelClass <- getModelClass(NaturalMortality@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(NaturalMortality@Model))) {
      NaturalMortality <- PopulateMeanAtLength(NaturalMortality, Length,
                                               Years, Ages, nsim,
                                               seed,silent)
    } else {
      NaturalMortality <- PopulateMeanAtAge(NaturalMortality, Ages, Years)
    }
  }
  
  NaturalMortality <- MeanAtLength2MeanAtAge(NaturalMortality, Length, Ages,
                                             nsim, Years, seed, silent)
  if (CalcAtLength)
    NaturalMortality <- MeanAtAge2MeanAtLength(NaturalMortality, Length, 
                                               Ages, nsim, Years, seed,
                                               silent)
  
  NaturalMortality <- PopulateRandom(NaturalMortality)
  
  # NaturalMortality <- AddMeanAtAgeAttributes(NaturalMortality, Years, Ages)
  SetDigest(NaturalMortality, argList)
}

PopulateMaturity <- function(Maturity,
                             Ages=NULL,
                             Length=NULL,
                             Weight=NULL,
                             nsim=NULL,
                             Years=NULL,
                             CalcAtLength=FALSE,
                             seed=NULL,
                             silent=FALSE) {
  
  # Years <- YearAttributes(Maturity, Years)
  argList <- list(Ages, Length, nsim, Years, CalcAtLength, seed)
  
  if (CheckDigest(Maturity, argList) | EmptyObject(Maturity))
    return(Maturity)
  
  SetSeed(Maturity, seed)

  Maturity@Pars <- StructurePars(Pars=Maturity@Pars, nsim, Years)
  Maturity@Model <- FindModel(Maturity)
  ModelClass <- getModelClass(Maturity@Model)
  
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Maturity@Model))) {
      Maturity <- PopulateMeanAtLength(Maturity, Length, Years, Ages, nsim,
                                       seed, silent)
    } else if (grepl('at-Weight',getModelClass(Maturity@Model))) {
      Maturity <- PopulateMeanAtWeight(Maturity, Weight, Years, Ages, nsim,
                                       seed, silent)
    } else {
      Maturity <- PopulateMeanAtAge(Maturity, Ages, Years)
    }
  }
  
  Maturity <- MeanAtLength2MeanAtAge(Maturity, Length, Ages, nsim, 
                                     Years, seed, silent)
  
  Maturity <- MeanAtWeight2MeanAtAge(Maturity, Weight, Ages, nsim, Years,
                                     seed, silent)
  
  if (CalcAtLength)
    Maturity <- MeanAtAge2MeanAtLength(Maturity, Length, Ages, nsim, 
                                       Years, seed, silent)
  
  # Maturity <- AddMeanAtAgeAttributes(Maturity, Years, Ages)
  
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
  Maturity@MeanAtAge <- AddDimNames(Maturity@MeanAtAge, Years=Years, Ages=Ages@Classes)
  Maturity@Semelparous <- AddDimNames(Maturity@Semelparous, Years=Years, Ages=Ages@Classes)
  
  SetDigest(Maturity, argList)
}

PopulateFecundity <- function(Fecundity,
                              Ages=NULL,
                              Length=NULL,
                              Weight=NULL,
                              Maturity=NULL,
                              nsim=NULL,
                              Years=NULL,
                              CalcAtLength=FALSE,
                              seed=NULL,
                              silent=FALSE) {
  # Years <- YearAttributes(Fecundity, Years)
  argList <- list(Ages, Length, Weight, Maturity, nsim, Years, CalcAtLength, seed)
  
  if (EmptyObject(Fecundity)) {
    
    CheckRequiredObject(Ages, 'ages', 'Ages')
    CheckRequiredObject(Weight, 'weight', 'Weight')
    CheckRequiredObject(Length, 'length', 'Length')
    CheckRequiredObject(Maturity, 'maturity', 'Maturity')
    
    Weight <- PopulateWeight(Weight, Ages, Length, nsim, Years,
                             seed=seed, ASK=FALSE)
    Maturity <- PopulateMaturity(Maturity, Ages, Length, Weight, nsim, Years,
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
  
  Fecundity@Pars <- StructurePars(Pars=Fecundity@Pars, nsim, Years)
  Fecundity@Model <- FindModel(Fecundity)
  
  if (is.null(Fecundity@Model)| all(is.na(Fecundity@Pars))) {
    if (is.null(Fecundity@MeanAtAge)) {
      
      CheckRequiredObject(Ages, 'ages', 'Ages')
      CheckRequiredObject(Weight, 'weight', 'Weight')
      CheckRequiredObject(Length, 'length', 'Length')
      CheckRequiredObject(Maturity, 'maturity', 'Maturity')
      
      Weight <- PopulateWeight(Weight, Ages, Length, Weight, nsim, Years,
                               seed=seed, ASK=FALSE)
      Maturity <- PopulateMaturity(Maturity, Ages, Length, nsim, Years, seed=seed)
      Fecundity@MeanAtAge <- ArrayMultiply(array1=Weight@MeanAtAge,
                                           array2=Maturity@MeanAtAge)
      
      return(SetDigest(Fecundity, argList))
    }
  }
  
  ModelClass <- getModelClass(Fecundity@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Fecundity@Model))) {
      Fecundity <- PopulateMeanAtLength(Fecundity, Length, Years, Ages, nsim,
                                        seed, silent)
    } else {
      Fecundity <- PopulateMeanAtAge(Fecundity, Ages, Years)
    }
  }
  
  Fecundity <- MeanAtLength2MeanAtAge(Fecundity, Length, Ages, nsim, 
                                      Years, seed, silent)
  if (CalcAtLength)
    Fecundity <- MeanAtAge2MeanAtLength(Fecundity, Length, Ages, nsim, 
                                        Years, seed, silent)
  
  Fecundity@MeanAtAge <- AddDimNames(Fecundity@MeanAtAge, Years=Years, Ages=Ages@Classes)
  
  SetDigest(Fecundity, argList)
}

PopulateSRR <- function(SRR,
                        Ages=NULL,
                        CurrentYear=NULL,
                        Years=NULL,
                        nsim=NULL,
                        seed=NULL,
                        silent=FALSE) {
  argList <- list(Ages, CurrentYear, Years, nsim, seed)
  
  MaxAge <- Ages@MaxAge
  if (is.null(MaxAge))
    cli::cli_abort('`MaxAge` cannot be NULL')
  
  if (is.null(CurrentYear))
    cli::cli_abort('`CurrentYear` cannot be NULL')
  
  if (is.null(Years))
    cli::cli_abort('`Years` cannot be NULL')
  
  if (is.null(nsim)) {
    cli::cli_alert_info('`nsim` not specified. Assuming `nsim=1` and no recruitment process error.')
    nsim <-1
  }
  
  tYears <- floor(Years)
  HistTS <- Years[tYears<=CurrentYear]
  ProjTS <- Years[tYears>CurrentYear]
  nHistTS <- length(HistTS)
  nProjTS <- length(ProjTS)
  
  if (CheckDigest(SRR, argList) | EmptyObject(SRR))
    return(SRR)
  
  SetSeed(SRR, seed)
  
  SRR@Pars <- StructurePars(Pars=SRR@Pars, nsim, Years)
  SRR@Model <- FindModel(SRR)
  
  pars <- StructurePars(list(SRR@R0, SRR@SD, SRR@AC), nsim, Years)
  SRR@R0 <- pars[[1]]
  SRR@SD <- pars[[2]][,1, drop=FALSE] # only one time step for now
  SRR@AC <- pars[[3]][,1, drop=FALSE] # only one time step for now
  SRR@AC[!is.finite(SRR@AC)] <- 0
  SRR@SD[SRR@SD==0] <- 1E-6 # for reproducibility in rnorm
  

  EmptyObjects <- c(EmptyObject(SRR@RecDevInit),
                    EmptyObject(SRR@RecDevHist),
                    EmptyObject(SRR@RecDevProj))
  
  if (all(!EmptyObjects)) {
    
    dd <- dim(SRR@RecDevInit)
    dimnames(SRR@RecDevInit) <- list(
      Sim=1:dd[1],
      Age=Ages@Classes[-1]
    )
    
    dd <- dim(SRR@RecDevHist)
    dimnames(SRR@RecDevHist) <- list(
      Sim=1:dd[1],
      Year=HistTS
    )
    
    dd <- dim(SRR@RecDevProj)
    dimnames(SRR@RecDevProj) <- list(
      Sim=1:dd[1],
      Year=ProjTS
    )
    return( SetDigest(SRR, argList))
  }
  
  RecDeviations <- GenerateRecruitmentDeviations(SD=SRR@SD,
                                                 AC=SRR@AC,
                                                 TruncSD=SRR@TruncSD,
                                                 Ages,
                                                 HistTS,
                                                 ProjTS,
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
    Year=HistTS
  )
  
  SRR@RecDevProj <- RecDeviations$RecDevProj
  dimnames(SRR@RecDevProj) <- list(
    Sim=1:nrow(SRR@RecDevProj),
    Year=ProjTS
  )
  SetDigest(SRR, argList)
}

PopulateSpatial <- function(Spatial,
                            Ages=NULL,
                            Years=NULL,
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
  DimNames <- c('Sim', 'Area', 'Age', 'Year')
  if (EmptyObject(Spatial)) {
    Spatial@RelativeSize <- AddDimNames(array(1, dim=c(1,1)), 
                                        DimNames[1:2], Years=Years)
    Spatial@ProbStaying <- AddDimNames(array(1, dim=c(1,1,1,1)), 
                                       DimNames, Years=Years)
    Spatial@FracOther <- AddDimNames(array(1, dim=c(1,1,1,1,1)),
                                     c("Sim",'FromArea', 'ToArea','Age', 'Year'), 
                                     Years=Years)
    Spatial@UnfishedDist  <- AddDimNames(array(1, dim=c(1,1,1,1)),
                                         DimNames, Years=Years)
    Spatial@Movement  <- AddDimNames(array(1, dim=c(1,1,1,1,1)),
                                     c("Sim",'FromArea', 'ToArea','Age', 'Year'),
                                     Years=Years)
    return(SetDigest(Spatial, argList))
  }
  
  
  if (is.null(Spatial@Movement)) {
    Spatial <- CalcMovement(Spatial, Years, nsim, seed, nits, plot, 
                            silent)
  } else {
    dd <- dim(Spatial@Movement)
    ndim <- length(dd)
    dnames <- dimnames(Spatial@Movement)
    Names <- names(dnames)
    
    if (!ndim%in% c(3,5)) 
      cli::cli_abort(c('x'="Incorrect dimensions on Spatial@Movemement",
                       'i'="`Spatial@Movement` should either have dimensions:",
                       '*'="{.val {c('Sim', 'FromArea', 'ToArea')}} OR",
                       '*'= "{.val {c('Sim', 'FromArea', 'ToArea', 'Age', 'Year')}}")
      )
    
    if (ndim==3) {
      Spatial@Movement <- AddAgeYearDimensions(Spatial@Movement, outdim=5)
    }
    
    if (dd[2] != dd[3])
      cli::cli_abort(c('x'="Incorrect dimensions on Spatial@Movemement",
                       'i'="`Spatial@Movement@FromArea` should be same length as Spatial@Movement@ToArea")
      )
    
    nArea <- dd[2]
    
    if (is.null(Names)) 
      names(dnames) <- c('Sim', 'FromArea', 'ToArea', 'Age', 'Year')
    
    if (!all(dnames$Sim %in% 1:nsim)) {
      if (length(dnames$Sim)==length(1:nsim)) {
        dnames$Sim <- 1:nsim
      } else {
        cli::cli_abort(c('x'="Incorrect dimension names for {.val Sim} in {.val Spatial@Movemement}",
                         'i'="Currently: {.val {dnames$Sim}}",
                         'i'="Values should match those in {.val 1:OM@nSim}: {.val {1:nsim}}")
        )
      }
    }
    
    if (!all(dnames$FromArea %in% 1:nArea)) {
      dnames$FromArea <- 1:nArea
    }
    
    if (!all(dnames$ToArea %in% 1:nArea)) {
      dnames$ToArea <- 1:nArea
    }
    
    if (!all(dnames$Age %in% Ages@Classes)) {
      if (length(dnames$Age)==length(Ages@Classes)) {
        dnames$Age <- Ages@Classes
      } else if (length(dnames$Age)==1) {
        dnames$Age <- Ages@Classes[1]
      } else {
        cli::cli_abort(c('x'="Incorrect dimension names for {.val Age} in {.val Spatial@Movemement}",
                         'i'="Currently: {.val {dnames$Age}}",
                         'i'="Values should match those in {.val Ages@Classes}: {.val {Ages@Classes}}")
        )
      }
    }
    
    
    if (!all(dnames$Year %in% Years)) {
     if (length(dnames$Year)==length(Years)) {
       dnames$Year <- Years
     } else if (length(dnames$Year)==1) {
       dnames$Year <- Years[1]
     } else {
       cli::cli_abort(c('x'="Incorrect dimension names for {.val Year} in {.val Spatial@Movemement}",
                        'i'="Currently: {.val {dnames$Year}}",
                        'i'="Values should match those in {.val OM@Years}: {.val {OM@Years}}")
       )
     }
    }
    dimnames(Spatial@Movement) <- dnames
    
    # Calc Unfished Dist
    nAreaUnfished <- dim(Spatial@UnfishedDist)[2] 
    if (nAreaUnfished!=nArea)
      Spatial <- CalcUnfishedDist(Spatial, Years)
    
    # Calc Relative Size 
    
  }
  
  
  if (is.null(Spatial@UnfishedDist)) {
    Spatial <- CalcUnfishedDist(Spatial, Years)
  } 
  
  
  if (is.null(Spatial@UnfishedDist))
    cli::cli_abort('`UnfishedDist` must be populated for `Spatial` objects')
  
  # if (is.null(object@ProbStaying))
  #   cli::cli_abort('`ProbStaying` must be populated for `Spatial` objects')
  Spatial <- CalculateRelativeSize(Spatial, nsim)

  
  
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
