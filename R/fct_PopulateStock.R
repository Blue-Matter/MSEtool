

PopulateStock <- function(Stock, 
                          nYear,
                          pYear,
                          CurrentYear=NULL,
                          nSim=NULL,
                          Seasons=1,
                          ALK=TRUE, 
                          AWK=TRUE, 
                          seed=102, 
                          silent=FALSE) {
  
  argList <- list(seed, ALK, AWK)
  if (CheckDigest(Stock, argList) | EmptyObject(Stock))
    return(Stock)

  if (is.null(CurrentYear))
    CurrentYear <- format(Sys.Date(), "%Y")
  Stock@nYear <- nYear
  Stock@pYear <- pYear
  Stock@CurrentYear <- CurrentYear
  Stock@nSim <- nSim
  
  SetSeed(seed)
  
  Stock@Years <- CalcYears(nYear=Stock@nYear, 
                                   pYear=Stock@pYear, 
                                   CurrentYear=Stock@CurrentYear, 
                                   Seasons=Stock@Seasons )
  # Require ALK and/or AWK?
  # ALK <- RequireALK(Stock)
  # AWK <- RequireAWK(Stock)
  
  Stock@Ages@Classes <- CalcAgeClasses(Stock@Ages)
  
  Stock@Length <- PopulateLength(Length=Stock@Length,
                                 Ages=Stock@Ages,
                                 Years=Years(Stock),
                                 nSim=nSim(Stock),
                                 ASK=ALK,
                                 seed+1,
                                 silent)
  
  Stock@Weight <- PopulateWeight(Weight=Stock@Weight,
                                 Ages=Stock@Ages,
                                 Length=Stock@Length,
                                 Years=Years(Stock),
                                 nSim=nSim(Stock),
                                 ASK=AWK,
                                 seed+2,
                                 silent)
  
  Stock@NaturalMortality <- PopulateNaturalMortality(Stock@NaturalMortality,
                                                     Ages=Stock@Ages,
                                                     Length=Stock@Length,
                                                     Years=Years(Stock),
                                                     nSim=nSim(Stock),
                                                     seed+3,
                                                     silent)
  
  Stock@Maturity <- PopulateMaturity(Maturity=Stock@Maturity,
                                     Ages=Stock@Ages,
                                     Length=Stock@Length,
                                     Weight=Stock@Weight,
                                     Years=Years(Stock),
                                     nSim=nSim(Stock),
                                     seed+4,
                                     silent)
  
  Stock@Fecundity <- PopulateFecundity(Stock@Fecundity,
                                       Ages=Stock@Ages,
                                       Length=Stock@Length,
                                       Weight=Stock@Weight,
                                       Maturity=Stock@Maturity,
                                       Years=Years(Stock),
                                       nSim=nSim(Stock),
                                       seed+5,
                                       silent)
  
  Stock@SRR <- PopulateSRR(SRR=Stock@SRR,
                           Ages=Stock@Ages,
                           CurrentYear=Stock@CurrentYear,
                           Years=Stock@Years,
                           nSim=Stock@nSim,
                           seed+6,
                           silent)
  
  Stock@Spatial <- PopulateSpatial(Spatial=Stock@Spatial,
                                   Ages=Stock@Ages,
                                   Years=Years(Stock),
                                   nSim=Stock@nSim,
                                   seed+7,
                                   silent)
  
  Stock@Depletion <- PopulateDepletion(Stock@Depletion,
                                       nSim=Stock@nSim,
                                       seed+8)
  
  SetDigest(Stock, argList)
}

DefaultAges <- function(Ages=NULL) {
  if (!is.null(Ages))
    return(Ages)
  Ages(MaxAge=10)
}

DefaultYears <- function(Years=NULL) {
  if (!is.null(Years))
    return(Years)
  
  CurrentYear <- format(Sys.Date(), "%Y") |>
    as.numeric()
  
  seq(1950, CurrentYear+5) 
}

PopulateLength <- function(Length,
                           Ages=NULL,
                           Years=NULL,
                           nSim=NULL,
                           ASK=TRUE,
                           seed=NULL,
                           silent=FALSE) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, nSim, Years, ASK, seed)
  
  if (CheckDigest(Length, argList) | EmptyObject(Length))
    return(Length)
  
  SetSeed(seed)
  
  Length@Pars <- StructurePars(Pars=Length@Pars, nSim, Years)
  Length@Model <- FindModel(Length)
  Length <- PopulateMeanAtAge(Length, Ages, Years)
  Length <- PopulateRandom(Length)
  Length@CVatAge <- StructureCV(Length@CVatAge, nSim)
  dd <- dim(Length@CVatAge)
  if (is.null(dimnames(Length@CVatAge)))
    dimnames(Length@CVatAge) <- list(Sim=(1:nSim)[1:dd[1]],
                                     Age=Ages@Classes[1:dd[2]],
                                     Year=Years[1:dd[3]])
  
  if (is.null(Length@CVatAge))
    ASK <- FALSE
  
  if (!is.null(Length@CVatAge))
    Length <- PopulateClasses(Length)
  
  if (ASK && !is.null(Length@Classes)) {
    Length <- PopulateASK(Length, Ages, Years, silent=silent)
  }
  
  # Length <- AddMeanAtAgeAttributes(Length, Years, Ages)
  SetDigest(Length, argList)
}


PopulateWeight <- function(Weight,
                           Ages=NULL,
                           Length=NULL,
                           Years=NULL,
                           nSim=NULL,
                           ASK=FALSE,
                           seed=NULL,
                           silent=FALSE,
                           CalcAtLength=FALSE) {
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, Length, nSim, Years, ASK,
                  CalcAtLength, seed)
  
  if (CheckDigest(Weight, argList) | EmptyObject(Weight))
    return(Weight)
  
  SetSeed(seed)
  
  Weight@Pars <- StructurePars(Pars=Weight@Pars, nSim, Years)
  Weight@Model <- FindModel(Weight)
  
  ModelClass <- getModelClass(Weight@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Weight@Model))) {
      CheckRequiredObject(Length, 'length', 'Length')
      # chk <- Check(Length, silent=TRUE)
      # if (!chk@populated) {
      CheckRequiredObject(Ages, 'ages', 'Ages')
      Length <- PopulateLength(Length, Ages, Years, nSim, seed, ASK=TRUE, silent)
      # }
      Weight <- PopulateMeanAtLength(Weight, Length, Years, Ages,
                                     nSim,  seed, silent)
    } else {
      Weight <- PopulateMeanAtAge(Weight, Ages, Years, Length)
    }
  }
  
  Weight <- MeanAtLength2MeanAtAge(Weight, Length)
  
  if (CalcAtLength) {
    Weight <- MeanAtAge2MeanAtLength(Weight, Length)
  }
  
  Weight <- PopulateRandom(Weight)
  Weight@CVatAge <- StructureCV(Weight@CVatAge, nSim)
  dd <- dim(Weight@CVatAge)
  if (!is.null(dd))
    dimnames(Weight@CVatAge) <- list(Sim=(1:nSim)[1:dd[1]],
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
                                     Years=NULL,
                                     nSim=NULL,
                                     seed=NULL,
                                     silent=FALSE,
                                     CalcAtLength=FALSE) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed)
  if (CheckDigest( NaturalMortality, argList) | EmptyObject(NaturalMortality))
    return(NaturalMortality)
  
  SetSeed(seed)
  
  NaturalMortality@Pars <- StructurePars(Pars=NaturalMortality@Pars, nSim, Years)
  NaturalMortality@Model <- FindModel(NaturalMortality)
  
  ModelClass <- getModelClass(NaturalMortality@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(NaturalMortality@Model))) {
      NaturalMortality <- PopulateMeanAtLength(NaturalMortality, Length,
                                               Years, Ages, nSim,
                                               seed,silent)
    } else {
      NaturalMortality <- PopulateMeanAtAge(NaturalMortality, Ages, Years)
    }
  }
  
  NaturalMortality <- MeanAtLength2MeanAtAge(NaturalMortality, Length)
  if (CalcAtLength)
    NaturalMortality <- MeanAtAge2MeanAtLength(NaturalMortality, Length, 
                                               Ages, nSim, Years, seed,
                                               silent)
  
  NaturalMortality <- PopulateRandom(NaturalMortality)
  
  # NaturalMortality <- AddMeanAtAgeAttributes(NaturalMortality, Years, Ages)
  SetDigest(NaturalMortality, argList)
}

PopulateMaturity <- function(Maturity,
                             Ages=NULL,
                             Length=NULL,
                             Weight=NULL,
                             Years=NULL,
                             nSim=NULL,
                             seed=NULL,
                             silent=FALSE,
                             CalcAtLength=FALSE) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed)
  
  if (CheckDigest(Maturity, argList) | EmptyObject(Maturity))
    return(Maturity)
  
  SetSeed(seed)

  Maturity@Pars <- StructurePars(Pars=Maturity@Pars, nSim, Years)
  Maturity@Model <- FindModel(Maturity)
  ModelClass <- getModelClass(Maturity@Model)
  
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Maturity@Model))) {
      CheckRequiredObject(Length, 'length', 'Length')
      CheckRequiredObject(Ages, 'ages', 'Ages')
      Length <- Populate(Length, Ages, Years, nSim, seed, ASK=TRUE, silent)
      
      Maturity <- PopulateMeanAtLength(Maturity, Length, Years, Ages, nSim,
                                       seed, silent)
    } else if (grepl('at-Weight',getModelClass(Maturity@Model))) {
      Maturity <- PopulateMeanAtWeight(Maturity, Weight, Years, Ages, nSim,
                                       seed, silent)
    } else {
      Maturity <- PopulateMeanAtAge(Maturity, Ages, Years)
    }
  }
  
  Maturity <- MeanAtLength2MeanAtAge(Maturity, Length)
  Maturity <- MeanAtWeight2MeanAtAge(Maturity, Weight)
  
  if (CalcAtLength)
    Maturity <- MeanAtAge2MeanAtLength(Maturity, Length, Ages, nSim, 
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
                              Years=NULL,
                              nSim=NULL,
                              seed=NULL,
                              silent=FALSE,
                              CalcAtLength=FALSE) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, Length, Weight, Maturity, nSim, Years, CalcAtLength, seed)
  
  if (EmptyObject(Fecundity)) {
    
    CheckRequiredObject(Ages, 'ages', 'Ages')
    CheckRequiredObject(Weight, 'weight', 'Weight')
    CheckRequiredObject(Length, 'length', 'Length')
    CheckRequiredObject(Maturity, 'maturity', 'Maturity')
    
    Weight <- PopulateWeight(Weight, 
                             Ages, 
                             Length, 
                             Years, 
                             nSim,
                             seed, 
                             ASK=FALSE)
    Maturity <- PopulateMaturity(Maturity,
                                 Ages, 
                                 Length,
                                 Weight,
                                 Years, 
                                 nSim,
                                 seed)
    
    Fecundity@MeanAtAge <- ArrayMultiply(array1=Weight@MeanAtAge,
                                         array2=Maturity@MeanAtAge)
    
    # object@MeanAtAge <- Weight@MeanAtAge # egg production is fecundity x maturity - calculated internally
    # fecundity is the egg production of a MATURE individual 
    
    return(SetDigest(Fecundity, argList))
  }
  
  if (CheckDigest(Fecundity, argList))
    return(Fecundity)
  
  SetSeed(seed)
  
  Fecundity@Pars <- StructurePars(Pars=Fecundity@Pars, nSim, Years)
  Fecundity@Model <- FindModel(Fecundity)
  
  if (is.null(Fecundity@Model)| all(is.na(Fecundity@Pars))) {
    if (is.null(Fecundity@MeanAtAge)) {
      
      CheckRequiredObject(Ages, 'ages', 'Ages')
      CheckRequiredObject(Weight, 'weight', 'Weight')
      CheckRequiredObject(Length, 'length', 'Length')
      CheckRequiredObject(Maturity, 'maturity', 'Maturity')
      
      Weight <- PopulateWeight(Weight, 
                               Ages, 
                               Length, 
                               Years, 
                               nSim,
                               seed, 
                               ASK=FALSE)
      
      Maturity <- PopulateMaturity(Maturity,
                                   Ages, 
                                   Length,
                                   Weight,
                                   Years, 
                                   nSim,
                                   seed)
      
      Fecundity@MeanAtAge <- ArrayMultiply(array1=Weight@MeanAtAge,
                                           array2=Maturity@MeanAtAge)
      
      return(SetDigest(Fecundity, argList))
    }
  }
  
  ModelClass <- getModelClass(Fecundity@Model)
  if (!is.null(ModelClass)) {
    if (grepl('at-Length',getModelClass(Fecundity@Model))) {
      Fecundity <- PopulateMeanAtLength(Fecundity, Length, Years, Ages, nSim,
                                        seed, silent)
    } else {
      Fecundity <- PopulateMeanAtAge(Fecundity, Ages, Years)
    }
  }
  
  Fecundity <- MeanAtLength2MeanAtAge(Fecundity, Length)
  
  if (CalcAtLength) {
    Fecundity <- MeanAtAge2MeanAtLength(Fecundity, Length)
  }
    
  
  Fecundity@MeanAtAge <- AddDimNames(Fecundity@MeanAtAge, Years=Years, Ages=Ages@Classes)
  
  SetDigest(Fecundity, argList)
}

PopulateSRR <- function(SRR,
                        Ages=NULL,
                        CurrentYear=NULL,
                        Years=NULL,
                        nSim=NULL,
                        seed=NULL,
                        silent=FALSE) {
  
  Ages <- DefaultAges(Ages)
  if (is.null(CurrentYear))
    CurrentYear <- format(Sys.Date(), "%Y") |>
      as.numeric()
  
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, CurrentYear, Years, nSim, seed)
  
  MaxAge <- Ages@MaxAge
  if (is.null(MaxAge))
    cli::cli_abort('`MaxAge` cannot be NULL')
  
  if (is.null(CurrentYear))
    cli::cli_abort('`CurrentYear` cannot be NULL')
  
  if (is.null(Years))
    cli::cli_abort('`Years` cannot be NULL')
  
  if (is.null(nSim)) {
    cli::cli_alert_info('`nSim` not specified. Assuming `nSim=1` and no recruitment process error.')
    nSim <-1
  }
  
  tYears <- floor(Years)
  HistTS <- Years[tYears<=CurrentYear]
  ProjTS <- Years[tYears>CurrentYear]
  nHistTS <- length(HistTS)
  nProjTS <- length(ProjTS)
  
  if (CheckDigest(SRR, argList) | EmptyObject(SRR))
    return(SRR)
  
  SetSeed(seed)
  
  SRR@Pars <- StructurePars(Pars=SRR@Pars, nSim, Years)
  SRR@Model <- FindModel(SRR)
  
  pars <- StructurePars(list(SRR@R0, SRR@SD, SRR@AC), nSim, Years)
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
                                                 nSim=nSim,
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









PopulateInitial <- function(Initial, nSim=NA, name='Initial') {
  if (length(Initial)<1)
    return(Initial)
  if (all(is.na(Initial)))
    return(Initial)
  
  if (length(Initial)==1) 
    nSim <- 1
  
  if (length(Initial)==2) {
    # sample from uniform distribution
    if (is.na(nSim))
      cli::cli_abort(c('`nSim` required to generate stochastic values',
                       'i'='Provide number of simulations to `nSim` argument')
      )
    Initial <- sort(Initial)
    Initial <- stats::runif(nSim, Initial[1], Initial[2])
  }
  nSim <- length(Initial)
  array(Initial, dim=nSim, dimnames = list(Sim=1:nSim))
}

PopulateDepletion <- function(Depletion,  
                              nSim=NULL,
                              seed=NULL,
                              silent=FALSE) {
  argList <- list(nSim, seed)
  
  if (CheckDigest(Depletion, argList) | EmptyObject(Depletion))
    return(Depletion)
  
  SetSeed(seed)
  
  Depletion@Initial <- PopulateInitial(Depletion@Initial, nSim)
  Depletion@Final <- PopulateInitial(Depletion@Final, nSim, 'Final')
  
  validReference <- c('B0', 'BMSY', 'SB0', 'SBMSY')
  if (!Depletion@Reference %in% validReference)
    cli::cli_abort(c('Invalid value for `Reference`',
                     "x"="Currently {.val {Depletion@Reference}}. Must be one of: {.val {validReference}}")
    )
  SetDigest(Depletion, argList)
}
