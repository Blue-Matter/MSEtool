#' @rdname PopulateOM
#' @export
PopulateFleet <- function(Fleet,
                          Stock,
                          seed = 103,
                          silent = FALSE,
                          force=FALSE) {
  Ages <- Stock@Ages
  Length <- Stock@Length
  Weight <- Stock@Weight
  Maturity <- Stock@Maturity
  RelativeSize <- Stock@Spatial@RelativeSize

  Fleet@CurrentYear <- Stock@CurrentYear
  Fleet@nSim <- Stock@nSim
  Fleet@Years <- Stock@Years
  Fleet@nYear <- Stock@nYear
  Fleet@pYear <- Stock@pYear
  Fleet@Seasons <- Stock@Seasons

  Fleet@Years <- CalcYears(
    nYear = Stock@nYear,
    pYear = Stock@pYear,
    CurrentYear = Stock@CurrentYear,
    Seasons = Stock@Seasons
  )

  nSim <- Fleet@nSim
  Years <- Fleet@Years
  HistYears <- Years(Fleet, "Historical")
  ProjYears <- Years[!Years %in% HistYears]
  nArea <- ncol(RelativeSize)

  argList <- list(Ages, Length, Weight, RelativeSize, nSim, Years, seed)
  
  if (EmptyObject(Fleet)) {
    return(Fleet)
  }
  
  if (CheckDigest(Fleet, argList) & !force) {
    return(Fleet)
  }

  SetSeed(seed)

  Fleet@Effort <- PopulateEffort(
    Effort = Fleet@Effort,
    HistYears,
    nArea,
    nSim,
    seed
  )

  Fleet@Catchability <- PopulateCatchability(
    Catchability=Fleet@Catchability,
    RelativeSize,
    nSim,
    HistYears,
    ProjYears,
    seed,
    silent
  )

  Fleet@Selectivity <- PopulateSelectivity(
    Selectivity = Fleet@Selectivity,
    Ages,
    Length,
    Weight,
    Maturity,
    nSim,
    Years,
    nArea,
    CalcAtLength = TRUE,
    seed,
    silent = silent
  )

  Fleet@Retention <- PopulateRetention(
    Retention = Fleet@Retention,
    Ages,
    Length,
    Weight,
    Maturity,
    nSim,
    Years,
    nArea,
    CalcAtLength = TRUE,
    seed,
    silent = silent,
    force=force
  )

  Fleet@DiscardMortality <- PopulateDiscardMortality(
    DiscardMortality = Fleet@DiscardMortality,
    Ages,
    Length,
    nSim,
    Years,
    nArea,
    CalcAtLength = TRUE,
    seed = seed,
    silent
  )


  Fleet@Closure <- PopulateClosure(
    Closure = Fleet@Closure,
    nArea,
    nSim,
    Years,
    silent
  )


  if (all(is.na(Fleet@WeightFleet))) {
    Fleet@WeightFleet <- Weight@MeanAtAge
  } else {
    Fleet@WeightFleet
    # stop('need to add populate for Fleet@WeightFleet')
  }


  SetDigest(Fleet, argList)
}


PopulateEffort <- function(Effort, HistYears, nArea = 1, nSim = 5, seed = NULL) {
  SetSeed(seed)

  if (is.null(Effort@Effort)) {
    return(Effort)
  }

  if (inherits(Effort@Effort, "data.frame")) {
    Effort@Effort <- GenerateHistoricalEffort(Effort@Effort, nSim, HistYears)
    Effort@Units <- 'unitless'
  }

  dd <- dim(Effort@Effort)
  if (dd[2] != length(HistYears)) {
    cli::cli_abort("`ncol(Effort@Effort)` is not equal to `length(HistYears)`")
  }
  dimnames(Effort@Effort) <- list(
    Sim = 1:nrow(Effort@Effort),
    Year = HistYears
  )

  Effort@Distribution <- PopulateDistribution(
    Distribution = Effort@Distribution,
    nSim,
    HistYears,
    nArea
  )
  
  Effort@Targeting <- PopulateTargeting(Effort@Targeting, nSim, HistYears)
  
  Effort
}


PopulateTargeting <- function(Targeting, nSim, Years) {
  if (is.null(Targeting)) {
    return(
      array(0.8, c(1,1), 
            dimnames = list(Sim=1, Year=Years[1]))  
    )
  }
  
  if (is.array(Targeting)) {
    dd <- dim(Targeting)
    dnames <- dimnames(Targeting)
    if (length(dd)>2) {
      cli::cli_abort("`Targeting` must be numeric or an array with dimensions: Sim x Year")
    }
      
    if ((dd[2]!=1 | dd[2]!=length(Years)) & is.null(dnames[['Year']])) {
      cli::cli_abort("Year dimension of `Targeting`array must be length 1, length `Years(OM)` or have named dimensions.")
    }
    
    if (is.null(dnames)) {
      dimnames(Targeting) <- list(
        Sim=1:dd[1],
        Years=Years[1:dd[2]]
      )
    }
    
  }
  Targeting
}

PopulateDistribution <- function(Distribution,
                                 nSim = 5,
                                 HistYears = NULL,
                                 nArea = NULL) {
  if (is.null(Distribution)) {
    Distribution <- array(tiny,
      dim = c(1, 1, nArea),
      dimnames = list(
        Sim = 1,
        Year = HistYears[1],
        Area = 1:nArea
      )
    )
  } else {
    CheckClass(Distribution, c("array", "matrix"))
    dd <- dim(Distribution)
    # TODO check dimensions
    # TODO add dimnames if needed
  }

  Distribution
}


PopulateCatchability <- function(Catchability,
                                 RelativeSize,
                                 nSim = 5,
                                 HistYears = NULL,
                                 ProjYears = NULL,
                                 seed = NULL,
                                 silent = FALSE) {

  pYears <- length(ProjYears)
  Years <- c(HistYears, ProjYears)
  
  if (is.null(Catchability@Efficiency)) {
    # Catchability not provided  - set all values to 1
    Catchability@Efficiency <- array(1,
                                dim = c(nSim, length(Years)),
                                dimnames = list(
                                  Sim = 1:nSim,
                                  Year = Years
                                )
    )
  }

  # Check Catchability@Efficiency dimensions 
  dd <- dim(Catchability@Efficiency)
  if (dd[1] != nSim && dd[1] != 1) {
      cli::cli_abort(c(
        "x" = "Incorrect number of rows in matrix: `Fleet |> Catchability() |> Efficiency()`",
        "i" = "Must have either {.val {1}} row or `nSim` ({.val {nSim}}) rows. "
      ))
  }

  # Add Dimension Names if Needed  
  if (is.null(dimnames(Catchability@Efficiency))) {
    dimnames(Catchability@Efficiency) <- list(
      Sim = 1:nrow(Catchability@Efficiency),
      Year = Years[1:ncol(Catchability@Efficiency)]
    )
  }

  # Extend q values for all historical years 
  Catchability@Efficiency <- ExtendYears(Catchability@Efficiency, HistYears)
  

  # Apply qInc and qCV if applicable - only really for backwards compatibility
  if (!is.null(Catchability@qInc)) {
    qIncs <- StructurePars_(Catchability@qInc, nSim, Years)[, 1]
    qIncs <- sapply(qIncs, function(x) {
      (1 + x / 100)^(1:pYears)
    }) |> t()

    dimnames(qIncs) <- list(
      Sim = 1:nSim,
      Year = ProjYears
    )

    qfuture <- ArrayMultiply(SubsetYear(Catchability@Efficiency, ProjYears), qIncs)
    ArrayFill(Catchability@Efficiency) <- qfuture
    Catchability@qInc <- qIncs
  }

  if (!is.null(Catchability@qCV)) {
    qCVs <- StructurePars_(Catchability@qCV, nSim, Years)[, 1]
    Catchability@qCV <- qCVs

    qmu <- -0.5 * qCVs^2
    qvar <- array(exp(rnorm(pYears * nSim, rep(qmu, pYears), rep(qCVs, pYears))), c(nSim, pYears),
      dimnames = list(
        Sim = 1:nSim,
        Year = ProjYears
      )
    )

    qfuture <- ArrayMultiply(SubsetYear(Catchability@Efficiency, ProjYears), qvar)
    if (!all(qfuture == 1)) {
      ArrayFill(Catchability@Efficiency) <- qfuture
    }
  }

  Catchability
}


FindL50_vec <- function(prob_vec) {
  classes <- names(prob_vec) |> as.numeric()
  LinInterp(prob_vec, y=classes, 0.5)
}

FindL50 <- function(Maturity) {
  if (!is.null(Maturity@Pars$L50)) {
    return(Maturity@Pars$L50)
  }
  
  MaturityAtLength <- Maturity@MeanAtLength
  if (is.null(MaturityAtLength)) {
    cli::cli_abort("Values required for `Maturity@MeanAtLength` if `Selectivity@isRel == TRUE`")
  }
  
  apply(Maturity@MeanAtLength, c('Sim', 'Year'), FindL50_vec)
}

PopulateSelectivity <- function(Selectivity,
                                Ages = NULL,
                                Length = NULL,
                                Weight = NULL,
                                Maturity = NULL,
                                nSim = 5,
                                Years = NULL,
                                nArea = 1,
                                CalcAtLength = TRUE,
                                seed = NULL,
                                silent = FALSE,
                                CheckMaxValue = TRUE) {
  argList <- list(Ages, Length, Weight, Years, nArea, nSim, CalcAtLength, seed)
  
  if (CheckDigest(Selectivity, argList)) {
    return(Selectivity)
  }
  
  SetSeed(seed)
  
  Selectivity@Pars <- StructurePars(Pars = Selectivity@Pars, nSim, Years, nArea)
  Selectivity@Model <- FindModel(Selectivity)
  ModelClass <- getModelClass(Selectivity@Model)
  
  if (!is.null(ModelClass)) {
    # Model & Parameters exist
    
    if (Selectivity@isRel) {
      CheckRequiredObject(Maturity, 'maturity', 'Maturity')
      L50 <- FindL50(Maturity)
      # TODO - extend for other parameters? 
      Selectivity@Pars$L5 <- ArrayMultiply(Selectivity@Pars$L5, L50)
      Selectivity@Pars$LFS <- ArrayMultiply(Selectivity@Pars$LFS, L50)
    }
    
    if (grepl("at-Length", getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtLength(
        Selectivity,
        Length,
        Years,
        Ages,
        nSim,
        seed,
        silent
      )
    } else if (grepl("at-Weight", getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtWeight(Selectivity, Weight, Years, Ages, nSim, seed, silent)
    } else if (grepl("at-Age", getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtAge(Selectivity, Ages, Years, Length)
    }
  }
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, max1 = TRUE)
  Selectivity <- MeanAtWeight2MeanAtAge(Selectivity, Weight, max1 = TRUE)
  
  if (CalcAtLength) {
    Selectivity <- MeanAtAge2MeanAtLength(Selectivity, Length, replace = FALSE)
  }
  
  
  if (is.null(Selectivity@MeanAtAge)) {
    cli::cli_abort(" {.var Selectivity} requires values for either `Model` & `Pars` or `MeanAtAge`")
  }
  
  # Check Selectivity has a max value of 1 across age classes
  if (CheckMaxValue) {
    Selectivity@MeanAtAge <- CheckSelectivityMaximum(Selectivity@MeanAtAge)
  }
  
  # Add Area Dimension 
  Selectivity@MeanAtLength <- AddDimension(Selectivity@MeanAtLength, 'Area')
  Selectivity@MeanAtWeight <- AddDimension(Selectivity@MeanAtWeight, 'Area') 
  Selectivity@MeanAtAge <- AddDimension(Selectivity@MeanAtAge, 'Area')
  
  # Add Dimension Names
  if (is.null(dimnames(Selectivity@MeanAtLength))) {
    dd <- dim(Selectivity@MeanAtLength)
    if (!is.null(dd)) {
      dimnames(Selectivity@MeanAtLength) <- list(
        Sim=1:dd[1],
        Class=Selectivity@Classes,
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(Selectivity@MeanAtWeight))) {
    dd <- dim(Selectivity@MeanAtWeight)
    if (!is.null(dd)) {
      dimnames(Selectivity@MeanAtWeight) <- list(
        Sim=1:dd[1],
        Class=Selectivity@Classes,
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(Selectivity@MeanAtAge))) {
    dd <- dim(Selectivity@MeanAtAge)
    if (!is.null(dd)) {
      dimnames(Selectivity@MeanAtAge) <- list(
        Sim=1:dd[1],
        Age=Ages@Classes[1:dd[2]],
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }
  
  SetDigest(Selectivity, argList)
}

PopulateRetention <- function(Retention,
                              Ages = NULL,
                              Length = NULL,
                              Weight = NULL,
                              Maturity=NULL,
                              nSim = 5,
                              Years = NULL,
                              nArea = 1,
                              CalcAtLength = TRUE,
                              seed = NULL,
                              silent = FALSE,
                              force=FALSE) {
  argList <- list(Ages, Length, Years, nSim, CalcAtLength, seed)

  if (CheckDigest(Retention, argList) & !force) {
    return(Retention)
  }

  # Default at retained
  if (EmptyObject(Retention)) {
    Retention@MeanAtAge <- array(1, dim = c(1, length(Ages@Classes), 1, 1)) 
    dimnames(Retention@MeanAtAge) <- list(
      Sim=1,
      Age=Ages@Classes,
      Year=Years[1],
      Area=1
    )
    
    Retention@Classes <- Length@Classes
    Retention@MeanAtLength <- array(1, dim = c(1, length(Retention@Classes), 1, 1)) 
    dimnames(Retention@MeanAtLength) <- list(
      Sim=1,
      Class=Retention@Classes,
      Year=Years[1],
      Area=1
    )
    
    return(SetDigest(Retention, argList))
  }

  SetSeed(seed)

  Retention@Pars <- StructurePars(Pars = Retention@Pars, nSim, Years)
  Retention@Model <- FindModel(Retention)
  ModelClass <- getModelClass(Retention@Model)

  if (!is.null(ModelClass)) {
    # Model & Parameters exist
    
    
    if (Selectivity@isRel) {
      CheckRequiredObject(Maturity, 'maturity', 'Maturity')
      L50 <- FindL50(Maturity)
      # TODO - extend for other parameters? 
      Retention@Pars$LR5 <- ArrayMultiply(Retention@Pars$LR5, L50)
      Retention@Pars$LFR <- ArrayMultiply(Retention@Pars$LFR, L50)
    }
    
    if (grepl("at-Length", getModelClass(Retention@Model))) {
      Retention <- PopulateMeanAtLength(
        Retention,
        Length,
        Years,
        Ages,
        nSim,
        seed,
        silent
      )
    } else if (grepl("at-Weight", getModelClass(Retention@Model))) {
      Retention <- PopulateMeanAtWeight(Retention, Weight, Years, Ages, nSim, seed, silent)
    } else if (grepl("at-Age", getModelClass(Retention@Model))) {
      Retention <- PopulateMeanAtAge(Retention, Ages, Years, Length)
    }
  }

  Retention <- MeanAtLength2MeanAtAge(Retention, Length, max1 = FALSE)
  Retention <- MeanAtWeight2MeanAtAge(Retention, Weight, max1 = FALSE)

  if (CalcAtLength) {
    Retention <- MeanAtAge2MeanAtLength(Retention, Length, replace = FALSE)
  }
  
  # Add Area Dimension 
  Retention@MeanAtLength <- AddDimension(Retention@MeanAtLength, 'Area')
  Retention@MeanAtWeight <- AddDimension(Retention@MeanAtWeight, 'Area') 
  Retention@MeanAtAge <- AddDimension(Retention@MeanAtAge, 'Area')
  
  # Add Dimension Names
  if (is.null(dimnames(Retention@MeanAtLength))) {
    dd <- dim(Retention@MeanAtLength)
    if (!is.null(dd)) {
      dimnames(Retention@MeanAtLength) <- list(
        Sim=1:dd[1],
        Class=Retention@Classes,
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(Retention@MeanAtWeight))) {
    dd <- dim(Retention@MeanAtWeight)
    if (!is.null(dd)) {
      dimnames(Retention@MeanAtWeight) <- list(
        Sim=1:dd[1],
        Class=Retention@Classes,
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(Retention@MeanAtAge))) {
    dd <- dim(Retention@MeanAtAge)
    if (!is.null(dd)) {
      dimnames(Retention@MeanAtAge) <- list(
        Sim=1:dd[1],
        Age=Ages@Classes[1:dd[2]],
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }

  SetDigest(Retention, argList)
}


PopulateDiscardMortality <- function(DiscardMortality,
                                     Ages = NULL,
                                     Length = NULL,
                                     nSim = 5,
                                     Years = NULL,
                                     nArea = 1,
                                     CalcAtLength = TRUE,
                                     seed = NULL,
                                     silent = FALSE,
                                     force=FALSE) {
  # Years <- YearAttributes(DiscardMortality, Years)
  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed)

  if (CheckDigest(DiscardMortality, argList) & !force) {
    return(DiscardMortality)
  }


  # Default - no discard mortality
  if (EmptyObject(DiscardMortality)) {
    DiscardMortality@MeanAtAge <- array(0, dim = c(1, length(Ages@Classes), 1, 1)) |>
      SetDimNames_SAYR(Age = Ages@Classes, Years = Years)
    
    DiscardMortality@Classes <- Length@Classes
    DiscardMortality@MeanAtLength <- array(0, 
                                           dim = c(1, length(DiscardMortality@Classes), 1, 1)) 
    dimnames(DiscardMortality@MeanAtLength) <- list(
      Sim=1,
      Class=DiscardMortality@Classes,
      Year=Years[1],
      Area=1
    )
    return(SetDigest(DiscardMortality, argList))
  }

  SetSeed(seed)
  DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, Length)

  if (CalcAtLength) {
    DiscardMortality <- MeanAtAge2MeanAtLength(DiscardMortality, Length, replace = FALSE)
  }

  # Add Area Dimension 
  DiscardMortality@MeanAtLength <- AddDimension(DiscardMortality@MeanAtLength, 'Area')
  DiscardMortality@MeanAtAge <- AddDimension(DiscardMortality@MeanAtAge, 'Area')
  
  if (is.null(dimnames(DiscardMortality@MeanAtLength))) {
    dd <- dim(DiscardMortality@MeanAtLength)
    if (!is.null(dd)) {
      dimnames(DiscardMortality@MeanAtLength) <- list(
        Sim=1:dd[1],
        Class=DiscardMortality@Classes,
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(DiscardMortality@MeanAtAge))) {
    dd <- dim(DiscardMortality@MeanAtAge)
    if (!is.null(dd)) {
      dimnames(DiscardMortality@MeanAtAge) <- list(
        Sim=1:dd[1],
        Age=Ages@Classes[1:dd[2]],
        Year=Years[1:dd[3]],
        Area=1:dd[4]
      )
    }
  }
  
  SetDigest(DiscardMortality, argList)
}


PopulateClosure <- function(Closure, nArea, nSim = 5, Years, silent) {
  argList <- list(nArea, Years, nSim)

  if (EmptyObject(Closure)) {
    Closure <- array(1,
      dim = c(1, 1, nArea),
      dimnames = list(
        Sim = 1,
        Year = Years[1],
        Area = 1:nArea
      )
    )
  } else {
    dd <- dim(Closure)
    if (dd[3] != nArea) {
      if (dd[1] == 1 && dd[2] == 1) {
        Closure <- array(1,
          dim = c(1, 1, nArea),
          dimnames = list(
            Sim = 1,
            Year = Years[1],
            Area = 1:nArea
          )
        )
      } else {
        cli::cli_abort("Error in {.val Fleet@Closure", .internal = TRUE)
      }
    }


    # TODO: check dimensions
    # TODO: add dimnames if neccessary
  }

  SetDigest(Closure, argList)
}
