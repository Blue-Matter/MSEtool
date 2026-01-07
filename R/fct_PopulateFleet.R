#' @describeIn Populate Populate an [fleet-class()] object
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

  argList <- list(Ages, Length, Weight, RelativeSize, nsim, Years, seed)
  
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

  Fleet <- PopulateCatchability(
    Fleet,
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
    CalcAtLength = FALSE,
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
    CalcAtLength = FALSE,
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
    CalcAtLength = FALSE,
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

  if (is.null(Effort@Value)) {
    return(Effort)
  }

  if (inherits(Effort@Value, "data.frame")) {
    Effort <- GenerateHistoricalEffort(Effort, nSim, HistYears)
  }

  dd <- dim(Effort@Value)
  if (dd[2] != length(HistYears)) {
    cli::cli_abort("`ncol(Effort@Value)` is not equal to `length(HistYears)`")
  }
  dimnames(Effort@Value) <- list(
    Sim = 1:nrow(Effort@Value),
    Year = HistYears
  )

  Effort@Distribution <- PopulateDistribution(
    Distribution = Effort@Distribution,
    nSim,
    HistYears,
    nArea
  )
  Effort
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


PopulateCatchability <- function(Fleet,
                                 RelativeSize,
                                 nSim = 5,
                                 HistYears = NULL,
                                 ProjYears = NULL,
                                 seed = NULL,
                                 silent = FALSE) {
  Catchability <- Fleet@Catchability
  pYears <- length(ProjYears)

  Years <- c(HistYears, ProjYears)

  if (all(is.na(Catchability@Value)) || all(Catchability@Value <= tiny)) {
    Catchability@Value <- array(1,
      dim = c(nSim, length(Years)),
      dimnames = list(
        Sim = 1:nSim,
        Year = Years
      )
    )
  } else {
    dd <- dim(Catchability@Value)
    if (dd[1] != nSim) {
      if (dd[1] != 1) {
        cli::cli_abort(c(
          "x" = "Incorrect number of rows in `Catchability@Value` matrix.",
          "i" = "Must have either 1 row or `nSim` ({.val {nSim}}) rows. "
        ))
      }
    }

    if (is.null(dimnames(Catchability@Value))) {
      dimnames(Catchability@Value) <- list(
        Sim = 1:nrow(Catchability@Value),
        Year = Years[1:ncol(Catchability@Value)]
      )
    }

    Catchability@Value <- ExtendYears(Catchability@Value, HistYears)
  }

  if (!is.null(Fleet@Catchability@qInc)) {
    qIncs <- StructurePars_(Fleet@Catchability@qInc, nSim, Years)[, 1]
    qIncs <- sapply(qIncs, function(x) {
      (1 + x / 100)^(1:pYears)
    }) |> t()

    dimnames(qIncs) <- list(
      Sim = 1:nSim,
      Year = ProjYears
    )

    qfuture <- ArrayMultiply(SubsetYear(Catchability@Value, ProjYears), qIncs)
    ArrayFill(Catchability@Value) <- qfuture
    Fleet@Catchability@qInc <- qIncs
  }


  if (!is.null(Fleet@Catchability@qCV)) {
    qCVs <- StructurePars_(Fleet@Catchability@qCV, nSim, Years)[, 1]
    Fleet@Catchability@qCV <- qCVs

    qmu <- -0.5 * qCVs^2
    qvar <- array(exp(rnorm(pYears * nSim, rep(qmu, pYears), rep(qCVs, pYears))), c(nSim, pYears),
      dimnames = list(
        Sim = 1:nSim,
        Year = ProjYears
      )
    )

    qfuture <- ArrayMultiply(SubsetYear(Catchability@Value, ProjYears), qvar)
    if (!all(qfuture == 1)) {
      ArrayFill(Catchability@Value) <- qfuture
    }
  }
  Fleet@Catchability@Value <- Catchability@Value

  if (EmptyObject(Fleet@Catchability@qArea)) {
    Fleet@Catchability@qArea <- ArrayDivide(
      array1 = AddDimension(Catchability@Value, "Area"),
      array2 = AddDimension(RelativeSize, "Year", val = Years[1]) |>
        aperm(c("Sim", "Year", "Area"))
    )
  } else {
    dd <- dim(Fleet@Catchability@qArea)
    # TODO: check dimensions
    # TODO: add dimnames if neccessary
  }
  Fleet
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
    Retention@MeanAtAge <- array(1, dim = c(1, length(Ages@Classes), 1, 1)) |>
      SetDimNames_SAYR(Age = Ages@Classes, Years = Years)
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
  Retention@MeanAtLength <- SetDimNames_SCYR(
    Retention@MeanAtLength,
    Retention@Classes,
    Years
  )

  Retention@MeanAtWeight <- SetDimNames_SCYR(
    Retention@MeanAtWeight,
    Retention@Classes,
    Years
  )

  Retention@MeanAtAge <- SetDimNames_SAYR(
    Retention@MeanAtAge,
    Ages@Classes,
    Years
  )

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
    DiscardMortality@MeanAtAge <- array(1, dim = c(1, length(Ages@Classes), 1, 1)) |>
      SetDimNames_SAYR(Age = Ages@Classes, Years = Years)
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
  
  DiscardMortality@MeanAtLength <- SetDimNames_SCYR(
    DiscardMortality@MeanAtLength,
    DiscardMortality@Classes,
    Years
  )
  DiscardMortality@MeanAtAge <- SetDimNames_SAYR(
    DiscardMortality@MeanAtAge,
    Ages@Classes,
    Years
  )

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
