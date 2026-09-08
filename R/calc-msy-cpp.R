# Parallel C++ implementation of the MSY per-recruit calculation
.FlattenAgeArray <- function(arr) {
  arr <- .Aperm(arr, c('Sim', 'Age', 'Year'))
  as.numeric(arr[1, , 1])
}

.FlattenAgeFleetArray <- function(arr) {
  arr <- .Aperm(arr, c('Sim', 'Age', 'Year', 'Fleet'))
  d <- dim(arr)
  matrix(arr[1, , 1, ], nrow = d[2], ncol = d[4])
}

.FlattenAgeSeasonArray <- function(arr) {
  arr <- .Aperm(arr, c('Sim', 'Age', 'Year'))
  d <- dim(arr)
  matrix(arr[1, , ], nrow = d[2], ncol = d[3])
}

.FlattenAgeSeasonFleetArray <- function(arr) {
  arr <- .Aperm(arr, c('Sim', 'Age', 'Year', 'Fleet'))
  d <- dim(arr)
  matrix(arr[1, , , ], nrow = d[2], ncol = d[3] * d[4])
}

.FlattenPerRecruitInputsForCpp <- function(inputs) {
  if (!is.null(inputs$nSeason) && inputs$nSeason > 1L)
    return(.FlattenPerRecruitInputsForCppSeasonal(inputs))
  .FlattenPerRecruitInputsForCppAnnual(inputs)
}

.FlattenPerRecruitInputsForCppAnnual <- function(inputs) {
  StockAlloc <- .Aperm(inputs$StockFleetAllocation, c('Sim', 'Stock', 'Year', 'Fleet'))
  d <- dim(StockAlloc)
  if (d[1] != 1L || d[3] != 1L)
    cli::cli_abort("CalcPerRecruitFScalarCpp() requires Sim=1, Year=1 inputs (per-sim, per-year MSY search only).")
  StockFleetAllocMat <- matrix(StockAlloc[1, , 1, ], nrow = d[2], ncol = d[4])

  list(
    StockFleetAllocation      = StockFleetAllocMat,
    NaturalMortalityList      = purrr::map(inputs$NaturalMortalityList, .FlattenAgeArray),
    PlusGroupVec              = unlist(inputs$PlusGroupList, use.names = FALSE),
    MaturityList              = purrr::map(inputs$MaturityList, .FlattenAgeArray),
    SemelparousList           = purrr::map(inputs$SemelparousList, .FlattenAgeArray),
    WeightList                = purrr::map(inputs$WeightList, .FlattenAgeArray),
    SpawnTimeFracVec          = unlist(inputs$SpawnTimeFracList, use.names = FALSE),
    FecundityList             = purrr::map(inputs$FecundityList, .FlattenAgeArray),
    WeightFleetRetainedList   = purrr::map(inputs$WeightFleetRetainedList, .FlattenAgeFleetArray),
    WeightFleetSelectedList   = purrr::map(inputs$WeightFleetSelectedList, .FlattenAgeFleetArray),
    SelectivityFleetList      = purrr::map(inputs$SelectivityFleetList, .FlattenAgeFleetArray),
    RetentionFleetList        = purrr::map(inputs$RetentionFleetList, .FlattenAgeFleetArray),
    DiscardMortalityFleetList = purrr::map(inputs$DiscardMortalityFleetList, .FlattenAgeFleetArray),
    NPR0List                  = purrr::map(inputs$NPR0List, .FlattenAgeArray),
    NPR0_SPList               = purrr::map(inputs$NPR0_SPList, .FlattenAgeArray),
    SPR0_full                 = inputs$SPR0List |> List2Array('Stock', pos = 2),
    SPR0_target               = inputs$SPR0List |> List2Array('Stock', pos = 2) |> .ArraySubsetYear(inputs$Years)
  )
}


.FlattenPerRecruitInputsForCppSeasonal <- function(inputs) {
  StockAlloc <- .Aperm(inputs$StockFleetAllocation, c('Sim', 'Stock', 'Year', 'Fleet'))
  d <- dim(StockAlloc)
  if (d[1] != 1L)
    cli::cli_abort("CalcPerRecruitFScalarCpp() requires Sim=1 inputs (per-sim MSY search only).")
  nStock <- d[2]; nSeason <- d[3]; nFleet <- d[4]

  StockNames <- names(inputs$NaturalMortalityList)

  StockFleetAllocationList <- purrr::map(seq_len(nStock), \(st)
    matrix(StockAlloc[1, st, , ], nrow = nSeason, ncol = nFleet)
  )
  names(StockFleetAllocationList) <- StockNames

  IsSpawnTimeFrac <- any(unlist(inputs$SpawnTimeFracList) != 0)

  RefSeasonWeights <- .RefSeasonWeights(inputs$FecundityList, inputs$MaturityList,
                                        inputs$nSeason, inputs$RefSeason)
  RefSeasonWeightsVec <- as.numeric(RefSeasonWeights[1, ])

  NPR0_noList <- purrr::map(StockNames, \(nm) {
    Z0 <- inputs$NaturalMortalityList[[nm]]
    Semel <- inputs$SemelparousList[[nm]]
    npr <- .CalcNPRSeasonal(Z0, inputs$PlusGroupList[[nm]], 0, Semel)
    matrix(npr[1, , ], nrow = dim(npr)[2], ncol = dim(npr)[3])
  })
  names(NPR0_noList) <- StockNames

  NPR0_spList <- purrr::map(StockNames, \(nm) {
    if (!IsSpawnTimeFrac) return(NPR0_noList[[nm]])
    Z0 <- inputs$NaturalMortalityList[[nm]]
    Semel <- inputs$SemelparousList[[nm]]
    npr <- .CalcNPRSeasonal(Z0, inputs$PlusGroupList[[nm]],
                            inputs$SpawnTimeFracList[[nm]], Semel)
    matrix(npr[1, , ], nrow = dim(npr)[2], ncol = dim(npr)[3])
  })
  names(NPR0_spList) <- StockNames

  list(
    nSeason                    = inputs$nSeason,
    StockFleetAllocationList   = StockFleetAllocationList,
    NaturalMortalityList       = purrr::map(inputs$NaturalMortalityList, .FlattenAgeSeasonArray),
    PlusGroupVec               = unlist(inputs$PlusGroupList, use.names = FALSE),
    MaturityList                = purrr::map(inputs$MaturityList, .FlattenAgeSeasonArray),
    SemelparousList             = purrr::map(inputs$SemelparousList, .FlattenAgeSeasonArray),
    WeightList                  = purrr::map(inputs$WeightList, .FlattenAgeSeasonArray),
    SpawnTimeFracVec            = unlist(inputs$SpawnTimeFracList, use.names = FALSE),
    FecundityList                = purrr::map(inputs$FecundityList, .FlattenAgeSeasonArray),
    WeightFleetRetainedList      = purrr::map(inputs$WeightFleetRetainedList, .FlattenAgeSeasonFleetArray),
    WeightFleetSelectedList      = purrr::map(inputs$WeightFleetSelectedList, .FlattenAgeSeasonFleetArray),
    SelectivityFleetList         = purrr::map(inputs$SelectivityFleetList, .FlattenAgeSeasonFleetArray),
    RetentionFleetList           = purrr::map(inputs$RetentionFleetList, .FlattenAgeSeasonFleetArray),
    DiscardMortalityFleetList    = purrr::map(inputs$DiscardMortalityFleetList, .FlattenAgeSeasonFleetArray),
    SeasonalWeightsList          = purrr::map(inputs$SeasonalWeightsList, \(x) as.numeric(.Aperm(x, c('Sim', 'Year'))[1, ])),
    NPR0_noList                  = NPR0_noList,
    NPR0_spList                  = NPR0_spList,
    RefSeasonWeights              = RefSeasonWeightsVec,
    SPFromVec                    = as.integer(unname(inputs$SPFrom[StockNames])),
    IsSpawnTimeFrac               = IsSpawnTimeFrac
  )
}

.CalcPerRecruitFScalarCpp <- function(apicalF, inputs, flat) {
  if (!is.null(inputs$nSeason) && inputs$nSeason > 1L)
    return(.CalcPerRecruitFScalarCppSeasonal(apicalF, inputs, flat))
  .CalcPerRecruitFScalarCppAnnual(apicalF, inputs, flat)
}

.CalcPerRecruitFScalarCppAnnual <- function(apicalF, inputs, flat) {
  out <- CalcPerRecruitFScalarCpp_(
    apicalF                    = apicalF,
    StockFleetAllocation       = flat$StockFleetAllocation,
    NaturalMortalityList       = flat$NaturalMortalityList,
    PlusGroupVec                = flat$PlusGroupVec,
    MaturityList                = flat$MaturityList,
    SemelparousList             = flat$SemelparousList,
    WeightList                  = flat$WeightList,
    SpawnTimeFracVec            = flat$SpawnTimeFracVec,
    FecundityList                = flat$FecundityList,
    WeightFleetRetainedList      = flat$WeightFleetRetainedList,
    WeightFleetSelectedList      = flat$WeightFleetSelectedList,
    SelectivityFleetList         = flat$SelectivityFleetList,
    RetentionFleetList           = flat$RetentionFleetList,
    DiscardMortalityFleetList    = flat$DiscardMortalityFleetList,
    NPR0List                     = flat$NPR0List,
    NPR0_SPList                  = flat$NPR0_SPList,
    IsSpawnTimeFrac               = flat$IsSpawnTimeFrac
  )

  StockNames <- names(inputs$NaturalMortalityList)
  nStock     <- length(StockNames)

  ref_dn   <- dimnames(inputs$NaturalMortalityList[[1]])
  SimName  <- ref_dn$Sim
  YearName <- ref_dn$Year

  .Wrap <- function(vec) {
    array(vec[StockNames], dim = c(1, nStock, 1),
          dimnames = list(Sim = SimName, Stock = StockNames, Year = YearName))
  }

  sp_ind <- match(names(inputs$SPFrom), StockNames)
  SPRF_reordered <- out$SPRF[sp_ind]
  names(SPRF_reordered) <- StockNames

  SPR0_full <- flat$SPR0_full

  SPR0_vals <- flat$SPR0_target[1, StockNames, 1]
  SPR_vals  <- SPRF_reordered[StockNames] / SPR0_vals
  SPR_vals[is.na(SPR_vals)]       <- 0   
  SPR_vals[is.infinite(SPR_vals)] <- 0
  SPR <- .Wrap(SPR_vals)

  IsSpawnTimeFrac <- flat$IsSpawnTimeFrac

  PerRecruit             <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- .Wrap(out$NPR0)
  PerRecruit@NPR0_SP     <- if (IsSpawnTimeFrac) .Wrap(out$NPR0_SP) else .Wrap(out$NPR0)
  PerRecruit@SPR0        <- SPR0_full
  PerRecruit@NPRF        <- .Wrap(out$NPRF)
  PerRecruit@NPRF_SP     <- if (IsSpawnTimeFrac) .Wrap(out$NPRF_SP) else .Wrap(out$NPRF)
  PerRecruit@SPRF        <- .Wrap(SPRF_reordered)
  PerRecruit@SPR         <- SPR
  PerRecruit@Biomass     <- .Wrap(out$Biomass)
  PerRecruit@SBiomass    <- .Wrap(out$SBiomass)
  PerRecruit@SProduction <- .Wrap(out$SProduction)
  PerRecruit@Removals    <- .Wrap(out$Removals)
  PerRecruit@Landings    <- .Wrap(out$Landings)
  PerRecruit
}

.CalcPerRecruitFScalarCppSeasonal <- function(apicalF, inputs, flat) {
  out <- CalcPerRecruitFScalarSeasonalCpp_(
    apicalF                    = apicalF,
    StockFleetAllocationList   = flat$StockFleetAllocationList,
    NaturalMortalityList       = flat$NaturalMortalityList,
    PlusGroupVec                = flat$PlusGroupVec,
    MaturityList                = flat$MaturityList,
    SemelparousList             = flat$SemelparousList,
    WeightList                  = flat$WeightList,
    SpawnTimeFracVec            = flat$SpawnTimeFracVec,
    FecundityList                = flat$FecundityList,
    WeightFleetRetainedList      = flat$WeightFleetRetainedList,
    WeightFleetSelectedList      = flat$WeightFleetSelectedList,
    SelectivityFleetList         = flat$SelectivityFleetList,
    RetentionFleetList           = flat$RetentionFleetList,
    DiscardMortalityFleetList    = flat$DiscardMortalityFleetList,
    SeasonalWeightsList          = flat$SeasonalWeightsList,
    NPR0_noList                  = flat$NPR0_noList,
    NPR0_spList                  = flat$NPR0_spList,
    RefSeasonWeights              = flat$RefSeasonWeights,
    SPFromVec                    = flat$SPFromVec,
    IsSpawnTimeFrac               = flat$IsSpawnTimeFrac,
    nSeason                       = flat$nSeason
  )

  StockNames <- names(inputs$NaturalMortalityList)
  nStock     <- length(StockNames)

  SimName  <- dimnames(inputs$NaturalMortalityList[[1]])$Sim
  YearName <- as.character(inputs$CalendarYears)

  .Wrap <- function(vec) {
    array(vec[StockNames], dim = c(1, nStock, 1),
          dimnames = list(Sim = SimName, Stock = StockNames, Year = YearName))
  }

  IsSpawnTimeFrac <- flat$IsSpawnTimeFrac

  PerRecruit             <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- .Wrap(out$NPR0)
  PerRecruit@NPR0_SP     <- .Wrap(out$NPR0_SP)
  PerRecruit@SPR0        <- .Wrap(out$SPR0f)
  PerRecruit@NPRF        <- .Wrap(out$NPRF)
  PerRecruit@NPRF_SP     <- if (IsSpawnTimeFrac) .Wrap(out$NPRF_SP) else .Wrap(out$NPRF)
  PerRecruit@SPRF        <- .Wrap(out$SPRFf)
  PerRecruit@SPR         <- .Wrap(out$SPR)
  PerRecruit@Biomass     <- .Wrap(out$Biomass)
  PerRecruit@SBiomass    <- .Wrap(out$SBiomass)
  PerRecruit@SProduction <- .Wrap(out$SProduction)
  PerRecruit@Removals    <- .Wrap(out$Removals)
  PerRecruit@Landings    <- .Wrap(out$Landings)
  F_annual_apical_val <- out$F_annual_apical
  names(F_annual_apical_val) <- SimName
  PerRecruit@Misc        <- list(F_annual_apical = list(F_annual_apical_val))
  PerRecruit
}

.CalcPerRecruitFCpp <- function(apicalF, inputs, flat, spr_threshold = 0.001) {
  names(apicalF) <- as.character(apicalF)

  PRList    <- vector('list', length(apicalF))
  names(PRList) <- names(apicalF)
  collapsed <- FALSE

  for (i in seq_along(apicalF)) {
    if (collapsed) {
      PRList[[i]] <- PRList[[i - 1]]
    } else {
      PRList[[i]] <- .CalcPerRecruitFScalarCpp(apicalF[i], inputs, flat)
      if (!is.null(PRList[[i]]@SPR) && min(PRList[[i]]@SPR, na.rm = TRUE) < spr_threshold)
        collapsed <- TRUE
    }
  }

  zero_fill <- \(arr) { arr[] <- 0; arr }
  if (collapsed) {
    first_collapsed <- which(
      purrr::map_lgl(PRList, \(pr) !is.null(pr@SPR) && min(pr@SPR, na.rm = TRUE) < spr_threshold)
    )[1]
    for (i in seq(first_collapsed, length(apicalF))) {
      pr <- PRList[[i]]
      pr@NPRF        <- zero_fill(pr@NPRF)
      pr@NPRF_SP     <- zero_fill(pr@NPRF_SP)
      pr@SPRF        <- zero_fill(pr@SPRF)
      pr@SPR         <- zero_fill(pr@SPR)
      pr@Biomass     <- zero_fill(pr@Biomass)
      pr@SBiomass    <- zero_fill(pr@SBiomass)
      pr@SProduction <- zero_fill(pr@SProduction)
      pr@Removals    <- zero_fill(pr@Removals)
      pr@Landings    <- zero_fill(pr@Landings)
      PRList[[i]]    <- pr
    }
  }

  PerRecruit             <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- PRList[[1]]@NPR0
  PerRecruit@NPR0_SP     <- PRList[[1]]@NPR0_SP
  PerRecruit@SPR0        <- PRList[[1]]@SPR0
  PerRecruit@NPRF        <- purrr::map(PRList, \(pr) pr@NPRF) |> List2Array('F')
  PerRecruit@NPRF_SP     <- purrr::map(PRList, \(pr) pr@NPRF_SP) |> List2Array('F')
  PerRecruit@SPRF        <- purrr::map(PRList, \(pr) pr@SPRF)        |> List2Array('F')
  PerRecruit@SPR         <- purrr::map(PRList, \(pr) pr@SPR)         |> List2Array('F')
  PerRecruit@Biomass     <- purrr::map(PRList, \(pr) pr@Biomass)     |> List2Array('F')
  PerRecruit@SBiomass    <- purrr::map(PRList, \(pr) pr@SBiomass)    |> List2Array('F')
  PerRecruit@SProduction <- purrr::map(PRList, \(pr) pr@SProduction) |> List2Array('F')
  PerRecruit@Removals    <- purrr::map(PRList, \(pr) pr@Removals)    |> List2Array('F')
  PerRecruit@Landings    <- purrr::map(PRList, \(pr) pr@Landings)    |> List2Array('F')
  PerRecruit@Misc        <- PRList[[1]]@Misc
  PerRecruit
}

.OptCalcRefMSYSimsCpp <- function(logApicalF, inputs, flat, complex_name,
                                  type = c('Removals', 'Landings'),
                                  option = 1) {
  type <- match.arg(type)

  if (length(logApicalF) > 1) {
    cli::cli_alert_danger(
      '{.var logApicalF} must be length 1. Using first value {.val {logApicalF[1]}}')
    logApicalF <- logApicalF[1]
  }
  apicalF <- exp(logApicalF)

  PerRecruit <- .CalcPerRecruitFCpp(apicalF, inputs, flat)

  if (option == 1) {
    Eq <- .CalcEquilibriumInternal(PerRecruit, inputs)
    Removals <- Eq@Removals |> DropDimension("F")
    Landings <- Eq@Landings |> DropDimension("F")

    if (type == 'Removals') {
      Catch <- SumOverStock(Removals)
    } else {
      Catch <- SumOverStock(Landings)
    }
    return(-Catch)
  }

  Eq <- .CalcEquilibriumInternal(PerRecruit, inputs)

  Removals <- Eq@Removals |> DropDimension("F")
  Landings <- Eq@Landings |> DropDimension("F")
  Discards <- ArraySubtract(Removals, Landings)

  fmsy_val <- if (!is.null(PerRecruit@Misc$F_annual_apical)) {
    PerRecruit@Misc$F_annual_apical[[1L]]
  } else {
    apicalF
  }
  FMSY <- array(fmsy_val, dim(Eq@Biomass |> DropDimension("F")),
                dimnames = dimnames(Eq@Biomass |> DropDimension("F"))) |>
    DropDimension('Stock', warn = FALSE) |>
    AddDimension('Stock', complex_name, pos = 2)

  MSYRefPoints             <- new("refpointsMSY")
  MSYRefPoints@FMSY        <- FMSY
  MSYRefPoints@BMSY        <- Eq@Biomass     |> DropDimension("F")
  MSYRefPoints@SBMSY       <- Eq@SBiomass    |> DropDimension("F")
  MSYRefPoints@SPMSY       <- Eq@SProduction |> DropDimension("F")
  MSYRefPoints@SPRMSY      <- Eq@SPR         |> DropDimension("F") |> .Aperm(c('Sim', 'Stock', 'Year'))
  MSYRefPoints@MSYLandings <- Landings
  MSYRefPoints@MSYDiscards <- Discards
  MSYRefPoints
}

.CalcRefMSYComplexCpp <- function(Hist, complex_stocks, complex_name, Years,
                                  type, parallel = FALSE, silent = FALSE,
                                  RefSeason = NULL, RefEffortYears = NULL) {

  StockList <- Hist@OM@Stock[complex_stocks]
  FleetList <- Hist@OM@Fleet[complex_stocks]

  nSim          <- nSim(Hist)
  IdenticalHist <- .IdenticalSims(Hist@OM, ignore = c('RecDevInit',
                                                      'RecDevHist',
                                                      'RecDevProj',
                                                      'Allocation',
                                                      'Obs',
                                                      'Data'))

  SPR0_Full_List <- Array2List(Hist@Reference@SPR0) |>
    .SubsetStock(Stocks = complex_stocks)

  logApicalFRange <- log(c(1E-5, Hist@OM@maxF))

  MSYRefPoints <- Hist@Reference@MSY

  RunOneSim <- function(StockList_sim, FleetList_sim, SPR0_List_sim) {
    purrr::map(seq_along(Years), \(ts) {
      inputs <- .PrepPerRecruitInputs(StockList_sim, FleetList_sim,
                                      SPR0_List_sim, Years[ts],
                                      EffortYears = RefEffortYears, RefSeason = RefSeason)
      flat <- .FlattenPerRecruitInputsForCpp(inputs)

      opt <- optimize(
        .OptCalcRefMSYSimsCpp,
        logApicalFRange,
        inputs       = inputs,
        flat         = flat,
        complex_name = complex_name,
        type         = type,
        option       = 1
      )

      .OptCalcRefMSYSimsCpp(
        logApicalF   = opt$minimum,
        inputs       = inputs,
        flat         = flat,
        complex_name = complex_name,
        type         = type,
        option       = 2
      )
    })
  }

  if (IdenticalHist) {
    if (!silent)
      cli::cli_progress_message("Calculating MSY reference points (C++): {.val {complex_name}}")

    results_sim1 <- RunOneSim(
      Subset(StockList,      Sims = 1),
      Subset(FleetList,      Sims = 1),
      Subset(SPR0_Full_List, Sims = 1)
    )

    for (sl in setdiff(slotNames(MSYRefPoints), 'Misc')) {
      arr <- slot(MSYRefPoints, sl)
      if (is.null(arr)) next
      for (ts in seq_along(Years)) {
        val <- slot(results_sim1[[ts]], sl)
        if (!is.null(val))
          ArrayFill(arr) <- val
      }
      slot(MSYRefPoints, sl) <- arr
    }

    Hist@Reference@MSY <- ReduceDims(MSYRefPoints)
    return(Hist)
  }

  CalcRefMSY_Sim <- function(sim) {
    RunOneSim(
      .SubsetSim(StockList,      Sims = sim, keep_sim_name = TRUE),
      .SubsetSim(FleetList,      Sims = sim, keep_sim_name = TRUE),
      .SubsetSim(SPR0_Full_List, Sims = sim, keep_sim_name = TRUE)
    )
  }

  parallel <- CheckParallel(parallel)

  if (!parallel) {
    if (!silent) {
      id <- cli::cli_progress_bar(
        name   = paste0("Calculating MSY reference points (C++): ", complex_name),
        total  = nSim,
        format = "{cli::pb_name} {cli::pb_bar} {cli::pb_current}/{cli::pb_total} sims | {cli::pb_elapsed}"
      )
    }

    results_by_sim <- purrr::map(seq_len(nSim), \(sim) {
      result <- CalcRefMSY_Sim(sim)
      if (!silent) cli::cli_progress_update(id = id)
      result
    })
  } else {
    if (!silent)
      cli::cli_inform(
        "Calculating MSY reference points (C++): {.val {complex_name}} \\
         ({.val {nSim}} simulation{?s}, parallel) ..."
      )
    CheckPackage('furrr')
    results_by_sim <- furrr::future_map(
      seq_len(nSim),
      CalcRefMSY_Sim,
      .options = furrr::furrr_options(
        globals  = c('StockList', 'FleetList', 'SPR0_Full_List', 'Years',
                     'complex_name', 'type', 'logApicalFRange',
                     'RefSeason', 'RefEffortYears'),
        packages = "MSEtool",
        seed     = 101
      )
    )
  }

  for (sl in setdiff(slotNames(MSYRefPoints), 'Misc')) {
    arr <- slot(MSYRefPoints, sl)
    if (is.null(arr)) next
    for (sim in seq_len(nSim)) {
      for (ts in seq_along(Years)) {
        val <- slot(results_by_sim[[sim]][[ts]], sl)
        if (!is.null(val))
          ArrayFill(arr) <- val
      }
    }
    slot(MSYRefPoints, sl) <- arr
  }

  Hist@Reference@MSY <- ReduceDims(MSYRefPoints)
  Hist
}

CalcMSYCpp <- function(Hist,
                       Years = NULL,
                       type = c('Removals', 'Landings'),
                       parallel = FALSE,
                       silent = FALSE) {

  type <- match.arg(type)
  .CheckClass(Hist, c('om', 'hist'))

  if (inherits(Hist, 'om'))
    Hist <- .OM2Hist(Hist, silent = TRUE)

  .CheckClass(Hist, 'hist', 'Hist')

  nSeason        <- Hist@OM@Seasons
  RefSeason      <- Hist@OM@RefSeason
  RefEffortYears <- Hist@OM@RefEffortYears

  if (is.null(Years)) {
    Years <- utils::tail(Years(Hist@OM, 'Historical'), 1)
    if (nSeason > 1L) Years <- unique(floor(Years))
  }

  if (is.null(Hist@Reference@SPR0))
    Hist@Reference@SPR0 <- CalcSPR0(Hist, silent = TRUE)

  Hist <- .InitMSYRefPoints(Hist, Years)

  StockNames <- StockNames(Hist)

  complexes <- Complexes(Hist)

  for (i in seq_along(complexes)) {
    Hist <- .CalcRefMSYComplexCpp(Hist,
                                  complex_stocks = StockNames[complexes[[i]]],
                                  complex_name   = names(complexes)[i],
                                  Years          = Years,
                                  type           = type,
                                  parallel       = parallel,
                                  silent         = silent,
                                  RefSeason      = RefSeason,
                                  RefEffortYears = RefEffortYears)
  }

  if (!silent)
    cli::cli_alert_success("Calculated MSY Reference Points (C++)")

  Hist@Reference@MSY
}
