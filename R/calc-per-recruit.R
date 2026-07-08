#' Calculate Per-Recruit Quantities
#'
#' Extracts biological and fishery parameters from a [hist-class] or [om-class]
#' object and evaluates per-recruit quantities at a given apical fishing
#' mortality.
#'
#' @param OM A [om-class] or [hist-class] object. If an `om` is supplied
#'   it is first converted to a `hist` object.
#' @param apicalF Numeric vector of apical fishing mortality values at which
#'   per-recruit quantities are evaluated. Default `0.1`.
#' @param Years Integer vector of years for which per-recruit quantities are
#'   evaluated. Biological and fishery parameters are subset to these years
#'   If `NULL` (default), the final historical year is used.
#' @param Complex Character vector of complex names to evaluate. If `NULL`
#'   (default), all complexes are evaluated.
#'
#' @return A [perrecruit-class] object containing numbers-per-recruit,
#'   spawning-per-recruit, biomass-per-recruit, removals, and landings
#'   evaluated at each value of `apicalF`, with results reported per stock
#'   and apical F defined as the maximum F across all stocks within each
#'   complex.
#'
#' @details
#'
#' ## Complexes
#' Per-recruit quantities are calculated separately for each complex, with
#' `apicalF` defined as the maximum fishing mortality across all ages and
#' stocks within the complex. Results are reported per stock and reassembled
#' into a single [perrecruit-class] object spanning all stocks. The `Complex`
#' argument can be used to restrict calculations to a subset of complexes.
#'
#' ## Fleet allocation
#' The relative contribution of each fleet to total fishing mortality is
#' computed as the effort-weighted catchability
#' (i.e. `Effort × Efficiency`) normalised across fleets. These proportions
#' are used to distribute `apicalF` across fleets before selectivity is applied.
#'
#' ## Years
#' When `Years` contains multiple values, biological and fishery parameters
#' are subset to those years and the per-recruit calculations are evaluated
#' for each year independently. When `Years = NULL`, only the final historical
#' year is used, giving a single set of per-recruit quantities representative
#' of current conditions.
#'
#' @seealso [CalcSPR0()], [perrecruit-class]
#' @export
CalcPerRecruit <- function(OM, apicalF=0.1, Years=NULL, Complex=NULL) {
  CheckClass(OM, c('om', 'hist'))

  if (inherits(OM, 'om'))
    Hist <- OM2Hist(OM, silent=TRUE)

  if (inherits(OM, 'hist'))
    Hist <- OM

  CheckClass(Hist, 'hist', 'Hist')

  nSeason <- Hist@OM@Seasons

  if (is.null(Years)) {
    Years <- utils::tail(Years(Hist@OM, 'Historical'), 1)
    if (nSeason > 1L) Years <- unique(floor(Years))
  }

  StockNames <- StockNames(Hist)

  complexes <- Complexes(Hist)
  if (!is.null(Complex))
    complexes <- complexes[Complex]

  if (is.null(Hist@Reference@SPR0))
    Hist@Reference@SPR0 <- CalcSPR0(Hist, silent=TRUE)

  SPR0List <- Array2List(Hist@Reference@SPR0)

  PRByComplex <- purrr::map(complexes, \(stockInd) {
    CalcPerRecruit_StockList(
      StockList = Hist@OM@Stock[stockInd],
      FleetList = Hist@OM@Fleet[stockInd],
      apicalF   = apicalF,
      Years     = Years,
      SPR0List  = SPR0List[stockInd]
    )
  })
  
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- purrr::map(PRByComplex, \(pr) pr@NPR0)        |> JoinStockArrays(StockNames)
  PerRecruit@NPR0_SP     <- purrr::map(PRByComplex, \(pr) pr@NPR0_SP)     |> JoinStockArrays(StockNames)
  PerRecruit@SPR0        <- purrr::map(PRByComplex, \(pr) pr@SPR0)        |> JoinStockArrays(StockNames)
  PerRecruit@NPRF        <- purrr::map(PRByComplex, \(pr) pr@NPRF)        |> JoinStockArrays(StockNames)
  PerRecruit@NPRF_SP     <- purrr::map(PRByComplex, \(pr) pr@NPRF_SP)     |> JoinStockArrays(StockNames)
  PerRecruit@SPRF        <- purrr::map(PRByComplex, \(pr) pr@SPRF)        |> JoinStockArrays(StockNames)
  PerRecruit@SPR         <- purrr::map(PRByComplex, \(pr) pr@SPR)         |> JoinStockArrays(StockNames)
  PerRecruit@Biomass     <- purrr::map(PRByComplex, \(pr) pr@Biomass)     |> JoinStockArrays(StockNames)
  PerRecruit@SBiomass    <- purrr::map(PRByComplex, \(pr) pr@SBiomass)    |> JoinStockArrays(StockNames)
  PerRecruit@SProduction <- purrr::map(PRByComplex, \(pr) pr@SProduction) |> JoinStockArrays(StockNames)
  PerRecruit@Removals    <- purrr::map(PRByComplex, \(pr) pr@Removals)    |> JoinStockArrays(StockNames)
  PerRecruit@Landings    <- purrr::map(PRByComplex, \(pr) pr@Landings)    |> JoinStockArrays(StockNames)
  PerRecruit
}

CalcPerRecruit_StockList <- function(StockList, FleetList, apicalF=0.1, Years, SPR0List) {

  inputs <- PrepPerRecruitInputs(StockList, FleetList, SPR0List, Years)
  
  PR <- CalcPerRecruit_F(
    apicalF                   = apicalF,
    StockFleetAllocation      = inputs$StockFleetAllocation,
    NaturalMortalityList      = inputs$NaturalMortalityList,
    PlusGroupList             = inputs$PlusGroupList,
    MaturityList              = inputs$MaturityList,
    SemelparousList           = inputs$SemelparousList,
    WeightList                = inputs$WeightList,
    SpawnTimeFracList         = inputs$SpawnTimeFracList,
    SPFrom                    = inputs$SPFrom,
    SPR0List                  = inputs$SPR0List,
    FecundityList             = inputs$FecundityList,
    WeightFleetList           = inputs$WeightFleetList,
    SelectivityFleetList      = inputs$SelectivityFleetList,
    RetentionFleetList        = inputs$RetentionFleetList,
    DiscardMortalityFleetList = inputs$DiscardMortalityFleetList,
    FleetNames                = inputs$FleetNames,
    Years                     = inputs$Years,
    nSeason                   = inputs$nSeason,
    SeasonalWeightsList       = inputs$SeasonalWeightsList,
    CalendarYears             = inputs$CalendarYears
  )

  PR
}

JoinStockArrays <- function(arrayList, StockNames) {
  
  arrayList <- purrr::compact(arrayList)
  if (length(arrayList) == 0) return(NULL)
  
  StockInd <- match("Stock", names(dimnames(arrayList[[1]])))
  if (is.na(StockInd))
    cli::cli_abort("`Stock` dimension not found in arrays passed to `JoinStockArrays()`",
                   .internal = TRUE)
  
  # bind across complexes along Stock dimension
  # list names are complex names; stock names are in array dimnames
  combined <- abind::abind(arrayList, along = StockInd, use.dnns = TRUE)
  
  # reorder to StockNames order
  currentStocks <- dimnames(combined)$Stock
  if (!setequal(currentStocks, StockNames))
    cli::cli_abort(
      "Stock names in arrays {.val {currentStocks}} do not match expected {.val {StockNames}}",
      .internal = TRUE
    )
  
  ArraySubsetStock(combined, match(StockNames, currentStocks))
}

CalcFleetAllocationF <- function(FleetList, Years) {

  FDistribution <- purrr::map(FleetList, \(Fleet) {
    ArrayMultiply(Fleet@Effort@Effort |>  ArraySubsetYear(Years),
                  Fleet@Catchability@Efficiency |>  ArraySubsetYear(Years))
  }) |>
    List2Array('Fleet', pos=3)

  FDistributionTotal <- SumOverFleet(FDistribution)  # [Sim, Year]
  nYear <- dim(FDistributionTotal)[['Year']]

  if (nYear > 1L) {
    # Seasonal: normalise by the peak-season total so apicalF = peak-season F.
    # This preserves the seasonal effort pattern while keeping the scale correct.
    FPeak <- apply(FDistributionTotal, 1L, max)   # max over seasons, [Sim]
    FPeak <- pmax(FPeak, .Machine$double.eps)
    sweep(FDistribution, 1L, FPeak, "/")          # [Sim, Year, Fleet]
  } else {
    FDistributionTotal <- AddDimension(FDistributionTotal, 'Fleet') |>
      ExtendFleets(Fleets = names(FleetList))
    dimnames(FDistributionTotal)[['Fleet']] <- names(FleetList)
    ArrayDivide(FDistribution, FDistributionTotal)
  }
}

# ── Seasonal per-recruit helpers ─────────────────────────────────────────────

# Compute numbers-per-recruit for every birth season using the direct formula.
# Z: [Sim, Age, Season] — total mortality (M + F_dead) for each age and
#   calendar season within the target year.
# SpawnTimeFrac: scalar or length-nSim. Fraction of time step before spawning.
# Semelparous: [Sim, Age, Season] or FALSE.
# Returns NPR[Sim, Age, BirthSeason] where BirthSeason = 1..nSeason and
# corresponds to the calendar-season index within the supplied year.
# When SpawnTimeFrac > 0 the returned array gives numbers AT the time of
# spawning; call with SpawnTimeFrac = 0 for numbers at the start of each step.
CalcNPR_seasonal <- function(Z, PlusGroup, SpawnTimeFrac, Semelparous) {

  nSim    <- dim(Z)[1]
  nAge    <- dim(Z)[2]
  nSeason <- dim(Z)[3]

  if (length(SpawnTimeFrac) != nSim)
    SpawnTimeFrac <- rep(SpawnTimeFrac, nSim)[seq_len(nSim)]

  if (isFALSE(Semelparous))
    Semelparous <- array(0, dim(Z), dimnames(Z))

  NPR <- array(0, dim = c(nSim, nAge, nSeason))

  for (s in seq_len(nSeason)) {
    # Calendar-season index for each age class of a cohort born in season s
    seas_at_age <- ((s - 1L) + seq_len(nAge) - 1L) %% nSeason + 1L

    # Age class 1 (recruitment): survival up to the spawning point
    NPR[, 1, s] <- exp(-Z[, 1, seas_at_age[1L]] * SpawnTimeFrac)

    for (a in 2:nAge) {
      s_prev       <- seas_at_age[a - 1L]
      s_curr       <- seas_at_age[a]
      Semel_prev   <- Semelparous[, a - 1L, s_prev]
      NPR[, a, s]  <- NPR[, a - 1L, s] *
        exp(-(Z[, a-1L, s_prev] * (1 - SpawnTimeFrac) +
              Z[, a,    s_curr] *      SpawnTimeFrac)) *
        (1 - Semel_prev)
    }

    # Standard plus-group adjustment at the last age class
    if (PlusGroup) {
      s_last  <- seas_at_age[nAge]
      z_last  <- Z[, nAge, s_last]
      denom   <- 1 - exp(-z_last)
      ok      <- denom > .Machine$double.eps
      NPR[ok,  nAge, s] <- NPR[ok,  nAge, s] / denom[ok]
    }
  }

  NPR   # [Sim, Age, BirthSeason]
}

# Aggregate a seasonal per-recruit quantity to an annual scalar.
# NPR_bs[Sim, Age, BirthSeason] — per-recruit abundance by birth season.
# q_sa[Sim, Age, Season]        — per-unit quantity for each age × season.
# pi_s[Sim, Season]             — seasonal recruitment weights (sum_s pi_s = 1).
# Returns [Sim] annual aggregate: sum_s { pi_s * sum_a { NPR_s[a] * q[a,s_at_a] } }.
AggSeasonalProduct <- function(NPR_bs, q_sa, pi_s) {

  nSim    <- dim(NPR_bs)[1]
  nAge    <- dim(NPR_bs)[2]
  nSeason <- dim(NPR_bs)[3]

  result <- numeric(nSim)

  for (s in seq_len(nSeason)) {
    seas_at_age <- ((s - 1L) + seq_len(nAge) - 1L) %% nSeason + 1L
    # Build q_s[Sim, Age] using the season appropriate for each age class
    idx   <- cbind(
      rep(seq_len(nSim),  nAge),
      rep(seq_len(nAge),  each = nSim),
      rep(seas_at_age,    each = nSim)
    )
    q_s    <- matrix(q_sa[idx], nrow = nSim, ncol = nAge)
    result <- result + pi_s[, s] * rowSums(NPR_bs[, , s] * q_s)
  }

  result
}

# Aggregate seasonal yield-per-recruit (summed over fleets).
# NPR_bs[Sim, Age, BirthSeason] — unfished/fished NPR by birth season.
# FDead_saf[Sim, Age, Season, Fleet]   — fleet fishing mortality (dead).
# ZTotal_sa[Sim, Age, Season]          — total mortality Z.
# WeightFleet_saf[Sim, Age, Season, Fleet] — weight for each fleet.
# pi_s[Sim, Season] — seasonal recruitment weights.
# Returns [Sim] annual yield per annual recruit.
AggSeasonalYield <- function(NPR_bs, FDead_saf, ZTotal_sa, WeightFleet_saf, pi_s,
                             type = c('Removals', 'Landings'),
                             FRetain_saf = NULL) {
  type    <- match.arg(type)
  nSim    <- dim(NPR_bs)[1]
  nAge    <- dim(NPR_bs)[2]
  nSeason <- dim(NPR_bs)[3]

  F_use <- if (type == 'Removals') FDead_saf else {
    if (is.null(FRetain_saf))
      cli::cli_abort("`FRetain_saf` required for Landings yield.", .internal=TRUE)
    FRetain_saf
  }

  result <- numeric(nSim)

  for (s in seq_len(nSeason)) {
    seas_at_age <- ((s - 1L) + seq_len(nAge) - 1L) %% nSeason + 1L

    # Yield per recruit for this birth season, summed over ages and fleets
    Y_s <- numeric(nSim)
    for (a in seq_len(nAge)) {
      sa <- seas_at_age[a]
      Z_a  <- ZTotal_sa[, a, sa]
      denom <- pmax(Z_a, .Machine$double.eps)
      # Fraction of total mortality from each fleet (harvest rate)
      nFleet <- dim(FDead_saf)[4]
      for (f in seq_len(nFleet)) {
        F_af <- F_use[, a, sa, f]
        W_af <- WeightFleet_saf[, a, sa, f]
        Y_s  <- Y_s + NPR_bs[, a, s] * (1 - exp(-Z_a)) * (F_af / denom) * W_af
      }
    }
    result <- result + pi_s[, s] * Y_s
  }

  result
}


CalcPerRecruit_F <- function(apicalF = 0.1, spr_threshold = 0.001, ...) {
  names(apicalF) <- as.character(apicalF)
  
  PRList    <- vector('list', length(apicalF))
  names(PRList) <- names(apicalF)
  collapsed <- FALSE
  
  for (i in seq_along(apicalF)) {
    if (collapsed) {
      PRList[[i]] <- PRList[[i-1]] 
    } else {
      PRList[[i]] <- CalcPerRecruit_F_scalar(apicalF = apicalF[i], ...)
      if (!is.null(PRList[[i]]@SPR) && min(PRList[[i]]@SPR, na.rm=TRUE) < spr_threshold)
        collapsed <- TRUE
    }
  }
  
  # zero-fill all F steps at and beyond the collapse point
  zero_fill <- \(arr) { arr[] <- 0; arr }
  if (collapsed) {
    first_collapsed <- which(
      purrr::map_lgl(PRList, \(pr) 
                     !is.null(pr@SPR) && min(pr@SPR, na.rm=TRUE) < spr_threshold)
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
  PerRecruit@NPRF_SP     <- purrr::map(PRList, \(pr) pr@NPRF_SP)|> List2Array('F')
  PerRecruit@SPRF        <- purrr::map(PRList, \(pr) pr@SPRF)        |> List2Array('F')
  PerRecruit@SPR         <- purrr::map(PRList, \(pr) pr@SPR)         |> List2Array('F')
  PerRecruit@Biomass     <- purrr::map(PRList, \(pr) pr@Biomass)     |> List2Array('F')
  PerRecruit@SBiomass    <- purrr::map(PRList, \(pr) pr@SBiomass)    |> List2Array('F')
  PerRecruit@SProduction <- purrr::map(PRList, \(pr) pr@SProduction) |> List2Array('F')
  PerRecruit@Removals    <- purrr::map(PRList, \(pr) pr@Removals)    |> List2Array('F')
  PerRecruit@Landings    <- purrr::map(PRList, \(pr) pr@Landings)    |> List2Array('F')
  PerRecruit
}


CalcPerRecruit_F_scalar <- function(apicalF = 0.1,
                                    StockFleetAllocation,
                                    NaturalMortalityList,
                                    PlusGroupList,
                                    MaturityList,
                                    SemelparousList,
                                    WeightList,
                                    SpawnTimeFracList,
                                    SPFrom,
                                    SPR0List,
                                    FecundityList,
                                    WeightFleetList,
                                    SelectivityFleetList,
                                    RetentionFleetList,
                                    DiscardMortalityFleetList,
                                    FleetNames,
                                    Years,
                                    nSeason          = 1L,
                                    SeasonalWeightsList = NULL,
                                    CalendarYears    = NULL) {

  # Dispatch to seasonal implementation when there are multiple seasons.
  if (nSeason > 1L)
    return(CalcPerRecruit_F_scalar_seasonal(
      apicalF                   = apicalF,
      StockFleetAllocation      = StockFleetAllocation,
      NaturalMortalityList      = NaturalMortalityList,
      PlusGroupList             = PlusGroupList,
      MaturityList              = MaturityList,
      SemelparousList           = SemelparousList,
      WeightList                = WeightList,
      SpawnTimeFracList         = SpawnTimeFracList,
      SPFrom                    = SPFrom,
      SPR0List                  = SPR0List,
      FecundityList             = FecundityList,
      WeightFleetList           = WeightFleetList,
      SelectivityFleetList      = SelectivityFleetList,
      RetentionFleetList        = RetentionFleetList,
      DiscardMortalityFleetList = DiscardMortalityFleetList,
      FleetNames                = FleetNames,
      Years                     = Years,
      nSeason                   = nSeason,
      SeasonalWeightsList       = SeasonalWeightsList,
      CalendarYears             = CalendarYears
    ))

  apicalFAge     <- apicalF * StockFleetAllocation |>
    AddDimension("Age", pos = 3)
  apicalFAgeList <- Array2List(apicalFAge)
  
  FInteractList     <- purrr::map2(apicalFAgeList, SelectivityFleetList, ArrayMultiply)
  FRetainList       <- purrr::map2(FInteractList, RetentionFleetList, ArrayMultiply)
  FDiscardTotalList <- purrr::map2(FInteractList, FRetainList, ArraySubtract)
  FDiscardDeadList  <- purrr::map2(FDiscardTotalList, DiscardMortalityFleetList, ArrayMultiply)
  FDeadList         <- purrr::map2(FRetainList, FDiscardDeadList, ArraySum)
  FDeadTotalList    <- purrr::map(FDeadList, SumOverFleet)
  
  # max F over ages per stock (Sim x Stock x Year), then max over stocks
  ActualApicalFByStock <- purrr::map(
    FDeadTotalList,
    \(FDeadTotal) apply(FDeadTotal, setdnames(c('Sim', 'Year')), max)
  ) |>
    List2Array('Stock', pos = 2)
  
  ActualApicalF <- apply(ActualApicalFByStock, c('Sim', 'Year'), max)
  
  if (apicalF > 0 && any(abs(ActualApicalF / apicalF - 1) > 1E-2)) {
    # single scalar adjustment based on the controlling stock
    # (the one producing max F across the complex)
    apicalFSimTS <- array(
      apicalF,
      dim      = dim(ActualApicalF),
      dimnames = dimnames(ActualApicalF)
    )
    
    adjust <- ArrayDivide(apicalFSimTS, ActualApicalF) |>  # Sim x Year
      AddDimension("Stock", pos = 2) |>
      AddDimension("Age",   pos = 3) |>
      AddDimension("Fleet", pos = 5) |>
      ExtendFleets(Fleets = FleetNames) |>
      ExtendStocks(Stocks = names(NaturalMortalityList))
    
    FInteractList <- purrr::map2(FInteractList,Array2List(adjust, 'Stock'),
      ArrayMultiply)
    
    FRetainList       <- purrr::map2(FInteractList, RetentionFleetList,
                                     ArrayMultiply)
    FDiscardTotalList <- purrr::map2(FInteractList, FRetainList, 
                                     ArraySubtract)
    FDiscardDeadList  <- purrr::map2(
      FDiscardTotalList, DiscardMortalityFleetList, ArrayMultiply
    )
    FDeadList         <- purrr::map2(FRetainList, FDiscardDeadList, ArraySum)
    FDeadTotalList    <- purrr::map(FDeadList, SumOverFleet)
  }
  
  ZDeadTotalList <- purrr::map2(
    FDeadTotalList, NaturalMortalityList, ArraySum
  )
  
  NPR0List <- purrr::pmap(
    list(
      NaturalMortalityList, PlusGroupList, SemelparousList
    ),
    \(NaturalMortality, PlusGroup, Semelparous)
    CalcSurvival(
      NaturalMortality, FishingMortality  = NULL,
      PlusGroup, SpawnTimeFrac = 0, Semelparous
    )
  )
  
  NPRFList <- purrr::pmap(
    list(
      NaturalMortalityList, FDeadTotalList,
      PlusGroupList, SemelparousList
    ),
    \(NaturalMortality, FishingMortality, PlusGroup, Semelparous)
    CalcSurvival(
      NaturalMortality, FishingMortality,
      PlusGroup, SpawnTimeFrac = 0, Semelparous
    )
  )

  IsSpawnTimeFrac <- any(unlist(SpawnTimeFracList)!=0)
  if (IsSpawnTimeFrac) {
    
    NPR0_SPList <- purrr::pmap(
      list(
        NaturalMortalityList, PlusGroupList,SemelparousList, SpawnTimeFracList),
      \(NaturalMortality, PlusGroup, Semelparous, SpawnTimeFrac)
      CalcSurvival(
        NaturalMortality, FishingMortality = NULL,
        PlusGroup, SpawnTimeFrac, Semelparous
      )
    )
    
    NPRF_SPList <- purrr::pmap(
      list(
        NaturalMortalityList, FDeadTotalList, PlusGroupList,
        SemelparousList, SpawnTimeFracList
      ),
      \(NaturalMortality, FishingMortality,
        PlusGroup, Semelparous, SpawnTimeFrac)
      CalcSurvival(
        NaturalMortality, FishingMortality,
        PlusGroup, SpawnTimeFrac, Semelparous
      )
    )
  } else {
    NPR0_SPList <- NPR0List
    NPRF_SPList <- NPRFList
  }

  # SPR
  SPRFList <- purrr::map2(
    NPRF_SPList, FecundityList,
    \(NPRF_SP, Fecundity) {
      SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> SumOverAge()
      if (!is.array(SPRF))
        SPRF <- array(SPRF, length(SPRF), dimnames = list(Year = Years))
      SPRF
    }
  )
  if (length(SPRFList)> 1)
    SPRFList         <- SPRFList[SPFrom]
  names(SPRFList)  <- names(NPRFList)

  SPR <- purrr::map2(SPRFList, SPR0List, \(SPRF, SPR0) 
                     ArrayDivide(SPRF, SPR0)) |>
    List2Array('Stock') |>
    ArraySubsetYear(Years)
  
  # Removals
  FishingDeadList <- purrr::map2(
    FDeadList, ZDeadTotalList,
    \(FDead, ZDeadTotal) {
      ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') |>
        ExtendFleets(Fleets = FleetNames)
      ArrayDivide(FDead, ZDeadTotalFleet)
    }
  )
  names(FishingDeadList) <- names(NaturalMortalityList)
  
  NDeadList <- purrr::map2(
    NPRFList, ZDeadTotalList,
    \(NPRF, ZDeadTotal) ArrayMultiply(NPRF, (1 - exp(-ZDeadTotal)))
  )
  
  Removals <- purrr::pmap(
    list(FishingDeadList, NDeadList, WeightFleetList),
    \(FishingDead, NDead, WeightFleet) {
      NDeadFleet <- AddDimension(NDead, 'Fleet') |>
        ExtendFleets(Fleets = FleetNames)
      ArrayMultiply(FishingDead, NDeadFleet) |>
        ArrayMultiply(WeightFleet) |>
        SumOverFleet() |>
        SumOverAge()
    }
  ) |>
    List2Array('Stock', pos = 2)
  
  # Landings
  FishingRetainList <- purrr::map2(
    FRetainList, ZDeadTotalList,
    \(FRetain, ZDeadTotal) {
      ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') |>
        ExtendFleets(Fleets = FleetNames)
      ArrayDivide(FRetain, ZDeadTotalFleet)
    }
  )
  names(FishingRetainList) <- names(NaturalMortalityList)
  
  Landings <- purrr::pmap(
    list(FishingRetainList, NDeadList, WeightFleetList),
    \(FishingRetain, NDead, WeightFleet) {
      NDeadFleet <- AddDimension(NDead, 'Fleet') |>
        ExtendFleets(Fleets = FleetNames)
      ArrayMultiply(FishingRetain, NDeadFleet) |>
        ArrayMultiply(WeightFleet) |>
        SumOverFleet() |>
        SumOverAge()
    }
  ) |>
    List2Array('Stock', pos = 2)
  
  # Biomass, Spawning Biomass, and Spawning Production
  Biomass <- purrr::map2(
    NPRFList, WeightList,
    \(NPRF, Weight) ArrayMultiply(NPRF, Weight) |> SumOverAge()
  ) |>
    List2Array("Stock", pos = 2)
  
  SBiomass <- purrr::pmap(
    list(NPRF_SPList, WeightList, MaturityList),
    \(NPRF_SP, Weight, Maturity)
    ArrayMultiply(NPRF_SP, Weight) |>
      ArrayMultiply(Maturity) |>
      SumOverAge()
  ) |>
    List2Array("Stock", pos = 2)
  
  SProduction <- purrr::map2(
    NPRF_SPList, FecundityList,
    \(NPRF_SP, Fecundity) ArrayMultiply(NPRF_SP, Fecundity) |> SumOverAge()
  ) |>
    List2Array("Stock", pos = 2)
  
  NPR0    <- purrr::map(NPR0List, SumOverAge) |> List2Array("Stock", pos = 2)
  NPRF    <- purrr::map(NPRFList, SumOverAge) |> List2Array("Stock", pos = 2)
  
  NPR0_SP <- purrr::map(NPR0_SPList, SumOverAge) |> List2Array("Stock", pos = 2)
  NPRF_SP <- purrr::map(NPRF_SPList, SumOverAge) |> List2Array("Stock", pos = 2)
  
  SPR0    <- SPR0List |> List2Array("Stock", pos = 2)
  SPRF    <- SPRFList |> List2Array("Stock", pos = 2)
  
  PerRecruit             <- new('perrecruit')
  PerRecruit@NPR0        <- NPR0
  PerRecruit@NPR0_SP     <- if (IsSpawnTimeFrac) NPR0_SP else NPR0
  PerRecruit@apicalF     <- apicalF
  PerRecruit@SPR0        <- SPR0
  PerRecruit@NPRF        <- NPRF
  PerRecruit@NPRF_SP     <- if (IsSpawnTimeFrac) NPRF_SP else NPRF
  PerRecruit@SPRF        <- SPRF
  PerRecruit@SPR         <- SPR
  PerRecruit@Biomass     <- Biomass
  PerRecruit@SBiomass    <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals    <- Removals
  PerRecruit@Landings    <- Landings
  PerRecruit
}



CalcPerRecruit_F_scalar_seasonal <- function(apicalF,
                                             StockFleetAllocation,
                                             NaturalMortalityList,
                                             PlusGroupList,
                                             MaturityList,
                                             SemelparousList,
                                             WeightList,
                                             SpawnTimeFracList,
                                             SPFrom,
                                             SPR0List,
                                             FecundityList,
                                             WeightFleetList,
                                             SelectivityFleetList,
                                             RetentionFleetList,
                                             DiscardMortalityFleetList,
                                             FleetNames,
                                             Years,
                                             nSeason,
                                             SeasonalWeightsList,
                                             CalendarYears) {

  nStocks    <- length(NaturalMortalityList)
  nCalYears  <- length(CalendarYears)
  StockNames <- names(NaturalMortalityList)

  # Fleet F by stock × age × season × fleet
  # StockFleetAllocation: [Sim, Stock, Year(nSeason), Fleet]
  apicalFAge     <- apicalF * StockFleetAllocation |> AddDimension("Age", pos = 3)
  apicalFAgeList <- Array2List(apicalFAge)      # list by Stock, each [Sim, Age(1), Year, Fleet]

  FInteractList     <- purrr::map2(apicalFAgeList, SelectivityFleetList, ArrayMultiply)
  FRetainList       <- purrr::map2(FInteractList, RetentionFleetList,     ArrayMultiply)
  FDiscardTotalList <- purrr::map2(FInteractList, FRetainList,            ArraySubtract)
  FDiscardDeadList  <- purrr::map2(FDiscardTotalList, DiscardMortalityFleetList, ArrayMultiply)
  FDeadList         <- purrr::map2(FRetainList, FDiscardDeadList,         ArraySum)
  FDeadTotalList    <- purrr::map(FDeadList, SumOverFleet)   # [Sim, Age, Year(nSeason)]

  # Apical-F normalisation (same logic as annual path)
  ActualApicalFByStock <- purrr::map(FDeadTotalList, \(FDeadTotal)
    apply(FDeadTotal, setdnames(c('Sim', 'Year')), max)
  ) |> List2Array('Stock', pos = 2)

  ActualApicalF <- apply(ActualApicalFByStock, c('Sim', 'Year'), max)

  if (apicalF > 0 && any(abs(ActualApicalF / apicalF - 1) > 1E-2)) {
    apicalFSimTS <- array(apicalF, dim = dim(ActualApicalF), dimnames = dimnames(ActualApicalF))
    adjust <- ArrayDivide(apicalFSimTS, ActualApicalF) |>
      AddDimension("Stock", pos = 2) |>
      AddDimension("Age",   pos = 3) |>
      AddDimension("Fleet", pos = 5) |>
      ExtendFleets(Fleets = FleetNames) |>
      ExtendStocks(Stocks = StockNames)

    FInteractList     <- purrr::map2(FInteractList, Array2List(adjust, 'Stock'), ArrayMultiply)
    FRetainList       <- purrr::map2(FInteractList, RetentionFleetList,         ArrayMultiply)
    FDiscardTotalList <- purrr::map2(FInteractList, FRetainList,                ArraySubtract)
    FDiscardDeadList  <- purrr::map2(FDiscardTotalList, DiscardMortalityFleetList, ArrayMultiply)
    FDeadList         <- purrr::map2(FRetainList, FDiscardDeadList,             ArraySum)
    FDeadTotalList    <- purrr::map(FDeadList, SumOverFleet)
  }

  ZDeadTotalList <- purrr::map2(FDeadTotalList, NaturalMortalityList, ArraySum)

  IsSpawnTimeFrac <- any(unlist(SpawnTimeFracList) != 0)

  # Sim names from the first stock's M array (consistent across stocks)
  SimNames <- dimnames(NaturalMortalityList[[1]])[['Sim']]

  # For each calendar year, compute annual per-recruit quantities
  perYear <- purrr::map(seq_len(nCalYears), \(cy) {

    ts_idx <- ((cy - 1L) * nSeason + 1L):(cy * nSeason)   # season indices within Years

    purrr::pmap(
      list(
        ZDeadTotalList, NaturalMortalityList, PlusGroupList,
        SemelparousList, SpawnTimeFracList, FecundityList, WeightList,
        MaturityList, FDeadList, FDeadTotalList, FRetainList,
        WeightFleetList, SPR0List, SeasonalWeightsList
      ),
      \(Z_full, M_full, PlusGroup, Semel_full, STF, Fec_full, W_full,
        Mat_full, FDead_full, FDeadTot_full, FRetain_full,
        WF_full, SPR0_full, pi_s_full) {

        nSim <- dim(Z_full)[1]
        nAge <- dim(Z_full)[2]

        seas_ts <- Years[ts_idx]  # seasonal time step labels for this calendar year

        # Extract this calendar year's seasons: [Sim, Age, nSeason]
        Z_cy     <- Z_full[,  , ts_idx, drop = FALSE]
        M_cy     <- M_full[,  , ts_idx, drop = FALSE]
        Fec_cy   <- Fec_full[, , ts_idx, drop = FALSE]
        W_cy     <- W_full[,  , ts_idx, drop = FALSE]
        Mat_cy   <- Mat_full[, , ts_idx, drop = FALSE]
        Semel_cy <- if (is.array(Semel_full)) Semel_full[, , ts_idx, drop=FALSE] else FALSE

        # pi_s: seasonal recruitment weights [Sim, nSeason]
        pi_s_cy_arr <- ArraySubsetYear(pi_s_full, seas_ts)
        pi_s_cy     <- matrix(as.numeric(pi_s_cy_arr), nrow = nSim)

        # Seasonal SPR0 [Sim, nSeason], weighted average gives annual SPR0 [Sim]
        SPR0_cy_arr <- ArraySubsetYear(SPR0_full, seas_ts)  # [Sim, nSeason]
        SPR0_cy_mat <- matrix(as.numeric(SPR0_cy_arr), nrow = nSim)
        SPR0_ann    <- rowSums(pi_s_cy * SPR0_cy_mat)

        # NPR: F=0 (abundances), unfished
        Z0_cy    <- M_cy
        NPR0_no  <- CalcNPR_seasonal(Z0_cy, PlusGroup, 0,   Semel_cy)
        NPR0_sp  <- if (IsSpawnTimeFrac)
          CalcNPR_seasonal(Z0_cy, PlusGroup, STF, Semel_cy) else NPR0_no

        # NPR: with fishing
        NPRF_no  <- CalcNPR_seasonal(Z_cy, PlusGroup, 0,   Semel_cy)
        NPRF_sp  <- if (IsSpawnTimeFrac)
          CalcNPR_seasonal(Z_cy, PlusGroup, STF, Semel_cy) else NPRF_no

        # Annual aggregate quantities: [nSim] scalars
        ones_cy <- array(1, dim(Z_cy))   # for summing NPR over ages without weighting

        NPR0_ann    <- AggSeasonalProduct(NPR0_no, ones_cy, pi_s_cy)
        NPR0_SP_ann <- AggSeasonalProduct(NPR0_sp, ones_cy, pi_s_cy)
        NPRF_ann    <- AggSeasonalProduct(NPRF_no, ones_cy, pi_s_cy)
        NPRF_SP_ann <- AggSeasonalProduct(NPRF_sp, ones_cy, pi_s_cy)

        SPR0f_ann   <- AggSeasonalProduct(NPR0_sp, Fec_cy,  pi_s_cy)   # unfished SP per recruit
        SPRFf_ann   <- AggSeasonalProduct(NPRF_sp, Fec_cy,  pi_s_cy)   # fished SP per recruit
        SPR_ann     <- SPRFf_ann / pmax(SPR0f_ann, .Machine$double.eps)

        Biomass_ann  <- AggSeasonalProduct(NPRF_no, W_cy, pi_s_cy)
        SBiomass_ann <- AggSeasonalProduct(NPRF_sp, W_cy * Mat_cy, pi_s_cy)

        # Fleet arrays [Sim, Age, Season, Fleet] — extract for yield
        FDead_cy   <- FDead_full[,   , ts_idx, , drop = FALSE]
        FRetain_cy <- FRetain_full[, , ts_idx, , drop = FALSE]
        ZTot_cy    <- FDeadTot_full[, , ts_idx, drop = FALSE]
        WF_cy      <- WF_full[,      , ts_idx, , drop = FALSE]

        Removals_ann <- AggSeasonalYield(NPRF_no, FDead_cy, ZTot_cy, WF_cy,
                                          pi_s_cy, type = 'Removals')
        Landings_ann <- AggSeasonalYield(NPRF_no, FDead_cy, ZTot_cy, WF_cy,
                                          pi_s_cy, type = 'Landings', FRetain_saf = FRetain_cy)

        list(
          NPR0        = NPR0_ann,
          NPR0_SP     = NPR0_SP_ann,
          SPR0_ann    = SPR0_ann,    # for SPR denominator
          NPRF        = NPRF_ann,
          NPRF_SP     = NPRF_SP_ann,
          SPR0f       = SPR0f_ann,
          SPRFf       = SPRFf_ann,
          SPR         = SPR_ann,
          Biomass     = Biomass_ann,
          SBiomass    = SBiomass_ann,
          SProduction = SPRFf_ann,
          Removals    = Removals_ann,
          Landings    = Landings_ann
        )
      }
    )  # list over stocks, each a named list of [nSim] vectors
  })   # list over calendar years

  # Assemble into [Sim, Stock, Year(nCalYears)] arrays
  WrapSlot <- function(slot_name) {
    # Build [Sim, Stock, Year(nCalYears)]
    nSim_ws <- length(SimNames)
    vals <- array(
      unlist(lapply(seq_len(nCalYears), \(cy)
        lapply(seq_len(nStocks), \(st)
          perYear[[cy]][[st]][[slot_name]]))),
      dim      = c(nSim_ws, nStocks, nCalYears),
      dimnames = list(Sim = SimNames, Stock = StockNames, Year = CalendarYears)
    )
    vals
  }

  # Assemble named slots: each is [Sim, Stock, Year(nCalYears)]
  NPR0        <- WrapSlot('NPR0')
  NPR0_SP     <- WrapSlot('NPR0_SP')
  NPRF        <- WrapSlot('NPRF')
  NPRF_SP     <- WrapSlot('NPRF_SP')
  SPR0f       <- WrapSlot('SPR0f')    # unfished SP per recruit, per stock
  SPRFf       <- WrapSlot('SPRFf')    # fished SP per recruit, per stock
  Biomass     <- WrapSlot('Biomass')
  SBiomass    <- WrapSlot('SBiomass')
  SProduction <- WrapSlot('SProduction')
  Removals    <- WrapSlot('Removals')
  Landings    <- WrapSlot('Landings')

  # Apply SPFrom: SPR[i] = SPRFf[SPFrom[i]] / SPR0f[SPFrom[i]]
  SPR <- array(0, dim = dim(SPRFf), dimnames = dimnames(SPRFf))
  eps <- .Machine$double.eps
  for (i in seq_len(nStocks)) {
    spf <- SPFrom[i]
    SPR[, i, ] <- SPRFf[, spf, ] / pmax(SPR0f[, spf, ], eps)
  }

  # Annual apical F: for each calendar year, sum seasonal F_dead rates at the
  # apical age across all seasons.  
  F_annual_apical <- purrr::map(seq_len(nCalYears), \(cy) {
    ts_idx <- ((cy - 1L) * nSeason + 1L):(cy * nSeason)
    f <- purrr::map(FDeadTotalList, \(FDT) {
      annual_by_age <- apply(FDT[, , ts_idx, drop = FALSE], c(1L, 2L), sum)
      apply(annual_by_age, 1L, max)
    })
    base::Reduce(pmax, f)  # max over stocks
  })

  PerRecruit             <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- NPR0
  PerRecruit@NPR0_SP     <- NPR0_SP
  PerRecruit@SPR0        <- SPR0f
  PerRecruit@NPRF        <- NPRF
  PerRecruit@NPRF_SP     <- if (IsSpawnTimeFrac) NPRF_SP else NPRF
  PerRecruit@SPRF        <- SPRFf
  PerRecruit@SPR         <- SPR
  PerRecruit@Biomass     <- Biomass
  PerRecruit@SBiomass    <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals    <- Removals
  PerRecruit@Landings    <- Landings
  PerRecruit@Misc        <- list(F_annual_apical = F_annual_apical)
  PerRecruit
}



PrepPerRecruitInputs <- function(StockList, FleetList, SPR0List, Years) {

  FleetNames <- names(FleetList[[1]])

  M_ref  <- StockList[[1]]@NaturalMortality@MeanAtAge
  all_ts <- as.numeric(dimnames(M_ref)[['Year']])

  if (!is.null(all_ts)) {
    nSeason_model <- as.integer(round(length(all_ts) / length(unique(floor(all_ts)))))
    if (nSeason_model > 1L) {
      
      cal_yrs_req <- unique(floor(Years))
      Years_expanded <- all_ts[floor(all_ts) %in% cal_yrs_req]
      if (length(Years_expanded) == 0L)
        cli::cli_abort(c("No seasonal time steps found for the requested {.arg Years}.",
                         "i" = "Provide integer calendar years present in the operating model."))
      if (!all(Years == floor(Years)))
        cli::cli_warn(c("Decimal {.arg Years} supplied to a seasonal model.",
                        "i" = "Snapping to calendar year(s): {.val {cal_yrs_req}}."))
      Years <- Years_expanded
    } else {
      
      seasonal_ts <- all_ts[floor(all_ts) %in% floor(Years)]
      if (length(seasonal_ts) >= length(Years))
        Years <- seasonal_ts
    }
  }

  cal_years <- unique(floor(Years))
  nSeason   <- as.integer(length(Years) / length(cal_years))

  FleetNames <- names(FleetList[[1]])

  NaturalMortalityList <- purrr::map(StockList, \(Stock)
                                     Stock@NaturalMortality@MeanAtAge |> ArraySubsetYear(Years))
  
  PlusGroupList <- purrr::map(StockList, \(Stock) Stock@Ages@PlusGroup)
  
  MaturityList <- purrr::map(StockList, \(Stock)
                             Stock@Maturity@MeanAtAge |> ArraySubsetYear(Years))
  
  SemelparousList <- purrr::map(StockList, \(Stock)
                                Stock@Maturity@Semelparous |> ArraySubsetYear(Years))
  
  WeightList <- purrr::map(StockList, \(Stock)
                           Stock@Weight@MeanAtAge |> ArraySubsetYear(Years))
  
  SpawnTimeFracList <- purrr::map(StockList, \(Stock) Stock@SRR@SpawnTimeFrac)
  
  FecundityList <- purrr::map(StockList, \(Stock)
                              Stock@Fecundity@MeanAtAge |> ArraySubsetYear(Years))
  
  SPFrom <- purrr::imap(StockList, \(stock, i) {
    spfrom <- stock@SRR@SPFrom
    if (is.null(spfrom))      spfrom <- i
    if (is.character(spfrom)) spfrom <- match(spfrom, names(StockList))
    spfrom
  }) |> unlist()
  
  # F-invariant: fleet allocation depends only on effort and efficiency
  StockFleetAllocation <- purrr::map(FleetList, \(fl)
                                     CalcFleetAllocationF(fl, Years)
  ) |> List2Array('Stock', pos = 2)
  
  WeightFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) {
      Fleet@WeightFleet |> ArraySubsetYear(Years)
    }) |>
      List2Array(pos = 4)
  })
  
  SelectivityFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@Selectivity@MeanAtAge |> ArraySubsetYear(Years)) |>
      List2Array(pos = 4) |>
      CheckSpatial('Selectivity')
  })
  
  RetentionFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@Retention@MeanAtAge |> ArraySubsetYear(Years)) |>
      List2Array(pos = 4) |> 
      CheckSpatial('Retention')
  }) 
  
  DiscardMortalityFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@DiscardMortality@MeanAtAge |> ArraySubsetYear(Years)) |>
      List2Array(pos = 4) |> 
      CheckSpatial('DiscardMortality')
  }) 
  
  # SRR quantities needed for MSY recruitment scaling
  # For seasonal models, collapse to annual R0 and SPR0 for the equilibrium
  # scaling
  R0SeasonalList <- purrr::map(StockList, \(Stock)
    Stock@SRR@R0 |> ArraySubsetYear(Years)   # [Sim, Year(nSeason)]
  )

  if (nSeason > 1L) {
    nCalYears <- length(cal_years)

    # Seasonal weights pi_s[sim, season] — proportion of annual R0 in each season
    SeasonalWeightsList <- purrr::map(R0SeasonalList, \(R0_s) {
      nSim <- dim(R0_s)[1]
      # Sum per calendar year to get annual R0
      R0_mat <- matrix(as.numeric(R0_s), nrow = nSim)     # [Sim, nSeason*nCalYears]
      R0_ann <- matrix(0, nrow = nSim, ncol = nCalYears)
      for (cy in seq_len(nCalYears))
        R0_ann[, cy] <- rowSums(R0_mat[, ((cy-1)*nSeason + 1):(cy*nSeason), drop=FALSE])
      # Expand annual back to seasonal denominator
      R0_ann_rep <- R0_mat  # same shape
      for (cy in seq_len(nCalYears))
        R0_ann_rep[, ((cy-1)*nSeason + 1):(cy*nSeason)] <- R0_ann[, cy]
      pi <- R0_mat / pmax(R0_ann_rep, .Machine$double.eps)
      array(pi, dim = dim(R0_s), dimnames = dimnames(R0_s))
    })

    # Annual R0 [Sim, Stock, Year(nCalYears)]
    R0 <- purrr::map(R0SeasonalList, \(R0_s) {
      nSim <- dim(R0_s)[1]
      R0_mat <- matrix(as.numeric(R0_s), nrow = nSim)
      R0_ann <- matrix(0, nrow = nSim, ncol = nCalYears,
                       dimnames = list(Sim = dimnames(R0_s)$Sim, Year = cal_years))
      for (cy in seq_len(nCalYears))
        R0_ann[, cy] <- rowSums(R0_mat[, ((cy-1)*nSeason + 1):(cy*nSeason), drop=FALSE])
      R0_ann
    }) |> List2Array('Stock') |> aperm(c('Sim', 'Stock', 'Year'))

    RecParsList <- purrr::map2(StockList, SPR0List, \(Stock, SPR0) {
      sname <- Stock@Name
      nSim  <- dim(R0SeasonalList[[sname]])[1]
      pi_s  <- SeasonalWeightsList[[sname]]           # [Sim, nSeason*nCalYears]
      R0_mat  <- matrix(as.numeric(R0SeasonalList[[sname]]), nrow = nSim)
      SPR0_mat <- matrix(as.numeric(SPR0 |> ArraySubsetYear(Years)), nrow = nSim)
      pi_mat  <- matrix(as.numeric(pi_s), nrow = nSim)

      # Per-calendar-year R0 and weighted-average SPR0
      R0_cy   <- matrix(0, nrow = nSim, ncol = nCalYears,
                        dimnames = list(Sim = dimnames(pi_s)$Sim, Year = cal_years))
      SPR0_cy <- matrix(0, nrow = nSim, ncol = nCalYears,
                        dimnames = list(Sim = dimnames(pi_s)$Sim, Year = cal_years))
      for (cy in seq_len(nCalYears)) {
        idx <- ((cy - 1L) * nSeason + 1L):(cy * nSeason)
        R0_cy[,   cy] <- rowSums(R0_mat[,  idx, drop = FALSE])
        SPR0_cy[, cy] <- rowSums(pi_mat[,  idx, drop = FALSE] *
                                 SPR0_mat[, idx, drop = FALSE])
      }

      Pars <- purrr::map(Stock@SRR@Pars, \(pars) {
        # SRR parameters (e.g. steepness) don't vary by season; take one per year
        pars_s <- ArraySubsetYear(pars, Years)
        pars_s[, seq(1L, nSeason * nCalYears, by = nSeason), drop = FALSE]
      })
      Pars$R0   <- R0_cy
      Pars$SPR0 <- SPR0_cy
      Pars
    })
  } else {
    SeasonalWeightsList <- NULL

    R0 <- R0SeasonalList |> List2Array('Stock') |> aperm(c('Sim', 'Stock', 'Year'))

    RecParsList <- purrr::map2(StockList, SPR0List, \(Stock, SPR0) {
      Pars      <- purrr::map(Stock@SRR@Pars, \(pars) ArraySubsetYear(pars, Years))
      Pars$R0   <- ArraySubsetYear(Stock@SRR@R0, Years)
      Pars$SPR0 <- ArraySubsetYear(SPR0, Years)
      Pars
    })
  }
  if (length(RecParsList)> 1) {
    RecParsList <- RecParsList[SPFrom]
    names(RecParsList) <- names(SPFrom)
  }

  RelRecFunList <- purrr::map(StockList, \(Stock) {
    if (!is.null(Stock@SRR@Model) && inherits(Stock@SRR@Model, 'character')) {
      if (is.null(Stock@SRR@RelRecFun)) {
        mod <- get(paste0(Stock@SRR@Model, 'RelRec'))
        class(mod) <- 'function'
        Stock@SRR@RelRecFun <- mod
      }
    }
    if (inherits(Stock@SRR@RelRecFun, 'character'))
      Stock@SRR@RelRecFun <- get(Stock@SRR@RelRecFun)
    Stock@SRR@RelRecFun
  })

  list(
    NaturalMortalityList      = NaturalMortalityList,
    PlusGroupList             = PlusGroupList,
    MaturityList              = MaturityList,
    SemelparousList           = SemelparousList,
    WeightList                = WeightList,
    SpawnTimeFracList         = SpawnTimeFracList,
    FecundityList             = FecundityList,
    SPFrom                    = SPFrom,
    SPR0List                  = SPR0List,
    StockFleetAllocation      = StockFleetAllocation,
    WeightFleetList           = WeightFleetList,
    SelectivityFleetList      = SelectivityFleetList,
    RetentionFleetList        = RetentionFleetList,
    DiscardMortalityFleetList = DiscardMortalityFleetList,
    RecParsList               = RecParsList,
    R0                        = R0,
    RelRecFunList             = RelRecFunList,
    FleetNames                = FleetNames,
    Years                     = Years,
    nSeason                   = nSeason,
    SeasonalWeightsList       = SeasonalWeightsList,
    CalendarYears             = cal_years
  )
}

