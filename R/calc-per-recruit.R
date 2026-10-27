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
  .CheckClass(OM, c('om', 'hist'))

  if (inherits(OM, 'om'))
    Hist <- .OM2Hist(OM, silent=TRUE)

  if (inherits(OM, 'hist'))
    Hist <- OM

  .CheckClass(Hist, 'hist', 'Hist')

  nSeason <- Hist@OM@Seasons
  RefSeason      <- Hist@OM@RefSeason
  RefEffortYears <- Hist@OM@RefEffortYears

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
    .CalcPerRecruitStockList(
      StockList      = Hist@OM@Stock[stockInd],
      FleetList      = Hist@OM@Fleet[stockInd],
      apicalF        = apicalF,
      Years          = Years,
      SPR0List       = SPR0List[stockInd],
      RefSeason      = RefSeason,
      RefEffortYears = RefEffortYears,
      Herm           = .HermForStocks(Hist@OM, StockNames[stockInd])
    )
  })
  
  PerRecruit <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- purrr::map(PRByComplex, \(pr) pr@NPR0)        |> .JoinStockArrays(StockNames)
  PerRecruit@NPR0_SP     <- purrr::map(PRByComplex, \(pr) pr@NPR0_SP)     |> .JoinStockArrays(StockNames)
  PerRecruit@SPR0        <- purrr::map(PRByComplex, \(pr) pr@SPR0)        |> .JoinStockArrays(StockNames)
  PerRecruit@NPRF        <- purrr::map(PRByComplex, \(pr) pr@NPRF)        |> .JoinStockArrays(StockNames)
  PerRecruit@NPRF_SP     <- purrr::map(PRByComplex, \(pr) pr@NPRF_SP)     |> .JoinStockArrays(StockNames)
  PerRecruit@SPRF        <- purrr::map(PRByComplex, \(pr) pr@SPRF)        |> .JoinStockArrays(StockNames)
  PerRecruit@SPR         <- purrr::map(PRByComplex, \(pr) pr@SPR)         |> .JoinStockArrays(StockNames)
  PerRecruit@Biomass     <- purrr::map(PRByComplex, \(pr) pr@Biomass)     |> .JoinStockArrays(StockNames)
  PerRecruit@SBiomass    <- purrr::map(PRByComplex, \(pr) pr@SBiomass)    |> .JoinStockArrays(StockNames)
  PerRecruit@SProduction <- purrr::map(PRByComplex, \(pr) pr@SProduction) |> .JoinStockArrays(StockNames)
  PerRecruit@Removals    <- purrr::map(PRByComplex, \(pr) pr@Removals)    |> .JoinStockArrays(StockNames)
  PerRecruit@Landings    <- purrr::map(PRByComplex, \(pr) pr@Landings)    |> .JoinStockArrays(StockNames)
  HermCross <- purrr::map(unname(PRByComplex), \(pr) pr@Misc$HermCross) |>
    purrr::compact() |> purrr::list_flatten()
  if (length(HermCross)) PerRecruit@Misc$HermCross <- HermCross
  PerRecruit
}

# Lifetime per-recruit quantity: for each stock receiving recruits, add the
# amounts accrued after transitioning to other Herm-linked stocks
.AddHermLifetime <- function(Arr, HermCross, Q) {
  if (!length(HermCross)) return(Arr)
  DimNames <- names(dimnames(Arr))
  Out <- .Aperm(Arr, c('Sim', 'Stock', 'Year', 'F'))
  for (k in names(HermCross)) {
    Cross <- .Aperm(HermCross[[k]][[Q]], c('Sim', 'Stock', 'Year', 'F'))
    Total <- apply(Cross, c(1, 3, 4), sum) |> AddDimension('Stock', val = k, pos = 2)
    Slice <- Out[, k, , , drop = FALSE]
    Ones  <- Slice
    Ones[] <- 1
    Out[, k, , ] <- Slice + .AlignToTemplate(Total, Ones)
  }
  .Aperm(Out, DimNames)
}

.CalcPerRecruitStockList <- function(StockList, FleetList, apicalF=0.1, Years, SPR0List,
                                     RefSeason = NULL, RefEffortYears = NULL, Herm = NULL) {

  inputs <- .PrepPerRecruitInputs(StockList, FleetList, SPR0List, Years,
                                  EffortYears = RefEffortYears, RefSeason = RefSeason,
                                  Herm = Herm)

  PR <- .CalcPerRecruitF(
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
    WeightFleetRetainedList   = inputs$WeightFleetRetainedList,
    WeightFleetSelectedList   = inputs$WeightFleetSelectedList,
    SelectivityFleetList      = inputs$SelectivityFleetList,
    RetentionFleetList        = inputs$RetentionFleetList,
    DiscardMortalityFleetList = inputs$DiscardMortalityFleetList,
    FleetNames                = inputs$FleetNames,
    Years                     = inputs$Years,
    nSeason                   = inputs$nSeason,
    SeasonalWeightsList       = inputs$SeasonalWeightsList,
    CalendarYears             = inputs$CalendarYears,
    RefSeason                 = RefSeason,
    NPR0List                  = inputs$NPR0List,
    NPR0_SPList               = inputs$NPR0_SPList,
    Herm                      = inputs$Herm
  )

  PR
}

.JoinStockArrays <- function(arrayList, StockNames) {
  
  arrayList <- purrr::compact(arrayList)
  if (length(arrayList) == 0) return(NULL)
  
  StockInd <- match("Stock", names(dimnames(arrayList[[1]])))
  if (is.na(StockInd))
    cli::cli_abort("`Stock` dimension not found in arrays passed to `.JoinStockArrays()`",
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
  
  .ArraySubsetStock(combined, match(StockNames, currentStocks))
}

.CalcFleetAllocationF <- function(FleetList, Years, EffortYears = NULL) {

  .BuildFDistribution <- \(yrs) purrr::map(FleetList, \(Fleet) {
    ArrayMultiply(Fleet@Effort@Effort |>  .ArraySubsetYear(yrs),
                  Fleet@Catchability@Efficiency |>  .ArraySubsetYear(yrs))
  }) |>
    List2Array('Fleet', pos=3)

  if (is.null(EffortYears)) {
    FDistribution <- .BuildFDistribution(Years)
  } else {
    # Decouple the seasonal effort *shape* from the biology reference year:
    # average Effort x Efficiency across the requested calendar year(s),
    # matching season-of-year position, before peak-season normalisation.
    all_ts  <- as.numeric(dimnames(FleetList[[1]]@Effort@Effort)[['Year']])
    cal_yrs <- unique(floor(EffortYears))

    FDistList <- purrr::map(cal_yrs, \(cy) .BuildFDistribution(all_ts[floor(all_ts) %in% cy]))
    FDistribution <- Reduce(`+`, FDistList) / length(FDistList)
    dimnames(FDistribution)[['Year']] <- as.character(Years)
  }

  FDistributionTotal <- SumOverFleet(FDistribution)  # [Sim, Year]
  nYear <- Dim(FDistributionTotal)[['Year']]

  if (nYear > 1L) {
    # Seasonal: normalise by the peak-season total so apicalF = peak-season F.
    # Preserves the seasonal effort pattern while keeping the scale correct.
    FPeak <- apply(FDistributionTotal, 1L, max)   # max over seasons, [Sim]
    FPeak <- pmax(FPeak, .Machine$double.eps)
    sweep(FDistribution, 1L, FPeak, "/")          # [Sim, Year, Fleet]
  } else {
    FDistributionTotal <- AddDimension(FDistributionTotal, 'Fleet') |>
      .ExtendFleets(Fleets = names(FleetList))
    dimnames(FDistributionTotal)[['Fleet']] <- names(FleetList)
    ArrayDivide(FDistribution, FDistributionTotal)
  }
}

# Seasonal per-recruit helpers

# Numbers-per-recruit by birth season. Returns NPR[Sim, Age, BirthSeason];
# SpawnTimeFrac > 0 gives numbers at the time of spawning, 0 gives numbers at
# the start of each step.
# Seasonal models: plus-group fish pass through every season, so the plus age
# is carried forward as nSeason - 1 extra age classes, one per later season.
.ExtendPlusGroupSeasons <- function(arr, nSeason, nAge) {
  if (!is.array(arr)) return(arr)
  dn      <- dimnames(arr)
  age_pos <- match('Age', names(dn))
  if (is.na(age_pos) || dim(arr)[age_pos] != nAge) return(arr)

  idx_list          <- rep(list(quote(expr = )), length(dim(arr)))
  idx_list[[age_pos]] <- c(seq_len(nAge), rep(nAge, nSeason - 1L))
  out <- do.call('[', c(list(arr), idx_list, list(drop = FALSE)))
  dn[[age_pos]] <- c(dn[[age_pos]], paste0(dn[[age_pos]][nAge], '_', seq_len(nSeason - 1L)))
  dimnames(out) <- dn
  out
}

.CalcNPRSeasonal <- function(Z, PlusGroup, SpawnTimeFrac, Semelparous) {

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

    # Plus group: the last nSeason ages are one annual cycle of the plus age
    # (see .ExtendPlusGroupSeasons()), repeated with full-cycle survival
    if (PlusGroup) {
      block <- (nAge - nSeason + 1L):nAge
      Step  <- function(a_from, a_to) {
        exp(-(Z[, a_from, seas_at_age[a_from]] * (1 - SpawnTimeFrac) +
              Z[, a_to,   seas_at_age[a_to]]   *      SpawnTimeFrac)) *
          (1 - Semelparous[, a_from, seas_at_age[a_from]])
      }
      CycleSurv <- Step(nAge, block[1])
      for (a in block[-1]) CycleSurv <- CycleSurv * Step(a - 1L, a)
      denom <- 1 - CycleSurv
      ok    <- denom > .Machine$double.eps
      NPR[ok, block, s] <- NPR[ok, block, s] / denom[ok]
    }
  }

  NPR   # [Sim, Age, BirthSeason]
}

# Joint numbers-per-recruit for a Herm-linked group of stocks over one cycle of
# nSeason steps (nSeason = 1 for annual models). Follows a unit recruit entering
# each stock: survival, then Herm transition at the new age, as in the engine.
# Returns [Sim, Age, BirthSeason, To, From] arrays at the start of each step
# (`no`) and at spawning (`sp`).
.CalcNPRHermBlock <- function(ZList, SemelList, STFList, PlusGroupFlag, HazardList,
                              From, To, CycleSemel = TRUE, tol = 1e-14, maxCycle = 20000) {
  Stocks <- names(ZList)
  nC     <- length(Stocks)
  d      <- dim(ZList[[1]])
  nSim   <- d[1]; nAge <- d[2]; nSeason <- d[3]

  Z  <- array(unlist(ZList),     c(nSim, nAge, nSeason, nC))
  Sm <- array(unlist(SemelList), c(nSim, nAge, nSeason, nC))
  f  <- matrix(unlist(STFList),  nSim, nC)
  PG <- as.numeric(unlist(PlusGroupFlag))
  fi <- match(From, Stocks)
  ti <- match(To, Stocks)

  NO <- SP <- array(0, c(nSim, nAge, nSeason, nC, nC))
  block <- if (any(PG > 0)) (nAge - nSeason + 1L):nAge else integer(0)

  for (b in seq_len(nSeason)) {
    sa <- ((b - 1L) + seq_len(nAge) - 1L) %% nSeason + 1L
    Za <- \(a) matrix(Z[, a, sa[a], ], nSim, nC)
    Sa <- \(a) matrix(Sm[, a, sa[a], ], nSim, nC)
    Ha <- \(a) purrr::map(HazardList, \(h) h[, a, sa[a]])

    St <- array(0, c(nSim, nC, nC))
    for (k in seq_len(nC)) St[, k, k] <- 1

    Path  <- c(seq_len(nAge), rep(block, length.out = length(block) * maxCycle))
    Semel <- c(rep(TRUE, nAge), rep(CycleSemel, length(Path) - nAge))
    Plus  <- c(rep(FALSE, nAge), rep(TRUE, length(Path) - nAge))

    for (i in seq_along(Path)) {
      a <- Path[i]
      if (i > 1L) {
        a_from <- Path[i - 1L]
        surv <- .HermSurvival(Za(a_from), if (Semel[i]) Sa(a_from), if (Plus[i]) PG)
        St   <- .HermMove(St * array(surv, c(nSim, nC, nC)), Ha(a), fi, ti)
      }
      NO[, a, b, , ] <- NO[, a, b, , ] + as.vector(St)
      SP[, a, b, , ] <- SP[, a, b, , ] + as.vector(St * array(exp(-Za(a) * f), c(nSim, nC, nC)))
      if (i > nAge && a == nAge && max(St) < tol) break
    }
    if (length(block) && max(St) >= tol)
      cli::cli_warn("Herm per-recruit plus group did not converge for stocks {.val {Stocks}}.")
  }
  list(no = NO, sp = SP)
}

# Survival over one step for [Sim, Stock] mortality; Semel and PlusGroup optional
.HermSurvival <- function(Z, Semel = NULL, PlusGroup = NULL) {
  surv <- exp(-Z)
  if (!is.null(Semel)) surv <- surv * (1 - Semel)
  if (!is.null(PlusGroup)) surv <- sweep(surv, 2L, PlusGroup, `*`)
  surv
}

# Herm transitions applied to St[Sim, To, From] with per-pair hazards
.HermMove <- function(St, Hazard, FromIdx, ToIdx) {
  for (p in seq_along(FromIdx)) {
    Nmov <- St[, FromIdx[p], , drop = FALSE] * Hazard[[p]]
    St[, ToIdx[p], ]   <- St[, ToIdx[p], , drop = FALSE] + Nmov
    St[, FromIdx[p], ] <- St[, FromIdx[p], , drop = FALSE] - Nmov
  }
  St
}

# Annual joint numbers-per-recruit for Herm-linked stocks, one equilibrium per year.
# Returns NPR[[from]][[to]] = list(no, sp) of [Sim, Age, Year] arrays.
.CalcNPRHermAnnual <- function(comp, Herm, ZList, SemelparousList, SpawnTimeFracList,
                               PlusGroupList) {
  Z <- purrr::map(ZList[comp], \(x) .Aperm(x, c('Sim', 'Age', 'Year')))
  nSim  <- max(purrr::map_int(Z, \(x) dim(x)[1]))
  Z     <- purrr::map(Z, \(x) ExtendSims(x, nSim))
  dn    <- dimnames(Z[[1]])
  Years <- as.numeric(dn$Year)
  AgeClasses <- as.numeric(dn$Age)
  d     <- dim(Z[[1]])

  Semel <- purrr::map(SemelparousList[comp], \(x)
    .ProcessSemelparous(x, nSim, AgeClasses, Years) |> .SubsetYear(Years) |>
      ExtendSims(nSim) |> .Aperm(c('Sim', 'Age', 'Year')))
  STF <- purrr::map(SpawnTimeFracList[comp], \(x) rep(x, nSim)[seq_len(nSim)])
  PG  <- purrr::map_lgl(PlusGroupList[comp], \(x) isTRUE(as.logical(x)))

  pairs  <- which(Herm$From %in% comp & Herm$To %in% comp)
  Hazard <- purrr::map(Herm$Hazard[pairs], \(h)
    .ArraySubsetYear(h, Years) |> ExtendSims(nSim) |> .Aperm(c('Sim', 'Age', 'Year')))

  out <- purrr::map(stats::setNames(comp, comp), \(k)
    purrr::map(stats::setNames(comp, comp), \(s)
      list(no = array(0, d, dn), sp = array(0, d, dn))))

  for (y in seq_len(d[3])) {
    res <- .CalcNPRHermBlock(
      ZList         = purrr::map(Z, \(x) x[, , y, drop = FALSE]),
      SemelList     = purrr::map(Semel, \(x) x[, , y, drop = FALSE]),
      STFList       = STF,
      PlusGroupFlag = PG,
      HazardList    = purrr::map(Hazard, \(x) x[, , y, drop = FALSE]),
      From          = Herm$From[pairs],
      To            = Herm$To[pairs],
      CycleSemel    = FALSE
    )
    for (ki in seq_along(comp)) for (si in seq_along(comp)) {
      out[[ki]][[si]]$no[, , y] <- res$no[, , 1, si, ki]
      out[[ki]][[si]]$sp[, , y] <- res$sp[, , 1, si, ki]
    }
  }
  out
}

# Herm-linked components (2+ stocks) among `Stocks`, from inputs$Herm
.HermComponents <- function(Herm, Stocks) {
  if (is.null(Herm) || !length(Herm$From)) return(list())
  comps <- .HermConnectedComponents(match(Herm$From, Stocks), match(Herm$To, Stocks),
                                    length(Stocks))
  purrr::keep(comps, \(x) length(x) > 1L) |> purrr::map(\(x) Stocks[x])
}

# Aggregate a seasonal per-recruit quantity to an annual [Sim] scalar,
# weighted by seasonal recruitment weights pi_s (sum_s pi_s = 1).
.AggSeasonalProduct <- function(NPR_bs, q_sa, pi_s) {

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

.AggCrossSectional <- function(NPR_bs, q_sa, pi_s, s_ref) {

  nSim    <- dim(NPR_bs)[1]
  nAge    <- dim(NPR_bs)[2]
  nSeason <- dim(NPR_bs)[3]
  q_ref <- q_sa[, , s_ref, drop = FALSE]
  dim(q_ref) <- dim(q_ref)[1:2]

  result <- numeric(nSim)

  for (b in seq_len(nSeason)) {
    a0   <- 1L + ((s_ref - b) %% nSeason)
    ages <- seq(a0, nAge, by = nSeason)
    NPR_sel <- NPR_bs[, ages, b, drop = FALSE]
    dim(NPR_sel) <- dim(NPR_sel)[1:2]   # [Sim, length(ages)], matching q_ref
    result <- result + pi_s[, b] *
      rowSums(NPR_sel * q_ref[, ages, drop = FALSE])
  }

  result
}

.AggRefSeason <- function(NPR_bs, q_sa, pi_s, RefSeasonWeights) {

  nSim    <- dim(NPR_bs)[1]
  nSeason <- dim(NPR_bs)[3]
  result  <- numeric(nSim)

  for (s in seq_len(nSeason)) {
    w <- RefSeasonWeights[, s]
    if (all(w == 0)) next
    result <- result + w * .AggCrossSectional(NPR_bs, q_sa, pi_s, s)
  }

  result
}

.RefSeasonWeights <- function(FecundityList, MaturityList, nSeason, RefSeason) {

  nSim <- max(vapply(FecundityList, \(x) dim(x)[1], integer(1)))

  if (nSeason == 1L)
    return(matrix(1, nrow = nSim, ncol = 1))

  if (!is.null(RefSeason)) {
    W <- matrix(0, nrow = nSim, ncol = nSeason)
    W[, RefSeason] <- 1 / length(RefSeason)
    return(W)
  }

  eps  <- .Machine$double.eps
  Flag <- matrix(FALSE, nrow = nSim, ncol = nSeason)

  for (i in seq_along(FecundityList)) {
    Spawn <- FecundityList[[i]]
    if (length(Spawn) == 0) Spawn <- MaturityList[[i]]
    SumByAgeSeason <- apply(Spawn, c(1, 3), sum)   # [Sim, nSeason]
    if (nrow(SumByAgeSeason) == 1L && nSim > 1L)
      SumByAgeSeason <- SumByAgeSeason[rep(1L, nSim), , drop = FALSE]
    Flag <- Flag | (SumByAgeSeason > eps)
  }

  NoneDetected <- rowSums(Flag) == 0
  if (any(NoneDetected)) Flag[NoneDetected, ] <- TRUE

  Flag / rowSums(Flag)
}

.AggSeasonalYield <- function(NPR_bs, FDead_saf, ZTotal_sa, WeightFleet_saf, pi_s,
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

.AggSeasonalDiscards <- function(NPR_bs, FInteract_saf, FRetain_saf, FDiscDead_saf,
                                ZTotal_sa, WeightFleetSelected_saf,
                                WeightFleetRetained_saf, pi_s) {
  nSim    <- dim(NPR_bs)[1]
  nAge    <- dim(NPR_bs)[2]
  nSeason <- dim(NPR_bs)[3]
  nFleet  <- dim(FInteract_saf)[4]

  result <- numeric(nSim)

  for (s in seq_len(nSeason)) {
    seas_at_age <- ((s - 1L) + seq_len(nAge) - 1L) %% nSeason + 1L

    Y_s <- numeric(nSim)
    for (a in seq_len(nAge)) {
      sa      <- seas_at_age[a]
      Z_a     <- ZTotal_sa[, a, sa]
      denom   <- pmax(Z_a, .Machine$double.eps)
      Ndead_a <- NPR_bs[, a, s] * (1 - exp(-Z_a))

      for (f in seq_len(nFleet)) {
        Inum <- Ndead_a * (FInteract_saf[, a, sa, f] / denom)
        Lnum <- Ndead_a * (FRetain_saf[, a, sa, f]   / denom)
        Dnum <- Ndead_a * (FDiscDead_saf[, a, sa, f] / denom)

        IW <- Inum * WeightFleetSelected_saf[, a, sa, f]
        LW <- Lnum * WeightFleetRetained_saf[, a, sa, f]
        discTotN <- Inum - Lnum
        DW <- ifelse(discTotN > 1e-12, (IW - LW) * Dnum / discTotN, 0)

        Y_s <- Y_s + DW
      }
    }
    result <- result + pi_s[, s] * Y_s
  }

  result
}


.CalcPerRecruitF <- function(apicalF = 0.1, spr_threshold = 0.001,
                             NPR0List = NULL, NPR0_SPList = NULL, ...) {
  names(apicalF) <- as.character(apicalF)
  dots <- list(...)

  if (is.null(NPR0List) && (is.null(dots$nSeason) || dots$nSeason <= 1L)) {
    NPR0List <- purrr::pmap(
      list(dots$NaturalMortalityList, dots$PlusGroupList, dots$SemelparousList),
      \(NaturalMortality, PlusGroup, Semelparous)
      CalcSurvival(NaturalMortality, FishingMortality = NULL,
                  PlusGroup, SpawnTimeFrac = 0, Semelparous)
    )
    IsSpawnTimeFrac <- any(unlist(dots$SpawnTimeFracList) != 0)
    NPR0_SPList <- if (IsSpawnTimeFrac) {
      purrr::pmap(
        list(dots$NaturalMortalityList, dots$PlusGroupList,
            dots$SemelparousList, dots$SpawnTimeFracList),
        \(NaturalMortality, PlusGroup, Semelparous, SpawnTimeFrac)
        CalcSurvival(NaturalMortality, FishingMortality = NULL,
                    PlusGroup, SpawnTimeFrac, Semelparous)
      )
    } else {
      NPR0List
    }
  }

  PRList    <- vector('list', length(apicalF))
  names(PRList) <- names(apicalF)
  collapsed <- FALSE

  for (i in seq_along(apicalF)) {
    if (collapsed) {
      PRList[[i]] <- PRList[[i-1]]
    } else {
      PRList[[i]] <- .CalcPerRecruitFScalar(apicalF = apicalF[i], ...,
                                            NPR0List = NPR0List,
                                            NPR0_SPList = NPR0_SPList)
      if (!is.null(PRList[[i]]@SPR) && min(PRList[[i]]@SPR, na.rm=TRUE) < spr_threshold)
        collapsed <- TRUE
    }
  }
  
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
      if (length(pr@Misc$HermCross))
        pr@Misc$HermCross <- purrr::map(pr@Misc$HermCross, \(x) {
          Fixed <- x$SPR0
          x <- purrr::map(x, zero_fill)
          x$SPR0 <- Fixed
          x
        })
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
  PerRecruit@Misc        <- PRList[[1]]@Misc
  PerRecruit@Misc$HermCross <- .BindHermCrossF(PRList)
  PerRecruit
}

# Herm cross per-recruit terms bound over the F grid (SPR0 is F-invariant)
.BindHermCrossF <- function(PRList) {
  Cross1 <- PRList[[1]]@Misc$HermCross
  if (!length(Cross1)) return(NULL)
  purrr::imap(Cross1, \(x, k) {
    out <- purrr::imap(x, \(arr, v)
      if (v == 'SPR0') arr else purrr::map(PRList, \(pr) pr@Misc$HermCross[[k]][[v]]) |> List2Array('F'))
    out
  })
}


.CalcPerRecruitFScalar <- function(apicalF = 0.1,
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
                                   WeightFleetRetainedList,
                                   WeightFleetSelectedList,
                                   SelectivityFleetList,
                                   RetentionFleetList,
                                   DiscardMortalityFleetList,
                                   FleetNames,
                                   Years,
                                   nSeason          = 1L,
                                   SeasonalWeightsList = NULL,
                                   CalendarYears    = NULL,
                                   RefSeason        = NULL,
                                   NPR0List         = NULL,
                                   NPR0_SPList      = NULL,
                                   Herm             = NULL) {

  # Dispatch to seasonal implementation when there are multiple seasons.
  if (nSeason > 1L)
    return(.CalcPerRecruitFScalarSeasonal(
      Herm                      = Herm,
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
      WeightFleetRetainedList   = WeightFleetRetainedList,
      WeightFleetSelectedList   = WeightFleetSelectedList,
      SelectivityFleetList      = SelectivityFleetList,
      RetentionFleetList        = RetentionFleetList,
      DiscardMortalityFleetList = DiscardMortalityFleetList,
      FleetNames                = FleetNames,
      Years                     = Years,
      nSeason                   = nSeason,
      SeasonalWeightsList       = SeasonalWeightsList,
      CalendarYears             = CalendarYears,
      RefSeason                 = RefSeason
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
    \(FDeadTotal) apply(FDeadTotal, .SetDnames(c('Sim', 'Year')), max)
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
    
    adjust <- ArrayDivide(apicalFSimTS, ActualApicalF) |>  
      AddDimension("Stock", pos = 2) |>
      AddDimension("Age",   pos = 3) |>
      AddDimension("Fleet", pos = 5) |>
      .ExtendFleets(Fleets = FleetNames) |>
      .ExtendStocks(Stocks = names(NaturalMortalityList))
    
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
  
  if (is.null(NPR0List)) {
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
  }

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
  if (is.null(NPR0_SPList)) {
    NPR0_SPList <- if (IsSpawnTimeFrac) {
      purrr::pmap(
        list(
          NaturalMortalityList, PlusGroupList,SemelparousList, SpawnTimeFracList),
        \(NaturalMortality, PlusGroup, Semelparous, SpawnTimeFrac)
        CalcSurvival(
          NaturalMortality, FishingMortality = NULL,
          PlusGroup, SpawnTimeFrac, Semelparous
        )
      )
    } else {
      NPR0List
    }
  }

  if (IsSpawnTimeFrac) {
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
    NPRF_SPList <- NPRFList
  }

  HermCrossNPR <- list()
  for (comp in .HermComponents(Herm, names(NaturalMortalityList))) {
    JointF <- .CalcNPRHermAnnual(comp, Herm, ZDeadTotalList, SemelparousList,
                                 SpawnTimeFracList, PlusGroupList)
    Joint0 <- .CalcNPRHermAnnual(comp, Herm, NaturalMortalityList, SemelparousList,
                                 SpawnTimeFracList, PlusGroupList)
    for (st in comp) {
      NPRFList[[st]]    <- JointF[[st]][[st]]$no
      NPRF_SPList[[st]] <- JointF[[st]][[st]]$sp
      NPR0List[[st]]    <- Joint0[[st]][[st]]$no
      NPR0_SPList[[st]] <- Joint0[[st]][[st]]$sp
    }
    for (k in comp) {
      Others <- setdiff(comp, k)
      HermCrossNPR[[k]] <- list(
        NPRF    = purrr::map(stats::setNames(Others, Others), \(s) JointF[[k]][[s]]$no),
        NPRF_SP = purrr::map(stats::setNames(Others, Others), \(s) JointF[[k]][[s]]$sp),
        NPR0_SP = purrr::map(stats::setNames(Others, Others), \(s) Joint0[[k]][[s]]$sp)
      )
    }
  }

  SPRFList <- purrr::map2(
    NPRF_SPList, FecundityList,
    \(NPRF_SP, Fecundity) {
      SPRF <- ArrayMultiply(NPRF_SP, Fecundity) |> SumOverAge()
      if (!is.array(SPRF))
        SPRF <- array(SPRF, length(SPRF), dimnames = list(Year = Years))
      SPRF
    }
  )
  names(SPRFList)  <- names(NPRFList)
  if (length(SPRFList)> 1) {
    sp_ind   <- match(names(SPFrom), names(SPRFList))
    SPRFList <- SPRFList[sp_ind]
  }

  StockNamesOrdered <- names(NPRFList)
  SPR0OwnList <- .CalcSPR0Own(NPR0_SPList, FecundityList, Years)
  SelfOnly    <- .SPFromSelfOnly(SPFrom, StockNamesOrdered)
  SPR <- purrr::map(StockNamesOrdered, \(stock_name) {
    w <- SPFrom[[stock_name]]
    Reduce(`+`, purrr::map2(w$from, w$weight, \(idx, wt) {
      src <- StockNamesOrdered[idx]
      SPR0src <- if (SelfOnly[[src]]) SPR0List[[src]] else SPR0OwnList[[src]]
      wt * ArrayDivide(SPRFList[[src]], .ArraySubsetYear(SPR0src, Years))
    }))
  }) |>
    stats::setNames(StockNamesOrdered) |>
    List2Array('Stock', pos = 2) |>
    .ArraySubsetYear(Years)
  
  ToFishingRatioList <- function(FList) {
    out <- purrr::map2(
      FList, ZDeadTotalList,
      \(F_, ZDeadTotal) {
        ZDeadTotalFleet <- AddDimension(ZDeadTotal, 'Fleet') |>
          .ExtendFleets(Fleets = FleetNames)
        ArrayDivide(F_, ZDeadTotalFleet)
      }
    )
    names(out) <- names(NaturalMortalityList)
    out
  }

  FishingInteractList <- ToFishingRatioList(FInteractList)
  FishingRetainList   <- ToFishingRatioList(FRetainList)
  FishingDiscDeadList <- ToFishingRatioList(FDiscardDeadList)

  .DerivePerRecruit <- function(NPRFList, NPRF_SPList) {
    NDeadList <- purrr::map2(
      NPRFList, ZDeadTotalList,
      \(NPRF, ZDeadTotal) ArrayMultiply(NPRF, (1 - exp(-ZDeadTotal)))
    )

    Landings <- purrr::pmap(
      list(FishingRetainList, NDeadList, WeightFleetRetainedList),
      \(FishingRetain, NDead, WeightFleet) {
        NDeadFleet <- AddDimension(NDead, 'Fleet') |>
          .ExtendFleets(Fleets = FleetNames)
        ArrayMultiply(FishingRetain, NDeadFleet) |>
          ArrayMultiply(WeightFleet) |>
          SumOverFleet() |>
          SumOverAge()
      }
    ) |>
      List2Array('Stock', pos = 2)

    Discards <- purrr::pmap(
      list(FishingInteractList, FishingRetainList, FishingDiscDeadList, NDeadList,
          WeightFleetSelectedList, WeightFleetRetainedList),
      \(FInteractRatio, FRetainRatio, FDiscDeadRatio, NDead, WSel, WRet) {
        NDeadFleet <- AddDimension(NDead, 'Fleet') |>
          .ExtendFleets(Fleets = FleetNames)
        Inum <- ArrayMultiply(FInteractRatio, NDeadFleet)
        Lnum <- ArrayMultiply(FRetainRatio, NDeadFleet)
        Dnum <- ArrayMultiply(FDiscDeadRatio, NDeadFleet)
        IW   <- ArrayMultiply(Inum, WSel)
        LW   <- ArrayMultiply(Lnum, WRet)
        discTotN <- ArraySubtract(Inum, Lnum)
        DW <- ArrayDivide(ArrayMultiply(ArraySubtract(IW, LW), Dnum), discTotN)
        SumOverFleet(DW) |> SumOverAge()
      }
    ) |>
      List2Array('Stock', pos = 2)

    Removals <- ArraySum(Landings, Discards)

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

    NPRF <- purrr::map(NPRFList, SumOverAge) |> List2Array("Stock", pos = 2)

    list(Landings = Landings, Removals = Removals, Biomass = Biomass,
         SBiomass = SBiomass, SProduction = SProduction, NPRF = NPRF)
  }

  Derived     <- .DerivePerRecruit(NPRFList, NPRF_SPList)
  Landings    <- Derived$Landings
  Removals    <- Derived$Removals
  Biomass     <- Derived$Biomass
  SBiomass    <- Derived$SBiomass
  SProduction <- Derived$SProduction

  HermCross <- purrr::imap(HermCrossNPR, \(x, k) {
    Full <- \(lst) purrr::map(stats::setNames(names(NPRFList), names(NPRFList)), \(s)
      if (s %in% names(lst)) lst[[s]] else NPRFList[[s]] * 0)
    Cross <- .DerivePerRecruit(Full(x$NPRF), Full(x$NPRF_SP))
    Cross$SPRF <- Cross$SProduction
    Cross$SPR0 <- purrr::map2(Full(x$NPR0_SP), FecundityList, \(NPR0_SP, Fecundity)
      ArrayMultiply(NPR0_SP, Fecundity) |> SumOverAge()) |>
      List2Array("Stock", pos = 2)
    Cross
  })

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
  SPR0Own <- SPR0OwnList[StockNamesOrdered] |> List2Array("Stock", pos = 2)
  SPR0Own <- array(SPR0Own, dim = unname(dim(SPR0Own)), dimnames = dimnames(SPR0Own))
  PerRecruit@Misc        <- list(SPR0Own = SPR0Own)
  if (length(HermCross)) PerRecruit@Misc$HermCross <- HermCross
  PerRecruit
}

# Unfished spawning production per recruit from each stock's own numbers (annual models)
.CalcSPR0Own <- function(NPR0_SPList, FecundityList, Years) {
  out <- purrr::map2(NPR0_SPList, FecundityList, \(NPR0_SP, Fecundity) {
    SPR0 <- ArrayMultiply(NPR0_SP, Fecundity) |> SumOverAge()
    if (!is.array(SPR0))
      SPR0 <- array(SPR0, length(SPR0), dimnames = list(Year = Years))
    SPR0
  })
  names(out) <- names(NPR0_SPList)
  out
}

.SPFromSelfOnly <- function(SPFrom, StockNames) {
  purrr::map_lgl(stats::setNames(StockNames, StockNames), \(nm) {
    w <- SPFrom[[nm]]
    length(w$from) == 1L && identical(StockNames[w$from], nm)
  })
}



.CalcPerRecruitFScalarSeasonal <- function(apicalF,
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
                                             WeightFleetRetainedList,
                                             WeightFleetSelectedList,
                                             SelectivityFleetList,
                                             RetentionFleetList,
                                             DiscardMortalityFleetList,
                                             FleetNames,
                                             Years,
                                             nSeason,
                                             SeasonalWeightsList,
                                             CalendarYears,
                                             RefSeason = NULL,
                                             Herm      = NULL) {

  nStocks    <- length(NaturalMortalityList)
  nCalYears  <- length(CalendarYears)
  StockNames <- names(NaturalMortalityList)

  # aggregation below is matrix-based, so all inputs need a common Sim length
  SimInputs <- list(StockFleetAllocation, NaturalMortalityList, MaturityList, SemelparousList,
                    WeightList, SPR0List, FecundityList, WeightFleetRetainedList,
                    WeightFleetSelectedList, SelectivityFleetList, RetentionFleetList,
                    DiscardMortalityFleetList, SeasonalWeightsList)
  nSimAll <- max(1L, rapply(SimInputs, \(x) if (is.array(x) && 'Sim' %in% names(dimnames(x)))
    dim(x)[names(dimnames(x)) == 'Sim'] else 1L, how = 'unlist'))
  if (nSimAll > 1) {
    StockFleetAllocation      <- ExtendSims(StockFleetAllocation, nSimAll)
    NaturalMortalityList      <- ExtendSims(NaturalMortalityList, nSimAll)
    MaturityList              <- ExtendSims(MaturityList, nSimAll)
    SemelparousList           <- ExtendSims(SemelparousList, nSimAll)
    WeightList                <- ExtendSims(WeightList, nSimAll)
    SPR0List                  <- ExtendSims(SPR0List, nSimAll)
    FecundityList             <- ExtendSims(FecundityList, nSimAll)
    WeightFleetRetainedList   <- ExtendSims(WeightFleetRetainedList, nSimAll)
    WeightFleetSelectedList   <- ExtendSims(WeightFleetSelectedList, nSimAll)
    SelectivityFleetList      <- ExtendSims(SelectivityFleetList, nSimAll)
    RetentionFleetList        <- ExtendSims(RetentionFleetList, nSimAll)
    DiscardMortalityFleetList <- ExtendSims(DiscardMortalityFleetList, nSimAll)
    SeasonalWeightsList       <- ExtendSims(SeasonalWeightsList, nSimAll)
  }

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

  ActualApicalFByStock <- purrr::map(FDeadTotalList, \(FDeadTotal)
    apply(FDeadTotal, .SetDnames(c('Sim', 'Year')), max)
  ) |> List2Array('Stock', pos = 2)

  ActualApicalF <- apply(ActualApicalFByStock, c('Sim', 'Year'), max)

  # one scale per sim and calendar year, so apicalF is the peak-season F and the seasonal pattern is kept
  for (cy in seq_len(nCalYears)) {
    ts_idx <- ((cy - 1L) * nSeason + 1L):(cy * nSeason)
    ActualApicalF[, ts_idx] <- apply(ActualApicalF[, ts_idx, drop = FALSE], 1L, max)
  }

  if (apicalF > 0 && any(abs(ActualApicalF / apicalF - 1) > 1E-2)) {
    apicalFSimTS <- array(apicalF, dim = dim(ActualApicalF), dimnames = dimnames(ActualApicalF))
    adjust <- ArrayDivide(apicalFSimTS, ActualApicalF) |>
      AddDimension("Stock", pos = 2) |>
      AddDimension("Age",   pos = 3) |>
      AddDimension("Fleet", pos = 5) |>
      .ExtendFleets(Fleets = FleetNames) |>
      .ExtendStocks(Stocks = StockNames)

    FInteractList     <- purrr::map2(FInteractList, Array2List(adjust, 'Stock'), ArrayMultiply)
    FRetainList       <- purrr::map2(FInteractList, RetentionFleetList,         ArrayMultiply)
    FDiscardTotalList <- purrr::map2(FInteractList, FRetainList,                ArraySubtract)
    FDiscardDeadList  <- purrr::map2(FDiscardTotalList, DiscardMortalityFleetList, ArrayMultiply)
    FDeadList         <- purrr::map2(FRetainList, FDiscardDeadList,             ArraySum)
    FDeadTotalList    <- purrr::map(FDeadList, SumOverFleet)
  }

  ZDeadTotalList <- purrr::map2(FDeadTotalList, NaturalMortalityList, ArraySum)

  IsSpawnTimeFrac <- any(unlist(SpawnTimeFracList) != 0)

  SimNames <- dimnames(NaturalMortalityList[[1]])[['Sim']]

  HermComps  <- .HermComponents(Herm, StockNames)
  HermPairs  <- purrr::map(HermComps, \(comp) which(Herm$From %in% comp & Herm$To %in% comp))
  names(HermPairs) <- purrr::map_chr(HermComps, \(comp) paste(comp, collapse = '|'))
  HermHazard <- if (length(HermComps))
    purrr::map(Herm$Hazard, \(h) ExtendSims(h, nSimAll) |> .Aperm(c('Sim', 'Age', 'Year')))

  # For each calendar year, compute annual per-recruit quantities
  perYear <- purrr::map(seq_len(nCalYears), \(cy) {

    ts_idx <- ((cy - 1L) * nSeason + 1L):(cy * nSeason)   # season indices within Years
    FecundityList_cy    <- purrr::map(FecundityList, \(x) x[, , ts_idx, drop = FALSE])
    MaturityList_cy     <- purrr::map(MaturityList,  \(x) x[, , ts_idx, drop = FALSE])
    RefSeasonWeights_cy <- .RefSeasonWeights(FecundityList_cy, MaturityList_cy, nSeason, RefSeason)

    seas_ts <- Years[ts_idx]  # seasonal time step labels for this calendar year
    Slice   <- \(x) if (is.array(x)) x[, , ts_idx, drop = FALSE] else x

    # pi_s: seasonal recruitment weights [Sim, nSeason]
    PiList <- purrr::map(SeasonalWeightsList, \(pi_s_full)
      matrix(as.numeric(.ArraySubsetYear(pi_s_full, seas_ts)), nrow = nSimAll))

    NPRList <- purrr::map(stats::setNames(StockNames, StockNames), \(st) {
      Z_cy     <- Slice(ZDeadTotalList[[st]])
      Z0_cy    <- Slice(NaturalMortalityList[[st]])
      Semel_cy <- Slice(SemelparousList[[st]])
      STF      <- SpawnTimeFracList[[st]]
      PG       <- PlusGroupList[[st]]
      NPR0_no  <- .CalcNPRSeasonal(Z0_cy, PG, 0, Semel_cy)
      NPRF_no  <- .CalcNPRSeasonal(Z_cy,  PG, 0, Semel_cy)
      list(NPR0_no = NPR0_no,
           NPR0_sp = if (IsSpawnTimeFrac) .CalcNPRSeasonal(Z0_cy, PG, STF, Semel_cy) else NPR0_no,
           NPRF_no = NPRF_no,
           NPRF_sp = if (IsSpawnTimeFrac) .CalcNPRSeasonal(Z_cy, PG, STF, Semel_cy) else NPRF_no)
    })

    CrossNPR <- list()
    for (comp in HermComps) {
      Args <- list(
        SemelList     = purrr::map(SemelparousList[comp], \(x)
          if (is.array(Slice(x))) Slice(x) else array(0, dim(Slice(ZDeadTotalList[[1]])))),
        STFList       = purrr::map(SpawnTimeFracList[comp], \(x) rep(x, nSimAll)[seq_len(nSimAll)]),
        PlusGroupFlag = purrr::map_lgl(PlusGroupList[comp], \(x) isTRUE(as.logical(x))),
        HazardList    = purrr::map(HermHazard[HermPairs[[paste(comp, collapse = '|')]]], Slice),
        From          = Herm$From[HermPairs[[paste(comp, collapse = '|')]]],
        To            = Herm$To[HermPairs[[paste(comp, collapse = '|')]]],
        CycleSemel    = TRUE
      )
      JointF <- do.call(.CalcNPRHermBlock, c(list(ZList = purrr::map(ZDeadTotalList[comp], Slice)), Args))
      Joint0 <- do.call(.CalcNPRHermBlock, c(list(ZList = purrr::map(NaturalMortalityList[comp], Slice)), Args))
      Get <- \(J, s, k, v) J[[v]][, , , match(s, comp), match(k, comp), drop = TRUE] |>
        array(dim = c(nSimAll, dim(J[[v]])[2:3]))
      for (s in comp)
        NPRList[[s]] <- list(NPR0_no = Get(Joint0, s, s, 'no'), NPR0_sp = Get(Joint0, s, s, 'sp'),
                             NPRF_no = Get(JointF, s, s, 'no'), NPRF_sp = Get(JointF, s, s, 'sp'))
      for (k in comp) for (s in setdiff(comp, k))
        CrossNPR[[k]][[s]] <- list(NPR0_no = Get(Joint0, s, k, 'no'), NPR0_sp = Get(Joint0, s, k, 'sp'),
                                   NPRF_no = Get(JointF, s, k, 'no'), NPRF_sp = Get(JointF, s, k, 'sp'))
    }

    AggStock <- function(st, NPR, pi_s_cy) {
        i        <- match(st, StockNames)
        nSim     <- nSimAll
        Z_cy     <- Slice(ZDeadTotalList[[i]])
        Fec_cy   <- Slice(FecundityList[[i]])
        W_cy     <- Slice(WeightList[[i]])
        Mat_cy   <- Slice(MaturityList[[i]])
        FDead_full     <- FDeadList[[i]]
        FRetain_full   <- FRetainList[[i]]
        FInteract_full <- FInteractList[[i]]
        FDiscDead_full <- FDiscardDeadList[[i]]
        WF_full        <- WeightFleetRetainedList[[i]]
        WFSel_full     <- WeightFleetSelectedList[[i]]

        # Seasonal SPR0 [Sim, nSeason], weighted average gives annual SPR0 [Sim]
        SPR0_cy_arr <- .ArraySubsetYear(SPR0List[[i]], seas_ts)  # [Sim, nSeason]
        SPR0_cy_mat <- matrix(as.numeric(SPR0_cy_arr), nrow = nSim)
        SPR0_ann    <- rowSums(pi_s_cy * SPR0_cy_mat)

        NPR0_no <- NPR$NPR0_no
        NPR0_sp <- NPR$NPR0_sp
        NPRF_no <- NPR$NPRF_no
        NPRF_sp <- NPR$NPRF_sp

        # Annual aggregate quantities: [nSim] scalars
        ones_cy <- array(1, dim(Z_cy))   # for summing NPR over ages without weighting

        NPR0_ann    <- .AggSeasonalProduct(NPR0_no, ones_cy, pi_s_cy)
        NPR0_SP_ann <- .AggSeasonalProduct(NPR0_sp, ones_cy, pi_s_cy)
        NPRF_ann    <- .AggSeasonalProduct(NPRF_no, ones_cy, pi_s_cy)
        NPRF_SP_ann <- .AggSeasonalProduct(NPRF_sp, ones_cy, pi_s_cy)

        SPR0f_ann   <- .AggSeasonalProduct(NPR0_sp, Fec_cy,  pi_s_cy)   # unfished SP per recruit
        SPRFf_ann   <- .AggSeasonalProduct(NPRF_sp, Fec_cy,  pi_s_cy)   # fished SP per recruit
        SPR_ann     <- SPRFf_ann / pmax(SPR0f_ann, .Machine$double.eps)

        Biomass_ann  <- .AggRefSeason(NPRF_no, W_cy,          pi_s_cy, RefSeasonWeights_cy)
        SBiomass_ann <- .AggRefSeason(NPRF_sp, W_cy * Mat_cy, pi_s_cy, RefSeasonWeights_cy)
        SProd_ann    <- .AggRefSeason(NPRF_sp, Fec_cy,        pi_s_cy, RefSeasonWeights_cy)

        # Fleet arrays [Sim, Age, Season, Fleet] — extract for yield
        FDead_cy     <- FDead_full[,     , ts_idx, , drop = FALSE]
        FRetain_cy   <- FRetain_full[,   , ts_idx, , drop = FALSE]
        FInteract_cy <- FInteract_full[, , ts_idx, , drop = FALSE]
        FDiscDead_cy <- FDiscDead_full[, , ts_idx, , drop = FALSE]
        ZTot_cy      <- Z_cy   
        WF_cy        <- WF_full[,        , ts_idx, , drop = FALSE]
        WFSel_cy     <- WFSel_full[,     , ts_idx, , drop = FALSE]

        # Landings, Discards, and Removals .
        Landings_ann <- .AggSeasonalYield(NPRF_no, FDead_cy, ZTot_cy, WF_cy,
                                          pi_s_cy, type = 'Landings', FRetain_saf = FRetain_cy)
        Discards_ann <- .AggSeasonalDiscards(NPRF_no, FInteract_cy, FRetain_cy, FDiscDead_cy,
                                            ZTot_cy, WFSel_cy, WF_cy, pi_s_cy)
        Removals_ann <- Landings_ann + Discards_ann

        list(
          NPR0        = NPR0_ann,
          NPR0_SP     = NPR0_SP_ann,
          SPR0_ann    = SPR0_ann,    
          NPRF        = NPRF_ann,
          NPRF_SP     = NPRF_SP_ann,
          SPR0f       = SPR0f_ann,
          SPRFf       = SPRFf_ann,
          SPR         = SPR_ann,
          Biomass     = Biomass_ann,
          SBiomass    = SBiomass_ann,
          SProduction = SProd_ann,
          Removals    = Removals_ann,
          Landings    = Landings_ann
        )
    }

    Own   <- purrr::map(StockNames, \(st) AggStock(st, NPRList[[st]], PiList[[st]]))
    Cross <- purrr::imap(CrossNPR, \(targets, k)
      purrr::imap(targets, \(NPR, s) AggStock(s, NPR, PiList[[k]])))
    list(Own = Own, Cross = Cross)
  })   # list over calendar years
  HermCrossYear <- purrr::map(perYear, \(x) x$Cross)
  perYear       <- purrr::map(perYear, \(x) x$Own)

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
  SPR0ann     <- WrapSlot('SPR0_ann')
  SPRFf       <- WrapSlot('SPRFf')    # fished SP per recruit, per stock
  Biomass     <- WrapSlot('Biomass')
  SBiomass    <- WrapSlot('SBiomass')
  SProduction <- WrapSlot('SProduction')
  Removals    <- WrapSlot('Removals')
  Landings    <- WrapSlot('Landings')

  # Apply SPFrom: SPR[i] = weighted sum of SPRFf[src] / SPR0f[src] over sources
  SPR <- array(0, dim = dim(SPRFf), dimnames = dimnames(SPRFf))
  eps <- .Machine$double.eps
  for (i in seq_len(nStocks)) {
    w <- SPFrom[[i]]
    acc <- 0
    for (k in seq_along(w$from)) {
      spf <- w$from[k]
      acc <- acc + w$weight[k] * (SPRFf[, spf, ] / pmax(SPR0f[, spf, ], eps))
    }
    SPR[, i, ] <- acc
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

  SelfOnly <- .SPFromSelfOnly(SPFrom, StockNames)
  SPR0 <- SPR0f
  SPR0[, !SelfOnly, ] <- SPR0ann[, !SelfOnly, ]

  PerRecruit             <- new('perrecruit')
  PerRecruit@apicalF     <- apicalF
  PerRecruit@NPR0        <- NPR0
  PerRecruit@NPR0_SP     <- NPR0_SP
  PerRecruit@SPR0        <- SPR0
  PerRecruit@NPRF        <- NPRF
  PerRecruit@NPRF_SP     <- if (IsSpawnTimeFrac) NPRF_SP else NPRF
  PerRecruit@SPRF        <- SPRFf
  PerRecruit@SPR         <- SPR
  PerRecruit@Biomass     <- Biomass
  PerRecruit@SBiomass    <- SBiomass
  PerRecruit@SProduction <- SProduction
  PerRecruit@Removals    <- Removals
  PerRecruit@Landings    <- Landings
  PerRecruit@Misc        <- list(F_annual_apical = F_annual_apical, SPR0Own = SPR0f)

  Sources <- names(HermCrossYear[[1]])
  if (length(Sources)) {
    Slots <- c(NPRF = 'NPRF', Biomass = 'Biomass', SBiomass = 'SBiomass',
               SProduction = 'SProduction', Removals = 'Removals', Landings = 'Landings',
               SPRF = 'SPRFf', SPR0 = 'SPR0f')
    PerRecruit@Misc$HermCross <- purrr::map(stats::setNames(Sources, Sources), \(k)
      purrr::map(Slots, \(v) {
        arr <- array(0, dim = c(length(SimNames), nStocks, nCalYears),
                     dimnames = list(Sim = SimNames, Stock = StockNames, Year = CalendarYears))
        for (cy in seq_len(nCalYears))
          for (s in names(HermCrossYear[[cy]][[k]]))
            arr[, s, cy] <- HermCrossYear[[cy]][[k]][[s]][[v]]
        arr
      }))
  }
  PerRecruit
}



# First-year unfished spawning production per recruit for each stock, as used
# by the stock-recruitment relationship in the population dynamics
.SRRPhi0Ref <- function(x) {
  OM <- if (inherits(x, 'hist')) x@OM else x
  UnfishedSP <- if (inherits(x, 'hist') && !EmptyObject(x@Unfished@Equilibrium))
    x@Unfished@Equilibrium@SProduction else
      CalcUnfished_Equilibrium(OM, silent = TRUE)@SProduction
  R0List <- purrr::map(OM@Stock, \(stock) stock@SRR@R0)
  nSimR0 <- max(purrr::map_int(R0List, \(r0) dim(r0)[1]), nSim(OM))
  R0     <- purrr::map(R0List, \(r0) ExtendSims(r0, nSimR0)) |> List2Array('Stock', pos = 2)
  .Phi0RefSeed(ExtendSims(UnfishedSP, nSimR0), R0, .RecLag(OM@Stock)) |>
    Array2List('Stock')
}

.HermSubsetSim <- function(Herm, sim) {
  if (is.null(Herm)) return(NULL)
  Herm$Hazard <- purrr::map(Herm$Hazard, \(h) {
    h <- .Aperm(h, c('Sim', 'Age', 'Year'))
    h[min(sim, dim(h)[1]), , , drop = FALSE]
  })
  Herm
}

# Herm pairs among `Stocks` (one complex), for .PrepPerRecruitInputs()
.HermForStocks <- function(OM, Stocks) {
  Resolved <- .HermResolvePairs(OM, Years(OM, 'Hist'))
  if (!length(Resolved$From)) return(NULL)
  StockNms <- StockNames(OM)
  From <- StockNms[Resolved$From]
  To   <- StockNms[Resolved$To]
  inFrom <- From %in% Stocks
  inTo   <- To %in% Stocks
  if (any(inFrom != inTo))
    cli::cli_warn(c(
      "!" = "{.field Herm}-linked stocks {.val {unique(c(From[inFrom != inTo], To[inFrom != inTo]))}} are in different complexes.",
      "i" = "Per-recruit and MSY reference points ignore {.field Herm} transitions between complexes. Put {.field Herm}-linked stocks in the same {.field OM@Complexes} entry to include them."
    ), .frequency = "once", .frequency_id = "MSEtool_herm_cross_complex")
  keep <- inFrom & inTo
  if (!any(keep)) return(NULL)
  list(From = From[keep], To = To[keep], Hazard = Resolved$Hazard[keep])
}

.PrepPerRecruitInputs <- function(StockList, FleetList, SPR0List, Years, EffortYears = NULL,
                                  RefSeason = NULL, Herm = NULL, Phi0Ref = NULL) {

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
                                     Stock@NaturalMortality@MeanAtAge |> .ArraySubsetYear(Years))
  
  PlusGroupList <- purrr::map(StockList, \(Stock) Stock@Ages@PlusGroup)
  
  MaturityList <- purrr::map(StockList, \(Stock)
                             Stock@Maturity@MeanAtAge |> .ArraySubsetYear(Years))
  
  SemelparousList <- purrr::map(StockList, \(Stock)
                                Stock@Maturity@Semelparous |> .ArraySubsetYear(Years))
  
  WeightList <- purrr::map(StockList, \(Stock)
                           Stock@Weight@MeanAtAge |> .ArraySubsetYear(Years))
  
  SpawnTimeFracList <- purrr::map(StockList, \(Stock) Stock@SRR@SpawnTimeFrac)
  
  FecundityList <- purrr::map(StockList, \(Stock)
                              Stock@Fecundity@MeanAtAge |> .ArraySubsetYear(Years))
  
  SPFrom <- purrr::map2(StockList, seq_along(StockList), \(stock, i)
    .ResolveSPFromWeights(stock@SRR@SPFrom, names(StockList), i)
  ) |> stats::setNames(names(StockList))
  
  # F-invariant: fleet allocation depends only on effort and efficiency
  StockFleetAllocation <- purrr::map(FleetList, \(fl)
                                     .CalcFleetAllocationF(fl, Years, EffortYears = EffortYears)
  ) |> List2Array('Stock', pos = 2)

  nSim_true            <- dim(StockFleetAllocation)[1]
  NaturalMortalityList <- purrr::map(NaturalMortalityList, ExtendSims, nSim = nSim_true)
  MaturityList         <- purrr::map(MaturityList,         ExtendSims, nSim = nSim_true)
  SemelparousList      <- purrr::map(SemelparousList,      ExtendSims, nSim = nSim_true)
  WeightList           <- purrr::map(WeightList,           ExtendSims, nSim = nSim_true)
  FecundityList        <- purrr::map(FecundityList,        ExtendSims, nSim = nSim_true)
  SPR0List             <- purrr::map(SPR0List,             ExtendSims, nSim = nSim_true)

  NPR0List <- NPR0_SPList <- NULL
  if (nSeason <= 1L) {
    NPR0List <- purrr::pmap(
      list(NaturalMortalityList, PlusGroupList, SemelparousList),
      \(NaturalMortality, PlusGroup, Semelparous)
      CalcSurvival(NaturalMortality, FishingMortality = NULL,
                  PlusGroup, SpawnTimeFrac = 0, Semelparous)
    )
    IsSpawnTimeFrac <- any(unlist(SpawnTimeFracList) != 0)
    NPR0_SPList <- if (IsSpawnTimeFrac) {
      purrr::pmap(
        list(NaturalMortalityList, PlusGroupList, SemelparousList, SpawnTimeFracList),
        \(NaturalMortality, PlusGroup, Semelparous, SpawnTimeFrac)
        CalcSurvival(NaturalMortality, FishingMortality = NULL,
                    PlusGroup, SpawnTimeFrac, Semelparous)
      )
    } else {
      NPR0List
    }
  }

  WeightFleetRetainedList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) {
      Fleet@WeightFleetRetained |> .ArraySubsetYear(Years)
    }) |>
      List2Array(pos = 4)
  })

  WeightFleetSelectedList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) {
      Fleet@WeightFleetSelected |> .ArraySubsetYear(Years)
    }) |>
      List2Array(pos = 4)
  })

  SelectivityFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@Selectivity@MeanAtAge |> .ArraySubsetYear(Years)) |>
      List2Array(pos = 4) |>
      .CheckSpatial('Selectivity')
  })
  
  RetentionFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@Retention@MeanAtAge |> .ArraySubsetYear(Years)) |>
      List2Array(pos = 4) |> 
      .CheckSpatial('Retention')
  }) 
  
  DiscardMortalityFleetList <- purrr::map(FleetList, \(fl) {
    purrr::map(fl, \(Fleet) Fleet@DiscardMortality@MeanAtAge |> .ArraySubsetYear(Years)) |>
      List2Array(pos = 4) |> 
      .CheckSpatial('DiscardMortality')
  }) 
  
  # SRR quantities needed for MSY recruitment scaling
  # For seasonal models, collapse to annual R0 and SPR0 for the equilibrium
  SimNamesInputs <- dimnames(StockFleetAllocation)[['Sim']]
  R0SeasonalList <- purrr::map(StockList, \(Stock) {
    R0 <- Stock@SRR@R0 |> .ArraySubsetYear(Years) |> ExtendSims(nSim_true)   # [Sim, Year(nSeason)]
    # a sim-invariant R0 collapsed to one sim carries a different Sim name after .SubsetSim()
    if (dim(R0)[1] == length(SimNamesInputs)) dimnames(R0)[[1]] <- SimNamesInputs
    R0
  })

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
    }) |> List2Array('Stock') |> .Aperm(c('Sim', 'Stock', 'Year'))

    RecParsList <- purrr::map2(StockList, SPR0List, \(Stock, SPR0) {
      sname <- Stock@Name
      nSim  <- dim(R0SeasonalList[[sname]])[1]
      pi_s  <- SeasonalWeightsList[[sname]]           # [Sim, nSeason*nCalYears]
      R0_mat  <- matrix(as.numeric(R0SeasonalList[[sname]]), nrow = nSim)
      SPR0_mat <- matrix(as.numeric(SPR0 |> .ArraySubsetYear(Years)), nrow = nSim)
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
        pars_s <- .ArraySubsetYear(pars, Years)
        pars_s[, seq(1L, nSeason * nCalYears, by = nSeason), drop = FALSE]
      })
      Pars$R0   <- R0_cy
      Pars$SPR0 <- SPR0_cy
      Pars
    })
  } else {
    SeasonalWeightsList <- NULL

    R0 <- R0SeasonalList |> List2Array('Stock') |> .Aperm(c('Sim', 'Stock', 'Year'))

    RecParsList <- purrr::map2(StockList, SPR0List, \(Stock, SPR0) {
      Pars      <- purrr::map(Stock@SRR@Pars, \(pars) .ArraySubsetYear(pars, Years))
      Pars$R0   <- .ArraySubsetYear(Stock@SRR@R0, Years)
      Pars$SPR0 <- .ArraySubsetYear(SPR0, Years)
      Pars
    })
  }
  
  if (length(RecParsList)> 1) {
    sp_ind             <- match(names(SPFrom), names(RecParsList))
    RecParsList        <- RecParsList[sp_ind]
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

  SRRModelList <- purrr::map(StockList, \(Stock) {
    mod <- Stock@SRR@Model
    if (inherits(mod, 'character')) mod <- get(mod)
    mod
  })

  Phi0SRR <- if (!is.null(Phi0Ref))
    .AnnualPhi0Ref(Phi0Ref[names(StockList)], .RecLag(StockList), Years, nSim_true,
                   SimNamesInputs, nSeason, SeasonalWeightsList, cal_years)

  if (!is.null(Herm)) {
    nAgeHerm <- length(StockList[[match(Herm$From[1], names(StockList))]]@Ages@Classes)
    HermPG   <- purrr::map_lgl(unique(c(Herm$From, Herm$To)), \(st)
      isTRUE(as.logical(PlusGroupList[[st]])))
    if (nSeason > 1L && length(unique(HermPG)) > 1L)
      cli::cli_abort("{.field Herm}-linked stocks must all have the same {.field Ages@PlusGroup} in seasonal models.")
    Herm$Hazard <- purrr::map(Herm$Hazard, \(h) {
      h <- .ArraySubsetYear(h, Years) |> ExtendSims(nSim_true)
      if (nSeason > 1L && all(HermPG)) h <- .ExtendPlusGroupSeasons(h, nSeason, nAgeHerm)
      h
    })
  }

  if (nSeason > 1L) {
    for (st in seq_along(StockList)) {
      if (!isTRUE(as.logical(PlusGroupList[[st]]))) next
      nAge <- dim(NaturalMortalityList[[st]])[match('Age', names(dimnames(NaturalMortalityList[[st]])))]
      Ext  <- \(x) .ExtendPlusGroupSeasons(x, nSeason, nAge)
      NaturalMortalityList[[st]]      <- Ext(NaturalMortalityList[[st]])
      MaturityList[[st]]              <- Ext(MaturityList[[st]])
      SemelparousList[[st]]           <- Ext(SemelparousList[[st]])
      WeightList[[st]]                <- Ext(WeightList[[st]])
      FecundityList[[st]]             <- Ext(FecundityList[[st]])
      WeightFleetRetainedList[[st]]   <- Ext(WeightFleetRetainedList[[st]])
      WeightFleetSelectedList[[st]]   <- Ext(WeightFleetSelectedList[[st]])
      SelectivityFleetList[[st]]      <- Ext(SelectivityFleetList[[st]])
      RetentionFleetList[[st]]        <- Ext(RetentionFleetList[[st]])
      DiscardMortalityFleetList[[st]] <- Ext(DiscardMortalityFleetList[[st]])
    }
  }

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
    WeightFleetRetainedList   = WeightFleetRetainedList,
    WeightFleetSelectedList   = WeightFleetSelectedList,
    SelectivityFleetList      = SelectivityFleetList,
    RetentionFleetList        = RetentionFleetList,
    DiscardMortalityFleetList = DiscardMortalityFleetList,
    RecParsList               = RecParsList,
    R0                        = R0,
    RelRecFunList             = RelRecFunList,
    SRRModelList              = SRRModelList,
    FleetNames                = FleetNames,
    Years                     = Years,
    nSeason                   = nSeason,
    SeasonalWeightsList       = SeasonalWeightsList,
    CalendarYears             = cal_years,
    RefSeason                 = RefSeason,
    NPR0List                  = NPR0List,
    NPR0_SPList               = NPR0_SPList,
    Herm                      = Herm,
    Phi0SRR                   = Phi0SRR
  )
}

# First-year unfished spawning production per recruit for each equilibrium year
# [Sim, Stock, Year]. Seasonal: recruits in season s come from spawning in
# season s - lag, weighted by the seasonal recruitment weights.
.AnnualPhi0Ref <- function(Phi0Ref, RecLag, Years, nSim, SimNames, nSeason,
                           SeasonalWeightsList, CalYears) {
  nSimRef <- max(nSim, purrr::map_int(Phi0Ref, \(x) dim(x)[1]))
  if (nSimRef > length(SimNames))
    SimNames <- dimnames(Phi0Ref[[which.max(purrr::map_int(Phi0Ref, \(x) dim(x)[1]))]])[[1]]
  purrr::imap(Phi0Ref, \(seed, st) {
    seed <- matrix(as.numeric(ExtendSims(seed, nSimRef)), nrow = nSimRef)
    if (nSeason <= 1L) {
      out <- matrix(seed[, 1], nrow = nSimRef, ncol = length(Years))
      dimnames(out) <- list(Sim = SimNames, Year = Years)
      return(out)
    }
    pi_s <- matrix(as.numeric(SeasonalWeightsList[[st]]), ncol = nSeason * length(CalYears))
    pi_s <- pi_s[pmin(seq_len(nSimRef), nrow(pi_s)), , drop = FALSE]
    out  <- matrix(0, nrow = nSimRef, ncol = length(CalYears),
                   dimnames = list(Sim = SimNames, Year = CalYears))
    for (cy in seq_along(CalYears)) {
      idx <- ((cy - 1L) * nSeason + 1L):(cy * nSeason)
      for (s in seq_len(nSeason)) {
        spawn <- ((s - 1L - RecLag[[st]]) %% nSeason) + 1L
        out[, cy] <- out[, cy] + pi_s[, idx[s]] * seed[, spawn]
      }
    }
    out
  }) |>
    List2Array('Stock') |>
    .Aperm(c('Sim', 'Stock', 'Year'))
}
