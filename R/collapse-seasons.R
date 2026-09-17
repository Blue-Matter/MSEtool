
#' Collapse a Seasonal Operating Model to an Annual Operating Model
#'
#' Converts an Operating Model with sub-annual seasonal resolution
#' (`OM@Seasons > 1`) into an annual Operating Model (`Seasons = 1`) that
#' approximates the same annual-scale fishery dynamics. 
#'
#' @param OM A seasonal [OM()] object (`OM@Seasons > 1`). 
#' Multi-area (`nArea > 1`) OMs are not yet supported.
#' @param silent `logical(1)`. If `TRUE`, suppresses informational console
#'   messages. Defaults to `FALSE`.
#'
#' @return An updated [OM()] object with `Seasons = 1` and re-derived
#'   biology/fishery arrays at annual resolution.
#'
#' ## Method
#'
#' Seasons are included in the `Year` dimension (as fractional-year timesteps) and the
#' `Age` dimension (as fractional-age classes) of every populated array. So
#' "collapsing seasons" means re-deriving both dimensions at annual
#' resolution, following a cohort's actual path through the `Seasons`
#' sub-annual steps of a calendar year (age `a`, `a + 1/Seasons`, ...,
#' `a + (Seasons-1)/Seasons`):
#'
#' - **Natural mortality** and **fecundity** are summed
#' - **Length-** and **weight-at-age**, and **maturity-at-age** are read
#'   directly off the populated seasonal array at the exact integer-age, first-season-of-year cell
#' - **Seasonal `R0`** is summed to annual `R0`
#' - **Fleet dynamics** (effort, catchability, selectivity, retention,
#'   discard mortality, weight-at-age-in-the-catch) follow the same
#'   F-weighted-average reconstruction used by [CombineFleets()], but
#'   weighting across a cohort's season-steps within a year instead of
#'   across fleets.
#'
#' This is an approximation, not an exact transformation: the population
#' dynamics apply mortality and fishing simultaneously within each season
#' (`exp(-(M+F))` per timestep), so an annual model cannot exactly reproduce
#' `Seasons` sequential simultaneous-mortality events with a single annual
#' one. The approximation is expected to be close whenever seasonal
#' heterogeneity in `F` and `M` is mild.
#'
#' @seealso [CombineFleets()] for the analogous fleet-aggregation function.
#'
#' @export
CollapseSeasons <- function(OM, silent = FALSE) {

  .CheckClass(OM)

  if (is.null(OM@Seasons) || OM@Seasons <= 1)
    return(OM)

  Seasons <- OM@Seasons

  for (st in seq_len(nStock(OM))) {
    if (nArea(OM, st) > 1)
      cli::cli_abort(c(
        "x" = "{.fn CollapseSeasons} does not yet support multi-area OMs.",
        "i" = "Stock {.val {st}} has {.val {nArea(OM, st)}} areas."
      ))
  }

  OM <- Populate(OM, silent = TRUE)

  IndexMap <- .CollapseSeasonsYearMap(OM)

  if (!silent)
    cli::cli_alert_info("Collapsing {.val {Seasons}}-season OM to an annual OM")

  nSt <- nStock(OM)
  StockNew <- vector("list", nSt)
  FleetNew <- vector("list", nSt)

  for (st in seq_len(nSt)) {
    AgeMap <- .CollapseSeasonsAgeMap(OM@Stock[[st]]@Ages, Seasons)
    StockNew[[st]] <- .CollapseSeasonsStock(OM@Stock[[st]], AgeMap, IndexMap)

    nFl <- length(OM@Fleet[[st]])
    FleetNew[[st]] <- vector("list", nFl)
    names(FleetNew[[st]]) <- names(OM@Fleet[[st]])
    for (fl in seq_len(nFl)) {
      FleetNew[[st]][[fl]] <- .CollapseSeasonsFleet(OM@Fleet[[st]][[fl]], AgeMap, IndexMap)
    }
  }
  names(StockNew) <- names(OM@Stock)

  OM@Stock <- StockNew
  OM@Fleet <- FleetNew
  OM@Seasons <- 1
  OM@RefSeason <- NULL
  OM@RefEffortYears <- NULL
  OM@Years <- CalcYears(OM@nYear, OM@pYear, OM@CurrentYear, Seasons = 1)

  if (length(OM@Data))
    OM <- .CollapseSeasonsData(OM, IndexMap)

  methods::validObject(OM)
  OM
}


.CollapseSeasonsYearMap <- function(OM) {
  Seasons <- OM@Seasons
  SeasonalYears <- CalcYears(OM@nYear, OM@pYear, OM@CurrentYear, Seasons = Seasons)
  AnnualYears   <- seq(OM@CurrentYear - OM@nYear + 1, OM@CurrentYear + OM@pYear)

  YearBlock <- rep(seq_along(AnnualYears), each = Seasons)[seq_along(SeasonalYears)]

  list(
    Seasons       = Seasons,
    SeasonalYears = SeasonalYears,
    AnnualYears   = AnnualYears,
    YearBlock     = YearBlock
  )
}

.CollapseSeasonsAgeMap <- function(Ages, Seasons) {

  AgeSeasons <- CalcSeasons(Ages@Units)
  if (!isTRUE(AgeSeasons == Seasons))
    cli::cli_abort(c(
      "x" = "`Ages@Units` ({.val {Ages@Units}}) does not match `OM@Seasons` ({.val {Seasons}}).",
      "i" = "{.fn CollapseSeasons} requires the stock's age-class resolution to match the OM's season resolution."
    ))

  MaxAgeYears <- Ages@MaxAge / AgeSeasons
  MinAgeYears <- Ages@MinAge / AgeSeasons

  AnnualMinAge <- ceiling(MinAgeYears)
  AnnualAges   <- seq(AnnualMinAge, MaxAgeYears)

  SeasonalClasses <- Ages@Classes

  PathIdx <- sapply(AnnualAges, function(a) {
    sapply(seq_len(Seasons), function(s) {
      ageval <- min(a + (s - 1) / Seasons, MaxAgeYears)
      match(round(ageval, 3), round(SeasonalClasses, 3))
    })
  })
  if (is.null(dim(PathIdx)))
    PathIdx <- matrix(PathIdx, nrow = Seasons)

  if (anyNA(PathIdx))
    cli::cli_abort("Internal error in {.fn CollapseSeasons}: could not map annual ages to seasonal age classes.")

  ExactIdx <- match(round(AnnualAges, 3), round(SeasonalClasses, 3))
  if (anyNA(ExactIdx))
    cli::cli_abort("Internal error in {.fn CollapseSeasons}: integer ages are not a subset of the seasonal age classes.")

  if (AnnualMinAge > MinAgeYears) {
    gap_ages <- seq(MinAgeYears, AnnualMinAge - 1 / Seasons, by = 1 / Seasons)
    PartialGapIdx <- match(round(gap_ages, 3), round(SeasonalClasses, 3))
    if (anyNA(PartialGapIdx))
      cli::cli_abort("Internal error in {.fn CollapseSeasons}: could not map the pre-recruit gap ages to seasonal age classes.")
  } else {
    PartialGapIdx <- integer(0)
  }

  list(
    AgeSeasons    = AgeSeasons,
    PartialGapIdx = PartialGapIdx,
    AnnualAges    = AnnualAges,
    AnnualAgesObj = Ages(MaxAge = MaxAgeYears, MinAge = AnnualMinAge, Units = "year", PlusGroup = Ages@PlusGroup),
    PathIdx       = PathIdx,
    ExactIdx      = ExactIdx
  )
}


.CollapseEnsureFullYears <- function(arr, IndexMap) {
  ExtendYears(arr, Years = IndexMap$SeasonalYears, backfill = TRUE)
}

.CollapseCohortPathSum <- function(arr, AgeMap, IndexMap) {

  arr <- .CollapseEnsureFullYears(arr, IndexMap)
  nSim <- dim(arr)[1]
  nAnnualAge <- length(AgeMap$AnnualAges)
  Seasons <- IndexMap$Seasons

  arrYears  <- as.numeric(dimnames(arr)$Year)
  yearIdx   <- match(arrYears, IndexMap$SeasonalYears)
  blockAll  <- IndexMap$YearBlock[yearIdx]
  uBlocks   <- sort(unique(blockAll))

  out <- array(
    0,
    dim = c(nSim, nAnnualAge, length(uBlocks)),
    dimnames = list(
      Sim  = dimnames(arr)$Sim,
      Age  = AgeMap$AnnualAges,
      Year = IndexMap$AnnualYears[uBlocks]
    )
  )

  for (bi in seq_along(uBlocks)) {
    ts_idx <- which(blockAll == uBlocks[bi])
    for (s in seq_along(ts_idx)) {
      slice <- arr[, AgeMap$PathIdx[s, ], ts_idx[s], drop = FALSE]
      dim(slice) <- dim(slice)[1:2]
      out[, , bi] <- out[, , bi] + slice
    }
  }
  out
}

.CollapseCohortPathSum4D <- function(arr, AgeMap, IndexMap) {
  arr3 <- DropDimension(arr, "Area", warn = FALSE)
  out3 <- .CollapseCohortPathSum(arr3, AgeMap, IndexMap)
  AddDimension(out3, "Area", pos = 4)
}

.CollapseCohortPathPick <- function(arr, AgeMap, IndexMap) {

  arr <- .CollapseEnsureFullYears(arr, IndexMap)
  nSim <- dim(arr)[1]

  arrYears <- as.numeric(dimnames(arr)$Year)
  yearIdx  <- match(arrYears, IndexMap$SeasonalYears)
  blockAll <- IndexMap$YearBlock[yearIdx]
  uBlocks  <- sort(unique(blockAll))

  firstTS <- vapply(uBlocks, \(b) which(blockAll == b)[1], integer(1))

  out <- arr[, AgeMap$ExactIdx, firstTS, drop = FALSE]
  dimnames(out) <- list(
    Sim  = dimnames(arr)$Sim,
    Age  = AgeMap$AnnualAges,
    Year = IndexMap$AnnualYears[uBlocks]
  )
  out
}

.CollapseYearSum <- function(arr, IndexMap) {

  arr <- .CollapseEnsureFullYears(arr, IndexMap)
  nSim <- dim(arr)[1]
  arrYears <- as.numeric(dimnames(arr)$Year)
  yearIdx  <- match(arrYears, IndexMap$SeasonalYears)
  blockAll <- IndexMap$YearBlock[yearIdx]
  uBlocks  <- sort(unique(blockAll))

  out <- matrix(
    0, nrow = nSim, ncol = length(uBlocks),
    dimnames = list(Sim = dimnames(arr)$Sim, Year = IndexMap$AnnualYears[uBlocks])
  )
  for (bi in seq_along(uBlocks)) {
    ts_idx <- which(blockAll == uBlocks[bi])
    out[, bi] <- rowSums(arr[, ts_idx, drop = FALSE])
  }
  out
}

.CollapseYearMean <- function(arr, IndexMap) {

  arr <- .CollapseEnsureFullYears(arr, IndexMap)
  nSim <- dim(arr)[1]
  arrYears <- as.numeric(dimnames(arr)$Year)
  yearIdx  <- match(arrYears, IndexMap$SeasonalYears)
  blockAll <- IndexMap$YearBlock[yearIdx]
  uBlocks  <- sort(unique(blockAll))

  out <- matrix(
    0, nrow = nSim, ncol = length(uBlocks),
    dimnames = list(Sim = dimnames(arr)$Sim, Year = IndexMap$AnnualYears[uBlocks])
  )
  for (bi in seq_along(uBlocks)) {
    ts_idx <- which(blockAll == uBlocks[bi])
    out[, bi] <- rowMeans(arr[, ts_idx, drop = FALSE])
  }
  out
}

.CollapseRecDevWeighted <- function(RecDev, R0, IndexMap) {
  if (is.null(RecDev)) return(NULL)

  origYears  <- as.numeric(dimnames(RecDev)$Year)
  origIdx    <- match(origYears, IndexMap$SeasonalYears)
  origBlocks <- sort(unique(IndexMap$YearBlock[origIdx]))

  RecDev <- .CollapseEnsureFullYears(RecDev, IndexMap)
  R0     <- .CollapseEnsureFullYears(R0, IndexMap)

  WeightedSum <- .CollapseYearSum(ArrayMultiply(RecDev, R0), IndexMap)
  R0Sum       <- .CollapseYearSum(R0, IndexMap)

  keep <- as.numeric(dimnames(WeightedSum)$Year) %in% IndexMap$AnnualYears[origBlocks]
  WeightedSum <- WeightedSum[, keep, drop = FALSE]
  R0Sum       <- R0Sum[, keep, drop = FALSE]

  out <- ArrayDivide(WeightedSum, R0Sum)
  out[out == 0] <- 1
  out
}


.CollapseSpatialAtAge <- function(arr, AgeMap, IndexMap, Stock) {
  if (is.null(arr)) return(NULL)
  dn <- names(dimnames(arr))
  if (is.null(dn) || !"Age" %in% dn) return(arr)

  if (dim(arr)[which(dn == "Age")] != length(Stock@Ages@Classes)) return(arr)

  extra_dims <- setdiff(dn, c("Sim", "Age", "Year"))
  extra_positions <- match(extra_dims, dn)

  arr3 <- arr
  for (ed in extra_dims) arr3 <- DropDimension(arr3, ed, warn = FALSE)

  out <- .CollapseCohortPathPick(arr3, AgeMap, IndexMap)
  for (i in order(extra_positions)) out <- AddDimension(out, extra_dims[i], pos = extra_positions[i])
  out
}

.CollapseAtAgeParam <- function(x, AgeMap, IndexMap, Stock) {
  if (is.null(x) || !is.array(x) || !"Age" %in% names(dimnames(x)))
    return(x)
  if (dim(x)[which(names(dimnames(x)) == "Age")] != length(Stock@Ages@Classes)) {
    if (length(x) == 1) return(as.numeric(x))
    return(x)
  }
  .CollapseCohortPathPick(x, AgeMap, IndexMap)
}

.ApplyPreRecruitSurvival <- function(R0, M, AgeMap, IndexMap) {

  if (!length(AgeMap$PartialGapIdx))
    return(R0)

  R0 <- .CollapseEnsureFullYears(R0, IndexMap)
  M  <- .CollapseEnsureFullYears(M,  IndexMap)

  nTS <- dim(R0)[2]
  nSim <- dim(R0)[1]
  logSurvival <- matrix(0, nSim, nTS)

  for (k in seq_along(AgeMap$PartialGapIdx)) {
    age_idx <- AgeMap$PartialGapIdx[k]
    shift   <- k - 1L
    src_idx <- pmin(seq_len(nTS) + shift, nTS)
    Mk <- M[, age_idx, src_idx, drop = FALSE]
    dim(Mk) <- dim(Mk)[c(1, 3)]
    logSurvival <- logSurvival + Mk
  }

  R0 * exp(-logSurvival)
}

.CollapseSeasonsStock <- function(Stock, AgeMap, IndexMap) {

  M_annual   <- .CollapseCohortPathSum(Stock@NaturalMortality@MeanAtAge, AgeMap, IndexMap)
  Fec_annual <- .CollapseCohortPathSum(Stock@Fecundity@MeanAtAge, AgeMap, IndexMap)

  Len_annual <- .CollapseCohortPathPick(Stock@Length@MeanAtAge, AgeMap, IndexMap)
  Wt_annual  <- .CollapseCohortPathPick(Stock@Weight@MeanAtAge, AgeMap, IndexMap)
  Mat_annual <- .CollapseCohortPathPick(Stock@Maturity@MeanAtAge, AgeMap, IndexMap)

  R0_seasonal_adj <- .ApplyPreRecruitSurvival(Stock@SRR@R0, Stock@NaturalMortality@MeanAtAge, AgeMap, IndexMap)
  R0_annual <- .CollapseYearSum(R0_seasonal_adj, IndexMap)

  NewStock <- Stock
  NewStock@Ages    <- AgeMap$AnnualAgesObj
  NewStock@Seasons <- 1

  NewStock@NaturalMortality <- NaturalMortality(MeanAtAge = M_annual, Units = "year")

  NewStock@Length <- Length(
    MeanAtAge = Len_annual,
    Units     = Stock@Length@Units,
    CVatAge   = .CollapseAtAgeParam(Stock@Length@CVatAge, AgeMap, IndexMap, Stock),
    Dist      = Stock@Length@Dist,
    TruncSD   = Stock@Length@TruncSD
  )

  NewStock@Weight <- Weight(
    MeanAtAge = Wt_annual,
    Units     = Stock@Weight@Units,
    CVatAge   = .CollapseAtAgeParam(Stock@Weight@CVatAge, AgeMap, IndexMap, Stock),
    Dist      = Stock@Weight@Dist,
    TruncSD   = Stock@Weight@TruncSD
  )

  NewStock@Maturity  <- Maturity(MeanAtAge = Mat_annual)
  NewStock@Fecundity <- Fecundity(MeanAtAge = Fec_annual, Units = Stock@Fecundity@Units)

  NewStock@Spatial@UnfishedDist <- .CollapseSpatialAtAge(Stock@Spatial@UnfishedDist, AgeMap, IndexMap, Stock)
  NewStock@Spatial@ProbStaying  <- .CollapseSpatialAtAge(Stock@Spatial@ProbStaying,  AgeMap, IndexMap, Stock)
  NewStock@Spatial@Movement     <- .CollapseSpatialAtAge(Stock@Spatial@Movement,     AgeMap, IndexMap, Stock)

  NewStock@SRR@R0         <- R0_annual
  NewStock@SRR@RecDevHist <- .CollapseRecDevWeighted(Stock@SRR@RecDevHist, R0_seasonal_adj, IndexMap)
  NewStock@SRR@RecDevProj <- .CollapseRecDevWeighted(Stock@SRR@RecDevProj, R0_seasonal_adj, IndexMap)
  NewStock@SRR@RecDevInit <- .CollapseRecDevInit(Stock@SRR@RecDevInit, AgeMap)

  if (length(Stock@SRR@SpawnLag))
    NewStock@SRR@SpawnLag <- round(Stock@SRR@SpawnLag / AgeMap$AgeSeasons)

  if (length(Stock@SRR@SpawnTimeFrac))
    NewStock@SRR@SpawnTimeFrac <- Stock@SRR@SpawnTimeFrac / AgeMap$AgeSeasons

  NewStock
}


#' @keywords internal
.CollapseRecDevInit <- function(RecDevInit, AgeMap) {
  if (is.null(RecDevInit)) return(NULL)

  idx <- AgeMap$ExactIdx[-1] - 1L
  RecDevInit[, idx, drop = FALSE] |>
    `dimnames<-`(list(Sim = dimnames(RecDevInit)$Sim, Age = AgeMap$AnnualAges[-1]))
}

.CollapseSeasonsFleet <- function(Fleet, AgeMap, IndexMap) {

  Effort_s     <- Fleet@Effort@Effort
  Efficiency_s <- Fleet@Catchability@Efficiency
  Sel_s        <- Fleet@Selectivity@MeanAtAge
  Ret_s        <- Fleet@Retention@MeanAtAge
  Disc_s       <- Fleet@DiscardMortality@MeanAtAge
  WFSel_s      <- Fleet@WeightFleetSelected
  WFRet_s      <- Fleet@WeightFleetRetained

  apicalF_s <- ArrayMultiply(Effort_s, Efficiency_s)
  apicalF_s_expanded <- apicalF_s |>
    AddDimension("Age", pos = 2) |>
    AddDimension("Area", pos = 4)

  FInteract_s <- ArrayMultiply(apicalF_s_expanded, Sel_s)
  FRetain_s   <- ArrayMultiply(FInteract_s, Ret_s)

  discZ_s <- -log(1 - Disc_s)
  discZ_s[!is.finite(discZ_s)] <- Inf
  discZF_s <- ArrayMultiply(FInteract_s, discZ_s)

  WFSel_s_x <- ArrayMultiply(FInteract_s, AddDimension(WFSel_s, "Area"))
  WFRet_s_x <- ArrayMultiply(FRetain_s,   AddDimension(WFRet_s, "Area"))

  FInteract_a <- .CollapseCohortPathSum4D(FInteract_s, AgeMap, IndexMap)
  FRetain_a   <- .CollapseCohortPathSum4D(FRetain_s,   AgeMap, IndexMap)
  discZF_a    <- .CollapseCohortPathSum4D(discZF_s,    AgeMap, IndexMap)
  WFSel_a_x   <- .CollapseCohortPathSum4D(WFSel_s_x,   AgeMap, IndexMap)
  WFRet_a_x   <- .CollapseCohortPathSum4D(WFRet_s_x,   AgeMap, IndexMap)

  Selectivity_a <- .StandardizeF(FInteract_a)

  nms <- names(dimnames(FInteract_a))
  age_ind <- which(nms == "Age")
  apicalF_a <- apply(FInteract_a, nms[-age_ind], max)
  names(dimnames(apicalF_a)) <- nms[-age_ind]
  apicalF_a <- DropDimension(apicalF_a, "Area", warn = FALSE)

  Retention_a        <- ArrayDivide(FRetain_a, FInteract_a)
  DiscardMortality_a <- 1 - exp(-ArrayDivide(discZF_a, FInteract_a))

  WeightFleetSelected_a <- ArrayDivide(WFSel_a_x, FInteract_a) |> DropDimension("Area", warn = FALSE)
  WeightFleetRetained_a <- ArrayDivide(WFRet_a_x, FRetain_a)   |> DropDimension("Area", warn = FALSE)

  HistBlockYears <- {
    arrYears <- as.numeric(dimnames(Effort_s)$Year)
    yearIdx  <- match(arrYears, IndexMap$SeasonalYears)
    blockAll <- IndexMap$YearBlock[yearIdx]
    IndexMap$AnnualYears[sort(unique(blockAll))]
  }
  apicalF_a_hist <- apicalF_a[, as.character(HistBlockYears), drop = FALSE]

  Efficiency_a <- .CollapseYearMean(Efficiency_s, IndexMap)
  Efficiency_a_hist <- Efficiency_a[, as.character(HistBlockYears), drop = FALSE]
  Effort_a     <- ArrayDivide(apicalF_a_hist, Efficiency_a_hist)

  NewFleet <- Fleet
  NewFleet@Seasons <- 1

  Effort(NewFleet)               <- Effort(Effort = Effort_a)
  Catchability(NewFleet)         <- Catchability(Efficiency = Efficiency_a)
  Selectivity(NewFleet)          <- Selectivity(MeanAtAge = Selectivity_a)
  Retention(NewFleet)            <- Retention(MeanAtAge = Retention_a)
  DiscardMortality(NewFleet)     <- DiscardMortality(MeanAtAge = DiscardMortality_a)
  WeightFleetSelected(NewFleet)  <- WeightFleetSelected_a
  WeightFleetRetained(NewFleet)  <- WeightFleetRetained_a

  NewFleet
}

.CollapseSeasonsData <- function(OM, IndexMap) {

  Seasons <- IndexMap$Seasons

  for (st in seq_along(OM@Data)) {
    d <- OM@Data[[st]]
    if (is.null(d) || !length(d@Years)) next

    d@Seasons <- 1

    for (type in c("Landings", "Discards")) {
      cd <- slot(d, type)
      if (is.null(cd@Value)) next

      YearsSeasonal <- as.numeric(rownames(cd@Value) %||% d@Years)
      yearIdx  <- match(YearsSeasonal, IndexMap$SeasonalYears)
      blockAll <- IndexMap$YearBlock[yearIdx]
      uBlocks  <- sort(unique(blockAll))

      NewValue <- matrix(
        NA_real_, nrow = length(uBlocks), ncol = ncol(cd@Value),
        dimnames = list(Year = IndexMap$AnnualYears[uBlocks], Fleet = colnames(cd@Value))
      )
      for (bi in seq_along(uBlocks)) {
        rows <- which(blockAll == uBlocks[bi])
        NewValue[bi, ] <- colSums(cd@Value[rows, , drop = FALSE], na.rm = TRUE)
      }
      cd@Value <- NewValue
      slot(d, type) <- cd
    }

    for (type in c("CPUE", "Survey")) {
      idata <- slot(d, type)
      if (is.null(idata@Value)) next

      YearsSeasonal <- as.numeric(rownames(idata@Value) %||% d@Years)
      yearIdx  <- match(YearsSeasonal, IndexMap$SeasonalYears)
      blockAll <- IndexMap$YearBlock[yearIdx]
      AnnualYr <- IndexMap$AnnualYears[blockAll]
      withinYearFrac <- YearsSeasonal - floor(YearsSeasonal)

      rownames(idata@Value) <- AnnualYr
      if (!is.null(idata@CV)) rownames(idata@CV) <- AnnualYr

      if (length(idata@Timing) != ncol(idata@Value))
        idata@Timing <- rep(idata@Timing, length.out = ncol(idata@Value))
      for (i in seq_len(ncol(idata@Value))) {
        obs <- which(!is.na(idata@Value[, i]))
        if (length(obs)) idata@Timing[i] <- withinYearFrac[obs[1]]
      }

      slot(d, type) <- idata
    }

    d@Years <- IndexMap$AnnualYears
    OM@Data[[st]] <- d
  }
  OM
}
