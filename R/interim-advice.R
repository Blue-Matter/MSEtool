
.BuildInterimAdvice <- function(Proj, Year, YearsProj, FleetNames, Areas) {

  OM      <- Proj@OM
  nSim    <- OM@nSim
  Seasons <- OM@Seasons %||NA% 1

  YearsHist <- Years(OM, "Historical")
  AllYears  <- c(YearsHist, YearsProj)
  TSIndex   <- match(Year, AllYears)
  Season    <- ((TSIndex - 1) %% Seasons) + 1
  CalYear   <- floor(Year)

  ComplexNames <- names(OM@Complexes)

  AdviceSimList <- MakeNamedList(seq_len(nSim))
  for (sim in seq_len(nSim)) {
    AdviceList <- MakeNamedList(ComplexNames)
    for (cx in ComplexNames) {
      AdviceList[[cx]] <- .BuildInterimAdviceOne(
        Proj, sim, CalYear, Season, cx, FleetNames, Areas, YearsHist, Seasons
      )
    }
    AdviceSimList[[sim]] <- AdviceList
  }

  list(
    AdviceSimList      = AdviceSimList,
    AggBagLimitSimList = MakeNamedList(seq_len(nSim))
  )
}

.BuildInterimAdviceOne <- function(Proj, sim, CalYear, Season, Stock, FleetNames,
                                  Areas, YearsHist, Seasons) {

  Advice <- methods::new("advice")

  InterimAdvice <- Proj@OM@InterimAdvice
  if (!is.null(InterimAdvice)) {
    if ("Stock" %in% names(InterimAdvice)) {
      rows <- InterimAdvice[InterimAdvice$Year == CalYear & InterimAdvice$Stock == Stock, , drop = FALSE]
    } else {
      rows <- InterimAdvice[InterimAdvice$Year == CalYear, , drop = FALSE]
    }

    if (nrow(rows)) {
      if (!"CV" %in% names(rows)) rows$CV <- NA_real_
      if (!"Max" %in% names(rows)) rows$Max <- NA_real_
      if (!"Fleet" %in% names(rows)) rows$Fleet <- NA_character_

      tac_rows <- rows[rows$Type == "TAC", , drop = FALSE]
      eff_rows <- rows[rows$Type == "Effort", , drop = FALSE]

      if (nrow(tac_rows))
        Advice <- .FillInterimTAC(Advice, Proj, sim, CalYear, Season, Stock, FleetNames,
                                 tac_rows, YearsHist, Seasons)

      if (nrow(eff_rows))
        Advice <- .FillInterimEffort(Advice, Proj, sim, CalYear, Season, Stock, FleetNames,
                                    eff_rows, YearsHist, Seasons, Areas)
    }
  }

  .CheckAdvice(Advice, Proj, FleetNames, Areas, sim, name = Stock)
}

# one mean-1 lognormal multiplier per sim, shared by all rows of a Year x Stock x Type
.SampleInterimMultiplier <- function(OM, CalYear, Stock, Type, rows, sim) {
  pos <- rows$Mean > 0
  if (!any(pos)) return(1)
  CV <- rows$CV[pos][1]
  if (is.na(CV) || CV <= 0) return(1)

  sigma <- sqrt(log(1 + CV^2))
  mu    <- -sigma^2 / 2
  Upper <- min(c(Inf, rows$Max[pos] / rows$Mean[pos]), na.rm = TRUE)
  pUpper <- if (is.finite(Upper)) stats::plnorm(Upper, mu, sigma) else 1

  key <- paste(OM@Seed, CalYear, Stock, Type, sep = "_")
  seed_val <- digest::digest2int(key)

  has_seed <- exists(".Random.seed", envir = .GlobalEnv)
  old_seed <- if (has_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (has_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
  })

  set.seed(seed_val)
  draws <- stats::qlnorm(stats::runif(OM@nSim, 0, pUpper), mu, sigma)
  draws[sim]
}

.LastHistYearIndices <- function(YearsHist, Seasons) {
  n <- length(YearsHist)
  (n - Seasons + 1):n
}

.SeasonalFractionSum <- function(hist_vals, Season, Seasons) {
  if (Seasons == 1) return(1)
  total <- sum(hist_vals, na.rm = TRUE)
  if (!is.finite(total) || total <= 0) return(1 / Seasons)
  hist_vals[Season] / total
}

.SeasonalFractionMean <- function(hist_vals, Season, Seasons) {
  if (Seasons == 1) return(1)
  m <- mean(hist_vals, na.rm = TRUE)
  if (!is.finite(m) || m <= 0) return(1)
  hist_vals[Season] / m
}

.FillInterimTAC <- function(Advice, Proj, sim, CalYear, Season, Stock, FleetNames,
                           rows, YearsHist, Seasons) {

  nFleet  <- length(FleetNames)
  last_ts <- .LastHistYearIndices(YearsHist, Seasons)
  stk_idx <- match(Stock, dimnames(Proj@Landings)[["Stock"]])
  if (is.na(stk_idx))
    stk_idx <- Proj@OM@Complexes[[Stock]][1]

  has_fleet <- !is.na(rows$Fleet) & nzchar(rows$Fleet)

  if (!any(has_fleet)) {
    if (nrow(rows) > 1)
      cli::cli_abort(c(
        "Multiple unfleeted {.val TAC} rows in `InterimAdvice` for Year {.val {CalYear}}, Stock {.val {Stock}}.",
        "i" = "Supply one row (`Fleet = NA`, a stock total) or one row per fleet."
      ))
    row <- rows[1, ]
    hist_arr  <- Proj@Landings[sim, stk_idx, last_ts, , drop = FALSE]
    hist_arr  <- array(hist_arr, dim = dim(hist_arr)[3:4])  # Season x Fleet
    hist_vals <- rowSums(hist_arr)
    frac  <- .SeasonalFractionSum(hist_vals, Season, Seasons)
    value <- row$Mean * .SampleInterimMultiplier(Proj@OM, CalYear, Stock, "TAC", row, sim)

    Advice@TAC     <- value * frac
    Advice@TACType <- if (!is.null(row$TACType) && !is.na(row$TACType)) row$TACType else "Removals"
    Advice@TACUnit <- if (!is.null(row$TACUnit) && !is.na(row$TACUnit)) row$TACUnit else "Biomass"
    return(Advice)
  }

  if (sum(has_fleet) != nFleet)
    cli::cli_abort(c(
      "`InterimAdvice` {.val TAC} rows for Year {.val {CalYear}}, Stock {.val {Stock}} specify some but not all fleets.",
      "i" = "Specify all {nFleet} fleets, or none (a stock total allocated across fleets)."
    ))

  mult    <- .SampleInterimMultiplier(Proj@OM, CalYear, Stock, "TAC", rows, sim)
  tac_vec <- numeric(nFleet)
  for (i in seq_len(nFleet)) {
    fl     <- FleetNames[i]
    row    <- rows[rows$Fleet == fl, ][1, ]
    fl_idx <- match(fl, dimnames(Proj@Landings)[["Fleet"]])
    hist_vals <- Proj@Landings[sim, stk_idx, last_ts, fl_idx]
    frac   <- .SeasonalFractionSum(hist_vals, Season, Seasons)
    tac_vec[i] <- row$Mean * mult * frac
  }

  Advice@TAC     <- tac_vec
  Advice@TACType <- if (!is.null(rows$TACType) && !is.na(rows$TACType[1])) rows$TACType[1] else "Removals"
  Advice@TACUnit <- if (!is.null(rows$TACUnit) && !is.na(rows$TACUnit[1])) rows$TACUnit[1] else "Biomass"
  Advice
}

.FillInterimEffort <- function(Advice, Proj, sim, CalYear, Season, Stock, FleetNames,
                              rows, YearsHist, Seasons, Areas) {

  nFleet    <- length(FleetNames)
  last_ts   <- .LastHistYearIndices(YearsHist, Seasons)
  season_ts <- last_ts[Season]

  has_fleet <- !is.na(rows$Fleet) & nzchar(rows$Fleet)

  eff_vec <- numeric(nFleet)

  if (!any(has_fleet)) {
    if (nrow(rows) > 1)
      cli::cli_abort(c(
        "Multiple unfleeted {.val Effort} rows in `InterimAdvice` for Year {.val {CalYear}}, Stock {.val {Stock}}.",
        "i" = "Supply one row (`Fleet = NA`, applied identically to every fleet) or one row per fleet."
      ))
    row <- rows[1, ]
    hist_mat  <- Proj@Effort[sim, last_ts, , drop = FALSE]
    hist_mat  <- array(hist_mat, dim = dim(hist_mat)[2:3])  # Season x Fleet
    hist_vals <- rowMeans(hist_mat)
    frac  <- .SeasonalFractionMean(hist_vals, Season, Seasons)
    value <- row$Mean * .SampleInterimMultiplier(Proj@OM, CalYear, Stock, "Effort", row, sim)
    eff_vec[] <- value * frac

  } else {

    if (sum(has_fleet) != nFleet)
      cli::cli_abort(c(
        "`InterimAdvice` {.val Effort} rows for Year {.val {CalYear}}, Stock {.val {Stock}} specify some but not all fleets.",
        "i" = "Specify all {nFleet} fleets, or none (identical value across fleets)."
      ))

    mult <- .SampleInterimMultiplier(Proj@OM, CalYear, Stock, "Effort", rows, sim)
    for (i in seq_len(nFleet)) {
      fl     <- FleetNames[i]
      row    <- rows[rows$Fleet == fl, ][1, ]
      fl_idx <- match(fl, dimnames(Proj@Effort)[["Fleet"]])
      hist_vals <- Proj@Effort[sim, last_ts, fl_idx]
      frac   <- .SeasonalFractionMean(hist_vals, Season, Seasons)
      eff_vec[i] <- row$Mean * mult * frac
    }
  }

  dist_mat <- Proj@Distribution[sim, season_ts, , , drop = FALSE]
  dist_mat <- array(dist_mat, dim = dim(dist_mat)[3:4])  # Fleet x Area

  Advice@Effort <- dist_mat * eff_vec  
  dimnames(Advice@Effort) <- list(Fleet = FleetNames, Area = Areas)
  Advice@EffType <- "Abs"
  Advice
}
