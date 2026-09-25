
.InterimTimesteps <- function(OM) {
  YearsProj <- Years(OM, "Projection")
  if (is.null(OM@MPStartYear))
    return(YearsProj[0])
  YearsProj[floor(YearsProj) < OM@MPStartYear]
}

.CheckInterimAdvice <- function(Proj) {
  OM <- Proj@OM
  IA <- OM@InterimAdvice
  InterimTS <- .InterimTimesteps(OM)

  if (!length(InterimTS)) {
    if (!is.null(IA))
      cli::cli_abort(c(
        "`InterimAdvice` is specified but there are no interim years.",
        "i" = "Interim years are projection years before `MPStartYear`; set `MPStartYear` after the first projection year ({.val {floor(Years(OM, 'Projection')[1])}}), or set `InterimAdvice = NULL`."
      ))
    return(Proj)
  }

  if (is.null(IA))
    cli::cli_abort(c(
      "`MPStartYear = {OM@MPStartYear}` leaves interim year(s) {.val {unique(floor(InterimTS))}} but `InterimAdvice` is not specified.",
      "i" = "Supply `InterimAdvice` for every interim year, or set `MPStartYear = NULL`."
    ))

  if ("Stock" %in% names(IA) && !"Complex" %in% names(IA)) {
    cli::cli_warn(c(
      "`InterimAdvice$Stock` is deprecated; use `InterimAdvice$Complex`.",
      "i" = "Treating `Stock` as `Complex`."
    ))
    names(IA)[names(IA) == "Stock"] <- "Complex"
  }

  ComplexNames <- names(OM@Complexes)
  FleetNames   <- FleetNames(OM)
  Seasons      <- max(1L, as.integer(OM@Seasons %||NA% 1))

  if (!"Complex" %in% names(IA)) {
    if (length(ComplexNames) > 1)
      cli::cli_abort("`InterimAdvice` must have a `Complex` column when the OM has more than one complex.")
    IA$Complex <- ComplexNames
  }
  IA$Complex <- as.character(IA$Complex)
  bad <- setdiff(unique(IA$Complex), ComplexNames)
  if (length(bad))
    cli::cli_abort(c(
      "`InterimAdvice$Complex` contains name(s) not in `names(OM@Complexes)`: {.val {bad}}.",
      "i" = "Valid complex names: {.val {ComplexNames}}."
    ))

  if (!"Fleet" %in% names(IA)) IA$Fleet <- NA_character_
  IA$Fleet <- as.character(IA$Fleet)
  IA$Fleet[!is.na(IA$Fleet) & !nzchar(IA$Fleet)] <- NA_character_
  bad <- setdiff(unique(stats::na.omit(IA$Fleet)), FleetNames)
  if (length(bad))
    cli::cli_abort(c(
      "`InterimAdvice$Fleet` contains name(s) not in `FleetNames(OM)`: {.val {bad}}.",
      "i" = "Valid fleet names: {.val {FleetNames}}, or `NA` for the complex total."
    ))

  for (col in c("CV", "Max")) if (!col %in% names(IA)) IA[[col]] <- NA_real_
  defaults <- c(TACType = "Removals", TACUnit = "Biomass", EffType = "Abs")
  for (col in names(defaults)) {
    if (!col %in% names(IA)) IA[[col]] <- NA_character_
    IA[[col]] <- as.character(IA[[col]])
    IA[[col]][is.na(IA[[col]])] <- defaults[[col]]
  }
  bad <- setdiff(unique(IA$EffType[IA$Type == "Effort"]), c("Abs", "Rel"))
  if (length(bad))
    cli::cli_abort("`InterimAdvice$EffType` must be {.val Abs} or {.val Rel}; found {.val {bad}}.")

  tsIdx <- vapply(IA$Year, \(y) {
    d <- abs(InterimTS - y)
    if (min(d) < 5e-4) which.min(d) else NA_integer_
  }, integer(1))
  if (anyNA(tsIdx))
    cli::cli_abort(c(
      "`InterimAdvice$Year` has value(s) that are not interim timesteps: {.val {unique(IA$Year[is.na(tsIdx)])}}.",
      "i" = "Interim timesteps are the projection timesteps before `MPStartYear = {OM@MPStartYear}`, as given by `Years(OM)`: {.val {InterimTS}}."
    ))
  IA$TS      <- InterimTS[tsIdx]
  IA$CalYear <- floor(IA$TS)
  IA$Season  <- ((tsIdx - 1L) %% Seasons) + 1L

  # per fleet (or complex total) and calendar year: one integer-year row, or one row per season
  FleetKey <- ifelse(is.na(IA$Fleet), "<complex total>", IA$Fleet)
  grp <- paste(IA$Complex, IA$Type, FleetKey, IA$CalYear, sep = "\r")
  IA$Seasonal <- FALSE
  for (g in unique(grp)) {
    ind  <- which(grp == g)
    seas <- IA$Season[ind]
    r    <- IA[ind[1], ]
    if (length(ind) == 1 && seas == 1L) next
    if (Seasons > 1 && length(ind) == Seasons && setequal(seas, seq_len(Seasons))) {
      IA$Seasonal[ind] <- TRUE
      next
    }
    cli::cli_abort(c(
      "`InterimAdvice` {.val {r$Type}} rows for Complex {.val {r$Complex}}, Fleet {.val {FleetKey[ind[1]]}}, Year {.val {r$CalYear}} must be either a single row for the calendar year ({.val {r$CalYear}}) or one row for each of the {Seasons} seasons.",
      "x" = "Found {length(ind)} row(s) for season(s) {.val {sort(seas)}}.",
      "i" = if (Seasons > 1) "Seasonal timesteps for {r$CalYear}: {.val {InterimTS[floor(InterimTS) == r$CalYear]}}."
    ))
  }

  InterimYears <- unique(floor(InterimTS))
  for (cx in ComplexNames) {
    missing <- setdiff(InterimYears, IA$CalYear[IA$Complex == cx])
    if (length(missing))
      cli::cli_abort(c(
        "`InterimAdvice` has no rows for Complex {.val {cx}} in interim year(s) {.val {missing}}.",
        "i" = "Supply TAC and/or Effort for every interim year ({.val {InterimYears}})."
      ))
  }

  cyGrp <- paste(IA$Complex, IA$Type, IA$CalYear, sep = "\r")
  for (g in unique(cyGrp)) {
    ind <- which(cyGrp == g)
    r   <- IA[ind[1], ]
    explicit <- unique(stats::na.omit(IA$Fleet[ind]))
    hasTotal <- anyNA(IA$Fleet[ind])
    rest     <- setdiff(FleetNames, explicit)
    if (!hasTotal && length(rest))
      cli::cli_abort(c(
        "`InterimAdvice` {.val {r$Type}} rows for Complex {.val {r$Complex}}, Year {.val {r$CalYear}} do not cover fleet(s) {.val {rest}}.",
        "i" = "Add rows for the missing fleet(s), or a `Fleet = NA` row giving the total for the fleets not listed."
      ))
    if (hasTotal && !length(rest))
      cli::cli_abort(c(
        "`InterimAdvice` {.val {r$Type}} rows for Complex {.val {r$Complex}}, Year {.val {r$CalYear}} list every fleet and also a `Fleet = NA` total.",
        "i" = "The `Fleet = NA` row is allocated across the fleets not listed individually; remove it."
      ))
  }

  Proj@OM@InterimAdvice <- IA[, setdiff(names(IA), c("TS", "CalYear", "Season", "Seasonal")), drop = FALSE]
  Proj@Misc$InterimAdvice <- IA
  Proj
}

.BuildInterimAdvice <- function(Proj, Year, YearsProj, FleetNames, Areas) {

  OM      <- Proj@OM
  nSim    <- OM@nSim
  Seasons <- max(1L, as.integer(OM@Seasons %||NA% 1))

  YearsHist <- Years(OM, "Historical")
  TSIndex   <- match(Year, c(YearsHist, YearsProj))
  Season    <- ((TSIndex - 1) %% Seasons) + 1
  CalYear   <- floor(Year)

  IAYear <- Proj@Misc$InterimAdvice
  IAYear <- IAYear[IAYear$CalYear == CalYear, , drop = FALSE]
  IA     <- IAYear[!IAYear$Seasonal | IAYear$Season == Season, , drop = FALSE]

  ComplexNames <- names(OM@Complexes)
  last_ts <- .LastHistYearIndices(YearsHist, Seasons)

  AdviceSimList <- MakeNamedList(seq_len(nSim))
  for (sim in seq_len(nSim)) {
    AdviceList <- MakeNamedList(ComplexNames)
    for (i in seq_along(ComplexNames)) {
      cx <- ComplexNames[i]
      AdviceList[[cx]] <- .BuildInterimAdviceOne(
        Proj, sim, CalYear, Season, i, cx, FleetNames, Areas,
        rows     = IA[IA$Complex == cx, , drop = FALSE],
        yearRows = IAYear[IAYear$Complex == cx, , drop = FALSE],
        last_ts  = last_ts
      )
    }
    AdviceSimList[[sim]] <- AdviceList
  }

  list(
    AdviceSimList      = AdviceSimList,
    AggBagLimitSimList = MakeNamedList(seq_len(nSim))
  )
}

.BuildInterimAdviceOne <- function(Proj, sim, CalYear, Season, cxInd, Complex,
                                   FleetNames, Areas, rows, yearRows, last_ts) {

  Advice <- methods::new("advice")

  for (Type in c("TAC", "Effort")) {
    r <- rows[rows$Type == Type, , drop = FALSE]
    if (!nrow(r)) next
    mult <- .SampleInterimMultiplier(Proj@OM, CalYear, Complex, Type,
                                     yearRows[yearRows$Type == Type, , drop = FALSE], sim)
    vals <- .InterimFleetValues(Proj, sim, Season, cxInd, FleetNames, r, mult, last_ts, Type)
    if (Type == "TAC") {
      Advice@TAC     <- vals$value
      Advice@TACType <- vals$TACType
      Advice@TACUnit <- vals$TACUnit
    } else {
      dist_mat <- Proj@Distribution[sim, last_ts[Season], , , drop = FALSE]
      dist_mat <- array(dist_mat, dim = dim(dist_mat)[3:4])  # Fleet x Area
      Advice@Effort <- dist_mat * vals$value
      dimnames(Advice@Effort) <- list(Fleet = FleetNames, Area = Areas)
      Advice@EffType <- "Abs"
    }
  }

  .CheckAdvice(Advice, Proj, FleetNames, Areas, sim, name = Complex)
}

# absolute per-fleet value for this timestep: annual rows split by SeasonalAllocation,
# a Fleet = NA total split by Fleet/EffortAllocation over the fleets not listed
.InterimFleetValues <- function(Proj, sim, Season, cxInd, FleetNames, rows, mult,
                                last_ts, Type) {
  nFleet  <- length(FleetNames)
  SA      <- Proj@OM@SeasonalAllocation[[cxInd]]
  SAs     <- if (is.null(SA)) rep(1, nFleet) else SA[min(sim, dim(SA)[1]), Season, ]
  LastEff <- Proj@Effort[sim, last_ts[Season], ]

  value   <- numeric(nFleet)
  TACType <- rep("Removals", nFleet)
  TACUnit <- rep("Biomass", nFleet)

  explicit <- rows[!is.na(rows$Fleet), , drop = FALSE]
  for (k in seq_len(nrow(explicit))) {
    fl <- match(explicit$Fleet[k], FleetNames)
    v  <- explicit$Mean[k] * mult
    if (Type == "Effort" && explicit$EffType[k] == "Rel") {
      v <- v * LastEff[fl]
    } else if (!explicit$Seasonal[k]) {
      v <- v * SAs[fl]
    }
    value[fl]   <- v
    TACType[fl] <- explicit$TACType[k]
    TACUnit[fl] <- explicit$TACUnit[k]
  }

  total <- rows[is.na(rows$Fleet), , drop = FALSE]
  if (nrow(total)) {
    rest <- which(!FleetNames %in% explicit$Fleet)
    v    <- total$Mean[1] * mult
    if (Type == "Effort" && total$EffType[1] == "Rel") {
      value[rest] <- v * LastEff[rest]
    } else {
      Alloc <- if (Type == "TAC") Proj@OM@FleetAllocation[[cxInd]] else Proj@OM@EffortAllocation[[cxInd]]
      w <- if (is.null(Alloc)) rep(1, nFleet) else Alloc[min(sim, nrow(Alloc)), ]
      w <- w[rest]
      if (total$Seasonal[1]) w <- w * SAs[rest]
      w <- if (sum(w) > 0) w / sum(w) else rep(1 / length(rest), length(rest))
      value[rest] <- v * w
      if (!total$Seasonal[1]) value[rest] <- value[rest] * SAs[rest]
    }
    TACType[rest] <- total$TACType[1]
    TACUnit[rest] <- total$TACUnit[1]
  }

  list(value = value, TACType = TACType, TACUnit = TACUnit)
}

# one mean-1 lognormal multiplier per sim, shared by all rows of a Year x Complex x Type
.SampleInterimMultiplier <- function(OM, CalYear, Complex, Type, rows, sim) {
  pos <- rows$Mean > 0
  if (!any(pos)) return(1)
  CV <- rows$CV[pos][1]
  if (is.na(CV) || CV <= 0) return(1)

  sigma <- sqrt(log(1 + CV^2))
  mu    <- -sigma^2 / 2
  Upper <- min(c(Inf, rows$Max[pos] / rows$Mean[pos]), na.rm = TRUE)
  pUpper <- if (is.finite(Upper)) stats::plnorm(Upper, mu, sigma) else 1

  key <- paste(OM@Seed, CalYear, Complex, Type, sep = "_")
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
