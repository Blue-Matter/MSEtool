#' Data Helper Functions
#'
#' A collection of helper functions for extracting key values from a `Data`
#' object, including TAC advice, and year indices for historical and projection
#' time steps.
#'
#' @param Data An object of class [data].
#' 
#' @seealso [DataTrim()]
#'
#' @name DataHelpers
NULL

#' @describeIn DataHelpers Get the last TAC (Total Allowable Catch) from the
#'   advice slot. If no TAC has been set, returns the catch of type `TACType`
#'   (summed over fleets, ignoring `NA`) in the most recent year; for
#'   seasonal data (`Seasons > 1`), the most recent complete calendar year
#'   (see [AnnualData()]).
#'
#' @param TACType Character. Catch returned when no TAC has been set:
#'   `'Removals'` (default; landings plus discards) or `'Landings'`.
#'
#' @return
#' - `LastTAC()`: A numeric scalar giving the last TAC, or the most recent
#'   annual catch of type `TACType` if no TAC is available.
#'
#' @export
LastTAC <- function(Data, TACType = c('Removals', 'Landings')) {
  .CheckClass(Data, 'data', 'Data')
  TACType <- match.arg(TACType)
  LastTAC <- utils::tail(Data@Advice@TAC, 1) |> as.numeric()
  if (length(LastTAC) < 1) {
    Annual <- AnnualData(Data)
    LastRow <- function(x) if (is.null(x@Value)) NULL else utils::tail(x@Value, 1)
    LastTAC <- LastRow(Annual@Landings)
    if (TACType == 'Removals')
      LastTAC <- c(LastTAC, LastRow(Annual@Discards))
  }
  sum(LastTAC, na.rm = TRUE)
}

#' @describeIn DataHelpers Get the index of the last historical year within
#'   `Data@@Years`. This is the position of the final year that does not exceed
#'   `Data@@YearLH`, and is typically used to subset or index time series
#'   vectors aligned to `Data@@Years`.
#'
#' @return
#' - `LastHistYearInd()`: An integer giving the index of the last historical
#'   year in `Data@@Years`.
#'
#' @export
LastHistYearInd <- function(Data) {
  .CheckClass(Data, 'data', 'Data')
  which.max(Data@Years[Data@Years < Data@YearLH + 1])
}

#' @describeIn DataHelpers Get the index of the current projection time step
#'   relative to `Data@@YearLH`. Returns `1` in the first projection year,
#'   incrementing by one each subsequent timestep
#'
#' @return
#' - `ProjectionYear()`: A positive integer giving the current projection year
#'   index.
#'
#' @export
ProjectionYear <- function(Data) {
  length(Data@Years[Data@Years >= Data@YearLH + 1])
}

#

#' @describeIn DataHelpers Get the season of a given (or, by default, the
#'   current/most recent) year. `x` may be a `data` object (`Seasons` is then
#'   taken from `x@@Seasons` and `Year` defaults to the most recent element of
#'   `x@@Years`) or a plain numeric vector of years, in which case `Seasons`
#'   must be supplied.
#'
#' @param x A [data] object, or a numeric vector of years.
#' @param Seasons Integer. Seasons per year. Required when `x` is not a
#'   `data` object; ignored otherwise.
#' @param Year Numeric. The year to query. Defaults to the most recent year
#'   in the resolved years vector.
#'
#' @return
#' - `SeasonOfYear()`: A list with elements `Seasons` (the number of seasons)
#'   and `SeasonInd` (the season index, `1..Seasons`, of `Year`).
#'
#' @export
SeasonOfYear <- function(x, Seasons = NULL, Year = NULL) {
  if (inherits(x, "data")) {
    Years   <- x@Years
    Seasons <- x@Seasons
  } else {
    Years <- x
  }
  Seasons <- max(1L, as.integer(Seasons %||NA% 1))
  TSIndex <- if (is.null(Year)) length(Years) else match(Year, Years)
  list(Seasons = Seasons,
       SeasonInd = ((TSIndex - 1L) %% Seasons) + 1L)
}


#' Aggregate Seasonal Data to Annual Time Steps
#'
#' Converts a seasonal [data-class] object (`Seasons > 1`, one row per time
#' step) to one row per calendar year. Data with `Seasons = 1` is returned
#' unchanged.
#'
#' - `Landings`, `Discards`, `Effort`, and the composition slots
#'   (`LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, `DiscardsAtSize`)
#'   are summed over the seasons of each calendar year. A cell with no
#'   non-`NA` value in a year stays `NA`. The `CV` of summed catch and effort
#'   assumes independent errors among seasons.
#' - `CPUE` and `Survey` values and `CV`s are averaged (simple mean, ignoring
#'   `NA` and negative values) over the seasons in `IndexSeasons`. Index `Ref`
#'   values are not modified, and so are assumed to be in the units of the
#'   aggregated annual index.
#'
#' The `Advice`, `LifeHistory`, `Exploitation`, and `Reference` slots are not
#' modified.
#'
#' @param Data A [data-class] object.
#' @param IndexSeasons Seasons (integers in `1:Data@@Seasons`) averaged to
#'   form each annual index value. `NULL` (default) uses every season. Either
#'   an integer vector applied to every index, or a list with elements
#'   `Survey` and/or `CPUE`, each either an integer vector applied to every
#'   index in that slot or a list with one element (integer vector, or `NULL`
#'   for every season) per index. Ignored when `Seasons = 1`.
#' @param CompleteYears Logical. If `TRUE` (default), calendar years with
#'   fewer than `Seasons` time steps in `Data@@Years` (e.g. a partially
#'   observed final year) are dropped, so a partial year's catch is not read
#'   as a low annual catch.
#'
#' @return A [data-class] object with `Seasons = 1` and whole-number `Years`.
#'   Every aggregated slot has one row per element of the returned `Years`.
#'
#' @seealso [DataHelpers], [IndexMPs]
#' @export
AnnualData <- function(Data, IndexSeasons = NULL, CompleteYears = TRUE) {
  .CheckClass(Data, 'data', 'Data')
  Seasons <- max(1L, as.integer(Data@Seasons %||NA% 1))
  if (Seasons == 1L)
    return(Data)

  if (is.list(IndexSeasons) && !all(names(IndexSeasons) %in% c('Survey', 'CPUE')))
    cli::cli_abort("A list {.arg IndexSeasons} must have elements named {.val Survey} and/or {.val CPUE}.")

  CalYear     <- .CalendarYear(Data@Years)
  nPerYear    <- table(CalYear)
  AnnualYears <- as.numeric(names(nPerYear))
  if (CompleteYears)
    AnnualYears <- AnnualYears[as.integer(nPerYear) >= Seasons]

  for (sl in c('Landings', 'Discards', 'Effort')) {
    obj <- slot(Data, sl)
    if (is.null(obj@Value)) next
    RowYears <- .AnnualRowYears(obj@Value, Data@Years)
    if (!is.null(obj@CV)) {
      obj@CV <- if (identical(dim(obj@CV), dim(obj@Value))) {
        Var <- .AnnualSum((obj@CV * obj@Value)^2, RowYears, AnnualYears)
        sqrt(Var) / .AnnualSum(obj@Value, RowYears, AnnualYears)
      } else {
        .AnnualIndexMean(obj@CV, .AnnualRowYears(obj@CV, Data@Years), AnnualYears, Seasons)
      }
    }
    obj@Value <- .AnnualSum(obj@Value, RowYears, AnnualYears)
    slot(Data, sl) <- obj
  }

  for (sl in c('LandingsAtAge', 'DiscardsAtAge', 'LandingsAtSize', 'DiscardsAtSize')) {
    obj <- slot(Data, sl)
    if (is.null(obj@Value)) next
    obj@Value <- .AnnualSum(obj@Value, .AnnualRowYears(obj@Value, Data@Years), AnnualYears)
    slot(Data, sl) <- obj
  }

  for (sl in c('Survey', 'CPUE')) {
    obj <- slot(Data, sl)
    if (is.null(obj@Value)) next
    SeasonsList <- .ResolveIndexSeasons(IndexSeasons, sl, ncol(obj@Value), Seasons)
    Value <- obj@Value
    Value[!is.na(Value) & Value < 0] <- NA
    obj@Value <- .AnnualIndexMean(Value, .AnnualRowYears(Value, Data@Years), AnnualYears,
                                  Seasons, SeasonsList)
    if (!is.null(obj@CV))
      obj@CV <- .AnnualIndexMean(obj@CV, .AnnualRowYears(obj@CV, Data@Years), AnnualYears,
                                 Seasons, SeasonsList)
    slot(Data, sl) <- obj
  }

  Data@Years   <- AnnualYears
  Data@Seasons <- 1
  Data
}

.CalendarYear <- function(Years) floor(Years + 1e-6)

.SeasonOfTimestep <- function(Years, Seasons) {
  (round((Years - .CalendarYear(Years)) * Seasons) %% Seasons) + 1L
}

.AnnualRowYears <- function(Value, Years) {
  rn <- dimnames(Value)[[1]]
  if (!is.null(rn))
    return(as.numeric(rn))
  if (dim(Value)[1] == length(Years))
    return(Years)
  cli::cli_abort("Cannot match the rows of a data array to {.code Data@Years}: it has no year dimnames and {dim(Value)[1]} rows for {length(Years)} years.")
}

.AnnualArray <- function(Out, Value, AnnualYears) {
  d  <- dim(Value)
  dn <- dimnames(Value)
  if (is.null(dn)) dn <- vector('list', length(d))
  dn[[1]] <- as.character(AnnualYears)
  array(Out, dim = c(length(AnnualYears), d[-1]), dimnames = dn)
}

.AnnualSum <- function(Value, RowYears, AnnualYears) {
  Mat <- matrix(Value, nrow = dim(Value)[1])
  CalYear <- .CalendarYear(RowYears)
  Out <- matrix(NA_real_, length(AnnualYears), ncol(Mat))
  for (k in seq_along(AnnualYears)) {
    Blk <- Mat[CalYear == AnnualYears[k], , drop = FALSE]
    if (!nrow(Blk)) next
    Sum <- colSums(Blk, na.rm = TRUE)
    Sum[colSums(!is.na(Blk)) == 0] <- NA
    Out[k, ] <- Sum
  }
  .AnnualArray(Out, Value, AnnualYears)
}

.AnnualIndexMean <- function(Value, RowYears, AnnualYears, Seasons, SeasonsList = NULL) {
  Mat <- matrix(Value, nrow = dim(Value)[1])
  CalYear <- .CalendarYear(RowYears)
  Season  <- .SeasonOfTimestep(RowYears, Seasons)
  Out <- matrix(NA_real_, length(AnnualYears), ncol(Mat))
  for (j in seq_len(ncol(Mat))) {
    Use <- if (is.null(SeasonsList[[j]])) TRUE else Season %in% SeasonsList[[j]]
    for (k in seq_along(AnnualYears)) {
      x <- Mat[CalYear == AnnualYears[k] & Use, j]
      if (any(!is.na(x))) Out[k, j] <- mean(x, na.rm = TRUE)
    }
  }
  .AnnualArray(Out, Value, AnnualYears)
}

.ResolveIndexSeasons <- function(IndexSeasons, Source, nIndex, Seasons) {
  x <- if (is.list(IndexSeasons)) IndexSeasons[[Source]] else IndexSeasons
  if (!is.list(x)) x <- rep(list(x), nIndex)
  if (length(x) != nIndex)
    cli::cli_abort("{.arg IndexSeasons${Source}} must have one element per index ({.val {nIndex}}).")
  Bad <- setdiff(unlist(x), seq_len(Seasons))
  if (length(Bad))
    cli::cli_abort("{.arg IndexSeasons} must be integers in {.val 1} to {.val {Seasons}}; found {.val {Bad}}.")
  x
}
