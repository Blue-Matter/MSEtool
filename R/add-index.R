#' Add an Abundance Index to an Operating Model
#'
#' Adds an observed CPUE or survey index to an [om-class] object, together
#' with its observation model: the index values are appended to
#' `OM@Data[[Complex]]@Survey` (or `@CPUE`), and an [indicesobs-class] object
#' with the index's `Selectivity` is placed in `OM@Obs[[Complex]][[Name]]`.
#'
#' `AddAgeIndices()` adds an index observed by age class (e.g., survey
#' numbers-at-age) as one index per age class, named `"<Name> Age <age>"`,
#' each with a selectivity of 1 for that age and 0 for all others. Each
#' age-specific index is conditioned separately, so each has its own
#' `Efficiency`, `Beta`, residual `CV`, and autocorrelation.
#'
#' @param OM An [om-class] object.
#' @param Value For `AddIndex()`, a numeric vector of index values named by
#'   year, or a one-column matrix with years as row names. For
#'   `AddAgeIndices()`, a numeric matrix with years as rows and age classes as
#'   columns. Years must be in `Years(OM, "Historical")`; missing years are
#'   `NA`.
#' @param Name `character(1)`. Name of the index. For `AddIndex()`, defaults
#'   to the column name of a one-column matrix `Value`. A CPUE index named
#'   after a fleet in `OM@Fleet` uses that fleet's selectivity unless
#'   `Selectivity` is given.
#' @param CV `NULL`, a scalar, or values matching `Value`. Coefficient of
#'   variation of each observation. Default `NULL`.
#' @param Units `character(1)`. How the population is converted to the index:
#'   `"Biomass"`, `"Number"`, or `"Recruitment"` (`AddIndex()`, default
#'   `"Biomass"`); `"Number"` or `"Biomass"` (`AddAgeIndices()`, default
#'   `"Number"`). See [IndicesObs()].
#' @param Timing `numeric(1)`. Timing of the observations as a fraction of the
#'   time step (0-1). Default `0`.
#' @param Selectivity Selectivity-at-age of the index: `"Biomass"`,
#'   `"SBiomass"`, a numeric vector named by age, an array, or a list of these
#'   by stock. See [IndicesObs()] for details. Required unless the index is
#'   named after a fleet.
#' @param Type `character(1)`. `"Survey"` (default) for fishery-independent
#'   indices, or `"CPUE"` for fishery-dependent indices.
#' @param Complex Integer. Index of the stock complex (`OM@Data` element) the
#'   index belongs to. Default `1`.
#' @param IndexObs An [indicesobs-class] object with other observation model
#'   settings for the index, e.g. `CV`, `Beta`, or `Areas`. Default
#'   `IndicesObs()`. For `AddIndex()`, give `Selectivity` either here or as
#'   the `Selectivity` argument, not both. For `AddAgeIndices()`, used as the
#'   template for every age, and `Selectivity` must not be set.
#'
#' @return `OM` with the index (or indices) added to `OM@Data` and `OM@Obs`.
#'
#' @seealso [IndicesData()], [IndicesObs()]
#'
#' @examples
#' OM <- SingleStockOM
#' Years <- Years(OM, "Historical")
#'
#' # spawning biomass survey
#' Index <- setNames(runif(length(Years)), Years)
#' OM <- AddIndex(OM, Index, Name = "Acoustic", Selectivity = "SBiomass", CV = 0.2)
#'
#' # survey of ages 3-5
#' OM <- AddIndex(OM, Index, Name = "Trawl", Units = "Number",
#'                Selectivity = c("3" = 1, "4" = 1, "5" = 1))
#'
#' # survey numbers-at-age, one index per age
#' Ages <- 2:5
#' Value <- matrix(runif(length(Years) * length(Ages)), length(Years), length(Ages),
#'                 dimnames = list(Year = Years, Age = Ages))
#' OM <- AddAgeIndices(OM, Value, Name = "Longline Survey", CV = 0.3)
#'
#' OM@Data[[1]]@Survey@Name
#'
#' @export
AddIndex <- function(OM,
                     Value,
                     Name        = NULL,
                     CV          = NULL,
                     Units       = c("Biomass", "Number", "Recruitment"),
                     Timing      = 0,
                     Selectivity = NULL,
                     Type        = c("Survey", "CPUE"),
                     Complex     = 1,
                     IndexObs    = IndicesObs()) {

  .CheckClass(OM, "om", "OM")
  .CheckClass(IndexObs, "indicesobs", "IndexObs")
  Units <- match.arg(Units)
  Type  <- match.arg(Type)

  if (is.matrix(Value)) {
    if (ncol(Value) != 1)
      cli::cli_abort("{.arg Value} must be a vector or a one-column matrix; use {.fn AddAgeIndices} for indices by age.")
    if (is.null(Name)) Name <- colnames(Value)
    Value <- stats::setNames(as.numeric(Value), rownames(Value))
  }
  if (!is.numeric(Value) || is.null(names(Value)))
    cli::cli_abort("{.arg Value} must be a numeric vector named by year, or a one-column matrix with years as row names.")
  if (is.null(Name) || length(Name) != 1 || !nzchar(Name))
    cli::cli_abort("{.arg Name} must be a single, non-empty character string.")

  HistYears <- Years(OM, "Historical")
  ObsYears  <- suppressWarnings(as.numeric(names(Value)))
  BadYears  <- names(Value)[is.na(ObsYears) | !ObsYears %in% HistYears]
  if (length(BadYears))
    cli::cli_abort("{.arg Value} has {cli::qty(length(BadYears))}value{?s} for year{?s} {.val {BadYears}} outside {.code Years(OM, 'Historical')}.")
  if (anyDuplicated(ObsYears))
    cli::cli_abort("{.arg Value} has duplicated {.val {unique(ObsYears[duplicated(ObsYears)])}} year{?s}.")

  if (!is.null(CV) && !length(CV) %in% c(1, length(Value)))
    cli::cli_abort("{.arg CV} must be {.code NULL}, a scalar, or one value per element of {.arg Value}.")

  Complexes <- .AddIndexComplexes(OM)
  if (Complex > length(Complexes))
    cli::cli_abort("{.arg Complex} is {Complex} but {.arg OM} has {length(Complexes)} stock complex{?es}.")
  stocks <- Complexes[[Complex]]
  OM@Data <- .AddIndexDataList(OM@Data, Complexes)

  if (!is.null(Selectivity) && !is.null(IndexObs@Selectivity))
    cli::cli_abort("Give {.field Selectivity} in {.arg Selectivity} or in {.arg IndexObs}, not both.")
  Selectivity <- Selectivity %||% IndexObs@Selectivity
  FleetNms <- FleetNames(OM)
  if (is.null(Selectivity) && !Name %in% FleetNms)
    cli::cli_abort(.MsgIndexNotFleet(Name, FleetNms))
  .CheckIndexSelectivity(Selectivity, OM, stocks, Name)

  # Data
  if (length(OM@Data) < Complex) {
    if (Complex > length(OM@Data) + 1)
      cli::cli_abort("{.code OM@Data} has {length(OM@Data)} element{?s}; add {.code OM@Data} for complex {Complex - 1} first.")
    OM@Data <- c(OM@Data, list(Data()))
    names(OM@Data)[Complex] <- names(Complexes)[Complex]
  }
  data    <- OM@Data[[Complex]]
  Indices <- slot(data, Type)
  if (Name %in% Indices@Name)
    cli::cli_abort("{.code OM@Data[[{Complex}]]@{Type}} already has an index named {.val {Name}}.")

  # full historical rows: conditioning reads index rows from the first historical year
  .FullRows <- function(x) {
    out <- matrix(NA_real_, length(HistYears), 1, dimnames = list(Year = HistYears, Fleet = Name))
    out[match(ObsYears, HistYears), 1] <- x
    out
  }
  ValueNew <- .FullRows(Value)
  CVNew    <- if (is.null(CV)) NULL else .FullRows(rep_len(CV, length(Value)))

  nOld     <- length(Indices@Name)
  OldValue <- Indices@Value
  if (!is.null(OldValue) && is.null(rownames(OldValue)))
    rownames(OldValue) <- c(HistYears, Years(OM, "Projection"))[seq_len(nrow(OldValue))]

  if (!is.null(Indices@CV) || !is.null(CVNew))
    Indices@CV <- .AppendIndexColumns(Indices@CV %||% .NALike(OldValue),
                                      CVNew %||% .NALike(ValueNew))
  Indices@Value  <- .AppendIndexColumns(OldValue, ValueNew)
  Indices@Name   <- c(Indices@Name, Name)
  Indices@Units  <- c(.PadIndexEntries(Indices@Units, nOld, NA_character_), Units)
  Indices@Timing <- c(.PadIndexEntries(Indices@Timing, nOld, NA_real_), Timing)
  if (!is.null(Indices@Ref))
    Indices@Ref <- c(.PadIndexEntries(Indices@Ref, nOld, NA_real_), NA_real_)
  slot(data, Type) <- Indices
  OM@Data[[Complex]] <- data

  .SetIndexObs(OM, Name, Type, Complex, IndexObs, Selectivity)
}

# set the indicesobs of index `Name`, creating its Obs entry if needed
.SetIndexObs <- function(OM, Name, Type, Complex, IndexObs, Selectivity) {
  Complexes <- .AddIndexComplexes(OM)
  if (EmptyObject(OM@Obs))
    OM@Obs <- MakeNamedList(names(Complexes), MakeNamedList(FleetNames(OM), new("obs")))
  if (inherits(OM@Obs, "obs"))
    OM@Obs <- MakeNamedList(names(Complexes), MakeNamedList(FleetNames(OM), OM@Obs))

  obs <- OM@Obs[[Complex]][[Name]] %||% Obs(Name = Name)
  # keep settings already on the index unless new ones are given
  if (identical(IndexObs, IndicesObs()) && !EmptyObject(slot(obs, Type)))
    IndexObs <- slot(obs, Type)
  IndexObs@Selectivity <- Selectivity
  slot(obs, Type) <- IndexObs
  OM@Obs[[Complex]][[Name]] <- obs
  OM
}

#' Get or Set the Selectivity of an Index
#'
#' Access or replace the selectivity-at-age of a CPUE or survey index in an
#' [om-class] object, stored in `OM@Obs[[Complex]][[Name]]@Survey` (or
#' `@CPUE`). The replacement form creates the index's [obs-class] entry when
#' it does not exist, e.g. for a survey added directly to `OM@Data`.
#'
#' @param x An [om-class] object.
#' @param Name `character(1)`. Name of the index, as in `OM@Data`.
#' @param Type `character(1)`. `"Survey"` (default) or `"CPUE"`.
#' @param Complex Integer. Index of the stock complex. Default `1`.
#' @param value Selectivity of the index: `"Biomass"`, `"SBiomass"`, a
#'   numeric vector named by age, an array, a list of these by stock, or
#'   `NULL` for the selectivity of the fleet with the same name. See
#'   [IndicesObs()].
#'
#' @return `IndexSelectivity()` returns the index's selectivity
#'   specification, or `NULL` when none is set. `IndexSelectivity<-` returns
#'   `x` with the selectivity replaced.
#'
#' @seealso [AddIndex()], [IndicesObs()]
#'
#' @examples
#' OM <- SingleStockOM
#' Years <- Years(OM, "Historical")
#' Value <- matrix(runif(length(Years)), ncol = 1, dimnames = list(Year = Years, Index = "Acoustic"))
#' Data(OM) <- Data(Survey = IndicesData(Name = "Acoustic", Value = Value))
#' IndexSelectivity(OM, "Acoustic") <- "SBiomass"
#' IndexSelectivity(OM, "Acoustic")
#'
#' @export
IndexSelectivity <- function(x, Name, Type = c("Survey", "CPUE"), Complex = 1) {
  .CheckClass(x, "om", "x")
  Type <- match.arg(Type)
  if (!is.list(x@Obs) || length(x@Obs) < Complex) return(NULL)
  obs <- x@Obs[[Complex]][[Name]]
  if (is.null(obs)) return(NULL)
  slot(obs, Type)@Selectivity
}

#' @rdname IndexSelectivity
#' @export
`IndexSelectivity<-` <- function(x, Name, Type = c("Survey", "CPUE"), Complex = 1, value) {
  .CheckClass(x, "om", "x")
  Type <- match.arg(Type)
  Complexes <- .AddIndexComplexes(x)
  if (Complex > length(Complexes))
    cli::cli_abort("{.arg Complex} is {Complex} but {.arg x} has {length(Complexes)} stock complex{?es}.")
  if (is.null(value) && !Name %in% FleetNames(x))
    cli::cli_abort(.MsgIndexNotFleet(Name, FleetNames(x)))
  .CheckIndexSelectivity(value, x, Complexes[[Complex]], Name)
  .SetIndexObs(x, Name, Type, Complex, IndicesObs(), value)
}

#' @rdname AddIndex
#' @export
AddAgeIndices <- function(OM,
                          Value,
                          Name     = "Survey",
                          CV       = NULL,
                          Units    = c("Number", "Biomass"),
                          Timing   = 0,
                          Type     = c("Survey", "CPUE"),
                          Complex  = 1,
                          IndexObs = IndicesObs()) {

  .CheckClass(IndexObs, "indicesobs", "IndexObs")
  Units <- match.arg(Units)
  Type  <- match.arg(Type)
  if (!is.null(IndexObs@Selectivity))
    cli::cli_abort("{.code IndexObs@Selectivity} must be {.code NULL}; {.fn AddAgeIndices} sets the selectivity of each age.")
  if (!is.numeric(Value) || length(dim(Value)) != 2)
    cli::cli_abort("{.arg Value} must be a numeric matrix with years as rows and age classes as columns.")
  if (is.null(rownames(Value)) || is.null(colnames(Value)))
    cli::cli_abort("{.arg Value} needs row names (years) and column names (age classes).")
  Ages <- colnames(Value)
  if (anyDuplicated(Ages))
    cli::cli_abort("{.arg Value} has duplicated age class{?es} {.val {unique(Ages[duplicated(Ages)])}}.")
  if (!is.null(CV) && length(CV) != 1 && !identical(dim(CV), dim(Value)))
    cli::cli_abort("{.arg CV} must be {.code NULL}, a scalar, or a matrix with the same dimensions as {.arg Value}.")

  Complexes <- .AddIndexComplexes(OM)
  if (Complex > length(Complexes))
    cli::cli_abort("{.arg Complex} is {Complex} but {.arg OM} has {length(Complexes)} stock complex{?es}.")
  for (st in Complexes[[Complex]]) {
    BadAges <- setdiff(Ages, as.character(OM@Stock[[st]]@Ages@Classes))
    if (length(BadAges))
      cli::cli_abort("{.arg Value} has {cli::qty(length(BadAges))}age class{?es} {.val {BadAges}} not in the age classes of stock {.val {StockNames(OM)[st]}}.")
  }

  Names <- paste(Name, "Age", Ages)
  OM@Data  <- .AddIndexDataList(OM@Data, Complexes)
  OldNames <- if (length(OM@Data) >= Complex) slot(OM@Data[[Complex]], Type)@Name
  ObsNames <- if (is.list(OM@Obs) && length(OM@Obs) >= Complex) names(OM@Obs[[Complex]])
  Taken <- intersect(Names, c(OldNames, ObsNames, FleetNames(OM)))
  if (length(Taken))
    cli::cli_abort("{cli::qty(length(Taken))}Index name{?s} {.val {Taken}} already used in {.arg OM}; choose a different {.arg Name}.")

  for (a in seq_along(Ages)) {
    OM <- AddIndex(OM,
                   Value       = stats::setNames(as.numeric(Value[, a]), rownames(Value)),
                   Name        = Names[a],
                   CV          = if (is.null(CV) || length(CV) == 1) CV else as.numeric(CV[, a]),
                   Units       = Units,
                   Timing      = Timing,
                   Selectivity = stats::setNames(1, Ages[a]),
                   Type        = Type,
                   Complex     = Complex,
                   IndexObs    = IndexObs)
  }
  OM
}

# OM@Data as a list by complex
.AddIndexDataList <- function(Data, Complexes) {
  if (isS4(Data)) return(MakeNamedList(names(Complexes)[1], Data))
  Data
}

.AddIndexComplexes <- function(OM) {
  Complexes <- Complexes(OM)
  if (!length(Complexes))
    Complexes <- as.list(seq_along(OM@Stock)) |> stats::setNames(StockNames(OM))
  Complexes
}

# per-index entries of the first n indices, padded with `fill`
.PadIndexEntries <- function(x, n, fill) {
  if (!n) return(x[0])
  out <- rep(fill, n)
  if (length(x)) out[seq_len(min(n, length(x)))] <- x[seq_len(min(n, length(x)))]
  out
}
