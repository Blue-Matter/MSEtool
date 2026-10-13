#' Add an Age-Disaggregated Index to an Operating Model
#'
#' Adds an index observed by age class (e.g., survey numbers-at-age) to an
#' [om-class] object as one index per age class. Each age becomes a column of
#' `OM@Data[[Complex]]@Survey` (or `@CPUE`) named `"<Name> Age <age>"`, with a
#' matching [obs-class] object in `OM@Obs[[Complex]]` whose `Selectivity` is 1
#' for that age and 0 for all others.
#'
#' Each age-specific index is conditioned separately, so each has its own
#' `Efficiency`, `Beta`, residual `CV`, and autocorrelation.
#'
#' @param OM An [om-class] object.
#' @param Value Numeric matrix of index values with years as rows and age
#'   classes as columns. Row names are years in `Years(OM, "Historical")`
#'   and column names are age classes in `OM@Stock[[st]]@Ages@Classes`.
#' @param Name `character(1)`. Base name of the index. Default `"Survey"`.
#' @param CV `NULL`, a scalar, or a matrix with the same dimensions as
#'   `Value`. Coefficient of variation of each observation. Default `NULL`.
#' @param Units `character(1)`. `"Number"` (default) or `"Biomass"`.
#' @param Timing `numeric(1)`. Timing of the observations as a fraction of the
#'   time step (0-1). Default `0`.
#' @param Type `character(1)`. `"Survey"` (default) or `"CPUE"`.
#' @param Complex Integer. Index of the stock complex (`OM@Data` element) the
#'   index belongs to. Default `1`.
#' @param IndexObs An [indicesobs-class] object used as the template for the
#'   observation model of every age, e.g. to set `CV`, `Beta`, or `Areas`.
#'   `Selectivity` must not be set. Default `IndicesObs()`.
#'
#' @return `OM` with the age-specific indices added to `OM@Data` and `OM@Obs`.
#'
#' @seealso [IndicesData()], [IndicesObs()]
#'
#' @examples
#' OM <- SingleStockOM
#' Years <- Years(OM, "Historical")
#' Ages <- 2:5
#' Value <- matrix(runif(length(Years) * length(Ages)), length(Years), length(Ages),
#'                 dimnames = list(Year = Years, Age = Ages))
#' OM <- AddAgeIndices(OM, Value, Name = "Acoustic", CV = 0.3)
#' OM@Data[[1]]@Survey@Name
#' OM@Obs[[1]][["Acoustic Age 3"]]@Survey@Selectivity
#'
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

  .CheckClass(OM, "om", "OM")
  Units <- match.arg(Units)
  Type  <- match.arg(Type)
  .CheckClass(IndexObs, "indicesobs", "IndexObs")
  if (!is.null(IndexObs@Selectivity))
    cli::cli_abort("{.code IndexObs@Selectivity} must be {.code NULL}; {.fn AddAgeIndices} sets the selectivity of each age.")

  if (!is.numeric(Value) || length(dim(Value)) != 2)
    cli::cli_abort("{.arg Value} must be a numeric matrix with years as rows and age classes as columns.")
  if (is.null(rownames(Value)) || is.null(colnames(Value)))
    cli::cli_abort("{.arg Value} needs row names (years) and column names (age classes).")

  Ages      <- colnames(Value)
  HistYears <- Years(OM, "Historical")
  ObsYears  <- as.numeric(rownames(Value))
  BadYears  <- ObsYears[!ObsYears %in% HistYears]
  if (length(BadYears))
    cli::cli_abort("{.arg Value} has row{?s} for year{?s} {.val {BadYears}} outside {.code Years(OM, 'Historical')}.")
  if (anyDuplicated(Ages))
    cli::cli_abort("{.arg Value} has duplicated age class{?es} {.val {unique(Ages[duplicated(Ages)])}}.")

  Complexes <- Complexes(OM)
  if (!length(Complexes))
    Complexes <- as.list(seq_along(OM@Stock)) |> stats::setNames(StockNames(OM))
  if (Complex > length(Complexes))
    cli::cli_abort("{.arg Complex} is {Complex} but {.arg OM} has {length(Complexes)} stock complex{?es}.")

  for (st in Complexes[[Complex]]) {
    BadAges <- setdiff(Ages, as.character(OM@Stock[[st]]@Ages@Classes))
    if (length(BadAges))
      cli::cli_abort("{.arg Value} has age class{?es} {.val {BadAges}} not in the age classes of stock {.val {StockNames(OM)[st]}}.")
  }

  if (is.null(CV)) {
    CVNew <- NULL
  } else if (length(CV) == 1) {
    CVNew <- Value
    CVNew[] <- CV
  } else if (identical(dim(CV), dim(Value))) {
    CVNew <- CV
  } else {
    cli::cli_abort("{.arg CV} must be {.code NULL}, a scalar, or a matrix with the same dimensions as {.arg Value}.")
  }

  NewNames <- paste(Name, "Age", Ages)

  # full historical rows: conditioning reads index rows from the first historical year
  .FullRows <- function(x) {
    out <- matrix(NA_real_, length(HistYears), length(Ages),
                  dimnames = list(Year = HistYears, Fleet = NewNames))
    out[match(ObsYears, HistYears), ] <- x
    out
  }
  ValueNew <- .FullRows(Value)
  if (!is.null(CVNew)) CVNew <- .FullRows(CVNew)

  if (length(OM@Data) < Complex) {
    if (Complex > length(OM@Data) + 1)
      cli::cli_abort("{.code OM@Data} has {length(OM@Data)} element{?s}; add {.code OM@Data} for complex {Complex - 1} first.")
    OM@Data <- c(OM@Data, list(Data()))
    names(OM@Data)[Complex] <- names(Complexes)[Complex]
  }
  data    <- OM@Data[[Complex]]
  Indices <- slot(data, Type)
  nOld    <- length(Indices@Name)

  Taken <- intersect(NewNames, c(Indices@Name, names(OM@Obs[[Complex]]), FleetNames(OM)))
  if (length(Taken))
    cli::cli_abort("Index name{?s} {.val {Taken}} already used in {.arg OM}; choose a different {.arg Name}.")

  OldValue <- Indices@Value
  if (!is.null(OldValue) && is.null(rownames(OldValue)))
    rownames(OldValue) <- c(HistYears, Years(OM, "Projection"))[seq_len(nrow(OldValue))]

  if (!is.null(Indices@CV) || !is.null(CVNew))
    Indices@CV <- .AppendIndexColumns(Indices@CV %||% .NALike(OldValue),
                                      CVNew %||% .NALike(ValueNew))
  Indices@Value  <- .AppendIndexColumns(OldValue, ValueNew)
  Indices@Name   <- c(Indices@Name, NewNames)
  Indices@Units  <- c(.PadIndexEntries(Indices@Units, nOld, NA_character_), rep(Units, length(Ages)))
  Indices@Timing <- c(.PadIndexEntries(Indices@Timing, nOld, NA_real_), rep(Timing, length(Ages)))
  if (!is.null(Indices@Ref))
    Indices@Ref <- c(.PadIndexEntries(Indices@Ref, nOld, NA_real_), rep(NA_real_, length(Ages)))
  slot(data, Type) <- Indices
  OM@Data[[Complex]] <- data

  if (EmptyObject(OM@Obs))
    OM@Obs <- MakeNamedList(names(Complexes), MakeNamedList(FleetNames(OM), new("obs")))
  if (inherits(OM@Obs, "obs"))
    OM@Obs <- MakeNamedList(names(Complexes), MakeNamedList(FleetNames(OM), OM@Obs))

  for (a in seq_along(Ages)) {
    obs <- Obs(Name = NewNames[a])
    IndexObs@Selectivity <- stats::setNames(1, Ages[a])
    slot(obs, Type) <- IndexObs
    OM@Obs[[Complex]][[NewNames[a]]] <- obs
  }
  OM
}

# per-index entries of the first n indices, padded with `fill`
.PadIndexEntries <- function(x, n, fill) {
  if (!n) return(x[0])
  out <- rep(fill, n)
  if (length(x)) out[seq_len(min(n, length(x)))] <- x[seq_len(min(n, length(x)))]
  out
}
