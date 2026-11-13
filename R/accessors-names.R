#' Stock, Fleet, Index, Complex, and MP Names
#'
#' Return the names of the stocks, fleets, indices, stock complexes, and
#' management procedures in an object.
#'
#' @param object An [om-class], [hist-class], [mse-class], [stock-class],
#'   [fleet-class], or [data-class] object, or a list of these objects.
#' @param x For `IndexNames()`, an [om-class], [hist-class], [mse-class], or
#'   [data-class] object, or a list of [data-class] objects. For
#'   `ComplexNames()`, an [om-class], [hist-class], or [mse-class] object. For
#'   `MPNames()`, an [mse-class] or [pm-class] object.
#' @param IncSurvey Logical. Include the names of the `CPUE` and `Survey`
#'   indices? Default `FALSE`.
#' @param Type Character. The index types returned by `IndexNames()`: `"CPUE"`,
#'   `"Survey"`, or both (default).
#'
#' @return
#' - `StockNames()` returns a character vector of stock names.
#' - `FleetNames()` returns a character vector of fleet names, taken from the
#'   first stock. For a [data-class] object (or a list of them) the names are
#'   taken from the catch, effort, and composition data, and
#'   `IncSurvey = TRUE` adds the names of the `CPUE` and `Survey` indices.
#' - `IndexNames()` returns the names of the `CPUE` and/or `Survey` indices.
#'   For a [data-class] object (or a list of them) these are the indices in
#'   the data. For an [om-class] object they are the indices in `Data`, and for
#'   a [hist-class] or [mse-class] object also the indices simulated from
#'   `Obs`.
#' - `ComplexNames()` returns the names of the stock complexes. When
#'   `Complexes` is not specified, these are the complexes created by
#'   [Simulate()] (see [Complexes()]).
#' - `MPNames()` returns the names of the management procedures.
#'
#' @seealso [Dimensions] for the number of stocks, fleets, complexes, and
#'   management procedures; [Name()] for the `Name` slot of an object.
#'
#' @family slot-accessors
#'
#' @examples
#' StockNames(SingleStockOM)
#' FleetNames(SingleStockOM)
#'
#' @name Names
NULL

#' @rdname Names
#' @export
StockNames <- function(object) {
  if (inherits(object, c("stock", "data")))
    return(object@Name)

  if (!is.list(object) && !inherits(object, c("om", "hist", "mse")))
    return(NULL)

  stocks <- .StockList(object)
  nms <- names(stocks)
  if (!is.null(nms))
    return(nms)

  purrr::map_chr(stocks, Name)
}

#' @rdname Names
#' @export
FleetNames <- function(object, IncSurvey = FALSE) {

  if (inherits(object, "data"))
    object <- list(object)

  if (.IsDataList(object)) {
    slots <- c("Landings", "Discards", "Effort", "LandingsAtAge", "DiscardsAtAge",
               "LandingsAtSize", "DiscardsAtSize")
    if (IncSurvey)
      slots <- c(slots, "CPUE", "Survey")
    fleetnames <- purrr::map(object, \(d) purrr::map(slots, \(sl) .DataFleetNames(methods::slot(d, sl))))
    return(as.character(unique(unlist(fleetnames))))
  }

  if (!is.list(object))
    .CheckClass(object, c("om", "hist", "mse", "fleet", "data"), "object")

  fleets <- .FleetList(object)
  fleetnames <- names(fleets)
  if (is.null(fleetnames))
    fleetnames <- purrr::map_chr(fleets, \(fl) if (length(fl@Name)) fl@Name[1] else NA_character_)
  fleetnames <- as.character(fleetnames)

  if (inherits(object, c("hist", "mse")))
    object <- object@OM

  if (IncSurvey && inherits(object, "om") && !is.null(object@Data)) {
    DataList <- if (inherits(object@Data, "data")) list(object@Data) else object@Data
    surveynames <- lapply(DataList, \(d) c(d@Survey@Name, d@CPUE@Name)) |> unlist()
    fleetnames <- unique(c(fleetnames, surveynames))
  }
  fleetnames
}

.DataFleetNames <- function(DataSlot) {
  if (!is.null(DataSlot@Name))
    return(DataSlot@Name)
  Value <- DataSlot@Value
  if (is.null(Value))
    return(NULL)
  dimnames(Value)[["Fleet"]] %||% paste("Fleet", seq_len(NCOL(Value)))
}

#' @rdname Names
#' @export
IndexNames <- function(x, Type = c("CPUE", "Survey")) {
  Type <- match.arg(Type, several.ok = TRUE)

  if (inherits(x, "data"))
    x <- list(x)
  if (.IsDataList(x))
    return(purrr::map(x, \(d) purrr::map(Type, \(type) .DataFleetNames(methods::slot(d, type)))) |>
             unlist() |> unique() |> as.character())

  .CheckClass(x, c("om", "hist", "mse", "data"), "x")
  Simulated <- inherits(x, c("hist", "mse"))
  if (Simulated)
    x <- x@OM

  DataList <- x@Data
  if (inherits(DataList, "data"))
    DataList <- list(DataList)
  Names <- if (length(DataList)) IndexNames(DataList, Type) else character(0)

  if (Simulated) {
    ObsNames <- purrr::map(x@Obs, \(ObsList) {
      HasIndex <- purrr::map_lgl(ObsList, \(obs) any(purrr::map_lgl(Type, \(type) !is.null(methods::slot(obs, type)@Error))))
      names(ObsList)[HasIndex]
    })
    Names <- c(Names, unlist(ObsNames))
  }
  unique(as.character(Names))
}

#' @rdname Names
#' @export
ComplexNames <- function(x) {
  names(.ComplexList(x)) %||% character(0)
}

#' @rdname Names
#' @export
MPNames <- function(x) {
  .CheckClass(x, c('mse', 'pm'), 'x')
  if (inherits(x, 'pm'))
    return(x@MPs)
  names(x@MPs) %||% character(0)
}
