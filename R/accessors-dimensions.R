#' Object Dimensions
#'
#' Return the number of simulations, stocks, fleets, areas, ages, length
#' classes, complexes, management procedures, or advice replicates in an
#' object, and its age and length classes.
#'
#' @param x An [om-class], [hist-class], or [mse-class] object, or a component
#'   object or list as described under Value.
#' @param value Integer. The number of simulations (`nSim<-`), areas
#'   (`nArea<-`), or replicates (`nReps<-`) to assign.
#' @param st Integer or character. Stock index or name used when `x` contains
#'   multiple stocks. Default `1` for `nArea()` and `NULL` for `nAge()`,
#'   `nLength()`, `AgeClasses()`, and `LengthClasses()` (returns all stocks as
#'   a list when `NULL` and `x` has more than one stock).
#'
#' @return
#' - `nSim()` returns the number of simulations in an [om-class],
#'   [stock-class], or [fleet-class] object (or the `OM` of a [hist-class] or
#'   [mse-class] object), or the length of the `Sim` dimension of an array.
#'   A [data-class] object, or a list of them (one per stock), holds one
#'   simulation, so `nSim()` returns `1`; for a list by simulation such as
#'   `Hist@Data` it returns the length of the list.
#'   `nSim<-` returns `x` with the `nSim` slot replaced.
#' - `nStock()` and `nFleet()` return the number of stocks and fleets in an
#'   [om-class], [hist-class], or [mse-class] object, a [stock-class] or
#'   [fleet-class] object, or a list of these objects, and `0` when none are
#'   specified. For a [data-class] object, `nStock()` returns `1` and
#'   `nFleet()` returns the number of fleets given by [FleetNames()].
#' - `nArea()` returns the number of areas implied by the populated slots of
#'   the [spatial-class] object (from a `spatial`, `stock`, `om`, `hist`, or
#'   `mse` object), or `1` when no spatial structure is specified. It gives an
#'   error if the populated slots imply different numbers of areas. For a
#'   [data-class] object it returns the `nArea` slot, and for a list of them
#'   the `nArea` slot of stock `st`. `nArea<-` replaces the `nArea` slot of a
#'   [data-class] object, or of every object in a list of them; the number of
#'   areas of an operating model is set by its [Spatial()] objects.
#' - `AgeClasses()` and `LengthClasses()` return the age classes and the lower
#'   bounds of the length bins, and `nAge()` and `nLength()` return their
#'   number. They accept an [ages-class] or [length-class] object
#'   respectively, a [stock-class] or [data-class] object, or an [om-class],
#'   [hist-class], or [mse-class] object or list of stock or data objects, for
#'   which the result is a list with one element per stock when there is more
#'   than one stock and `st` is `NULL`. For a [data-class] object the classes
#'   are taken from the age or size composition data, or else from
#'   `LifeHistory`. When the size compositions of different fleets use
#'   different bins, `LengthClasses()` returns a list and `nLength()` an
#'   integer vector, with one element per fleet.
#' - `nComplex()` returns the number of stock complexes in an [om-class],
#'   [hist-class], or [mse-class] object (see [ComplexNames()]).
#' - `nMP()` returns the number of management procedures in an [mse-class] or
#'   [pm-class] object.
#' - `nReps()` returns the number of stochastic replicates used to generate
#'   management advice in an [om-class], [hist-class], or [mse-class] object.
#'   `nReps<-` returns `x` with the `nReps` slot replaced; it can be replaced
#'   in [om-class] and [hist-class] objects.
#'
#' @seealso [Names] for the names of the stocks, fleets, indices, complexes,
#'   and management procedures; [Years] for the number of historical and
#'   projection years and seasons; [Classes()] for the `Classes` slot of a
#'   single model component.
#'
#' @family slot-accessors
#'
#' @examples
#' nSim(SingleStockOM)
#' nStock(SingleStockOM)
#' nFleet(SingleStockOM)
#' nAge(SingleStockOM)
#' AgeClasses(SingleStockOM)
#'
#' @name Dimensions
NULL

#' @rdname Dimensions
#' @export
nSim <- function(x) {
  if (inherits(x, 'data') || .IsDataList(x))
    return(1)
  if (.IsSimDataList(x))
    return(length(x))

  if (isS4(x)) {
    slots <- slotNames(x)
    if ('nSim' %in% slots)
      return(x@nSim)
    return(x@OM@nSim)
  }
  
  if (is.list(x))
    return(purrr::map(x, nSim) |> unlist())
  
  dnames <- dimnames(x)
  if (!is.null(dnames))
    return(length(dnames[['Sim']]))
}

#' @rdname Dimensions
#' @export
`nSim<-` <- function(x, value) {
  .AssignSlot(x, value, 'nSim')
}

#' @rdname Dimensions
#' @export
nArea <- function(x, st = 1) {
  if (inherits(x, 'hist') || inherits(x, 'mse'))
    x <- x@OM
  
  if (.IsDataList(x))
    x <- x[[.ValidStock(st, x)]]

  if (inherits(x, 'data'))
    return(x@nArea)

  if (inherits(x, 'om')) {
    x <- x@Stock
    if (is.null(x) || (is.list(x) && !length(x)))
      return(1)
    if (is.list(x))
      x <- x[[.ValidStock(st, x)]]
  }

  if (inherits(x, 'stock'))
    x <- x@Spatial

  .CheckClass(x, c('spatial', 'stock', 'om', 'hist', 'mse', 'data'), 'x')
  .SpatialNArea(x)
}

#' @rdname Dimensions
#' @export
`nArea<-` <- function(x, value) {
  if (.IsDataList(x))
    return(purrr::map(x, \(d) .AssignSlot(d, value, 'nArea')))
  .CheckClass(x, 'data', 'x')
  .AssignSlot(x, value, 'nArea')
}

#' @rdname Dimensions
#' @export
nAge <- function(x, st = NULL) {
  if (inherits(x, 'ages'))
    return(length(x@Classes))
  .MapStocks(x, st, \(s) length(.AgeClasses(s)))
}

#' @rdname Dimensions
#' @export
nLength <- function(x, st = NULL) {
  if (inherits(x, 'length'))
    return(length(x@Classes))
  .MapStocks(x, st, \(s) {
    Classes <- .LengthClasses(s)
    if (is.list(Classes)) lengths(Classes) else length(Classes)
  })
}

#' @rdname Dimensions
#' @export
AgeClasses <- function(x, st = NULL) {
  if (inherits(x, 'ages'))
    return(x@Classes)
  .MapStocks(x, st, .AgeClasses)
}

#' @rdname Dimensions
#' @export
LengthClasses <- function(x, st = NULL) {
  if (inherits(x, 'length'))
    return(x@Classes)
  .MapStocks(x, st, .LengthClasses)
}

# apply `fn` to a stock or data object, or to each (or stock `st`) of an om/hist/mse or list
.MapStocks <- function(x, st, fn) {
  if (inherits(x, c('stock', 'data')))
    return(fn(x))
  if (!is.list(x) || isS4(x))
    .CheckClass(x, c('om', 'hist', 'mse', 'stock', 'data'), 'x')

  Stocks <- .StockList(x)
  if (!is.null(st))
    return(fn(Stocks[[.ValidStock(st, Stocks)]]))
  out <- purrr::map(Stocks, fn)
  if (length(out) == 1)
    return(out[[1]])
  out
}

.AgeClasses <- function(x) {
  if (inherits(x, 'stock'))
    return(x@Ages@Classes)

  for (sl in c('LandingsAtAge', 'DiscardsAtAge')) {
    Comp <- methods::slot(x, sl)
    Classes <- Comp@Classes %||% dimnames(Comp@Value)[['Age']]
    if (length(Classes))
      return(as.numeric(Classes))
  }
  .DataLifeHistoryClasses(x, 'Ages')
}

# a list (one per fleet) when the size compositions use different bins
.LengthClasses <- function(x) {
  if (inherits(x, 'stock'))
    return(x@Length@Classes)

  for (sl in c('LandingsAtSize', 'DiscardsAtSize')) {
    Classes <- methods::slot(x, sl)@Classes
    if (!length(Classes))
      next
    if (!is.list(Classes))
      return(Classes)
    Classes <- unique(Classes)
    if (length(Classes) == 1)
      return(Classes[[1]])
    return(methods::slot(x, sl)@Classes)
  }
  .DataLifeHistoryClasses(x, 'Length')
}

.DataLifeHistoryClasses <- function(x, SlotName) {
  LH <- x@LifeHistory
  if (!is.list(LH))
    LH <- list(LH)
  Classes <- purrr::map(LH, \(lh) methods::slot(lh, SlotName)@Classes) |> purrr::compact() |> unique()
  if (length(Classes) == 1)
    return(Classes[[1]])
  NULL
}

#' @rdname Dimensions
#' @export
nComplex <- function(x) {
  length(.ComplexList(x))
}

# OM@Complexes, or the complexes `Simulate()` will create when not yet populated
.ComplexList <- function(x) {
  .CheckClass(x, c('om', 'hist', 'mse'), 'x')
  if (inherits(x, c('hist', 'mse')))
    x <- x@OM
  if (!length(x@Complexes) && nStock(x) > 0)
    x <- .PopulateComplexes(x)
  x@Complexes
}

#' @rdname Dimensions
#' @export
nMP <- function(x) {
  length(MPNames(x))
}

#' @rdname Dimensions
#' @export
nStock <- function(x) {
  if (inherits(x, c('stock', 'data')))
    return(1)
  if (!is.list(x))
    .CheckClass(x, c('om', 'hist', 'mse', 'stock', 'data'), 'x')
  length(.StockList(x))
}

#' @rdname Dimensions
#' @export
nFleet <- function(x) {
  if (inherits(x, 'data') || .IsDataList(x))
    return(length(FleetNames(x)))
  if (!is.list(x))
    .CheckClass(x, c('om', 'hist', 'mse', 'fleet', 'data'), 'x')

  if (inherits(x, c('hist', 'mse')) && length(x@LandingsAtAge) &&
      length(dim(x@LandingsAtAge[[1]])) >= 4)
    return(dim(x@LandingsAtAge[[1]])[[4]])

  length(.FleetList(x))
}

#' @rdname Dimensions
#' @export
nReps <- function(x) .IsHist(x, "nReps")

#' @rdname Dimensions
#' @export
`nReps<-` <- function(x, value) .AssignOMSlot(x, value, "nReps", "proj")
