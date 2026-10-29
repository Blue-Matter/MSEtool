#' Common Slot Accessors
#'
#' Access and replace the `Name` and `Misc` slots, which are shared by most S4
#' objects in the `openMSE` package ecosystem.
#'
#' Slot accessors for specific object types are documented by topic:
#' - [Dimensions] for the number of simulations, stocks, fleets, areas, ages,
#'   and complexes, and stock and fleet names.
#' - [Model-accessors] for the slots of stock and fleet model components
#'   (e.g. [Pars()], [MeanAtAge()]).
#' - [Obs-accessors] for observation-error settings (e.g. [CV()], [Bias()]).
#' - [Data-accessors] for the slots of [obs-class] and [data-class] objects and
#'   their components (e.g. [CPUE()], [Value()]).
#' - [PM-accessors] for performance metric objects (e.g. [Prob()]).
#' - [OM-accessors] for operating model settings and metadata.
#' - [ImpSlot()] and [Imp()] for implementation objects.
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot.
#'
#' @return
#' - `Name()` and `Misc()` return the value stored in the named slot of `x`.
#'   `Name()` on a [hist-class] or [mse-class] object returns the names of the
#'   embedded `OM`, and on a list returns a list of names.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @family slot-accessors
#'
#' @examples
#' Name(SingleStockOM)
#'
#' @name Access
NULL

#' Object Dimensions and Names
#'
#' Return the number of simulations, stocks, fleets, areas, ages, length
#' classes, or complexes in an object, its age and length classes, and the names
#' of its stocks and fleets.
#'
#' @param x An [om-class], [hist-class], or [mse-class] object, or a component
#'   object or list as described under Value.
#' @param value Integer. The number of simulations to assign.
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
#'   `nFleet()` returns the number of fleets given by `FleetNames()`.
#' - `nArea()` returns the number of areas implied by the populated slots of
#'   the [spatial-class] object (from a `spatial`, `stock`, `om`, `hist`, or
#'   `mse` object), or `1` when no spatial structure is specified. It gives an
#'   error if the populated slots imply different numbers of areas. For a
#'   [data-class] object it returns the `nArea` slot, and for a list of them
#'   the `nArea` slot of stock `st`.
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
#'   [hist-class], or [mse-class] object.
#' - `StockNames()` returns a character vector of stock names.
#' - `FleetNames()` returns a character vector of fleet names, taken from the
#'   first stock. For a [data-class] object (or a list of them) the names are
#'   taken from the catch, effort, and composition data, and
#'   `IncSurvey = TRUE` adds the names of the `CPUE` and `Survey` indices.
#'
#' @seealso [OM-accessors] for the number of historical and projection years
#'   and seasons; [Years()] for the year values.
#'
#' @family slot-accessors
#'
#' @examples
#' nSim(SingleStockOM)
#' nStock(SingleStockOM)
#' nFleet(SingleStockOM)
#' nAge(SingleStockOM)
#' AgeClasses(SingleStockOM)
#' StockNames(SingleStockOM)
#' FleetNames(SingleStockOM)
#'
#' @name Dimensions
NULL

#' Stock and Fleet Model Component Accessors
#'
#' Access and replace the slots of the stock and fleet model components:
#' [ages-class], [length-class], [weight-class], [maturity-class],
#' [fecundity-class], [naturalmortality-class], [srr-class],
#' [selectivity-class], [retention-class], and [discardmortality-class].
#' Functions work on any S4 object that contains the corresponding slot.
#'
#' | Function | Slot of |
#' |---|---|
#' | `Pars()`, `Model()` | all model components except `ages` and `discardmortality` |
#' | `Units()` | `ages`, `length`, `weight`, `fecundity`, `naturalmortality`, `srr`; also observation and data components |
#' | `Classes()` | all model components except `srr`; also [compdata-class]; for a [stock-class] object returns `Ages@Classes` |
#' | `MeanAtAge()` | all model components except `ages` and `srr` |
#' | `MeanAtLength()` | all model components except `ages`, `length`, and `srr` |
#' | `MeanAtWeight()` | `maturity`, `selectivity`, `retention` |
#' | `CVatAge()`, `Dist()` | `length`, `weight` |
#' | `Random()` | `length`, `weight`, `naturalmortality` |
#' | `Timing()` | `length`, `weight`, `fecundity`; also [indicesdata-class] |
#' | `TruncSD()` | `length`, `weight`, `srr`; also [indicesobs-class] |
#' | `SD()` | `srr`; also [impslot-class] |
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot.
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @family slot-accessors
#'
#' @examples
#' len <- Length(Pars = list(Linf = 80, K = 0.2, t0 = -0.5))
#' Pars(len)
#' Model(len)
#' Units(len) <- "cm"
#'
#' @name Model-accessors
NULL

#' Observation Model Accessors
#'
#' Access and replace the observation-error settings of the observation model
#' components: [catchobs-class], [effortobs-class], [indicesobs-class], and
#' [compobs-class]. Functions work on any S4 object that contains the
#' corresponding slot.
#'
#' | Function | Slot of |
#' |---|---|
#' | `Bias()` | `catchobs`, `effortobs` |
#' | `CV()` | `catchobs`, `effortobs`, `indicesobs`; also the data components |
#' | `DataCV()` | `catchobs`, `effortobs`, `indicesobs` |
#' | `Error()` | `catchobs`, `effortobs`, `indicesobs`; also [impslot-class] |
#' | `AC()` | `indicesobs`; also [srr-class] and [stocktargeting-class] |
#' | `Beta()`, `Areas()`, `Stats()`, `Type()` | `indicesobs` |
#' | `MaxGap()` | `indicesobs`, `compobs` |
#' | `ESS()`, `SampleSize()`, `Shift()` | `compobs` |
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot.
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @seealso [Obs()], [CatchObs()], [EffortObs()], [IndicesObs()],
#'   [CompObs()]
#'
#' @family slot-accessors
#'
#' @examples
#' obs <- CatchObs()
#' CV(obs) <- 0.2
#' CV(obs)
#'
#' @name Obs-accessors
NULL

#' Observation and Data Accessors
#'
#' Access and replace the slots of [obs-class] and [data-class] objects and
#' their data components ([catchdata-class], [effortdata-class],
#' [indicesdata-class], [compdata-class]). Functions work on any S4 object
#' that contains the corresponding slot.
#'
#' | Function | Slot of |
#' |---|---|
#' | `CPUE()`, `Survey()`, `LifeHistory()`, `Exploitation()` | `obs`, `data` |
#' | `LandingsAtAge()`, `LandingsAtSize()`, `DiscardsAtAge()`, `DiscardsAtSize()` | `obs`, `data`, [hist-class], [mse-class], [timeseries-class] |
#' | `Reference()` | `data`, `hist`, `mse`, [depletion-class] |
#' | `Value()` | the data components |
#' | `Ref()` | `catchdata`, `indicesdata`; also the observation components and [pm-class] |
#' | `RefCV()` | `catchdata`, `indicesdata` |
#' | `YearLH()` | `data` |
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot.
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @seealso [Data()], [Obs()], [CatchData()], [EffortData()], [CompData()]
#'
#' @family slot-accessors
#'
#' @name Data-accessors
NULL

#' Performance Metric Accessors
#'
#' Access and replace the slots of [pm-class] objects.
#'
#' @param x A [pm-class] object, or another S4 object with the corresponding
#'   slot. `Mean()` also applies to [impslot-class] and
#'   [stocktargeting-class] objects, and `MPs()` to [mse-class] objects.
#' @param value The value to assign to the slot.
#'
#' @return
#' - `Stat()`, `Prob()`, and `Mean()` on a [pm-class] object convert the
#'   named `Sim x Stock x MP` (or `Stock x MP`) array to a tidy data frame
#'   via [Array2DF()]. On other objects they return the slot value.
#' - `Caption()` and `MPs()` return the value stored in the named slot of `x`.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @seealso [PM()], [PerformanceMetric()]
#'
#' @family slot-accessors
#'
#' @name PM-accessors
NULL


#' @rdname Obs-accessors
#' @export
AC <- function(x) {
  .AccessSlot(x, 'AC')
}

#' @rdname Obs-accessors
#' @export
`AC<-` <- function(x, value) {
  .AssignSlot(x, value, 'AC')
}

#' @rdname Obs-accessors
#' @export
Areas <- function(x) {
  .AccessSlot(x, 'Areas')
}

#' @rdname Obs-accessors
#' @export
`Areas<-` <- function(x, value) {
  .AssignSlot(x, value, 'Areas')
}

#' @rdname Obs-accessors
#' @export
Beta <- function(x) {
  .AccessSlot(x, 'Beta')
}

#' @rdname Obs-accessors
#' @export
`Beta<-` <- function(x, value) {
  .AssignSlot(x, value, 'Beta')
}

#' @rdname Obs-accessors
#' @export
Bias <- function(x) {
  .AccessSlot(x, 'Bias')
}

#' @rdname Obs-accessors
#' @export
`Bias<-` <- function(x, value) {
  .AssignSlot(x, value, 'Bias')
}

#' @rdname PM-accessors
#' @export
Caption <- function(x) {
  .AccessSlot(x, 'Caption')
}

#' @rdname PM-accessors
#' @export
`Caption<-` <- function(x, value) {
  .AssignSlot(x, value, 'Caption')
}


#' @rdname Model-accessors
#' @export
Classes <- function(x) {
  if (inherits(x, 'stock'))
    return(x@Ages@Classes)

  if (inherits(x, 'StockList'))
    return(purrr::map(x, Classes))

  if (isS4(x) && methods::.hasSlot(x, 'Classes'))
    return(x@Classes)

  cli::cli_abort(c("Argument {.arg x} must be a {.cls stock} or an object with a {.code Classes} slot",
                   "x" = "You've supplied an object of class {.cls {class(x)}}"), call = NULL)
}

#' @rdname Model-accessors
#' @export
`Classes<-` <- function(x, value) {
  .AssignSlot(x, value, 'Classes')
}

#' @rdname ImpSlot
#' @export
Compliance <- function(x) {
  .AccessSlot(x, 'Compliance')
}

#' @rdname ImpSlot
#' @export
`Compliance<-` <- function(x, value) {
  .AssignSlot(x, value, 'Compliance')
}

#' @rdname Data-accessors
#' @export
CPUE <- function(x) {
  .AccessSlot(x, 'CPUE')
}

#' @rdname Data-accessors
#' @export
`CPUE<-` <- function(x, value) {
  .AssignSlot(x, value, 'CPUE')
}

#' @rdname Obs-accessors
#' @export
CV <- function(x) {
  .AccessSlot(x, 'CV')
}

#' @rdname Obs-accessors
#' @export
`CV<-` <- function(x, value) {
  .AssignSlot(x, value, 'CV')
}

#' @rdname Model-accessors
#' @export
CVatAge <- function(x) {
  .AccessSlot(x, 'CVatAge')
}

#' @rdname Model-accessors
#' @export
`CVatAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'CVatAge')
}

#' @rdname Obs-accessors
#' @export
DataCV <- function(x) {
  .AccessSlot(x, 'DataCV')
}

#' @rdname Obs-accessors
#' @export
`DataCV<-` <- function(x, value) {
  .AssignSlot(x, value, 'DataCV')
}

#' @rdname Model-accessors
#' @export
Dist <- function(x) {
  .AccessSlot(x, 'Dist')
}

#' @rdname Model-accessors
#' @export
`Dist<-` <- function(x, value) {
  .AssignSlot(x, value, 'Dist')
}

#' @rdname Data-accessors
#' @export
DiscardsAtAge <- function(x) {
  .AccessSlot(x, 'DiscardsAtAge')
}

#' @rdname Data-accessors
#' @export
`DiscardsAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'DiscardsAtAge')
}

#' @rdname Data-accessors
#' @export
DiscardsAtSize <- function(x) {
  .AccessSlot(x, 'DiscardsAtSize')
}

#' @rdname Data-accessors
#' @export
`DiscardsAtSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'DiscardsAtSize')
}

#' @rdname Obs-accessors
#' @export
ESS <- function(x) {
  .AccessSlot(x, 'ESS')
}

#' @rdname Obs-accessors
#' @export
`ESS<-` <- function(x, value) {
  .AssignSlot(x, value, 'ESS')
}

#' @rdname Obs-accessors
#' @export
Error <- function(x) {
  .AccessSlot(x, 'Error')
}

#' @rdname Obs-accessors
#' @export
`Error<-` <- function(x, value) {
  .AssignSlot(x, value, 'Error')
}

#' @rdname Data-accessors
#' @export
Exploitation <- function(x) {
  .AccessSlot(x, 'Exploitation')
}

#' @rdname Data-accessors
#' @export
`Exploitation<-` <- function(x, value) {
  .AssignSlot(x, value, 'Exploitation')
}


#' @rdname Data-accessors
#' @export
LandingsAtAge <- function(x) {
  .AccessSlot(x, 'LandingsAtAge')
}

#' @rdname Data-accessors
#' @export
`LandingsAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'LandingsAtAge')
}

#' @rdname Data-accessors
#' @export
LandingsAtSize <- function(x) {
  .AccessSlot(x, 'LandingsAtSize')
}

#' @rdname Data-accessors
#' @export
`LandingsAtSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'LandingsAtSize')
}

#' @rdname Data-accessors
#' @export
LifeHistory <- function(x) {
  .AccessSlot(x, 'LifeHistory')
}

#' @rdname Data-accessors
#' @export
`LifeHistory<-` <- function(x, value) {
  .AssignSlot(x, value, 'LifeHistory')
}


#' @rdname Obs-accessors
#' @export
MaxGap <- function(x) {
  .AccessSlot(x, 'MaxGap')
}

#' @rdname Obs-accessors
#' @export
`MaxGap<-` <- function(x, value) {
  if (!is.null(value)) value <- as.numeric(value)
  .AssignSlot(x, value, 'MaxGap')
}

#' @rdname PM-accessors
#' @export
Mean <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Mean')))
  .AccessSlot(x, 'Mean')
}

#' @rdname PM-accessors
#' @export
`Mean<-` <- function(x, value) {
  .AssignSlot(x, value, 'Mean')
}


#' @rdname Model-accessors
#' @export
MeanAtAge <- function(x) {
  .AccessSlot(x, 'MeanAtAge')
}

#' @rdname Model-accessors
#' @export
`MeanAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtAge')
}

#' @rdname Model-accessors
#' @export
MeanAtLength <- function(x) {
  .AccessSlot(x, 'MeanAtLength')
}

#' @rdname Model-accessors
#' @export
`MeanAtLength<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtLength')
}

#' @rdname Model-accessors
#' @export
MeanAtWeight <- function(x) {
  .AccessSlot(x, 'MeanAtWeight')
}

#' @rdname Model-accessors
#' @export
`MeanAtWeight<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtWeight')
}

#' @rdname Access
#' @export
Misc <- function(x) {
  .AccessSlot(x, 'Misc')
}

#' @rdname Access
#' @export
`Misc<-` <- function(x, value) {
  .AssignSlot(x, value, 'Misc')
}

#' @rdname PM-accessors
#' @export
MPs <- function(x) {
  if (!isS4(x) && is.list(x))
    return(purrr::map(x, MPs))
  .AccessSlot(x, 'MPs')
}

#' @rdname PM-accessors
#' @export
`MPs<-` <- function(x, value) {
  .AssignSlot(x, value, 'MPs')
}

#' @rdname Model-accessors
#' @export
Model <- function(x) {
  .AccessSlot(x, 'Model')
}


#' @rdname Model-accessors
#' @export
`Model<-` <- function(x, value) {
  .AssignSlot(x, value, 'Model')
}

#' @rdname Access
#' @export
Name <- function(x) {
  if (inherits(x, 'mse') || inherits(x, 'hist'))
    x <- x@OM

  if (is.list(x))
    return(purrr::map(x, Name))

  .AccessSlot(x, 'Name')
}

#' @rdname Access
#' @export
`Name<-` <- function(x, value) {
  if (inherits(x, 'mse') || inherits(x, 'hist')) {
    x@OM <- .AssignSlotRecursive(x@OM, value, 'Name')
    return(x)
  }
  .AssignSlotRecursive(x, value, 'Name')
}

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
    x <- x[[.CheckStock(st, x)]]

  if (inherits(x, 'data'))
    return(x@nArea)

  if (inherits(x, 'om')) {
    x <- x@Stock
    if (is.null(x) || (is.list(x) && !length(x)))
      return(1)
    if (is.list(x))
      x <- x[[.CheckStock(st, x)]]
  }

  if (inherits(x, 'stock'))
    x <- x@Spatial

  .CheckClass(x, c('spatial', 'stock', 'om', 'hist', 'mse', 'data'), 'x')
  .SpatialNArea(x)
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
    return(fn(Stocks[[.CheckStock(st, Stocks)]]))
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
  .CheckClass(x, c('om', 'hist', 'mse'), 'x')
  
  if (inherits(x, 'om'))
    return(length(x@Complexes))
  
  if (inherits(x, 'hist'))
    return(length(x@OM@Complexes))
  
  if (inherits(x, 'mse'))
    return(length(x@OM@Complexes))
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

# named list of stock objects from an om/hist/mse, a single stock, or a list
.StockList <- function(x) {
  if (inherits(x, c('hist', 'mse')))
    x <- x@OM
  if (inherits(x, 'om'))
    x <- x@Stock
  if (is.null(x))
    return(list())
  if (!is.list(x))
    x <- list(x)
  x
}

# flat list of fleet objects (first stock) from an om/hist/mse, a fleet, or a flat/nested list
.FleetList <- function(x) {
  if (inherits(x, c('hist', 'mse')))
    x <- x@OM
  if (inherits(x, 'om'))
    x <- x@Fleet
  if (is.null(x))
    return(list())
  if (!is.list(x))
    return(list(x))
  if (length(x) && is.list(x[[1]]))
    return(x[[1]])
  x
}

.IsDataList <- function(x) {
  is.list(x) && !isS4(x) && length(x) > 0 && all(purrr::map_lgl(x, \(d) inherits(d, 'data')))
}

# list by simulation of data objects or lists of data objects, e.g. `Hist@Data`
.IsSimDataList <- function(x) {
  is.list(x) && !isS4(x) && length(x) > 0 &&
    all(purrr::map_lgl(x, \(d) inherits(d, 'data') || .IsDataList(d)))
}

# `fn` applied to a data object or each of a list; a single value when all are identical
.DataValue <- function(x, fn) {
  if (inherits(x, 'data'))
    return(fn(x))
  out <- purrr::map(x, fn)
  if (length(unique(out)) == 1)
    return(out[[1]])
  out
}

.CheckStock <- function(st, Stocks) {
  if (is.numeric(st) && st > length(Stocks) || is.character(st) && !st %in% names(Stocks))
    cli::cli_abort("Stock {.val {st}} not found", call = NULL)
  st
}

#' @rdname Model-accessors
#' @export
Pars <- function(x) {
  .AccessSlot(x, 'Pars')
}

#' @rdname Model-accessors
#' @export
`Pars<-` <- function(x, value) {
  .AssignSlot(x, value, 'Pars')
}

#' @rdname PM-accessors
#' @export
Prob <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Prob')))
  .AccessSlot(x, 'Prob')
}

#' @rdname PM-accessors
#' @export
`Prob<-` <- function(x, value) {
  .AssignSlot(x, value, 'Prob')
}

#' @rdname Model-accessors
#' @export
Random <- function(x) {
  .AccessSlot(x, 'Random')
}

#' @rdname Data-accessors
#' @export
Ref <- function(x) {
  .AccessSlot(x, 'Ref')
}

#' @rdname Data-accessors
#' @export
`Ref<-` <- function(x, value) {
  .AssignSlot(x, value, 'Ref')
}

#' @rdname Data-accessors
#' @export
RefCV <- function(x) {
  .AccessSlot(x, 'RefCV')
}

#' @rdname Data-accessors
#' @export
`RefCV<-` <- function(x, value) {
  .AssignSlot(x, value, 'RefCV')
}

#' @rdname Data-accessors
#' @export
Reference <- function(x) {
  .AccessSlot(x, 'Reference')
}

#' @rdname Data-accessors
#' @export
`Reference<-` <- function(x, value) {
  .AssignSlot(x, value, 'Reference')
}

#' @rdname Obs-accessors
#' @export
SampleSize <- function(x) {
  .AccessSlot(x, 'SampleSize')
}

#' @rdname Obs-accessors
#' @export
`SampleSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'SampleSize')
}

#' @rdname Imp
#' @export
Size <- function(x) {
  .AccessSlot(x, 'Size')
}

#' @rdname Imp
#' @export
`Size<-` <- function(x, value) {
  .AssignSlot(x, value, 'Size')
}

#' @rdname Model-accessors
#' @export
SD <- function(x) {
  .AccessSlot(x, 'SD')
}

#' @rdname Model-accessors
#' @export
`SD<-` <- function(x, value) {
  .AssignSlot(x, value, 'SD')
}

#' @rdname PM-accessors
#' @export
Stat <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Stat')))
  .AccessSlot(x, 'Stat')
}

#' @rdname PM-accessors
#' @export
`Stat<-` <- function(x, value) {
  .AssignSlot(x, value, 'Stat')
}

#' @rdname Obs-accessors
#' @export
Shift <- function(x) {
  .AccessSlot(x, 'Shift')
}

#' @rdname Obs-accessors
#' @export
`Shift<-` <- function(x, value) {
  .AssignSlot(x, value, 'Shift')
}

#' @rdname Obs-accessors
#' @export
Stats <- function(x) {
  .AccessSlot(x, 'Stats')
}

#' @rdname Obs-accessors
#' @export
`Stats<-` <- function(x, value) {
  .AssignSlot(x, value, 'Stats')
}

#' @rdname Data-accessors
#' @export
Survey <- function(x) {
  .AccessSlot(x, 'Survey')
}

#' @rdname Data-accessors
#' @export
`Survey<-` <- function(x, value) {
  .AssignSlot(x, value, 'Survey')
}

#' @rdname Model-accessors
#' @export
Timing <- function(x) {
  .AccessSlot(x, 'Timing')
}

#' @rdname Model-accessors
#' @export
TruncSD <- function(x) {
  .AccessSlot(x, 'TruncSD')
}

#' @rdname Model-accessors
#' @export
`TruncSD<-` <- function(x, value) {
  .AssignSlot(x, value, 'TruncSD')
}

#' @rdname Obs-accessors
#' @export
Type <- function(x) {
  .AccessSlot(x, 'Type')
}

#' @rdname Obs-accessors
#' @export
`Type<-` <- function(x, value) {
  .AssignSlot(x, value, 'Type')
}

#' @rdname Model-accessors
#' @export
Units <- function(x) {
  .AccessSlot(x, 'Units')
}

#' @rdname Model-accessors
#' @export
`Units<-` <- function(x, value) {
  .AssignSlot(x, value, 'Units')
}

#' @rdname Data-accessors
#' @export
Value <- function(x) {
  .AccessSlot(x, 'Value')
}

#' @rdname Data-accessors
#' @export
`Value<-` <- function(x, value) {
  .AssignSlot(x, value, 'Value')
}

#' @rdname Data-accessors
#' @export
YearLH <- function(x) {
  .AccessSlot(x, 'YearLH')
}

#' @rdname Data-accessors
#' @export
`YearLH<-` <- function(x, value) {
  .AssignSlot(x, value, 'YearLH')
}


.AccessSlot <- function(x, slotname) {
  if (is.null(x)) return(NULL)
  .CheckClass(slotname, 'character', 'slotname')
  if (!isS4(x))
    cli::cli_abort("{.arg x} is not an S4 object.")
  if (!slotname %in% slotNames(x))
    cli::cli_abort("Slot {.val {slotname}} not found in class {.cls {class(x)}}.")
  slot(x, slotname)
}

.AccessSlotRecursive <- function(x, SlotName) {
  if (is.list(x))
    return(purrr::map(x, \(xi) .AccessSlotRecursive(xi, SlotName)))
  .AccessSlot(x, SlotName)
}


.AssignSlot <- function(x, value, slot) {
  if (!isS4(x) || !slot %in% methods::slotNames(x))
    cli::cli_abort("Slot {.code {slot}} not found in class {.cls {class(x)}}.", call = NULL)
  slot(x, slot) <- value
  methods::validObject(x)
  x
}

.AssignSlotRecursive <- function(x, value, SlotName) {
  if (is.list(x)) {
    if (!is.list(value) || length(value) != length(x))
      cli::cli_abort(c(
        "x" = "`value` must be a list of length {length(x)}",
        "i" = "`value` has length {length(value)}"
      ))
    return(purrr::map2(x, value, \(xi, vi) .AssignSlotRecursive(xi, vi, SlotName)))
  }
  .AssignSlot(x, value, SlotName)
}

# Is `x` a non-empty list whose every element inherits from S4 class `what`?
.IsTypeList <- function(x, what) {
  is.list(x) && length(x) > 0 &&
    all(vapply(x, inherits, logical(1), what = what))
}

.IsStockList <- function(x) {
  .IsTypeList(x, 'stock')
}

.IsStockOrList <- function(x) {
  inherits(x, 'stock') ||
    inherits(x, 'om')    ||
    .IsStockList(x)
}


.IsFleetList <- function(x) {
  .IsTypeList(x, 'fleet')
}

.IsStockFleetList <- function(x) {
  is.list(x) && length(x) > 0 &&
    all(vapply(x, .IsFleetList, logical(1)))
}

.IsFleetOrList <- function(x) {
  inherits(x, 'fleet') ||
    inherits(x, 'om')    ||
    .IsFleetList(x)        ||
    .IsStockFleetList(x)
}


.ExtractStockSlot <- function(x, SlotName) {
  if (inherits(x, 'stock'))
    return(slot(x, SlotName))
  
  if (inherits(x, 'om')) {
    out <- purrr::map(x@Stock, slot, SlotName)
    if (!length(out))
      return(NULL)
    
    class(out) <- 'StockList'
    return(out)
  }
  
  if (.IsStockList(x)) {
    out <- purrr::map(x, slot, SlotName)
    if (!length(out))
      return(NULL)
    class(out) <- 'StockList'
    return(out)
  }
  
  NULL
}

.ExtractFleetSlot <- function(x, SlotName) {
  if (inherits(x, 'fleet'))
    return(slot(x, SlotName))
  
  if (inherits(x, 'om')) {
    out <- purrr::map(x@Fleet, \(FleetList) {
      fl <- purrr::map(FleetList, slot, SlotName)
      class(fl) <- 'FleetList'
      fl
    })
    if (!length(out))
      return(NULL)
    class(out) <- 'StockFleetList'
    return(out)
  }
  
  if (.IsStockFleetList(x)) {
    out <- purrr::map(x, \(FleetList) {
      fl <- purrr::map(FleetList, slot, SlotName)
      class(fl) <- 'FleetList'
      fl
    })
    if (!length(out))
      return(NULL)
    class(out) <- 'StockFleetList'
    return(out)
  }
  
  if (.IsFleetList(x)) {
    out <- purrr::map(x, slot, SlotName)
    if (!length(out))
      return(NULL)
    class(out) <- 'FleetList'
    return(out)
  }
  
  NULL
}

.AssignFleetSlot <- function(x, value, SlotName) {
  if (inherits(x, 'om')) {
    x@Fleet <- .AssignSlotRecursive(x@Fleet, value, SlotName)
    return(x)
  }
  .AssignSlotRecursive(x, value, SlotName)
}
