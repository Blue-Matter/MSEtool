#' Access and Assign Slots from Compatible S4 Objects
#'
#' Generic accessor and replacement functions to retrieve and assign named
#' slots from compatible S4 objects across the `openMSE` package ecosystem.
#' Functions work on any S4 object that contains the corresponding slot.
#'
#' @param x An S4 object with the corresponding slot.
#' @param value The value to assign to the slot
#' @param st Integer. Stock index used when `x` contains multiple stocks.
#'   Default `1` for `nArea()` and `NULL` for `nAge()` (returns all stocks as
#'   a list when `NULL`).
#'
#' @return
#' - Accessor functions return the value stored in the named slot of `x`.
#'   For `Stat()`, `Prob()`, and `Mean()` on a [pm-class] object, the named
#'   `Sim x Stock x MP` (or `Stock x MP`) array is converted to a tidy
#'   data frame via [Array2DF()] instead of being returned as a raw array.
#' - Replacement functions return `x` with the named slot updated to `value`.
#'
#' @examples
#' MyLength <- Length()
#' MeanAtAge(MyLength)
#'
#' @name Access
NULL


#' @rdname Access
#' @export
AC <- function(x) {
  .AccessSlot(x, 'AC')
}

#' @rdname Access
#' @export
`AC<-` <- function(x, value) {
  .AssignSlot(x, value, 'AC')
}

#' @rdname Access
#' @export
Areas <- function(x) {
  .AccessSlot(x, 'Areas')
}

#' @rdname Access
#' @export
`Areas<-` <- function(x, value) {
  .AssignSlot(x, value, 'Areas')
}

#' @rdname Access
#' @export
Beta <- function(x) {
  .AccessSlot(x, 'Beta')
}

#' @rdname Access
#' @export
`Beta<-` <- function(x, value) {
  .AssignSlot(x, value, 'Beta')
}

#' @rdname Access
#' @export
Bias <- function(x) {
  .AccessSlot(x, 'Bias')
}

#' @rdname Access
#' @export
`Bias<-` <- function(x, value) {
  .AssignSlot(x, value, 'Bias')
}

#' @rdname Access
#' @export
Caption <- function(x) {
  .AccessSlot(x, 'Caption')
}

#' @rdname Access
#' @export
`Caption<-` <- function(x, value) {
  .AssignSlot(x, value, 'Caption')
}


#' @rdname Access
#' @export
Classes <- function(x) {
  validclasses <- c('ages', 'length', 'weight', 'maturity', 'fecundity',
                    'selectivity', 'discardmortality', 'retention')
  .CheckClass(x, c('stock', 'StockList', validclasses), 'x')
  
  if (inherits(x, validclasses))
    return(x@Classes)
  
  if (inherits(x, 'stock'))
    return(x@Ages@Classes)
  
  if (inherits(x, 'StockList'))
    return(purrr::map(x, Recall))
}

#' @rdname Access
#' @export
`Classes<-` <- function(x, value) {
  .AssignSlot(x, value, 'Classes')
}

#' @rdname Access
#' @export
Compliance <- function(x) {
  .AccessSlot(x, 'Compliance')
}

#' @rdname Access
#' @export
`Compliance<-` <- function(x, value) {
  .AssignSlot(x, value, 'Compliance')
}

#' @rdname Access
#' @export
CPUE <- function(x) {
  .AccessSlot(x, 'CPUE')
}

#' @rdname Access
#' @export
`CPUE<-` <- function(x, value) {
  .AssignSlot(x, value, 'CPUE')
}

#' @rdname Access
#' @export
CV <- function(x) {
  .AccessSlot(x, 'CV')
}

#' @rdname Access
#' @export
`CV<-` <- function(x, value) {
  .AssignSlot(x, value, 'CV')
}

#' @rdname Access
#' @export
CVatAge <- function(x) {
  .AccessSlot(x, 'CVatAge')
}

#' @rdname Access
#' @export
`CVatAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'CVatAge')
}

#' @rdname Access
#' @export
Dist <- function(x) {
  .AccessSlot(x, 'Dist')
}

#' @rdname Access
#' @export
`Dist<-` <- function(x, value) {
  .AssignSlot(x, value, 'Dist')
}

#' @rdname Access
#' @export
DiscardsAtAge <- function(x) {
  .AccessSlot(x, 'DiscardsAtAge')
}

#' @rdname Access
#' @export
`DiscardsAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'DiscardsAtAge')
}

#' @rdname Access
#' @export
DiscardsAtSize <- function(x) {
  .AccessSlot(x, 'DiscardsAtSize')
}

#' @rdname Access
#' @export
`DiscardsAtSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'DiscardsAtSize')
}

#' @rdname Access
#' @export
ESS <- function(x) {
  .AccessSlot(x, 'ESS')
}

#' @rdname Access
#' @export
`ESS<-` <- function(x, value) {
  .AssignSlot(x, value, 'ESS')
}

#' @rdname Access
#' @export
Error <- function(x) {
  .AccessSlot(x, 'Error')
}

#' @rdname Access
#' @export
`Error<-` <- function(x, value) {
  .AssignSlot(x, value, 'Error')
}

#' @rdname Access
#' @export
Exploitation <- function(x) {
  .AccessSlot(x, 'Exploitation')
}

#' @rdname Access
#' @export
`Exploitation<-` <- function(x, value) {
  .AssignSlot(x, value, 'Exploitation')
}


#' @rdname Access
#' @export
LandingsAtAge <- function(x) {
  .AccessSlot(x, 'LandingsAtAge')
}

#' @rdname Access
#' @export
`LandingsAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'LandingsAtAge')
}

#' @rdname Access
#' @export
LandingsAtSize <- function(x) {
  .AccessSlot(x, 'LandingsAtSize')
}

#' @rdname Access
#' @export
`LandingsAtSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'LandingsAtSize')
}

#' @rdname Access
#' @export
LifeHistory <- function(x) {
  .AccessSlot(x, 'LifeHistory')
}

#' @rdname Access
#' @export
`LifeHistory<-` <- function(x, value) {
  .AssignSlot(x, value, 'LifeHistory')
}


#' @rdname Access
#' @export
Mean <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Mean')))
  .AccessSlot(x, 'Mean')
}

#' @rdname Access
#' @export
`Mean<-` <- function(x, value) {
  .AssignSlot(x, value, 'Mean')
}


#' @rdname Access
#' @export
MeanAtAge <- function(x) {
  .AccessSlot(x, 'MeanAtAge')
}

#' @rdname Access
#' @export
`MeanAtAge<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtAge')
}

#' @rdname Access
#' @export
MeanAtLength <- function(x) {
  .AccessSlot(x, 'MeanAtLength')
}

#' @rdname Access
#' @export
`MeanAtLength<-` <- function(x, value) {
  .AssignSlot(x, value, 'MeanAtLength')
}

#' @rdname Access
#' @export
MeanAtWeight <- function(x) {
  .AccessSlot(x, 'MeanAtWeight')
}

#' @rdname Access
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

#' @rdname Access
#' @export
MPs <- function(x) {
  if (!isS4(x) && is.list(x))
    return(purrr::map(x, MPs))
  .AccessSlot(x, 'MPs')
}

#' @rdname Access
#' @export
`MPs<-` <- function(x, value) {
  .AssignSlot(x, value, 'MPs')
}

#' @rdname Access
#' @export
Model <- function(x) {
  .AccessSlot(x, 'Model')
}


#' @rdname Access
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

#' @rdname Access
#' @export
nSim <- function(x) {
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

#' @rdname Access
#' @export
`nSim<-` <- function(x, value) {
  .AssignSlot(x, value, 'nSim')
}

#' @rdname Access
#' @export
nArea <- function(x, st = 1) {
  if (inherits(x, 'hist') || inherits(x, 'mse'))
    x <- x@OM
  
  if (inherits(x, 'data'))
    return(x@nArea)
  
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock))
      stock <- stock[[st]]
  } else {
    stock <- x
  }
  
  dd <- dim(stock@Spatial@UnfishedDist)
  d1 <- length(stock@Spatial@UnfishedDist)
  
  if (length(dd) < 1) {
    if (length(d1) > 0) return(d1)
    return(1)
  }
  
  nms <- names(dimnames(stock@Spatial@UnfishedDist))
  as.numeric(dd[which(nms == 'Area')])
}

#' @rdname Access
#' @export
nAge <- function(x, st = NULL) {
  if (inherits(x, 'ages'))
    return(length(x@Classes))
  
  if (inherits(x, 'hist') || inherits(x, 'mse'))
    x <- x@OM
  
  if (inherits(x, 'om')) {
    stock <- x@Stock
    if (is.list(stock)) {
      if (!is.null(st))
        return(length(stock[[st]]@Ages@Classes))
      return(lapply(stock, nAge))
    }
    return(length(stock@Ages@Classes))
  }
  
  if (inherits(x, 'stock'))
    return(length(x@Ages@Classes))
}

#' @rdname Access
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

#' @rdname Access
#' @export
nStock <- function(x) {
  .CheckClass(x, c('om', 'hist', 'mse'), 'x')
  
  if (inherits(x, 'om'))
    return(length(x@Stock))
  
  if (inherits(x, 'hist'))
    return(length(x@OM@Stock))
  
  if (inherits(x, 'mse'))
    return(length(x@OM@Stock))
}

#' @rdname Access
#' @export
nFleet <- function(x) {
  .CheckClass(x, c('om', 'hist', 'mse', 'data'), 'x')
  
  if (inherits(x, 'om')) {
    fleet <- x@Fleet
    if (is.null(fleet))
      return(0)
    if (inherits(fleet, 'fleet'))
      return(1)
    if (is.list(fleet[[1]]))
      return(length(fleet[[1]]))
    if (isS4(fleet[[1]])) {
      dd <- dim(x@Fleet[[1]]@Selectivity@MeanAtAge)
      return(dd[3])
    }
  }
  
  if (inherits(x, 'data')) {
    return(
      lapply(list(x@Landings@Value,
                  x@Discards@Value,
                  x@Survey@Value,
                  x@CPUE@Value,
                  NULL), ncol) |>
        unlist() |> max()
    )
  }
  
  return(dim(x@LandingsAtAge[[1]])[[4]])
}

#' @rdname Access
#' @export
Pars <- function(x) {
  .AccessSlot(x, 'Pars')
}

#' @rdname Access
#' @export
`Pars<-` <- function(x, value) {
  .AssignSlot(x, value, 'Pars')
}

#' @rdname Access
#' @export
Period <- function(x) {
  .AccessSlot(x, 'Period')
}

#' @rdname Access
#' @export
`Period<-` <- function(x, value) {
  .AssignSlot(x, value, 'Period')
}

#' @rdname Access
#' @export
Prob <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Prob')))
  .AccessSlot(x, 'Prob')
}

#' @rdname Access
#' @export
`Prob<-` <- function(x, value) {
  .AssignSlot(x, value, 'Prob')
}

#' @rdname Access
#' @export
Random <- function(x) {
  .AccessSlot(x, 'Random')
}

#' @rdname Access
#' @export
Ref <- function(x) {
  .AccessSlot(x, 'Ref')
}

#' @rdname Access
#' @export
`Ref<-` <- function(x, value) {
  .AssignSlot(x, value, 'Ref')
}

#' @rdname Access
#' @export
RefCV <- function(x) {
  .AccessSlot(x, 'RefCV')
}

#' @rdname Access
#' @export
`RefCV<-` <- function(x, value) {
  .AssignSlot(x, value, 'RefCV')
}

#' @rdname Access
#' @export
Reference <- function(x) {
  .AccessSlot(x, 'Reference')
}

#' @rdname Access
#' @export
`Reference<-` <- function(x, value) {
  .AssignSlot(x, value, 'Reference')
}

#' @rdname Access
#' @export
SampleSize <- function(x) {
  .AccessSlot(x, 'SampleSize')
}

#' @rdname Access
#' @export
`SampleSize<-` <- function(x, value) {
  .AssignSlot(x, value, 'SampleSize')
}

#' @rdname Access
#' @export
Size <- function(x) {
  .AccessSlot(x, 'Size')
}

#' @rdname Access
#' @export
`Size<-` <- function(x, value) {
  .AssignSlot(x, value, 'Size')
}

#' @rdname Access
#' @export
SD <- function(x) {
  .AccessSlot(x, 'SD')
}

#' @rdname Access
#' @export
`SD<-` <- function(x, value) {
  .AssignSlot(x, value, 'SD')
}

#' @rdname Access
#' @export
Stat <- function(x) {
  if (inherits(x, 'pm'))
    return(Array2DF(.AccessSlot(x, 'Stat')))
  .AccessSlot(x, 'Stat')
}

#' @rdname Access
#' @export
`Stat<-` <- function(x, value) {
  .AssignSlot(x, value, 'Stat')
}

#' @rdname Access
#' @export
Shift <- function(x) {
  .AccessSlot(x, 'Shift')
}

#' @rdname Access
#' @export
`Shift<-` <- function(x, value) {
  .AssignSlot(x, value, 'Shift')
}

#' @rdname Access
#' @export
Stats <- function(x) {
  .AccessSlot(x, 'Stats')
}

#' @rdname Access
#' @export
`Stats<-` <- function(x, value) {
  .AssignSlot(x, value, 'Stats')
}

#' @rdname Access
#' @export
Survey <- function(x) {
  .AccessSlot(x, 'Survey')
}

#' @rdname Access
#' @export
`Survey<-` <- function(x, value) {
  .AssignSlot(x, value, 'Survey')
}

#' @rdname Access
#' @export
Timing <- function(x) {
  .AccessSlot(x, 'Timing')
}

#' @rdname Access
#' @export
TruncSD <- function(x) {
  .AccessSlot(x, 'TruncSD')
}

#' @rdname Access
#' @export
`TruncSD<-` <- function(x, value) {
  .AssignSlot(x, value, 'TruncSD')
}

#' @rdname Access
#' @export
Type <- function(x) {
  .AccessSlot(x, 'Type')
}

#' @rdname Access
#' @export
`Type<-` <- function(x, value) {
  .AssignSlot(x, value, 'Type')
}

#' @rdname Access
#' @export
Units <- function(x) {
  .AccessSlot(x, 'Units')
}

#' @rdname Access
#' @export
`Units<-` <- function(x, value) {
  .AssignSlot(x, value, 'Units')
}

#' @rdname Access
#' @export
Value <- function(x) {
  .AccessSlot(x, 'Value')
}

#' @rdname Access
#' @export
`Value<-` <- function(x, value) {
  .AssignSlot(x, value, 'Value')
}

#' @rdname Access
#' @export
YearLH <- function(x) {
  .AccessSlot(x, 'YearLH')
}

#' @rdname Access
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
  if (!slot %in% slotNames(x)) {
    cli::cli_alert_warning(
      "Slot {.code {slot}} not found in class {.cls {class(x)}}."
    )
    return(invisible(NULL))
  }
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
