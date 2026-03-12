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
  AccessSlot(x, 'AC')
}

#' @rdname Access
#' @export
`AC<-` <- function(x, value) {
  AssignSlot(x, value, 'AC')
}

#' @rdname Access
#' @export
BioEconomic <- function(x) {
  AccessSlot(x, 'BioEconomic')
}

#' @rdname Access
#' @export
`BioEconomic<-` <- function(x, value) {
  AssignSlot(x, value, 'BioEconomic')
}

#' @rdname Access
#' @export
Classes <- function(x) {
  validclasses <- c('ages', 'length', 'weight', 'maturity', 'fecundity',
                    'selectivity', 'discardmortality', 'retention')
  CheckClass(x, c('stock', 'StockList', validclasses), 'x')
  
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
  AssignSlot(x, value, 'Classes')
}

#' @rdname Access
#' @export
CPUE <- function(x) {
  AccessSlot(x, 'CPUE')
}

#' @rdname Access
#' @export
`CPUE<-` <- function(x, value) {
  AssignSlot(x, value, 'CPUE')
}

#' @rdname Access
#' @export
CV <- function(x) {
  AccessSlot(x, 'CV')
}

#' @rdname Access
#' @export
`CV<-` <- function(x, value) {
  AssignSlot(x, value, 'CV')
}

#' @rdname Access
#' @export
CVatAge <- function(x) {
  AccessSlot(x, 'CVatAge')
}

#' @rdname Access
#' @export
`CVatAge<-` <- function(x, value) {
  AssignSlot(x, value, 'CVatAge')
}

#' @rdname Access
#' @export
Dist <- function(x) {
  AccessSlot(x, 'Dist')
}

#' @rdname Access
#' @export
`Dist<-` <- function(x, value) {
  AssignSlot(x, value, 'Dist')
}

#' @rdname Access
#' @export
DiscardsAtAge <- function(x) {
  AccessSlot(x, 'DiscardsAtAge')
}

#' @rdname Access
#' @export
`DiscardsAtAge<-` <- function(x, value) {
  AssignSlot(x, value, 'DiscardsAtAge')
}

#' @rdname Access
#' @export
DiscardsAtSize <- function(x) {
  AccessSlot(x, 'DiscardsAtSize')
}

#' @rdname Access
#' @export
`DiscardsAtSize<-` <- function(x, value) {
  AssignSlot(x, value, 'DiscardsAtSize')
}

#' @rdname Access
#' @export
Dynamics <- function(x) {
  AccessSlot(x, 'Dynamics')
}

#' @rdname Access
#' @export
`Dynamics<-` <- function(x, value) {
  AssignSlot(x, value, 'Dynamics')
}

#' @rdname Access
#' @export
Exploitation <- function(x) {
  AccessSlot(x, 'Exploitation')
}

#' @rdname Access
#' @export
`Exploitation<-` <- function(x, value) {
  AssignSlot(x, value, 'Exploitation')
}

#' @rdname Access
#' @export
LandingsAtAge <- function(x) {
  AccessSlot(x, 'LandingsAtAge')
}

#' @rdname Access
#' @export
`LandingsAtAge<-` <- function(x, value) {
  AssignSlot(x, value, 'LandingsAtAge')
}

#' @rdname Access
#' @export
LandingsAtSize <- function(x) {
  AccessSlot(x, 'LandingsAtSize')
}

#' @rdname Access
#' @export
`LandingsAtSize<-` <- function(x, value) {
  AssignSlot(x, value, 'LandingsAtSize')
}

#' @rdname Access
#' @export
LifeHistory <- function(x) {
  AccessSlot(x, 'LifeHistory')
}

#' @rdname Access
#' @export
`LifeHistory<-` <- function(x, value) {
  AssignSlot(x, value, 'LifeHistory')
}

#' @rdname Access
#' @export
Log <- function(x) {
  AccessSlot(x, 'Log')
}

#' @rdname Access
#' @export
MeanAtAge <- function(x) {
  AccessSlot(x, 'MeanAtAge')
}

#' @rdname Access
#' @export
`MeanAtAge<-` <- function(x, value) {
  AssignSlot(x, value, 'MeanAtAge')
}

#' @rdname Access
#' @export
MeanAtLength <- function(x) {
  AccessSlot(x, 'MeanAtLength')
}

#' @rdname Access
#' @export
`MeanAtLength<-` <- function(x, value) {
  AssignSlot(x, value, 'MeanAtLength')
}

#' @rdname Access
#' @export
MeanAtWeight <- function(x) {
  AccessSlot(x, 'MeanAtWeight')
}

#' @rdname Access
#' @export
`MeanAtWeight<-` <- function(x, value) {
  AssignSlot(x, value, 'MeanAtWeight')
}

#' @rdname Access
#' @export
Misc <- function(x) {
  AccessSlot(x, 'Misc')
}

#' @rdname Access
#' @export
`Misc<-` <- function(x, value) {
  AssignSlot(x, value, 'Misc')
}

#' @rdname Access
#' @export
Model <- function(x) {
  AccessSlot(x, 'Model')
}

#' @rdname Access
#' @export
`Model<-` <- function(x, value) {
  AssignSlot(x, value, 'Model')
}

#' @rdname Access
#' @export
Name <- function(x) {
  if (inherits(x, 'mse'))
    x <- x@OM
  AccessSlot(x, 'Name')
}

#' @rdname Access
#' @export
`Name<-` <- function(x, value) {
  AssignSlot(x, value, 'Name')
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
  AssignSlot(x, value, 'nSim')
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
nStock <- function(object) {
  CheckClass(object, c('om', 'hist', 'mse'), 'object')
  
  if (inherits(object, 'om'))
    return(length(object@Stock))
  
  if (inherits(object, 'hist'))
    return(length(object@OM@Stock))
  
  if (inherits(object, 'mse'))
    return(length(object@OM@Stock))
}

#' @rdname Access
#' @export
nFleet <- function(object) {
  CheckClass(object, c('om', 'hist', 'mse', 'data'), 'object')
  
  if (inherits(object, 'om')) {
    fleet <- object@Fleet
    if (is.null(fleet))
      return(0)
    if (inherits(fleet, 'fleet'))
      return(1)
    if (is.list(fleet[[1]]))
      return(length(fleet[[1]]))
    if (isS4(fleet[[1]])) {
      dd <- dim(object@Fleet[[1]]@Selectivity@MeanAtAge)
      return(dd[3])
    }
  }
  
  if (inherits(object, 'data')) {
    return(
      lapply(list(object@Landings@Value,
                  object@Discards@Value,
                  object@Survey@Value,
                  object@CPUE@Value,
                  NULL), ncol) |>
        unlist() |> max()
    )
  }
  
  return(dim(object@LandingsAtAge[[1]])[[4]])
}

#' @rdname Access
#' @export
Pars <- function(x) {
  AccessSlot(x, 'Pars')
}

#' @rdname Access
#' @export
`Pars<-` <- function(x, value) {
  AssignSlot(x, value, 'Pars')
}

#' @rdname Access
#' @export
Period <- function(x) {
  AccessSlot(x, 'Period')
}

#' @rdname Access
#' @export
`Period<-` <- function(x, value) {
  AssignSlot(x, value, 'Period')
}

#' @rdname Access
#' @export
Random <- function(x) {
  AccessSlot(x, 'Random')
}

#' @rdname Access
#' @export
Ref <- function(x) {
  AccessSlot(x, 'Ref')
}

#' @rdname Access
#' @export
`Ref<-` <- function(x, value) {
  AssignSlot(x, value, 'Ref')
}

#' @rdname Access
#' @export
RefCV <- function(x) {
  AccessSlot(x, 'RefCV')
}

#' @rdname Access
#' @export
`RefCV<-` <- function(x, value) {
  AssignSlot(x, value, 'RefCV')
}

#' @rdname Access
#' @export
Reference <- function(x) {
  AccessSlot(x, 'Reference')
}

#' @rdname Access
#' @export
`Reference<-` <- function(x, value) {
  AssignSlot(x, value, 'Reference')
}

#' @rdname Access
#' @export
Seed <- function(x) {
  AccessSlot(x, 'Seed')
}

#' @rdname Access
#' @export
`Seed<-` <- function(x, value) {
  AssignSlot(x, value, 'Seed')
}

#' @rdname Access
#' @export
SD <- function(x) {
  AccessSlot(x, 'SD')
}

#' @rdname Access
#' @export
`SD<-` <- function(x, value) {
  AssignSlot(x, value, 'SD')
}

#' @rdname Access
#' @export
Survey <- function(x) {
  AccessSlot(x, 'Survey')
}

#' @rdname Access
#' @export
`Survey<-` <- function(x, value) {
  AssignSlot(x, value, 'Survey')
}

#' @rdname Access
#' @export
Timing <- function(x) {
  AccessSlot(x, 'Timing')
}

#' @rdname Access
#' @export
TruncSD <- function(x) {
  AccessSlot(x, 'TruncSD')
}

#' @rdname Access
#' @export
`TruncSD<-` <- function(x, value) {
  AssignSlot(x, value, 'TruncSD')
}

#' @rdname Access
#' @export
Units <- function(x) {
  AccessSlot(x, 'Units')
}

#' @rdname Access
#' @export
`Units<-` <- function(x, value) {
  AssignSlot(x, value, 'Units')
}

#' @rdname Access
#' @export
Value <- function(x) {
  AccessSlot(x, 'Value')
}

#' @rdname Access
#' @export
`Value<-` <- function(x, value) {
  AssignSlot(x, value, 'Value')
}

#' @rdname Access
#' @export
YearLH <- function(x) {
  AccessSlot(x, 'YearLH')
}

#' @rdname Access
#' @export
`YearLH<-` <- function(x, value) {
  AssignSlot(x, value, 'YearLH')
}


# ---- Helpers ----

#' Access a Named Slot from an S4 Object
#'
#' Internal helper that retrieves a named slot from an S4 object, with
#' informative errors if `x` is not S4 or the slot does not exist.
#'
#' @param x An S4 object.
#' @param slotname Character. Name of the slot to retrieve.
#'
#' @return The value stored in `slot(x, slotname)`.
#' @keywords internal
AccessSlot <- function(x, slotname) {
  CheckClass(slotname, 'character', 'slotname')
  if (!isS4(x))
    cli::cli_abort("{.arg x} is not an S4 object.")
  if (!slotname %in% slotNames(x))
    cli::cli_abort("Slot {.val {slotname}} not found in class {.cls {class(x)}}.")
  slot(x, slotname)
}

#' Assign a Value to a Named Slot of an S4 Object
#'
#' Internal helper that assigns a value to a named slot of an S4 object and
#' validates the result. Emits a warning and returns `NULL` invisibly if the
#' slot does not exist.
#'
#' @param x An S4 object.
#' @param value The value to assign.
#' @param slot Character. Name of the slot to assign to.
#'
#' @return `x` with the named slot set to `value`, after
#'   [methods::validObject()] is called. Returns `NULL` invisibly if the slot
#'   is not found.
#' @keywords internal
AssignSlot <- function(x, value, slot) {
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
