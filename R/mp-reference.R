#' Reference Management Procedures
#'
#' Diagnostic management procedures that fish at fixed fractions of `FMSY`
#' (effort-based) or `MSY` (TAC-based), using true operating-model reference
#' points. 
#'
#' `refFMSY`, `refFMSY75`, `refFMSY50` set effort so that realised F
#' equals 100%, 75%, or 50% of `FMSY`, and default to being applied every
#' year (like any MP, this can be overridden via `OM@Interval`).
#'
#' `refMSY`, `refMSY75`, `refMSY50` set TAC to 100%, 75%, or 50% of
#' `MSY` (landings plus discards).
#'
#' `refFCurr` fixes F at its last historical value. It is currently a wrapper
#' around [CurrentEffort()], since `advice-class` has no way to specify
#' apical F directly. In spatial models, or where catchability (`q`) drifts
#' over time, realised F can diverge from the last historical effort. It will
#' be updated to set F directly once `advice-class` supports it.
#'
#' `NoFishing` sets Effort to 0 every year
#'
#' `FMSY`/`MSY` are computed once, during `Simulate()`, at the terminal
#' historical year (or `OM@Control$RefYears`) and held fixed for the whole
#' projection; i.e., they are not recalculated per projection year.
#'
#' If reference points are not available for an `OM`  each reference MP returns
#' empty advice and a single warning is issued the first time it is called.
#'
#' In spatial `OM`s, fleet targeting means the realised F from `refFMSY*`
#' may differ from the specified fraction of `FMSY`, because the `FMSY`
#' calculation does not account for spatial structure.
#'
#' @param Data A [data-class] object containing historical observations.
#'
#' @return An [advice-class] object.
#'
#' @seealso [Advice()], [data-class], [advice-class], [CalcMSY()], [F_FMSY()],
#'   [CurrentEffort()]
#' @name ReferenceMPs
NULL


#' @rdname ReferenceMPs
#' @export
refFMSY <- function(Data) .RefFMSYAdvice(Data, 1)
class(refFMSY) <- 'mp'
attr(refFMSY, 'Interval') <- 1
attr(refFMSY, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refFMSY75 <- function(Data) .RefFMSYAdvice(Data, 0.75)
class(refFMSY75) <- 'mp'
attr(refFMSY75, 'Interval') <- 1
attr(refFMSY75, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refFMSY50 <- function(Data) .RefFMSYAdvice(Data, 0.5)
class(refFMSY50) <- 'mp'
attr(refFMSY50, 'Interval') <- 1
attr(refFMSY50, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refMSY <- function(Data) .RefMSYAdvice(Data, 1)
class(refMSY) <- 'mp'
attr(refMSY, 'Interval') <- 1
attr(refMSY, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refMSY75 <- function(Data) .RefMSYAdvice(Data, 0.75)
class(refMSY75) <- 'mp'
attr(refMSY75, 'Interval') <- 1
attr(refMSY75, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refMSY50 <- function(Data) .RefMSYAdvice(Data, 0.5)
class(refMSY50) <- 'mp'
attr(refMSY50, 'Interval') <- 1
attr(refMSY50, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refFCurr <- function(Data) CurrentEffort(Data)
class(refFCurr) <- 'mp'
attr(refFCurr, 'Interval') <- 1

#' @rdname ReferenceMPs
#' @export
NoFishing <- function(Data) Advice(Effort = 0, EffType = 'Abs')
class(NoFishing) <- 'mp'
attr(NoFishing, 'Interval') <- 1

#' @rdname ReferenceMPs
#' @export
ReferenceMPs <- function() {
  c('refFMSY', 'refFMSY75', 'refFMSY50', 'refFCurr', 'NoFishing')
}



.RefPointValue <- function(arr, StockName) {
  if (is.null(arr) || !length(arr))
    return(NA_real_)
  
  dn        <- dimnames(arr)
  stock_pos <- which(names(dn) == 'Stock')
  
  if (length(stock_pos) && !is.null(StockName) && StockName %in% dn[[stock_pos]]) {
    idx <- rep(list(TRUE), length(dim(arr)))
    idx[[stock_pos]] <- match(StockName, dn[[stock_pos]])
    arr <- do.call('[', c(list(arr), idx, list(drop = TRUE)))
  }
  
  val <- as.numeric(arr)
  val[length(val)]
}


.RefMSYCatchValue <- function(arr, StockName) {
  if (is.null(arr) || !length(arr))
    return(NA_real_)
  
  dn        <- dimnames(arr)
  stock_pos <- which(names(dn) == 'Stock')
  
  if (!length(stock_pos))
    return(.RefPointValue(arr, StockName))
  
  if (!is.null(StockName) && StockName %in% dn[[stock_pos]])
    return(.RefPointValue(arr, StockName))
  
  sub <- apply(arr, setdiff(names(dn), 'Stock'), sum, na.rm = TRUE)
  val <- as.numeric(sub)
  val[length(val)]
}

.RefFMSYAdvice <- function(Data, fraction) {
  ref <- Data@Misc$DataOM@Reference
  FMSY     <- .RefPointValue(ref@MSY@FMSY,           Data@Misc$StockName)
  FCurrent <- .RefPointValue(ref@MSY@Misc$FCurrent,  Data@Misc$StockName)
  
  if (is.na(FMSY) || is.na(FCurrent) || FCurrent <= 0)
    return(Advice())
  
  Advice(Effort = fraction * FMSY / FCurrent, EffType = 'Rel')
}

.RefMSYAdvice <- function(Data, fraction) {
  ref <- Data@Misc$DataOM@Reference
  Landings <- .RefMSYCatchValue(ref@MSY@MSYLandings, Data@Misc$StockName)
  Discards <- .RefMSYCatchValue(ref@MSY@MSYDiscards, Data@Misc$StockName)
  
  if (is.na(Landings))
    return(Advice())
  if (is.na(Discards))
    Discards <- 0
  
  Advice(TAC = fraction * (Landings + Discards), TACType = 'Removals')
}
