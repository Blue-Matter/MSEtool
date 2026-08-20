#' Reference Management Procedures
#'
#' Diagnostic management procedures that fish at fixed fractions of `FMSY`
#' (effort-based) or `MSY` (TAC-based), using true operating-model reference
#' points. 
#'
#' `refFMSY_1`, `refFMSY_075`, `refFMSY_05` set effort so that realised F
#' equals 100%, 75%, or 50% of `FMSY`, and default to being applied every
#' year (like any MP, this can be overridden via `OM@Interval`).
#' 
#' `refMSY_1`, `refMSY_075`, `refMSY_05` set TAC to 100%, 75%, or 50% of
#' `MSY` (landings plus discards).
#'
#' `FMSY`/`MSY` are computed once, during `Simulate()`, at the terminal
#' historical year (or `OM@Control$RefYears`) and held fixed for the whole
#' projection; i.e., they are not recalculated per projection year. 
#' 
#' If reference points are not available for an `OM`  each reference MP returns
#' empty advice and a single warning is issued the first time it is called.
#'
#' In spatial `OM`s, fleet targeting means the realised F from `refFMSY_x`
#' may differ from the specified fraction of `FMSY`, because the `FMSY`
#' calculation does not account for spatial structure.
#'
#' @param Data A [data-class] object containing historical observations.
#'
#' @return An [advice-class] object.
#'
#' @seealso [Advice()], [data-class], [advice-class], [CalcMSY()], [F_FMSY()]
#' @name ReferenceMPs
NULL


#' @rdname ReferenceMPs
#' @export
refFMSY_1 <- function(Data) .RefFMSYAdvice(Data, 1)
class(refFMSY_1) <- 'mp'
attr(refFMSY_1, 'Interval') <- 1
attr(refFMSY_1, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refFMSY_075 <- function(Data) .RefFMSYAdvice(Data, 0.75)
class(refFMSY_075) <- 'mp'
attr(refFMSY_075, 'Interval') <- 1
attr(refFMSY_075, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refFMSY_05 <- function(Data) .RefFMSYAdvice(Data, 0.5)
class(refFMSY_05) <- 'mp'
attr(refFMSY_05, 'Interval') <- 1
attr(refFMSY_05, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refMSY_1 <- function(Data) .RefMSYAdvice(Data, 1)
class(refMSY_1) <- 'mp'
attr(refMSY_1, 'Interval') <- 1
attr(refMSY_1, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refMSY_075 <- function(Data) .RefMSYAdvice(Data, 0.75)
class(refMSY_075) <- 'mp'
attr(refMSY_075, 'Interval') <- 1
attr(refMSY_075, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
refMSY_05 <- function(Data) .RefMSYAdvice(Data, 0.5)
class(refMSY_05) <- 'mp'
attr(refMSY_05, 'Interval') <- 1
attr(refMSY_05, 'DataOM') <- list(Reference = TRUE)

#' @rdname ReferenceMPs
#' @export
ReferenceMPs <- function() {
  c('refFMSY_1', 'refFMSY_075', 'refFMSY_05',
    'refMSY_1',  'refMSY_075',  'refMSY_05')
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
