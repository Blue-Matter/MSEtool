#' Access MSY Reference Point Slots
#'
#' Accessor functions for MSY-based biological reference points stored in
#' `Hist@Reference@MSY`. Each function returns the corresponding array from
#' a [hist-class] or [mse-class] object.
#'
#' @param Hist A [hist-class] or [mse-class] object.
#'
#' @return A numeric array with dimensions `Sim × Stock × Year`, except
#'   `FMSY()` which returns `Sim × Complex × Year`. Returns `NULL` if the
#'   slot has not yet been populated (i.e. [CalcMSY()] has not been run).
#'
#' @details
#' The available accessor functions and the quantities they return are:
#'
#' - `MSYRefs()`: the full [refpointsMSY-class] object containing all slots.
#' - `FMSY()`: apical fishing mortality at MSY, defined at the complex
#'   level (`Sim × Complex × Year`).
#' - `BMSY()`: total biomass at MSY.
#' - `SBMSY()`: spawning biomass at MSY.
#' - `SPMSY()`: spawning production at MSY.
#' - `SPRMSY()`: spawning potential ratio at MSY.
#' - `MSYLandings()`: landed catch at MSY.
#' - `MSYDiscards()`: dead discards at MSY.
#'
#'
#' @seealso [CalcMSY()], [refpointsMSY-class]
#' @name MSY-accessors
NULL

#' @rdname MSY-accessors
#' @export
MSYRefs <- function(Hist) {
  .CheckClass(Hist, c('hist', 'mse'), 'Hist')
  .AccessSlot(Hist@Reference, 'MSY')
}


.ResolveRefpointsMSY <- function(Hist, fn_name = "MSY accessor") {
  supported <- c('hist', 'mse', 'reference', 'refpointsMSY')
  .CheckClass(Hist, supported, fn_name)
  
  if (inherits(Hist, 'refpointsMSY')) return(Hist)
  if (inherits(Hist, 'reference'))    return(Hist@MSY)
  # hist and mse
  Hist@Reference@MSY
}

#' @rdname MSY-accessors
#' @export
FMSY <- function(Hist) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, 'FMSY'), 'FMSY')
}

#' @rdname MSY-accessors
#' @export
BMSY <- function(Hist) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, 'BMSY'), 'BMSY')
}

#' @rdname MSY-accessors
#' @export
SBMSY <- function(Hist) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, 'SBMSY'), 'SBMSY')
}

#' @rdname MSY-accessors
#' @export
SPMSY <- function(Hist) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, 'SPMSY'), 'SPMSY')
}

#' @rdname MSY-accessors
#' @export
SPRMSY <- function(Hist) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, 'SPRMSY'), 'SPRMSY')
}

#' @rdname MSY-accessors
#' @export
MSYLandings <- function(Hist) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, 'MSYLandings'), 'MSYLandings')
}

#' @rdname MSY-accessors
#' @export
MSYDiscards <- function(Hist) {
  .AccessSlot(.ResolveRefpointsMSY(Hist, 'MSYDiscards'), 'MSYDiscards')
}
