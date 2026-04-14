#' Unfished Reference Point Quantities
#'
#' Extract unfished reference point quantities from a [hist-class] object.
#' `B0()` returns total biomass, `SB0()` spawning biomass, `SP0()` spawning
#' production, and `N0()` numbers-at-age. All functions support both
#' equilibrium and dynamic unfished calculations (see `type` argument).
#'
#' @param Hist A [hist-class] object; the output of [Simulate()], containing
#'   the simulated historical fishery dynamics.
#' @param type Character. One of `'Equilibrium'` or `'Dynamic'`; partial
#'   matching is supported.
#'
#'   - `'Equilibrium'`: unfished values assuming equilibrium conditions — no
#'     recruitment deviations and all life-history parameters constant over
#'     time.
#'   - `'Dynamic'`: unfished values calculated by running the full population
#'     dynamics model, including recruitment deviations and any time-varying
#'     parameters.
#' @param Reduce Logical. If `TRUE` (default), calls [ReduceDims()] to
#'   collapse any `Sim`, `Year`, or `Age` dimensions where values are
#'   identical across that dimension, reducing array size. If `FALSE`, the
#'   slot contents are returned as-is with all dimensions intact.
#'
#' @return
#' - `N0()`: a named list with one element per Stock. Each element is an array
#'   with dimensions `[Sim, Age, Year]`. The `Sim` and `Year` dimensions are
#'   collapsed by [ReduceDims()] when `Reduce = TRUE` and values do not vary
#'   across those dimensions.
#' - `B0()`, `SB0()`, `SP0()`: an array with dimensions `[Sim, Stock, Year]`.
#'   The `Sim` and `Year` dimensions are collapsed by [ReduceDims()] when
#'   `Reduce = TRUE` and values do not vary across those dimensions.
#'
#' @seealso [ReduceDims()] for details on dimension collapsing.
#'
#' @name unfished
NULL

.unfished_slot <- function(Hist, type, slot_name, Reduce) {
  CheckClass(Hist, 'hist', 'Hist')
  type <- match.arg(type, c('Equilibrium', 'Dynamic'))
  
  obj <- if (type == 'Equilibrium') {
    slot(Hist@Unfished@Equilibrium, slot_name)
  } else {
    slot(Hist@Unfished@Dynamic, slot_name)
  }
  
  if (Reduce) ReduceDims(obj, IncYear = TRUE) else obj
}

#' @export
#' @rdname unfished
B0  <- function(Hist, type=c('Equilibrium','Dynamic'), Reduce=TRUE) 
  .unfished_slot(Hist, type, 'Biomass', Reduce)

#' @export
#' @rdname unfished
SB0 <- function(Hist, type=c('Equilibrium','Dynamic'), Reduce=TRUE) 
  .unfished_slot(Hist, type, 'SBiomass', Reduce)

#' @export
#' @rdname unfished
SP0 <- function(Hist, type=c('Equilibrium','Dynamic'), Reduce=TRUE) 
  .unfished_slot(Hist, type, 'SProduction', Reduce)

#' @export
#' @rdname unfished
N0 <- function(Hist, type=c('Equilibrium','Dynamic'), Reduce=TRUE) 
  .unfished_slot(Hist, type, 'Number', Reduce)

