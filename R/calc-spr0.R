
#' Unfished Spawning Production Per Recruit
#'
#' Calculates the unfished spawning production per recruit (SPR0) — the
#' denominator of the spawning potential ratio — under equilibrium conditions.
#' SPR0 is computed as the element-wise ratio of unfished spawning production
#' [SP0()] to unfished recruitment [R0()], with array broadcasting handled by
#' [ArrayDivide()].
#'
#' @param OM Either a [om-class] or [hist-class] object. If an [om-class]
#'   object is provided, the historical dynamics are populated internally via
#'   [Populate()] and [CalcUnfished_Equilibrium()], which may be
#'   computationally expensive. If a [hist-class] object is provided (the output
#'   of [Simulate()]), it is used directly.
#' @param silent Logical. If `TRUE`, suppresses progress messages during
#'   population and simulation. Only used when `object` is an [om-class].
#'   Default is `FALSE`.
#'
#' @return An array with dimensions `[Sim, Stock, Year]` containing the
#'   unfished spawning production per recruit. Dimensions where values are
#'   identical across simulations or years are collapsed by [ReduceDims()].
#'
#' @seealso [SP0()], [R0()], [ArrayDivide()]
#' @export
CalcSPR0 <- function(OM, silent = FALSE) {
  if (inherits(OM, 'om')) {
    OM <- Populate(OM, silent=silent)
    Hist <- OM2Hist(OM=OM, silent=silent)
  } else if (inherits(OM, 'hist')) {
    Hist <- OM
  } else {
    cli::cli_abort("`OM` must be class `om` or class `hist`")
  }
  
  if (EmptyObject(Hist@Unfished@Equilibrium))
    Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Hist, silent)
  
  SP0 <- SP0(Hist)
  R0 <- R0(Hist)
  ArrayDivide(SP0,R0) |> ReduceDims()

}


