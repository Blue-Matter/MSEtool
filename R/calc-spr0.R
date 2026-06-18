
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

  SP0 <- SP0(Hist, Reduce = FALSE)
  R0  <- R0(Hist)

  # SPR0[t] = SP0[t-lag] / R0[t]: the spawning that produced each recruit cohort
  # divided by the number of recruits. For equilibrium (stationary seasonal
  # pattern), circular shift is correct.
  nT <- dim(SP0)[3]
  for (st in seq_along(Hist@OM@Stock)) {
    lag <- round(min(Hist@OM@Stock[[st]]@Ages@Classes) * Hist@OM@Stock[[st]]@Seasons)
    if (lag > 0) {
      idx <- ((seq_len(nT) - 1L - lag) %% nT) + 1L
      SP0[, st, ] <- SP0[, st, idx]
    }
  }

  ArrayDivide(SP0, R0) |> ReduceDims()

}


