
#' Unfished Spawning Production Per Recruit
#'
#' Calculates the unfished spawning production per recruit (SPR0) — the
#' denominator of the spawning potential ratio — under equilibrium conditions.
#' SPR0 is computed as the element-wise ratio of unfished spawning production
#' [SP0()] to unfished recruitment [R0()], with array broadcasting handled by
#' [ArrayDivide()].
#'
#' @param OM A [stock-class], [om-class], or [hist-class] object. For a
#'   [stock-class] or [om-class] object, only the stock components are
#'   populated and used, so the OM does not need `Fleet`, `Obs`, or `Imp`
#'   objects. A [stock-class] object is placed in an OM created with [OM()],
#'   using the arguments in `...`. If a [hist-class] object is provided (the
#'   output of [Simulate()]), it is used directly.
#' @param silent Logical. If `TRUE`, suppresses progress messages during
#'   population. Default is `FALSE`.
#' @param ... Arguments passed to [OM()] when `OM` is a [stock-class] object,
#'   e.g., `nSim`, `nYear`, `pYear`, `CurrentYear`, and `Seasons`.
#'
#' @return An array with dimensions `[Sim, Stock, Year]` containing the
#'   unfished spawning production per recruit. Dimensions where values are
#'   identical across simulations or years are collapsed by [ReduceDims()].
#'
#' @seealso [SP0()], [R0()], [ArrayDivide()]
#' @export
CalcSPR0 <- function(OM, silent = FALSE, ...) {
  if (inherits(OM, 'stock'))
    OM <- OM(Stock = OM, ...)

  if (inherits(OM, 'om')) {
    OM@Fleet <- NULL
    OM     <- .PopulateStocksOnly(OM, silent = silent)
    SP0    <- CalcUnfished_Equilibrium(OM, silent = silent)@SProduction
    R0     <- R0(OM)
    Stocks <- OM@Stock
  } else if (inherits(OM, 'hist')) {
    Hist <- OM
    if (EmptyObject(Hist@Unfished@Equilibrium))
      Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Hist, silent)
    SP0    <- SP0(Hist, Reduce = FALSE)
    R0     <- R0(Hist)
    Stocks <- Hist@OM@Stock
  } else {
    cli::cli_abort("`OM` must be class `stock`, `om`, or `hist`")
  }

  # SPR0[t] = SP0[t-lag] / R0[t]: the spawning that produced each recruit cohort
  # divided by the number of recruits. For equilibrium (stationary seasonal
  # pattern), circular shift is correct.
  nT <- dim(SP0)[3]
  for (st in seq_along(Stocks)) {
    stock <- Stocks[[st]]
    lag <- if (!is.null(stock@SRR@SpawnLag)) {
      as.integer(round(stock@SRR@SpawnLag))
    } else {
      round(min(stock@Ages@Classes) * stock@Seasons)
    }
    if (lag > 0) {
      idx <- ((seq_len(nT) - 1L - lag) %% nT) + 1L
      SP0[, st, ] <- SP0[, st, idx]
    }
  }

  ArrayDivide(SP0, R0) |> ReduceDims()

}

