#' Calculate the BLow Reference Point
#'
#' Computes `BLow`: the current spawning biomass at which it would take
#' `HZN` mean generation times to rebuild to `Bfrac x SBMSY` under zero
#' future catch. Stored in `Hist@Reference@BLow`.
#'
#' @param Hist A [hist-class] or [om-class] object. If an `om` is supplied
#'   it is first converted to a `hist` object.
#' @param HZN Numeric. Number of mean generation times (see [CalcMGT()])
#'   defining the rebuilding horizon. Default `2`.
#' @param Bfrac Numeric. Target spawning biomass as a fraction of `SBMSY`
#'   (see [SBMSY()]). Default `0.5`.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' For each simulation and stock, the current (last historical year)
#' numbers-at-age are scaled by a factor `k` and projected forward under
#' zero fishing effort for `round(HZN x MGT)` years. `k` is found via
#' [stats::optimize()] such that the projected spawning biomass at the end
#' of that horizon equals `Bfrac x SBMSY`. `BLow` is `k` times the current
#' spawning biomass.
#'
#' Requires `Hist@Reference@MSY` (see [CalcMSY()]) and enough projection
#' years in the operating model to cover the rebuilding horizon for every
#' simulation/stock; horizons exceeding the available projection years are
#' capped with a warning.
#'
#' This is substantially more expensive than the other reference points in
#' [CalcRefPoints()] (a full population projection plus a numerical search,
#' repeated per simulation and stock) and is not included by
#' `CalcRefPoints()`'s default reference-point set.
#'
#' @return The input [hist-class] object with `Hist@Reference@BLow`
#'   populated.
#'
#' @seealso [CalcMGT()], [CalcMSY()], [CalcRefPoints()], [reference-class]
#' @export
CalcBLow <- function(Hist, HZN = 2, Bfrac = 0.5, silent = FALSE) {

  .CheckClass(Hist, c('om', 'hist'))
  if (inherits(Hist, 'om')) Hist <- .OM2Hist(Hist, silent = TRUE)
  .CheckClass(Hist, 'hist', 'Hist')

  if (is.null(Hist@Reference@MSY))
    Hist@Reference@MSY <- CalcMSY(Hist, silent = TRUE)

  MGT <- CalcMGT(Hist, silent = TRUE) |> Extend(nSim = nSim(Hist)) |>
    DropDimension('Year', warn = FALSE)
  SBMSY_ <- SBMSY(Hist) |> Extend(nSim = nSim(Hist)) |>
    DropDimension('Year', warn = FALSE)
  MGT    <- .BroadcastStockDim(MGT,    SBMSY_)
  SBMSY_ <- .BroadcastStockDim(SBMSY_, MGT)

  MGThorizon <- array(pmax(1, round(HZN * MGT)), dim = dim(MGT), dimnames = dimnames(MGT))

  ProjYears <- Years(Hist@OM, 'Projection')
  nProjYears <- length(ProjYears)
  if (any(MGThorizon > nProjYears)) {
    cli::cli_alert_warning(
      "Rebuilding horizon exceeds available projection years for some sim/stock combinations; capping at {.val {nProjYears}}."
    )
    MGThorizon[MGThorizon > nProjYears] <- nProjYears
  }

  HistYears <- Years(Hist@OM, 'Historical')
  LastYearInd <- length(HistYears)
  StockNames <- StockNames(Hist)
  nSim_ <- nSim(Hist)
  nStock_ <- nStock(Hist)

  CurrentSSB <- SBiomass(Hist, df = FALSE)[, , LastYearInd, drop = FALSE] |>
    DropDimension('Year', warn = FALSE)

  BLow <- array(NA_real_, dim = c(nSim_, nStock_),
               dimnames = list(Sim = seq_len(nSim_), Stock = StockNames))

  for (sim in seq_len(nSim_)) {
    HistSim <- Subset(Hist, sim)
    for (st in seq_len(nStock_)) {
      BLow[sim, st] <- .CalcBlowSimStock(
        HistSim, st, LastYearInd, MGThorizon[sim, st],
        SBMSY_[sim, st], Bfrac, CurrentSSB[sim, st]
      )
    }
  }

  Hist@Reference@BLow <- ReduceDims(BLow)

  if (!silent)
    cli::cli_alert_success("Calculated BLow reference point")

  Hist
}

.BroadcastStockDim <- function(x, template) {
  if (!is.null(dim(x)) && 'Stock' %in% names(dimnames(x))) return(x)
  x <- AddDimension(x, 'Stock', dimnames(template)[['Stock']][1], pos = 2)
  .ExtendAlongDim(x, 2, dimnames(template)[['Stock']])
}

.CalcBlowSimStock <- function(HistSim, st, LastYearInd, horizon, sbmsy_target, Bfrac, current_ssb) {

  if (!is.finite(sbmsy_target) || sbmsy_target <= 0) return(NA_real_)

  target <- Bfrac * sbmsy_target
  StockName <- StockNames(HistSim)[st]
  mp_name <- '.CalcBLowZeroEffortMP'

  objective <- function(logk) {
    HistScaled <- HistSim
    HistScaled@Number[[st]][, , LastYearInd, ] <-
      HistScaled@Number[[st]][, , LastYearInd, ] * exp(logk)

    mse <- Project(HistScaled, MPs = mp_name, parallel = FALSE, silent = TRUE)
    projSSB <- SBiomass(mse, df = FALSE)[1, st, horizon, 1]
    (log(projSSB) - log(target))^2
  }

  opt <- stats::optimize(objective, log(c(0.05, 20)), tol = 1e-3)
  exp(opt$minimum) * current_ssb
}

.CalcBLowZeroEffortMP <- function(Data) Advice(Effort = 0)
class(.CalcBLowZeroEffortMP) <- 'mp'
