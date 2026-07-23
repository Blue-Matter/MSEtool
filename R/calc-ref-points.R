#' Calculate Per-Recruit and Replacement-Line Reference Points
#'
#' Computes `F0.1`, `Fmax`, `Fx%SPR`, `Fmed`, `Fcrash`, and `SPRcrash`
#' biological reference points and stores them in `Hist@Reference`.
#'
#' @param Hist A [hist-class] or [om-class] object. If an `om` is supplied
#'   it is first converted to a `hist` object.
#' @param Years Integer vector of years for which reference points are
#'   evaluated. If `NULL` (default), the final historical year is used.
#' @param SPRTarget Numeric vector of SPR targets (e.g. `0.4` for F40%) at
#'   which to evaluate `Fx%SPR`. Default `c(0.3, 0.4, 0.5)`.
#' @param type Character. Whether yield-per-recruit is defined in terms of
#'   total removals or landings only. One of `'Removals'` (default) or
#'   `'Landings'`.
#' @param nF Integer. Number of apical F values in the log-spaced search
#'   grid spanning `0` to `Hist@OM@maxF`. Default `100`.
#' @param BLow Logical. Also calculate the `BLow` reference point (see
#'   [CalcBLow()])? Default `FALSE`. Substantially more expensive than the
#'   other reference points here -- a full population projection plus a
#'   numerical search, repeated per simulation and stock -- so it is kept
#'   as a separate opt-in rather than bundled into the default set.
#' @param HZN,Bfrac Passed to [CalcBLow()] when `BLow = TRUE`.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' All reference points except `BLow` are evaluated on the same per-recruit
#' apical-F grid via [CalcPerRecruit()], reusing a single evaluation rather
#' than re-optimizing per reference point.
#'
#' - `F0.1`: apical F at which the slope of the yield-per-recruit curve is
#'   10% of the slope at the origin.
#' - `Fmax`: apical F maximising yield-per-recruit. Undefined (returned as
#'   the upper grid bound) for asymptotic selectivity, where
#'   yield-per-recruit has no interior maximum.
#' - `Fx%SPR`: apical F at which spawning-per-recruit relative to unfished
#'   equals each value in `SPRTarget`.
#' - `Fcrash`/`SPRcrash`: the apical F (and corresponding relative SPR) at
#'   which the per-recruit replacement line crosses the compensation ratio
#'   implied by steepness, evaluated against a fixed reference
#'   spawning-per-recruit at the first historical year -- matching the
#'   fixed-alpha/beta stock-recruitment curve.
#'   Only defined for `BevertonHolt` and `Ricker` stock-recruitment models.
#' - `Fmed`: apical F at which the per-recruit replacement line equals the
#'   median historical recruits-per-spawner ratio.
#' - `BLow` (optional): see [CalcBLow()].
#'
#' @return The input [hist-class] object with `Hist@Reference@F01`,
#'   `@FMax`, `@FSPR`, `@FMed`, `@FCrash`, `@SPRcrash`, and (if
#'   `BLow = TRUE`) `@BLow` populated.
#'
#' @seealso [CalcPerRecruit()], [CalcMSY()], [CalcBLow()], [reference-class],
#'   [F01()], [FMax()], [FSPR()], [FMed()], [FCrash()], [SPRcrash()]
#' @export
CalcRefPoints <- function(Hist,
                          Years     = NULL,
                          SPRTarget = c(0.3, 0.4, 0.5),
                          type      = c('Removals', 'Landings'),
                          nF        = 100,
                          BLow      = FALSE,
                          HZN       = 2,
                          Bfrac     = 0.5,
                          silent    = FALSE) {

  type <- match.arg(type)
  .CheckClass(Hist, c('om', 'hist'))
  if (inherits(Hist, 'om')) Hist <- .OM2Hist(Hist, silent = TRUE)
  .CheckClass(Hist, 'hist', 'Hist')

  nSeason <- Hist@OM@Seasons
  if (is.null(Years)) {
    Years <- utils::tail(Years(Hist@OM, 'Historical'), 1)
    if (nSeason > 1L) Years <- unique(floor(Years))
  }

  if (is.null(Hist@Reference@SPR0))
    Hist@Reference@SPR0 <- CalcSPR0(Hist, silent = TRUE)

  RefYear <- utils::head(Years(Hist@OM, 'Historical'), 1)
  if (nSeason > 1L) RefYear <- floor(RefYear)
  Phi0Ref <- CalcPerRecruit(Hist, apicalF = 1e-4, Years = RefYear)@SPR0 |>
    DropDimension('Year', warn = FALSE)

  Fgrid <- exp(seq(log(1e-4), log(Hist@OM@maxF), length.out = nF))

  PerRecruit <- CalcPerRecruit(Hist, apicalF = Fgrid, Years = Years)
  YPR  <- if (type == 'Removals') PerRecruit@Removals else PerRecruit@Landings
  SPRF <- PerRecruit@SPRF
  SPR  <- PerRecruit@SPR
  RPS  <- 1 / SPRF

  F01  <- .ApplyOverF(YPR, Fgrid, .FindF01)
  FMax <- .ApplyOverF(YPR, Fgrid, .FindFmax)

  FSPR <- purrr::map(SPRTarget, \(tgt)
    .ApplyOverF(SPR, Fgrid, .Interp1, target = tgt)
  ) |> List2Array('Target', pos = length(dim(F01)) + 1)
  dimnames(FSPR)[['Target']] <- as.character(SPRTarget)

  # CR, Phi0Ref, and the Fmed target can each independently stay at Sim=1
  # (e.g. deterministic biology with only stochastic fleet effort), so they
  # are explicitly extended to the true nSim before combining with
  # per-recruit quantities that may already be fully Sim-extended.
  nSim_ <- nSim(Hist)
  CR      <- .CompensationRatio(Hist, Years) |> ExtendSims(nSim_)
  Phi0Ref <- ExtendSims(Phi0Ref, nSim_)
  Alpha   <- ArrayDivide(CR, .BroadcastTo(Phi0Ref, CR))

  CrashRes <- .CalcFcrash(RPS, SPR, Fgrid, Alpha)

  MedTarget <- .MedianReplacement(Hist) |> ExtendSims(nSim_)
  FMed <- .ApplyOverF(RPS, Fgrid, .Interp1,
                        target = .BroadcastTo(MedTarget, RPS |> DropDimension('F', warn = FALSE)))

  Hist@Reference@F01      <- ReduceDims(F01)
  Hist@Reference@FMax     <- ReduceDims(FMax)
  Hist@Reference@FSPR     <- ReduceDims(FSPR)
  Hist@Reference@FMed     <- ReduceDims(FMed)
  Hist@Reference@FCrash   <- ReduceDims(CrashRes$Fcrash)
  Hist@Reference@SPRcrash <- ReduceDims(CrashRes$SPRcrash)

  if (!silent)
    cli::cli_alert_success("Calculated F0.1, Fmax, FSPR, Fmed, Fcrash and SPRcrash reference points")

  if (BLow)
    Hist <- CalcBLow(Hist, HZN = HZN, Bfrac = Bfrac, silent = silent)

  Hist
}

# Broadcasts a [Sim, Stock] (or similar, missing a dim present in `template`)
# array to match `template`'s dims by adding and replicating the missing dim.
.BroadcastTo <- function(x, template) {
  missing_dim <- setdiff(names(dimnames(template)), names(dimnames(x)))
  if (!length(missing_dim)) return(x)
  pos <- match(missing_dim, names(dimnames(template)))
  x1  <- AddDimension(x, missing_dim, dimnames(template)[[pos]][1], pos = pos)
  .ExtendAlongDim(x1, pos, dimnames(template)[[pos]])
}

# Moves the `F` dimension of `arr` last, flattens the remaining dims into
# matrix rows, applies FUN row-wise, and reshapes the result back to an
# array with the original (non-F) dims.
.ApplyOverF <- function(arr, Fgrid, FUN, target = NULL) {
  Fdim <- which(names(dimnames(arr)) == 'F')
  perm <- c(setdiff(seq_along(dim(arr)), Fdim), Fdim)
  arr_p <- .Aperm(arr, perm)
  d  <- dim(arr_p)
  nF <- d[length(d)]
  mat <- matrix(arr_p, ncol = nF)

  out <- if (is.null(target)) {
    apply(mat, 1, FUN, Fgrid = Fgrid)
  } else {
    target_vec <- if (length(target) == 1) rep(target, nrow(mat)) else as.numeric(target)
    vapply(seq_len(nrow(mat)), \(i) FUN(mat[i, ], Fgrid, target_vec[i]), numeric(1))
  }

  array(out, dim = d[-length(d)], dimnames = dimnames(arr_p)[-length(d)])
}

# Finds y at x == target by linear interpolation (x need not be pre-sorted).
.Interp1 <- function(x, y, target, rule = 1) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 2) return(NA_real_)
  x <- x[ok]; y <- y[ok]
  ord <- order(x)
  stats::approx(x[ord], y[ord], xout = target, rule = rule, ties = 'ordered')$y
}

.FindF01 <- function(ypr, Fgrid) {
  ok <- is.finite(ypr)
  if (sum(ok) < 3) return(NA_real_)
  y <- ypr[ok]; f <- Fgrid[ok]
  slope <- diff(y) / diff(f)
  .Interp1(slope, f[-length(f)], target = 0.1 * slope[1])
}

.FindFmax <- function(ypr, Fgrid) {
  if (!any(is.finite(ypr))) return(NA_real_)
  Fgrid[which.max(ypr)]
}

# Compensation ratio (initial slope of recruits-per-spawner) implied by
# steepness. NA for stock-recruitment models other than BevertonHolt/Ricker.
.CompensationRatio <- function(Hist, Years) {
  purrr::map(Hist@OM@Stock, \(stock) {
    model <- stock@SRR@Model
    if (identical(model, 'BevertonHolt')) {
      h <- .ArraySubsetYear(stock@SRR@Pars$h, Years)
      return(4 * h / (1 - h))
    }
    if (identical(model, 'Ricker')) {
      hR <- .ArraySubsetYear(stock@SRR@Pars$hR, Years)
      return((5 * hR)^1.25)
    }
    na <- .ArraySubsetYear(stock@SRR@Pars[[1]], Years)
    na[] <- NA_real_
    na
  }) |> List2Array('Stock', pos = 2)
}

.CalcFcrash <- function(RPS, SPR, Fgrid, Alpha) {
  Fdim <- which(names(dimnames(RPS)) == 'F')
  perm <- c(setdiff(seq_along(dim(RPS)), Fdim), Fdim)
  RPS_p <- .Aperm(RPS, perm)
  SPR_p <- .Aperm(SPR, perm)
  d  <- dim(RPS_p)
  nF <- d[length(d)]
  RPS_mat <- matrix(RPS_p, ncol = nF)
  SPR_mat <- matrix(SPR_p, ncol = nF)
  alpha_vec <- as.numeric(Alpha)

  n <- nrow(RPS_mat)
  Fcrash <- numeric(n); SPRcrash <- numeric(n)

  for (i in seq_len(n)) {
    rps <- RPS_mat[i, ]; spr <- SPR_mat[i, ]; a <- alpha_vec[i]

    if (!is.finite(a)) {
      Fcrash[i] <- NA_real_; SPRcrash[i] <- NA_real_
    } else if (min(rps, na.rm = TRUE) >= a) {
      Fcrash[i]   <- 0
      SPRcrash[i] <- min(1, rps[1] / a)
    } else if (max(rps, na.rm = TRUE) <= a) {
      Fcrash[i]   <- max(Fgrid)
      SPRcrash[i] <- max(.Interp1(rps, spr, target = a, rule = 2), 0.01)
    } else {
      Fcrash[i]   <- .Interp1(rps, Fgrid, target = a)
      SPRcrash[i] <- .Interp1(rps, spr,   target = a)
    }
  }

  list(
    Fcrash   = array(Fcrash,   dim = d[-length(d)], dimnames = dimnames(RPS_p)[-length(d)]),
    SPRcrash = array(SPRcrash, dim = d[-length(d)], dimnames = dimnames(RPS_p)[-length(d)])
  )
}

# Median historical recruits-per-spawner (age-at-recruitment numbers over
# spawning biomass), per [Sim, Stock].
.MedianReplacement <- function(Hist) {
  HistYears <- Years(Hist@OM, 'Historical')
  SSB <- Hist@SBiomass

  purrr::map(seq_along(Hist@OM@Stock), \(st) {
    AgeInd <- which.min(Hist@OM@Stock[[st]]@Ages@Classes)
    N      <- Hist@Number[[st]]
    AgeDim <- which(names(dimnames(N)) == 'Age')
    R <- abind::asub(N, AgeInd, AgeDim, drop = FALSE) |>
      DropDimension('Age', warn = FALSE) |>
      SumOverArea()
    S <- abind::asub(SSB, st, which(names(dimnames(SSB)) == 'Stock'), drop = FALSE) |>
      DropDimension('Stock', warn = FALSE)

    RPS <- R / S
    apply(RPS, which(names(dimnames(RPS)) == 'Sim'), stats::median, na.rm = TRUE)
  }) |> List2Array('Stock', pos = 2)
}
