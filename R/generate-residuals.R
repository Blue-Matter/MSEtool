
#' Generate log-normally distributed multiplicative residuals with optional AR(1)-autocorrelation 
#'
#' Generate random, AR(1)-autocorrelated multiplicative (normal-space)
#' residuals for multiple simulations and years, lognormally distributed with
#' marginal standard deviation `SD` (in log-space) and autocorrelation `AC`.
#'
#' @param SD Numeric vector of length nSim giving the marginal standard
#'   deviation, in log-space, of the returned (AC-propagated) residuals.
#' @param AC Numeric vector of length nSim giving autocorrelation (must be
#'   within -1 and 1, exclusive).
#' @param Years Numeric vector of years for which residuals are generated.
#' @param nSim Optional integer number of simulations (defaults to length(SD)).
#' @param TruncSD Numeric scalar specifying how many standard deviations at
#'   which to truncate the innovations. 
#' @param nSeasons Number of seasons in the array (default 1).
#' @param NA_Season Optional. List length `nSim` with each element containing integer vector
#' indicating which (if any) seasons have NA values for all years
#' @param LastError Optional numeric vector of length nSim giving the starting
#'   value for the AR(1) recursion, in normal space; i.e. the last
#'   observed multiplicative residual. `NA` (default) means no prior state
#'
#' @return Numeric array of dimensions nSim x nYear containing multiplicative
#'   (normal-space) lognormal residuals with AR(1) autocorrelation `AC`
#'   applied.
#' @seealso [ApplyAC()], [CalcResidualStats()]
#' @export
GenResiduals <- function(SD,
                         AC,
                         Years,
                         nSim = NULL,
                         TruncSD = 2,
                         nSeasons = 1,
                         NA_Season = list(),
                         LastError = NULL) {

  if (any(SD < 0))
    cli::cli_abort("`SD` cannot be negative")

  if (!is.numeric(TruncSD) || length(TruncSD) != 1 || TruncSD <= 0)
    cli::cli_abort("`TruncSD` must be a positive scalar")

  if (is.null(nSim)) nSim <- length(SD)

  nYear <- length(Years)

  if (is.null(LastError)) LastError <- rep(NA_real_, nSim)

  Innov <- .InnovationPars(SD, AC, TruncSD)

  # Expand mu, sigma, lower, upper for each year
  mu_mat <- matrix(Innov$mu, nrow = nSim, ncol = nYear)
  SD_mat <- matrix(Innov$sigma, nrow = nSim, ncol = nYear)
  lower_mat <- matrix(Innov$lower, nrow = nSim, ncol = nYear)
  upper_mat <- matrix(Innov$upper, nrow = nSim, ncol = nYear)

  LogResid <- .Rtnorm(nSim * nYear,
                as.vector(mu_mat),
                as.vector(SD_mat),
                as.vector(lower_mat),
                as.vector(upper_mat))

  arr <- array(LogResid, dim = c(nSim, nYear),
               dimnames = list(Sim = seq_len(nSim), Year = Years))

  if (length(NA_Season)) {
    if (length(NA_Season)!=nSim)
      cli::cli_abort("`NA_Season` must be a length {.val nSim ({nSim})}")

    for (s in seq_along(NA_Season)) {
      NA_ind <- .ExpandSeasons(NA_seas = NA_Season[[s]],
                               nSeasons = nSeasons,
                               nYear = nYear)

      arr[s,NA_ind] <- NA
    }
  }

  freshStart <- is.na(LastError)
  if (any(freshStart)) {
    Innov0 <- .InnovationPars(SD, rep(0, nSim), TruncSD)
    for (s in which(freshStart)) {
      firstPos <- which(!is.na(arr[s, ]))[1]
      if (is.na(firstPos)) next
      arr[s, firstPos] <- .Rtnorm(1, Innov0$mu[s], Innov0$sigma[s],
                                  Innov0$lower[s], Innov0$upper[s])
    }
  }

  ApplyAC(Resid = exp(arr), AC = AC, LastError = LastError)
}


.TruncSDScale <- function(TruncSD) {
  sqrt(1 - 2 * TruncSD * dnorm(TruncSD) / (2 * pnorm(TruncSD) - 1))
}

.LogMeanExpAR1 <- function(sigma, TruncSD, AC, nTerm = 200) {
  j <- seq_len(nTerm) - 1
  z <- log(2 * pnorm(TruncSD) - 1)
  vapply(seq_along(sigma), function(i) {
    sw <- sigma[i] * sqrt(1 - AC[i]^2) * AC[i]^j
    sum(sw^2 / 2 + log(pnorm(TruncSD - sw) - pnorm(-TruncSD - sw)) - z)
  }, numeric(1))
}

.InnovationPars <- function(SD, AC, TruncSD) {
  if (any(abs(AC) >= 1, na.rm = TRUE))
    cli::cli_abort("{.arg AC} must be within {.val {c(-1, 1)}} exclusive: an AR(1) process with {.code abs(AC) >= 1} has no stationary distribution.")

  sigma <- SD / .TruncSDScale(TruncSD)
  mu    <- -.LogMeanExpAR1(sigma, TruncSD, AC) * (1 - AC) / sqrt(1 - AC^2)

  list(mu    = mu,
       sigma = sigma,
       lower = mu - TruncSD * sigma,
       upper = mu + TruncSD * sigma)
}

.TruncDevScale <- function(SD, TruncSD) {
  sigma <- SD / .TruncSDScale(TruncSD)
  lo    <- pnorm(-TruncSD)
  hi    <- pnorm(TruncSD)
  bias  <- if (sigma > 0) {
    sigma^2 / 2 +
      log(pnorm(TruncSD - sigma) - pnorm(-TruncSD - sigma)) - log(hi - lo)
  } else 0
  list(sigma = sigma, lo = lo, hi = hi, bias = bias)
}

.LatentToDev <- function(z, SD, TruncSD) {
  p <- .TruncDevScale(SD, TruncSD)
  if (p$sigma <= 0) return(rep(0, length(z)))
  qnorm(pnorm(z) * (p$hi - p$lo) + p$lo) * p$sigma - p$bias
}

.DevToLatent <- function(x, SD, TruncSD) {
  p <- .TruncDevScale(SD, TruncSD)
  if (p$sigma <= 0) return(rep(0, length(x)))
  xs <- pmin(pmax((x + p$bias) / p$sigma, -TruncSD), TruncSD)
  u  <- (pnorm(xs) - p$lo) / (p$hi - p$lo)
  qnorm(pmin(pmax(u, 1e-12), 1 - 1e-12))
}

.ExpandSeasons <- function(NA_seas, nSeasons, nYear) {
  rep(seq(0, nYear / nSeasons - 1) * nSeasons, each = length(NA_seas)) + rep(NA_seas, times = nYear / nSeasons)
}

#' Apply AR(1) autocorrelation to residuals
#'
#' Applies AR(1) propagation to a numeric matrix of multiplicative
#' (normal-space) residuals (`sim x year`), respecting missing values (NAs).
#' 
#' 
#' Autocorrelation is applied only across non-NA values within each
#' simulation. The recursion itself happens in log-space (the only space in
#' which it is valid), but both the input and the returned residuals are in
#' normal space.
#'
#' @param Resid Numeric matrix of multiplicative (normal-space) residuals,
#'   dimensions nSim x nYear.
#' @param AC Numeric vector of length nSim, autocorrelation coefficient per simulation (must be in between -1 &  1).
#' @param LastError Numeric vector of length nSim giving the starting value for
#'   the AR(1) recursion, in normal (natural) space — i.e. the last observed
#'   multiplicative residual. 
#' @return Numeric matrix of same dimensions as `Resid` with autocorrelated
#'   multiplicative (normal-space) residuals.
#' @seealso [GenResiduals()], [CalcResidualStats()]
#' @export
ApplyAC <- function(Resid, AC, LastError) {

  if (!is.numeric(Resid) || length(dim(Resid)) != 2) {
    cli::cli_abort("`Resid` must be a numeric matrix")
  }

  nSim <- nrow(Resid)
  nYear <- ncol(Resid)

  if (!is.numeric(AC) || length(AC) != nSim || any(abs(AC) > 1))
    cli::cli_abort("`AC` must be a numeric vector of length {.val nSim ({nSim})} with values in [-1,1]")

  if (!is.numeric(LastError) || length(LastError) != nSim)
    cli::cli_abort("`LastError` must be a numeric vector of length {.val nSim ({nSim})}")
  if (any(Resid <= 0, na.rm = TRUE))
    cli::cli_abort("`Resid` must be positive (it is in normal, not log, space)")
  if (any(LastError <= 0, na.rm = TRUE))
    cli::cli_abort("`LastError` must be positive (it is in normal, not log, space)")

  LogResid <- log(Resid)

  freshStart <- is.na(LastError)
  LastError <- log(LastError)
  LastError[freshStart] <- 0

  scale <- sqrt(1 - AC^2)


  for (s in seq_len(nSim)) {

    non_na_idx <- which(!is.na(LogResid[s, ]))
    if (length(non_na_idx) == 0) next

    if (!freshStart[s]) {
      LogResid[s, non_na_idx[1]] <- AC[s] * LastError[s] +
        LogResid[s, non_na_idx[1]] * scale[s]
    }

    # Apply AR(1) to remaining non-NA values
    if (length(non_na_idx) > 1) {
      for (t in 2:length(non_na_idx)) {
        i <- non_na_idx[t]
        prev <- non_na_idx[t - 1]
        LogResid[s, i] <- AC[s] * LogResid[s, prev] + LogResid[s, i] * scale[s]
      }
    }
  }

  exp(LogResid)
}

#' Calculate residual statistics for log-space index residuals
#'
#' Computes standard deviation and lag-1 autocorrelation of log residuals
#' for a simulated index (`sim x year`).
#'
#' Seasons that are NA in every year are treated as seasons the index is never
#' observed in, and are dropped; the seasons they occupy are recorded in
#' `NA_Season` so [GenResiduals()] can reproduce the same pattern in the
#' projection. The remaining values are taken in time order, and `AC` is the
#' lag-1 autocorrelation between consecutive observations. 
#' For an index observed in a single season, that is
#' the year-to-year autocorrelation. This matches how [ApplyAC()] propagates
#' the projection residuals, which steps from one observation to the next.
#'
#' `AC` may be negative, and is returned as estimated.
#'
#' @param LogResiduals A numeric matrix or array with dimensions `sim x year`.
#' @param nSeasons Number of seasons in the array (default 1).
#' @return A data.frame with columns:
#' * `AC`: lag-1 autocorrelation between consecutive observations, or `NA` when
#'   fewer than two observations are available
#' * `SD`: standard deviation of residuals
#' * `NA_Season`: a list length `nSim` with each element containing integer vector
#' indicating which (if any) seasons have NA values for all years
#' @seealso [ApplyAC()], [GenResiduals()]
#' @export
CalcResidualStats <- function(LogResiduals, nSeasons=1) {
  
  if (!is.array(LogResiduals) || length(dim(LogResiduals)) != 2) {
    cli::cli_abort("`LogResiduals` must be a 2D array (sim x year)", .internal = TRUE)
  }
  
  nSim <- dim(LogResiduals)[1]
  nYear <- dim(LogResiduals)[2]
  
  AC <- numeric(nSim)
  SD <- numeric(nSim)
  
  NA_Season <- vector('list', nSim)
  for (s in seq_len(nSim)) {
    res <- LogResiduals[s, ]
    
    # Identify seasons with any non-NA values across all years
    seasons_to_keep <- vapply(seq_len(nSeasons), function(season) {
      season_idx <- seq(season, nYear, by = nSeasons)
      any(!is.na(res[season_idx, drop = FALSE]))
    }, logical(1))
    
    NA_Season[[s]] <- which(!seasons_to_keep)
    
    if (!any(seasons_to_keep)) {
      cli::cli_warn("All seasons are fully NA in simulation {.val {s}}")
      AC[s] <- NA_real_
      SD[s] <- NA_real_
      next
    }
    
    valid_seasons <- sort(unlist(lapply(which(seasons_to_keep), function(season) {
      seq(season, nYear, by = nSeasons)
    })))

    res <- res[valid_seasons]
    res[!is.finite(res)] <- NA

    # lag-1 autocorrelation between consecutive observations
    non_na_idx <- which(!is.na(res))
    if (length(non_na_idx) <= 1) {
      AC[s] <- NA_real_
    } else {
      AC[s] <- acf(res[non_na_idx], plot = FALSE)$acf[2]
    }
    
    SD[s] <- sd(res, na.rm = TRUE)
    
  }
  
  data.frame(Sim = seq_len(nSim), AC = AC, SD = SD, NA_Season=I(NA_Season))
}


