
#' Generate (log-space) residuals with truncated log-normal distribution
#'
#' Generate random residuals for multiple simulations and years.
#' Each simulation has a standard deviation `SD` and autocorrelation `AC`, 
#' and residuals are truncated to `TruncSD` standard deviations around the mean.
#' The resulting residuals can be propagated with autocorrelation using [ApplyAC()].
#'
#' @param SD Numeric vector of length nSim giving the marginal standard
#'   deviation of the residuals once [ApplyAC()] has propagated them.
#' @param AC Numeric vector of length nSim giving autocorrelation (must be
#'   within -1 and 1, exclusive).
#' @param Years Numeric vector of years for which residuals are generated.
#' @param nSim Optional integer number of simulations (defaults to length(SD)).
#' @param TruncSD Numeric scalar specifying how many standard deviations at
#'   which to truncate the innovations. The innovation standard deviation is
#'   inflated to offset the variance lost to truncation, so the propagated
#'   residuals retain marginal standard deviation `SD`.
#' @param nSeasons Number of seasons in the array (default 1).
#' @param NA_Season Optional. List length `nSim` with each element containing integer vector
#' indicating which (if any) seasons have NA values for all years
#' 
#' @return Numeric array of dimensions nSim x nYear containing truncated log-normal residuals.
#' @seealso [ApplyAC()], [CalcResidualStats()]
#' @export
GenResiduals <- function(SD, 
                         AC, 
                         Years, 
                         nSim = NULL, 
                         TruncSD = 2,
                         nSeasons = 1,
                         NA_Season = list()) {
  
  if (any(SD < 0)) 
    cli::cli_abort("`SD` cannot be negative")
  
  if (!is.numeric(TruncSD) || length(TruncSD) != 1 || TruncSD <= 0)
    cli::cli_abort("`TruncSD` must be a positive scalar")

  if (is.null(nSim)) nSim <- length(SD)

  nYear <- length(Years)

  # Innovation parameters; AR(1) is applied downstream by ApplyAC()
  Innov <- .InnovationPars(SD, AC, TruncSD)

  # Expand mu, sigma, lower, upper for each year
  mu_mat <- matrix(Innov$mu, nrow = nSim, ncol = nYear)
  SD_mat <- matrix(Innov$sigma, nrow = nSim, ncol = nYear)
  lower_mat <- matrix(Innov$lower, nrow = nSim, ncol = nYear)
  upper_mat <- matrix(Innov$upper, nrow = nSim, ncol = nYear)
  
  # Generate residuals
  LogResid <- .Rtnorm(nSim * nYear, 
                as.vector(mu_mat),
                as.vector(SD_mat),
                as.vector(lower_mat), 
                as.vector(upper_mat))
  
  arr <- array(LogResid, dim = c(nSim, nYear),
               dimnames = list(Sim = seq_len(nSim), Year = Years))
  

  
  if (!length(NA_Season)) return(arr)
    
  if (length(NA_Season)!=nSim)
    cli::cli_abort("`NA_Season` must be a length {.val nSim ({nSim})}")
  
  for (s in seq_along(NA_Season)) {
    NA_ind <- .ExpandSeasons(NA_seas = NA_Season[[s]], 
                             nSeasons = nSeasons, 
                             nYear = nYear)
    
    arr[s,NA_ind] <- NA
  }
  arr
}

# sd of a symmetric truncated normal, relative to its nominal sigma
.TruncSDScale <- function(TruncSD) {
  sqrt(1 - 2 * TruncSD * dnorm(TruncSD) / (2 * pnorm(TruncSD) - 1))
}

# log E[exp(x)] for the stationary AR(1) x_t = AC x_{t-1} + e_t sqrt(1 - AC^2),
# with e ~ symmetric truncated normal on +/- TruncSD * sigma
.LogMeanExpAR1 <- function(sigma, TruncSD, AC, nTerm = 200) {
  j <- seq_len(nTerm) - 1
  z <- log(2 * pnorm(TruncSD) - 1)
  vapply(seq_along(sigma), function(i) {
    sw <- sigma[i] * sqrt(1 - AC[i]^2) * AC[i]^j
    sum(sw^2 / 2 + log(pnorm(TruncSD - sw) - pnorm(-TruncSD - sw)) - z)
  }, numeric(1))
}

# Innovation mean/sd/bounds such that, after AR(1) propagation, the log
# deviations have marginal sd `SD` and mean(exp(deviation)) of 1. Truncating
# the innovation shrinks its variance, so sigma is inflated to compensate, and
# the lognormal bias correction accounts for the truncated (non-normal) shape.
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

# Latent spread, truncation probabilities, and lognormal bias offset shared by
# .LatentToDev() and its inverse
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

# Map a standard-normal AR(1) series onto a symmetric truncated-normal
# marginal via the probability integral transform, then bias-correct so
# mean(exp(x)) is 1. Because the bound applies to the marginal rather than to
# the innovations, it does not widen as autocorrelation increases.
.LatentToDev <- function(z, SD, TruncSD) {
  p <- .TruncDevScale(SD, TruncSD)
  if (p$sigma <= 0) return(rep(0, length(z)))
  qnorm(pnorm(z) * (p$hi - p$lo) + p$lo) * p$sigma - p$bias
}

# Inverse of .LatentToDev(), used to seed a latent AR(1) from observed
# deviations. Values outside the truncation support are clamped to it.
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

#' Apply AR(1) autocorrelation to residuals, skipping NAs
#'
#' Applies AR(1) propagation to a numeric matrix of residuals (`sim x year`), 
#' respecting missing values (NAs). Autocorrelation is applied only across 
#' non-NA values within each simulation.
#'
#' @param LogResid Numeric matrix of log residuals, dimensions nSim x nYear.
#' @param AC Numeric vector of length nSim, autocorrelation coefficient per simulation (must be in between -1 &  1).
#' @param LastError Numeric vector of length nSim, starting value for each simulation.
#' @return Numeric matrix of same dimensions as `LogResid` with autocorrelated residuals.
#' @seealso [GenResiduals()], [CalcResidualStats()]
#' @export
ApplyAC <- function(LogResid, AC, LastError) {
  
  if (!is.numeric(LogResid) || length(dim(LogResid)) != 2) {
    cli::cli_abort("`LogResid` must be a numeric matrix")
  }
  
  nSim <- nrow(LogResid)
  nYear <- ncol(LogResid)
  
  if (!is.numeric(AC) || length(AC) != nSim || any(abs(AC) > 1))
    cli::cli_abort("`AC` must be a numeric vector of length {.val nSim ({nSim})} with values in [-1,1]")
  
  if (!is.numeric(LastError) || length(LastError) != nSim)
    cli::cli_abort("`LastError` must be a numeric vector of length {.val nSim ({nSim})}")
  
  scale <- sqrt(1 - AC^2)
  
  # Apply AR(1) 
  for (s in seq_len(nSim)) {
    
    non_na_idx <- which(!is.na(LogResid[s, ]))
    if (length(non_na_idx) == 0) next
    
    # first time step
    LogResid[s, non_na_idx[1]] <- AC[s] * LastError[s] + 
      LogResid[s, non_na_idx[1]] * scale[s]
    
 
    # Apply AR(1) to remaining non-NA values
    if (length(non_na_idx) > 1) {
      for (t in 2:length(non_na_idx)) {
        i <- non_na_idx[t]
        prev <- non_na_idx[t - 1]
        LogResid[s, i] <- AC[s] * LogResid[s, prev] + LogResid[s, i] * scale[s]
      }
    }
  }
  
  LogResid
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
#' lag-1 autocorrelation between *consecutive observations* rather than
#' consecutive time steps. For an index observed in a single season, that is
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
    
    # sorted so the retained observations stay in time order; concatenating the
    # per-season sequences would group them by season and collapse AC toward
    # AC^nSeasons
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


