
#' Generate (log-space) residuals with truncated log-normal distribution
#'
#' Generate random residuals for multiple simulations and years.
#' Each simulation has a standard deviation `SD` and autocorrelation `AC`, 
#' and residuals are truncated to `TruncSD` standard deviations around the mean.
#' The resulting residuals can be propagated with autocorrelation using [ApplyAC()].
#'
#' @param SD Numeric vector of length nSim giving standard deviation of residuals.
#' @param AC Numeric vector of length nSim giving autocorrelation (must be in [-1,1]).
#' @param Years Numeric vector of years for which residuals are generated.
#' @param nSim Optional integer number of simulations (defaults to length(SD)).
#' @param TruncSD Numeric scalar specifying how many SDs to truncate the residuals.
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
  
  if (any(abs(AC) > 1)) 
    cli::cli_abort("`AC` must be in [-1, 1]")
  
  if (!is.numeric(TruncSD) || length(TruncSD) != 1 || TruncSD <= 0) 
    cli::cli_abort("`TruncSD` must be a positive scalar")
  
  if (is.null(nSim)) nSim <- length(SD)
  
  nYear <- length(Years)
  
  # Calculate mean for truncated log-normal
  mu <- -0.5 * SD^2 * (1 - AC) / sqrt(1 - AC^2)
  lower <- mu - TruncSD * SD
  upper <- mu + TruncSD * SD
  
  # Expand mu, SD, lower, upper for each year
  mu_mat <- matrix(mu, nrow = nSim, ncol = nYear)
  SD_mat <- matrix(SD, nrow = nSim, ncol = nYear)
  lower_mat <- matrix(lower, nrow = nSim, ncol = nYear)
  upper_mat <- matrix(upper, nrow = nSim, ncol = nYear)
  
  # Generate residuals
  LogResid <- rtnorm(nSim * nYear, 
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
    NA_ind <- expand_seasons(NA_seas = NA_Season[[s]], 
                             nSeasons = nSeasons, 
                             nYear = nYear)
    
    arr[s,NA_ind] <- NA
  }
  arr
}

expand_seasons <- function(NA_seas, nSeasons, nYear) {
  rep(seq(0, nYear / nSeasons - 1) * nSeasons, each = length(NA_seas)) + rep(NA_seas, times = nYear / nSeasons)
}

#' Apply AR(1) autocorrelation to residuals, skipping NAs
#'
#' Applies AR(1) propagation to a numeric matrix of residuals (`sim x year`), 
#' respecting missing values (NAs). Autocorrelation is applied only across 
#' non-NA values within each simulation.
#'
#' @param LogResid Numeric matrix of log residuals, dimensions nSim x nYear.
#' @param AC Numeric vector of length nSim, autocorrelation coefficient per simulation (must be in [-1,1]).
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
#' Contiguous NA values are handled by splitting into separate groups 
#' for autocorrelation calculation within each simulation. 
#' Seasons with all NA values across all years are dropped.
#'
#' @param LogResiduals A numeric matrix or array with dimensions `sim x year`.
#' @param nSeasons Number of seasons in the array (default 1).
#' @return A data.frame with columns:
#' * `AC`: weighted lag-1 autocorrelation of residuals
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
    
    valid_seasons <- unlist(lapply(which(seasons_to_keep), function(season) {
      seq(season, nYear, by = nSeasons)
    }))
    
    res <- res[valid_seasons]

    # Compute autocorrelation for each contiguous block of non-NA values
    non_na_idx <- which(!is.na(res))
    if (length(non_na_idx) <= 1) {
      AC[s] <- NA_real_
    } else {
      AC[s] <- acf(res[non_na_idx], plot = FALSE)$acf[2]
      AC[s] <- max(AC[s], 0)
    }
    
    SD[s] <- sd(res, na.rm = TRUE)
  }
  
  data.frame(Sim = seq_len(nSim), AC = AC, SD = SD, NA_Season=I(NA_Season))
}



