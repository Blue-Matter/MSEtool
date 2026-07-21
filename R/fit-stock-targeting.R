#' Fit statistical properties of historical stock targeting
#'
#' Estimates the covariance of log-targeting deviations from historical
#' targeting weights stored in `OM@StockTargeting@Targeting`, as produced
#' by `.StandardizeEffort()`.
#'
#' For each simulation, fleet, and year, the fleet-level log-effort is the
#' geometric mean of log-targeting weights over **active stocks at that time
#' step** (those with positive targeting weight):
#'
#' \deqn{
#'   \log \hat{E}_{f,t} = \frac{1}{|\mathcal{A}_{f,t}|}
#'     \sum_{s \in \mathcal{A}_{f,t}} \log T_{s,f,t}, \quad
#'   \mathcal{A}_{f,t} = \{s : T_{s,f,t} > 0\}
#' }
#'
#' Stock targeting deviations are the multiplicative departures from this
#' fleet mean:
#'
#' \deqn{
#'   \tilde{w}_{s,f,t} = \log T_{s,f,t} - \log \hat{E}_{f,t}
#' }
#'
#' The stationary covariance \eqn{\Sigma_f} is estimated from
#' \eqn{\tilde{w}_{s,f,t}} across historical years for stocks that are
#' consistently active (positive targeting in strictly more than
#' `active_thresh` proportion of years). Element \eqn{(\Sigma_f)_{s,s'}}
#' captures the co-variation in log-targeting deviations between stocks
#' \eqn{s} and \eqn{s'} for fleet \eqn{f}.
#'
#' The function returns `OM` unchanged if `@Mean` and `@Covariance` are
#' already fully populated, or if `nStock == 1`.
#'
#' @param OM An operating model object ([om-class]) with a populated
#'   `@StockTargeting@Targeting` array of dimension
#'   `[nSim, nStock, nFleet, nYearHist]`, as produced by
#'   `.StandardizeEffort()`.
#' @param tol `numeric(1)`. Tolerance below which targeting values are treated
#'   as zero for the purpose of covariance estimation. Default `1e-6`.
#' @param active_thresh `numeric(1)`. Minimum proportion of historical years
#'   with positive targeting weight for a stock-fleet combination to be
#'   included in covariance estimation (strictly greater than this value).
#'   Default `0.1`.
#'
#' @return The input `OM` object with the following slots populated:
#'   - `@StockTargeting@Mean`: temporal mean of log-targeting deviations,
#'     exponentiated to the natural scale. Array of dimension
#'     `[nSim, nStock, nFleet]`. Values are approximately 1 for active stocks
#'     (close to zero on the log scale by the sum-to-zero construction of
#'     `.StandardizeEffort()`, but not guaranteed to be exactly 1) and exactly
#'     1 for inactive stocks (neutral, log-deviation fixed at zero).
#'   - `@StockTargeting@Covariance`: stationary covariance matrix of
#'     log-targeting deviations, array of dimension
#'     `[nSim, nStock, nStock, nFleet]`. Rows and columns for inactive stocks
#'     are zero.
#'
#' @seealso `.StandardizeEffort()`, [GenerateStockTargeting()]
#' @export
FitStockTargeting <- function(OM, tol = 1e-6, active_thresh = 0.1) {
  
  if (nStock(OM) == 1)
    return(OM)
  
  HistYears   <- Years(OM, 'H')
  STarget     <- OM@StockTargeting
  Targ        <- STarget@Targeting |> Subset(Years = HistYears)
  
  if (!all(is.na(STarget@Mean)) && !all(is.na(STarget@Covariance)))
    return(OM)
  
  nSim        <- nSim(OM)
  n_stock     <- nStock(OM)
  n_fleet     <- nFleet(OM)
  nYear       <- length(HistYears)
  stock_names <- StockNames(OM)
  fleet_names <- FleetNames(OM)
  
  mu_arr <- array(NA_real_,
                  dim      = c(nSim, n_stock, n_fleet),
                  dimnames = list(Sim   = seq_len(nSim),
                                  Stock = stock_names,
                                  Fleet = fleet_names))
  
  cov_arr <- array(NA_real_,
                   dim      = c(nSim, n_stock, n_stock, n_fleet),
                   dimnames = list(Sim   = seq_len(nSim),
                                   Stock = stock_names,
                                   Stock = stock_names,
                                   Fleet = fleet_names))
  
  for (sim in seq_len(nSim)) {
    for (fl in seq_len(n_fleet)) {
      dd <- dim(Targ)
      
      targ_mat <- t(Targ[min(c(dd[1], sim)), , fl, ])  # [nYear x nStock]
      
      # Active stocks for covariance estimation:
      # positive targeting in strictly more than active_thresh proportion of years.
      prop_active <- colMeans(targ_mat > 0, na.rm = TRUE)
      active_idx  <- which(prop_active > active_thresh)
      
      if (length(active_idx) == 0) {
        mu_arr[sim, , fl]    <- 1
        cov_arr[sim, , , fl] <- diag(n_stock)
        next
      }
      
      # Log-targeting for active stocks: [nYear x nActiveStock]
      # NA where targeting is zero (inactive at that time step)
      log_targ <- matrix(NA_real_, nrow = nYear, ncol = length(active_idx),
                         dimnames = list(Year  = HistYears,
                                         Stock = stock_names[active_idx]))
      
      for (j in seq_along(active_idx)) {
        x <- targ_mat[, active_idx[j]]
        x[x <= 0] <- NA_real_
        log_targ[, j] <- log(x)
      }
      
      # Fleet-level log-effort: geometric mean of log-targeting over active
      # stocks at each time step. 
      log_E_f_t <- rowMeans(log_targ, na.rm = TRUE)
      
      # Log-deviations: log(delta_{s,f,t}) = log(T_{s,f,t}) - log(E_{f,t})
      # Sum to zero over active stocks at each time step by construction
      dev_mat <- sweep(log_targ, 1, log_E_f_t, FUN = "-")
      
      # Temporal mean of log-deviations 
      mu_active <- colMeans(dev_mat, na.rm = TRUE)
      
      # Covariance across time
      complete_rows <- stats::complete.cases(dev_mat)
      if (sum(complete_rows) < 2) {
        cli::cli_alert_warning(
          "Insufficient data for Fleet {.val {fleet_names[fl]}}, \\
          simulation {sim}. Using identity covariance."
        )
        Sigma_active <- diag(length(active_idx))
      } else {
        Sigma_active <- stats::cov(dev_mat[complete_rows, , drop = FALSE])
      }
      
      mu_full <- rep(0, n_stock)
      mu_full[active_idx] <- mu_active
      
      Sigma_full <- matrix(0, n_stock, n_stock)
      Sigma_full[active_idx, active_idx] <- Sigma_active
      
      mu_arr[sim, , fl]    <- mu_full
      cov_arr[sim, , , fl] <- Sigma_full
    }
  }
  
  OM@StockTargeting@Mean       <- exp(ReduceDims(mu_arr))
  OM@StockTargeting@Covariance <- ReduceDims(cov_arr)
  
  OM
}
