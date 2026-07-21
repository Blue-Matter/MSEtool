#' Generate Stochastic Stock Targeting Trajectories
#'
#' Simulates stochastic stock-specific targeting multipliers for each fleet
#' using independent multivariate lognormal draws at each time step.
#'
#' @param OM An operating model object ([om-class]) containing a populated
#'   `@StockTargeting` slot with elements:
#'   - `@Mean`: geometric mean targeting weight on the natural scale (from
#'     [FitStockTargeting()] or user defined)
#'   - `@Covariance`: stationary covariance matrix of log-targeting deviations
#'
#' @param Period Character string specifying which period to simulate:
#'   - `"Historical"`: assigns or replaces historical targeting values
#'   - `"Projection"`: appends simulated projection years
#' @param TruncSD `numeric(1)`. Number of standard deviations beyond which
#'   log-deviations are truncated. Prevents extreme targeting values when
#'   variance is high relative to the historical record length. Default `2`.
#'   Set to `Inf` to disable truncation.
#' @param n_recent `integer(1)`. For `Period = "Projection"` only: number of
#'   recent historical years used to determine whether a stock has been
#'   continuously inactive at the end of history. A stock with zero targeting
#'   weight in all `n_recent` years is treated as inactive and receives
#'   \eqn{T_{s,t} = 0} throughout the projection. Default `5`.
#' @param seed `numeric(1)`. Offset added to `OM@Seed` for the random number
#'   generator, ensuring independence from other stochastic processes.
#'   Default `101`.
#'
#' @return The input `OM` object with updated `@StockTargeting@Targeting`.
#'
#' Targeting is modelled in log-space as multivariate normal deviations at
#' each time step, centred at the bias-corrected historical log-mean:
#'
#' \deqn{
#'   \boldsymbol{\delta}_t \sim \mathrm{MVN}(\mathbf{0},\, \Sigma_w)
#' }
#'
#' \deqn{
#'   T_{s,t} = \exp\!\left(\mu_s^* + \delta_{s,t}\right)
#' }
#'
#' where:
#' \itemize{
#'   \item \eqn{T_{s,t}} is the targeting multiplier for stock \eqn{s} at
#'     time \eqn{t}
#'   \item \eqn{\delta_{s,t}} is the log-space deviation for stock \eqn{s}
#'     at time \eqn{t}, drawn from the multivariate normal with mean zero and
#'     covariance \eqn{\Sigma_w}
#'   \item \eqn{\Sigma_w} is the stationary covariance matrix of
#'     log-targeting deviations estimated by [FitStockTargeting()]
#'   \item \eqn{\mu_s^* = \bar{w}_s - \tfrac{1}{2}\sigma^2_{w,s}} is the
#'     lognormal bias correction for stock \eqn{s}, where \eqn{\bar{w}_s =
#'     \log(\texttt{Mean}_s)} is the fitted log-scale mean from
#'     [FitStockTargeting()] and \eqn{\sigma^2_{w,s} = (\Sigma_w)_{ss}} is
#'     the marginal variance. This ensures \eqn{\mathrm{E}[T_{s,t}] =
#'     \exp(\bar{w}_s)}, preserving the historical mean targeting level for
#'     each stock on the natural scale.
#' }
#'
#' Deviations are truncated to
#' \eqn{|\delta_{s,t}| \le \texttt{TruncSD} \times \sigma_{w,s}}
#' to guard against extreme values when variance is poorly estimated from
#' short historical series. When variance is low, truncation has little effect
#' and simulated values stay close to the historical mean, as intended.
#'
#' Inactive stocks (zero marginal variance or non-finite mean in
#' `@StockTargeting@Mean`, or continuously inactive over the last `n_recent`
#' historical years when `Period = "Projection"`) receive \eqn{T_{s,t} = 0}
#' throughout.
#'
#' @examples
#' \dontrun{
#' OM <- GenerateStockTargeting(OM, Period = "Projection")
#' OM <- GenerateStockTargeting(OM, Period = "Projection", TruncSD = 2, n_recent = 5)
#' }
#'
#' @seealso [FitStockTargeting()], `.StandardizeEffort()`
#'
#' @export
GenerateStockTargeting <- function(OM,
                                   Period = c('Historical', 'Projection'),
                                   TruncSD = 2,
                                   n_recent = 5,
                                   seed = 101) {
  
  CheckPackage('MASS')
  
  Period <- match.arg(Period)
  .SetSeed(OM@Seed + seed)
  
  years   <- Years(OM, Period)
  n_years <- length(years)
  n_stock <- nStock(OM)
  
  if (n_stock < 2) return(OM)
  
  n_sim   <- nSim(OM)
  n_fleet <- nFleet(OM)
  
  stock_names <- StockNames(OM)
  fleet_names <- FleetNames(OM)
  
  STarget <- OM@StockTargeting
  
  if (all(years %in% dimnames(STarget@Targeting)$Year))
    return(OM)
  
  log_mu_arr <- Extend(log(STarget@Mean), nSim = n_sim)  # log-scale mean [sim, stock, fleet]
  cov_arr    <- Extend(STarget@Covariance, nSim = n_sim) # [sim, stock, stock, fleet]
  
  TargValues <- array(NA_real_,
                      dim = c(n_sim, n_stock, n_fleet, n_years),
                      dimnames = list(Sim   = seq_len(n_sim),
                                      Stock = stock_names,
                                      Fleet = fleet_names,
                                      Year  = years))
  
  for (sim in seq_len(n_sim)) {
    for (fl in seq_len(n_fleet)) {
      
      log_mean       <- log_mu_arr[sim, , fl]
      stationary_cov <- cov_arr[sim, , , fl]
      
      marginal_var <- diag(stationary_cov)
      marginal_sd  <- sqrt(pmax(marginal_var, 0))
      max_dev      <- TruncSD * marginal_sd
      
      log_mean_bc <- log_mean - 0.5 * pmax(marginal_var, 0)
      
      # Inactive: zero variance or non-finite mean in the fitted targeting
      inactive <- !is.finite(log_mean) | marginal_var == 0
      
      # Additionally suppress stocks with zero targeting weight across all
      # of the last n_recent historical years
      if (Period == 'Projection') {
        hist_targ   <- STarget@Targeting
        n_hist      <- dim(hist_targ)[4]
        recent_idx  <- seq(max(1L, n_hist - n_recent + 1L), n_hist)
        recent_targ <- hist_targ[sim, , fl, recent_idx, drop = FALSE]
        recently_inactive <- apply(recent_targ, 2,
                                   function(x) all(!is.finite(x) | x <= 0))
        inactive <- inactive | recently_inactive
      }
      
      active     <- which(!inactive)
      cov_active <- stationary_cov[active, active, drop = FALSE]
      eps        <- rep(0, n_stock)
      
      for (t in seq_len(n_years)) {
        
        if (length(active) > 0 && any(abs(cov_active) > 1e-12)) {
          eps[active] <- as.numeric(
            MASS::mvrnorm(1, mu = rep(0, length(active)), Sigma = cov_active)
          )
        }
        
        dev_t           <- pmin(pmax(eps, -max_dev), max_dev)
        dev_t[inactive] <- 0
        
        T_t           <- exp(log_mean_bc + dev_t)
        T_t[inactive] <- 0
        
        TargValues[sim, , fl, t] <- T_t
      }
    }
  }
  
  if (Period == 'Historical') {
    OM@StockTargeting@Targeting <- TargValues
  } else {
    OM@StockTargeting@Targeting <- abind::abind(
      OM@StockTargeting@Targeting, TargValues, use.dnns = TRUE
    )
  }
  
  OM
}
