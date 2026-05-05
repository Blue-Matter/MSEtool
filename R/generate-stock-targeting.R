
#' Generate Stochastic Stock Targeting Trajectories
#'
#' Simulates stochastic stock-specific targeting multipliers for each fleet
#' using a multivariate lognormal AR(1) process.
#'
#'
#' @param OM An operating model object ([om-class]) containing a populated
#'   `@StockTargeting` slot with elements:
#'   - `@Mean` (log-scale mean)
#'   - `@AC` (lag-1 autocorrelation)
#'   - `@Covariance` (stationary covariance)
#'
#' @param Period Character string specifying which period to simulate:
#'   - `"Historical"`: assigns or replaces historical targeting values
#'   - `"Projection"`: appends simulated projection years
#'
#' @return The input `OM` object with updated `@StockTargeting@Targeting`
#'
#' Targeting is modeled in log-space to ensure positivity:
#'
#' \deqn{
#'   Z_t = \mu + \Phi (Z_{t-1} - \mu) + \epsilon_t
#' }
#'
#' \deqn{
#'   T_t = \exp(Z_t)
#' }
#'
#' where:
#' - \eqn{\mu} is the mean log-targeting
#' - \eqn{\Phi} is a diagonal matrix of lag-1 autocorrelation coefficients
#' - \eqn{\epsilon_t \sim MVN(0, \Sigma_\epsilon)}
#'
#' The innovation covariance matrix \eqn{\Sigma_\epsilon} is derived internally
#' from the stationary covariance matrix.
#' 
#' - Stocks that are inactive for a given fleet (e.g., dummy fleets with no
#'   fishing activity) are assigned neutral targeting values of 1.
#' - If the innovation covariance matrix is numerically zero, the process
#'   reduces to a deterministic AR(1).
#' - For projection simulations, the initial state is taken from the final
#'   historical year.
#'
#' @examples
#' \dontrun{
#' OM <- GenerateStockTargeting(OM, Period = "Projection")
#' }
#'
#' @seealso [CalcInnovationCov()]
#'
#' @export
GenerateStockTargeting <- function(OM, Period=c('Historical', 'Projection')) {
  
  SetSeed(OM@Seed + 100) # make sure it's different from seed used elsewhere
  
  Period <- match.arg(Period)
  years <- Years(OM, Period)
  n_years <- length(years)
  
  n_stock <- nStock(OM)
  if (n_stock < 2) return(OM)
  
  n_sim <- nSim(OM)
  n_fleet <- nFleet(OM)
  
  stock_names <- StockNames(OM)
  fleet_names <- FleetNames(OM)
  
  STarget  <- OM@StockTargeting
  mu_arr   <- STarget@Mean            # sim, stock, fleet
  phi_arr  <- STarget@AC              # sim, stock, fleet
  cov_arr  <- STarget@Covariance      # sim, stock, stock, fleet
  
  TargValues <- array(NA_real_,
                      dim = c(n_sim, n_stock, n_fleet, n_years),
                      dimnames = list(
                        Sim   = seq_len(n_sim),
                        Stock = stock_names,
                        Fleet = fleet_names,
                        Year  = years
                      ))
  
  for (sim in seq_len(n_sim)) {
    for (fl in seq_len(n_fleet)) {
      
      mu  <- mu_arr[sim, , fl]
      phi <- phi_arr[sim, , fl]
      Sigma_Z <- cov_arr[sim, , , fl]
      Sigma_eps <- CalcInnovationCov(Sigma_Z, phi)
      
      if (Period=='Historical') {
        Z_prev <- mu  
      } else {
        dd <- dim(OM@StockTargeting@Targeting)
        Z_prev <- log(OM@StockTargeting@Targeting[sim,,fl,dd[4]])
      }
      inactive <- !is.finite(Z_prev) | !is.finite(mu) | is.na(mu)
      
    
      for (t in seq_len(n_years)) {
        
        # generate values
        if (all(abs(Sigma_eps) < 1e-12)) {
          eps <- rep(0, n_stock)
        } else {
          eps <- as.numeric(MASS::mvrnorm(1, mu = rep(0, n_stock), Sigma = Sigma_eps))
        }
        
        # AR1
        Z_t <- mu + phi * (Z_prev - mu) + eps
        T_t <- exp(Z_t)
        
        T_t[inactive] <- 1   
        
        TargValues[sim, , fl, t] <- T_t
        Z_prev <- Z_t
      }
    }
  }
  
  if (Period=='Historical') {
    OM@StockTargeting@Targeting <- TargValues  
  } else {
    OM@StockTargeting@Targeting <- abind::abind(OM@StockTargeting@Targeting, TargValues, use.dnns = TRUE)  
  }
  
  OM
}


