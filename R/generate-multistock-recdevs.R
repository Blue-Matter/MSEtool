#' Generate Correlated Multi-Stock Recruitment Deviations
#'
#' Generates stochastic recruitment deviations for multiple stocks using a
#' multivariate lognormal AR(1) process, preserving both cross-stock
#' covariance and temporal autocorrelation.
#'
#' @param OM An operating model object of class [om-class].
#' @param TruncSD Numeric scalar. Number of standard deviations at which to
#'   truncate the innovation distribution. Defaults to `2`.
#' @param silent `logical(1)` Display messages?
#'
#' @return The input `OM` object with `RecDevProj` populated for all stocks
#'   and simulations.
#'
#' @details
#' 
#' Deviations are simulated in log-space:
#'
#' \deqn{
#'   Z_t = \mu + \Phi (Z_{t-1} - \mu) + \epsilon_t
#' }
#'
#' \deqn{
#'   R_t = \exp(Z_t)
#' }
#'
#' where:
#' - \eqn{\mu} is a bias-corrected mean
#' - \eqn{\Phi} is a diagonal matrix of lag-1 autocorrelation coefficients
#' - \eqn{\epsilon_t \sim MVN(0, \Sigma_\epsilon)}
#'
#' The innovation covariance matrix \eqn{\Sigma_\epsilon} is derived from the
#' stationary covariance \eqn{\Sigma_Z} using [CalcInnovationCov()].
#' 
#' The function proceeds as follows for each simulation:
#'
#' 1. **Extract stock-specific parameters** (`sd`, `ac`, `mu`)
#' 2. **Impute missing historical deviations** using univariate AR(1)
#' 3. **Estimate stationary covariance** \eqn{\Sigma_Z} from historical log deviations
#' 4. **Compute innovation covariance** via [CalcInnovationCov()]
#' 5. **Simulate projection deviations** using a multivariate AR(1) process
#'
#' Truncation is applied to the innovation term \eqn{\epsilon_t}, not the
#' deviations themselves.
#'
#' @seealso [CalcInnovationCov()], `.AddAutoCorrelation()`
#' @export
GenMultiStockRecDevs <- function(OM, TruncSD = 2, silent = FALSE) {
  
  CheckPackage('tmvtnorm')
  
  .CheckClass(OM)
  set.seed(OM@Seed)
  
  nSim      <- OM@nSim
  n_stock   <- nStock(OM)
  ProjYears <- Years(OM, "P")
  pYear     <- length(ProjYears)
  
  if (n_stock < 2) return(OM)
  
  pb <- if (!silent) {
    cli::cli_progress_bar(
      name = "Simulating correlated recruitment deviations for projection period",
      total = nSim,
      clear = TRUE
    )
  }
  
  for (sim in seq_len(nSim)) {
    
    if (!silent) cli::cli_progress_update(pb, set = sim)
    
    # Extract stats 
    RecDevStats <- purrr::map(OM@Stock, \(stock) 
                              .GetRecDevStats(stock@SRR, sim)
    ) |> dplyr::bind_rows() |> as.data.frame()
    
    # Extract historical deviations
    RecDevHist <- purrr::map(OM@Stock, \(stock) {
      recdevhist <- stock@SRR@RecDevHist
      if (dim(recdevhist)[1] < sim) return(recdevhist[1, ])
      recdevhist[sim, ]
    }) |> dplyr::bind_rows() |> as.data.frame()
    
    # Impute missing historical values (univariate AR1) 
    for (st in seq_len(n_stock)) {
      na_ind <- which(is.na(RecDevHist[st, ]))
      if (!length(na_ind)) next
      
      last_ind <- na_ind[1] - 1
      z_prev   <- log(as.numeric(RecDevHist[st, last_ind]))
      
      for (k in seq_along(na_ind)) {
        eps <- rnorm(1, 0, RecDevStats$sd[st] * sqrt(1 - RecDevStats$ac[st]^2))
        z_t <- RecDevStats$mu[st] +
          RecDevStats$ac[st] * (z_prev - RecDevStats$mu[st]) +
          eps
        
        RecDevHist[st, na_ind[k]] <- exp(z_t)
        z_prev <- z_t
      }
    }
    
    # Estimate covariance
    ignore_cols <- apply(RecDevHist, 2, function(col) any(col == 1 | is.na(col)))
    use_cols    <- which(!ignore_cols)
    
    log_hist <- log(RecDevHist[, use_cols, drop = FALSE])
    Sigma_Z  <- stats::cov(t(log_hist))
    
    if (any(eigen(Sigma_Z)$values < 0)) {
      Sigma_Z <- as.matrix(Matrix::nearPD(Sigma_Z)$mat)
    }
    
    # Build AR(1) components 
    mu  <- RecDevStats$mu
    phi <- RecDevStats$ac
    
    Sigma_eps <- CalcInnovationCov(Sigma_Z, phi)
    
    lower <- -TruncSD * RecDevStats$sd
    upper <-  TruncSD * RecDevStats$sd
    
    # Initialize state 
    Z_prev <- log(as.numeric(RecDevHist[, ncol(RecDevHist)]))
    Z_prev[!is.finite(Z_prev)] <- mu[!is.finite(Z_prev)]
    
    Z_proj <- matrix(NA_real_, nrow = n_stock, ncol = pYear)
    
    # Sample values
    if (all(abs(Sigma_eps) < 1e-12)) {
      eps_mat <- matrix(0, nrow = pYear, ncol = n_stock)
    } else {
      if (TruncSD > 5) {
        eps_mat <- MASS::mvrnorm(pYear, mu = rep(0, n_stock), Sigma = Sigma_eps)
      } else {
        
        Sigma_eps
        eig <- eigen(Sigma_eps, symmetric = TRUE)
        if (any(eig$values <= 1e-10)) {
          Sigma_eps <- Sigma_eps + diag(1e-8, nrow(Sigma_eps))
        }
        
        
        eps_mat <- tmvtnorm::rtmvnorm(
          n     = pYear,
          mean  = rep(0, n_stock),
          sigma = Sigma_eps,
          lower = lower,
          upper = upper
        )
      }
    }
    
    # Add AR(1) 
    for (t in seq_len(pYear)) {
      eps <- eps_mat[t, ]
      
      Z_t <- mu + phi * (Z_prev - mu) + eps
      
      Z_proj[, t] <- Z_t
      Z_prev <- Z_t
    }
    
    
    for (st in seq_len(n_stock)) {
      OM@Stock[[st]]@SRR@RecDevProj[sim, ] <- exp(Z_proj[st, ])
    }
    
  } # end sim loop
  cli::cli_progress_done() 
  OM
}


.GetRecDevStats <- function(SRR, sim = 1) {
  if (!is.finite(SRR@SD))
    cli::cli_abort("Non-finite SRR@SD", .internal = TRUE)
  
  if (!is.finite(SRR@AC))
    cli::cli_abort("Non-finite SRR@AC", .internal = TRUE)
  
  sd <- ifelse(dim(SRR@SD)[1] < sim, SRR@SD[1, 1], SRR@SD[sim, 1]) |> as.numeric()
  ac <- ifelse(dim(SRR@AC)[1] < sim, SRR@AC[1, 1], SRR@AC[sim, 1]) |> as.numeric()
  
  mu <- -0.5 * sd^2 * (1 - ac) / sqrt(1 - ac^2)
  c(sd = sd, ac = ac, mu = mu)
}


                          


