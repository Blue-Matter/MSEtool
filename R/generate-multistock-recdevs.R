#' Generate Correlated Multi-Stock Recruitment Deviations
#'
#' Generates stochastic recruitment deviations for multiple stocks using a
#' multivariate lognormal AR(1) process, preserving both cross-stock
#' covariance and temporal autocorrelation.
#'
#' @param OM An operating model object of class [om-class].
#' @param TruncSD Numeric scalar. Number of standard deviations at which the
#'   log deviations are bounded, as in [GenRecDevs()]. Defaults to `3`.
#' @param silent `logical(1)` Display messages?
#'
#' @return The input `OM` object with `RecDevProj` populated for all stocks
#'   and simulations.
#'
#' @details
#'
#' The AR(1) process runs on a unit-variance latent scale that carries the
#' cross-stock correlation:
#'
#' \deqn{
#'   Z_t = \Phi Z_{t-1} + \epsilon_t, \quad
#'   \epsilon_t \sim MVN(0, \Sigma_\epsilon)
#' }
#'
#' where \eqn{\Phi} is a diagonal matrix of lag-1 autocorrelation coefficients
#' and \eqn{\Sigma_\epsilon} is derived by [CalcInnovationCov()] from the
#' cross-stock correlation matrix.
#'
#' Each stock's latent series is then mapped onto a truncated normal marginal
#' via the probability integral transform, scaled to that stock's standard
#' deviation, and bias-corrected so that mean recruitment multiplier is 1.
#' This is the same transform [GenRecDevs()] applies, so bounded and
#' correlated deviations follow the same convention. 
#'
#' The function proceeds as follows for each simulation:
#'
#' 1. **Extract stock-specific parameters** (`sd`, `ac`)
#' 2. **Impute missing historical deviations** using univariate AR(1)
#' 3. **Estimate stationary covariance** \eqn{\Sigma_Z} from historical log deviations
#' 4. **Compute latent innovation covariance** via [CalcInnovationCov()]
#' 5. **Simulate projection deviations** using a multivariate AR(1) process
#'
#' @seealso [GenRecDevs()], [CalcInnovationCov()]
#' @export
GenMultiStockRecDevs <- function(OM, TruncSD = 3, silent = FALSE) {
  
  CheckPackage('MASS')

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
    
    # Impute missing historical values (univariate AR1 in latent space)
    for (st in seq_len(n_stock)) {
      na_ind <- which(is.na(RecDevHist[st, ]))
      if (!length(na_ind)) next

      sd_st <- RecDevStats$sd[st]
      ac_st <- RecDevStats$ac[st]

      last_ind <- na_ind[1] - 1
      zl_prev  <- .DevToLatent(log(as.numeric(RecDevHist[st, last_ind])),
                               sd_st, TruncSD)

      for (k in seq_along(na_ind)) {
        zl_prev <- ac_st * zl_prev + rnorm(1) * sqrt(1 - ac_st^2)
        RecDevHist[st, na_ind[k]] <- exp(.LatentToDev(zl_prev, sd_st, TruncSD))
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
    
    # Build AR(1) components. The AR(1) runs on a unit-variance latent scale
    # carrying the cross-stock correlation; per-stock marginals are imposed
    # afterwards by .LatentToDev(), which bounds them and removes lognormal bias.
    phi      <- RecDevStats$ac
    sd_marg  <- sqrt(diag(as.matrix(Sigma_Z)))
    active   <- sd_marg > 0

    R_latent <- matrix(0, n_stock, n_stock)
    if (any(active))
      R_latent[active, active] <- stats::cov2cor(Sigma_Z[active, active, drop = FALSE])

    Sigma_eps <- CalcInnovationCov(R_latent, phi)

    # Initialize latent state from the last historical deviation
    Z_last <- log(as.numeric(RecDevHist[, ncol(RecDevHist)]))
    Zl_prev <- vapply(seq_len(n_stock), function(st) {
      if (!is.finite(Z_last[st])) return(0)
      .DevToLatent(Z_last[st], sd_marg[st], TruncSD)
    }, numeric(1))

    Zl_proj <- matrix(NA_real_, nrow = n_stock, ncol = pYear)

    # Sample latent innovations
    if (all(abs(Sigma_eps) < 1e-12)) {
      eps_mat <- matrix(0, nrow = pYear, ncol = n_stock)
    } else {
      eig <- eigen(Sigma_eps, symmetric = TRUE)
      if (any(eig$values <= 1e-10))
        Sigma_eps <- Sigma_eps + diag(1e-8, nrow(Sigma_eps))

      eps_mat <- MASS::mvrnorm(pYear, mu = rep(0, n_stock), Sigma = Sigma_eps)
      if (n_stock == 1) eps_mat <- matrix(eps_mat, ncol = 1)
    }

    # Add AR(1) on the latent scale
    for (t in seq_len(pYear)) {
      Zl_prev      <- phi * Zl_prev + eps_mat[t, ]
      Zl_proj[, t] <- Zl_prev
    }

    for (st in seq_len(n_stock)) {
      OM@Stock[[st]]@SRR@RecDevProj[sim, ] <-
        exp(.LatentToDev(Zl_proj[st, ], sd_marg[st], TruncSD))
    }

  } # end sim loop
  cli::cli_progress_done() 
  OM
}


.GetRecDevStats <- function(SRR, sim = 1) {

  # SD/AC may be a [Sim, Year] array or a plain per-sim vector
  pick <- function(x, nm) {
    if (!length(x) || any(!is.finite(x)))
      cli::cli_abort("Non-finite or empty {.code SRR@{nm}}", .internal = TRUE)
    d <- dim(x)
    as.numeric(if (is.null(d)) x[min(sim, length(x))] else x[min(sim, d[1]), 1])
  }

  c(sd = pick(SRR@SD, "SD"), ac = pick(SRR@AC, "AC"))
}


                          


