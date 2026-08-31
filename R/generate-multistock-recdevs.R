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
#' @param overwrite `logical`, length `nStock(OM)`, or `NULL` (default).
#'   Controls which stocks' `RecDevProj` are written. `NULL` overwrites every
#'   stock. Stocks with `overwrite = FALSE` keep their existing `RecDevProj`
#'   (e.g. user-supplied values) untouched, but their historical deviations
#'   still contribute to the estimated cross-stock covariance.
#'
#' @return The input `OM` object with `RecDevProj` populated for all stocks
#'   and simulations (subject to `overwrite`).
#'
#' @details
#'
#' Within each simulation, stocks whose historical deviations
#' (`SRR@RecDevHist[sim, ]`) are byte-identical are treated as a single
#' group: one latent series is simulated for the group and copied to every
#' member, so linked stocks (e.g. a female/male pair sharing recruitment)
#' stay identical in the projection period rather than drifting apart under
#' independent stochastic draws. Remaining stocks/groups are correlated, not
#' forced identical, via the process below.
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
#'
#' @seealso [GenRecDevs()], [CalcInnovationCov()]
#' @export
GenMultiStockRecDevs <- function(OM, TruncSD = 3, silent = FALSE, overwrite = NULL) {

  CheckPackage('MASS')

  .CheckClass(OM)
  set.seed(OM@Seed)

  nSim      <- OM@nSim
  n_stock   <- nStock(OM)
  ProjYears <- Years(OM, "P")
  pYear     <- length(ProjYears)
  nSeason   <- max(1L, as.integer(OM@Seasons %||% 1))

  if (n_stock < 2) return(OM)

  if (is.null(overwrite)) overwrite <- rep(TRUE, n_stock)
  if (length(overwrite) != n_stock)
    cli::cli_abort("`overwrite` must be length `nStock(OM)` ({n_stock})", .internal = TRUE)

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

    grp     <- .IdenticalRecDevGroups(RecDevHist)
    rep_idx <- match(unique(grp), grp)
    n_grp   <- length(rep_idx)

    RecDevStatsGrp <- RecDevStats[rep_idx, , drop = FALSE]
    RecDevHistGrp  <- RecDevHist[rep_idx, , drop = FALSE]
    nHistTS        <- ncol(RecDevHistGrp)

    active_phase <- matrix(TRUE, nrow = n_grp, ncol = nSeason)
    for (g in seq_len(n_grp)) {
      for (p in seq_len(nSeason)) {
        cols  <- seq(p, nHistTS, by = nSeason)
        known <- as.numeric(RecDevHistGrp[g, cols])
        known <- known[!is.na(known)]
        if (length(known) && all(known == 0)) active_phase[g, p] <- FALSE
      }
    }
    hist_phase <- ((seq_len(nHistTS) - 1) %% nSeason) + 1
    proj_phase <- ((nHistTS + seq_len(pYear) - 1) %% nSeason) + 1
    ActiveHist <- active_phase[, hist_phase, drop = FALSE]
    ActiveProj <- active_phase[, proj_phase, drop = FALSE]

    for (g in seq_len(n_grp)) {
      inactive_na <- which(!ActiveHist[g, ] & is.na(RecDevHistGrp[g, ]))
      if (length(inactive_na)) RecDevHistGrp[g, inactive_na] <- 0

      active_cols <- which(ActiveHist[g, ])
      na_ind <- active_cols[is.na(RecDevHistGrp[g, active_cols])]
      if (!length(na_ind)) next

      sd_st <- RecDevStatsGrp$sd[g]
      ac_st <- RecDevStatsGrp$ac[g]

      prior_active <- active_cols[active_cols < na_ind[1]]
      zl_prev <- if (length(prior_active)) {
        .DevToLatent(log(as.numeric(RecDevHistGrp[g, max(prior_active)])),
                     sd_st, TruncSD)
      } else 0

      for (k in seq_along(na_ind)) {
        zl_prev <- ac_st * zl_prev + rnorm(1) * sqrt(1 - ac_st^2)
        RecDevHistGrp[g, na_ind[k]] <- exp(.LatentToDev(zl_prev, sd_st, TruncSD))
      }
    }

    ignore_cols <- vapply(seq_len(nHistTS), function(j) {
      col <- as.numeric(RecDevHistGrp[, j])
      any(col == 1 | is.na(col)) || any(!ActiveHist[, j])
    }, logical(1))
    use_cols <- which(!ignore_cols)

    log_hist <- log(RecDevHistGrp[, use_cols, drop = FALSE])
    Sigma_Z  <- stats::cov(t(log_hist))

    if (any(eigen(Sigma_Z)$values < 0)) {
      Sigma_Z <- as.matrix(Matrix::nearPD(Sigma_Z)$mat)
    }

    phi      <- RecDevStatsGrp$ac
    sd_marg  <- sqrt(diag(as.matrix(Sigma_Z)))
    active   <- sd_marg > 0

    R_latent <- matrix(0, n_grp, n_grp)
    if (any(active))
      R_latent[active, active] <- stats::cov2cor(Sigma_Z[active, active, drop = FALSE])

    Sigma_eps <- CalcInnovationCov(R_latent, phi)

    Zl_prev <- vapply(seq_len(n_grp), function(g) {
      active_cols <- which(ActiveHist[g, ])
      if (!length(active_cols)) return(0)
      z_last <- log(as.numeric(RecDevHistGrp[g, max(active_cols)]))
      if (!is.finite(z_last)) return(0)
      .DevToLatent(z_last, sd_marg[g], TruncSD)
    }, numeric(1))

    Zl_proj <- matrix(NA_real_, nrow = n_grp, ncol = pYear)

    # Sample latent innovations
    if (all(abs(Sigma_eps) < 1e-12)) {
      eps_mat <- matrix(0, nrow = pYear, ncol = n_grp)
    } else {
      eig <- eigen(Sigma_eps, symmetric = TRUE)
      if (any(eig$values <= 1e-10))
        Sigma_eps <- Sigma_eps + diag(1e-8, nrow(Sigma_eps))

      eps_mat <- MASS::mvrnorm(pYear, mu = rep(0, n_grp), Sigma = Sigma_eps)
      if (n_grp == 1) eps_mat <- matrix(eps_mat, ncol = 1)
    }

    for (t in seq_len(pYear)) {
      active_t <- ActiveProj[, t]
      Zl_prev[active_t] <- phi[active_t] * Zl_prev[active_t] + eps_mat[t, active_t]
      Zl_proj[, t] <- Zl_prev
    }

    for (g in seq_len(n_grp)) {
      proj_dev  <- rep(0, pYear)
      active_t  <- ActiveProj[g, ]
      proj_dev[active_t] <- exp(.LatentToDev(Zl_proj[g, active_t], sd_marg[g], TruncSD))
      members  <- which(grp == grp[rep_idx[g]])
      for (st in members) {
        if (!overwrite[st]) next
        OM@Stock[[st]]@SRR@RecDevProj[sim, ] <- proj_dev
      }
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

.IdenticalRecDevGroups <- function(RecDevHist) {
  n   <- nrow(RecDevHist)
  grp <- seq_len(n)
  if (n < 2) return(grp)

  for (i in seq_len(n - 1)) {
    if (anyNA(RecDevHist[i, ])) next
    for (j in seq(i + 1, n)) {
      if (grp[j] == grp[i] || anyNA(RecDevHist[j, ])) next
      if (identical(as.numeric(RecDevHist[i, ]), as.numeric(RecDevHist[j, ])))
        grp[j] <- grp[i]
    }
  }
  grp
}


