#' Adjust Fecundity to Reflect Seasonal Spawning Activity
#'
#' Scales `Fecundity@MeanAtAge` for each [stock-class] in an [om-class] object
#' so that seasonal spawning production in model outputs is concentrated in the
#' same seasons as recruitment, matching the biological reality that fish
#' actively spawn only during certain months.
#'
#'
#' @param OM An [om-class] object. `Stock@SRR@R0` should already encode the
#'   seasonal recruitment pattern (e.g. via [SetSeasonalR0()]).
#' @param silent Logical. Suppress messages. Default `FALSE`.
#'
#' @details
#'
#' Seasonal recruitment is controlled entirely by `SRR@R0`: at unfished
#' equilibrium the stock-recruit relationship returns exactly `R0[m]` recruits
#' in season `m`, regardless of how fecundity is distributed across seasons.
#' Use [SetSeasonalR0()] to encode the target seasonal proportions into `R0`
#' before calling this function.
#'
#' What this function adds is biological realism in spawning output. Without
#' it, fecundity (and therefore reported spawning production) is spread uniformly
#' across all seasons even when recruitment is highly concentrated, a scenario
#' that is not biologically feasible. Fish that do not spawn in a given season
#' contribute no eggs regardless of their maturity, and if they did spawn but
#' consistently produced fewer recruits than expected, natural selection would
#' have eliminated that strategy over evolutionary time.
#' 
#' Applying a per-season spawning activity fraction `phi_m` to fecundity 
#' concentrates spawning production in the same seasons as recruitment,
#' representing the fraction of mature fish that are actively spawning each season.
#'
#'
#' @return The [om-class] object with `Fecundity@MeanAtAge` updated for all
#'   stocks with non-uniform seasonal recruitment. 
#'   
#' @seealso [SetSeasonalR0()], [WrappedNormal()], [CalcUnfishedNumber()]
#'
#' @export
AdjustSeasonalFecundity <- function(OM, silent = FALSE) {

  if (OM@Seasons == 1L)
    return(OM)

  if (!IsSeasonalRecruitment(OM))
    return(OM)

  OM <- PopulateOM(OM,
                   standardize_effort = FALSE,
                   adjust_fecundity = FALSE,
                   silent = TRUE)

  n_seasons <- OM@Seasons
  nYear     <- OM@nYear
  HistYears <- Years(OM, 'H')
  
  # Unfished N-at-age accounting for SpawnTimeFrac: list of length nStock,
  # each sim x age x year
  UnfishedN <- CalcUnfishedNumber(OM, SP = TRUE)

  for (st in seq_along(OM@Stock)) {
    Stock <- OM@Stock[[st]]
    
    if (!is.null(Stock@SRR@SPFrom) && Stock@SRR@SPFrom != Stock@Name)
      next
    
    nSim  <- Stock@nSim
    R0    <- Stock@SRR@R0 |> Extend(nSim  = nSim(OM),
                                    Years = HistYears)

    pi_y1      <- R0[, 1:n_seasons, drop = FALSE]
    pi_y1_norm <- pi_y1 / rowSums(pi_y1)
    if (all(apply(pi_y1_norm, 1, function(x) diff(range(x))) < .Machine$double.eps^0.5)) {
      next
    }

    N_age <- UnfishedN[[st]]            # sim x age x year
    Stock@Fecundity@MeanAtAge <- Extend(Stock@Fecundity@MeanAtAge,
                                        nSim  = nSim(OM),
                                        Years = HistYears)
    Fec         <- Stock@Fecundity@MeanAtAge
    recruit_lag <- round(min(Stock@Ages@Classes) * n_seasons)
    SP_unscaled <- ArrayMultiply(N_age, Fec) |> SumOverAge()

    for (y in seq_len(nYear)) {
      year_idx <- ((y - 1) * n_seasons + 1):(y * n_seasons)

      # Shift season indices forward by lag (circular within year) to get the
      # recruitment timestep driven by spawning at each season in year y
      lagged_ts <- ((y - 1) * n_seasons) + ((year_idx - 1 + recruit_lag) %% n_seasons) + 1

      R0_annual_y <- rowSums(R0[, lagged_ts, drop = FALSE])
      pi_m_y      <- R0[, lagged_ts, drop = FALSE] / R0_annual_y

      SP_season_y <- SP_unscaled[, year_idx, drop = FALSE]
      SP_total_y  <- rowSums(SP_season_y)

      if (all(SP_total_y < .Machine$double.eps)) next

      SP_shares_y <- SP_season_y / SP_total_y

      phi_m_y <- pi_m_y / SP_shares_y
      phi_m_y[is.nan(phi_m_y)] <- 0

      phi_max          <- apply(phi_m_y, 1, max)
      phi_max[phi_max == 0] <- 1
      phi_m_y          <- phi_m_y / phi_max

      if (!silent) {
        if (any(SP_shares_y < 1e-6 & pi_m_y > 0.01))
          cli::cli_alert_warning(
            "Stock {st} ({CommonName(Stock)}) year {y}: one or more seasons have near-zero spawning production but non-trivial target recruitment. The seasonal pattern may be incompatible with this  life history."
          )
      }

      for (m in seq_len(n_seasons)) {
        t_m <- year_idx[m]
        Stock@Fecundity@MeanAtAge[, , t_m] <-
          Stock@Fecundity@MeanAtAge[, , t_m] * phi_m_y[, m]
      }
    }

    OM@Stock[[st]] <- Stock
  }

  OM
}
