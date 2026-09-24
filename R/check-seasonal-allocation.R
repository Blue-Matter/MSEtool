#' Check and validate seasonal TAC/Effort allocation for a `hist` object
#'
#' Validates the `SeasonalAllocation` slot in the operating model (OM) of a
#' `hist` object. If unspecified, derives it from `HistoricalWeight`, blending
#' the historical seasonal removals pattern with the population biomass
#' seasonal pattern. Returns the updated `hist` object with fully-specified,
#' validated `HistoricalWeight` and `SeasonalAllocation` lists.
#'
#' @param Hist A `hist` object containing operating model data.
#'
#' @return The input `hist` object with `hist@OM@HistoricalWeight` and
#'   `hist@OM@SeasonalAllocation` populated and validated for all complexes.
#'
#' For each complex, `SeasonalAllocation` is resolved in this order:
#'
#' 1. Use `OM@SeasonalAllocation` if specified.
#' 2. Derive from `OM@HistoricalWeight[f]*historical_share[s,f] +
#'    (1-OM@HistoricalWeight[f])*abundance_share[s]`, pooled over the last
#'    `OM@Control$SeasonalAllocationYears` years (default 5).
#'
#' @keywords internal
.CheckSeasonalAllocation <- function(Hist) {

  nFleetTot <- nFleet(Hist)
  nSimTot   <- nSim(Hist)
  Seasons   <- max(1L, as.integer(Hist@OM@Seasons %||NA% 1))
  HistYears <- Years(Hist, 'H')
  FleetNms  <- FleetNames(Hist@OM)

  Complexes    <- Hist@OM@Complexes
  nComplex     <- length(Complexes)
  ComplexNames <- names(Complexes)

  SeasonalAllocation <- Hist@OM@SeasonalAllocation
  if (!length(SeasonalAllocation))
    SeasonalAllocation <- MakeNamedList(ComplexNames)
  if (length(SeasonalAllocation) != nComplex)
    cli::cli_abort('`SeasonalAllocation` must be a list length 0 or length `nComplex(OM)`')

  HistoricalWeight <- Hist@OM@HistoricalWeight
  if (!length(HistoricalWeight))
    HistoricalWeight <- MakeNamedList(ComplexNames)
  if (length(HistoricalWeight) != nComplex)
    cli::cli_abort('`HistoricalWeight` must be a list length 0 or length `nComplex(OM)`')

  nYears      <- Hist@OM@Control$SeasonalAllocationYears %||NA% 5
  windowRows  <- utils::tail(seq_len(length(HistYears)), nYears * Seasons)
  seasonOfRow <- ((windowRows - 1) %% Seasons) + 1

  for (i in seq_len(nComplex)) {

    if (!is.null(SeasonalAllocation[[i]])) {
      SeasonalAllocation[[i]] <- .ValidateSeasonalAllocation(SeasonalAllocation[[i]], nSimTot, Seasons, FleetNms)
      next
    }

    hwSpecified <- !is.null(HistoricalWeight[[i]])
    hw <- .ResolveHistoricalWeight(HistoricalWeight[[i]], FleetNms)
    if (!hwSpecified && Seasons > 1) {
      Hist <- .CaptureLog(Hist,
        string = cli::format_inline("`HistoricalWeight(OM)` has not been specified for Complex {.val {ComplexNames[i]}}"),
        name = "SeasonalAllocation", type = 'assumption')
      Hist <- .CaptureLog(Hist,
        string = cli::format_inline("Assuming seasonal allocation follows the historical pattern only (HistoricalWeight = 1)"),
        type = 'assumption')
    }
    HistoricalWeight[[i]] <- hw

    stocks <- Complexes[[i]]

    removals <- ArraySum(Hist@Landings[, stocks, windowRows, , drop = FALSE],
                        Hist@Discards[, stocks, windowRows, , drop = FALSE])
    removals <- apply(removals, c('Sim', 'Year', 'Fleet'), sum)

    historical_share <- array(0, dim = c(Seasons, nSimTot, nFleetTot))
    for (s in seq_len(Seasons)) {
      rows_s <- which(seasonOfRow == s)
      historical_share[s, , ] <- apply(removals[, rows_s, , drop = FALSE], c('Sim', 'Fleet'), sum)
    }
    season_totals <- apply(historical_share, c(2, 3), sum)
    historical_share <- sweep(historical_share, c(2, 3), season_totals, '/')
    historical_share[is.nan(historical_share)] <- 1 / Seasons

    abundance_share <- NULL
    if (any(hw < 1)) {
      biomass <- Hist@Biomass[, stocks, windowRows, drop = FALSE]
      biomass <- apply(biomass, c('Sim', 'Year'), sum)

      abundance_share <- matrix(0, nrow = Seasons, ncol = nSimTot)
      for (s in seq_len(Seasons)) {
        rows_s <- which(seasonOfRow == s)
        abundance_share[s, ] <- apply(biomass[, rows_s, drop = FALSE], 'Sim', sum)
      }
      season_totals_b <- colSums(abundance_share)
      abundance_share <- sweep(abundance_share, 2, season_totals_b, '/')
      abundance_share[is.nan(abundance_share)] <- 1 / Seasons
    }

    weight <- array(0, dim = c(nSimTot, Seasons, nFleetTot),
                    dimnames = list(Sim = 1:nSimTot, Season = 1:Seasons, Fleet = FleetNms))
    for (f in seq_len(nFleetTot)) {
      if (hw[f] >= 1 || is.null(abundance_share)) {
        weight[, , f] <- t(historical_share[, , f])
      } else {
        weight[, , f] <- hw[f] * t(historical_share[, , f]) + (1 - hw[f]) * t(abundance_share)
      }
    }

    SeasonalAllocation[[i]] <- weight
  }

  Hist@OM@HistoricalWeight   <- HistoricalWeight
  Hist@OM@SeasonalAllocation <- SeasonalAllocation
  Hist
}

.ResolveHistoricalWeight <- function(hw, FleetNms) {
  nFleetTot <- length(FleetNms)

  if (is.null(hw)) {
    resolved <- stats::setNames(rep(1, nFleetTot), FleetNms)
  } else {
    nms <- names(hw)
    if (is.null(nms)) {
      resolved <- stats::setNames(rep(hw[1], nFleetTot), FleetNms)
    } else {
      resolved <- stats::setNames(rep(NA_real_, nFleetTot), FleetNms)
      matched  <- nms[nms %in% FleetNms]
      resolved[matched] <- hw[matched]

      unnamed <- hw[nms == '']
      default <- if (length(unnamed)) unnamed[1] else 1
      resolved[is.na(resolved)] <- default
    }
  }

  if (any(resolved < 0 | resolved > 1) || any(!is.finite(resolved)))
    cli::cli_abort('Values in `OM@HistoricalWeight` must be between 0 and 1')

  resolved
}

.ValidateSeasonalAllocation <- function(SA, nSimTot, Seasons, FleetNms) {
  nFleetTot <- length(FleetNms)
  dd <- dim(SA)
  if (length(dd) != 3 || dd[2] != Seasons || dd[3] != nFleetTot || (dd[1] != nSimTot && dd[1] != 1))
    cli::cli_abort('`OM@SeasonalAllocation` must be a list length `nComplex(OM)` with an `nSim` (or 1) x `Seasons` x `nFleet` array for each complex')

  if (any(SA < 0) || any(!is.finite(SA)))
    cli::cli_abort('Values in `OM@SeasonalAllocation` must be positive')

  colsum <- apply(SA, c(1, 3), sum)
  tol <- sqrt(.Machine$double.eps)
  if (any(abs(colsum - 1) > tol))
    cli::cli_abort(
      c('Values in `OM@SeasonalAllocation` must sum to 1 across seasons',
        'i' = 'Max deviation: {.val {max(abs(colsum - 1))}}')
    )

  if (dd[1] == 1)
    SA <- SA[rep(1, nSimTot), , , drop = FALSE]

  dimnames(SA) <- list(Sim = 1:nSimTot, Season = 1:Seasons, Fleet = FleetNms)
  SA
}
