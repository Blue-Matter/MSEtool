#' Standardize fishing effort across stocks for each fleet
#'
#' For a multi-stock operating model, effort for a given fleet must be
#' identical across stocks. When stocks have been specified with differing
#' effort trajectories (e.g. derived from stock-specific estimated Fs), this
#' function standardizes effort to the mean across stocks and back-calculates
#' stock-fleet targeting weights that preserve effective fishing mortality.
#'
#' For each fleet `fl`, the standardized effort is the element-wise mean of
#' the effort arrays across all stocks. 
#' 
#' The targeting weight for each stock is then:
#'
#' \deqn{
#'   \text{Targeting}_{s,i,y} =
#'     \frac{E_{\text{nominal},s,i,y}}{E_{\text{standard},i,y}}
#' }
#'
#' so that effective fishing mortality is conserved:
#'
#' \deqn{
#'   E_{\text{standard}} \times \text{Targeting}_s \times q_s =
#'   E_{\text{nominal},s} \times q_s
#' }
#'
#' Targeting weights are by construction mean-1 across stocks for each
#' fleet-year. Where `StandardEffort` is near zero (below tolerance `1e-4`),
#' targeting is set to 1 (neutral).
#'
#' The effort `Distribution` array must be identical across stocks for each
#' fleet.
#'
#' The function returns `OM` unchanged if `nStock == 1`.
#'
#' @param OM An operating model object ([om-class]). 
#' @param silent `logical(1)`. If `FALSE` (default), emits a warning and
#'   informational message when effort is found to differ across stocks for
#'   any fleet. Set to `TRUE` to suppress these messages.
#' @param populate  `logical(1)`. Populate `OM` at beginning of call? Used internally.
#'
#' @return The input `OM` object with:
#'   - `Fleet[[st]][[fl]]@Effort@Effort` set to the mean effort across stocks
#'     for each fleet where effort differed.
#'   - `OM@StockTargeting` populated with back-calculated targeting
#'     weights.
#'
#'
#' @examples
#' \dontrun{
#' OM_standardized <- StandardizeEffort(OM, silent = FALSE)
#' # Inspect back-calculated targeting
#' OM_standardized@StockTargeting@Targeting
#' }
StandardizeEffort <- function(OM, silent = FALSE, populate=TRUE) {
  
  if (populate)
    OM <- PopulateOM(OM, silent=TRUE, standardize_effort=FALSE)
  
  n_stock <- nStock(OM)
  n_fleet <- nFleet(OM)
  fleet_names <- FleetNames(OM)
  
  if (n_stock == 1) {
    OM@StockTargeting <- neW()
    return(OM)
  }
    
  FleetList  <- OM@Fleet
  stock_seq  <- seq_len(n_stock)
  fleet_seq  <- seq_len(n_fleet)
  tol        <- 1e-4
  
  HistYears <- Years(OM,'H')
  
  # Initialise TargetingModel with neutral targeting (all 1s)
  STarget <- StockTargeting(OM)
  
  
  for (fl in fleet_seq) {
    Fleet_fl_all_stocks <- purrr::map(FleetList, `[[`, fl)
    
    EffortObjectList <- purrr::map(Fleet_fl_all_stocks, slot, "Effort")
    DistArrayList   <- purrr::map(EffortObjectList, slot, "Distribution")
    EffortArrayList <- purrr::map(EffortObjectList, slot, "Effort")
    
    # Extend all effort arrays to common dimensions
    EffortArrayList <- purrr::map(EffortArrayList, Extend, nSim=OM@nSim, NULL, Years=HistYears)
    
    # Validate Distribution arrays are identical across stocks
    ref_dist <- DistArrayList[[1]]
    for (st in stock_seq[-1]) {
      if (!identical(dim(DistArrayList[[st]]), dim(ref_dist))) {
        cli::cli_abort(c(
          "Effort {.val Distribution} dimensions differ across stocks for Fleet {fl}.",
          "i" = "{.code Fleet |> Effort() |> Distribution()} must be identical across stocks."
        ))
      }
      dev <- DistArrayList[[st]] - ref_dist
      if (!any(is.na(dev)) && any(abs(dev) > tol)) {
        cli::cli_abort(c(
          "Effort {.val Distribution} values differ across stocks for Fleet {fl}.",
          "i" = "{.code Fleet |> Effort() |> Distribution()} must be identical across stocks."
        ))
      }
    }
    
    # Standard effort: mean across stocks
    StandardEffort <- Reduce("+", EffortArrayList) / n_stock
    
    # Check if any stock differs meaningfully from the mean
    anyDiff <- purrr::map_lgl(
      EffortArrayList, \(E) any(abs(E - StandardEffort) > tol)
    )
    if (!any(anyDiff)) next
    
    if (!silent) {
      cli::cli_alert_warning(
        "Effort values for Fleet {.val {fleet_names[fl]}} differ across stocks."
      )
      cli::cli_alert_info(
        "Standardizing to mean effort across stocks; absorbing deviations into {.val Targeting}."
      )
    }
    
    ok <- abs(StandardEffort) > tol
    
    # Back-calculate targeting weights per stock and store in StockTargeting
    for (st in stock_seq) {
      Effort_nominal <- EffortArrayList[[st]]
      
      targeting_st <- array(1, dim = dim(StandardEffort))
      dimnames(targeting_st) <- dimnames(StandardEffort)
      targeting_st[ok]  <- Effort_nominal[ok] / StandardEffort[ok]
      targeting_st[!ok] <- 1
      
      STarget@Targeting[, st, fl, ] <- targeting_st
    }
    
    # Update effort for all stocks for this fleet
    for (st in stock_seq) {
      FleetList[[st]][[fl]]@Effort@Effort <- StandardEffort
    }
  }
  
  # Reduce dims
  for (st in stock_seq) {
    for (fl in fleet_seq) {
      FleetList[[st]][[fl]]@Effort@Effort <- ReduceDims(FleetList[[st]][[fl]]@Effort@Effort)
    }
  }
  
  OM@Fleet          <- FleetList
  OM@StockTargeting <- ReduceDims(STarget)
  
  FitStockTargeting(OM)
}


FitStockTargeting <- function(OM, tol=1E-6, active_thresh = 0.1) {
  
  if (nStock(OM) == 1)
    return(OM)
  
  STarget <- OM@StockTargeting
  Targ    <- STarget@Targeting   # [sim, stock, fleet, year]
  
  if (
    !all(is.na(STarget@Mean)) &&
    !all(is.na(STarget@Covariance)) &&
    !all(is.na(STarget@AC))
  )  return(OM) # already populated
  
  dims     <- dim(Targ)
  nSim     <- dims[1]
  n_stock  <- dims[2]
  n_fleet  <- dims[3]
  nYear    <- dims[4]
  Years <- as.numeric(dimnames(Targ)$Year)
  
  stock_names <- StockNames(OM)
  fleet_names <- FleetNames(OM)

  mu_arr <- array(NA_real_,
                  dim = c(nSim, n_stock, n_fleet),
                  dimnames = list(Sim = seq_len(nSim),
                                  Stock = stock_names,
                                  Fleet = fleet_names))
  
  cov_arr <- array(NA_real_,
                   dim = c(nSim, n_stock, n_stock, n_fleet),
                   dimnames = list(Sim = seq_len(nSim),
                                   Stock = stock_names,
                                   Stock = stock_names,
                                   Fleet = fleet_names))
  
  ac_arr <- array(NA_real_,
                  dim = c(nSim, n_stock, n_fleet),
                  dimnames = list(Sim = seq_len(nSim),
                                  Stock = stock_names,
                                  Fleet = fleet_names))

  for (sim in seq_len(OM@nSim)) {
    for (fl in fleet_seq) {
      
      # determine stocks where this fleet is active
      active_stock <- logical(n_stock)
      names(active_stock) <- stock_names
      
      StdEffort <- OM@Fleet[[1]][[fl]]@Effort@Effort         
      zero_fishing_years <- which(StdEffort[sim,] <= 0)
      
      for (st in seq_len(n_stock)) {
        x <- StdEffort[sim,] * Targ[sim, st, fl, ]
        prop_active <- mean(x > tol, na.rm = TRUE)
        active_stock[st] <- prop_active > active_thresh
      }
      
      active_idx <- which(active_stock)
      
      # no active stocks 
      if (length(active_idx) == 0) {
        mu_arr[sim, , fl] <- 0
        cov_arr[sim, , , fl] <- diag(n_stock)
        ac_arr[sim, , fl] <- 0
        innov_cov_arr[sim, , , fl] <- diag(n_stock)
        
        next
      }
      
      # targeting matrix for active stocks 
      targ_mat <- array(NA_real_, dim=c(nYear, length(active_idx)),
                        dimnames = list(
                          Year = Years,
                          Stock = stock_names[active_idx]
                        )
      )
                
      for (j in seq_along(active_idx)) {
        st <- active_idx[j]
        x <- Targ[sim, st, fl, ]
        x[x <= tol] <- NA_real_
        targ_mat[, j] <- log(x)
      }
      targ_mat[zero_fishing_years,] <- NA_real_
      
      # mean
      mu_active <- colMeans(targ_mat, na.rm = TRUE)
      
      # covariance
      complete_rows <- stats::complete.cases(targ_mat)
      if (sum(complete_rows) < 2) {
        cli::cli_alert_warning("Insufficient data for Fleet {.val {fleet_names[fl]}}, simulation {sim}. Using identity covariance.")
      
        Sigma_active <- diag(n_stock)
      } else {
        Sigma_active <- stats::cov(targ_mat[complete_rows, , drop = FALSE])
      }
      
      # autocorrelation 
      phi_active <- numeric(length(active_idx))
      
      for (j in seq_along(active_idx)) {
        x <- targ_mat[, j]
        ok <- is.finite(x)
        x <- x[ok]
        
        if (length(x) < 2) {
          phi_active[j] <- 0
          next
        }
        
        x1 <- x[-length(x)]
        x2 <- x[-1]
        
        ok2 <- is.finite(x1) & is.finite(x2)
        
        if (sum(ok2) < 2 || sum(x1) == 0) {
          phi_active[j] <- 0
        } else {
          phi_active[j] <- stats::cor(x1[ok2], x2[ok2]) # better than acf for short series
        }
      }
      
      # mean 
      mu_full <- rep(0, n_stock)
      mu_full[active_idx] <- mu_active
      
      # covar
      Sigma_full <- diag(n_stock)
      Sigma_full[active_idx, active_idx] <- Sigma_active
      
      # AC
      phi_full <- rep(0, n_stock)
      phi_full[active_idx] <- phi_active
      
      mu_arr[sim, , fl] <- mu_full
      cov_arr[sim, , , fl] <- Sigma_full
      ac_arr[sim, , fl] <- phi_full
    
    } # end fleet loop
  } # end sim loop
  
  OM@StockTargeting@Mean <- ReduceDims(mu_arr)
  OM@StockTargeting@Covariance <- ReduceDims(cov_arr) 
  OM@StockTargeting@AC <- ReduceDims(ac_arr)
  
  OM
}
