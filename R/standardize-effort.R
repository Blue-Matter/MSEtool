#' Standardize fishing effort across stocks for each fleet
#'
#' For a multi-stock operating model, effort for a given fleet should be
#' identical across stocks and areas. When stocks have been specified with
#' differing effort trajectories, this function standardizes effort to the
#' mean across stocks and absorbs the per-stock deviations into fishing
#' efficiency (catchability), preserving effective fishing mortality
#' (`E * q`).
#'
#' For each fleet `fl`, the standardized effort is computed as the
#' element-wise mean of the effort arrays across all stocks, after first
#' extending all arrays to a common set of simulations and years via
#' [Extend()]. Each stock's catchability efficiency is then rescaled so
#' that:
#'
#' \deqn{E_{\text{standard}} \times q_{\text{updated}} =
#'       E_{\text{nominal}} \times q_{\text{nominal}}}
#'
#' Where `StandardEffort` is near zero (below tolerance `1e-4`), the
#' original catchability value is retained unchanged.
#'
#' The effort `Distribution` array must be identical across stocks for
#' each fleet; the function errors informatively if this is violated.
#'
#' The function returns `OM` unchanged if `nStock == 1` or `nFleet == 1`.
#'
#' @param OM An operating model object ([om-class]).
#' @param silent `logical(1)`. If `FALSE` (default), emits a warning and
#'   informational message when effort is found to differ across stocks
#'   for any fleet. Set to `TRUE` to suppress these messages.
#'
#' @return The input `OM` object with updated `Fleet` slot. For each fleet
#'   where effort differed across stocks:
#'   - `Effort@Effort` is set to the mean effort across stocks.
#'   - `Catchability@Efficiency` is rescaled to conserve effective effort.
#'
#' @export
#'
#' @seealso [Extend()] for array dimension alignment.
#'
#' @examples
#' \dontrun{
#' # OM with two stocks whose effort arrays differ slightly
#' OM_standardized <- StandardizeEffort(OM, silent = FALSE)
#' }
StandardizeEffort <- function(OM, silent = FALSE) {
  
  # For any given Fleet, Effort should be the same for all Stocks 
  # and all areas.
  
  # Deviations in Effort assumed to be differences in stock- and/or time-varying
  # fishing efficiency (catchability)
  
  n_stock <- nStock(OM)
  n_fleet <- nFleet(OM)
  fleet_names <- FleetNames(OM)
  
  if (n_stock == 1 || n_fleet == 1) 
    return(OM)
  
  FleetList <- OM@Fleet
  stock_seq <- seq_len(n_stock)
  fleet_seq <- seq_len(n_fleet)
  tol <- 1e-4
  
  for (fl in fleet_seq) {
    Fleet_fl_all_stocks <- purrr::map(FleetList, `[[`, fl)
    
    EffortObjectList <- purrr::map(Fleet_fl_all_stocks, slot, 'Effort')
    qObjectList     <- purrr::map(Fleet_fl_all_stocks, slot, 'Catchability')
    
    EffortArrayList <- purrr::map(EffortObjectList, slot, "Effort")
    qArrayList      <- purrr::map(qObjectList,      slot, "Efficiency")
    DistArrayList   <- purrr::map(EffortObjectList, slot, "Distribution")
    
    EffortDims <- purrr::map(EffortArrayList, dim)
    qDims      <- purrr::map(qArrayList, dim)
    
    nSim <- max(
      vapply(EffortDims, `[`, numeric(1), 1L),
      vapply(qDims,      `[`, numeric(1), 1L)
    )
    
    Years <- c(
      purrr::map(EffortArrayList, \(x) dimnames(x)[["Year"]]),
      purrr::map(qArrayList,      \(x) dimnames(x)[["Year"]])
    ) |> unlist() |> unique() |> as.numeric() |> sort()
    
    # Extend all arrays to common dimensions
    EffortArrayList <- purrr::map(EffortArrayList, Extend, nSim, NULL, Years)
    qArrayList <- purrr::map(qArrayList, Extend, nSim, NULL, Years)
    
    # Validate Distribution arrays are identical across stocks
    ref_dist <- DistArrayList[[1]]
    for (st in seq_len(n_stock)[-1]) {
      if (!identical(dim(DistArrayList[[st]]), dim(ref_dist))) {
        cli::cli_abort(c(
          "Effort Distribution dimensions differ across stocks for Fleet {fl}.",
          "i" = "(`Fleet |> Effort() |> Distribution()`) must be identical across stocks."
        ))
      }
      dev <- DistArrayList[[st]] - ref_dist
      if (!any(is.na(dev)) && any(abs(dev) > tol)) {
        cli::cli_abort(c(
          "Effort Distribution values differ across stocks for Fleet {fl}.",
          "i" = "(`Fleet |> Effort() |> Distribution()`) must be identical across stocks."
        ))
      }
    }   
    
    # Compute standard effort (mean across stocks) 
    # TODO consider median for large nStock with high variable effort
    StandardEffort <- Reduce("+", EffortArrayList) / n_stock
    
    # Check if any stock differs meaningfully from the mean
    anyDiff <- purrr::map_lgl(EffortArrayList, \(E) any(abs(E - StandardEffort) > tol))
    
    if (!any(anyDiff))  next
    
    if (!silent) {
      cli::cli_alert_warning(
        "Effort values for Fleet {.val {fleet_names[fl]}} differ across stocks."
      )
      cli::cli_alert(
        "Standardizing to mean effort across stocks; absorbing deviations into {.val Catchability}."
      )
    }
    
    ok <- abs(StandardEffort) > tol 
    
    for (st in stock_seq) {
      Effort_nominal <- EffortArrayList[[st]]
      q_nominal      <- qArrayList[[st]]
      
      q_updated        <- q_nominal
      q_updated[ok]    <- (Effort_nominal[ok] * q_nominal[ok]) / StandardEffort[ok]
      q_updated[!ok]   <- q_nominal[!ok]
      
      # EffortArrayList[[st]] <- StandardEffort
      # qArrayList[[st]]      <- q_updated
      
      FleetList[[st]][[fl]]@Effort@Effort           <- StandardEffort
      FleetList[[st]][[fl]]@Catchability@Efficiency <- q_updated
    }
  }
  OM@Fleet <- FleetList
  OM
}