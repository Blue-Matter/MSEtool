#' Standardize fishing effort across stocks for each fleet
#'
#' For a multi-stock operating model, effort for a given fleet must be
#' identical across stocks. When stocks have been specified with differing
#' effort trajectories (e.g. derived from stock-specific estimated Fs), this
#' function standardizes effort to the geometric mean across **active stocks**
#' (those with positive effort) at each time step, and back-calculates
#' stock-fleet targeting weights that preserve effective fishing mortality.
#'
#' For each fleet `fl`, year `t`, and simulation `i`, the standardized effort
#' is the geometric mean of stock-specific effort over stocks with positive
#' effort at that time step:
#'
#' \deqn{
#'   E_{f,t} = \exp\!\left(
#'     \frac{1}{|\mathcal{A}_{f,t}|}
#'     \sum_{s \in \mathcal{A}_{f,t}} \log E_{s,f,t}
#'   \right)
#' }
#'
#' where \eqn{\mathcal{A}_{f,t} = \{s : E_{s,f,t} > 0\}} is the set of
#' active stocks for fleet \eqn{f} at time \eqn{t}.
#'
#' The targeting weight for each stock is then:
#'
#' \deqn{
#'   \delta_{s,f,t} = \frac{E_{s,f,t}}{E_{f,t}}
#' }
#'
#' so that effective fishing mortality is conserved:
#'
#' \deqn{
#'   E_{f,t} \times \delta_{s,f,t} \times q_{s,f} =
#'   E_{s,f,t} \times q_{s,f} = F_{s,f,t}
#' }
#'
#' By construction, the targeting weights have geometric mean 1 over active
#' stocks at each time step — equivalently, the log-targeting deviations sum
#' to zero:
#'
#' \deqn{
#'   \sum_{s \in \mathcal{A}_{f,t}} \log \delta_{s,f,t} = 0
#' }
#'
#' Inactive stocks (\eqn{E_{s,f,t} = 0}) receive targeting weight
#' \eqn{\delta_{s,f,t} = 0}.
#'
#' These targeting weights are stored in `OM@StockTargeting@Targeting` and
#' are subsequently used by [FitStockTargeting()] to estimate the historical
#' covariance of log-targeting deviations.
#'
#' If populated, the effort `Distribution` array must be identical across stocks
#' for each fleet. The function returns `OM` unchanged if `nStock == 1`
#' (any pre-existing `StockTargeting` data is also cleared in this case, as
#' it is not meaningful for single-stock models).
#'
#' @param OM An operating model object ([om-class]).
#' @param populate `logical(1)`. Populate `OM` at beginning of call?
#'   Used internally. Default `TRUE`.
#' @param fit_stock_targeting `logical(1)`. Run [FitStockTargeting()] after
#'   standardizing? Default `TRUE`.
#' @param generate_stock_targeting `logical(1)`. Run [GenerateStockTargeting()]
#'   after fitting? Default `TRUE`. Ignored (with a warning) if
#'   `fit_stock_targeting = FALSE`, since [GenerateStockTargeting()] requires
#'   fitted targeting parameters.
#' @param record_assumption `logical(1)`. Record the effort-standardization as
#'   an `"assumption"` in `OM@Log`? Default `TRUE`. Set `FALSE` when the
#'   differing per-stock effort is structural to how `OM` was built (e.g. a
#'   sex-structured `ImportSS()` model, where per-sex effort naturally differs
#'   due to sex-specific selectivity) rather than an unexpected input.
#'
#' @return The input `OM` object with:
#'   - `Fleet[[st]][[fl]]@Effort@Effort` set to the geometric mean effort
#'     over active stocks for **all stocks** in each fleet where effort differed
#'     across any stock.
#'   - `OM@StockTargeting@Targeting` populated with back-calculated targeting
#'     weights \eqn{\delta_{s,f,t}}. Over active stocks at each time step, the
#'     weights have geometric mean 1 (equivalently, their logs sum to zero).
#'     Inactive stocks receive weight 0.
#'
#' @seealso [FitStockTargeting()], [GenerateStockTargeting()]
#'
#' @examples
#' \dontrun{
#' OM_standardized <- StandardizeEffort(OM)
#' # Inspect back-calculated targeting
#' OM_standardized@StockTargeting@Targeting
#' }
#' @export
StandardizeEffort <- function(OM,
                              populate = TRUE,
                              fit_stock_targeting = TRUE,
                              generate_stock_targeting = TRUE,
                              record_assumption = TRUE) {
  
  if (!fit_stock_targeting && generate_stock_targeting) {
    cli::cli_warn(c(
      "{.arg generate_stock_targeting} is {.val TRUE} but {.arg fit_stock_targeting} is {.val FALSE}.",
      "i" = "{.fn GenerateStockTargeting} requires fitted targeting parameters; skipping."
    ))
    generate_stock_targeting <- FALSE
  }
  
  if (populate)
    OM <- PopulateOM(OM, silent = TRUE, standardize_effort = FALSE)
  
  n_stock     <- nStock(OM)
  n_fleet     <- nFleet(OM)
  fleet_names <- FleetNames(OM)
  
  if (n_stock == 1) {
    OM@StockTargeting <- StockTargeting(OM)
    return(OM)
  }
  
  FleetList <- OM@Fleet
  stock_seq <- seq_len(n_stock)
  fleet_seq <- seq_len(n_fleet)
  tol       <- 1e-6
  
  HistYears <- Years(OM, 'H')
  n_years   <- length(HistYears)
  
  STarget <- OM@StockTargeting
  
  if (EmptyObject(STarget))
    STarget <- StockTargeting(OM)
  
  for (fl in fleet_seq) {
    Fleet_fl_all_stocks <- purrr::map(FleetList, `[[`, fl)
    
    EffortObjectList <- purrr::map(Fleet_fl_all_stocks, slot, "Effort")
    DistArrayList    <- purrr::map(EffortObjectList, slot, "Distribution")
    EffortArrayList  <- purrr::map(EffortObjectList, slot, "Effort")
    
    # Extend all effort arrays to common dimensions
    EffortArrayList <- purrr::map(EffortArrayList, Extend,
                                  nSim = OM@nSim, NULL, Years = HistYears)
    
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
    
    # Check if any stock differs meaningfully from the first
    ref_effort <- EffortArrayList[[1]]
    effort_differs <- purrr::map_lgl(
      EffortArrayList, \(E) any(abs(E - ref_effort) > tol, na.rm = TRUE)
    )
    
    if (!any(effort_differs)) {
      if (!all(is.na(STarget@Mean[,,fl])))
        next
      
      STarget@Mean[,,fl] <- 1
      dd <- dim(STarget@Covariance)
      for (i in seq_len(dd[1])) {
        STarget@Covariance[i,,,fl] <- diag(1, n_stock, n_stock)
      }
      STarget@Targeting[,,fl,] <- 1
      
      next
    }
    
    if (record_assumption) {
      OM <- .CaptureLog(OM,
                       string = cli::format_inline(
                         "Effort values for Fleet {.val {fleet_names[fl]}} differ across stocks."),
                       name = 'StandardizeEffort',
                       type = 'assumption')

      OM <- .CaptureLog(OM,
                       string = cli::format_inline(
                         "Standardizing to geometric mean effort over active stocks; absorbing deviations into {.val Targeting}."),
                       type = 'assumption')
    }

    # Geometric mean effort over active stocks at each [sim, year] cell.
    # A stock is active if its effort exceeds tol 
    log_effort_sum <- array(0, dim = dim(EffortArrayList[[1]]),
                            dimnames = dimnames(EffortArrayList[[1]]))
    n_active       <- array(0L, dim = dim(EffortArrayList[[1]]),
                            dimnames = dimnames(EffortArrayList[[1]]))
    
    for (st in stock_seq) {
      E         <- EffortArrayList[[st]]
      is_active <- E > tol
      log_effort_sum[is_active] <- log_effort_sum[is_active] + log(E[is_active])
      n_active[is_active] <- n_active[is_active] + 1L
    }
    
    # Geometric mean
    StandardEffort <- array(0, dim = dim(log_effort_sum),
                            dimnames = dimnames(log_effort_sum))
    has_active <- n_active > 0L
    StandardEffort[has_active] <- exp(log_effort_sum[has_active] /
                                        n_active[has_active])
    
    # Back-calculate targeting weights per stock:
    #   delta_{s,f,t} = E_{s,f,t} / E_{f,t}  (0 for inactive stocks)
    for (st in stock_seq) {
      E_st         <- EffortArrayList[[st]]
      targeting_st <- array(0, dim = dim(StandardEffort),
                            dimnames = dimnames(StandardEffort))
      
      is_active <- E_st > tol & has_active
      targeting_st[is_active] <- E_st[is_active] / StandardEffort[is_active]
      
      sims <- dimnames(STarget@Targeting)$Sim |> as.numeric()
      STarget@Targeting[, st, fl, seq_len(n_years)] <- .SubsetSim(targeting_st,Sims = sims)
    }
    
    # Update effort for all stocks for this fleet to the geometric mean
    for (st in stock_seq) {
      FleetList[[st]][[fl]]@Effort@Effort <- ReduceDims(StandardEffort)
    }
  }
  
  OM@Fleet          <- FleetList
  OM@StockTargeting <- ReduceDims(STarget)
  
  if (fit_stock_targeting)
    OM <- FitStockTargeting(OM)
  
  # if (fit_stock_targeting && generate_stock_targeting)
  #   OM <- GenerateStockTargeting(OM, 'Projection')
  
  OM
}
