#' Check and validate fleet allocation for a `hist` object
#'
#' Validates the `Allocation` slot in the operating model (OM) of a
#' `hist` object. If allocation is unspecified, falls back to `CatchFrac`
#' or derives allocation from the mean of the last five historical years.
#' Returns the updated `hist` object with a fully-specified, validated
#' `Allocation` list.
#'
#' @param Hist A `hist` object containing operating model data.
#'
#' @return The input `hist` object with `hist@OM@Allocation` populated
#'   and validated for all stocks.
#'
#' For each stock, allocation is resolved in this order:
#'
#' 1. Use `OM@Allocation` if specified.
#' 2. Fall back to `OM@CatchFrac` if available.
#' 3. Derive from the mean relative removals (landings + discards)
#'    over the last five historical years.
#'
#' After resolution, each allocation matrix is validated:
#'
#' - Dimensions must be `nSim` × `nFleet`.
#' - All values must be finite and non-negative.
#' - Each row must sum to 1 (within a tolerance of `sqrt(.Machine$double.eps)`).
#'
#' @keywords internal
CheckAllocation <- function(Hist) {
  
  StockNames <- StockNames(Hist)
  Allocation <- Hist@OM@Allocation
  
  nStock    <- nStock(Hist)
  nComplex  <- length(Hist@OM@Complexes)
  nFleet    <- nFleet(Hist)
  nSim      <- nSim(Hist)
  HistYears <- Years(Hist,'H')
  
  ComplexNames  <- names(Hist@OM@Complexes)
  
  if (!length(Allocation)) 
    Allocation <- MakeNamedList(ComplexNames)
  
  if (length(Allocation)!= nComplex)
    cli::cli_abort('`Allocation` must be a list length 0 or length `nComplex(OM)` ')
  
  for (st in 1:nComplex) {
    AllocationFleet <- Allocation[[st]] 
    
    if (is.null(AllocationFleet)) {
      
      if (nFleet == 1) {
        AllocationFleet <- matrix(1, nSim, nFleet)
        next
      } 
      
      if (!is.null(Hist@OM@CatchFrac[[st]])) {
        AllocationFleet <- Hist@OM@CatchFrac[[st]]
        
        Hist <- CaptureLog(Hist,
                         string = cli::format_inline(
                           "`Allocation(OM)` has not been specified for Complex {.val {ComplexNames[st]}}"
                           ),
                         name = "Allocation"
                         )
        
        Hist <- CaptureLog(Hist,
                         string = cli::format_inline(
                           "Assuming distribution of TAC in projections is the same as `CatchFrac`"
                           )
                         
        )

      } else {
        Hist <- CaptureLog(Hist,
                              string = cli::format_inline(
                                "`Allocation(OM)` has not been specified for Complex {.val {ComplexNames[st]}}"
                                ),
                              name = "Allocation"
        )
        Hist <- CaptureLog(Hist,
                              string = cli::format_inline(
                                "Assuming TAC Allocation in projections is same as mean removals from last 5 historical years"
                                )
                              
        )
        
        last_5_years <- utils::tail(seq_len(length(HistYears)), 5)
        
        removals <- Hist@Landings[,st,last_5_years, ,drop=FALSE] + 
          Hist@Discards[,st,last_5_years, ,drop=FALSE]
        removals <- apply(removals, c('Sim', 'Fleet'), sum)
        rel_removals <- removals/apply(removals, 'Sim', sum)
        
        AllocationFleet <- matrix(rel_removals, nSim, nFleet)
      }
    } 
    
    dd <- dim(AllocationFleet)
    if (dd[1] != nSim && dd[1] != 1)
      cli::cli_abort('`OM@Allocation` must be a list length `nComplex(OM)` with a `nSim` by `nFleet` matrix  for each stock')
    
    if (dd[2]!=nFleet)
      cli::cli_abort('`OM@Allocation` must be a list length `nComplex(OM)` with a `nSim` by `nFleet` matrix  for each stock')
    
    if (any(AllocationFleet<0) || any(!is.finite(AllocationFleet)))
      cli::cli_abort('Values in `OM@Allocation` must be positive')
    
    rsum <- rowSums(AllocationFleet)
    tol  <- sqrt(.Machine$double.eps)
    if (any(abs(rsum - 1) > tol))
      cli::cli_abort(
        c('Values in `OM@Allocation` must sum to 1 across rows',
          'i' = 'Max deviation: {.val {max(abs(rsum - 1))}}')
      )
    
    dimnames(AllocationFleet) <- list("Sim"=1:dd[1],
                                      "Fleet"=FleetNames(Hist@OM))
    
    Allocation[[st]] <- AllocationFleet
  }
  
  Hist@OM@Allocation <- Allocation
  Hist
}