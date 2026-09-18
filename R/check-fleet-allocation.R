#' Check and validate fleet allocation for a `hist` object
#'
#' Validates the `FleetAllocation` slot in the operating model (OM) of a
#' `hist` object. If allocation is unspecified, falls back to `CatchFrac`
#' or derives allocation from the mean of the last five historical years.
#' Returns the updated `hist` object with a fully-specified, validated
#' `FleetAllocation` list.
#'
#' @param Hist A `hist` object containing operating model data.
#'
#' @return The input `hist` object with `hist@OM@FleetAllocation` populated
#'   and validated for all stocks.
#'
#' For each stock, allocation is resolved in this order:
#'
#' 1. Use `OM@FleetAllocation` if specified.
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
.CheckFleetAllocation <- function(Hist) {

  StockNames <- StockNames(Hist)
  Allocation <- Hist@OM@FleetAllocation
  if (!length(Allocation) && length(Hist@OM@Allocation))
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
    cli::cli_abort('`FleetAllocation` must be a list length 0 or length `nComplex(OM)` ')

  for (st in 1:nComplex) {
    AllocationFleet <- Allocation[[st]]

    if (is.null(AllocationFleet)) {

      if (nFleet == 1) {
        AllocationFleet <- matrix(1, nSim, nFleet)
        next
      }

      if (!is.null(Hist@OM@CatchFrac[[st]])) {
        AllocationFleet <- Hist@OM@CatchFrac[[st]]

        Hist <- .CaptureLog(Hist,
                         string = cli::format_inline(
                           "`FleetAllocation(OM)` has not been specified for Complex {.val {ComplexNames[st]}}"
                           ),
                         name = "FleetAllocation",
                         type = 'assumption'
                         )

        Hist <- .CaptureLog(Hist,
                         string = cli::format_inline(
                           "Assuming distribution of TAC in projections is the same as `CatchFrac`"
                           ),
                         type = 'assumption'
        )

      } else {
        Hist <- .CaptureLog(Hist,
                              string = cli::format_inline(
                                "`FleetAllocation(OM)` has not been specified for Complex {.val {ComplexNames[st]}}"
                                ),
                              name = "FleetAllocation",
                              type = 'assumption'
        )
        Hist <- .CaptureLog(Hist,
                              string = cli::format_inline(
                                "Assuming TAC allocation in projections is same as mean removals from last 5 historical years"
                                ),
                              type = 'assumption'
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
      cli::cli_abort('`OM@FleetAllocation` must be a list length `nComplex(OM)` with a `nSim` by `nFleet` matrix  for each stock')

    if (dd[2]!=nFleet)
      cli::cli_abort('`OM@FleetAllocation` must be a list length `nComplex(OM)` with a `nSim` by `nFleet` matrix  for each stock')

    if (any(AllocationFleet<0) || any(!is.finite(AllocationFleet)))
      cli::cli_abort('Values in `OM@FleetAllocation` must be positive')

    rsum <- rowSums(AllocationFleet)
    tol  <- sqrt(.Machine$double.eps)
    if (any(abs(rsum - 1) > tol))
      cli::cli_abort(
        c('Values in `OM@FleetAllocation` must sum to 1 across rows',
          'i' = 'Max deviation: {.val {max(abs(rsum - 1))}}')
      )

    dimnames(AllocationFleet) <- list("Sim"=1:dd[1],
                                      "Fleet"=FleetNames(Hist@OM))

    Allocation[[st]] <- AllocationFleet
  }

  Hist@OM@FleetAllocation <- Allocation
  Hist
}
