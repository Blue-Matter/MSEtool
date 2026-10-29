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
#' For each complex, allocation is resolved in this order:
#'
#' 1. Use `OM@FleetAllocation` if specified.
#' 2. Fall back to `OM@CatchFrac` if it is set for every stock in the
#'    complex, weighting each stock's `CatchFrac` by its removals in the
#'    final historical year.
#' 3. Derive from the mean relative removals (landings + discards) of all
#'    stocks in the complex over the last five historical years, or the last
#'    five years with non-zero removals, or an equal split if there are none.
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

  ComplexNames  <- ComplexNames(Hist)

  if (!length(Allocation))
    Allocation <- MakeNamedList(ComplexNames)

  if (length(Allocation)!= nComplex)
    cli::cli_abort('`FleetAllocation` must be a list length 0 or length `nComplex(OM)` ')

  for (cx in 1:nComplex) {
    AllocationFleet <- Allocation[[cx]]

    if (is.null(AllocationFleet)) {

      if (nFleet == 1) {
        AllocationFleet <- matrix(1, nSim, nFleet)
        next
      }

      Stocks   <- Hist@OM@Complexes[[cx]]
      HistInd  <- seq_along(HistYears)
      Removals <- Hist@Landings[, Stocks, HistInd, , drop = FALSE] +
        Hist@Discards[, Stocks, HistInd, , drop = FALSE]

      Hist <- .CaptureLog(Hist,
                          string = cli::format_inline(
                            "`FleetAllocation(OM)` has not been specified for Complex {.val {ComplexNames[cx]}}"
                          ),
                          name = "FleetAllocation",
                          type = 'assumption'
      )

      CatchFrac <- Hist@OM@CatchFrac[Stocks]
      if (length(CatchFrac) && !any(vapply(CatchFrac, is.null, logical(1)))) {
        AllocationFleet <- .ComplexCatchFrac(CatchFrac, Removals, nSim)

        Hist <- .CaptureLog(Hist,
                            string = cli::format_inline(
                              "Assuming distribution of TAC in projections is the same as `CatchFrac`"
                            ),
                            type = 'assumption'
        )

      } else {
        Hist <- .CaptureLog(Hist,
                            string = cli::format_inline(
                              "Assuming TAC allocation in projections is same as mean removals from last 5 historical years"
                            ),
                            type = 'assumption'
        )

        Recent <- .RecentShare(apply(Removals, c(1, 3, 4), sum))
        Hist <- .LogRecentShare(Hist, Recent, 'removals')
        AllocationFleet <- Recent$Share
      }
    }

    dd <- dim(AllocationFleet)
    if (dd[1] != nSim && dd[1] != 1)
      cli::cli_abort('`OM@FleetAllocation` must be a list length `nComplex(OM)` with a `nSim` by `nFleet` matrix  for each stock')

    if (dd[2]!=nFleet)
      cli::cli_abort('`OM@FleetAllocation` must be a list length `nComplex(OM)` with a `nSim` by `nFleet` matrix  for each stock')

    if (any(AllocationFleet<0) || any(!is.finite(AllocationFleet)))
      cli::cli_abort('Values in `OM@FleetAllocation` must be finite and non-negative')

    rsum <- rowSums(AllocationFleet)
    tol  <- sqrt(.Machine$double.eps)
    if (any(abs(rsum - 1) > tol))
      cli::cli_abort(
        c('Values in `OM@FleetAllocation` must sum to 1 across rows',
          'i' = 'Max deviation: {.val {max(abs(rsum - 1))}}')
      )

    dimnames(AllocationFleet) <- list("Sim"=1:dd[1],
                                      "Fleet"=FleetNames(Hist@OM))

    Allocation[[cx]] <- AllocationFleet
  }

  Hist@OM@FleetAllocation <- Allocation
  Hist
}

# Stocks' CatchFrac combined across a complex, weighted by each stock's final-year removals
.ComplexCatchFrac <- function(CatchFrac, Removals, nSim) {
  if (length(CatchFrac) == 1) return(CatchFrac[[1]])

  nYear   <- dim(Removals)[3]
  Weights <- matrix(apply(Removals[, , nYear, , drop = FALSE], c(1, 2), sum), ncol = length(CatchFrac))
  Weights <- Weights[rep_len(seq_len(nrow(Weights)), nSim), , drop = FALSE]
  Total   <- rowSums(Weights)
  Weights[Total == 0, ] <- 1
  Weights <- Weights / rowSums(Weights)

  Out <- 0
  for (k in seq_along(CatchFrac)) {
    CF  <- CatchFrac[[k]]
    CF  <- CF[rep_len(seq_len(nrow(CF)), nSim), , drop = FALSE]
    Out <- Out + Weights[, k] * CF
  }
  unname(Out)
}
