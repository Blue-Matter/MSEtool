.CheckEffortAllocation <- function(Hist) {

  EffortAllocation <- Hist@OM@EffortAllocation
  if (!length(EffortAllocation) && length(Hist@OM@EFactor))
    EffortAllocation <- Hist@OM@EFactor

  nComplex  <- length(Hist@OM@Complexes)
  nFleet    <- nFleet(Hist)
  nSim      <- nSim(Hist)
  HistYears <- Years(Hist, 'H')

  ComplexNames <- names(Hist@OM@Complexes)

  if (!length(EffortAllocation))
    EffortAllocation <- MakeNamedList(ComplexNames)

  if (length(EffortAllocation) != nComplex)
    cli::cli_abort('`EffortAllocation` must be a list length 0 or length `nComplex(OM)`')

  DefaultEffortAllocation <- NULL

  for (cx in 1:nComplex) {
    EffortAllocationFleet <- EffortAllocation[[cx]]

    if (is.null(EffortAllocationFleet)) {

      if (nFleet == 1) {
        EffortAllocationFleet <- matrix(1, nSim, nFleet)
        next
      }

      Hist <- .CaptureLog(Hist,
                       string = cli::format_inline(
                         "`EffortAllocation(OM)` has not been specified for Complex {.val {ComplexNames[cx]}}"
                         ),
                       name = "EffortAllocation",
                       type = 'assumption'
                       )
      Hist <- .CaptureLog(Hist,
                       string = cli::format_inline(
                         "Assuming absolute Effort advice is split across fleets according to mean relative effort over the last 5 historical years"
                         ),
                       type = 'assumption'
      )

      if (is.null(DefaultEffortAllocation)) {
        last_5_years <- utils::tail(seq_len(length(HistYears)), 5)

        effort <- Hist@Effort[, last_5_years, , drop = FALSE]
        effort <- apply(effort, c('Sim', 'Fleet'), sum)
        rel_effort <- effort / apply(effort, 'Sim', sum)

        DefaultEffortAllocation <- matrix(rel_effort, nSim, nFleet)
      }

      EffortAllocationFleet <- DefaultEffortAllocation
    }

    dd <- dim(EffortAllocationFleet)
    if (dd[1] != nSim && dd[1] != 1)
      cli::cli_abort('`OM@EffortAllocation` must be a list length `nComplex(OM)` with an `nSim` by `nFleet` matrix for each complex')

    if (dd[2] != nFleet)
      cli::cli_abort('`OM@EffortAllocation` must be a list length `nComplex(OM)` with an `nSim` by `nFleet` matrix for each complex')

    if (any(EffortAllocationFleet < 0) || any(!is.finite(EffortAllocationFleet)))
      cli::cli_abort('Values in `OM@EffortAllocation` must be positive')

    rsum <- rowSums(EffortAllocationFleet)
    tol  <- sqrt(.Machine$double.eps)
    if (any(abs(rsum - 1) > tol))
      cli::cli_abort(
        c('Values in `OM@EffortAllocation` must sum to 1 across rows',
          'i' = 'Max deviation: {.val {max(abs(rsum - 1))}}')
      )

    dimnames(EffortAllocationFleet) <- list("Sim" = 1:dd[1],
                                            "Fleet" = FleetNames(Hist@OM))

    EffortAllocation[[cx]] <- EffortAllocationFleet
  }

  Hist@OM@EffortAllocation <- EffortAllocation
  Hist
}
