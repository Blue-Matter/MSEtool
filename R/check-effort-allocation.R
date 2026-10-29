.CheckEffortAllocation <- function(Hist) {

  EffortAllocation <- Hist@OM@EffortAllocation
  if (!length(EffortAllocation) && length(Hist@OM@EFactor))
    EffortAllocation <- Hist@OM@EFactor

  nComplex  <- length(Hist@OM@Complexes)
  nFleet    <- nFleet(Hist)
  nSim      <- nSim(Hist)
  HistYears <- Years(Hist, 'H')

  ComplexNames <- ComplexNames(Hist)

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
        effort <- Hist@Effort[, seq_along(HistYears), , drop = FALSE]
        Recent <- .RecentShare(effort)
        Hist <- .LogRecentShare(Hist, Recent, 'effort')
        DefaultEffortAllocation <- Recent$Share
      }

      EffortAllocationFleet <- DefaultEffortAllocation
    }

    dd <- dim(EffortAllocationFleet)
    if (dd[1] != nSim && dd[1] != 1)
      cli::cli_abort('`OM@EffortAllocation` must be a list length `nComplex(OM)` with an `nSim` by `nFleet` matrix for each complex')

    if (dd[2] != nFleet)
      cli::cli_abort('`OM@EffortAllocation` must be a list length `nComplex(OM)` with an `nSim` by `nFleet` matrix for each complex')

    if (any(EffortAllocationFleet < 0) || any(!is.finite(EffortAllocationFleet)))
      cli::cli_abort('Values in `OM@EffortAllocation` must be finite and non-negative')

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

.CheckZeroEffort <- function(OM) {
  Fished <- rep(FALSE, nSim(OM))
  for (FleetList in OM@Fleet) {
    for (Fleet in FleetList) {
      Effort <- Fleet@Effort@Effort
      if (!is.numeric(Effort)) next
      Effort <- matrix(Effort, nrow = NROW(Effort))
      Total  <- rowSums(abs(Effort))
      Fished <- Fished | is.na(Total) | Total > 0
    }
  }
  ZeroSims <- which(!Fished)
  if (!length(ZeroSims)) return(invisible(OM))

  cli::cli_abort(c(
    'x' = 'Historical effort is zero for all fleets in every year in {cli::qty(length(ZeroSims))}Sim{?s} {ZeroSims}.',
    'i' = 'Correct the {.code Effort} inputs, or remove the simulations with {.code Subset(OM, Sims = ...)}.'
  ), call = NULL)
}

# Fleet shares over the last `nYear` years, or the last `nYear` years with a positive total
.RecentShare <- function(x, nYear = 5) {
  nSim   <- dim(x)[1]
  nFleet <- dim(x)[3]
  Total  <- apply(x, c(1, 2), sum)
  Share  <- matrix(NA_real_, nSim, nFleet)
  Shifted <- Equal <- integer(0)

  for (s in seq_len(nSim)) {
    Yrs <- utils::tail(seq_len(ncol(Total)), nYear)
    if (!sum(Total[s, Yrs]) > 0) {
      Yrs <- utils::tail(which(Total[s, ] > 0), nYear)
      if (length(Yrs)) Shifted <- c(Shifted, s) else Equal <- c(Equal, s)
    }
    if (!length(Yrs)) {
      Share[s, ] <- 1 / nFleet
      next
    }
    ByFleet <- colSums(matrix(x[s, Yrs, ], length(Yrs), nFleet))
    Share[s, ] <- ByFleet / sum(ByFleet)
  }
  list(Share = Share, Shifted = Shifted, Equal = Equal)
}

.LogRecentShare <- function(Hist, Recent, what) {
  if (length(Recent$Shifted))
    Hist <- .CaptureLog(Hist,
                        string = cli::format_inline("Total {what} is zero in the last 5 historical years for {cli::qty(length(Recent$Shifted))}Sim{?s} {Recent$Shifted}; using the last 5 years with non-zero {what}"),
                        type = 'assumption')
  if (length(Recent$Equal))
    Hist <- .CaptureLog(Hist,
                        string = cli::format_inline("Total {what} is zero in all historical years for {cli::qty(length(Recent$Equal))}Sim{?s} {Recent$Equal}; splitting equally across fleets"),
                        type = 'assumption')
  Hist
}
