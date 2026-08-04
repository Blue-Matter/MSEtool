#' Update fishing effort across all simulations
#'
#' Loops over simulations and delegates to `.UpdateEffortSim()`.
#'
#' @param Proj A `Proj` object.
#' @param Year Integer. Current projection year.
#' @param AdviceSimList Nested list of `advice` objects, indexed by sim then complex.
#' @param LastAdviceSimList Same structure as `AdviceSimList` for the previous year.
#' @param YearsHist Integer vector of historical years.
#' @param YearsProj Integer vector of projection years.
#' @param Areas Integer vector of area indices.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names. Used to (re)optimise
#'   stock-specific targeting weights for multi-stock OMs - see
#'   `.OptTargetingMultiStock()`.
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateEffort <- function(Proj,
                          Year,
                          AdviceSimList,
                          LastAdviceSimList,
                          YearsHist,
                          YearsProj,
                          Areas,
                          FleetNames,
                          StockNames) {

  nSim <- Proj@OM@nSim

  if (.AllAdviceNull(AdviceSimList, 'Effort'))
    return(Proj)

  for (sim in seq_len(nSim)) {
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]

    Proj <- .UpdateEffortSim(
      Proj           = Proj,
      sim            = sim,
      Year           = Year,
      YearsHist      = YearsHist,
      YearsProj      = YearsProj,
      AdviceList     = AdviceList,
      LastAdviceList = LastAdviceList,
      FleetNames     = FleetNames,
      StockNames     = StockNames,
      Complexes      = Proj@OM@Complexes,
      Areas          = Areas
    )
  }

  Proj

}

#' Update fishing effort for a single simulation
#'
#' Converts effort advice to absolute values, optionally distributes it over
#' areas, then writes the minimum prescribed effort across complexes into
#' `Proj@Effort` and `Proj@Distribution` for all future time steps. Skips
#' complexes where management is unchanged, effort is `NULL`, or the advice
#' object is not of class `"advice"`. For multi-stock OMs, also
#' (re)optimises each fleet's stock-specific targeting weights for the
#' current year at this fixed effort - see `.OptTargetingMultiStock()`.
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param Year Integer. Current projection year.
#' @param YearsHist Integer vector of historical years.
#' @param YearsProj Integer vector of projection years.
#' @param AdviceList List of `advice` objects for this simulation, one per complex.
#' @param LastAdviceList Same structure as `AdviceList` for the previous year.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names.
#' @param Complexes List mapping complex indices to stock indices.
#' @param Areas Integer vector of area indices.
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateEffortSim <- function(Proj,
                              sim,
                              Year,
                              YearsHist,
                              YearsProj,
                              AdviceList,
                              LastAdviceList,
                              FleetNames,
                              StockNames,
                              Complexes,
                              Areas,
                              lambda_scale = 1,
                              n_recent     = 5,
                              maxEval      = 500) {

  nFleet   <- length(FleetNames)
  nArea    <- length(Areas)
  AllYears <- c(YearsHist, YearsProj)
  TSIndex  <- match(Year, AllYears)
  ProjInd  <- TSIndex:length(AllYears)
  nComplex <- length(Complexes)
  nSeason  <- max(1L, as.integer(Proj@OM@Seasons))

  Distribution <- MakeNamedList(names(Complexes))

  # Relative effort is anchored to the last complete historical year; its
  # seasonal shape is reapplied below so 'Rel' advice keeps the seasonal
  # pattern rather than repeating the final season
  LastEffort <- .LastYearEffort(Proj, sim, length(YearsHist), nFleet, nSeason)

  # EffType per fleet per complex, to identify which fleets take the shape
  EffTypeMat <- matrix('Abs', nrow = nFleet, ncol = nComplex)

  for (i in seq_len(nComplex)) {
    Advice         <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]

    if (!inherits(Advice, "advice"))                              next
    if (is.null(Advice@Effort))                                   next

    # Scalar effort expanded to nFleet (skip if already a matrix)
    if (!is.array(Advice@Effort) && length(Advice@Effort)==1)
      Advice@Effort <- rep(Advice@Effort,nFleet)[seq_len(nFleet)]

    EffType <- Advice@EffType
    if (!length(EffType)) EffType <- 'Rel'
    EffTypeMat[, i] <- .RecycleToFleets(EffType, nFleet, 'EffType')

    # Convert relative fleets to absolute
    Advice <- .ConvertEffortAbs(Proj, sim, Advice, YearsHist, nFleet,
                                LastEffortLevel = LastEffort$level)

    temp <- .DistributeEffortArea(Proj, sim, TSIndex, Advice, nFleet,
                                   nArea, FleetNames,
                                   YearsHist, YearsProj)
    Distribution[[i]] <- temp$Distribution
    Advice          <- temp$Advice


    complex_name <- names(Complexes)[i]
    wrapped <- .ApplyImplementationError(
      setNames(list(Advice@Effort), complex_name),
      Proj, FleetNames, complex_name, sim, Year, 'Effort'
    )
    Advice@Effort <- wrapped[[1]]

    AdviceList[[i]] <- Advice
  }
  
  # do Effort Regulation exist?
  effort_exists <- purrr::map_lgl(AdviceList, \(a) {
    inherits(a, 'advice') && !is.null(a@Effort)
  })

  if (!any(effort_exists))
    return(Proj)

  # Determine minimum effort across complexes with existing Effort advice, per fleet.
  effort_idx      <- which(effort_exists)
  EffortArray     <- purrr::map(AdviceList[effort_idx], slot, "Effort") |> List2Array('Stock') 

  EffectiveArray <- EffortArray

  if (nComplex > 1L) {
    Compliance <- .ResolveComplianceMatrix(Proj, FleetNames,
                                           names(Complexes)[effort_idx],
                                           sim, Year, 'Effort')
    compset <- !is.na(Compliance)
    EffectiveArray[compset] <- EffortArray[compset] / pmax(Compliance[compset], 1e-3)
  }

  MinEffortInd    <- apply(EffectiveArray, 1, which.min) |> as.numeric()
  MinEffortValues <- EffectiveArray[cbind(seq_len(nrow(EffectiveArray)), MinEffortInd)]
  MinEffortInd    <- effort_idx[MinEffortInd]

  EffortMat <- matrix(MinEffortValues,
                      nrow  = length(ProjInd),
                      ncol  = nFleet,
                      byrow = TRUE)

  # Reapply the historical seasonal shape to fleets whose binding advice was
  # relative. Absolute advice is written as given - the MP sets its own pattern.
  if (nSeason > 1L) {
    RelFleet <- vapply(seq_len(nFleet),
                       \(fl) identical(EffTypeMat[fl, MinEffortInd[fl]], 'Rel'),
                       logical(1))
    if (any(RelFleet)) {
      seas <- ((ProjInd - 1L) %% nSeason) + 1L
      EffortMat[, RelFleet] <- EffortMat[, RelFleet, drop = FALSE] *
        LastEffort$shape[seas, RelFleet, drop = FALSE]
    }
  }

  Proj@Effort[sim, ProjInd, ] <- EffortMat

  # Apply spatial distribution if specified
  # TODO - review indexing for multi-complex spatial effort distribution
  if (!is.null(Distribution) && length(Distribution)) {
    Distribution <- Distribution[[min(MinEffortInd)]]
    ArrayFill(Proj@Distribution) <- Distribution
  }

  # For multi-stock OMs, (re)optimise this fleet's stock-specific targeting
  # weights at the now-fixed effort
  if (isTRUE(Proj@Misc$StockTargetingFlag == 1) && TSIndex > 1) {
    lambda <- .ResolveLambda(Proj, sim, TSIndex, StockNames, FleetNames,
                            lambda_scale, n_recent)

    # Single-sim slice for the repeated fishery-dynamics calls 
    ProjSim <- .SliceSim(Proj, sim, .DynamicsProbeSlots)

    result <- .OptTargetingMultiStock(
      Proj       = ProjSim,
      Year       = Year,
      TSIndex    = TSIndex,
      sim        = 1L,
      StockNames = StockNames,
      FleetNames = FleetNames,
      Effort     = MinEffortValues,
      lambda     = lambda,
      n_recent   = n_recent,
      maxEval    = maxEval
    )

    Proj@Misc$StockTargeting[sim, , , TSIndex] <- t(result$Delta)
  }

  Proj
}

#' Distribute effort over areas and return relative area allocation
#'
#' If `Advice@Effort` is a matrix of dimensions `[nFleet, nArea]`, computes
#' relative area allocations, stores them in `Distribution`, and collapses
#' `Advice@Effort` to a per-fleet total. Returns the advice unchanged when
#' effort is not a matrix or has only one dimension.
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param TSIndex Integer. Time-step index of the current year in `AllYears`.
#' @param Advice An `advice` object.
#' @param nFleet Integer. Number of fleets.
#' @param nArea Integer. Number of areas.
#' @return A named list with elements `Distribution` (array or `NULL`) and
#'   `Advice` (updated `advice` object).
#' @keywords internal
.DistributeEffortArea <- function(Proj,
                                   sim,
                                   TSIndex,
                                   Advice,
                                   nFleet,
                                   nArea,
                                   FleetNames,
                                   YearsHist,
                                   YearsProj) {
  
  no_change <- list(Distribution = NULL, Advice = Advice)
  
  if (!is.array(Advice@Effort))  {
    Advice@Effort <- array(.RecycleToFleets(Advice@Effort, nFleet, "Advice@Effort"), dimnames=list(Fleet=FleetNames))
    return(list(Distribution = NULL, Advice = Advice))
  }

  if (length(dim(Advice@Effort)) == 1) {
    Advice@Effort <- array(.RecycleToFleets(Advice@Effort, nFleet, "Advice@Effort"), dimnames=list(Fleet=FleetNames))
    return(list(Distribution = NULL, Advice = Advice))
  }
  
  if (!all(dim(Advice@Effort) == c(nFleet, nArea)))
    stop("If `Advice@Effort` is a matrix it must have dimensions ",
         "[nFleet, nArea] = [", nFleet, ", ", nArea, "]")
  

  ProjInd      <- TSIndex:dim(Proj@Distribution)[2]
  n            <- length(ProjInd)
  mat          <- .RelAreaEffort(Advice@Effort)                          # fleet x area
  Distribution          <- array(rep(mat, each = n), dim = c(n, nrow(mat), ncol(mat))) # year x fleet x area
  dimnames(Distribution) <- list(Year=c(YearsHist,YearsProj)[ProjInd],
                        Fleet=FleetNames,
                        Area=seq_len(nArea))
  Distribution <- AddDimension(Distribution, name='Sim', val=sim, pos=1)
  
  Advice@Effort <- array(rowSums(Advice@Effort))
  dimnames(Advice@Effort) <- list(Fleet=FleetNames)
  
  list(Distribution = Distribution, Advice = Advice)
}


.RelAreaEffort <- function(mat) {
  row_sums <- rowSums(mat)
  rel_mat <- mat
  nonzero <- row_sums != 0
  rel_mat[nonzero, ] <- mat[nonzero, , drop = FALSE] / row_sums[nonzero]
  rel_mat
}


#' Effort level and seasonal shape from the last complete historical year
#'
#' Returns each fleet's mean effort over the last complete historical year, and
#' that year's seasonal shape normalised to a mean of 1. For annual models the
#' shape is a single 1 and `level` is the final year's effort, so behaviour is
#' unchanged.
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param nHistTS Integer. Number of historical time steps.
#' @param nFleet Integer. Number of fleets.
#' @param nSeason Integer. Seasons per year.
#' @return List with `level` (length `nFleet`) and `shape` (`nSeason` x `nFleet`).
#' @keywords internal
.LastYearEffort <- function(Proj, sim, nHistTS, nFleet, nSeason) {
  nSeason <- max(1L, as.integer(nSeason))
  idx     <- seq(max(1L, nHistTS - nSeason + 1L), nHistTS)

  E     <- matrix(Proj@Effort[sim, idx, ], nrow = length(idx), ncol = nFleet)
  level <- colMeans(E)
  shape <- sweep(E, 2, ifelse(level > 0, level, 1), "/")

  list(level = level, shape = shape)
}

.ConvertEffortAbs <- function(Proj,
                               sim,
                               Advice,
                               YearsHist,
                               nFleet,
                               LastEffortLevel = NULL) {

  if (is.array(Advice@Effort) && length(dim(Advice@Effort)) > 1)
    return(Advice)

  # Recycle EffType to nFleet
  eff_type <- .RecycleToFleets(Advice@EffType, nFleet, 'EffType')

  if (all(eff_type == 'Abs'))
    return(Advice)

  # relative to the mean over the last complete historical year; the seasonal
  # shape is reapplied by .UpdateEffortSim() when effort is written forward
  if (is.null(LastEffortLevel))
    LastEffortLevel <- Proj@Effort[sim, length(YearsHist), ]

  effort <- rep(Advice@Effort, nFleet)[seq_len(nFleet)]

  rel_idx          <- eff_type == 'Rel'
  effort[rel_idx]  <- effort[rel_idx] * LastEffortLevel[rel_idx]

  Advice@Effort <- effort
  Advice
}
