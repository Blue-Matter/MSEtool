#' Update fishing effort across all simulations
#'
#' Loops over simulations and delegates to [Update_Effort_Sim()].
#'
#' @param Proj A `Proj` object.
#' @param Year Integer. Current projection year.
#' @param AdviceSimList Nested list of `advice` objects, indexed by sim then complex.
#' @param LastAdviceSimList Same structure as `AdviceSimList` for the previous year.
#' @param YearsHist Integer vector of historical years.
#' @param YearsProj Integer vector of projection years.
#' @param Areas Integer vector of area indices.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names (unused here, kept for
#'   consistent `update_funs` signature).
#' @return Updated `Proj` object.
#' @keywords internal
Update_Effort <- function(Proj,
                          Year, 
                          AdviceSimList, 
                          LastAdviceSimList, 
                          YearsHist,
                          YearsProj, 
                          Areas, 
                          FleetNames,
                          StockNames) {

  nSim <- Proj@OM@nSim
  
  if (AllAdviceNull(AdviceSimList, 'Effort'))
    return(Proj)
  
  for (sim in seq_len(nSim)) {
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]
    
    Proj <- Update_Effort_Sim(
      Proj           = Proj,
      sim            = sim,
      Year           = Year,
      YearsHist      = YearsHist,
      YearsProj      = YearsProj,
      AdviceList     = AdviceList,
      LastAdviceList = LastAdviceList,
      FleetNames     = FleetNames,
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
#' `Proj@Effort` and `Proj@Distribution` for all future time steps.
#' Skips complexes where management is unchanged, effort is `NULL`, or the
#' advice object is not of class `"advice"`.
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param Year Integer. Current projection year.
#' @param YearsHist Integer vector of historical years.
#' @param YearsProj Integer vector of projection years.
#' @param AdviceList List of `advice` objects for this simulation, one per complex.
#' @param LastAdviceList Same structure as `AdviceList` for the previous year.
#' @param FleetNames Character vector of fleet names.
#' @param Complexes List mapping complex indices to stock indices.
#' @param Areas Integer vector of area indices.
#' @return Updated `Proj` object.
#' @keywords internal
Update_Effort_Sim <- function(Proj,
                              sim,
                              Year,
                              YearsHist,
                              YearsProj,
                              AdviceList,
                              LastAdviceList,
                              FleetNames,
                              Complexes,
                              Areas) {
  
  nFleet   <- length(FleetNames)
  nArea    <- length(Areas)
  AllYears <- c(YearsHist, YearsProj)
  TSIndex  <- match(Year, AllYears)
  ProjInd  <- TSIndex:length(AllYears)
  nComplex <- length(Complexes)
  
  Distribution <- MakeNamedList(names(Complexes))
  
  for (i in seq_len(nComplex)) {
    Advice         <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    
    if (!inherits(Advice, "advice"))                              next
    if (is.null(Advice@Effort))                                   next
    
    if (!is.array(Advice@Effort) && length(Advice@Effort)==1) 
      Advice@Effort <- rep(Advice@Effort,nFleet)[seq_len(nFleet)]
    
    # if (UnchangedManagement(Advice, AdvicePrevious, "Effort"))    next
    
    # Convert from Relative to Absolute Effort
    Advice <- Convert_Effort_Abs(Proj, sim, Advice, YearsHist)
      
    # Distribute Effort over Areas if specified in MP 
    temp <- Distribute_Effort_Area(Proj, sim, TSIndex, Advice, nFleet, nArea, FleetNames, YearsHist, YearsProj)
    Distribution[[i]] <- temp$Distribution
    AdviceList[[i]] <- temp$Advice
  }
  
  # do Effort Regulation exist?
  effort_exists <- purrr::map_lgl(AdviceList, \(a) {
    inherits(a, 'advice') && !is.null(a@Effort)
  })
  
  if (!any(effort_exists))
    return(Proj)

  # Determine minimum effort across complexes, per fleet
  EffortArray    <- purrr::map(AdviceList, slot, "Effort") |> List2Array('Stock') # nFleet x nComplex
  MinEffortInd   <- apply(EffortArray, 1, which.min) |> as.numeric()       # complex index with lowest effort per fleet
  MinEffortValues <- EffortArray[cbind(seq_len(nrow(EffortArray)), MinEffortInd)]
  
  Proj@Effort[sim, ProjInd, ] <- matrix(MinEffortValues,
                                        nrow  = length(ProjInd),
                                        ncol  = nFleet,
                                        byrow = TRUE)
  
  Distribution <- Distribution[[min(MinEffortInd)]]
  
  # Apply spatial distribution if specified
  # TODO - review indexing for multi-complex spatial effort distribution
  if (!is.null(Distribution)) {
    ArrayFill(Proj@Distribution) <- Distribution
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
Distribute_Effort_Area <- function(Proj,
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
    Advice@Effort <- array(Advice@Effort, dimnames=list(Fleet=FleetNames))
    return(list(Distribution = NULL, Advice = Advice))
  }   
  
  if (length(dim(Advice@Effort)) == 1) {
    Advice@Effort <- array(Advice@Effort, dimnames=list(Fleet=FleetNames))
    return(list(Distribution = NULL, Advice = Advice))
  } 
  
  if (!all(dim(Advice@Effort) == c(nFleet, nArea)))
    stop("If `Advice@Effort` is a matrix it must have dimensions ",
         "[nFleet, nArea] = [", nFleet, ", ", nArea, "]")
  

  ProjInd      <- TSIndex:dim(Proj@Distribution)[2]
  n            <- length(ProjInd)
  mat          <- Rel_Area_Effort(Advice@Effort)                          # fleet x area
  Distribution          <- array(rep(mat, each = n), dim = c(n, nrow(mat), ncol(mat))) # year x fleet x area
  dimnames(Distribution) <- list(Year=c(YearsHist,YearsProj)[ProjInd],
                        Fleet=FleetNames,
                        Area=seq_len(nArea))
  Distribution <- AddDimension(Distribution, name='Sim', val=sim, pos=1)
  
  Advice@Effort <- array(rowSums(Advice@Effort))
  dimnames(Advice@Effort) <- list(Fleet=FleetNames)
  
  list(Distribution = Distribution, Advice = Advice)
}


Rel_Area_Effort <- function(mat) {
  row_sums <- rowSums(mat)
  rel_mat <- mat
  nonzero <- row_sums != 0
  rel_mat[nonzero, ] <- mat[nonzero, , drop = FALSE] / row_sums[nonzero]
  rel_mat
}


Convert_Effort_Abs <- function(Proj,
                               sim,
                               Advice, 
                               YearsHist) {
  
  if (is.array(Advice@Effort)) return(Advice)
  
  if (Advice@EffType == 'Abs') return(Advice)
  
 
  LastHistEffort <- Proj@Effort[sim, length(YearsHist), ]
  Advice@Effort  <- Advice@Effort * LastHistEffort 
  Advice
}

