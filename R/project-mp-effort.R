
Update_Effort <- function(Proj, Year, AdviceSimList, LastAdviceSimList, 
                          YearsHist, YearsProj, Areas, FleetNames) {

  nSim <- Proj@OM@nSim
  
  for (sim in seq_len(nSim)) {
    Proj <- Update_Effort_Sim(
      Proj = Proj,
      sim = sim,
      Year = Year,
      YearsHist = YearsHist,
      YearsProj = YearsProj,
      AdviceList = AdviceSimList[[sim]],
      LastAdviceList = LastAdviceSimList[[sim]],
      FleetNames = FleetNames,
      Complexes = Proj@OM@Complexes,
      Areas = Areas
    )
  }
  
  Proj

}

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
  
  nComplex <- length(Complexes)
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  AllYears <- c(YearsHist, YearsProj)
  TSIndex <- match(Year, AllYears)
  
  Distribution <- MakeNamedList(names(Complexes))
  
  for (i in seq_len(nComplex)) {
    stocks <- Complexes[[i]]
    Advice <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    
    if (is.null(Advice@Effort))
      next()
    
    if (UnchangedManagement(Advice, AdvicePrevious, 'Effort'))
      next()
    
    
    # Convert from Relative to Absolute Effort
    Advice <- Convert_Effort_Abs(Proj, sim, Advice)
    
    # Distribute Effort over Areas if specified in MP 
    temp <- Distribute_Effort_Area(Proj,
                                   sim,
                                   TSIndex,
                                   Advice,
                                   nFleet,
                                   nArea)
    
    Distribution[[i]] <- temp$Distribution
    AdviceList[[i]] <- temp$Advice
  }
  
  # do Effort Regulation exist?
  EffortExist <- purrr::map(AdviceList, \(Advice) {
    !is.null(Advice@Effort)
  }) |> unlist()
  
  if (!any(EffortExist))
    return(Proj)
  
  
  # Determine minimum effort by complex
  EffortArray <- purrr::map(AdviceList, slot, 'Effort') |> List2Array() # nFleet x nComplex
  MinEffortInd <- apply(EffortArray, 1, which.min) |> as.numeric() # complex with lowest effort
  row_idx <- seq_len(nrow(EffortArray))
  MinEffortValues <- EffortArray[cbind(row_idx, MinEffortInd)] # lowest prescribed effort by fleet
  
  ProjInd <- TSIndex:length(AllYears) # all future time steps
  n <- length(ProjInd)
  Proj@Effort[sim,ProjInd,] <- matrix(MinEffortValues, nrow = n, ncol = nFleet, byrow = TRUE)
  
  if (length(Distribution)) {
    # spatial distribution of effort has been specified 
    # distribute effort for each fleet according to minimum effort specified across stocks
    for (fl in 1:nFleet) {
      complex_ind <- MinEffortInd[fl]
      if (length(Distribution)>=fleet_ind) {
        Proj@Distribution[sim, ProjInd, fl] <-  Distribution[[complex_ind]]  
      }
    }
  }

  Proj
}

# Distribute Effort over areas if specified 
Distribute_Effort_Area <- function(Proj,
                                   sim,
                                   TSIndex,
                                   Advice,
                                   nFleet,
                                   nArea) {
  
  if (!is.array(Advice@Effort)) 
    return(list(Proj = Proj, 
                Advice = Advice))
  
  dd <- dim(Advice@Effort)
  if (length(dd)==1)
    return(list(Proj = Proj, 
                Advice = Advice))
  
  if (!all(dim(Advice@Effort)==c(nFleet, nArea)))
    stop("If `Advice@Effort` is a matrix, it must have nFleet rows and nArea columns")
  
  dd <- dim(Proj@Distribution)
  ProjInd <- TSIndex:dd[2] # all future time steps
  n <- length(ProjInd)
  
  mat <- Rel_Area_Effort(Advice@Effort) # fleet x area
  arr <- array(rep(mat, each = n), dim = c(n, nrow(mat), ncol(mat))) # year x fleet x area
  Distribution <- Proj@Distribution[sim, ProjInd, ,, drop=FALSE] |> abind::adrop(1:2)
  Distribution <- Distribution * arr
  
  Advice@Effort <- rowSums(Advice@Effort)

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
                               Advice) {
  
  if (is.array(Advice@Effort))
    return(Advice)
  
  if (Advice@EffType == 'Abs')
    return(Advice)
  
  dd <- dim(Proj@Effort)
  LastHistEffort <- Proj@Effort[sim, dd[2],] 
  
  Advice@Effort <- Advice@Effort *  LastHistEffort/sum(LastHistEffort)
  Advice
}

