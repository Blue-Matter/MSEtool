Update_Closure <- function(Proj, Year, AdviceSimList, LastAdviceSimList) {
  nArea <- nArea(Proj)
  
  if (nArea<2)
    return(Proj)
  
  Areas <- seq_len(nArea(Proj))
  
  nSim <- Proj@OM@nSim
  FleetNames <- FleetNames(Proj)
  if (is.list(FleetNames))
    FleetNames <- FleetNames[[1]]
  

  YearsProj <- Years(Proj, 'P')
  Complexes <- Proj@OM@Complexes
  
  for (sim in seq_len(nSim)) {
    Proj <- Update_Closure_Sim(
      Proj,
      sim,
      Year,
      YearsProj,
      AdviceSimList[[sim]],
      LastAdviceSimList[[sim]],
      FleetNames,
      Complexes,
      Areas,
      nSim
    )
  }
  
  Proj
}

Update_Closure_Sim <- function(Proj,
                               sim,
                               Year,
                               YearsProj,
                               AdviceList,
                               LastAdviceList,
                               FleetNames,
                               Complexes,
                               Areas,
                               nSim) {
  
  nComplex <- length(AdviceList)
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  nSim 
  
  for (i in seq_len(nComplex)) {
    stocks <- Complexes[[i]]
    Advice <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    if (UnchangedManagement(Advice, AdvicePrevious, 'Closure'))
      next()
    
    if (is.null(Advice@Closure))
      next()
    
    CheckClosureDimensions(Advice, FleetNames, Areas)
    
    FutureYears <- YearsProj[YearsProj>=Year]
    NewClosure <- Advice@Closure |>
      AddDimension("Year", Year, pos=1) |> 
      ExtendYears(Years=FutureYears) |>
      AddDimension("Sim", x, pos=1)
    
    for (st in stocks) {
      for (fl in seq_along(FleetNames)) {
        Proj <- ApplyClosureToFleet(
          Proj,
          stock = st,
          fleet = fl,
          sim = sim,
          nSim = nSim,
          NewClosure = NewClosure[,,fl,, drop = FALSE] |>
            abind::adrop(drop = 3)
        )
      }
    }
  }
  Proj
}

ApplyClosureToFleet <- function(Proj,
                                stock,
                                fleet,
                                sim,
                                nSim,
                                NewClosure) {
  
  Current <- Proj@OM@Fleet[[stock]][[fleet]]@Closure
  
  if (dim(Current)[1] < sim)
    Current <- ExtendSims(Current, nSim)
  
  ArrayFill(Current) <- NewClosure
  
  Proj@OM@Fleet[[stock]][[fleet]]@Closure <- Current
  Proj
}

CheckClosureDimensions <- function(Closure, FleetNames, Areas) {
  if (dim(Closure)[3] != length(FleetNames))
    stop("Closure dimension does not match number of fleets")
  
  if (dim(Closure)[4] != length(Areas))
    stop("Closure dimension does not match number of areas")
  
  invisible(TRUE)
}
