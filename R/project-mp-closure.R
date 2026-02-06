Update_Closure <- function(Proj, Year, AdviceSimList, LastAdviceSimList, YearsProj, Areas, FleetNames) {
  
  nArea <- length(Areas)
  
  if (nArea<2)
    return(Proj)
  
  nSim <- Proj@OM@nSim
  
  for (sim in seq_len(nSim)) {
    Proj <- Update_Closure_Sim(
      Proj=Proj,
      sim=sim,
      Year=Year,
      YearsProj=YearsProj,
      AdviceList=AdviceSimList[[sim]],
      LastAdviceList=LastAdviceSimList[[sim]],
      FleetNames =FleetNames,
      Complexes=Proj@OM@Complexes,
      Areas = Areas,
      nSim = Proj@OM@nSim
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
  
  stop("Need to use Proj@Misc@Closure instead")
  
  Current <- Proj@OM@Fleet[[stock]][[fleet]]@Closure
  
  if (dim(Current)[1] < sim)
    Current <- ExtendSims(Current, nSim)
  
  ArrayFill(Current) <- NewClosure
  
  Proj@Misc$Closure 
  
  
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
