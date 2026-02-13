Update_Closure <- function(Proj, Year, AdviceSimList, LastAdviceSimList, YearsProj, Areas, 
                           FleetNames, StockNames) {
  
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
      FleetNames = FleetNames,
      StockNames = StockNames,
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
                               StockNames,
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
    
    CheckClosureDimensions(Closure=Advice@Closure, FleetNames, Areas)
    
    FutureYears <- YearsProj[YearsProj>=Year]
    NewClosure <- Advice@Closure |>
      AddDimension("Year", Year, pos=1) |> 
      ExtendYears(Years=FutureYears) |>
      AddDimension("Sim", x, pos=1)
    
    # apply closure to all future time steps
    for (st in stocks) {
      for (fl in seq_along(FleetNames)) {
        Current <- Proj@OM@Fleet[[st]][[fl]]@Closure
        
        if (dim(Current)[1] < sim)
          Current <- ExtendSims(Current, nSim)
        
        ArrayFill(Current) <- DropDimension(NewClosure, 'Fleet', FALSE)
        Proj@OM@Fleet[[st]][[fl]]@Closure <- Current
        ArrayFill(Proj@Misc$Closure) <- AddDimension(NewClosure, 'Stock',
                                                     val=StockNames[st],
                                                     pos=2)
     
        
  
      }
    }
  }
  Proj
}



CheckClosureDimensions <- function(Closure, FleetNames, Areas) {
  if (dim(Closure)[1] != length(FleetNames))
    stop("Closure dimension does not match number of fleets")
  
  if (dim(Closure)[2] != length(Areas))
    stop("Closure dimension does not match number of areas")
  
  invisible(TRUE)
}
