#' Update area closures across all simulations
#'
#' Loops over simulations and delegates to `.UpdateClosureSim()`. Returns
#' `Proj` unchanged when there is only one area.
#'
#' @param Proj A `Proj` object.
#' @param Year Integer. Current projection year.
#' @param AdviceSimList Nested list of `advice` objects, indexed by sim then complex.
#' @param LastAdviceSimList Same structure as `AdviceSimList` for the previous year.
#' @param YearsHist Integer vector of historical years (unused here, kept for
#'   consistent `update_funs` signature).
#' @param YearsProj Integer vector of projection years.
#' @param Areas Integer vector of area indices.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names.
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateClosure <- function(Proj, 
                           Year, 
                           AdviceSimList, 
                           LastAdviceSimList, 
                           YearsHist,
                           YearsProj,
                           Areas, 
                           FleetNames, 
                           StockNames) {
  
  if (length(Areas) < 2)
    return(Proj)
  
  if (.AllAdviceNull(AdviceSimList, 'Closure'))
    return(Proj)
  
  for (sim in seq_len(Proj@OM@nSim)) {
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]
    
    Proj <- .UpdateClosureSim(
      Proj           = Proj,
      sim            = sim,
      Year           = Year,
      YearsProj      = YearsProj,
      AdviceList     = AdviceList,
      LastAdviceList = LastAdviceList,
      FleetNames     = FleetNames,
      StockNames     = StockNames,
      Complexes      = Proj@OM@Complexes,
      Areas          = Areas,
      nSim           = Proj@OM@nSim
    )
  }
  
  Proj
}

#' Update area closures for a single simulation
#'
#' Applies closure advice to all future projection years for each complex,
#' updating both `Proj@OM@Fleet` and `Proj@Misc$Closure`.
#' Skips a complex when management is unchanged, closure is `NULL`, or the
#' advice object is not of class `"advice"`.
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param Year Integer. Current projection year.
#' @param YearsProj Integer vector of projection years.
#' @param AdviceList List of `advice` objects for this simulation, one per complex.
#' @param LastAdviceList Same structure as `AdviceList` for the previous year.
#' @param FleetNames Character vector of fleet names.
#' @param StockNames Character vector of stock names.
#' @param Complexes List mapping complex indices to stock indices.
#' @param Areas Integer vector of area indices.
#' @param nSim Integer. Total number of simulations.
#' @return Updated `Proj` object.
#' @keywords internal
.UpdateClosureSim <- function(Proj,
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
  
 
  FutureYears <- YearsProj[YearsProj >= Year]
  nComplex <- length(Complexes)
  for (i in seq_len(nComplex)) {
    stocks         <- Complexes[[i]]
    Advice         <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]

    if (!inherits(Advice, 'advice')) next    
    if (is.null(Advice@Closure)) next
    if (.UnchangedManagement(Advice, AdvicePrevious, 'Closure')) next

    .CheckClosureDimensions(Closure=Advice@Closure, FleetNames, Areas)
    
    NewClosure <- Advice@Closure |>
      AddDimension("Year", Year, pos=1) |> 
      ExtendYears(Years=FutureYears) |>
      AddDimension("Sim", sim, pos=1)
    
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


.CheckClosureDimensions <- function(Closure, FleetNames, Areas) {
  if (dim(Closure)[1] != length(FleetNames))
    stop("Closure dimension does not match number of fleets")
  
  if (dim(Closure)[2] != length(Areas))
    stop("Closure dimension does not match number of areas")
  
  invisible(TRUE)
}
