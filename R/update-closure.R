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
                           StockNames,
                           EverySeason = FALSE) {
  
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
      ArrayFill(Proj@Misc$Closure) <- AddDimension(NewClosure, 'Stock',
                                                   val=StockNames[st],
                                                   pos=2)
      for (fl in seq_along(FleetNames)) {
        Current <- Proj@OM@Fleet[[st]][[fl]]@Closure

        if (dim(Current)[1] < sim)
          Current <- ExtendSims(Current, nSim)

        ArrayFill(Current) <- DropDimension(NewClosure[, , fl, , drop = FALSE], 'Fleet', FALSE)

        Proj@OM@Fleet[[st]][[fl]]@Closure <- Current
      }
    }
  }
  Proj <- .MaskClosedDistribution(Proj, sim, match(Year, dimnames(Proj@Distribution)$Year):dim(Proj@Distribution)[2])
  Proj
}


.CheckClosureDimensions <- function(Closure, FleetNames, Areas) {
  if (dim(Closure)[1] != length(FleetNames))
    stop("Closure dimension does not match number of fleets")
  
  if (dim(Closure)[2] != length(Areas))
    stop("Closure dimension does not match number of areas")
  
  invisible(TRUE)
}

# [Year, Fleet, Area] TRUE where the fleet is closed for every stock it can catch (q > 0)
.ClosedFleetAreas <- function(Closure, q, sim, Years) {
  Ind <- \(x, n) {
    i <- match(as.character(Years), dimnames(x)$Year)
    i[is.na(i)] <- n
    i
  }
  cl <- Closure[min(sim, dim(Closure)[1]), , Ind(Closure, dim(Closure)[3]), , , drop = FALSE]
  qq <- q[min(sim, dim(q)[1]), , Ind(q, dim(q)[3]), , drop = FALSE]
  dd <- dim(cl)[3:5]
  Open <- Catch <- array(FALSE, dd)
  for (st in seq_len(dim(cl)[2])) {
    Fishes <- array(qq[1, st, , ] > 0, dd)
    Fishes[is.na(Fishes)] <- FALSE
    Catch <- Catch | Fishes
    Open  <- Open | (Fishes & array(cl[1, st, , , ], dd) > 0)
  }
  Catch & !Open
}

# fixed area shares in closed cells move to the fleet's other fixed open cells; if none
# hold effort the row is left to the engine, which allocates over open cells only
.MaskDistributionRows <- function(Dist, Closed) {
  for (y in seq_len(dim(Dist)[1])) for (fl in seq_len(dim(Dist)[2])) {
    d <- Dist[y, fl, ]
    cl <- Closed[y, fl, ]
    Fixed <- !is.na(d)
    if (!any(Fixed & cl & d > 0)) next
    Mass <- sum(d[Fixed])
    d[cl & Fixed] <- 0
    OpenMass <- sum(d[Fixed & !cl])
    Dist[y, fl, ] <- if (OpenMass > 0) {
      d[Fixed & !cl] <- d[Fixed & !cl] * Mass / OpenMass
      d
    } else NA
  }
  Dist
}

.MaskClosedDistribution <- function(Proj, sim, TSInd) {
  Closure <- Proj@Misc$Closure
  if (is.null(Closure) || dim(Proj@Distribution)[4] < 2 || all(Closure > 0) || !length(TSInd))
    return(Proj)
  Dist <- Proj@Distribution[sim, TSInd, , , drop = FALSE]
  if (all(is.na(Dist))) return(Proj)
  Years <- dimnames(Proj@Distribution)[[2]][TSInd]
  Closed <- .ClosedFleetAreas(Closure, Proj@Misc$Catchability, sim, Years)
  Proj@Distribution[sim, TSInd, , ] <- .MaskDistributionRows(
    array(Dist, dim(Dist)[-1]), Closed)
  Proj
}
