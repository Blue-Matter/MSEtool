#' Update selectivity or retention across all simulations
#'
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
#' @param type One of `"Selectivity"` or `"Retention"`.
#' @return Updated `Proj` object.
#' @keywords internal
Update_Selectivity <- function(Proj,
                               Year, 
                               AdviceSimList, 
                               LastAdviceSimList,
                               YearsHist,
                               YearsProj, 
                               Areas, 
                               FleetNames,
                               StockNames,
                               type=c('Selectivity', 'Retention')) {
  
  type        <- match.arg(type)
  nSim        <- Proj@OM@nSim
  nStock      <- nStock(Proj)
  nFleet      <- length(FleetNames)
  FutureYears <- YearsProj[YearsProj >= Year]
  
  # Expand Selectivity/Retention Arrays
  for (st in seq_len(nStock)) {
    for (fl in seq_len(nFleet)) {
      target <- slot(Proj@OM@Fleet[[st]][[fl]], type)
      target@MeanAtAge    <- Extend(target@MeanAtAge,    nSim = nSim, NULL, FutureYears)
      target@MeanAtLength <- Extend(target@MeanAtLength, nSim = nSim, NULL, FutureYears)
      target@MeanAtWeight <- Extend(target@MeanAtWeight, nSim = nSim, NULL, FutureYears)
      slot(Proj@OM@Fleet[[st]][[fl]], type) <- target
    }
  }
  
  
  for (sim in seq_len(nSim)) {
    AdviceList <- AdviceSimList[[sim]]
    LastAdviceList <- LastAdviceSimList[[sim]]
    
    Proj <- Update_Selectivity_Sim(
      Proj           = Proj,
      sim            = sim,
      FutureYears    = FutureYears,
      AdviceList     = AdviceList,
      LastAdviceList = LastAdviceList,
      nFleet         = nFleet,
      Complexes      = Proj@OM@Complexes,
      nArea          = nArea,
      nSim           = nSim, 
      type           = type
    )
  }
  
  Proj
}

#' Update selectivity or retention for a single simulation
#'
#' Populates selectivity/retention arrays from advice for all future projection
#' years, updating both `Proj@OM@Fleet` and the relevant `Proj@Misc` lists.
#' Skips a complex when management is unchanged or advice is `NULL`.
#'
#' @param Proj A `Proj` object.
#' @param sim Integer. Simulation index.
#' @param FutureYears Integer vector of years from current year to end of projection.
#' @param AdviceList List of `advice` objects for this simulation, one per complex.
#' @param LastAdviceList Same structure as `AdviceList` for the previous year.
#' @param nFleet Integer. Number of fleets.
#' @param Complexes List mapping complex indices to stock indices.
#' @param nArea Integer. Number of areas.
#' @param nSim Integer. Total number of simulations.
#' @param type One of `"Selectivity"` or `"Retention"`.
#' @return Updated `Proj` object.
#' @keywords internal
Update_Selectivity_Sim <- function(Proj,
                                   sim,
                                   FutureYears,
                                   AdviceList,
                                   LastAdviceList,
                                   nFleet,
                                   Complexes,
                                   nArea,
                                   nSim,
                                   type=c('Selectivity', 'Retention')) {
  
  type <- match.arg(type, c('Selectivity', 'Retention'))
  populate <- if (type == "Selectivity") PopulateSelectivity else PopulateRetention
  
  

  for (i in seq_along(AdviceList)) {
    stocks          <- Complexes[[i]]
    Advice          <- AdviceList[[i]]
    AdvicePrevious  <- LastAdviceList[[i]]
    
    if (!inherits(Advice, 'advice')) next    
    if (is.null(slot(Advice,type))) next
    if (UnchangedManagement(Advice, AdvicePrevious, slotName=type)) next
    
    SelectList <- slot(Advice, type) # either selectivity or retention
    
    if (length(SelectList) > 1 && length(SelectList) != nFleet)
      stop("Advice@", type, " must be a `", type, "()` object or a list of ",
           "`", type, "()` objects of length nFleet (", nFleet, ")")
    
    for (st in stocks) {
      Ages     <- Proj@OM@Stock[[st]]@Ages
      Length   <- SubsetSim(Proj@OM@Stock[[st]]@Length,   sim)
      Weight   <- SubsetSim(Proj@OM@Stock[[st]]@Weight,   sim)
      Maturity <- SubsetSim(Proj@OM@Stock[[st]]@Maturity, sim)
      
      
      for (fl in seq_along(FleetNames)) {
        select <- if (is.list(SelectList)) SelectList[[fl]] else SelectList
        
        select <- populate(
          select,
          Ages, Length, Weight, Maturity,
          nSim        = 1,
          Years       = FutureYears,
          nArea       = nArea,
          CalcAtLength = TRUE,
          silent      = TRUE
        )
        
        select@MeanAtAge    <- set_sim_dimname(select@MeanAtAge,    sim) |> 
          ExtendAreas(1:nArea) |> 
          ExtendYears(FutureYears)
        
        select@MeanAtLength <- set_sim_dimname(select@MeanAtLength, sim) |>
          ExtendAreas(1:nArea) |>
          ExtendYears(FutureYears)
        
        select@MeanAtWeight <- set_sim_dimname(select@MeanAtWeight, sim) |> 
          ExtendAreas(1:nArea) |> 
          ExtendYears(FutureYears)
        

        target <- slot(Proj@OM@Fleet[[st]][[fl]], type)
        ArrayFill(target@MeanAtAge)    <- select@MeanAtAge
        ArrayFill(target@MeanAtLength) <- select@MeanAtLength
        ArrayFill(target@MeanAtWeight) <- select@MeanAtWeight
        slot(Proj@OM@Fleet[[st]][[fl]], type) <- target
        
        if (type=='Selectivity') {
          ArrayFill(Proj@Misc$SelAgeList[[st]]) <- DropDimension(select@MeanAtAge, 'Fleet', FALSE)
          ArrayFill(Proj@Misc$SelSizeList[[st]][[fl]]) <- select@MeanAtLength
        } else {
          ArrayFill(Proj@Misc$RetAgeList[[st]]) <- DropDimension(select@MeanAtAge, 'Fleet', FALSE)
          ArrayFill(Proj@Misc$RetSizeList[[st]][[fl]]) <- select@MeanAtLength
        }
        
      } # end fleet loop
    }  # end stock loop
  } # end complex loop
  Proj
} 

#' Update retention across all simulations
#'
#' Thin wrapper around [Update_Selectivity()] with `type = "Retention"`.
#'
#' @inheritParams Update_Selectivity
#' @return Updated `Proj` object.
#' @keywords internal
Update_Retention <- function(Proj,
                             Year, 
                             AdviceSimList,
                             LastAdviceSimList,
                             YearsHist,
                             YearsProj,
                             Areas, 
                             FleetNames,
                             StockNames) {
  
  Update_Selectivity(Proj,
                     Year, 
                     AdviceSimList,
                     LastAdviceSimList, 
                     YearsHist,
                     YearsProj,
                     Areas, 
                     FleetNames,
                     StockNames,
                     type='Retention')
  
}

set_sim_dimname <- function(x, sim) {
  if (is.null(x)) return(x)
  if (!is.array(x)) return(x)
  
  dn <- dimnames(x)
  if (is.null(dn) || !"Sim" %in% names(dn)) return(x)
  
  dn[["Sim"]] <- sim
  dimnames(x) <- dn
  x
}