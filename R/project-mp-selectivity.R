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
  
  type <- match.arg(type, c('Selectivity', 'Retention'))
  
  nSim <- Proj@OM@nSim
  nStock <- nStock(Proj)
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  FutureYears <- YearsProj[YearsProj>=Year]
  
  # Expand Selectivity/Retention Arrays
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      if (type=='Selectivity') {
        Proj@OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge <- Proj@OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge |> 
          Extend(nSim=nSim, NULL, FutureYears)
        
        Proj@OM@Fleet[[st]][[fl]]@Selectivity@MeanAtLength <- Proj@OM@Fleet[[st]][[fl]]@Selectivity@MeanAtLength |> 
          Extend(nSim=nSim, NULL, FutureYears)
      
        Proj@OM@Fleet[[st]][[fl]]@Selectivity@MeanAtWeight <- Proj@OM@Fleet[[st]][[fl]]@Selectivity@MeanAtWeight |> 
          Extend(nSim=nSim, NULL, FutureYears)
        
      } else {
        Proj@OM@Fleet[[st]][[fl]]@Retention@MeanAtAge <- Proj@OM@Fleet[[st]][[fl]]@Retention@MeanAtAge |> 
          Extend(nSim=nSim, NULL, FutureYears)
        
        Proj@OM@Fleet[[st]][[fl]]@Retention@MeanAtLength <- Proj@OM@Fleet[[st]][[fl]]@Retention@MeanAtLength |> 
          Extend(nSim=nSim, NULL, FutureYears)
        
        Proj@OM@Fleet[[st]][[fl]]@Retention@MeanAtWeight <- Proj@OM@Fleet[[st]][[fl]]@Retention@MeanAtWeight |> 
          Extend(nSim=nSim, NULL, FutureYears)
      }
      
    }
  }
  
  
  
  for (sim in seq_len(nSim)) {
    Proj <- Update_Selectivity_Sim(
      Proj = Proj,
      sim = sim,
      FutureYears = FutureYears,
      AdviceList = AdviceSimList[[sim]],
      LastAdviceList = LastAdviceSimList[[sim]],
      nFleet = nFleet,
      Complexes = Proj@OM@Complexes,
      nArea = nArea,
      nSim = nSim, 
      type = type
    )
  }
  
  Proj
}


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
  
  nComplex <- length(AdviceList)

  for (i in seq_len(nComplex)) {
    stocks <- Complexes[[i]]
    Advice <- AdviceList[[i]]
    AdvicePrevious <- LastAdviceList[[i]]
    if (UnchangedManagement(Current=Advice, Previous=AdvicePrevious, slotName=type))
      next()
    
    SelectList <- slot(Advice, type) # either selectivity or retention
    
    if (length(SelectList)>1 && length(SelectList)!=nFleet) {
      stop(
        paste0("Advice@", type, " must be a `", type, '()` object or a list of `', type, '()` objects length `nFleet`')
      )
    }
    
    for (st in stocks) {
      Ages <- Proj@OM@Stock[[st]]@Ages
      Length <- Proj@OM@Stock[[st]]@Length |> SubsetSim(sim)
      Weight <- Proj@OM@Stock[[st]]@Weight |> SubsetSim(sim)
      Maturity <- Proj@OM@Stock[[st]]@Maturity |> SubsetSim(sim)
      
      for (fl in seq_along(FleetNames)) {
        if (is.list(SelectList)) {
          select <- SelectList[[fl]]
        } else {
          select <- SelectList
        }
        
        if (type=='Selectivity') {
          select <- PopulateSelectivity(Selectivity=select, 
                                        Ages, 
                                        Length, 
                                        Weight,
                                        Maturity, 
                                        nSim = 1,
                                        Years = FutureYears,
                                        nArea = length(Areas),
                                        CalcAtLength = TRUE,
                                        silent=TRUE)
        } else {
          select <- PopulateRetention(Retention=select, 
                                      Ages, 
                                      Length, 
                                      Weight,
                                      Maturity, 
                                      nSim = 1,
                                      Years = FutureYears,
                                      nArea = length(Areas),
                                      CalcAtLength = TRUE,
                                      silent=TRUE)
        }
        select@MeanAtAge    <- set_sim_dimname(select@MeanAtAge, sim) |> ExtendAreas(1:nArea)  |>
          ExtendYears(FutureYears)
        select@MeanAtLength <- set_sim_dimname(select@MeanAtLength, sim) |> ExtendAreas(1:nArea) |>
          ExtendYears(FutureYears)
        select@MeanAtWeight <- set_sim_dimname(select@MeanAtWeight, sim) |> ExtendAreas(1:nArea)  |>
          ExtendYears(FutureYears)
       
        ArrayFill(slot(Proj@OM@Fleet[[st]][[fl]],type)@MeanAtAge) <- select@MeanAtAge
        ArrayFill(slot(Proj@OM@Fleet[[st]][[fl]],type)@MeanAtLength) <- select@MeanAtLength
        ArrayFill(slot(Proj@OM@Fleet[[st]][[fl]],type)@MeanAtWeight) <- select@MeanAtWeight
        
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


Advice_Selectivity_Pars <- function(select) {
  
  select@Model <- FindModel(select, doCheck = FALSE)
  if (is.null(select@Model)) {
    stop(
      paste0("Advice@", type, "@Pars is populated but cannot find matching model")
    )
  }
  
  PopulateSelectivity(select, Ages, )
  
  
}





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