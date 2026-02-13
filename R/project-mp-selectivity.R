Update_Selectivity <- function(Proj, Year, AdviceSimList, LastAdviceSimList,
                               YearsProj, Areas, FleetNames,
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
      Length <- Proj@OM@Stock[[st]]@Length
      Weight <- Proj@OM@Stock[[st]]@Weight
      Maturity <- Proj@OM@Stock[[st]]@Maturity
      
      for (fl in seq_along(FleetNames)) {
        if (is.list(SelectList)) {
          select <- SelectList[[fl]]
        } else {
          select <- SelecList
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

        ArrayFill(slot(Proj@OM@Fleet[[st]][[fl]],type)@MeanAtAge) <- select@MeanAtAge
        ArrayFill(slot(Proj@OM@Fleet[[st]][[fl]],type)@MeanAtLength) <- select@MeanAtLength
        ArrayFill(slot(Proj@OM@Fleet[[st]][[fl]],type)@MeanAtWeight) <- select@MeanAtWeight
        
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





Update_Retention <- function(Proj, Year, AdviceSimList, LastAdviceSimList,
                             YearsProj, Areas, FleetNames) {
  
  Update_Selectivity(Proj, Year, AdviceSimList, LastAdviceSimList, 
                     YearsProj, Areas, FleetNames, 
                     type='Retention')
  
}