CalcInitialTimeStep <- function(OMList, Unfished) {
  
  nStock <- length(OMList$NumberAtAgeArea)
  
  for (st in 1:nStock) {
  
    RecDevInit <- OMList$SRR$RecDevInit[[st]]
    RecDevHist <- OMList$SRR$RecDevs[[st]] 
  
    InitAgeClassRecDevs <- abind::abind(RecDevHist[,1,drop=FALSE],
                                        RecDevInit, along=2,
                                        use.dnns = TRUE,
                                        use.first.dimnames = FALSE)
    ages <- dimnames(InitAgeClassRecDevs)$Age |> as.numeric()
    ages[1] <- ages[2]-1
    dimnames(InitAgeClassRecDevs)$Age <- ages
    
    N0atAge <- Unfished@Equilibrium@Number[[st]][,,1,, drop=FALSE]
    
    InitAgeClassRecDevs <- InitAgeClassRecDevs |> 
      AddDimension('Time Step') |> 
      AddDimension('Area')
    
    NatAgeInitial <- ArrayMultiply(N0atAge, InitAgeClassRecDevs)
    
    if (length(OMList$Depletion$Initial[[st]]) > 0){
      cli::cli_abort('Initial depletion not done', .internal=TRUE)
      # NatAgeInitial update for initial depletion
    }
    
    OMList$NumberAtAgeArea[[st]][,,1,] <- NatAgeInitial
    
  }
  OMList
}





CalcPopDynamics <- function(Hist, TimeSteps=NULL, MP=NULL, silent=FALSE) {
  
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist, 'Historical')
  
  progress <- seq_along(TimeSteps)
  
  if (!silent)  {
    progress <- cli::cli_progress_along(TimeSteps,
                                        'Calculating Population Dyamics')
    on.exit(cli::cli_progress_done())
  }
  
  for (ts in progress) {
    
    TimeStep <- TimeSteps[ts]

    
    # ---- Do MICE stuff during this Time Step (if applicable) -----
    # TODO
    Hist <- CalcMICE(Hist, TimeStep=TimeStep)
    
    # ---- Update Biomass At Age etc ----
    # done after MICE to account for changes
    Hist <- UpdateBioArrays(Hist, TimeStep)
  
    # for MPs - Calculate Effort, Selectivity, etc
    # update fishery data 
    # these two steps should be done first
  
    # ---- Distribute Effort across Areas ----
    Hist <- DistributeEffort(Hist, TimeStep)
    
    # ---- Calculate Catch and Fishing Mortality ----
    Hist <- CalcCatch(Hist, TimeStep)
    
    # ---- Calculate Recruitment  Time Step ----
    Hist <- CalcRecruitment(Hist, TimeStep=TimeStep)
    
    # ---- Number, Biomass at beginning of Next Time Step and Move ----
    Hist <- CalcNumberNext(Hist, TimeStep)
    
    # print(sum(Hist@Number[[1]][1,,ts+1,]))
    
  }
  # tictoc::toc()
  

  
  
  Hist
}