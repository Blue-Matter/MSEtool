CalcInitialTimeStep <- function(StockParsList, Unfished) {
  
  TimeSteps <- dimnames(StockParsList$NumberAtAgeArea)[['Time Steps']]
  dd <- dim(StockParsList$NumberAtAgeArea)
  nSim <- dd[1]
  nAges <- dd[3]
  TimeStep1 <- TimeSteps[1]
  
  RecDevInit <- StockParsList$RecDevInit
  RecDevHist <- StockParsList$RecDevHist 
  
  N0atAge <- purrr::map(Unfished@Equilibrium@Number, \(x) 
                        ArrayExpand(x, nSim, nAges, TimeStep1)
  ) |> 
    List2Array('Stock') |>
    aperm(c('Sim', 'Stock', 'Age', 'Time Step', 'Area'))
  
  
  InitAgeClassRecDevs <- abind::abind(RecDevHist[,, 1,drop=FALSE],
                                      RecDevInit, along=3,
                                      use.dnns = TRUE,
                                      use.first.dimnames = FALSE)
  
  ages <- dimnames(InitAgeClassRecDevs)$Age |> as.numeric()
  ages[1] <- ages[2]-1
  dimnames(InitAgeClassRecDevs)$Age <- ages
  
  InitAgeClassRecDevs <- InitAgeClassRecDevs |> 
    AddDimension('Time Step') |> 
    AddDimension('Area')
  
  ArrayMultiply(N0atAge, InitAgeClassRecDevs)
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