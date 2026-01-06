#' Calculate the Movement Matrix
#'
#' @export
CalcMovement <- function(Spatial,
                         Ages=NULL,
                         Years=NULL,
                         nSim=5,
                         seed=NULL,
                         nits=100,
                         plot=FALSE,
                         silent=FALSE,
                         control = list(iter.max = 5e3, eval.max = 1e4)) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, Years, nSim, seed, nits, control)
  
  if (CheckDigest(Spatial, argList)) {
    # don't run if object hasn't changed
    return(Spatial)
  }
 
  SetSeed(seed)
  
  # UnfishedDist
  if (is.numeric(Spatial@UnfishedDist)) {
    
    
  }
  
  Spatial@UnfishedDist <- GenerateStochasticValues(Spatial@UnfishedDist, nSim, Years)
  
  
  
  GenerateStochasticValues <- function(object, nSim=NULL, Years=NULL) {
    if (is.array(object)) {
      # Check dimensions
      
      # Add dim names
      
      return(object)
    }
    
    if (length(object)!=2) {
      return(object)
    }
    
    if (is.null(nSim)) {
      cli::cli_abort('`nSim` required to generate stochastic values')
    }
    
    object <- StructurePars(list(object), nSim)[[1]]
    object
  }
  
  
  # add age and time-step dimensions
  Spatial@UnfishedDist  <- Spatial@UnfishedDist |>
    AddAgeYearDimensions() |>
    AddDimNames(c('Sim', 'Area', 'Age', 'Year'), Years=Years)
  
  nareas <- max(2,dim(Spatial@UnfishedDist)[2])
  
  if (nareas>2) {
    dd <- dim(Spatial@FracOther)
    if (length(dd)==0)
      cli::cli_abort(c('`FracOther` required for more than 2 areas.',
                       'i'='Must be an array with dimensions: `c(nSim, nArea, nArea)`')
      )
    if (length(dd)==2)
      cli::cli_abort('`FracOther` must be an array with dimensions: `c(nSim, nArea, nArea)`')
    
    if (any(dd[2:3] != nareas))
      cli::cli_abort('Second and third dimensions of `FracOther` must length `nArea`')
    
    Spatial@FracOther  <- Spatial@FracOther |>
      AddAgeYearDimensions(outdim=5) |>
      AddDimNames(c('Sim', 'FromArea', 'ToArea', 'Age', 'Year'), Years=Years)
  }
  
  # generate stochastic values if needed
  Spatial@ProbStaying <- GenerateStochasticValues(Spatial@ProbStaying, nSim)
  
  if (is.null(Spatial@ProbStaying))
    cli::cli_abort('`ProbStaying` is not populated.')
  
  if (is.null(dim(Spatial@ProbStaying))) # when ProbStaying is a single value
    Spatial@ProbStaying <- array(Spatial@ProbStaying, dim=c(1, nareas))
  
  # add age and time-step dimensions
  Spatial@ProbStaying  <- Spatial@ProbStaying |>
    AddAgeYearDimensions() |>
    AddDimNames(c('Sim', 'Area', 'Age', 'Year'), Years=Years)
  
  dims <- lapply(list(Spatial@UnfishedDist, Spatial@ProbStaying), dim)
  if (!is.null(Spatial@FracOther)) {
    dims <- c(dims, list(dim(Spatial@FracOther)[c(1,2,4,5)]))
    names(dims) <- c('UnfishedDist', 'ProbStaying', 'FracOther')
    outdims <- do.call('rbind',dims) |> apply(2, max)
  } else {
    names(dims) <- c('UnfishedDist', 'ProbStaying')
    outdims <- do.call('rbind',dims) |> apply(2, max)
  }
  
  Spatial@Movement <- AddDimNames(array(NA, dim=c(outdims[1],
                                                  nareas,
                                                  nareas,
                                                  outdims[3],
                                                  outdims[4])),
                                  c('Sim', 'FromArea', 'ToArea', 'Age', 'Year'),
                                  Years=Years)
  
  
  # nasty loop for now
  
  if (!silent)
    sb <- cli::cli_progress_bar('Calculating Movement Matrix',
                                total=outdims[1]*outdims[3]*outdims[4])
  
  UnfishedDist <- AddDimNames(array(NA, dim=c(outdims[1],
                                              nareas,
                                              outdims[3],
                                              outdims[4])),
                              c('Sim', 'Area', 'Age', 'Year'),
                              Years=Years)
  
  
  
  for (s in 1:outdims[1]) {
    
    FracArea_s <- FracOther_s <- ProbStaying_s <- s
    if (dims$ProbStaying[1] < s) ProbStaying_s <- 1
    if (dims$UnfishedDist[1] < s) FracArea_s <- 1
    if (nareas>2)
      if (dims$FracOther[1] < s) FracArea_s <- 1
    
    for (ts in 1:outdims[4]) {
      FracArea_ts <- ProbStaying_ts <- FracOther_ts <- ts
      if (dims$ProbStaying[4] < ts) ProbStaying_ts <- 1
      if (dims$UnfishedDist[4] < ts) FracArea_ts <- 1
      if (nareas>2)
        if (dims$FracOther[4] < ts) FracOther_ts <- 1
      
      for (age in 1:outdims[3]) {
        
        if (!silent)
          cli::cli_progress_update(id=sb)
        
        FracArea_age <- ProbStaying_age <-FracOther_age <-  age
        if (dims$ProbStaying[3] < age) ProbStaying_age <- 1
        if (dims$UnfishedDist[3] < age) FracArea_age <- 1
        if (nareas>2)
          if (dims$FracOther[3] < age) FracOther_age <- 1
        
        if (nareas==2) {
          optMovement <- stats::optim(logit(rep(Spatial@ProbStaying[ProbStaying_s,1,ProbStaying_age,ProbStaying_ts],2)),
                                      FitMovement,
                                      UnfishedDist = Spatial@UnfishedDist[FracArea_s,1,FracArea_age,FracArea_ts],
                                      ProbStaying= Spatial@ProbStaying[ProbStaying_s,1,ProbStaying_age,ProbStaying_ts],
                                      method = "L-BFGS-B")#,
          #lower = rep(-8, 2), upper = rep(8, 2))
          
          Movement <- MarkovFrac(LogitProbs=optMovement$par)
          Spatial@Movement[s,,,age,ts] <- Movement
          
          ExpectedDist <- c(Spatial@UnfishedDist[FracArea_s,1,FracArea_age,FracArea_ts],
                            1-Spatial@UnfishedDist[FracArea_s,1,FracArea_age,FracArea_ts])
          
          UnfishedDist[FracArea_s,,FracArea_age,FracArea_ts] <- CalcAsymptoticDist(Movement, ExpectedDist, plot=plot, nits=nits)
          
          
        } else {
          opt <- stats::nlminb(rep(0,nareas),
                               FitMovement,
                               UnfishedDist = Spatial@UnfishedDist[FracArea_s, , FracArea_age,FracArea_ts],
                               ProbStaying = Spatial@ProbStaying[ProbStaying_s,,ProbStaying_age,ProbStaying_ts],
                               FracOther = Spatial@FracOther[FracArea_s,,,FracOther_age,FracOther_ts],
                               CVDist = Spatial@CVDist,
                               CVStay=Spatial@CVStay,
                               nits=nits,
                               control = list(iter.max = 5e3, eval.max = 1e4))
          
          Spatial@Movement[s,,,age,ts] <- MarkovFrac(LogitProbs=opt$par,
                                                     FracOther = Spatial@FracOther[FracArea_s,,,FracOther_age,FracOther_ts])
          
          UnfishedDist[FracArea_s,,FracArea_age,FracArea_ts] <- CalcAsymptoticDist(Movement=Spatial@Movement[s,,,age,ts],
                                                                                   ExpectedDist=Spatial@UnfishedDist[FracArea_s,,FracArea_age,FracArea_ts],
                                                                                   plot=plot, nits=nits)
        }
      }
    }
  }
  
  if (!silent)
    cli::cli_progress_done()
  
  Spatial@UnfishedDist <- UnfishedDist
  
  SetDigest(Spatial, argList)
}


GenerateStochasticValues <- function(object, nSim=NULL, Years=NULL) {
  if (is.array(object)) {
    # Check dimensions
    
    # Add dim names
    
    return(object)
  }
  
  if (length(object)!=2) {
    return(object)
  }
  
  if (is.null(nSim)) {
    cli::cli_abort('`nSim` required to generate stochastic values')
  }
  
  object <- StructurePars(list(object), nSim)[[1]]
  object
}
