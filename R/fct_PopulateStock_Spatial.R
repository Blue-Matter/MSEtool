PopulateSpatial <- function(Spatial,
                            Ages=NULL,
                            Years=NULL,
                            nSim=5,
                            seed=NULL,
                            silent=FALSE,
                            plot=FALSE,
                            nits=100) {
  
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, nSim, seed, nits)
  
  if (CheckDigest(Spatial, argList)) {
    # don't run if object hasn't changed
    return(Spatial)
  }

  SetSeed(seed)
  
  if (EmptyObject(Spatial)) {
    # Empty Object - create 1-area (non-spatial) object
    Spatial <- DefaultSpatial(Ages, Years)
    return(SetDigest(Spatial, argList))
  }
  
  if (!is.null(Spatial@Movement)) {
    # Movement has been provided, calculate 
  
    # Check Spatial@Movement has correct structure
    Spatial <- CheckMovementDimensions(Spatial, Ages, Years)
    
    # Check that the dimensions are named correctly 
    Spatial <- CheckMovementDimNames(Spatial, Ages, Year)
    
    # Calculate Asymptotic Unfished Dist 
    Spatial <- CalcUnfishedDist(Spatial, Ages, Years)
   
    
    return(SetDigest(Spatial, argList))
    
  }

  
  CalcUnfishedDist <- function(Spatial,
                               Ages=NULL,
                               Years=NULL,
                               plot=FALSE,
                               nits=100) {
    
    Ages <- DefaultAges(Ages)
    Years <- DefaultYears(Years)
    
    dims <- dim(Spatial@Movement)
    if (is.null(dims)) {
      return(Spatial) 
    }
    nSim <- dims[1]
    nArea <- dims[2]
    nAge <- dims[4]
    nYear <- dims[5]
    UnfishedDist <- array(NA, dim=c(nSim, nArea, nAge, nYear),
                          dimnames = list(
                            Sim=1:nSim,
                            Area=1:nArea,
                            Age=Ages@Classes[1:nAge],
                            Year=Years[1:nYear]
                          ))

    for (s in 1:nSim) {
      for (ts in 1:nYear) {
        for (age in 1:nAge) {
          UnfishedDist[s,,age,ts] <- CalcAsymptoticDist(Movement=Spatial@Movement[s,,,age,ts],
                                                        plot=plot, nits=nits)
        }
      }
    }
    
    Spatial@UnfishedDist <- UnfishedDist
    Spatial
  }
  
  

  if (is.null(Spatial@Movement)) {
    # Populate Object and Calculate Movement Matrix
    
    Spatial <- CalcMovement(Spatial,
                            Ages,
                            Years, 
                            nSim, 
                            seed, 
                            nits, 
                            plot, 
                            silent)
    
    
    if (!is.null(Spatial@UnfishedDist)) {
      
      
      Spatial <- CalcUnfishedDist(Spatial, Years)
    }
    
    if (is.null(Spatial@UnfishedDist)) {
      Spatial <- CalcUnfishedDist(Spatial, Years)
    } 
    
    
    if (is.null(Spatial@UnfishedDist))
      cli::cli_abort('`UnfishedDist` must be populated for `Spatial` objects')
    
    # if (is.null(object@ProbStaying))
    #   cli::cli_abort('`ProbStaying` must be populated for `Spatial` objects')
    Spatial <- CalculateRelativeSize(Spatial, nSim)
    
    
    return(SetDigest(Spatial, argList))
  }
  
  
    

    nArea <- dd[2]
    

    
   
    
    # Calc Unfished Dist
    nAreaUnfished <- dim(Spatial@UnfishedDist)[2] 
    if (nAreaUnfished!=nArea)
      Spatial <- CalcUnfishedDist(Spatial, Years)
    
    # Calc Relative Size 
    
  
  
  
  if (is.null(Spatial@UnfishedDist)) {
    Spatial <- CalcUnfishedDist(Spatial, Years)
  } 
  
  
  if (is.null(Spatial@UnfishedDist))
    cli::cli_abort('`UnfishedDist` must be populated for `Spatial` objects')
  
  # if (is.null(object@ProbStaying))
  #   cli::cli_abort('`ProbStaying` must be populated for `Spatial` objects')
  Spatial <- CalculateRelativeSize(Spatial, nSim)
  
  
  
  SetDigest(Spatial, argList)
  
}

# Non-spatial object - 1 area default
DefaultSpatial <- function(Ages=NULL, Years=NULL) {
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  UnfishedDist <- array(1, dim=rep(1,4),
                        dimnames = list(
                          Sim=1,
                          Area=1,
                          Age=Ages@Classes[1],
                          Year=Years[1]
                        ))
  
  ProbStaying <- UnfishedDist
  
  RelativeSize <- array(1, dim=c(1,1),
                        dimnames = list(
                          Sim=1,
                          Area=1
                        ))
  
  Movement <- array(1, dim=rep(1,5),
                    dimnames = list(
                      Sim=1,
                      FromArea=1,
                      ToArea=1,
                      Age=Ages@Classes[1],
                      Year=Years[1]
                    ))
  
  Spatial(UnfishedDist,
          ProbStaying,
          RelativeSize,
          Movement)
}

CheckMovementDimensions <- function(Spatial, Ages=NULL, Years=NULL) {
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  dims <- dim(Spatial@Movement)
  nDim <- length(dims)
  
  # Spatial must be an array with either 3 or 5 dimensions
  if (!nDim%in% c(3,5)) {
    cli::cli_abort(c('x'="Incorrect dimensions on Spatial@Movemement",
                     'i'="`Spatial@Movement` should either have dimensions:",
                     '*'="{.val {c('Sim', 'FromArea', 'ToArea')}} OR",
                     '*'= "{.val {c('Sim', 'FromArea', 'ToArea', 'Age', 'Year')}}")
    )
  }
  
  if (dims[2] != dims[3]) {
    cli::cli_abort(c('x'="Incorrect dimensions on Spatial@Movemement",
                     'i'="`Spatial@Movement@FromArea` should be same length as Spatial@Movement@ToArea")
    )
  } 
  
  if (nDim==3) {
    # Add Age and Year dimensions
    Spatial@Movement <- Spatial@Movement |>
      AddDimension('Age', val=Ages@Classes[1]) |>
      AddDimension('Year', val=Years[1])
  }
  Spatial
}

CheckMovementDimNames <- function(Spatial, Ages, Year) {
  # TODO - may need to revisit this to check specific dimensions/dimnames
  #        or auto-name them relevant
  
  dims <- dim(Spatial@Movement)
  dnames <- dimnames(Spatial@Movement)
  Names <- names(dnames)
  nArea <- dims[2]
  
  if (is.null(dnames)) {
    cli::cli_abort('`Spatial@Movement` must have named dimensions')
  }
  Spatial
}
