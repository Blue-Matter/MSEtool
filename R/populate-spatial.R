#' Populate a Spatial Object
#'
#' Populate a `Spatial` object by validating or generating spatial structure
#' for a stock, including movement matrices, unfished distributions, and
#' relative area size.
#'
#' @param Spatial A [Spatial()] object to populate.
#' @param Ages An [Ages()] object defining age classes.
#' @param Years Numeric vector of model years.
#' @param nSim Integer. Number of simulation replicates.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#'   
#' @details
#' `PopulateSpatial()` handles population of spatial structure for a stock.
#' Steps include:
#'
#' * Creating a default non-spatial structure if the object is empty
#' * Validating supplied movement matrices and their dimensions
#' * Calculating unfished spatial distributions
#' * Generating movement probabilities and movement matrices when required
#' * Expanding spatial objects across simulations and years
#'
#' If a movement matrix is supplied, it is used to calculate unfished 
#' distribution.
#'
#' @return
#' A populated [Spatial()] object.
#'
#' @seealso
#' [Populate()], [PopulateStock()], [PopulateFleet()]
#'
#' @examples
#' \dontrun{
#' Spatial <- PopulateSpatial(
#'   Spatial,
#'   Ages = Ages,
#'   Years = Years,
#'   nSim = 100,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateSpatial <- function(Spatial,
                            Ages = NULL,
                            Years = NULL,
                            nSim = 5,
                            seed = NULL,
                            silent = FALSE,
                            force = FALSE) {
  
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  nSim  <- Get_nSim(Spatial, nSim)
  
  argList <- list(Ages, Years, nSim, seed)
  
  if (CheckDigest(Spatial, argList)) 
    return(Spatial)
  
  SetSeed(seed)
  
  if (EmptyObject(Spatial)) {
    Spatial <- DefaultSpatial(Ages, Years)
    return(SetDigest(Spatial, argList))
  }
  
  hasMovement <- !is.null(Spatial@Movement)
  
  if (hasMovement) {
    # Movement matrix supplied: validate + derive implied quantities
    Spatial <- CheckMovementDimensions(Spatial, Ages, Years)
    Spatial <- CalcUnfishedDist(Spatial, Ages, Years)
    Spatial <- ProcessRelativeSize(Spatial, nSim)
    
    return(SetDigest(Spatial, argList))
  }
  

  # Derive movement matrix from component processes
  Spatial <- Spatial |>
    ProcessUnfishedDist(Ages, Years, nSim) |>
    ProcessProbStaying(Ages, Years, nSim) |>
    InitMovementMatrix(Ages, Years, nSim)
  
  Spatial <- CheckFracArea(Spatial, Ages, Years, nSim)
  
  Mov_dim <- dim(Spatial@Movement)
  dnames  <- dimnames(Spatial@Movement)
  
  nsim  <- Mov_dim[1]
  nAge  <- Mov_dim[4]
  nYear <- Mov_dim[5]
  
  # unique years
  Years_actual <- as.numeric(dnames[["Year"]])
  # extend objects so that they all have same years
  Spatial@UnfishedDist <- ExtendYears(Spatial@UnfishedDist, Years_actual)
  Spatial@ProbStaying  <- ExtendYears(Spatial@ProbStaying,  Years_actual)
  Spatial@FracOther    <- ExtendYears(Spatial@FracOther,    Years_actual)
  
  # Fit movement matrices
  for (sim in seq_len(nsim)) {
    for (age in seq_len(nAge)) {
      for (year in seq_len(nYear)) {
        Spatial@Movement[sim, , , age, year] <- FitMovement(Spatial, sim, age, year)
      }
    }
  }
  
  Spatial <- CalcUnfishedDist(Spatial, Ages, Years)
  Spatial <- ProcessRelativeSize(Spatial, nSim)
  
  SetDigest(SetAgeDimnames(Spatial, Ages), argList)
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

  
  if (nDim == 4) {
    # Add Time Dimesions 
    Spatial@Movement <- AddDimension(Spatial@Movement, 'Year', Years[1])
    nDim <- length(dim(Spatial@Movement))
  }
  
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
  

  dims <- dim(Spatial@Movement)
  dnames <- dimnames(Spatial@Movement)
  Names <- names(dnames)
  nArea <- dims[2]
  
  if (is.null(Names)) {
    if (dims[5] != 1 && dims[5] != length(Years)) 
      cli::cli_abort('`Spatial@Movement` must have named dimensions if length of Year dimension != 1 || != length(Years)')
    
    dimnames(Spatial@Movement) <- list(
      Sim=1:dims[1],
      FromArea=1:dims[2],
      ToArea=1:dims[3],
      Age=Ages@Classes[1:dims[4]],
      Year=Years[1:dims[5]]
    )
    
    
  }
  Spatial
}

CheckUnfishedDist <- function(Spatial, Ages, Years, nSim) {
  dd <- dim(Spatial@UnfishedDist)
  
  # add dimensions and dimension names 
  if (length(dd)==2) {
    if (is.null(dimnames(Spatial@UnfishedDist))) {
      dimnames(Spatial@UnfishedDist) <- list(Sim=1:dd[1],
                                             Area=1:dd[2])
    }
    Spatial@UnfishedDist <- Spatial@UnfishedDist |> 
      AddDimension('Age', Ages@Classes[1]) |>
      AddDimension('Year', Years[1])
  } 
  
  if (length(dd)==3) {
    if (is.null(dimnames(Spatial@UnfishedDist))) {
      dimnames(Spatial@UnfishedDist) <- list(Sim=1:dd[1],
                                             Area=1:dd[2],
                                             Age=Ages@Classes[1:dd[3]])
    }
    Spatial@UnfishedDist <- Spatial@UnfishedDist |> 
      AddDimension('Year', Years[1])
  } 
  
  # Check it sums to 1 over areas
  Chk <- apply(Spatial@UnfishedDist, c('Sim', 'Age', 'Year'), sum)
  if (any(abs(Chk - 1) > 1e-8)) 
    cli::cli_abort("`UnfishedDist(Spatial)` does not sum to 1 over areas. Are all values < 1?")
  
  Spatial
}



ProcessUnfishedDist <- function(Spatial, 
                                Ages=NULL, 
                                Years=NULL, 
                                nSim=5) {
  
  if (is.array(Spatial@UnfishedDist)) 
    return(CheckUnfishedDist(Spatial, Ages, Years, nSim))

  if (is.numeric(Spatial@UnfishedDist)) {
    # 2 Area Model
    # Numeric - Constant over Age & Time
    
    
    
    if (length(Spatial@UnfishedDist) == 1) {
      # Constant over Sims
      Area_1 <- Spatial@UnfishedDist
      Area_2 <- 1-Area_1
      Spatial@UnfishedDist <- array(c(Area_1, Area_2), 
                                    dim=c(1, 2, 1, 1),
                                    dimnames = list(
                                      Sim=1,
                                      Area=1:2,
                                      Age=Ages@Classes[1],
                                      Year=Years[1]
                                    ))
      
      
    } else if (length(Spatial@UnfishedDist) == 2) {
      # Sample from Uniform
      Bounds <- sort(Spatial@UnfishedDist)
      Area_1 <- stats::runif(nSim, Bounds[1], Bounds[2])
      Area_2 <- 1-Area_1
      Spatial@UnfishedDist <- array(cbind(Area_1, Area_2), 
                                    dim=c(nSim, 2, 1, 1),
                                    dimnames = list(
                                      Sim=1:nSim,
                                      Area=1:2,
                                      Age=Ages@Classes[1],
                                      Year=Years[1]
                                    ))
      
      
    } else {
      cli::cli_abort("If `UnfishedDist(Spatial)` is a numeric value, it must be length 1 or length 2")
    }
    
    return(CheckUnfishedDist(Spatial, Ages, Years, nSim))
  }
}

CheckProbStaying <- function(Spatial) {
  if (is.null(Spatial@ProbStaying)) {
    cli::cli_abort('`ProbStaying` is not populated.')
  }
  
}

ProcessProbStaying <- function(Spatial, 
                               Ages=NULL, 
                               Years=NULL, 
                               nSim=5) {
  
  if (is.array(Spatial@ProbStaying)) {
    CheckProbStaying(Spatial)
    return(Spatial)
  } 
  
  if (is.numeric(Spatial@ProbStaying)) {
    # 2 Area Model
    # Numeric - Constant over Age & Time
    
    if (length(Spatial@ProbStaying) == 1) {
      # Constant over Sims
      Area_1 <- Spatial@ProbStaying
      Spatial@ProbStaying <- array(c(Area_1),
                                   dim=c(1, 1, 1, 1),
                                   dimnames = list(
                                     Sim=1,
                                     Area=1,
                                     Age=Ages@Classes[1],
                                     Year=Years[1]
                                   ))
      
      
    } else if (length(Spatial@ProbStaying) == 2) {
      # Sample from Uniform
      Bounds <- sort(Spatial@ProbStaying)
      Area_1 <- stats::runif(nSim, Bounds[1], Bounds[2])
      Spatial@ProbStaying <- array(cbind(Area_1), 
                                   dim=c(nSim, 1, 1, 1),
                                   dimnames = list(
                                     Sim=1:nSim,
                                     Area=1,
                                     Age=Ages@Classes[1],
                                     Year=Years[1]
                                   ))
      
      
    } else {
      nArea <- length(Spatial@ProbStaying)
      Spatial@ProbStaying <- array(Spatial@ProbStaying, 
                                   dim=c(1, nArea, 1, 1),
                                   dimnames = list(
                                     Sim=1,
                                     Area=1:nArea,
                                     Age=Ages@Classes[1],
                                     Year=Years[1]
                                   ))
    }
  }
  CheckProbStaying(Spatial)
  Spatial
}


InitMovementMatrix <- function(Spatial, Ages, Years, nSim) {
  # Calculate minimum dimensions 
  nArea <- max(2, dim(Spatial@UnfishedDist)[2])
  
  nSim_actual <- c(dimnames(Spatial@UnfishedDist)[["Sim"]] |> as.numeric(),
                   dimnames(Spatial@ProbStaying)[["Sim"]] |> as.numeric(),
                   dimnames(Spatial@FracOther)[["Sim"]] |> as.numeric()) |>
    unique() |> length()
  
  if (nSim_actual !=1 && nSim_actual !=nSim) {
    cli::cli_abort("Incorrect Sim dimensions on `UnfishedDist`, `ProbStaying`, or `FracOther`", .internal=TRUE)
  }
  
  nYear <- c(dimnames(Spatial@UnfishedDist)[["Year"]] |> as.numeric(),
             dimnames(Spatial@ProbStaying)[["Year"]] |> as.numeric(),
             dimnames(Spatial@FracOther)[["Year"]] |> as.numeric()) |>
    unique() |> length()
  
  nAge <- c(dimnames(Spatial@UnfishedDist)[["Age"]] |> as.numeric(),
            dimnames(Spatial@ProbStaying)[["Age"]] |> as.numeric(),
            dimnames(Spatial@FracOther)[["Age"]] |> as.numeric()) |>
    unique() |> length()
  
  # Create movement matrix
  Spatial@Movement <- array(NA, dim=c(nSim_actual,
                                      nArea,
                                      nArea,
                                      nAge,
                                      nYear),
                            dimnames = list(
                              Sim=1:nSim_actual,
                              FromArea=1:nArea,
                              ToArea=1:nArea,
                              Age=Ages@Classes[1:nAge],
                              Year=Years[1:nYear])
  )
  
  CheckMovementDimensions(Spatial, Ages, Years)
}

InitUnfishedDist <- function(Spatial, Ages, Years, nSim) {
  dd <- dim(Spatial@Movement)
  nSim_actual <- dd[1]
  nArea <- dd[2]
  nAge <- dd[4]
  nYear <- dd[5]
  
  array(NA, dim=c(nSim_actual,
                  nArea,
                  nAge,
                  nYear),
        dimnames = list(
          Sim=1:nSim_actual,
          Area=1:nArea,
          Age=Ages@Classes[1:nAge],
          Year=Years[1:nYear])
  )
}

ProcessRelativeSize <- function(Spatial, nSim) {
  
  nArea <- dim(Spatial@UnfishedDist)[2]
  
  if (is.null(Spatial@RelativeSize)) {
    cli::cli_alert_warning('`RelativeSize` is not specified. Assuming all areas are equal size')
    Spatial@RelativeSize <- array(1/nArea, dim=c(1,nArea),
                                  dimnames = list(
                                    Sim=1,
                                    Area=1:nArea
                                  ))
    return(Spatial)
  }
  
  
  if (inherits(Spatial@RelativeSize, 'character')) {
    if (Spatial@RelativeSize!="EqualDensity") {
      cli::cli_abort('If `Spatial@RelativeSize` is character, it can only be "EqualDensity"')
    } 
    Spatial@RelativeSize <- apply(Spatial@UnfishedDist, c('Sim', 'Area'), mean)   
    CheckRelativeSize(Spatial)
    return(Spatial)
  }
  
  
  if (is.array(Spatial@RelativeSize)) {
    CheckRelativeSize(Spatial)
    return(Spatial)
  } 
  
  
  if (is.numeric(Spatial@RelativeSize)) {
    # 2 Area Model
    if (length(Spatial@RelativeSize) == 1) {
      # Constant over Sims
      Area_1 <- Spatial@RelativeSize
      Area_2 <- 1-Area_1
      Spatial@RelativeSize <- array(c(Area_1, Area_2), 
                                    dim=c(1, 2),
                                    dimnames = list(
                                      Sim=1,
                                      Area=1:2
                                    ))
      
      
    } else if (length(Spatial@RelativeSize) == 2) {
      # Sample from Uniform
      Bounds <- sort(Spatial@RelativeSize)
      Area_1 <- stats::runif(nSim, Bounds[1], Bounds[2])
      Area_2 <- 1-Area_1
      Spatial@RelativeSize <- array(cbind(Area_1, Area_2), 
                                    dim=c(nSim, 2),
                                    dimnames = list(
                                      Sim=1:nSim,
                                      Area=1:2
                                    ))
      
    } else {
      nArea <- length(Spatial@RelativeSize)
      Spatial@RelativeSize <- array(Spatial@RelativeSize, 
                                    dim=c(1, nArea),
                                    dimnames = list(
                                      Sim=1,
                                      Area=1:nArea
                                    ))
    }
    
    CheckRelativeSize(Spatial)
    return(Spatial)
  }
}

CheckRelativeSize <- function(Spatial) {
  nArea <- dim(Spatial@UnfishedDist)[2]
  
  dd <- dim(Spatial@RelativeSize)
  if (dd[2]>nArea) {
    cli::cli_abort('`RelativeSize` is longer than `nArea` ({.val {nArea}})')
  }
  
  if (nArea>2) {
    if (dd[2]<nArea) {
      cli::cli_abort('`RelativeSize` must have `nArea` ({.val {nArea}}) columns')
    }
    rowsums <- apply(Spatial@RelativeSize, 1, sum) |> round(3)
    if (!all(rowsums==1))
      cli::cli_abort('`RelativeSize` must sum to 1 across columns')
  }
}

CheckFracArea <- function(Spatial, Ages, Years, nSim) {
  nArea <- dim(Spatial@Movement)[2]
  if (nArea <3) {
    return(Spatial)
  }
  dd <- dim(Spatial@FracOther)
  if (length(dd)==0) {
    cli::cli_abort(c('`FracOther` required for more than 2 areas.',
                     'i'='Must be an array with dimensions: `c(nSim, nArea, nArea)`')
    )
  }
  
  if (length(dd)==2) {
    cli::cli_abort('`FracOther` must be an array with dimensions: `c(nSim, nArea, nArea)`')
  }
  
  if (any(dd[2:3] != nArea)) {
    cli::cli_abort('Second and third dimensions of `FracOther` must length `nArea`')
  }
  
  # add dimensions and dimension names 
  if (length(dd)==3) {
    if (is.null(dimnames(Spatial@FracOther))) {
      dimnames(Spatial@FracOther) <- list(Sim=1:dd[1],
                                          FromArea=1:dd[2],
                                          ToArea=1:dd[2])
    }
    Spatial@FracOther <- Spatial@FracOther |> 
      AddDimension('Age', Ages@Classes[1]) |>
      AddDimension('Year', Years[1])
  } 
  
  if (length(dd)==4) {
    if (is.null(dimnames(Spatial@FracOther))) {
      dimnames(Spatial@FracOther) <- list(Sim=1:dd[1],
                                          FromArea=1:dd[2],
                                          ToArea=1:dd[2],
                                          Age=Ages@Classes[1:dd[3]])
    }
    Spatial@FracOther <- Spatial@FracOther |> 
      AddDimension('Year', Years[1])
  } 
  
  if (length(dd)==5) {
    if (is.null(dimnames(Spatial@FracOther))) {
      dimnames(Spatial@FracOther) <- list(Sim=1:dd[1],
                                          FromArea=1:dd[2],
                                          ToArea=1:dd[2],
                                          Age=Ages@Classes[1:dd[3]],
                                          Years=Years[1:dd[4]])
    }
  } 
  Spatial
}


