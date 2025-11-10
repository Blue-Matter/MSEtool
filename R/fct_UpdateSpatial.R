UpdateSpatial <- function(ProjSim, MPAdvice, MPAdvicePrevious, Year, YearsProj, st=1) {
  
  nFleet <- nFleet(ProjSim@OM)
  nArea <- nArea(ProjSim@OM)
  if (nArea<2)
    return(ProjSim)  
  
  if (is.null(MPAdvice))
    return(ProjSim)
  
  if (isS4(MPAdvice)) {
    # all fleets
    if (SpatialUnchanged(MPAdvice, MPAdvicePrevious))
      return(ProjSim)  
    
    SpatialErrorChecks(MPAdvice, nArea)
    
    Closure <- matrix(MPAdvice@Spatial, length(YearsProj), nArea, byrow=TRUE)
    Closure <- replicate(nFleet, Closure) 
    dimnames(Closure) <- list(
      Year=YearsProj,
      Area=1:nArea,
      Fleet=as.character(ProjSim@OM@Fleet[[st]]@Name)
    )
    
    ArrayFill(ProjSim@OM@Fleet[[st]]@Closure) <- aperm(Closure,
                                                       c('Year', 'Fleet', 'Area'))
    
  } else {
    for (fl in seq_along(MPAdvice)) {
      if (SpatialUnchanged(MPAdvice[[fl]], MPAdvicePrevious[[fl]]))
        next()
      
      SpatialErrorChecks(MPAdvice[[fl]], nArea)
      
      Closure <- matrix(MPAdvice[[fl]]@Spatial, length(YearsProj), nArea, byrow=TRUE)
      dimnames(Closure) <- list(
        Year=YearsProj,
        Area=1:nArea,
        Fleet=as.character(ProjSim@OM@Fleet[[st]]@Name[fl])
      )
      ArrayFill(ProjSim@OM@Fleet[[st]]@Closure) <- aperm(Closure,
                                                         c('Year', 'Fleet', 'Area'))
      
    }
  }
  ProjSim
}


SpatialErrorChecks <- function(MPAdvice, nArea) {
  if (length(MPAdvice@Spatial) != nArea)    
    cli::cli_abort(c(
      "x"="`length(Spatial(Advice))` ({.val {(length(MPAdvice@Spatial))}}) != `nArea` ({.val {nArea}})" 
    ), call=NULL)
  
  if (any(!MPAdvice@Spatial %in% c(0,1)))
    cli::cli_abort(c(
      "x"="`Spatial(Advice)` must only contain 0 or 1",
      "i"="Currently: {.val {MPAdvice@Spatial}}"
    ), call=NULL)
  
}

SpatialUnchanged <- function(MPAdvice, MPAdvicePrevious) {
  !is.null(MPAdvicePrevious) && 
    (IdenticalS4(MPAdvice@Spatial, MPAdvicePrevious@Spatial)) ||
    !length(MPAdvice@Spatial)
  
}

