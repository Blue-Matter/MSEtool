
CheckAdvice <- function(Advice, Proj, FleetNames, Areas, sim) {
  if (inherits(Advice, 'try-error'))
    return(Advice)
 
  Advice <- CheckAdvice_TAC(Advice, Proj, FleetNames, Areas)
  
  Advice <- CheckAdvice_Effort(Advice, Proj, FleetNames, Areas, sim)
  
  Advice <- CheckAdvice_Closure(Advice, Proj, FleetNames, Areas)
  
  Advice
}

CheckAdvice_TAC <- function(Advice, Proj, FleetNames, Areas) {
  
  if (is.null(Advice@TAC) || !length(Advice@TAC))
    return(Advice)
  
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  
  if (is.array(Advice@TAC)) {
    dd <- dim(Advice@TAC)
    if (dd[1]!=nFleet || dd[2]!=nArea) 
      stop('If Advice@TAC is an array, it must have `nFleet` rows and `nArea` columns')
    
    return(Advice)
  }

  if (length(Advice@TAC)!= 1 && length(Advice@TAC)!=nFleet)
    stop("If Advice@TAC is numeric vector, it must be either length 1 or length `nFleet`")
 
  Advice@TAC <- as.array(Advice@TAC)
  Advice
  
}

CheckAdvice_Effort <- function(Advice, Proj, FleetNames, Areas, sim) {
  
  Effort <- Advice@Effort
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  
  if (is.null(Effort))
    return(Advice)
  
  if (is.numeric(Effort) && !is.array(Effort)) {
    if (length(Effort)==1) {
      Effort <- array(Effort)
    } else if (length(Effort)==nFleet) {
      Effort <- array(Effort, nFleet, dimnames = list(Fleet=FleetNames))
    } else {
      stop("If `Advice@Effort` is numeric, is must be length 1 or length `nFleet`")
    }
    Advice@Effort <- Effort
    return(Advice)
  }
  
  if (is.array(Effort)) {
    dd <- dim(Effort)
    if (dd[1] != 1 && dd[1] != nFleet) 
      stop("If `Advice@Effort` is numeric or 1D array, is must be length 1 or length `nFleet`")
    
    if (length(dd)==1) {
      dimnames(Effort) <- list(Fleet=FleetNames)   
    }
    
    if (length(dd)==2) {
      # fleet by area
      if (dd[2] != nArea) 
        stop("If `Advice@Effort` is a 2D array, second dimension must be length `nArea`")
      
     dimnames(Effort) <- list(Fleet=FleetNames,
                              Area=Areas) 
    }
  }
  
  Advice@Effort <- Effort
  Advice
 
}


CheckAdvice_Closure <- function(Advice, Proj, FleetNames, Areas) {
  Closure <- Advice@Closure
  if (is.null(Closure))
    return(Advice)
  
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  
  if (nArea==1)
    return(Advice)
  
  Closure <- (Closure >= 0.5) * 1L
  
  if (!is.array(Closure)) {
    if (!length(Closure)==nArea)
      stop(paste("If `Advice@Closure` a vector, it must be length `nArea`: ", nArea))
    
    Closure <- matrix(Closure, nrow=nFleet, ncol=nArea, byrow=TRUE)
 
  }
  
  dd <- dim(Closure)
  
  if (dd[1] != nFleet)
    stop(paste("If `Advice@Closure` is a matrix, it must have `nFleet` rows: ", nFleet))
  
  if (dd[2] != nArea)
    stop(paste("If `Advice@Closure` is a matrix, it must have `nArea` columns:", nArea))
  
  
  dimnames(Closure) <- list(Fleet=FleetNames,
                            Area=Areas)
  
  Advice@Closure <- Closure
  Advice
}