
CheckAdvice <- function(MPAdvice, Proj, FleetNames, Areas, x) {
 
  MPAdvice <- CheckAdvice_TAC(MPAdvice, Proj)
  MPAdvice <- CheckAdvice_Effort(MPAdvice, Proj, FleetNames, Areas, x)
  
  MPAdvice <- CheckAdvice_Closure(MPAdvice, Proj, FleetNames, Areas)
  
  MPAdvice
}

CheckAdvice_TAC <- function(MPAdvice, Proj) {
  
  if (is.null(MPAdvice@TAC))
    return(MPAdvice)
  
  stop('TODO!!')
  
  
}

CheckAdvice_Effort <- function(MPAdvice, Proj, FleetNames, Areas, x) {
  
  Effort <- MPAdvice@Effort
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  
  if (is.null(Effort))
    return(MPAdvice)
  
  if (is.numeric(Effort)) {
    if (length(Effort)==1) {
      Effort <- array(Effort)
    } else if (length(Effort)==nFleet) {
      Effort <- array(Effort, nFleet, dimnames = list(Fleet=FleetNames))
    } else {
      stop("If `Advice@Effort` is numeric, is must be length 1 or length `nFleet`")
    }
    MPAdvice@Effort <- Effort
    return(MPAdvice)
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
  
  MPAdvice@Effort <- Effort
  MPAdvice
 
}


CheckAdvice_Closure <- function(MPAdvice, Proj, FleetNames, Areas) {
  Closure <- MPAdvice@Closure
  if (is.null(Closure))
    return(MPAdvice)
  
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  
  if (nArea==1)
    return(MPAdvice)
  
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
  
  MPAdvice@Closure <- Closure
  MPAdvice
}