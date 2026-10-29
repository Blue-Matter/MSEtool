.CheckAdvice <- function(Advice, Proj, FleetNames, Areas, sim, name) {
  if (inherits(Advice, 'try-error'))
    return(Advice)

  Advice <- try(.CheckAdviceTAC(Advice, Proj, FleetNames, Areas), silent=TRUE)
  if (!inherits(Advice, 'advice'))
    return(.AdviceCheckError(Advice, name))

  Advice <- try(.CheckAdviceEffort(Advice, Proj, FleetNames, Areas, sim), silent=TRUE)
  if (!inherits(Advice, 'advice'))
    return(.AdviceCheckError(Advice, name))

  Advice <- try(.CheckAdviceClosure(Advice, Proj, FleetNames, Areas), silent=TRUE)
  if (!inherits(Advice, 'advice'))
    return(.AdviceCheckError(Advice, name))

  Advice
}

# tags a validation failure with the stock/complex whose advice failed
.AdviceCheckError <- function(Error, name) {
  attr(Error, 'AdviceCheck') <- if (length(name)) as.character(name) else ''
  Error
}


.CheckAdviceTAC <- function(Advice, Proj, FleetNames, Areas) {
  
  if (is.null(Advice@TAC) || !length(Advice@TAC))
    return(Advice)
  
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  
  Advice@TAC <- .MatchAdviceFleets(Advice@TAC, FleetNames, 'TAC')

  if (is.array(Advice@TAC)) {
    dd <- dim(Advice@TAC)
    if (dd[1]!=nFleet || dd[2]!=nArea) 
      stop('If Advice@TAC is an array, it must have `nFleet` rows and `nArea` columns')
    
    return(Advice)
  }

  if (length(Advice@TAC)!= 1 && length(Advice@TAC)!=nFleet)
    stop("If Advice@TAC is numeric vector, it must be either length 1 or length `nFleet`")

  if (length(Advice@TAC) == nFleet) {
    Advice@TAC <- array(Advice@TAC, dim=nFleet, dimnames=list(Fleet=FleetNames))
  } else {
    Advice@TAC <- array(Advice@TAC, dim=1, dimnames=list(Fleet='Total'))
  }
  Advice

}

.CheckAdviceEffort <- function(Advice, Proj, FleetNames, Areas, sim) {
  
  Effort <- Advice@Effort
  nFleet <- length(FleetNames)
  nArea <- length(Areas)
  
  if (is.null(Effort))
    return(Advice)

  Effort <- .MatchAdviceFleets(Effort, FleetNames, 'Effort')

  if (is.numeric(Effort) && !is.array(Effort)) {
    if (length(Effort)==1) {
      Effort <- array(Effort, dim=1, dimnames=list(Fleet='Total'))
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


# reorder a TAC or Effort vector (or the rows of an array) named by fleet to the order of `FleetNames`
.MatchAdviceFleets <- function(x, FleetNames, SlotName) {
  nFleet <- length(FleetNames)
  Names <- if (is.array(x)) dimnames(x)[[1]] else names(x)
  if (nFleet < 2 || NROW(x) != nFleet || is.null(Names) || identical(Names, FleetNames) ||
      any(is.na(Names) | !nzchar(Names)) || !any(Names %in% FleetNames))
    return(x)

  if (!setequal(Names, FleetNames))
    stop(paste0("Names of `Advice@", SlotName, "` (", paste(Names, collapse = ", "),
                ") must match the fleet names: ", paste(FleetNames, collapse = ", ")))

  ind <- match(FleetNames, Names)
  if (is.array(x) && length(dim(x)) == 2)
    return(x[ind, , drop = FALSE])
  if (is.array(x))
    return(array(x[ind], dim = nFleet, dimnames = list(FleetNames)))
  x[ind]
}

.CheckAdviceClosure <- function(Advice, Proj, FleetNames, Areas) {
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
