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

  Advice <- try(.CheckAdviceSchedules(Advice, FleetNames), silent=TRUE)
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

  Advice@TAC <- .MatchAdviceFleets(Advice@TAC, FleetNames, 'TAC')

  dd <- dim(Advice@TAC)
  if (length(dd) > 1)
    cli::cli_abort(c(
      "x" = "{.code Advice@TAC} must be a numeric vector of length 1 or length {.code nFleet} ({nFleet}).",
      "i" = "It is an array with dimensions {paste(dd, collapse = ' x ')}; TACs by area are not supported."
    ), call = NULL)

  if (length(Advice@TAC)!= 1 && length(Advice@TAC)!=nFleet)
    cli::cli_abort(c(
      "x" = "If {.code Advice@TAC} is a numeric vector, it must be length 1 or length {.code nFleet} ({nFleet}).",
      "i" = "It has length {length(Advice@TAC)}."
    ), call = NULL)

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
      cli::cli_abort(c(
        "x" = "If {.code Advice@Effort} is a numeric vector, it must be length 1 or length {.code nFleet} ({nFleet}).",
        "i" = "It has length {length(Effort)}."
      ), call = NULL)
    }
    Advice@Effort <- Effort
    return(Advice)
  }
  
  if (is.array(Effort)) {
    dd <- dim(Effort)
    if (dd[1] != 1 && dd[1] != nFleet) 
      cli::cli_abort(c(
        "x" = "If {.code Advice@Effort} is an array, its first dimension must be length 1 or length {.code nFleet} ({nFleet}).",
        "i" = "It has dimensions {paste(dd, collapse = ' x ')}."
      ), call = NULL)
    
    if (length(dd)==1) {
      dimnames(Effort) <- list(Fleet=FleetNames)   
    }
    
    if (length(dd)==2) {
      # fleet by area
      if (dd[2] != nArea) 
        cli::cli_abort(c(
          "x" = "If {.code Advice@Effort} is a 2D array, its second dimension must be length {.code nArea} ({nArea}).",
          "i" = "It has dimensions {paste(dd, collapse = ' x ')}."
        ), call = NULL)
      
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
    cli::cli_abort(c(
      "x" = "Names of {.code Advice@{SlotName}} must match the fleet names.",
      "i" = "Fleet names: {.val {FleetNames}}",
      "i" = "Supplied names: {.val {Names}}"
    ), call = NULL)

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
      cli::cli_abort(c(
        "x" = "If {.code Advice@Closure} is a vector, it must be length {.code nArea} ({nArea}).",
        "i" = "It has length {length(Closure)}."
      ), call = NULL)
    
    Closure <- matrix(Closure, nrow=nFleet, ncol=nArea, byrow=TRUE)
 
  }
  
  dd <- dim(Closure)
  
  if (dd[1] != nFleet)
    cli::cli_abort(c(
      "x" = "If {.code Advice@Closure} is a matrix, it must have {.code nFleet} ({nFleet}) rows.",
      "i" = "It has dimensions {paste(dd, collapse = ' x ')}."
    ), call = NULL)
  
  if (dd[2] != nArea)
    cli::cli_abort(c(
      "x" = "If {.code Advice@Closure} is a matrix, it must have {.code nArea} ({nArea}) columns.",
      "i" = "It has dimensions {paste(dd, collapse = ' x ')}."
    ), call = NULL)
  
  
  dimnames(Closure) <- list(Fleet=FleetNames,
                            Area=Areas)
  
  Advice@Closure <- Closure
  Advice
}

# Selectivity/Retention/DiscardMortality: one object, or a list by fleet of objects or NULL (fleet unchanged)
.CheckAdviceSchedules <- function(Advice, FleetNames) {
  nFleet <- length(FleetNames)
  for (sl in c('Selectivity', 'Retention', 'DiscardMortality')) {
    x   <- methods::slot(Advice, sl)
    cls <- tolower(sl)
    if (is.null(x) || inherits(x, cls)) next
    Valid <- vapply(x, \(xi) is.null(xi) || inherits(xi, cls), logical(1))
    Bad <- which(!Valid)
    if (length(Bad))
      cli::cli_abort(c(
        "x" = "{.code Advice@{sl}} must be a {.fn {sl}} object or a list of {.fn {sl}} objects (or {.code NULL}), one per fleet.",
        "i" = "Not a {.fn {sl}} object: {cli::qty(length(Bad))}element{?s} {.val {Bad}}."
      ), call = NULL)
    Names <- names(x)
    if (length(x) == 1 && is.null(Names)) {
      x <- x[[1]]
    } else {
      if (length(x) != nFleet)
        cli::cli_abort(c(
          "x" = "If {.code Advice@{sl}} is a list, it must be length {.code nFleet} ({nFleet}).",
          "i" = "It has length {length(x)}."
        ), call = NULL)
      if (!is.null(Names) && !identical(Names, FleetNames)) {
        if (!setequal(Names, FleetNames))
          cli::cli_abort(c(
            "x" = "Names of {.code Advice@{sl}} must match the fleet names.",
            "i" = "Fleet names: {.val {FleetNames}}",
            "i" = "Supplied names: {.val {Names}}"
          ), call = NULL)
        x <- x[FleetNames]
      }
      if (all(vapply(x, is.null, logical(1)))) x <- NULL
    }
    methods::slot(Advice, sl) <- x
  }
  Advice
}
