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

  Advice <- try(.CheckAdviceSchedules(Advice, FleetNames, Proj, name), silent=TRUE)
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
.CheckAdviceSchedules <- function(Advice, FleetNames, Proj = NULL, name = NULL) {
  nFleet <- length(FleetNames)
  for (sl in c('Selectivity', 'Retention', 'DiscardMortality')) {
    x   <- methods::slot(Advice, sl)
    cls <- tolower(sl)
    if (is.null(x)) next
    if (inherits(x, cls)) {
      .CheckComplexSchedules(x, sl, Proj, name)
      next
    }
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
    .CheckComplexSchedules(x, sl, Proj, name)
  }
  Advice
}

# an at-age schedule, or an at-size schedule without `Classes`, applies to every stock in a
# complex only if the stocks share those classes
.CheckComplexSchedules <- function(x, sl, Proj, name) {
  if (is.null(x) || is.null(Proj) || !length(name)) return(invisible(NULL))
  Stocks <- Proj@OM@Complexes[[as.character(name)]]
  if (length(Stocks) < 2) return(invisible(NULL))
  StockNms <- StockNames(Proj@OM)[Stocks]
  Objects  <- if (is.list(x)) x else rep(list(x), length(Proj@OM@Fleet[[Stocks[1]]]))

  Differ <- \(Get) length(unique(lapply(Stocks, \(st) as.numeric(Get(st))))) > 1
  for (fl in seq_along(Objects)) {
    obj <- Objects[[fl]]
    if (is.null(obj)) next
    if (length(obj@MeanAtAge) > 1 && Differ(\(st) Proj@OM@Stock[[st]]@Ages@Classes))
      cli::cli_abort(c(
        "x" = "{.code Advice@{sl}} sets {.field MeanAtAge} for complex {.val {name}}, whose stocks have different age classes.",
        "i" = "Stocks: {.val {StockNms}}",
        "i" = "Use {.field Pars}, or {.field MeanAtLength} with {.field Classes}, which apply to each stock's sizes."
      ), call = NULL)
    for (SizeSlot in intersect(c('MeanAtLength', 'MeanAtWeight'), methods::slotNames(obj))) {
      if (length(methods::slot(obj, SizeSlot)) < 2 || length(obj@Classes)) next
      Default <- if (SizeSlot == 'MeanAtLength') 'Length' else 'Weight'
      if (Differ(\(st) .FleetScheduleClasses(methods::slot(Proj@OM@Fleet[[st]][[fl]], sl), SizeSlot,
                                               methods::slot(Proj@OM@Stock[[st]], Default)@Classes)))
        cli::cli_abort(c(
          "x" = "{.code Advice@{sl}} sets {.field {SizeSlot}} without {.field Classes} for complex {.val {name}}, whose stocks have different size classes.",
          "i" = "Stocks: {.val {StockNms}}",
          "i" = "Supply {.field Classes} (the lower bounds of the bins) so the schedule is placed on each stock's classes."
        ), call = NULL)
    }
  }
  invisible(NULL)
}
