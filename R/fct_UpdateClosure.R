

UpdateClosure <- function(ProjSim, MPAdviceList, MPAdviceList_Previous, Year, YearsProj) {
  
  nArea <- nArea(ProjSim@OM)
  if (nArea<2)
    return(ProjSim)  
  
  FleetNames <- FleetNames(ProjSim@OM)
  Complexes <- ProjSim@OM@Complexes
  YearsProj <- YearsProj[YearsProj>=Year]
  for (complex in seq_along(MPAdviceList)) {
    stocks <- Complexes[[complex]]
    MPAdvice <- MPAdviceList[[complex]]
    MPAdvicePrevious <- MPAdviceList_Previous[[complex]]
    
    if (ClosureUnchanged(MPAdvice, MPAdvicePrevious))
      next()

    Closure <- ProcessAdvice_Closure(MPAdvice@Closure, FleetNames, nArea, YearsProj)
    
    for (st in stocks) {
      ArrayFill(ProjSim@OM@Fleet[[st]]@Closure) <- Closure
    }
  }
  
  ProjSim
}

ClosureUnchanged <- function(MPAdvice, MPAdvicePrevious) {
  !is.null(MPAdvicePrevious) && 
    (IdenticalS4(MPAdvice@Closure, MPAdvicePrevious@Closure)) ||
    !length(MPAdvice@Closure)
  
}


ProcessAdvice_Closure <- function(Closure, FleetNames, nArea, YearsProj) {
  if (any(!Closure %in% c(0,1)))
    cli::cli_abort(c(
      "x"="`Closure(Advice)` must only contain 0 or 1",
      "i"="Currently: {.val {MPAdvice@Closure}}"
    ), call=NULL)
  
  if (is.array(Closure)) {
    if (!any(dim(Closure) == c(length(FleetNames), nArea))) 
      cli::cli_abort(c(
        "x"="`Closure(Advice)` must either be numeric length `nArea` or array dimensions `nFleet` by `nArea`" 
      ), call=NULL)
  } else {
    if (length(Closure) != nArea)    
      cli::cli_abort(c(
        "x"="`length(Closure(Advice))` ({.val {(length(MPAdvice@Closure))}}) != `nArea` ({.val {nArea}})" 
      ), call=NULL)
    
    Closure <- matrix(Closure, length(FleetNames), nArea, byrow=TRUE)
  }
  
  dimnames(Closure) <- list(
    Fleet=FleetNames,
    Area=1:nArea
  )
  
  Closure |> 
    AddDimension('Year', val=YearsProj[1]) |>
    ExtendYears(YearsProj) |>
    aperm(c('Year', 'Fleet', 'Area'))
  
}


