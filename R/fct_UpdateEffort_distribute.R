.DistributeEffort <- function(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex, LastHistIndex) {

  if (!is.array(MPAdvice@Effort)) {
    if (length(MPAdvice@Effort)==1) {
      ProjSim <- DistributeEffort_(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex, LastHistIndex)
    } else if (length(MPAdvice@Effort)==length(FleetNames)) {
      ProjSim <- DistributeEffort_(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex, LastHistIndex)
    } else {
      cli::cli_abort(c(
        "x"="If `Advice@Effort` is a numeric vector, it must be length 1 or length `nFleet` ({.val {nFleet}})"
      ))
    }
  } else {
    nFleet <- length(FleetNames)
    dd <- dim(MPAdvice@Effort)
    if (dd[1] != nFleet)
      cli::cli_abort(c(
        "x"="If `Advice@Effort` is an array, it must have `nFleet` ({.val {nFleet}}) rows and `nArea` ({.val {nArea}}) columns "
      ))
    if (dd[2] != nArea)
      cli::cli_abort(c(
        "x"="If `Advice@Effort` is an array, it must have `nFleet` ({.val {nFleet}}) rows and `nArea` ({.val {nArea}}) columns "
      ))
    dimnames(MPAdvice@Effort) <- list(
      Fleet=FleetNames,
      Area=1:nArea
    )
    ProjSim <- DistributeEffort_Area(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex, LastHistIndex)
  }
  ProjSim
}


DistributeEffort_ <- function(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex, LastHistIndex)  {
  nStock <- length(StockNames[stocks])
  nFleet <- length(FleetNames)
  nTS <- dim(ProjSim@Effort)[2]
  FutureYears <- TSIndex:nTS
  for (st in 1:nStock) {
    Effort <- ProjSim@Effort[st,LastHistIndex, ] *  MPAdvice@Effort
    FleetInd <- 1:nFleet

    ProjSim@Effort[st,FutureYears, FleetInd] <- matrix(Effort[FleetInd],
                                                       length(FutureYears),
                                                       length(FleetInd),
                                                       byrow=TRUE
    )

  }
  ProjSim
}

DistributeEffort_Area <- function(ProjSim, MPAdvice, nArea, FleetNames, StockNames, stocks, TSIndex, LastHistIndex)  {
  nStock <- length(StockNames[stocks])
  nFleet <- length(FleetNames)
  
  cli::cli_abort("Advice@Effort by Fleet and Area not done yet!", internal=TRUE)
  
  ProjSim
}
