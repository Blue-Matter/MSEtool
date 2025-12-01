# TODO - update for multiple stocks and stock complexes

#' Calculate Reference Yield
#' 
#' Highest Landings or Removals over the projection period with a constant F 
#' policy
#' 
#' @export
CalcRefLandings <- function(SimList, HistYears, ProjYears, type=c('Landings', 'Removals'), Calc=TRUE) {
  type <- match.arg(type, c('Landings', 'Removals'))
  
  if (is.logical(Calc) && !Calc)
    return(SimList)
  
  if (is.array(Calc)) {
    SimList <- purrr::imap(SimList, \(ProjSim, idx) {
      array <-  array(Calc[idx,, drop=FALSE] |> abind::adrop(1),
                      dimnames = list(Stock=dimnames(Calc)$Stock))
      if (type=='Landings') {
        ProjSim@RefLandings <- array
      } else {
        ProjSim@RefRemovals <- array
      }
      ProjSim
    })
    return(SimList)
  }
  nStock <- nStock(SimList[[1]]@OM)
  nFleet <- nFleet(SimList[[1]]@OM)
  if (nStock>1 || nFleet>1) {
    # cli::cli_alert_warning('Optimizing Reference Catch not currently working for multiple stocks/fleets')
    return(SimList)
  }
  
  # TODO 
  CheckIdenticalSims(SimList, c(HistYears, ProjYears))
  
  # for debugging
  ProjSim <- SimList[[1]]
  logF <- log(0.1)
  
  # Extend object to include projection years
  SimList_Extended <- purrr::map(SimList, \(ProjSim) 
                        ExtendHist(ProjSim)
                        )
  
  bounds <- c(1E-5, ProjSim@OM@maxF)
  SimList_Extended <- purrr::map(SimList_Extended, \(ProjSim) {
    doOpt <- optimize(OptRefLandings,
                      log(bounds),
                      ProjSim=ProjSim,
                      HistYears=HistYears,
                      ProjYears=ProjYears,
                      type=type,
                      tol=1e-2)
    
    if (type=='Landings') {
      ProjSim@RefLandings <- array(-doOpt$objective, 1,
                                   dimnames = list(
                                     Stock=StockNames(ProjSim@OM)
                                   ))
    } else {
      ProjSim@RefRemovals <- array(-doOpt$objective, 1,
                                   dimnames = list(
                                     Stock=StockNames(ProjSim@OM)
                                   ))
    }
    ProjSim
  }, .progress = list(
    type = "iterator",
    caller = environment(),
    format = "Calculating Reference {type} {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE))
  
  SimList <- purrr::map2(SimList_Extended, SimList, \(ProjSim_Extended, ProjSim) {
    ProjSim@RefLandings <- ProjSim_Extended@RefLandings
    ProjSim@RefRemovals <- ProjSim_Extended@RefRemovals
    ProjSim
  })
  
  SimList 
  
}




#' @describeIn CalcRefLandings Calculate Reference Removals
#' @export
CalcRefRemovals <- function(Hist, type=c('Landings', 'Removals')) {
  CalcRefLandings(Hist, type)
}

OptRefLandings <- function(logF, ProjSim, HistYears, ProjYears, type=c('Landings', 'Removals')) {
  type <- match.arg(type, c('Landings', 'Removals'))
  
  # TODO update for multiple stocks and fleets & complexes
  # TODO - test - this should equal MSY under equilibrium conditions
  st <- 1
  fl <- 1

  ProjYearInd <- match(ProjYears, c(HistYears, ProjYears))
  
  ProjSim@Effort[st,ProjYearInd,fl] <- exp(logF) 
  ProjSim@OM@Fleet[[st]]@Catchability[] <- 1
  nArea <- nArea(ProjSim@OM)
  RelativeSize <- as.numeric(ProjSim@OM@Stock[[st]]@Spatial@RelativeSize )
  qArea <- matrix(1/RelativeSize, length(ProjYearInd), nArea, byrow=TRUE)
  ProjSim@OM@Fleet[[st]]@qArea[ProjYearInd,fl,] <- qArea
  LastHistTS <- tail(HistYears,1)
  
  PopDynamicsProject <- ProjSim |>
    PopulateNumberNext_(LastHistTS) |>
    SimulateDynamics_(ProjYears)
 
  if (type=='Landings') {
    Yield <- PopDynamicsProject@Landings[[st]] |> List2Array("Year")
  } else {
    Landings <- PopDynamicsProject@Landings[[st]] |> List2Array("Year")
    Discards <- PopDynamicsProject@Discards[[st]] |> List2Array("Year")
    Yield <- Landings+Discards
  }
  
  lastnTS <- ProjSim@OM@Control$RefYield$lastnTS
  if (is.null(lastnTS))
    lastnTS <- 5
  
  dd <- dim(Yield)
  nTS <- dd[4]
  if (lastnTS >nTS)
    lastnTS <- nTS
  
  TSmean <- (nTS-lastnTS+1):nTS
  
  -mean(apply(Yield[,,,TSmean,drop=FALSE], 4, sum))
  
}

