#' @rdname Convert
#' @export
ConvertMOM <- function(MOM, Author='', CurrentYear=NULL, Seasons=1, Populate=TRUE, silent=FALSE) {
  
  CheckClass(MOM, c('MOM'), 'MOM')
  
  if (!silent)
    cli::cli_alert('Converting object of class {.cls MOM} to class {.cls om}')
  
  om <- OM()
  om@Name <- MOM@Name
  om@Agency <-  MOM@Agency
  om@Region <-  MOM@Region
  om@Author <- Author
  om@Longitude <- MOM@Longitude
  om@Latitude <- MOM@Latitude
  om@Sponsor <- MOM@Sponsor
  om@nSim <- MOM@nsim
  om@nYear <- MOM@Fleets[[1]][[1]]@nyears/Seasons
  om@pYear <- MOM@proyears/Seasons
  om@Interval <- MOM@interval
  om@Seed <- MOM@seed
  om@pStar <- MOM@pstar
  om@maxF <- MOM@maxF
  om@nReps <- MOM@reps
  om@Source <- MOM@Source
  
  if (is.null(CurrentYear)) {
    om@CurrentYear <- MOM@Fleets[[1]][[1]]@CurrentYr
    if (om@CurrentYear < 1000)
      om@CurrentYear <- as.numeric(format(Sys.Date(), '%Y'))
  } else {
    om@CurrentYear <- CurrentYear
  }
  
  TimeUnits <- CalcTSUnits(Seasons)
  om@Seasons <- Seasons
  om@Years <- CalcYears(nYear=om@nYear,
                                pYear=om@pYear,
                                CurrentYear=om@CurrentYear,
                                Seasons)
  
  YearsList <- list(HistTS=Years(om, 'Historical'),
                        ProjTS=Years(om, 'Projection'),
                        TimeUnits=TimeUnits,
                        Seasons=Seasons
  )
  
  om@Stock <- ConvertToList(MOM2stock(MOM, YearsList))
  
  StockNames <- lapply(MOM@Stocks, slot, 'Name') 
  StockNames <- lapply(StockNames, function(x) gsub("REPLACED -- ", '', x)) |> unlist()
  names(om@Stock) <- StockNames
  nStock <- MOM@Fleets |> length()
  nFleet <- MOM@Fleets[[1]] |> length()
  om@Fleet <- vector('list', nStock)
  FleetNames <- lapply(MOM@Fleets[[1]], slot, 'Name') 
  FleetNames <- lapply(FleetNames, function(x) gsub("REPLACED -- ", '', x)) |> unlist()
  names(om@Fleet) <- StockNames
  for (st in 1:nStock) {
    om@Stock[[st]]@Depletion@Final <- NULL #
    om@Fleet[[st]]  <- ConvertToList(MOM2fleet(MOM, st))
    names(om@Fleet[[st]]) <- FleetNames
  }
  
  om@EFactor <- MakeNamedList(StockNames(om), 
                              array(1, dim=c(om@nSim, nFleet(om)),
                                    dimnames = list(
                                      Sim=1:om@nSim,
                                      Fleet=FleetNames(om)
                                    )
                              ))   
  
  if (Populate)
    om <- PopulateOM(om, silent=FALSE)
  
  om
}

MOM2fleet <- function(MOM, st) {
  
  nfleets <- length(MOM@Fleets[[1]])
  FleetNames <- names(MOM@Fleets[[1]])
  if (is.null(FleetNames))
    FleetNames <- paste('Fleet', 1:nfleets)
  
  FleetList <- list()
  
  for (fl in 1:nfleets) {
    Fleet <- MOM@Fleets[[st]][[fl]]
    cpars <- MOM@cpars[[st]][[fl]]
    Fdisc <- MOM@Stocks[[st]]@Fdisc
    FleetList[[fl]] <- OM2fleet(Fleet, cpars, Fdisc)
  }
  names(FleetList) <- FleetNames
  if (nfleets==1) return(FleetList[[1]])
  FleetList
}

MOM2stock <- function(MOM, YearsList=NULL) {
  StockList <- list()
  stocks <- MOM@Stocks
  nstocks <- length(stocks)
  
  for (st in 1:nstocks) {
    Stock <- stocks[[st]]
    cpars <- MOM@cpars[[st]][[1]]
    StockList[[st]] <- OM2stock(Stock, cpars, YearsList, MOM@nsim, MOM@seed)
  }
  if (nstocks==1) return(StockList[[1]])
  StockList
}
