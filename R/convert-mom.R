#' Convert Legacy MOM Object to New om Class
#'
#' Converts a legacy [MOM-legacy-class] object to the current [om-class], copying
#' attributes, updating year vectors, and optionally populating the object
#' via [PopulateOM()].
#'
#' @param MOM A [MOM-legacy-class] object to convert.
#' @param Author Character. Author of the OM. Default `""`.
#' @param CurrentYear Numeric. Last historical calendar year. If `NULL`
#'   (default), taken from `MOM@Fleets[[1]][[1]]@CurrentYr`; if that is less
#'   than 1000, defaults to the current system year.
#' @param Seasons Integer. Number of seasons per year. Default `1`.
#' @param Populate Logical. If `TRUE` (default), calls [PopulateOM()] on the
#'   converted object.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @return An [om-class] object.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertStock()], [ConvertFleet()],
#'   [ConvertObs()], [ConvertImp()], [PopulateOM()]
#'
#' @examples
#' \dontrun{
#' MOMlegacy <- readRDS("MyLegacyMOM.rds")
#' om_new <- ConvertMOM(MOMlegacy)
#' }
#'
#' @export
ConvertMOM <- function(MOM, 
                       Author='', 
                       CurrentYear=NULL, 
                       Seasons=1, 
                       Populate=TRUE, 
                       silent=FALSE) {
  
  CheckClass(MOM, c('MOM'), 'MOM')
  
  if (!silent)
    cli::cli_alert('Converting object of class {.cls MOM} to class {.cls om}')
  
  om <- OM()
  om@Name <-  gsub("REPLACED -- ", '', MOM@Name)
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
  
  om@Seasons <- Seasons
  om@Years <- CalcYears(nYear=om@nYear,
                                pYear=om@pYear,
                                CurrentYear=om@CurrentYear,
                                Seasons)

  YearsList <- list(
    HistTS    = Years(om, 'Historical'),
    ProjTS    = Years(om, 'Projection'),
    TimeUnits = CalcTSUnits(Seasons),
    nYear = om@nYear,
    pYear = om@pYear,
    CurrentYear = om@CurrentYear,
    Seasons   = Seasons
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
  
  for (st in seq_len(nStock)) {
    # om@Stock[[st]]@Depletion@Final <- NULL #
    om@Fleet[[st]]  <- ConvertToList(MOM2fleet(MOM, st, YearsList))
    names(om@Fleet[[st]]) <- FleetNames
    
    if (all(is.finite(om@Fleet[[st]][[1]]@Catchability@Efficiency))) {
      om@Stock[[st]]@Depletion@Final <- NULL
    }
    
  }

  om@Obs <- MakeNamedList(
    StockNames, MakeNamedList(FleetNames))
  
  for (st in seq_len(nStock)) {
    for (fl in seq_len(nFleet)) {
      om@Obs[[st]][[fl]] <- ConvertObs(MOM@Obs[[st]][[fl]], silent = TRUE)
    }
  }

  if (is.list(MOM@CatchFrac) && length(MOM@CatchFrac)) {
    names(MOM@CatchFrac) <- StockNames
    for (st in seq_along(MOM@CatchFrac)) {
      dimnames(MOM@CatchFrac[[st]]) <- list(Sim = seq_len(MOM@nsim),
                                            Fleet = FleetNames)
      MOM@CatchFrac[[st]] <- ReduceDims(MOM@CatchFrac[[st]])
    }
    om@CatchFrac <- MOM@CatchFrac
  } 
  
  if (!length(MOM@Efactor)) {
    om@EFactor <- MakeNamedList(StockNames(om), 
                                array(1, dim=c(om@nSim, nFleet(om)),
                                      dimnames = list(
                                        Sim=seq_len(om@nSim),
                                        Fleet=FleetNames(om)
                                      )
                                ))  
  } else {
    om@EFactor <- MOM@Efactor
  }
  

  if (Populate)
    om <- PopulateOM(om, silent=FALSE)
  
  om
}


MOM2fleet <- function(MOM, st, YearsList) {
  
  nfleets <- length(MOM@Fleets[[1]])
  FleetNames <- names(MOM@Fleets[[1]])
  if (is.null(FleetNames))
    FleetNames <- paste('Fleet', seq_len(nfleets))
  
  FleetList <- list()
  
  for (fl in seq_len(nfleets)) {
    Fleet <- MOM@Fleets[[st]][[fl]]
    cpars <- MOM@cpars[[st]][[fl]]
    Fdisc <- MOM@Stocks[[st]]@Fdisc
    AgeClasses <- GetStockAges(MOM@Stocks[[st]])
    FleetList[[fl]] <- OM2fleet(Fleet, YearsList, cpars, Fdisc, AgeClasses )
  }
  names(FleetList) <- FleetNames
  FleetList
}


MOM2stock <- function(MOM, YearsList=NULL) {
  StockList <- list()
  stocks <- MOM@Stocks
  nstocks <- length(stocks)
  
  for (st in seq_len(nstocks)) {
    Stock <- stocks[[st]]
    cpars <- MOM@cpars[[st]][[1]]
    StockList[[st]] <- OM2stock(Stock, cpars, YearsList, nSim=MOM@nsim, MOM@seed)
  }
  StockList
}
