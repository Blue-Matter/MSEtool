
#' Retrieve Operating Model Metadata
#'
#' Extracts key metadata from an operating model (OM) object, including
#' number of simulations, stock-specific age classes,
#' number of areas, and stock names and fleet names
#'
#' @param OM An operating model object.
#' @param Period Character string specifying the model period. Must be one
#'   of `"Historical"`, `"Projection"`, or `"All"`. Default is `"All"`.
#' @param Years Optional vector of years to subset the OM. If `NULL`, all
#'   years for the specified period are returned.
#'
#' @return A named list with the following elements:
#' * `nSim` — number of simulations in the OM
#' * `AgeClasses` — list of age classes for each stock
#' * `nAges` — list of the number of age classes for each stock
#' * `nAreas` — number of spatial areas (must be consistent across stocks)
#' * `StockNames` — character vector of stock names
#' * `Years` — vector of years for the specified period
#' * `Period` — resolved period argument
#' * `FleetNames` — character vector of fleet names (from the first stock)
#'
#' @examples
#' \dontrun{
#' meta <- GetMetaData(MyOM, Period = "Historical")
#' meta$nSim
#' meta$AgeClasses
#' }
#'
#' @export
GetMetaData <- function(OM, Period=c('Historical', 'Projection', 'All'), Years=NULL) {
  
  Period <- match.arg(Period)
  
  if (is.null(Years))
    Years <- Years(OM, Period)
  
  AgeClasses <- purrr::map(OM@Stock, \(stock)
                           stock@Ages@Classes
  )
  
  nAges <- purrr::map(AgeClasses, length)
  nAreas <- unlist(purrr::map(OM@Stock, nArea)) |> unique()
  
  if (length(nAreas)>1) 
    cli::cli_abort('All Stocks must have the same number of areas')
  
  FleetNames <- FleetNames(OM)
  if (is.list(FleetNames))
    FleetNames <- FleetNames[[1]]
  
  list(nSim=nSim(OM),
       AgeClasses=AgeClasses,
       nAges=nAges,
       nAreas=nAreas,
       StockNames=StockNames(OM),
       Years=Years,
       Period=Period,
       FleetNames=FleetNames)
}
