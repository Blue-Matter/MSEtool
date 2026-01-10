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
  
  list(nSim=nSim(OM),
       AgeClasses=AgeClasses,
       nAges=nAges,
       nAreas=nAreas,
       StockNames=StockNames(OM),
       Years=Years,
       Period=Period,
       FleetNames=FleetNames(OM)[[1]])
}