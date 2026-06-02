InitMSYRefPoints <- function(Hist, Years = NULL) {
  
  if (is.null(Years))
    Years <- utils::tail(Years(Hist@OM, 'Historical'), 1)
  
  nSim <- Hist@OM@nSim
  n_ts <- length(Years)
  
  Complexes     <- Hist@OM@Complexes
  n_complexes   <- length(Complexes)
  complex_names <- names(Complexes)
  stock_names   <- StockNames(Hist)
  n_stock       <- length(stock_names)
  
  MSYRefPoints <- new("refpointsMSY")
  
  stock_arr <- function() array(NA, dim = c(nSim, n_stock, n_ts),
                                dimnames = list(Sim = seq_len(nSim),
                                                Stock = stock_names,
                                                Year  = Years))
  
  MSYRefPoints@FMSY <- array(NA, 
                             dim = c(nSim, n_complexes, n_ts),
                             dimnames = list(
                               Sim   = seq_len(nSim),
                               Stock = complex_names,
                               Year  = Years)
  )
  
  MSYRefPoints@BMSY        <- stock_arr()
  MSYRefPoints@SBMSY       <- stock_arr()
  MSYRefPoints@SPMSY       <- stock_arr()
  MSYRefPoints@SPRMSY      <- stock_arr()
  MSYRefPoints@MSYLandings <- stock_arr()
  MSYRefPoints@MSYDiscards <- stock_arr()
  
  Hist@Reference@MSY <- MSYRefPoints
  Hist
  
}