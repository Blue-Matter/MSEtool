.InitMSYRefPoints <- function(Hist, Years = NULL) {
  
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


.ValidateRefpointsMSY <- function(Hist, refpointsMSY, Years) {
  .CheckClass(refpointsMSY, 'refpointsMSY', 'refpointsMSY')

  nSim_         <- nSim(Hist)
  stock_names   <- StockNames(Hist)
  complex_names <- names(Hist@OM@Complexes)

  expected <- list(
    FMSY        = complex_names,
    BMSY        = stock_names,
    SBMSY       = stock_names,
    SPMSY       = stock_names,
    SPRMSY      = stock_names,
    MSYLandings = stock_names,
    MSYDiscards = stock_names
  )

  problems <- character(0)
  for (sl in names(expected)) {
    arr <- slot(refpointsMSY, sl)
    if (is.null(arr)) {
      problems <- c(problems, "{.field {sl}} is missing ({.code NULL})")
      next
    }
    dn <- dimnames(arr)

    if (is.null(dn$Sim) || !length(dn$Sim) %in% c(1L, nSim_))
      problems <- c(problems, paste0(
        sl, ": expected 1 or ", nSim_, " simulation(s), got ",
        if (is.null(dn$Sim)) 0 else length(dn$Sim)
      ))

    if (!identical(dn$Stock, expected[[sl]]))
      problems <- c(problems, paste0(sl, ": Stock dimnames do not match current OM"))

    if (!identical(as.character(dn$Year), as.character(Years)))
      problems <- c(problems, paste0(
        sl, ": Year dimnames (", paste(dn$Year, collapse=', '),
        ") do not match expected (", paste(Years, collapse=', '), ")"
      ))
  }

  if (length(problems) > 0) {
    cli::cli_abort(c(
      "Supplied {.arg refpointsMSY} is not compatible with {.arg OM}:",
      stats::setNames(problems, rep('x', length(problems)))
    ), call = NULL)
  }

  invisible(refpointsMSY)
}
