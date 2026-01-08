
#' Generate a Historical Effort Trend 
#' 
#' @export
GenerateHistoricalEffort <- function(Effort, nSim=5, Years=NULL) {
  if (!methods::is(Effort@Effort, 'data.frame'))
    cli::cli_abort('`Effort@Effort` must be a data.frame')
  
  if (!all(names(Effort@Effort) %in% c("Year", "Lower", "Upper", "CV" )))
    cli::cli_abort('`Effort@Effort` must be a data.frame with columns: "Year", "Lower", "Upper", "CV" ')
  
  
  if (any(Effort@Effort$Year< 1600)) {
    if (is.null(Years)) {
      cli::cli_abort("`Effort@Effort$Year` must contain calendar years, or `Years` must be specified")
    }
    Effort@Effort$Year <- c(min(Years),
                               Years[Effort@Effort$Year * length(Years)])
  }
  
  if (is.null(Years)) 
    Years <- seq(min(Effort@Effort$Year), by=1, to=max(Effort@Effort$Year))
  
  if (all(Effort@Effort$Year< 1000)) {
    chk <- max(Effort@Effort$Year) %in% seq_along(Years)  
    if (!chk)
      cli::cli_abort('`max(Effort@Effort$Year)` ({.val {max(Effort@Effort$Year)}})')
  } else {
    chk <- max(Effort@Effort$Year) %in% Years
    if (!chk)
      cli::cli_abort('`max(Effort@Effort$Year)` ({.val {max(Effort@Effort$Year)}})')
  }
  
  nYears <- length(Years)
  
  if (any(Effort@Effort$Lower-Effort@Effort$Upper > 0))
    cli::cli_abort('`Effort@Effort$Lower` ({.val {Effort@Effort$Lower}}) must be lower than `Effort@Effort$Upper` ({.val {Effort@Effort$Upper}})', call=NULL)
  
  EffortPoints <- mapply(runif, n = nSim, min = Effort@Effort$Lower, max = Effort@Effort$Upper)  # sample Effort
  if (nSim>1) {
    EffortTS <- t(sapply(1:nSim, function(x) 
      approx(x = Effort@Effort$Year,
             y = EffortPoints[x, ], 
             method = "linear", 
             n = nYears)$y)
    )
  } else {
    EffortTS <- approx(x = Effort@Effort$Year,
                       y = EffortPoints,
                       method = "linear", 
                       n = nYears)$y
  }
  
  Esd <- Effort@Effort$CV[1]
  if (!is.null(Esd)) {
    Emu <- -0.5 * Esd^2
    EffortError <- array(exp(rnorm(nYears * nSim, rep(Emu, nYears), 
                                   rep(Esd, nYears))), 
                         c(nSim, nYears))  
    EffortTS <- EffortTS * EffortError
  }
  EffortTS <- EffortTS |> AddDimNames(names=c('Sim', 'Year'), 
                                      Years = Years)
  
  EffortTS <- EffortTS/matrix(EffortTS[,nYears], nSim, nYears, byrow=FALSE)
  Effort@Effort <- EffortTS
  Effort@Units <- 'unitless'
  Effort
}
