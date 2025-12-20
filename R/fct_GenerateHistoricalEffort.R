
#' Generate a Historical Effort Trend 
#' 
#' @export
GenerateHistoricalEffort <- function(Effort, nSim=5, Years=NULL) {
  if (!methods::is(Effort@Value, 'data.frame'))
    cli::cli_abort('`Effort@Value` must be a data.frame')
  
  if (!all(names(Effort@Value) %in% c("Year", "Lower", "Upper", "CV" )))
    cli::cli_abort('`Effort@Value` must be a data.frame with columns: "Year", "Lower", "Upper", "CV" ')
  
  
  if (any(Effort@Value$Year< 1600)) {
    if (is.null(Years)) {
      cli::cli_abort("`Effort@Value$Year` must contain calendar years, or `Years` must be specified")
    }
    Effort@Value$Year <- c(min(Years),
                               Years[Effort@Value$Year * length(Years)])
  }
  
  if (is.null(Years)) 
    Years <- seq(min(Effort@Value$Year), by=1, to=max(Effort@Value$Year))
  
  if (all(Effort@Value$Year< 1000)) {
    chk <- max(Effort@Value$Year) %in% seq_along(Years)  
    if (!chk)
      cli::cli_abort('`max(Effort@Value$Year)` ({.val {max(Effort@Value$Year)}})')
  } else {
    chk <- max(Effort@Value$Year) %in% Years
    if (!chk)
      cli::cli_abort('`max(Effort@Value$Year)` ({.val {max(Effort@Value$Year)}})')
  }
  
  nYears <- length(Years)
  
  if (any(Effort@Value$Lower-Effort@Value$Upper > 0))
    cli::cli_abort('`Effort@Value$Lower` ({.val {Effort@Value$Lower}}) must be lower than `Effort@Value$Upper` ({.val {Effort@Value$Upper}})', call=NULL)
  
  EffortPoints <- mapply(runif, n = nSim, min = Effort@Value$Lower, max = Effort@Value$Upper)  # sample Effort
  if (nSim>1) {
    EffortTS <- t(sapply(1:nSim, function(x) 
      approx(x = Effort@Value$Year,
             y = EffortPoints[x, ], 
             method = "linear", 
             n = nYears)$y)
    )
  } else {
    EffortTS <- approx(x = Effort@Value$Year,
                       y = EffortPoints,
                       method = "linear", 
                       n = nYears)$y
  }
  
  Esd <- Effort@Value$CV[1]
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
  Effort@Value <- EffortTS
  Effort@Units <- 'unitless'
  Effort
}
