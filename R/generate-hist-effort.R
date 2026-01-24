#' Generate Stochastic Historical Effort Time Series
#'
#' Generates stochastic historical fishing effort trajectories from an
#' `data.frame` containing effort control points.
#' 
#' For each simulation, effort is sampled uniformly between specified
#' lower and upper bounds at control years, linearly interpolated to a
#' full annual time series, optionally perturbed by lognormal process
#' error, and finally normalised to equal 1 in the terminal year.
#'
#' @param Effort A `data.frame` with columns:
#'   \describe{
#'     \item{Year}{Calendar years or indices defining effort control points.}
#'     \item{Lower}{Lower bound on relative effort at each control year.}
#'     \item{Upper}{Upper bound on relative effort at each control year.}
#'     \item{CV}{Coefficient of variation for lognormal annual effort noise
#'       (typically constant across years; only the first value is used).}
#'   }
#' @param nSim Integer. Number of stochastic effort simulations to generate.
#' @param Years Optional integer vector of calendar years defining the full
#'   effort time series. Required if `Effort$Year` is specified
#'   using indices rather than calendar years.
#'
#' @details
#' For each simulation:
#' \enumerate{
#'   \item Effort at each control year is drawn independently from a
#'     uniform distribution between \code{Lower} and \code{Upper}.
#'   \item Effort is linearly interpolated to annual resolution across
#'     \code{Years}.
#'   \item If \code{CV} is non-\code{NULL}, multiplicative lognormal
#'     process error is applied with mean 1.
#'   \item The resulting effort trajectory is normalised so that effort
#'     in the final year equals 1.
#' }
#'
#' @return An \code{nSim x nYear} numeric array used in an [Effort()] object
#' @seealso [Effort()]
#' 
#' @export
GenHistEffort <- function(Effort, nSim=5, Years=NULL) {
  if (!methods::is(Effort, 'data.frame')) {
    cli::cli_abort('`Effort` must be a data.frame')
  }
  
  required_cols <- c("Year", "Lower", "Upper", "CV")
  if (!all(names(Effort) %in% required_cols)) {
    cli::cli_abort(
      "`Effort` must contain columns: {.val {required_cols}}"
    )
  }
  
  
  # Check year indexing 
  if (any(Effort$Year < 1600)) {
    if (is.null(Years)) {
      cli::cli_abort(
        "`Effort$Year` must be calendar years, or `Years` must be supplied",  call = NULL
      )
    }
    Effort$Year <- c(min(Years), Years[Effort$Year * length(Years)])
  }
  
  if (is.null(Years)) {
    Years <- seq(min(Effort$Year), max(Effort$Year), by = 1)
  }
  
  nYears <- length(Years)
  
  # Check bounds 
  if (any(Effort$Lower > Effort$Upper)) {
    cli::cli_abort(
      "`Lower` must be less than or equal to `Upper` for all rows", call = NULL
    )
  }
  
  # Sample effort at control points
  EffortPoints <- mapply(runif,
                         n = nSim,
                         min = Effort$Lower,
                         max = Effort$Upper)
  
  # Interpolate to annual time series
  if (nSim > 1) {
    EffortTS <- t(vapply(
      seq_len(nSim),
      function(i) {
        approx(
          x = Effort$Year,
          y = EffortPoints[i, ],
          n = nYears,
          method = "linear"
        )$y
      },
      numeric(nYears)
    ))
  } else {
    EffortTS <- approx(
      x = Effort$Year,
      y = EffortPoints,
      n = nYears,
      method = "linear"
    )$y
  }
  
  # Apply log-normal process error
  Esd <- Effort$CV[1]
  if (!is.null(Esd)) {
    Emu <- -0.5 * Esd^2
    EffortError <- array(
      exp(rnorm(nYears * nSim, Emu, Esd)),
      dim = c(nSim, nYears)
    )
    EffortTS <- EffortTS * EffortError
  }
  
  # Dimnames and normalize
  dimnames(EffortTS) <- list(Sim=1:nSim,
                             Year=Years)

  EffortTS <- EffortTS / matrix(EffortTS[, nYears], nSim, nYears, byrow = FALSE)

  EffortTS
}
