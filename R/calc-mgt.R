#' Mean Generation Time
#'
#' Calculates the mean generation time (MGT), defined as the mean age of a
#' mature female in the population at unfished equilibrium under natural
#' mortality only. MGT is used as the time unit for the \eqn{B_\text{Low}}
#' reference point calculation.
#'
#' @param OM An [om-class] or [hist-class] object.
#' @param Years Numeric; calendar year(s) at which MGT is evaluated. If `NULL`
#'   (default), the final historical year is used. For seasonal models, specify
#'   integer calendar years; all seasons within each year are used automatically.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#'
#' MGT is computed as:
#'
#' \deqn{\text{MGT} = \frac{\sum_a a \cdot l_a \cdot m_a}{\sum_a l_a \cdot m_a}}
#'
#' where \eqn{l_a} is survivorship to age \eqn{a} under natural mortality only
#' (with \eqn{l_1 = 1}) and \eqn{m_a} is maturity-at-age. In seasonal models
#' age classes are in decimal-year units (e.g. 0.25, 0.5, ...) and \eqn{M_a}
#' is the per-season instantaneous rate, so the formula gives MGT in years
#' consistent with an equivalent annual model.
#'
#' Parameters are taken from the last season of each requested calendar year.
#' If M or maturity vary by year, MGT will vary correspondingly.
#'
#' @return An array with dimensions `[Sim, Stock, Year]` containing MGT in
#'   years. Dimensions where values are identical across simulations or years
#'   are collapsed by [ReduceDims()]. Like [CalcSPR0()] and [CalcMSY()],
#'   `CalcMGT()` does not itself store the result -- assign it to
#'   `Hist@Reference@MGT` if needed.
#'
#' @seealso [CalcSPR0()], [reference-class]
#' @export
CalcMGT <- function(OM, Years = NULL, silent = FALSE) {

  .CheckClass(OM, c('om', 'hist'))

  if (inherits(OM, 'om')) {
    OM   <- Populate(OM, silent = silent)
    Hist <- .OM2Hist(OM = OM, silent = silent)
  } else {
    Hist <- OM
  }

  HistYears  <- Years(Hist, 'H')
  nSeason    <- Hist@OM@Seasons
  StockNames <- StockNames(Hist)
  nSim_      <- nSim(Hist)

  if (is.null(Years)) {
    Years <- utils::tail(HistYears, 1)
    if (nSeason > 1L) Years <- unique(floor(Years))
  }

  # For seasonal models, snap to integer years and expand to all season steps.
  if (nSeason > 1L) {
    cal_yrs <- unique(floor(Years))
    ts_use  <- HistYears[floor(HistYears) %in% cal_yrs]
  } else {
    ts_use <- Years
  }

  # Use the last season of each calendar year for parameter lookup.
  # For annual models this is just 'Years'.
  cal_years <- if (nSeason > 1L) cal_yrs else Years
  nCalYears <- length(cal_years)

  # One M and maturity-at-age entry per requested calendar year (last season).
  last_ts_per_year <- if (nSeason > 1L) {
    purrr::map_dbl(cal_yrs, \(y) utils::tail(ts_use[floor(ts_use) == y], 1))
  } else {
    cal_years
  }

  MGT_list <- purrr::map(Hist@OM@Stock, \(stock) {

    Ages <- stock@Ages@Classes   # decimal year units
    nAge <- length(Ages)

    M   <- stock@NaturalMortality@MeanAtAge |>
             Extend(nSim = nSim_, Years = HistYears)
    Mat <- stock@Maturity@MeanAtAge |>
             Extend(nSim = nSim_, Years = HistYears)

    SimNames <- dimnames(M)[['Sim']]

    mgt <- array(
      NA_real_,
      dim      = c(nSim_, nCalYears),
      dimnames = list(Sim = SimNames, Year = as.character(cal_years))
    )

    for (cy in seq_len(nCalYears)) {
      t_ref <- last_ts_per_year[cy]
      m_mat   <- M[,   , which(dimnames(M)[['Year']]   == as.character(t_ref)), drop = FALSE]
      mat_mat <- Mat[, , which(dimnames(Mat)[['Year']] == as.character(t_ref)), drop = FALSE]

      for (sim in seq_len(nSim_)) {
        m_a   <- as.numeric(m_mat[sim, , 1])
        mat_a <- as.numeric(mat_mat[sim, , 1])

        # Survivorship under M only: l_1 = 1, l_{a+1} = l_a * exp(-M_a)
        l_a <- cumprod(c(1, exp(-m_a[-nAge])))

        denom      <- sum(l_a * mat_a)
        mgt[sim, cy] <- if (denom > 0) sum(Ages * l_a * mat_a) / denom else NA_real_
      }
    }
    mgt
  })

  result <- List2Array(MGT_list, 'Stock', pos = 2)
  dimnames(result)[['Stock']] <- StockNames
  ReduceDims(result)
}
