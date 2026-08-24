#' Plot the Stock-Recruit Relationship
#'
#' Plots recruitment against spawning production, evaluating each stock's
#' `SRR` model (`[SRR()]`) at the last available calendar year. When `object`
#' has more than one simulation, the median curve is drawn with a `probs`
#' quantile ribbon.
#'
#' A bare [stock-class] `object` plots a *relative* curve -- relative
#' recruitment (`R/R0`) against relative spawning production (`SP/SP0`, both
#' `0`-`1`) -- using the stock's own SRR model function (`SRR@Model`, e.g.
#' [BevertonHolt()]) evaluated at `S0 = R0 = 1`. 
#'
#' An [om-class], [hist-class], or [mse-class] `object` instead plots the
#' *absolute* curve (real recruitment against real spawning production, using
#' `SP0` and `R0`).
#'
#' @param object A [stock-class]/[om-class]/[hist-class]/[mse-class] object. 
#' @param Sim Integer or `NULL` (default). Which simulation replicate to
#'   plot. `NULL` takes the median across all simulations and adds a `probs`
#'   quantile ribbon (unless `nSim == 1`).
#' @param probs Numeric vector of length 2. Lower and upper quantiles of the
#'   across-simulation ribbon drawn when `Sim = NULL`. Default `c(0.05, 0.95)`.
#' @param byStock One of `TRUE`, `FALSE`, or `NULL` (default, facets
#'   automatically when `object` has more than one stock). Ignored for a
#'   bare `Stock` (always a single curve).
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks). Ignored for a bare `Stock`.
#' @param nPoints Integer. Number of spawning-production values to evaluate
#'   the curve at. Default `50`.
#'
#' @return A `ggplot` object.
#'
#' @seealso [SRR()], [PlotDepletion()], [Stock()]
#' @export
PlotSRR <- function(object, Sim = NULL, byStock = NULL, Stocks = NULL,
                    nPoints = 50, probs = c(0.05, 0.95)) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')

  if (inherits(object, 'stock')) {
    object <- .EnsureSRRPopulated(object)
    df <- .SRRRelativeCurveDF(object, nPoints) |> dplyr::mutate(Stock = object@Name %||% 'Stock')
    ylab <- 'Relative Recruitment (R/R0)'
    xlab <- 'Relative Spawning Production (SP/SP0)'
  } else {
    OM         <- .ResolveOM(object)
    stockNames <- .ResolveStocks(object, Stocks)
    stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
    allStocks  <- StockNames(OM)

    SPR0 <- .ResolveSPR0(object)

    df <- purrr::map(seq_along(allStocks), \(st) {
      if (!allStocks[st] %in% stockNames) return(NULL)
      .SRRCurveDF(OM@Stock[[st]], SPR0[, allStocks[st], , drop = FALSE], nPoints) |>
        dplyr::mutate(Stock = allStocks[st])
    }) |> dplyr::bind_rows()
    ylab <- 'Recruitment'
    xlab <- 'Spawning Production'
  }

  df$Age <- df$S 
  p <- .BuildSchedulePlot(df, Sim = Sim, byStock = byStock, byFleet = FALSE,
                         ylab = ylab, xlab = xlab,
                         defaultYears = FALSE, breakpointYears = FALSE, probs = probs)
  if (length(unique(df$Year)) == 1)
    p <- p + ggplot2::guides(color = 'none', fill = 'none')
  p
}

.LastSimYearVal <- function(x, sim) {
  d <- dim(x)
  if (is.null(d)) return(x[min(length(x), sim)])
  if (length(d) == 1) return(x[min(d[1], sim)])
  x[min(d[1], sim), d[2]]
}


.EnsureSRRPopulated <- function(Stock) {
  if (!is.null(Stock@SRR@RelRecFun))
    return(Stock)

  nSim        <- if (length(Stock@nSim) && Stock@nSim > 0) Stock@nSim else 5
  CurrentYear <- as.numeric(format(Sys.Date(), '%Y'))
  Years       <- CalcYears(nYear = 20, pYear = 0, CurrentYear = CurrentYear, Seasons = 1)

  Stock@SRR <- PopulateSRR(Stock@SRR, Ages = Stock@Ages, CurrentYear = CurrentYear,
                           Years = Years, nSim = nSim, silent = TRUE)
  Stock@nSim <- nSim
  Stock
}

.SRRRelativeCurveDF <- function(Stock, nPoints) {
  SRR    <- Stock@SRR
  SRRFun <- if (is.character(SRR@Model)) get(SRR@Model) else SRR@Model
  nSim   <- max(Stock@nSim, 1L)
  SPRvec <- seq(0, 1, length.out = nPoints)

  purrr::map(seq_len(nSim), \(sim) {
    Pars <- purrr::map(SRR@Pars, .LastSimYearVal, sim = sim)
    Rvec <- do.call(SRRFun, c(list(S = SPRvec, S0 = 1, R0 = 1), Pars))
    data.frame(Sim = sim, S = SPRvec, Value = Rvec, Year = 'relative')
  }) |> dplyr::bind_rows()
}

.SRRCurveDF <- function(Stock, SPR0stock, nPoints) {
  SRR    <- Stock@SRR
  SRRFun <- if (is.character(SRR@Model)) get(SRR@Model) else SRR@Model

  nYearR0 <- dim(SRR@R0)[2]
  ly      <- nYearR0 # last year index into R0/Pars
  spr0_ly <- dim(SPR0stock)[3]

  nSim <- max(Stock@nSim, 1L)

  R0vec   <- vapply(seq_len(nSim), \(sim) .LastSimYearVal(SRR@R0, sim), numeric(1))
  SPR0vec <- vapply(seq_len(nSim), \(sim)
    SPR0stock[min(dim(SPR0stock)[1], sim), 1, spr0_ly], numeric(1))
  S0vec   <- SPR0vec * R0vec
  Svec    <- seq(0, stats::median(S0vec), length.out = nPoints)

  purrr::map(seq_len(nSim), \(sim) {
    Pars <- purrr::map(SRR@Pars, .LastSimYearVal, sim = sim)
    Rvec <- do.call(SRRFun, c(list(S = Svec, S0 = S0vec[sim], R0 = R0vec[sim]), Pars))

    data.frame(Sim = sim, S = Svec, Value = Rvec, Year = dimnames(SRR@R0)[[2]][ly])
  }) |> dplyr::bind_rows()
}

.ResolveSPR0 <- function(object) {
  if (inherits(object, 'om') || is.null(object@Reference@SPR0))
    return(CalcSPR0(object, silent = TRUE))
  object@Reference@SPR0
}
