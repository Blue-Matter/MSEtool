#' Plot Initial/Final Depletion
#'
#' Histogram of each stock's sampled depletion values (relative to
#' `Depletion@Reference`, e.g. `"B0"` or `"SB0"`) across simulation
#' replicates; the `Initial`/`Final` slots of `[Depletion()]`. Both are
#' shown, side by side, when a stock has both populated.
#'
#' For a [hist-class] or [mse-class] `object`, a second panel is added
#' below: the actual historical relative-biomass trajectory (`[B_B0()]` or
#' `[SB_SB0()]`, matching `Depletion@Reference`) for every simulation, so
#' the sampled targets above can be checked against what was actually
#' achieved. Where `Depletion@Initial`/`Final` are specified, the target
#' value for each simulation is overlaid as a point on its own trajectory
#' at the first/last historical year; a simulation whose achieved value
#' misses its target by more than `tol` (relative) is colored red rather
#' than grey, and listed in a warning (via [cli::cli_warn()]). This can
#' legitimately happen -- e.g. a low target depletion combined with
#' selectivity maturing well before maturity, or a `Final` target above the
#' unfished trajectory (`Unfished@Dynamic`) after a run of poor recruitment,
#' which no catchability can reach; the warning lists these separately.
#' [Simulate()] performs the same check itself, recording any misses to
#' `Hist@Log` (see [Log()]) rather than printing them.
#'
#' For stocks linked by `SRR@SPFrom`, the `Final` target of the source stock
#' applies to the linked group, and is shown in its own panel labelled by the
#' group's stocks: with `Reference = "B0"` the trajectory is the total biomass
#' of the group relative to its total B0, and with `Reference = "SB0"` it is
#' the spawning biomass of the source stock(s) relative to their SB0. `Initial`
#' targets are shown and checked for each stock.
#'
#' @param object   [stock-class], [om-class], [hist-class], or [mse-class] object.
#' @param byStock Logical. Facet by stock? Default `NULL` facets
#'   automatically when `object` has more than one stock.
#' @param Stocks Character or numeric vector. Restrict the plot to specific
#'   stocks, either by name (matching [StockNames()]) or by index. Default
#'   `NULL` (all stocks).
#' @param bins Integer. Number of histogram bins. Default `15`.
#' @param tol Numeric. Relative tolerance for flagging a simulation as
#'   having missed its `Depletion@Initial`/`Final` target -- a simulation is
#'   flagged when `abs(log(achieved / target)) > log(1 + tol)`. Default
#'   `0.1` (10%). Ignored for [stock-class]/[om-class] `object` (nothing yet
#'   simulated to check against).
#'
#' @return A `ggplot` object, or (for [hist-class]/[mse-class] `object`) a
#'   `patchwork` object combining the histogram and trajectory panels.
#'
#' @seealso [Depletion()], [B_B0()], [SB_SB0()], [PlotSRR()], [Stock()]
#' @export
PlotDepletion <- function(object, byStock = NULL, Stocks = NULL, bins = 15, tol = 0.1) {
  .CheckClass(object, c('stock', 'hist', 'mse', 'om'), 'object')
  if (inherits(object, 'stock')) object <- .StockToShellHist(object)
  OM         <- .ResolveOM(object)
  stockNames <- .ResolveStocks(object, Stocks)
  stockNames <- if (is.null(stockNames)) StockNames(OM) else stockNames
  allStocks  <- StockNames(OM)

  units <- .DepletionUnits(OM, stockNames)
  df <- purrr::map(units, \(u) data.frame(Stock = u$Label, Type = u$Type, Value = u$Target)) |>
    dplyr::bind_rows()

  if (!nrow(df))
    cli::cli_abort("No populated {.field Initial}/{.field Final} depletion values found.")
  df$Stock <- factor(df$Stock, levels = unique(df$Stock))

  refs <- purrr::map_chr(seq_along(allStocks), \(st) OM@Stock[[st]]@Depletion@Reference)
  names(refs) <- allStocks
  unitRefs <- unique(purrr::map_chr(units, 'Ref'))
  xlab <- if (length(unitRefs) == 1) paste0('Depletion (', unitRefs, ')') else 'Depletion'

  facetVars <- character(0)
  if (!isFALSE(byStock) && length(unique(df$Stock)) > 1)
    facetVars <- c(facetVars, 'Stock')

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Value, fill = .data$Type)) +
    ggplot2::geom_histogram(bins = bins, position = 'identity', alpha = 0.5, color = NA) +
    ggplot2::theme_bw() +
    ggplot2::expand_limits(x = c(0,1)) +
    ggplot2::labs(x = xlab, y = 'Count', fill = NULL)

  if (length(unique(df$Type)) == 1)
    p <- p + ggplot2::guides(fill = 'none')

  if (length(facetVars))
    p <- p + ggplot2::facet_wrap(facetVars, scales = 'free', labeller = ggplot2::label_wrap_gen(35))

  if (!inherits(object, c('hist', 'mse'))) return(p)

  trendP <- .PlotDepletionTrend(object, stockNames, refs, units, byStock, tol)
  if (is.null(trendP)) return(p)

  patchwork::wrap_plots(list(Target = p, Trend = trendP), ncol = 1)
}

.PlotDepletionTrend <- function(object, stockNames, refs, units, byStock, tol) {
  allStocks <- StockNames(object@OM)
  stockUnits <- purrr::map(stockNames, \(nm)
    list(Label = nm, Ref = refs[[nm]], Stocks = match(nm, allStocks)))
  series <- c(stockUnits, units)
  series <- series[!duplicated(purrr::map_chr(series, 'Label'))]

  trendDF <- purrr::map(series, \(u) .DepletionUnitTrend(object, u)) |> dplyr::bind_rows()
  if (!nrow(trendDF)) return(NULL)
  trendDF$Stock <- factor(trendDF$Stock, levels = purrr::map_chr(series, 'Label'))

  targetDF <- .ComputeDepletionAchievement(object, units, tol, trendDF)
  if (nrow(targetDF))
    targetDF$Stock <- factor(targetDF$Stock, levels = levels(trendDF$Stock))

  if (nrow(targetDF)) {
    failed <- dplyr::filter(targetDF, !.data$Pass)
    if (nrow(failed)) {
      msg <- failed |>
        dplyr::group_by(.data$Stock, .data$Type) |>
        dplyr::summarise(Sims = paste(sort(.data$Sim), collapse = ', '), .groups = 'drop') |>
        dplyr::mutate(line = paste0(.data$Stock, ' ', .data$Type, ': Sim ', .data$Sims)) |>
        dplyr::pull('line')
      unreach <- dplyr::filter(failed, .data$Unreachable) |>
        dplyr::group_by(.data$Stock) |>
        dplyr::summarise(Sims = paste(sort(.data$Sim), collapse = ', '), .groups = 'drop') |>
        dplyr::mutate(line = paste0(.data$Stock, ' Final: Sim ', .data$Sims)) |>
        dplyr::pull('line')
      cli::cli_warn(c(
        "!" = "{length(unique(failed$Sim))} simulation{?s} missed {?its/their} target depletion by more than {tol*100}%:",
        stats::setNames(msg, rep('*', length(msg))),
        if (length(unreach))
          c("i" = "Target is above the unfished depletion ({.code Unfished@Dynamic}) and cannot be reached at any catchability:",
            stats::setNames(unreach, rep('*', length(unreach))))
      ))
    }
  }

  seriesRefs <- unique(purrr::map_chr(series, 'Ref'))
  ylab <- if (length(seriesRefs) == 1) paste0('Depletion (', seriesRefs, ')') else 'Depletion'

  p <- ggplot2::ggplot(trendDF, ggplot2::aes(x = .data$Year, y = .data$Value, group = .data$Sim)) +
    ggplot2::geom_line(alpha = 0.3, color = 'grey30') +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = ylab)

  if (nrow(targetDF))
    p <- p + ggplot2::geom_point(
      data = targetDF,
      ggplot2::aes(x = .data$Year, y = .data$Target, color = .data$Pass, shape = .data$Type),
      inherit.aes = FALSE, size = 1.8
    ) +
    ggplot2::scale_color_manual(values = c('TRUE' = 'grey20', 'FALSE' = 'red'), guide = 'none') +
    ggplot2::labs(shape = NULL)

  if (!isFALSE(byStock) && length(unique(trendDF$Stock)) > 1)
    p <- p + ggplot2::facet_wrap('Stock', scales = 'free', labeller = ggplot2::label_wrap_gen(35))

  p
}


# Depletion targets on the basis they are fitted: `Initial` per stock, `Final` per SPFrom-linked group
.DepletionUnits <- function(OM, stockNames = StockNames(OM)) {
  allStocks <- StockNames(OM)
  initial <- purrr::map(seq_along(allStocks), \(st) {
    Dep <- OM@Stock[[st]]@Depletion
    if (!allStocks[st] %in% stockNames || !length(Dep@Initial)) return(NULL)
    list(Label = allStocks[st], Type = 'Initial', Ref = Dep@Reference, Stocks = st,
         Linked = FALSE, Target = as.numeric(Dep@Initial))
  })
  final <- purrr::map(.SPFromConnectedGroups(OM), \(g) {
    if (!any(allStocks[g$members] %in% stockNames)) return(NULL)
    src <- purrr::detect(g$sources, \(s) length(OM@Stock[[s]]@Depletion@Final) > 0)
    if (is.null(src)) return(NULL)
    Dep    <- OM@Stock[[src]]@Depletion
    Linked <- length(g$members) > 1
    Stocks <- if (Dep@Reference == 'B0') g$members else g$sources
    Label  <- if (!Linked) allStocks[src] else if (Dep@Reference == 'B0')
      paste0(paste(allStocks[g$members], collapse = ' + '), ' (total B/B0)')
    else
      paste0(paste(allStocks[g$members], collapse = ' + '), ' (SB/SB0 of ',
             paste(allStocks[g$sources], collapse = ' + '), ')')
    list(Label = Label, Type = 'Final', Ref = Dep@Reference, Stocks = Stocks,
         Linked = Linked, Target = as.numeric(Dep@Final))
  })
  purrr::compact(c(initial, final))
}

# Historical B/B0 or SB/SB0 of the summed stocks in a depletion unit
.DepletionUnitTrend <- function(object, unit) {
  slt <- if (identical(unit$Ref, 'B0')) 'Biomass' else 'SBiomass'
  if (is.null(slot(object@Unfished@Equilibrium, slt))) return(NULL)
  .ExtractRelative(object, num_slot = slt, denom_slot = slt, ref = 'Unfished',
                   var_name = 'Depletion', type = 'Equilibrium', Extend = TRUE,
                   stockNames = StockNames(object@OM)[unit$Stocks], sumStock = TRUE) |>
    dplyr::filter(.data$Period == 'Historical') |>
    dplyr::transmute(Sim = .data$Sim, Stock = unit$Label, Year = .data$Year, Value = .data$Value)
}

.ComputeDepletionAchievement <- function(object, units, tol, trendDF = NULL) {
  purrr::map(units, \(unit) {
    trend <- if (!is.null(trendDF) && unit$Label %in% trendDF$Stock)
      dplyr::filter(trendDF, .data$Stock == unit$Label)
    else .DepletionUnitTrend(object, unit)
    if (is.null(trend) || !nrow(trend)) return(NULL)
    yr <- if (unit$Type == 'Initial') min(trend$Year) else max(trend$Year)
    achieved <- dplyr::filter(trend, .data$Year == yr) |> dplyr::arrange(.data$Sim)
    target   <- rep(unit$Target, length.out = nrow(achieved))
    unfished <- if (unit$Type == 'Final')
      .UnfishedDepletion(object, unit$Stocks, unit$Ref, yr, achieved$Sim)
    else rep(NA_real_, nrow(achieved))
    data.frame(
      Stock    = unit$Label, Type = unit$Type, Ref = unit$Ref, Linked = unit$Linked,
      Sim      = achieved$Sim, Year = yr,
      Target   = target, Achieved = achieved$Value,
      Pass     = abs(log(achieved$Value / target)) <= log(1 + tol),
      Unfished = unfished,
      Unreachable = !is.na(unfished) & unfished < target
    )
  }) |> dplyr::bind_rows()
}

# Dynamic-unfished biomass relative to equilibrium B0/SB0, summed over `stocks`: the ceiling any catchability can reach
.UnfishedDepletion <- function(object, stocks, ref, yr, sims) {
  slt <- if (identical(ref, 'B0')) 'Biomass' else 'SBiomass'
  dyn <- slot(object@Unfished@Dynamic, slt)
  eq  <- slot(object@Unfished@Equilibrium, slt)
  if (!length(dyn) || !length(eq)) return(rep(NA_real_, length(sims)))

  nms   <- StockNames(object@OM)[stocks]
  ratio <- ArrayDivide(SumOverStock(.SubsetStockDim(dyn, nms)),
                       SumOverStock(.SubsetStockDim(eq, nms)))
  dn  <- dimnames(ratio)
  idx <- list(
    Sim   = if (length(dn$Sim) == 1) rep(1L, length(sims)) else match(as.character(sims), dn$Sim),
    Year  = rep(if (length(dn$Year) == 1) 1L else match(as.character(yr), dn$Year), length(sims))
  )
  as.numeric(ratio[do.call(cbind, idx[names(dn)])])
}

.DepletionRatioLabel <- function(ref) if (identical(ref, 'B0')) 'B/B0' else 'SB/SB0'

.LogDepletionAchievement <- function(Hist, tol = 0.1) {
  achieved <- .ComputeDepletionAchievement(Hist, .DepletionUnits(Hist@OM), tol)
  if (!nrow(achieved))
    return(Hist)
  failed   <- dplyr::filter(achieved, !.data$Pass)
  if (!nrow(failed)) return(Hist)

  nSim <- Hist@OM@nSim
  for (rows in split(failed, list(failed$Stock, failed$Type), drop = TRUE)) {
    st <- rows$Stock[1]; type <- rows$Type[1]; n <- nrow(rows)
    ratio <- .DepletionRatioLabel(rows$Ref[1])
    nUnreach <- sum(rows$Unreachable)
    msg <- cli::format_inline(
      "{type} depletion is more than {round(tol * 100)}% from its target in {n} of {nSim} simulation{?s}."
    )
    if (nUnreach > 0)
      msg <- paste(msg, cli::format_inline(
        "In {nUnreach} simulation{?s} the target is above the unfished {ratio} ({.code Unfished@Dynamic}), so it cannot be reached at any catchability."
      ))
    msg <- paste(msg, cli::format_inline("See {.code PlotDepletion(hist)}."))
    Hist <- .CaptureLog(Hist, msg, name = paste0('Depletion - ', st), type = 'warning')
    for (i in seq_len(n)) {
      detail <- sprintf("Achieved %s, target %s", round(rows$Achieved[i], 3), round(rows$Target[i], 3))
      if (rows$Unreachable[i])
        detail <- sprintf("%s; unfished %s %s (unreachable)", detail, ratio, round(rows$Unfished[i], 3))
      Hist <- .CaptureLog(Hist, detail, type = 'warning', sim = rows$Sim[i])
    }
  }
  Hist
}
