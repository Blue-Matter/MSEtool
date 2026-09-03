#' Plot Observed/Simulated Index Against the Underlying Population Trend
#'
#' Overlays the `CPUE`/`Survey` index, on its own natural scale, against the
#' underlying population trend scaled to match it. Diagnostic for how well
#' an index tracks the simulated dynamics (hyperstability, error,
#' autocorrelation). See [IndexFitTable()] for the matching parameter
#' summary.
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param type Character. One of `"CPUE"` or `"Survey"`.
#' @param Fleets Character vector. Restrict to these fleet names. Default
#'   `NULL` (all fleets with a populated `type` index).
#' @param Stocks Character vector. Restrict to these stock/complex names
#'   (matching `names(object@OM@Complexes)`, or `StockNames()` when
#'   `Complexes` is unset). Default `NULL` (all).
#' @param probs Numeric vector of length 2. Lower and upper quantiles of the
#'   shaded ribbon across simulations. Default `c(0.05, 0.95)`.
#' @param nsim Integer. Number of individual simulation replicates to overlay
#'   as thin lines, in addition to the median/ribbon. Default `0` (none).
#' @param Years Optional numeric vector. Subset the time series to these
#'   years before plotting.
#' @param IncHist Logical. For [mse-class] objects, include the historical
#'   period? Default `TRUE`. No effect for [hist-class] objects.
#' @param free_y Logical. Let each facet panel's y-axis scale independently.
#'   Default `TRUE`.
#'
#' @details
#' The `Index` series is plotted on its natural scale. The `True` (OM)
#' series is put on that same scale, per simulation, via
#' `Efficiency * NomIndex^Beta`. 
#'
#' Panels are faceted by `Fleet` (and `Stock` when more than one
#' stock/complex is plotted). For [mse-class] objects with more than one MP,
#' an `MP` facet column is added; the historical period is repeated in every
#' MP's panel so each line is unbroken across the historical/projection
#' boundary.
#'
#'
#' @return A `ggplot` object, or `NULL` invisibly if no populated `type`
#'   index is found.
#'
#' @seealso [IndexFitTable()], [PlotCPUE()], [PlotSurvey()], [Biomass()],
#'   [Number()]
#' @export
PlotIndexFit <- function(object,
                         type    = c('CPUE', 'Survey'),
                         Fleets  = NULL,
                         Stocks  = NULL,
                         probs   = c(0.05, 0.95),
                         nsim    = 0,
                         Years   = NULL,
                         IncHist = TRUE,
                         free_y  = TRUE) {
  type <- match.arg(type)
  .CheckClass(object, c('hist', 'mse'), 'object')

  df <- .BuildIndexFitDF(object, type, Fleets = Fleets, Stocks = Stocks)
  
  if (is.null(df) || !nrow(df)) {
    cli::cli_alert_info("No populated {.field {type}} index found to plot.")
    return(invisible(NULL))
  }

  df <- df |>
    .DropHistorical(IncHist) |>
    .FilterYears(Years)

  .BuildIndexFitPlot(df, probs = probs, nsim = nsim, free_y = free_y)
}

#' Summarize Fitted/Estimated Index Observation Error Parameters
#'
#' Tabulates the hyperstability (`Beta`), Beta fit diagnostics (`R2`,
#' `PValue`, `Status`), and residual (`SD`, `AC`, `nYears`) parameters
#' stored on each fleet's `CPUE`/`Survey` [indicesobs-class] object.
#' Companion summary for [PlotIndexFit()].
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param type Character. One of `"CPUE"` or `"Survey"`.
#' @param Fleets Character vector. Restrict to these fleet names. Default
#'   `NULL` (all fleets with a populated `type` index).
#' @param Stocks Character vector. Restrict to these stock/complex names.
#'   Default `NULL` (all).
#' @param print Logical. Print a markdown table to console with `cat()`?
#'   Default `TRUE`.
#'
#' @details
#' Each parameter may vary by simulation. When more than one distinct value
#' is present, the column reports `median [min, max]`; a single fixed value
#' is reported as-is. `R2` and `PValue` come from the per-simulation `Beta`
#' fit (`Misc$BetaFit`, see [EstimateBeta()]) and are `NA` where `Beta` was
#' not freely estimated (fixed by the user or `SimControl(EstimateBeta =
#' FALSE)`). `Status` lists the distinct fit outcomes across simulations
#' (`"estimated"`, `"fixed_user"`, `"fixed_insufficient_data"`,
#' `"fixed_low_variance"`, `"fixed_not_significant"`, `"fixed_bounds"`).
#' `nYears` is the number of years used in the fit. `Beta`, `R2`, `PValue`,
#' and `Status` are placed last since they're only meaningful when Beta
#' estimation was attempted; a column that is `NA` for every row (e.g. `R2`/
#' `PValue` when `Beta` was fixed everywhere) is dropped from the result.
#'
#' @return returns the `data.frame`.
#'
#' @seealso [PlotIndexFit()], [IndicesObs()]
#' @export
IndexFitTable <- function(object,
                          type   = c('CPUE', 'Survey'),
                          Fleets = NULL,
                          Stocks = NULL,
                          print  = TRUE) {
  type <- match.arg(type)
  .CheckClass(object, c('hist', 'mse'), 'object')

  OM <- object@OM
  Complexes <- .ResolveComplexes(OM)

  stockNames <- names(Complexes)
  if (!is.null(Stocks)) stockNames <- intersect(stockNames, Stocks)

  rows <- purrr::map_dfr(stockNames, function(cx) {
    obsList <- OM@Obs[[cx]]
    if (is.null(obsList)) return(NULL)

    fleetNames <- names(obsList)
    if (!is.null(Fleets)) fleetNames <- intersect(fleetNames, Fleets)

    purrr::map_dfr(fleetNames, function(fl) {
      idxObs <- slot(obsList[[fl]], type)
      if (EmptyObject(idxObs)) return(NULL)

      stats   <- idxObs@Stats
      BetaFit <- idxObs@Misc$BetaFit
      data.frame(
        Stock  = cx,
        Fleet  = fl,
        nYears = if (!is.null(BetaFit) && length(BetaFit$nPoints)) as.integer(BetaFit$nPoints[1]) else NA_integer_,
        SD     = .SummarizeParam(if (!is.null(stats)) stats$SD else NULL),
        AC     = .SummarizeParam(if (!is.null(stats)) stats$AC else NULL),
        Beta   = .SummarizeParam(idxObs@Beta %||% 1),
        R2     = .SummarizeParam(if (!is.null(BetaFit)) BetaFit$R2 else NULL),
        PValue = .SummarizeParam(if (!is.null(BetaFit)) BetaFit$PValue else NULL),
        Status = if (!is.null(BetaFit)) paste(unique(BetaFit$Status), collapse = ', ') else NA_character_,
        stringsAsFactors = FALSE
      )
    })
  })

  if (is.null(rows) || !nrow(rows)) {
    cli::cli_alert_info("No populated {.field {type}} index found to summarize.")
    return(invisible(NULL))
  }

  allNA   <- vapply(rows, function(x) all(is.na(x)), logical(1))
  dropCol <- names(rows)[allNA & !(names(rows) %in% c('Stock', 'Fleet'))]
  rows    <- rows[, setdiff(names(rows), dropCol), drop = FALSE]

  rownames(rows) <- NULL
  rows
}

# ---- internal helpers ----

.ResolveComplexes <- function(OM) {
  if (!is.null(OM@Complexes)) return(OM@Complexes)
  stockNames <- StockNames(OM)
  stats::setNames(as.list(seq_along(stockNames)), stockNames)
}

.SummarizeParam <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_character_)
  if (length(unique(x)) == 1) return(formatC(x[1], digits = 3, format = 'g'))
  sprintf('%.3g [%.3g, %.3g]', stats::median(x), min(x), max(x))
}

.ExtractIndexSeries <- function(object, isMSE, type, cx, fl) {
  if (!isMSE) {
    df <- .IndexSeriesFromDataList(purrr::map(object@Data, cx), type, fl)
    if (is.null(df) || !nrow(df)) return(NULL)
    df$MP     <- 'Historical'
    df$Period <- 'Historical'
    return(df)
  }

  lastHistYear <- max(Years(object, 'H'))
  mps <- names(object@PPD)

  perMP <- purrr::map(mps, function(mp) {
    mpList <- purrr::map(object@PPD[[mp]], cx)
    .IndexSeriesFromDataList(mpList, type, fl)
  })
  names(perMP) <- mps
  perMP <- purrr::compact(perMP)
  if (!length(perMP)) return(NULL)

  histDF <- perMP[[1]] |>
    dplyr::filter(.data$Year <= lastHistYear) |>
    dplyr::mutate(MP = 'Historical', Period = 'Historical')

  projDF <- purrr::imap_dfr(perMP, function(df, mp) {
    df <- dplyr::filter(df, .data$Year > lastHistYear)
    if (!nrow(df)) return(NULL)
    df$MP     <- mp
    df$Period <- 'Projection'
    df
  })

  dplyr::bind_rows(histDF, projDF)
}

.IndexSeriesFromDataList <- function(dataList, type, fl) {
  simIDs <- suppressWarnings(as.integer(names(dataList)))
  if (is.null(dataList) || !length(dataList) || anyNA(simIDs))
    simIDs <- seq_along(dataList)

  purrr::map_dfr(seq_along(dataList), function(di) {
    d <- dataList[[di]]
    if (is.null(d)) return(NULL)
    idx <- slot(d, type)
    val <- idx@Value
    if (is.null(val)) return(NULL)
    flIdx <- match(fl, idx@Name)
    if (is.na(flIdx)) return(NULL)
    data.frame(
      Sim   = simIDs[di],
      Year  = as.numeric(rownames(val)),
      Value = as.numeric(val[, flIdx])
    )
  })
}


.ExtractTrueSeries <- function(object, isMSE, type, cx, fl, Complexes) {
  stocks   <- Complexes[[cx]]
  OM       <- object@OM
  IndexObs <- slot(OM@Obs[[cx]][[fl]], type)
  Units    <- IndexObs@Units %||% 'Biomass'
  nSim_    <- nSim(object)

  fd      <- tryCatch(OM@Data[[cx]], error = function(e) NULL)
  idxData <- if (!is.null(fd)) slot(fd, type) else NULL
  fi      <- if (!is.null(idxData)) match(fl, idxData@Name) else NA_integer_

  SelectivityAtAge_Data <- if (!is.null(idxData) && !is.na(fi) && length(idxData@Selectivity) >= fi) {
    idxData@Selectivity[[fi]]
  } else {
    IndexObs@Selectivity
  }

  # Real-data Timing (fraction of the time step), when available.
  timing <- if (!is.null(idxData) && !is.na(fi) && length(idxData@Timing) >= fi) {
    idxData@Timing[fi]
  } else {
    NA_real_
  }

  HistYears  <- Years(object, 'H')
  HistNumber <- if (isMSE) object@Hist@Number[stocks] else object@Number[stocks]

  mps <- if (isMSE) dimnames(object@Number[[1]])$MP else NULL

  DecayObject <- function(mp) {
    if (!isMSE) return(object)
    tryCatch({
      obj <- object
      obj@FDeadArea[stocks] <- purrr::map(object@FDeadArea[stocks], \(arr) {
        .ArraySubsetMP(arr, mp) |> DropDimension('MP', warn = FALSE)
      })
      obj
    }, error = function(e) object)
  }

  histMat <- .CalcNomIndex(
    Number_List      = HistNumber,
    object           = DecayObject(if (isMSE) mps[1] else NULL),
    stocks           = stocks,
    fleet            = fl,
    IndexObs         = IndexObs,
    Years            = HistYears,
    SelectivityAtAge = SelectivityAtAge_Data,
    timing           = timing,
    Units            = Units
  ) |> ExtendSims(nSim = nSim_)

  histDF <- .NomIndexArrayToDF(histMat, MP = 'Historical', Period = 'Historical')

  if (!isMSE) return(histDF)

  ProjYears <- Years(object, 'P')

  projDF <- purrr::map_dfr(mps, function(mp) {
    ProjNumber <- purrr::map(object@Number[stocks], \(arr) {
      arr[,,,,mp, drop = FALSE] |>
        abind::adrop(5) |>
        .ArraySubsetYear(ProjYears)
    })

    projMat <- .CalcNomIndex(
      Number_List      = ProjNumber,
      object           = DecayObject(mp),
      stocks           = stocks,
      fleet            = fl,
      IndexObs         = IndexObs,
      Years            = ProjYears,
      SelectivityAtAge = SelectivityAtAge_Data,
      timing           = timing,
      Units            = Units
    )
    .NomIndexArrayToDF(projMat, MP = mp, Period = 'Projection')
  })

  dplyr::bind_rows(histDF, projDF)
}

.NomIndexArrayToDF <- function(arr, MP, Period) {
  df <- Array2DF(arr)
  df$MP     <- MP
  df$Period <- Period
  df
}


.ApplyObsCalibration <- function(df, Beta, Efficiency) {
  sim  <- pmin(df$Sim, length(Beta))
  simE <- pmin(df$Sim, length(Efficiency))
  df$Value <- Efficiency[simE] * df$Value ^ Beta[sim]
  df
}


.ResolveObsCalibration <- function(object, isMSE, type, cx, fl, IndexObs, nSim_) {
  Beta <- IndexObs@Beta
  if (is.null(Beta) || !length(Beta)) Beta <- 1
  Beta <- rep_len(Beta, nSim_)

  Efficiency <- IndexObs@Efficiency
  if (!is.null(Efficiency) && length(Efficiency))
    return(list(Beta = Beta, Efficiency = rep_len(Efficiency, nSim_)))

  list(Beta = Beta, Efficiency = .ExtractSimEfficiency(object, isMSE, type, cx, fl, nSim_))
}

.ExtractSimEfficiency <- function(object, isMSE, type, cx, fl, nSim_) {
  dataList <- if (isMSE) {
    mps <- names(object@PPD)
    if (!length(mps)) list() else purrr::map(object@PPD[[mps[1]]], cx)
  } else {
    purrr::map(object@Data, cx)
  }

  simIDs <- suppressWarnings(as.integer(names(dataList)))
  if (is.null(dataList) || !length(dataList) || anyNA(simIDs))
    simIDs <- seq_along(dataList)

  Efficiency <- rep(NA_real_, nSim_)
  for (di in seq_along(dataList)) {
    d <- dataList[[di]]
    if (is.null(d)) next
    idx   <- slot(d, type)
    flIdx <- match(fl, idx@Name)
    if (is.na(flIdx)) next
    IndexObsMisc <- idx@Misc$IndexObs
    if (is.null(IndexObsMisc) || length(IndexObsMisc) < flIdx) next
    eff <- IndexObsMisc[[flIdx]]@Efficiency
    if (is.null(eff) || !length(eff)) next
    sim <- simIDs[di]
    if (isTRUE(sim >= 1 && sim <= nSim_)) Efficiency[sim] <- eff[1]
  }

  missing <- is.na(Efficiency)
  if (any(missing)) {
    Efficiency[missing] <- 1
    cli::cli_alert_warning(
      "Could not resolve a fitted {.val Efficiency} for {.val {fl}} in {sum(missing)}/{nSim_} simulation{?s}; using {.val 1}."
    )
  }
  Efficiency
}

.BuildIndexFitDF <- function(object, type, Fleets = NULL, Stocks = NULL) {
  isMSE <- inherits(object, 'mse')
  OM    <- object@OM
  Complexes <- .ResolveComplexes(OM)

  stockNames <- names(Complexes)
  if (!is.null(Stocks)) {
    bad <- Stocks[!Stocks %in% stockNames]
    if (length(bad))
      cli::cli_abort(c(
        "{.val {bad}} {?is/are} not a valid stock/complex name.",
        "i" = "Available: {.val {stockNames}}."
      ))
    stockNames <- Stocks
  }

  rows <- purrr::map(stockNames, function(cx) {
    obsList <- OM@Obs[[cx]]
    if (is.null(obsList)) return(NULL)

    fleetNames <- names(obsList)
    if (!is.null(Fleets)) fleetNames <- intersect(fleetNames, Fleets)

    purrr::map(fleetNames, function(fl) {
      if (EmptyObject(slot(obsList[[fl]], type))) return(NULL)
      list(Stock = cx, Fleet = fl)
    }) |> purrr::compact()
  }) |> purrr::flatten()

  if (!length(rows)) return(NULL)

  purrr::map_dfr(rows, function(r) {
    idxDF <- .ExtractIndexSeries(object, isMSE, type, r$Stock, r$Fleet)
    
    if (is.null(idxDF)) return(NULL)
    
    idxDF$Stock <- r$Stock
    idxDF$Fleet <- r$Fleet

    trueDF <- .ExtractTrueSeries(object, isMSE, type, cx = r$Stock, fl = r$Fleet, Complexes)

    trueDF$Stock <- r$Stock

    IndexObs <- slot(OM@Obs[[r$Stock]][[r$Fleet]], type)
    calib <- .ResolveObsCalibration(object, isMSE, type, r$Stock, r$Fleet, IndexObs, nSim(object))
    trueDF <- .ApplyObsCalibration(trueDF, calib$Beta, calib$Efficiency)

    trueDF$Fleet <- r$Fleet

    dplyr::bind_rows(
      dplyr::mutate(idxDF,  Series = 'Index'),
      dplyr::mutate(trueDF, Series = 'True')
    )
  })
}

.BuildIndexFitPlot <- function(df, probs, nsim, free_y) {
  hasStock <- length(unique(df$Stock)) > 1
  hasFleet <- length(unique(df$Fleet)) > 1
  hasMP    <- 'MP' %in% colnames(df) && length(unique(df$MP[df$MP != 'Historical'])) > 1

  if (hasMP)
    df <- .ReplicateHistPerMp(df)

  if (hasFleet)
    df$Fleet <- factor(df$Fleet, levels = unique(df$Fleet), ordered = TRUE)
  
  
  group_vars <- c('Series', 'Stock', 'Fleet', 'Year', if (hasMP) 'MP')
  summ <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      Lower  = if (all(is.na(.data$Value))) NA_real_ else stats::quantile(.data$Value, min(probs), na.rm = TRUE),
      Median = if (all(is.na(.data$Value))) NA_real_ else stats::median(.data$Value, na.rm = TRUE),
      Upper  = if (all(is.na(.data$Value))) NA_real_ else stats::quantile(.data$Value, max(probs), na.rm = TRUE),
      .groups = 'drop'
    ) |>
    dplyr::filter(!is.na(.data$Median))
  summ$.group <- interaction(summ['Series'], drop = TRUE)

  p <- ggplot2::ggplot(summ, ggplot2::aes(x = .data$Year))

  if (nsim > 0) {
    simIDs <- utils::head(sort(unique(df$Sim)), nsim)
    simdf  <- dplyr::filter(df, .data$Sim %in% simIDs)
    simdf$.group <- interaction(simdf[c('Sim', 'Series')], drop = TRUE)

    p <- p + ggplot2::geom_line(
      data    = simdf,
      mapping = ggplot2::aes(y = .data$Value, group = .data$.group, color = .data$Series),
      alpha = 0.9, linewidth = 0.5, linetype = 3, na.rm = TRUE
    )
  }

  p <- p +
    ggplot2::geom_ribbon(
      data    = summ,
      mapping = ggplot2::aes(ymin = .data$Lower, ymax = .data$Upper,
                             fill = .data$Series, group = .data$.group),
      alpha = 0.2, color = NA, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data    = summ,
      mapping = ggplot2::aes(y = .data$Median, color = .data$Series,
                             linetype = .data$Series, group = .data$.group),
      linewidth = 0.7, na.rm = TRUE
    )

  seriesValues <- c(Index = 'steelblue', True = 'grey30')
  seriesLines  <- c(Index = 'solid', True = 'dashed')
  p <- p +
    ggplot2::scale_color_manual(values = seriesValues) +
    ggplot2::scale_fill_manual(values = seriesValues) +
    ggplot2::scale_linetype_manual(values = seriesLines)

  facetScales <- if (free_y) 'free_y' else 'fixed'

  if (hasMP) {
    rowVar <- if (hasStock) ggplot2::vars(.data$Stock, .data$Fleet) else ggplot2::vars(.data$Fleet)
    p <- p + ggplot2::facet_grid(rows = rowVar, cols = ggplot2::vars(.data$MP), scales = facetScales)
  } else if (hasStock && hasFleet) {
    p <- p + ggplot2::facet_grid(ggplot2::vars(.data$Stock), ggplot2::vars(.data$Fleet), scales = facetScales)
  } else if (hasStock) {
    p <- p + ggplot2::facet_wrap(~Stock, scales = facetScales)
  } else if (hasFleet) {
    p <- p + ggplot2::facet_wrap(~Fleet, scales = facetScales)
  }

  p +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05)),
                                labels = .YearLabels) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.05))) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Year', y = 'Index value (True on Index scale)',
                 color = 'Series', fill = 'Series', linetype = 'Series')
}
