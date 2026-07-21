#' Performance Metrics
#'
#' `PM_*` functions evaluate a performance metric against an [mse-class]
#' object, or a `list` of [mse-class] objects (combined via [CombineMSE()]
#' before calculation, so simulations from every list element are pooled
#' and treated as one ensemble). All `PM_*` functions operate on the
#' projection period only.
#'
#' Building-block PMs (`PM_FFMSY`, `PM_SBSBMSY`, `PM_SBSBlim`, `PM_Yield`,
#' `PM_AAVY`, `PM_AAVE`) return a [pm-class] object with `Prob`/`Mean`
#' populated whenever a reference/threshold is available (so `Prob`/`Mean`
#' always read higher-is-better); `PM_Yield`, `PM_AAVY`, and `PM_AAVE` have no
#' natural threshold on their own, so `Prob`/`Mean` are `NA` -- only `Stat` is
#' meaningful for those. Objective-level PMs (`PM_Status`, `PM_Safety`,
#' `PM_Rebuild`, `PM_RelYield` with `Ref` supplied, `PM_Stability`) always
#' populate `Prob`/`Mean`.
#'
#' PMs involving spawning biomass/production (`PM_SBSBMSY`, `PM_SBSBlim`,
#' `PM_Safety`, `PM_Status`, `PM_Rebuild`) automatically restrict to the
#' spawning ("female") stock(s) of each sex-structured complex, identified by
#' stock name (see `.FemaleStockNames()`) with a fallback to each stock's
#' `SRR@SPFrom` self-reference where the name match is ambiguous.
#'
#' Yield-based PMs (`PM_Yield`, `PM_RelYield`, `PM_AAVY`) sum landings+discards
#' over the stocks in each `OM@Complexes` group by default; pass `Stocks` to
#' override with an explicit set of stock names.
#'
#' `PM_FFMSY` and `PM_Status` are evaluated at the *complex* level
#' (`OM@Complexes`), not per stock, because `FMSY` is a single value
#' optimised jointly across a complex's member stocks (see [F_FMSY()]).
#' `PM_Status` sums `SB`/`SBMSY` across each complex's spawning stock(s)
#' before dividing, so both sides of the joint SB/F test are assessed at the
#' same aggregation level rather than pairing a per-stock SB ratio with an
#' identical, complex-wide F flag.
#'
#' @param object An [mse-class] object, or a `list` of [mse-class] objects.
#' @param Ref Numeric. Reference/threshold value.
#' @param Years Numeric vector of projection years to evaluate over. Default
#'   `NULL` uses all projection years.
#' @param Stocks Character vector of stock names to include. Default `NULL`
#'   uses the automatic complex/spawning-stock grouping described above.
#' @param silent Logical. Suppress the [CombineMSE()] summary message when
#'   `object` is a `list`. Default `TRUE`.
#'
#' @return A [pm-class] object.
#'
#' @name PM
NULL

# ---- Shared internals -------------------------------------------------------

.CoercePMInput <- function(object, silent = TRUE) {
  if (is.list(object) && !isS4(object))
    object <- CombineMSE(object, silent = silent)
  .CheckClass(object, 'mse', 'object')
  object
}

# Long-format Sim x <group_col> x MP array, averaging `valcol` over Year.
.PMArray <- function(df, valcol, group_col = 'Stock') {
  agg <- df |>
    dplyr::group_by(.data$Sim, .data[[group_col]], .data$MP) |>
    dplyr::summarise(Value = mean(.data[[valcol]], na.rm = TRUE), .groups = 'drop')

  simN   <- sort(unique(agg$Sim))
  grpN   <- unique(agg[[group_col]])
  mpN    <- unique(agg$MP)

  arr <- array(NA_real_, dim = c(length(simN), length(grpN), length(mpN)))
  dimnames(arr) <- stats::setNames(
    list(as.character(simN), as.character(grpN), as.character(mpN)),
    c('Sim', group_col, 'MP')
  )

  idx <- cbind(match(agg$Sim, simN), match(agg[[group_col]], grpN), match(agg$MP, mpN))
  arr[idx] <- agg$Value
  arr
}

.PMMean <- function(ProbArr) {
  d  <- dim(ProbArr)
  dn <- dimnames(ProbArr)
  out <- apply(ProbArr, c(2, 3), mean, na.rm = TRUE)
  dim(out) <- d[c(2, 3)]
  dimnames(out) <- dn[c(2, 3)]
  out
}

# Build a `pm` object from a long-format data.frame with Sim/<group>/Year/MP/Value(/Period).
# `op` compares Value against Ref (must return TRUE = good outcome); NULL means
# the PM has no natural threshold, so Prob/Mean are left NA.
.BuildPM <- function(df, Ref, Years, op, Name, Caption, group_col = 'Stock') {
  if ('Period' %in% names(df))
    df <- df[df$Period == 'Projection', ]
  if (!is.null(Years))
    df <- df[df$Year %in% Years, ]
  YearsOut <- if ('Year' %in% names(df)) sort(unique(df$Year)) else numeric(0)

  StatArr <- .PMArray(df, 'Value', group_col)

  if (is.null(op)) {
    ProbArr <- StatArr
    ProbArr[] <- NA_real_
  } else {
    df$.Met <- op(df$Value, Ref)
    ProbArr <- .PMArray(df, '.Met', group_col)
  }
  MeanArr <- .PMMean(ProbArr)

  methods::new('pm',
               Name    = Name,
               Caption = Caption,
               Stat    = StatArr,
               Ref     = if (is.null(Ref)) NA_real_ else Ref,
               Prob    = ProbArr,
               Mean    = MeanArr,
               MPs     = sort(unique(df$MP)),
               Years   = YearsOut)
}

.ResolveRefByStock <- function(Ref, stockcol) {
  if (length(Ref) == 1)
    return(rep(unname(Ref), length(stockcol)))
  if (!is.null(names(Ref))) {
    out <- Ref[stockcol]
    if (anyNA(out))
      cli::cli_abort("`Ref` is a named vector but does not cover stock(s): {.val {unique(stockcol[is.na(out)])}}.")
    return(unname(out))
  }
  cli::cli_abort("`Ref` must be length 1 or a named numeric vector keyed by stock name.")
}

# Identify the spawning ("female") stock in each sex-structured complex.
# Primary signal: stock name containing "female" (matches the convention
# already used by PlotSBiomass()/PlotSProduction() -- see .FemaleStockNames()
# in plot-hist.R). Falls back to SRR@SPFrom self-reference when a complex has
# more than one stock and the name match is ambiguous.
.SpawningStockNames <- function(OM) {
  stockNms <- StockNames(OM)
  if (length(stockNms) <= 1)
    return(stockNms)

  fem <- .FemaleStockNames(OM)
  if (!fem$ambiguous)
    return(fem$stocks)

  complexes <- OM@Complexes
  keep <- character(0)
  for (cx in complexes) {
    nms <- stockNms[cx]
    if (length(nms) == 1) {
      keep <- c(keep, nms)
      next
    }
    resolved <- nms[nms %in% fem$stocks]
    if (length(resolved) == 1) {
      keep <- c(keep, resolved)
      next
    }
    self_ref <- purrr::keep(cx, \(i) {
      spf <- OM@Stock[[i]]@SRR@SPFrom
      length(spf) == 0 ||
        (is.character(spf) && identical(spf, stockNms[i])) ||
        (is.numeric(spf)   && spf == i)
    })
    if (length(self_ref) == 1) {
      keep <- c(keep, stockNms[self_ref])
    } else {
      keep <- c(keep, nms)
    }
  }
  keep
}

.ResolveComplexGroups <- function(OM, Stocks) {
  stockNms <- StockNames(OM)
  if (!is.null(Stocks))
    return(list(All = Stocks))

  complexes <- OM@Complexes
  if (!length(complexes))
    return(stats::setNames(as.list(stockNms), stockNms))

  grp <- purrr::imap(complexes, \(idx, nm) stockNms[idx])
  leftover <- setdiff(stockNms, unlist(grp))
  if (length(leftover))
    grp <- c(grp, stats::setNames(as.list(leftover), leftover))
  grp
}

# Subset an array's `Stock` dimension to `keep` (by name), preserving all
# other dimensions.
.FilterStockDim <- function(arr, keep) {
  dn        <- dimnames(arr)
  stock_pos <- match('Stock', names(dn))
  idx_list              <- rep(list(quote(expr = )), length(dn))
  idx_list[[stock_pos]] <- which(dn[[stock_pos]] %in% keep)
  do.call('[', c(list(arr), idx_list, list(drop = FALSE)))
}

.GroupedRemovals <- function(object, Stocks, ManagementOnly = FALSE) {
  df <- Removals(object, df = TRUE, byFleet = FALSE, byAge = FALSE,
                bySize = FALSE, byArea = FALSE, Reduce = FALSE)
  df <- df[df$Period == 'Projection', ]

  if (ManagementOnly) {
    ManageYrs <- .CalcManagementYears(Years(object@OM, 'Projection'), object@OM@Interval)
    df <- df[df$Year %in% ManageYrs, ]
  }

  groups <- .ResolveComplexGroups(object@OM, Stocks)
  purrr::imap(groups, \(stk, grpName) {
    df[df$Stock %in% stk, ] |>
      dplyr::group_by(.data$Sim, .data$Year, .data$MP) |>
      dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = grpName)
  }) |> dplyr::bind_rows()
}

.GroupedEffort <- function(object, Fleets, ManagementOnly = FALSE) {
  df <- Effort(object, df = TRUE)
  df <- df[df$Period == 'Projection', ]

  if (ManagementOnly) {
    ManageYrs <- .CalcManagementYears(Years(object@OM, 'Projection'), object@OM@Interval)
    df <- df[df$Year %in% ManageYrs, ]
  }
  if (!is.null(Fleets))
    df <- df[df$Fleet %in% Fleets, ]

  df |>
    dplyr::group_by(.data$Sim, .data$Year, .data$MP) |>
    dplyr::summarise(Value = sum(.data$Value, na.rm = TRUE), .groups = 'drop') |>
    dplyr::mutate(Stock = 'All')
}

# Average annual variability: |Value[t] - Value[t-1]| / Value[t-1], computed
# on whatever set of years/timesteps is already in `df` (e.g. management
# intervals only, via .GroupedRemovals(..., ManagementOnly = TRUE)).
.AAV <- function(df, group_col = 'Stock') {
  df |>
    dplyr::arrange(.data$Sim, .data[[group_col]], .data$MP, .data$Year) |>
    dplyr::group_by(.data$Sim, .data[[group_col]], .data$MP) |>
    dplyr::mutate(Prev = dplyr::lag(.data$Value)) |>
    dplyr::filter(!is.na(.data$Prev)) |>
    dplyr::mutate(Value = abs(.data$Value - .data$Prev) / .data$Prev) |>
    dplyr::ungroup() |>
    dplyr::select('Sim', dplyr::all_of(group_col), 'Year', 'MP', 'Value')
}

# ---- Status: F/FMSY, SB/SBMSY, joint status --------------------------------

#' @rdname PM
#' @export
PM_FFMSY <- function(object, Ref = 1, Years = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- F_FMSY(object, df = TRUE, Reduce = FALSE)
  .BuildPM(df, Ref = Ref, Years = Years, op = `<`,
           Name = 'F_FMSY', Caption = paste0('P(F < ', Ref, ' FMSY)'))
}

#' @rdname PM
#' @export
PM_SBSBMSY <- function(object, Ref = 1, Years = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- SB_SBMSY(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Stock %in% .SpawningStockNames(object@OM), ]
  .BuildPM(df, Ref = Ref, Years = Years, op = `>`,
           Name = 'SB_SBMSY', Caption = paste0('P(SB > ', Ref, ' SBMSY)'))
}

#' @rdname PM
#' @export
PM_Status <- function(object, Years = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  spawn_stocks <- .SpawningStockNames(object@OM)

  # Both sides evaluated at the complex level, since FMSY is only ever
  # defined per complex (see F_FMSY()).
  sb_arr    <- .FilterStockDim(object@SBiomass, spawn_stocks)
  sbmsy_arr <- .FilterStockDim(SBMSY(object), spawn_stocks)

  sb_complex    <- .AggregateStockToComplex(sb_arr, object@OM, sum, strict = FALSE)
  sbmsy_complex <- .AggregateStockToComplex(sbmsy_arr, object@OM, sum, strict = FALSE)

  target_years  <- dimnames(sb_complex)[['Year']]
  sbmsy_aligned <- .AlignDenomYears(sbmsy_complex, target_years) |>
    AddDimension('MP', val = dimnames(sb_complex)[['MP']])

  sb_df <- ArrayDivide(sb_complex, sbmsy_aligned) |>
    Array2DF() |>
    dplyr::rename(Complex = 'Stock', SB = 'Value')

  ff <- F_FMSY(object, df = TRUE, Reduce = FALSE)
  ff <- ff[ff$Period == 'Projection', ] |>
    dplyr::rename(Complex = 'Stock') |>
    dplyr::select('Sim', 'Complex', 'Year', 'MP', F = 'Value')

  if (!is.null(Years)) {
    sb_df <- sb_df[sb_df$Year %in% Years, ]
    ff    <- ff[ff$Year %in% Years, ]
  }

  joined <- dplyr::inner_join(sb_df, ff, by = c('Sim', 'Complex', 'Year', 'MP'))
  joined$Value <- as.numeric(joined$SB > 1 & joined$F < 1)
  joined <- joined |> dplyr::rename(Stock = 'Complex')

  .BuildPM(joined, Ref = 1, Years = NULL, op = \(x, r) x >= r,
           Name = 'Status', Caption = 'P(SB > SBMSY & F < FMSY), complex-level')
}

# ---- Safety: limit reference points ----------------------------------------

#' @rdname PM
#' @export
PM_SBSBlim <- function(object, Blim, Years = NULL, silent = TRUE) {
  if (missing(Blim) || is.null(Blim))
    cli::cli_abort("`Blim` must be supplied -- there is no package default limit reference point.")
  object <- .CoercePMInput(object, silent)

  df <- SBiomass(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Stock %in% .SpawningStockNames(object@OM), ]
  df$Value <- df$Value / .ResolveRefByStock(Blim, df$Stock)

  .BuildPM(df, Ref = 1, Years = Years, op = `>`,
           Name = 'SB_SBlim', Caption = 'P(SB > SBlim)')
}

#' @rdname PM
#' @export
PM_Safety <- function(object, Blim, Years = NULL, silent = TRUE) {
  if (missing(Blim) || is.null(Blim))
    cli::cli_abort("`Blim` must be supplied -- there is no package default limit reference point.")
  object <- .CoercePMInput(object, silent)

  df <- SBiomass(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Period == 'Projection' & df$Stock %in% .SpawningStockNames(object@OM), ]
  if (!is.null(Years))
    df <- df[df$Year %in% Years, ]
  YearsOut <- sort(unique(df$Year))

  df$Ref   <- .ResolveRefByStock(Blim, df$Stock)
  df$Above <- df$Value > df$Ref

  StatArr <- .PMArray(df, 'Value')
  safe_df <- df |>
    dplyr::group_by(.data$Sim, .data$Stock, .data$MP) |>
    dplyr::summarise(Met = as.numeric(all(.data$Above)), .groups = 'drop')
  ProbArr <- .PMArray(safe_df, 'Met')
  MeanArr <- .PMMean(ProbArr)

  methods::new('pm',
               Name = 'Safety', Caption = 'P(SB never drops below SBlim during the projection)',
               Stat = StatArr, Ref = NA_real_, Prob = ProbArr, Mean = MeanArr,
               MPs = sort(unique(df$MP)), Years = YearsOut)
}

#' @rdname PM
#' @export
PM_Rebuild <- function(object, Year, Target = 1, silent = TRUE) {
  if (missing(Year))
    cli::cli_abort("`Year` (the rebuilding target year) must be supplied.")
  object <- .CoercePMInput(object, silent)

  sb <- SB_SBMSY(object, df = TRUE, Reduce = FALSE)
  sb <- sb[sb$Stock %in% .SpawningStockNames(object@OM), ]

  lastHistYr <- max(Years(object@OM, 'Historical'))
  baseline <- sb[sb$Period == 'Historical' & sb$Year == lastHistYr, ] |>
    dplyr::group_by(.data$Stock) |>
    dplyr::summarise(Overfished = mean(.data$Value) < 1, .groups = 'drop')
  overfished_stocks <- baseline$Stock[baseline$Overfished]

  if (!length(overfished_stocks))
    cli::cli_alert_info("No stocks are overfished (SB < SBMSY) at the end of the historical period.")

  target_df <- sb[sb$Period == 'Projection' & sb$Year == Year &
                    sb$Stock %in% overfished_stocks, ]
  target_df$Value <- target_df$Value / Target

  .BuildPM(target_df, Ref = 1, Years = Year, op = `>`,
           Name = 'Rebuild', Caption = paste0('P(SB > ', Target, ' SBMSY by ', Year, ')'))
}

# ---- Yield ------------------------------------------------------------------

#' @rdname PM
#' @export
PM_Yield <- function(object, Years = NULL, Stocks = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- .GroupedRemovals(object, Stocks)
  .BuildPM(df, Ref = NA_real_, Years = Years, op = NULL,
           Name = 'Yield', Caption = 'Mean projected yield')
}

#' @rdname PM
#' @export
PM_RelYield <- function(object, Ref = NULL, Years = NULL, Stocks = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  df <- .GroupedRemovals(object, Stocks)

  msy_df <- Array2DF(MSYLandings(object)) |>
    dplyr::group_by(.data$Sim, .data$Stock) |>
    dplyr::summarise(MSY = mean(.data$Value, na.rm = TRUE), .groups = 'drop')

  groups <- .ResolveComplexGroups(object@OM, Stocks)
  grp_msy <- purrr::imap(groups, \(stk, nm) {
    msy_df[msy_df$Stock %in% stk, ] |>
      dplyr::group_by(.data$Sim) |>
      dplyr::summarise(MSY = sum(.data$MSY, na.rm = TRUE), .groups = 'drop') |>
      dplyr::mutate(Stock = nm)
  }) |> dplyr::bind_rows()

  df <- dplyr::left_join(df, grp_msy, by = c('Sim', 'Stock'))
  df$Value <- df$Value / df$MSY

  op <- if (is.null(Ref)) NULL else `>`
  .BuildPM(df, Ref = Ref, Years = Years, op = op,
           Name = 'RelYield', Caption = 'Yield relative to MSY yield')
}

# ---- Stability ----------------------------------------------------------------

#' @rdname PM
#' @export
PM_AAVY <- function(object, Years = NULL, Stocks = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  ydf <- .GroupedRemovals(object, Stocks, ManagementOnly = TRUE)
  if (!is.null(Years))
    ydf <- ydf[ydf$Year %in% Years, ]
  aav <- .AAV(ydf)
  .BuildPM(aav, Ref = NA_real_, Years = NULL, op = NULL,
           Name = 'AAVY', Caption = 'Average annual variability in yield across management intervals')
}

#' @rdname PM
#' @export
PM_AAVE <- function(object, Years = NULL, Fleets = NULL, silent = TRUE) {
  object <- .CoercePMInput(object, silent)
  edf <- .GroupedEffort(object, Fleets, ManagementOnly = TRUE)
  if (!is.null(Years))
    edf <- edf[edf$Year %in% Years, ]
  aav <- .AAV(edf)
  .BuildPM(aav, Ref = NA_real_, Years = NULL, op = NULL,
           Name = 'AAVE', Caption = 'Average annual variability in effort across management intervals')
}

#' @rdname PM
#' @export
PM_Stability <- function(object, Threshold, Years = NULL, Stocks = NULL, silent = TRUE) {
  if (missing(Threshold))
    cli::cli_abort("`Threshold` (maximum acceptable AAVY) must be supplied.")
  object <- .CoercePMInput(object, silent)
  ydf <- .GroupedRemovals(object, Stocks, ManagementOnly = TRUE)
  if (!is.null(Years))
    ydf <- ydf[ydf$Year %in% Years, ]
  aav <- .AAV(ydf)
  .BuildPM(aav, Ref = Threshold, Years = NULL, op = `<`,
           Name = 'Stability', Caption = paste0('P(AAVY < ', Threshold, ')'))
}
