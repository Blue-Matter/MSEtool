
#' Convert an `mse` Object to a Slick Object
#'
#' Converts an [mse-class] object (or a list of [mse-class] objects) into a
#' [Slick::Slick()] data object for interactive visualization in the
#' [Slick App](https://shiny.bluematterscience.com/app/slick).
#'
#' A [Slick::Slick()] object is a standardized data structure containing MSE
#' results, MP metadata, and performance indicators. It can be passed directly
#' to [Slick::App()] for interactive exploration, or saved and uploaded to the
#' online Slick App.
#'
#' When `MSE` is a list, each element should represent a different Operating
#' Model (OM) tested with the same set of MPs. 
#' 
#' All [mse-class] objects in the list must have identical MPs, number of 
#' simulations, number of time steps, and stock complexes.
#'
#' The resulting [Slick::Slick()] object includes:
#' - **MPs**: management procedure codes, labels, and display colours.
#' - **OMs**: one factor level per list element of `MSE` (see `Design`).
#' - **Kobe**: annual SB/SBMSY and F/FMSY in the years the MPs are active
#'   (`KobeYears`). Kobe is single-stock so for a multi-complex OM,
#'   `KobeComplex` selects which one to show (default: the first).
#' - **Timeseries**: annual time series for the historical and projection
#'   periods (see `TimeseriesCode`).
#' - **Boxplot/Quilt/Spider/Tradeoff**: driven by `PMs`, a flexible list of
#'   performance-metric specs (see `PMs` below). Each is evaluated
#'   against every complex in every OM.
#'
#' `PMs` (and the per-panel `BoxplotPMs`/`QuiltPMs`/`SpiderPMs`/
#' `TradeoffPMs` overrides) is a list of performance-metric specifications.
#' Each element is one of:
#' - a `PM_*` function, e.g. `PM_Status` for [PM_Status()];
#' - a list `list(PM, Args = list(), Code = NULL, Label = NULL, Description = NULL)`
#'   (`fun` and `args` are accepted for `PM` and `Args`), e.g.
#'   `list(PM = PM_Yield, Args = list(Years = 2030:2039), Code = 'Yield_Short')`;
#' - a call, e.g. `quote(PM_Status(Years = 2030:2039))`, evaluated in the
#'   environment `MSE2Slick()` is called from.
#'
#' `Code`, `Label`, and `Description` default to the `Name` and `Caption` of
#' the [pm-class] object. The codes must be unique, so give a `Code` when the
#' same PM function is used more than once (e.g. with different `Years`; see
#' [PMYears()]). Each per-panel override falls back to `PMs` when `NULL`; a
#' spec list is only evaluated once even when shared across panels.
#'
#' A PM is a probability if its `Prob` is not all `NA` (e.g. [PM_Status()],
#' [PM_Safety()], [PM_FFMSY()] with a `Ref`), and a statistic otherwise (e.g.
#' [PM_Yield()], [PM_MinStatus()], [PM_AAVY()]). The `Boxplot` and `Quilt`
#' show the per-simulation `Prob` of a probability and the per-simulation
#' `Stat` of a statistic, and the `Tradeoff` and `Spider` the `Mean` over
#' simulations. The `Quilt` colour scale of a probability spans 0 to 1. In
#' the `Spider`, which needs values from 0 to 1, a statistic is divided by its
#' largest absolute value among the MPs in each OM.
#'
#' `TimeseriesCode` selects the variables of the `Timeseries` panel, one value
#' per calendar year: `'SB_SBMSY'`, `'F_FMSY'` (see [SB_SBMSY()],
#' [F_FMSY()]), `'SB_SB0'` (spawning biomass relative to equilibrium unfished
#' spawning biomass), `'SBiomass'`, `'Landings'`, `'Removals'`, and `'TAC'`
#' (the TAC in effect, projection years only). Other entries must name
#' functions that accept an [mse-class] object and return a tidy data frame
#' with columns `Sim`, `Year`, `MP`, `Period`, and `Value`. In seasonal OMs,
#' catches and F are summed over the seasons of each calendar year and the TAC
#' is the mean over the seasons; other variables are taken in the reference
#' season of each year (`SeasonalBasis = 'RefSeason'`, the season of the MSY
#' reference points; see [RefSeason()]) or are the mean over the seasons
#' (`SeasonalBasis = 'Mean'`).
#'
#' `MPCode`/`MPLabel`/`MPDescription` set the resulting `Slick` object's
#' `MPs@Code`/`Label`/`Description`. Each is `NULL` (default `MPCode`/`MPLabel` 
#' fall back to `names(MSE@MPs)`, `MPDescription` is left unset).
#'
#' `Design` gives each OM's level along one or more factorial-design factors
#' (e.g. `M`, steepness), as a `data.frame` (one row per OM, one column per
#' factor) or an equivalent named list of length-`nOM` vectors. The
#' per-factor level catalog Slick needs (`OMs@Factors`) is derived
#' automatically from the unique values in each column; `NULL` (default)
#' uses a single `Scenario` factor built from the `MSE` list names (or
#' `"OM1"`, `"OM2"`, ... if unnamed). 
#' 
#' When `Design` is supplied and `MSE`
#' has more than one element, `MSE` must be a *named* list.
#'
#' `AutoPreset` (default `TRUE`) auto-populates `OMs@Preset` with one button
#' per unique level of each `Design` column, unless `OMsPreset` overrides it
#' entirely; ignored when `Design` is `NULL`. `MPsPreset`/`BoxplotPreset`/
#' `QuiltPreset`/`SpiderPreset`/`TradeoffPreset` are pass-throughs (a
#' named list of MP or PI indices into that object's own `Code`; see
#' [Slick::Preset()]).
#'
#' @param MSE An [mse-class] object or a named list of [mse-class] objects,
#'   where each element represents a different OM. All objects in the list
#'   must have identical MPs, number of simulations, number of time steps,
#'   and stock complexes.
#' @param Title Character, or `NULL` (default). `NULL` uses
#'   `MSE@OM@Name` (or the first list element's, when `MSE` is a list).
#' @param Subtitle,Author,Email,Institution,Introduction Character. Optional
#'   metadata, all default `""`.
#' @param Date `Date`, or `NULL` (default) for `Sys.Date()`.
#' @param PMs A list of performance-metric specs, shared by default across
#'   `Boxplot`/`Quilt`/`Spider`/`Tradeoff`. `NULL` (default) uses
#'   [DefaultSlickPMs()]. See Details.
#' @param BoxplotPMs,QuiltPMs,SpiderPMs,TradeoffPMs Per-panel overrides for
#'   `PMs`, or `NULL` (default) to fall back to it. See Details.
#' @param TimeseriesCode Character. The variables of the `Timeseries` panel,
#'   or `NULL` (default) for `c('SB_SBMSY', 'F_FMSY', 'Landings')`. See
#'   Details.
#' @param TimeseriesLabel,TimeseriesDescription Character, one per
#'   `TimeseriesCode`, or `NULL` (default) for the codes and built-in
#'   descriptions.
#' @param TimeseriesTarget,TimeseriesLimit Numeric vectors named by
#'   `TimeseriesCode`, or `NULL` (default). The target and limit lines of the
#'   `Timeseries` panel. The defaults are a target of `1` for `SB_SBMSY` and
#'   `F_FMSY`, and limits of `0.4` for `SB_SBMSY` and `1` for `F_FMSY`;
#'   entries given here replace them.
#' @param SeasonalBasis Character. For seasonal OMs, the annual value of the
#'   `Timeseries` variables that are not summed over the seasons:
#'   `'RefSeason'` (default) or `'Mean'`. See Details.
#' @param KobeComplex Character, or `NULL` (default: the first complex).
#'   Which stock/complex the Kobe plot shows, when `MSE` has more than one.
#' @param KobeYears Numeric, or `NULL` (default: the years the MPs are
#'   active, [PMYears()]). The calendar years of the Kobe plot.
#' @param KobeLimit Numeric, length 2. The limits of SB/SBMSY and F/FMSY in
#'   the Kobe plot. Default `c(0.4, 1)`.
#' @param MPCode,MPLabel,MPDescription Overrides for the per-MP display
#'   text, or `NULL` (default). See Details.
#' @param Design The OM factorial design, or `NULL` (default) for a single
#'   `Scenario` factor built from the `MSE` list names. See Details.
#' @param AutoPreset Logical. Auto-populate `OMs@Preset` from `Design`?
#'   Default `TRUE`. See Details.
#' @param OMsPreset,MPsPreset,BoxplotPreset,QuiltPreset,SpiderPreset,TradeoffPreset
#'   Preset buttons for the App, or `NULL` (default) for none (except `OMs`,
#'   which still gets `AutoPreset`'s buttons). See Details.
#'
#' @return A [Slick::Slick()] object ready for use with [Slick::App()].
#'
#' @examples
#' \dontrun{
#' Slick <- MSE2Slick(MSE)
#' Slick::App(slick=Slick)
#'
#' # Multiple OMs, custom PMs
#' Slick <- MSE2Slick(
#'   list(Base = MSE_Base, LowM = MSE_LowM, HighM = MSE_HighM),
#'   Design = list(M = c('Base', 'Low', 'High')),
#'   PMs = list(
#'     PM_Status, PM_Safety,
#'     list(fun = PM_Yield, args = list(Years = 2000:2009), Code = 'Yield_early'),
#'     list(fun = PM_Yield, args = list(Years = 2010:2019), Code = 'Yield_late')
#'   )
#' )
#' Slick::App(slick=Slick)
#'
#' # The same PM over windows of years, probabilities and statistics
#' Slick <- MSE2Slick(
#'   MSE,
#'   PMs = list(
#'     list(PM = PM_Status, Code = 'PGK', Label = 'P(Kobe green)'),
#'     list(PM = PM_Status, Args = list(Years = PMYears(MSE, 'first', 10)),
#'          Code = 'PGK_Short', Label = 'P(Kobe green) (years 1-10)'),
#'     list(PM = PM_Red, Code = 'PRed', Label = 'P(Kobe red)'),
#'     list(PM = PM_MinStatus, Code = 'SB_SBMSY_Min', Label = 'Minimum SB/SBMSY'),
#'     list(PM = PM_FFMSY, Args = list(Ref = 1.4), Code = 'PLim_F',
#'          Label = 'P(F < 1.4 FMSY)'),
#'     list(PM = PM_Yield, Code = 'Yield', Label = 'Mean catch'),
#'     list(PM = PM_AAVY, Args = list(IncludeFirst = TRUE), Code = 'AAV_TAC',
#'          Label = 'Mean TAC change'),
#'     list(PM = PM_TACLimited, Code = 'P_TACLimited', Label = 'P(TAC change limited)')
#'   ),
#'   TimeseriesCode = c('SB_SBMSY', 'F_FMSY', 'SB_SB0', 'Removals', 'TAC'),
#'   TimeseriesLimit = c(SB_SBMSY = 0.4, F_FMSY = 1.4),
#'   KobeLimit = c(0.4, 1.4)
#' )
#' }
#'
#' @seealso [Slick::Slick()], [Slick::App()], [mse-class], [DefaultSlickPMs()],
#'   [PM], [PMYears()]
#' @export
MSE2Slick <- function(MSE,
                      Title        = NULL,
                      Subtitle     = "",
                      Author       = "",
                      Email        = "",
                      Institution  = "",
                      Introduction = "",
                      Date         = NULL,
                      PMs          = NULL,
                      BoxplotPMs   = NULL,
                      QuiltPMs     = NULL,
                      SpiderPMs    = NULL,
                      TradeoffPMs  = NULL,
                      TimeseriesCode  = NULL,
                      TimeseriesLabel = NULL,
                      TimeseriesDescription = NULL,
                      TimeseriesTarget = NULL,
                      TimeseriesLimit  = NULL,
                      SeasonalBasis    = c('RefSeason', 'Mean'),
                      KobeComplex  = NULL,
                      KobeYears    = NULL,
                      KobeLimit    = c(0.4, 1),
                      MPCode        = NULL,
                      MPLabel       = NULL,
                      MPDescription = NULL,
                      Design       = NULL,
                      AutoPreset      = TRUE,
                      OMsPreset       = NULL,
                      MPsPreset       = NULL,
                      BoxplotPreset   = NULL,
                      QuiltPreset     = NULL,
                      SpiderPreset    = NULL,
                      TradeoffPreset  = NULL) {
  .SlickChecks(MSE)
  SeasonalBasis <- match.arg(SeasonalBasis)
  env <- parent.frame()

  mse_ref  <- if (is.list(MSE)) MSE[[1]] else MSE
  PMs      <- PMs %||NA% DefaultSlickPMs()

  BoxplotPMs  <- BoxplotPMs  %||NA% PMs
  QuiltPMs    <- QuiltPMs    %||NA% PMs
  SpiderPMs   <- SpiderPMs   %||NA% PMs
  TradeoffPMs <- TradeoffPMs %||NA% PMs

  panelCache <- new.env(parent = emptyenv())
  .panel <- function(pms) {
    key <- rlang::hash(pms)
    if (is.null(panelCache[[key]])) panelCache[[key]] <- .EvalPMPanel(MSE, pms, env)
    panelCache[[key]]
  }

  Slick             <- Slick::Slick()
  Slick@Title       <- Title %||NA% mse_ref@OM@Name
  Slick@Subtitle    <- Subtitle
  Slick@Author      <- Author
  Slick@Email       <- Email
  Slick@Institution <- Institution
  Slick@Introduction <- Introduction
  Slick@Date        <- Date %||NA% Sys.Date()
  Slick@MPs         <- .MSE2MPs(mse_ref, MPCode, MPLabel, MPDescription) |> .ApplyPreset(MPsPreset)
  Slick@OMs         <- .MSE2OMs(MSE, Design, AutoPreset) |> .ApplyPreset(OMsPreset)
  Slick@Kobe        <- .MSE2Kobe(MSE, KobeComplex, KobeYears, KobeLimit)
  Slick@Timeseries  <- .MSE2Timeseries(MSE, TimeseriesCode, TimeseriesLabel,
                                       TimeseriesDescription, TimeseriesTarget,
                                       TimeseriesLimit, SeasonalBasis)
  Slick@Boxplot     <- .MSE2Boxplot(.panel(BoxplotPMs))     |> .ApplyPreset(BoxplotPreset)
  Slick@Quilt       <- .MSE2Quilt(.panel(QuiltPMs))         |> .ApplyPreset(QuiltPreset)
  Slick@Spider      <- .MSE2Spider(.panel(SpiderPMs))       |> .ApplyPreset(SpiderPreset)
  Slick@Tradeoff    <- .MSE2Tradeoff(.panel(TradeoffPMs))   |> .ApplyPreset(TradeoffPreset)
  Slick
}


.SlickChecks <- function(MSE) {
  MPs <- NULL # CRAN checks
  .CheckClass(MSE, c('mse', 'list'), 'MSE')
  
  if (!requireNamespace("Slick", quietly=TRUE))
    cli::cli_abort(c(
      "The {.pkg Slick} package is required.",
      "i" = "Install from CRAN: {.code install.packages('Slick')}",
      "i" = "Or from GitHub: {.code pak::pkg_install('blue-matter/Slick')}"
    ))
  
  if (inherits(MSE, 'list')) {
    purrr::walk(MSE, .SlickChecks)
    
    if (length(unique(nSim(MSE))) != 1)
      cli::cli_abort(
        "All MSE objects must have the same number of simulations. \\
         Use {.run nSim(MSE)} to check."
      )
    if (length(unique(MPs(MSE))) != 1)
      cli::cli_abort(
        "All MSE objects must have the same MPs. \\
         Use {.run MPs(MSE)} to check."
      )
    if (length(unique(Years(MSE))) != 1)
      cli::cli_abort(
        "All MSE objects must have the same time steps. \\
         Use {.run Years(MSE)} to check."
      )
    complex_sets <- purrr::map(MSE, \(mse) sort(names(Complexes(mse@OM)) %||NA% StockNames(mse@OM)))
    if (length(unique(complex_sets)) != 1)
      cli::cli_abort(
        "All MSE objects must have the same stock complexes. \\
         Use {.run names(Complexes(MSE[[1]]@OM))} to check."
      )
  }
  invisible(NULL)
}

.ApplyPreset <- function(obj, preset) {
  if (!is.null(preset)) Slick::Preset(obj) <- preset
  obj
}

#' Default Performance Metrics for `MSE2Slick()`
#'
#' The default `PMs` list used by [MSE2Slick()] (and, unless overridden, by
#' every one of its `BoxplotPMs`/`QuiltPMs`/`SpiderPMs`/`TradeoffPMs`
#' panel-specific arguments) when none is supplied: four metrics, chosen to
#' give a reasonably complete first look at a set of MPs without the caller
#' having to assemble a `PMs` list themselves.
#'
#' - [PM_Yield()]: mean projected catch, on its natural (biomass) scale; in
#'   the `Spider` it is relative to the highest value among the MPs.
#' - [PM_AAVY()]: average annual variability in the TAC, i.e. how much the
#'   TAC changes between management cycles; also natural-scale.
#' - [PM_Status()]: the joint probability that a stock/complex is neither
#'   overfished nor experiencing overfishing (`P(SB > SBMSY & F < FMSY)`).
#' - [PM_Safety()] with `Lim = 0.4`: the probability that `SB/SBMSY` never
#'   drops below 40% at any point in the projection.
#'
#' Pass your own list to `MSE2Slick(PMs = ...)` to override this entirely,
#' or start from `DefaultSlickPMs()` and append/replace entries (see 
#' [MSE2Slick()]'s Details section).
#'
#' @return A list of performance-metrics, in the shape [MSE2Slick()]'s
#'   `PMs` argument expects.
#' @seealso [MSE2Slick()], [PM]
#' @export
DefaultSlickPMs <- function() {
  list(
    PM_Yield,
    PM_AAVY,
    PM_Status,
    list(fun = PM_Safety, args = list(Lim = 0.4), Code = 'Safety',
        Label = 'Safety (SB > 40% SBMSY)')
  )
}


.MSE2MPs <- function(MSE, MPCode = NULL, MPLabel = NULL, MPDescription = NULL) {
  .SlickChecks(MSE)

  mpNames    <- names(MSE@MPs)
  MPs        <- Slick::MPs()
  MPs@Code   <- .ResolveMPOverride(MPCode,  mpNames, 'MPCode')
  MPs@Label  <- .ResolveMPOverride(MPLabel, mpNames, 'MPLabel')
  if (!is.null(MPDescription))
    MPs@Description <- .ResolveMPOverride(MPDescription, mpNames, 'MPDescription')
  MPs@Color  <- Slick::default_mp_colors(length(MPs@Code))
  MPs
}


.ResolveMPOverride <- function(override, mpNames, argName) {
  if (is.null(override)) return(mpNames)

  if (is.function(override)) return(unname(override(mpNames)))

  if (!is.null(names(override))) {
    idx <- match(mpNames, names(override))
    if (anyNA(idx))
      cli::cli_abort(c(
        "x" = "{.arg {argName}} is missing entries for: {.val {mpNames[is.na(idx)]}}.",
        "i" = "A named {.arg {argName}} must cover every MP in {.code names(MSE@MPs)}."
      ))
    return(unname(override[idx]))
  }

  if (length(override) != length(mpNames))
    cli::cli_abort(
      "{.arg {argName}} must have length {.val {length(mpNames)}} (one per MP) or be named."
    )
  unname(override)
}


.MSE2OMs <- function(MSE, Design = NULL, AutoPreset = TRUE) {
  nOM      <- if (is.list(MSE)) length(MSE) else 1L
  isList   <- is.list(MSE)
  mseNames <- if (isList) names(MSE) else NULL
  mseNamed <- isList && !is.null(mseNames) && all(nzchar(mseNames))

  if (!is.null(Design) && isList && nOM > 1 && !mseNamed)
    cli::cli_abort(c(
      "x" = "{.arg MSE} must be a named list (one name per OM) when {.arg Design} is supplied.",
      "i" = "This keeps {.arg Design}'s rows unambiguously matched to the right `mse` object -- \\
             e.g. {.code list(Base = MSE_Base, LowM = MSE_LowM)}, not an unnamed list."
    ))

  omNames <- if (isList) {
    if (mseNamed) mseNames else paste0('OM', seq_len(nOM))
  } else {
    "OM1"
  }

  autoDesign <- is.null(Design)
  if (autoDesign)
    Design <- list(Scenario = omNames)

  if (is.data.frame(Design)) {
    if (nrow(Design) != nOM)
      cli::cli_abort(c(
        "x" = "{.arg Design} must have {.val {nOM}} rows (one per OM).",
        "i" = "It has {.val {nrow(Design)}}."
      ))
    has_custom_rownames <- !identical(rownames(Design), as.character(seq_len(nrow(Design))))
    if (has_custom_rownames) {
      if (!setequal(rownames(Design), omNames))
        cli::cli_abort(c(
          "x" = "{.arg Design}'s row names don't match {.code names(MSE)}.",
          "i" = "{.arg Design} row names: {.val {rownames(Design)}}",
          "i" = "{.code names(MSE)}: {.val {omNames}}"
        ))
      Design <- Design[omNames, , drop = FALSE]  
    }
  } else {
    bad <- lengths(Design) != nOM
    if (any(bad))
      cli::cli_abort(c(
        "x" = "Each element of {.arg Design} must have length {.val {nOM}} (one per OM).",
        "i" = "{.val {names(Design)[bad]}} {?does/do} not."
      ))
  }

  design <- as.data.frame(lapply(Design, as.character), stringsAsFactors = FALSE)
  rownames(design) <- omNames

  OMs         <- Slick::OMs()
  OMs@Design  <- design
  OMs@Factors <- purrr::imap(design, \(levs, nm) {
    u <- unique(levs)
    data.frame(Factor = nm, Level = u, Description = u)
  }) |> dplyr::bind_rows()

  if (AutoPreset && !autoDesign && length(design)) {
    levelsList <- purrr::map(design, unique)
    preset <- list()
    for (col in names(design)) {
      lv <- levelsList[[col]]
      for (i in seq_along(lv)) {
        entry <- purrr::map(names(design), \(c2) if (c2 == col) i else seq_along(levelsList[[c2]]))
        preset[[paste0(col, '_', lv[i])]] <- entry
      }
    }
    Slick::Preset(OMs) <- preset
  }

  OMs
}


.MSE2Kobe <- function(MSE, Complex = NULL, Years = NULL, Limit = c(0.4, 1)) {
  .SlickChecks(MSE)

  mseList <- if (is.list(MSE)) MSE else list(MSE)
  mse_ref <- mseList[[1]]

  allComplexes <- names(Complexes(mse_ref@OM)) %||NA% StockNames(mse_ref@OM)
  Complex <- Complex %||NA% allComplexes[1]
  if (!Complex %in% allComplexes)
    cli::cli_abort("{.arg Complex} = {.val {Complex}} is not one of {.val {allComplexes}}.")

  Kobe              <- Slick::Kobe()
  Kobe@Code         <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Label        <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Description  <- c(
    'Spawning biomass (SB) relative to SB at maximum sustainable yield (SBMSY)',
    'Fishing mortality (F) relative to F at maximum sustainable yield (FMSY)'
  )
  Kobe@Time    <- Years %||NA% PMYears(mse_ref)
  Kobe@TimeLab <- .SlickTimeLab(mse_ref@OM)
  Kobe@Target  <- rep(1, 2)
  Kobe@Limit   <- Limit

  MPs_names <- names(mse_ref@MPs)
  nsim      <- mse_ref@OM@nSim
  Kobe@Value <- array(NA_real_, dim = c(nsim, length(mseList), length(MPs_names), 2L,
                                        length(Kobe@Time)))

  for (om in seq_along(mseList)) {
    df <- .KobeStatusDF(mseList[[om]], 'SBiomass', ActiveOnly = FALSE)
    df <- df[df$Stock == Complex & df$Year %in% Kobe@Time & df$MP %in% MPs_names, ]
    idx <- cbind(match(df$Sim, seq_len(nsim)), om, match(df$MP, MPs_names), 1L,
                 match(df$Year, Kobe@Time))
    Kobe@Value[idx] <- df$SB
    idx[, 4] <- 2L
    Kobe@Value[idx] <- df$F
  }
  Kobe
}

.ComplexStatusSeries <- function(object, Definition = c('SBiomass', 'SProduction')) {
  Definition <- match.arg(Definition)
  series <- .StockStatusSeries(object, Definition)
  .ComplexStatusRatio(series$value, series$msy, Definition, object@OM)
}

.ComplexStatusRatio <- function(value, msy, Definition, OM) {
  spawn_stocks <- .SpawningStockNames(OM)

  sb_arr    <- .FilterStockDim(value, spawn_stocks) |>
    .AnnualMSYNumerator(Definition, OM)
  sbmsy_arr <- .FilterStockDim(msy, spawn_stocks)

  sb_complex    <- .AggregateStockToComplex(sb_arr, OM, sum, strict = FALSE)
  sbmsy_complex <- .AggregateStockToComplex(sbmsy_arr, OM, sum, strict = FALSE)

  target_years  <- dimnames(sb_complex)[['Year']]
  sbmsy_aligned <- .AlignDenomYears(sbmsy_complex, target_years)
  if ('MP' %in% names(dimnames(sb_complex)))
    sbmsy_aligned <- AddDimension(sbmsy_aligned, 'MP', val = dimnames(sb_complex)[['MP']])

  ArrayDivide(sb_complex, sbmsy_aligned)
}

.ComplexUnfishedRatio <- function(value, unfished, OM) {
  spawn_stocks <- .SpawningStockNames(OM)
  num <- .FilterStockDim(value, spawn_stocks) |> .AggregateStockToComplex(OM, sum, strict = FALSE)
  den <- .FilterStockDim(unfished, spawn_stocks) |> .AggregateStockToComplex(OM, sum, strict = FALSE)
  den <- .AlignDenomYears(den, dimnames(num)[['Year']])
  if ('MP' %in% names(dimnames(num)))
    den <- AddDimension(den, 'MP', val = dimnames(num)[['MP']])
  ArrayDivide(num, den)
}

# Annual spawning biomass (or production) per complex relative to equilibrium
# unfished, historical and projection periods.
.ComplexDepletionDF <- function(object, Definition = c('SBiomass', 'SProduction'),
                                Basis = c('RefSeason', 'Mean'), type = 'Equilibrium') {
  Definition <- match.arg(Definition)
  Basis      <- match.arg(Basis)
  OM <- object@OM
  denom <- slot(slot(object@Unfished, type), Definition)
  .CheckRefPopulated(denom, 'Unfished', paste0('Unfished@', type, '@', Definition), 'SB_SB0')

  parts <- if (inherits(object, 'mse')) {
    list(Historical = slot(object@Hist, Definition), Projection = slot(object, Definition))
  } else {
    list(Historical = slot(object, Definition))
  }
  df <- purrr::imap(parts, \(arr, period) {
    .ComplexUnfishedRatio(arr, denom, OM) |>
      ExtendSims(OM@nSim) |>
      Array2DF() |>
      dplyr::mutate(Period = period)
  }) |> dplyr::bind_rows()
  df$MP <- if ('MP' %in% names(df)) as.character(df$MP) else NA_character_
  df$MP[df$Period == 'Historical'] <- 'Historical'
  .AnnualDF(df, OM, Basis)
}

.TimeseriesDefaults <- list(
  Label = c(SB_SBMSY = 'SB/SBMSY', F_FMSY = 'F/FMSY', SB_SB0 = 'SB/SB0',
            SBiomass = 'Spawning biomass', Landings = 'Landings', Removals = 'Removals',
            TAC = 'TAC'),
  Description = c(
    SB_SBMSY = 'Spawning biomass relative to spawning biomass at maximum sustainable yield (SBMSY)',
    F_FMSY   = 'Fishing mortality relative to fishing mortality at maximum sustainable yield (FMSY)',
    SB_SB0   = 'Spawning biomass relative to equilibrium unfished spawning biomass (SB0)',
    SBiomass = 'Spawning biomass',
    Landings = 'Landings of all fleets',
    Removals = 'Removals (landings and dead discards) of all fleets',
    TAC      = 'Total allowable catch (TAC)'
  ),
  Target = c(SB_SBMSY = 1, F_FMSY = 1),
  Limit  = c(SB_SBMSY = 0.4, F_FMSY = 1)
)

# Catch and F are summed over the seasons of each calendar year.
.SummableVars <- c('Landings', 'Removals', 'Discards', 'FInteract', 'FDead', 'FRetain', 'Effort')

.TimeseriesText <- function(Value, Default, Code, argName) {
  if (is.null(Value)) {
    out <- unname(Default[Code])
    out[is.na(out)] <- Code[is.na(out)]
    return(out)
  }
  if (length(Value) != length(Code))
    cli::cli_abort("{.arg {argName}} must have one entry per {.arg TimeseriesCode} ({length(Code)}).")
  unname(Value)
}

.TimeseriesRef <- function(Value, Default, Code) {
  Ref <- Default
  if (!is.null(Value)) {
    if (is.null(names(Value)))
      cli::cli_abort("{.arg TimeseriesTarget} and {.arg TimeseriesLimit} must be named by {.arg TimeseriesCode}.")
    Ref[names(Value)] <- Value
  }
  unname(Ref[Code])
}

.MSE2Timeseries <- function(MSE, Code = NULL, Label = NULL, Description = NULL,
                            Target = NULL, Limit = NULL, Basis = 'RefSeason') {
  .SlickChecks(MSE)
  Code        <- Code %||NA% c('SB_SBMSY', 'F_FMSY', 'Landings')
  Label       <- .TimeseriesText(Label, .TimeseriesDefaults$Label, Code, 'TimeseriesLabel')
  Description <- .TimeseriesText(Description, .TimeseriesDefaults$Description, Code,
                                 'TimeseriesDescription')
  Target <- .TimeseriesRef(Target, .TimeseriesDefaults$Target, Code)
  Limit  <- .TimeseriesRef(Limit, .TimeseriesDefaults$Limit, Code)

  mseList <- if (is.list(MSE)) MSE else list(MSE)
  mse_ref <- mseList[[1]]
  allComplexes <- names(Complexes(mse_ref@OM)) %||NA% StockNames(mse_ref@OM)
  nCx   <- length(allComplexes)
  multi <- nCx > 1

  Timeseries              <- Slick::Timeseries()
  Timeseries@Code         <- if (multi) paste0(rep(Code, each = nCx), '_', allComplexes) else Code
  Timeseries@Label        <- if (multi) paste0(rep(Label, each = nCx), ' [', allComplexes, ']') else Label
  Timeseries@Description  <- rep(Description, each = nCx)
  Timeseries@Time         <- .SlickTime(mse_ref@OM)
  Timeseries@TimeNow      <- max(.SlickTime(mse_ref@OM, 'Historical'))
  Timeseries@TimeLab      <- .SlickTimeLab(mse_ref@OM)
  Timeseries@Target       <- rep(Target, each = nCx)
  Timeseries@Limit        <- rep(Limit, each = nCx)

  nsim <- mse_ref@OM@nSim
  nMP  <- length(mse_ref@MPs)
  Timeseries@Value <- array(NA_real_, dim = c(nsim, length(mseList), nMP,
                                              length(Timeseries@Code), length(Timeseries@Time)))

  for (om in seq_along(mseList)) {
    pi <- 1
    for (i in seq_along(Code)) {
      for (cx in allComplexes) {
        Timeseries@Value[, om, , pi, ] <- .GetTimeseriesVariable(Code[i], mseList[[om]], cx,
                                                                 Timeseries@Time, Basis)
        pi <- pi + 1
      }
    }
  }
  Timeseries
}


.GetTimeseriesVariable <- function(Var, MSE, Complex, Time, Basis = 'RefSeason') {
  nsim <- MSE@OM@nSim
  MPs  <- names(MSE@MPs)

  DF <- .ComplexTimeseriesDF(Var, MSE, Complex, Basis) |>
    .AnnualTimeseriesDF(Var, MSE@OM, Complex, Basis) |>
    as.data.frame()
  DF$MP <- as.character(DF$MP)
  DF <- DF[DF$Year %in% Time, ]

  Array <- array(NA_real_, dim = c(nsim, length(MPs), length(Time)))
  Hist  <- DF[DF$MP == 'Historical', ]
  if (nrow(Hist))
    for (mm in seq_along(MPs))
      Array[cbind(match(Hist$Sim, seq_len(nsim)), mm, match(Hist$Year, Time))] <- Hist$Value
  Proj <- DF[DF$MP %in% MPs, ]
  Array[cbind(match(Proj$Sim, seq_len(nsim)), match(Proj$MP, MPs), match(Proj$Year, Time))] <- Proj$Value
  Array
}

# Slick time axis: complete calendar years for seasonal OMs.
.SlickTime <- function(OM, Period = NULL) {
  yrs <- if (is.null(Period)) Years(OM) else Years(OM, Period)
  if (.IsSeasonal(OM)) .CompleteCalendarYears(yrs, OM@Seasons) else yrs
}

.SlickTimeLab <- function(OM) {
  .FirstUp(CalcTSUnits(if (.IsSeasonal(OM)) 1 else OM@Seasons))
}

# Annual values of a time-step series for seasonal OMs: catches and F summed
# over the seasons, the TAC averaged, and other variables per `Basis`.
.AnnualTimeseriesDF <- function(DF, Var, OM, Complex, Basis = 'RefSeason') {
  if (!.IsSeasonal(OM) || all(abs(DF$Year - round(DF$Year)) < 1e-8))
    return(DF)
  Basis <- if (Var %in% .SummableVars) 'Sum' else if (Var == 'TAC') 'Mean' else Basis
  DF$Stock <- Complex
  out <- .AnnualDF(DF, OM, Basis)
  out$Stock <- NULL
  out
}

.StockComplexMap <- function(OM) {
  stockNms  <- StockNames(OM)
  complexes <- Complexes(OM)
  if (!length(complexes))
    complexes <- stats::setNames(as.list(seq_along(stockNms)), stockNms)
  out <- stats::setNames(character(length(stockNms)), stockNms)
  for (cx in names(complexes))
    out[stockNms[complexes[[cx]]]] <- cx
  out
}

.SumByComplex <- function(df, OM, Complex, FUN = sum) {
  complexOf  <- .StockComplexMap(OM)
  df$Complex <- complexOf[as.character(df$Stock)]
  df[df$Complex == Complex, ] |>
    dplyr::group_by(.data$Sim, .data$Year, .data$Period, .data$MP) |>
    dplyr::summarise(Value = FUN(.data$Value), .groups = 'drop')
}

.ComplexTimeseriesDF <- function(Var, MSE, Complex, Basis = 'RefSeason') {
  OM <- MSE@OM
  spawnStocks <- .SpawningStockNames(OM)

  if (Var == 'SBiomass') {
    df <- SBiomass(MSE, Reduce = FALSE) |> dplyr::filter(.data$Stock %in% spawnStocks)
    return(.SumByComplex(df, OM, Complex))
  }

  if (Var == 'SB_SBMSY') {
    hist_df <- .ComplexStatusRatio(MSE@Hist@SBiomass, MSE@Reference@MSY@SBMSY,
                                   'SBiomass', OM) |>
      Array2DF() |>
      dplyr::mutate(Period = 'Historical', MP = 'Historical')
    proj_df <- Array2DF(.ComplexStatusSeries(MSE, 'SBiomass')) |>
      dplyr::mutate(Period = 'Projection')
    return(dplyr::bind_rows(hist_df, proj_df) |>
             dplyr::filter(.data$Stock == !!Complex) |>
             dplyr::select('Sim', 'Year', 'Period', 'MP', 'Value'))
  }

  if (Var == 'SB_SB0') {
    return(.ComplexDepletionDF(MSE, 'SBiomass', Basis) |>
             dplyr::filter(.data$Stock == !!Complex) |>
             dplyr::select('Sim', 'Year', 'Period', 'MP', 'Value'))
  }

  if (Var %in% c('FInteract', 'FDead', 'FRetain')) {
    df <- do.call(Var, list(MSE, byAge = FALSE, byArea = FALSE, byFleet = FALSE, Reduce = FALSE))
    return(.SumByComplex(df, OM, Complex, max))
  }

  if (Var == 'F_FMSY') {
    return(F_FMSY(MSE, Reduce = FALSE) |>
             dplyr::filter(.data$Stock == !!Complex) |>
             dplyr::select('Sim', 'Year', 'Period', 'MP', 'Value'))
  }

  if (Var %in% c('Landings', 'Removals')) {
    df <- do.call(Var, list(MSE, byFleet = FALSE, Reduce = FALSE))
    return(.SumByComplex(df, OM, Complex))
  }

  if (Var == 'TAC') {
    df <- TACs(MSE)
    return(df[df$Stock == Complex, ] |>
             dplyr::group_by(.data$Sim, .data$Year, .data$Period, .data$MP) |>
             dplyr::summarise(Value = if (all(is.na(.data$Value))) NA_real_ else
                                sum(.data$Value, na.rm = TRUE), .groups = 'drop'))
  }

  df <- do.call(Var, list(MSE))
  if ('Stock' %in% names(df)) df <- df[df$Stock == Complex, ]
  df
}

# A PM spec as `list(fun, args, Code, Label, Description)`.
.ResolvePMSpec <- function(spec, env = parent.frame()) {
  if (is.function(spec))
    return(list(fun = spec, args = list()))
  if (is.call(spec))
    return(list(fun  = eval(spec[[1]], env),
                args = lapply(as.list(spec)[-1], eval, envir = env)))
  if (!is.list(spec))
    cli::cli_abort("Each element of {.arg PMs} must be a PM function, a call, or a list.")
  if (!is.null(names(spec))) {
    names(spec)[names(spec) == 'PM']   <- 'fun'
    names(spec)[names(spec) == 'Args'] <- 'args'
  }
  if (is.null(spec$fun) && length(spec) && is.function(spec[[1]]))
    spec$fun <- spec[[1]]
  if (!is.function(spec$fun))
    cli::cli_abort("A {.arg PMs} list must include a PM function as {.code PM} (or {.code fun}).")
  spec$args <- spec$args %||NA% list()
  spec
}

.EvalPMPanel <- function(MSE, PMs, env = parent.frame()) {
  specs   <- purrr::map(PMs, \(spec) .ResolvePMSpec(spec, env))
  mseList <- if (is.list(MSE)) MSE else list(MSE)
  nOM     <- length(mseList)

  results <- purrr::map(specs, \(spec)
    purrr::map(mseList, \(mse) do.call(spec$fun, c(list(mse), spec$args)))
  )

  piInfo <- purrr::list_flatten(purrr::imap(results, \(perOM, i) {
    pmobj     <- perOM[[1]]
    complexes <- dimnames(pmobj@Stat)[['Stock']]
    spec      <- specs[[i]]
    code      <- spec$Code  %||NA% pmobj@Name
    label     <- spec$Label %||NA% pmobj@Caption
    desc      <- spec$Description %||NA% pmobj@Caption
    isProb    <- any(purrr::map_lgl(perOM, \(pm) any(!is.na(pm@Prob))))
    purrr::map(complexes, \(cx) list(
      spec_i = i, complex = cx, isProb = isProb,
      Code        = if (length(complexes) > 1) paste0(code, '_', cx) else code,
      Label       = if (length(complexes) > 1) paste0(label, ' [', cx, ']') else label,
      Description = desc
    ))
  }))

  Codes <- purrr::map_chr(piInfo, 'Code')
  if (anyDuplicated(Codes))
    cli::cli_abort(c("Duplicated PM code{?s}: {.val {unique(Codes[duplicated(Codes)])}}.",
                     "i" = "Give each PM a unique {.code Code}, e.g. when the same PM function is used with different {.code Args}."))

  MPnames <- names(mseList[[1]]@MPs)
  nMP     <- length(MPnames)
  nSim    <- mseList[[1]]@OM@nSim
  nPI     <- length(piInfo)

  StatArr <- array(NA_real_, dim = c(nSim, nOM, nMP, nPI))
  ProbArr <- array(NA_real_, dim = c(nSim, nOM, nMP, nPI))
  MeanArr <- array(NA_real_, dim = c(nOM, nMP, nPI))

  for (pi in seq_len(nPI)) {
    info <- piInfo[[pi]]
    for (om in seq_len(nOM)) {
      pmobj <- results[[info$spec_i]][[om]]
      sims  <- match(as.numeric(dimnames(pmobj@Stat)[['Sim']]), seq_len(nSim))
      mps   <- match(dimnames(pmobj@Stat)[['MP']], MPnames)
      keep  <- !is.na(mps)
      StatArr[sims, om, mps[keep], pi] <- pmobj@Stat[, info$complex, keep]
      ProbArr[sims, om, mps[keep], pi] <- pmobj@Prob[, info$complex, keep]
      MeanArr[om, mps[keep], pi]       <- pmobj@Mean[info$complex, keep]
    }
  }
  StatArr[is.nan(StatArr)] <- NA
  ProbArr[is.nan(ProbArr)] <- NA
  MeanArr[is.nan(MeanArr)] <- NA

  isProb <- purrr::map_lgl(piInfo, 'isProb')
  Value  <- StatArr
  Value[, , , isProb] <- ProbArr[, , , isProb]

  list(
    Code        = Codes,
    Label       = purrr::map_chr(piInfo, 'Label'),
    Description = purrr::map_chr(piInfo, 'Description'),
    MPs = MPnames, isProb = isProb, Value = Value, Mean = MeanArr
  )
}


.MSE2Boxplot <- function(panel) {
  Slick::Boxplot(Code = panel$Code, Label = panel$Label,
                 Description = panel$Description, Value = panel$Value)
}

.MSE2Quilt <- function(panel) {
  Slick::Quilt(Code = panel$Code, Label = panel$Label,
               Description = panel$Description, Value = panel$Value,
               MinValue = ifelse(panel$isProb, 0, NA_real_),
               MaxValue = ifelse(panel$isProb, 1, NA_real_))
}

.MSE2Spider <- function(panel) {
  Value <- panel$Mean
  for (pi in which(!panel$isProb)) {
    Max <- apply(abs(Value[, , pi, drop = FALSE]), 1,
                 \(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
    Value[, , pi] <- Value[, , pi] / ifelse(is.finite(Max) & Max > 0, Max, NA_real_)
  }
  Slick::Spider(Code = panel$Code, Label = panel$Label,
                Description = panel$Description, Value = Value)
}

.MSE2Tradeoff <- function(panel) {
  Slick::Tradeoff(Code = panel$Code, Label = panel$Label,
                  Description = panel$Description, Value = panel$Mean)
}
