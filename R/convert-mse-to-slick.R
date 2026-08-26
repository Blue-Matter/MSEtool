
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
#' - **Kobe**: SB/SBMSY and F/FMSY time series for the Kobe plot. Kobe is
#'   single-stock so for a multi-complex OM, `KobeComplex` selects which one to
#'   show (default: the first).
#' - **Timeseries**: SB/SBMSY, F/FMSY, and landings time series.
#' - **Boxplot/Quilt/Spider/Tradeoff**: driven by `PMs`, a flexible list of
#'   performance-metric specs (see `PMs` below). Each is evaluated
#'   against every complex in every OM.
#'
#' `PMs` (and the per-panel `BoxplotPMs`/`QuiltPMs`/`SpiderPMs`/
#' `TradeoffPMs` overrides) is a list of performance-metric specifications: each
#' element is either a `PM_*` function (e.g., `PM_Status` for [PM_Status()]), or a list
#' `list(fun, args = list(), Code = NULL, Label = NULL, Description = NULL)`, e.g.
#' `list(fun = PM_Yield, args = list(Years = 2000:2009), Code = "Yield_early")`. Each per-panel override
#' falls back to `PMs` when `NULL`; a spec list is only re-evaluated once
#' even when shared across panels.
#'
#' `TimeseriesCode`/`TimeseriesLabel` override the variables built into the
#' `Timeseries` panel; entries must name functions that accept an [mse-class]
#'  object and return a tidy data frame with columns `Sim`, `Year`, `MP`, `Period`,
#'  and `Value`.
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
#' @param TimeseriesCode,TimeseriesLabel Overrides for the `Timeseries`
#'   panel's variables, or `NULL` (default) to keep `.MSE2Timeseries()`'s
#'   own defaults. See Details.
#' @param KobeComplex Character, or `NULL` (default: the first complex).
#'   Which stock/complex the Kobe plot shows, when `MSE` has more than one.
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
#' }
#'
#' @seealso [Slick::Slick()], [Slick::App()], [mse-class], [DefaultSlickPMs()]
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
                      KobeComplex  = NULL,
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

  mse_ref  <- if (is.list(MSE)) MSE[[1]] else MSE
  PMs      <- PMs %||% DefaultSlickPMs()

  BoxplotPMs  <- BoxplotPMs  %||% PMs
  QuiltPMs    <- QuiltPMs    %||% PMs
  SpiderPMs   <- SpiderPMs   %||% PMs
  TradeoffPMs <- TradeoffPMs %||% PMs

  panelCache <- new.env(parent = emptyenv())
  .panel <- function(pms) {
    key <- rlang::hash(pms)
    if (is.null(panelCache[[key]])) panelCache[[key]] <- .EvalPMPanel(MSE, pms)
    panelCache[[key]]
  }

  Slick             <- Slick::Slick()
  Slick@Title       <- Title %||% mse_ref@OM@Name
  Slick@Subtitle    <- Subtitle
  Slick@Author      <- Author
  Slick@Email       <- Email
  Slick@Institution <- Institution
  Slick@Introduction <- Introduction
  Slick@Date        <- Date %||% Sys.Date()
  Slick@MPs         <- .MSE2MPs(mse_ref, MPCode, MPLabel, MPDescription) |> .ApplyPreset(MPsPreset)
  Slick@OMs         <- .MSE2OMs(MSE, Design, AutoPreset) |> .ApplyPreset(OMsPreset)
  Slick@Kobe        <- .MSE2Kobe(MSE, KobeComplex)
  tsArgs <- list(MSE = MSE)
  if (!is.null(TimeseriesCode))  tsArgs$Code  <- TimeseriesCode
  if (!is.null(TimeseriesLabel)) tsArgs$Label <- TimeseriesLabel
  Slick@Timeseries  <- do.call(.MSE2Timeseries, tsArgs)
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
    complex_sets <- purrr::map(MSE, \(mse) sort(names(Complexes(mse@OM)) %||% StockNames(mse@OM)))
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
#' - [PM_Yield()]: mean projected catch, on its natural (biomass) scale;
#'   not a probability, so it's excluded from `Spider` (which requires
#'   every PI on a 0-1/0-100 scale) but included in `Boxplot`/`Quilt`/
#'   `Tradeoff`.
#' - [PM_AAVY()]: average annual variability in yield, i.e. how much catch
#'   swings year to year; also natural-scale, same `Spider` exclusion as
#'   `PM_Yield()`.
#' - [PM_Status()]: the joint probability that a stock/complex is neither
#'   overfished nor experiencing overfishing (`P(SB > SBMSY & F < FMSY)`).#'   
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


.MSE2Kobe <- function(MSE, Complex = NULL) {
  .SlickChecks(MSE)

  mse_ref <- if (is.list(MSE)) MSE[[1]] else MSE
  nOM     <- if (is.list(MSE)) length(MSE) else 1L

  allComplexes <- names(Complexes(mse_ref@OM)) %||% StockNames(mse_ref@OM)
  Complex <- Complex %||% allComplexes[1]
  if (!Complex %in% allComplexes)
    cli::cli_abort("{.arg Complex} = {.val {Complex}} is not one of {.val {allComplexes}}.")

  Kobe              <- Slick::Kobe()
  Kobe@Code         <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Label        <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Description  <- c(
    'Spawning biomass (SB) relative to SB at maximum sustainable yield (SBMSY)',
    'Fishing mortality (F) relative to F at maximum sustainable yield (FMSY)'
  )
  Kobe@Time    <- Years(mse_ref@OM, 'Projection')
  Kobe@TimeLab <- .FirstUp(CalcTSUnits(mse_ref@OM@Seasons))
  Kobe@Target  <- rep(1, 2)

  MPs_names    <- names(mse_ref@MPs)
  nsim         <- mse_ref@OM@nSim
  nMP          <- length(MPs_names)
  nTS          <- length(Kobe@Time)
  ProjectionTS <- Kobe@Time

  Kobe@Value <- array(NA, dim=c(nsim, nOM, nMP, 2L, nTS))

  for (om in seq_len(nOM)) {
    mse <- if (is.list(MSE)) MSE[[om]] else MSE

    sb_sbmsy_arr <- .ComplexStatusSeries(mse, 'SBiomass')
    sb_sbmsy <- Array2DF(sb_sbmsy_arr) |>
      dplyr::rename(Complex = 'Stock', Value = 'Value') |>
      dplyr::filter(.data$Year %in% ProjectionTS, .data$Complex == !!Complex) |>
      dplyr::arrange(.data$Sim, .data$Year, .data$MP)

    f_fmsy <- F_FMSY(mse) |>
      dplyr::filter(.data$Year %in% ProjectionTS, .data$Stock == !!Complex) |>
      dplyr::arrange(.data$Sim, .data$Year, .data$MP)

    for (mm in seq_len(nMP)) {
      Kobe@Value[, om, mm, 1, ] <- sb_sbmsy |>
        dplyr::filter(.data$MP == MPs_names[mm]) |>
        dplyr::pull(.data$Value) |>
        matrix(nrow=nsim, ncol=nTS, byrow=TRUE)

      Kobe@Value[, om, mm, 2, ] <- f_fmsy |>
        dplyr::filter(.data$MP == MPs_names[mm]) |>
        dplyr::pull(.data$Value) |>
        matrix(nrow=nsim, ncol=nTS, byrow=TRUE)
    }
  }
  Kobe
}

.ComplexStatusSeries <- function(object, Definition = c('SBiomass', 'SProduction')) {
  Definition <- match.arg(Definition)
  spawn_stocks <- .SpawningStockNames(object@OM)
  series <- .StockStatusSeries(object, Definition)

  sb_arr    <- .FilterStockDim(series$value, spawn_stocks)
  sbmsy_arr <- .FilterStockDim(series$msy, spawn_stocks)

  sb_complex    <- .AggregateStockToComplex(sb_arr, object@OM, sum, strict = FALSE)
  sbmsy_complex <- .AggregateStockToComplex(sbmsy_arr, object@OM, sum, strict = FALSE)

  target_years  <- dimnames(sb_complex)[['Year']]
  sbmsy_aligned <- .AlignDenomYears(sbmsy_complex, target_years) |>
    AddDimension('MP', val = dimnames(sb_complex)[['MP']])

  ArrayDivide(sb_complex, sbmsy_aligned)
}

.MSE2Timeseries <- function(MSE,
                            Code  = c('SB_SBMSY',
                                      'F_FMSY',
                                      'Landings'),
                            Label = c('SB/SBMSY',
                                      'F/FMSY',
                                      'Landings')) {
  .SlickChecks(MSE)

  mse_ref <- if (is.list(MSE)) MSE[[1]] else MSE
  nOM     <- if (is.list(MSE)) length(MSE) else 1L
  allComplexes <- names(Complexes(mse_ref@OM)) %||% StockNames(mse_ref@OM)
  multi   <- length(allComplexes) > 1

  piCode  <- if (multi) paste0(rep(Code,  each = length(allComplexes)), '_', allComplexes) else Code
  piLabel <- if (multi) paste0(rep(Label, each = length(allComplexes)), ' [', allComplexes, ']') else Label

  Timeseries              <- Slick::Timeseries()
  Timeseries@Code         <- piCode
  Timeseries@Label        <- piLabel
  Timeseries@Time         <- Years(mse_ref@OM)
  Timeseries@TimeNow      <- max(Years(mse_ref@OM, 'Historical'))
  Timeseries@TimeLab      <- .FirstUp(CalcTSUnits(mse_ref@OM@Seasons))

  refTarget <- c(SB_SBMSY = 1,   F_FMSY = NA)
  refLimit  <- c(SB_SBMSY = 0.4, F_FMSY = 1)
  Timeseries@Target       <- rep(unname(refTarget[Code]), length(allComplexes))
  Timeseries@Limit        <- rep(unname(refLimit[Code]),  length(allComplexes))

  nsim <- mse_ref@OM@nSim
  nMP  <- length(mse_ref@MPs)
  nPI  <- length(piCode)
  nTS  <- length(Timeseries@Time)

  Timeseries@Value <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))

  for (om in seq_len(nOM)) {
    mse <- if (is.list(MSE)) MSE[[om]] else MSE
    pi  <- 1
    for (i in seq_along(Code)) {
      for (cx in allComplexes) {
        Timeseries@Value[, om, , pi, ] <- .GetTimeseriesVariable(Code[i], mse, cx)
        pi <- pi + 1
      }
    }
  }
  Timeseries
}


.GetTimeseriesVariable <- function(Var, MSE, Complex) {
  nsim <- MSE@OM@nSim
  nMP  <- length(MSE@MPs)
  nTS  <- length(Years(MSE@OM))

  DF <- .ComplexTimeseriesDF(Var, MSE, Complex) |>
    dplyr::arrange(.data$Sim, .data$Year, .data$MP) |>
    dplyr::mutate(MP=as.character(.data$MP))

  MPs_proj <- DF$MP |> unique()
  MPs_proj <- MPs_proj[MPs_proj != 'Historical']

  nHistTS <- DF |>
    dplyr::filter(.data$Period == 'Historical') |>
    dplyr::pull(.data$Year) |>
    unique() |>
    length()

  HistValues <- DF |>
    dplyr::filter(.data$MP == 'Historical') |>
    dplyr::pull(.data$Value) |>
    matrix(nrow=nsim, ncol=nHistTS, byrow=TRUE)

  Array <- array(NA, dim=c(nsim, nMP, nTS))

  for (mm in seq_along(MPs_proj)) {
    ProjValues <- DF |>
      dplyr::filter(.data$MP == MPs_proj[mm]) |>
      dplyr::pull(.data$Value) |>
      matrix(nrow=nsim, ncol=nTS - nHistTS, byrow=TRUE)
    Array[, mm, ] <- cbind(HistValues, ProjValues)
  }
  Array
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


.ComplexTimeseriesDF <- function(Var, MSE, Complex) {
  complexOf   <- .StockComplexMap(MSE@OM)
  spawnStocks <- .SpawningStockNames(MSE@OM)

  if (Var == 'SBiomass') {
    df <- SBiomass(MSE) |> dplyr::filter(.data$Stock %in% spawnStocks)
    df$Complex <- complexOf[as.character(df$Stock)]
    df <- df[df$Complex == Complex, ]
    return(df |> dplyr::group_by(.data$Sim, .data$Year, .data$Period, .data$MP) |>
             dplyr::summarise(Value = sum(.data$Value), .groups = 'drop'))
  }

  if (Var == 'SB_SBMSY') {
    sb_df    <- SBiomass(MSE) |> dplyr::filter(.data$Stock %in% spawnStocks)
    ratio_df <- SB_SBMSY(MSE) |> dplyr::filter(.data$Stock %in% spawnStocks) |>
      dplyr::select('Sim', 'Stock', 'Year', 'Period', 'MP', Ratio = 'Value')
    merged <- dplyr::left_join(sb_df, ratio_df, by = c('Sim', 'Stock', 'Year', 'Period', 'MP'))
    merged$SBMSY <- merged$Value / merged$Ratio
    merged$Complex <- complexOf[as.character(merged$Stock)]
    merged <- merged[merged$Complex == Complex, ]
    return(merged |> dplyr::group_by(.data$Sim, .data$Year, .data$Period, .data$MP) |>
             dplyr::summarise(Value = sum(.data$Value) / sum(.data$SBMSY), .groups = 'drop'))
  }

  if (Var %in% c('FInteract', 'FDead', 'FRetain')) {
    df <- do.call(Var, list(MSE, byAge = FALSE, byArea = FALSE, byFleet = FALSE))
    df$Complex <- complexOf[as.character(df$Stock)]
    df <- df[df$Complex == Complex, ]
    return(df |> dplyr::group_by(.data$Sim, .data$Year, .data$Period, .data$MP) |>
             dplyr::summarise(Value = max(.data$Value), .groups = 'drop'))
  }

  if (Var == 'F_FMSY') {
    return(F_FMSY(MSE) |> dplyr::filter(.data$Stock == !!Complex) |> dplyr::select(-'Stock'))
  }

  if (Var == 'Landings') {
    df <- Landings(MSE) |>
      dplyr::group_by(.data$Sim, .data$Stock, .data$Year, .data$Period, .data$MP) |>
      dplyr::summarise(Value = sum(.data$Value), .groups = 'drop') # sum over Fleet
    df$Complex <- complexOf[as.character(df$Stock)]
    df <- df[df$Complex == Complex, ]
    return(df |> dplyr::group_by(.data$Sim, .data$Year, .data$Period, .data$MP) |>
             dplyr::summarise(Value = sum(.data$Value), .groups = 'drop'))
  }

  df <- do.call(Var, list(MSE))
  if ('Stock' %in% names(df)) df <- df[df$Stock == Complex, ]
  df
}

.ResolvePMSpec <- function(spec) {
  if (is.function(spec)) spec <- list(fun = spec)
  spec$args <- spec$args %||% list()
  spec
}

.EvalPMPanel <- function(MSE, PMs) {
  specs   <- purrr::map(PMs, .ResolvePMSpec)
  mseList <- if (is.list(MSE)) MSE else list(MSE)
  nOM     <- length(mseList)

  results <- purrr::map(specs, \(spec)
    purrr::map(mseList, \(mse) do.call(spec$fun, c(list(mse), spec$args)))
  )

  piInfo <- purrr::list_flatten(purrr::imap(results, \(perOM, i) {
    pmobj     <- perOM[[1]]
    complexes <- dimnames(pmobj@Stat)[['Stock']]
    spec      <- specs[[i]]
    code      <- spec$Code  %||% pmobj@Name
    label     <- spec$Label %||% pmobj@Caption
    desc      <- spec$Description %||% pmobj@Caption
    purrr::map(complexes, \(cx) list(
      spec_i = i, complex = cx,
      Code        = if (length(complexes) > 1) paste0(code, '_', cx) else code,
      Label       = if (length(complexes) > 1) paste0(label, ' [', cx, ']') else label,
      Description = desc
    ))
  }))

  MPnames <- results[[1]][[1]]@MPs
  nMP     <- length(MPnames)
  nSim    <- dim(results[[1]][[1]]@Stat)[1]
  nPI     <- length(piInfo)

  StatArr <- array(NA_real_, dim = c(nSim, nOM, nMP, nPI))
  ProbArr <- array(NA_real_, dim = c(nSim, nOM, nMP, nPI))
  MeanArr <- array(NA_real_, dim = c(nOM, nMP, nPI))

  for (pi in seq_len(nPI)) {
    info <- piInfo[[pi]]
    for (om in seq_len(nOM)) {
      pmobj <- results[[info$spec_i]][[om]]
      StatArr[, om, , pi] <- pmobj@Stat[, info$complex, ]
      ProbArr[, om, , pi] <- pmobj@Prob[, info$complex, ]
      MeanArr[om, , pi]   <- pmobj@Mean[info$complex, ]
    }
  }

  list(
    Code        = purrr::map_chr(piInfo, 'Code'),
    Label       = purrr::map_chr(piInfo, 'Label'),
    Description = purrr::map_chr(piInfo, 'Description'),
    MPs = MPnames, Stat = StatArr, Prob = ProbArr, Mean = MeanArr
  )
}


.MSE2Boxplot <- function(panel) {
  Slick::Boxplot(Code = panel$Code, Label = panel$Label,
                 Description = panel$Description, Value = panel$Stat)
}

.MSE2Quilt <- function(panel) {
  Slick::Quilt(Code = panel$Code, Label = panel$Label,
              Description = panel$Description, Value = panel$Stat)
}

.MSE2Spider <- function(panel) {
  Slick::Spider(Code = panel$Code, Label = panel$Label,
                Description = panel$Description, Value = panel$Mean)
}

.MSE2Tradeoff <- function(panel) {
  Slick::Tradeoff(Code = panel$Code, Label = panel$Label,
                  Description = panel$Description, Value = panel$Mean)
}
