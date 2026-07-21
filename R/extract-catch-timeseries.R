#' Extract or Assign Catch Time Series
#'
#' `Interactions()`, `Landings()`, `Discards()`, and `Removals()` extract
#' total interactions (encounters), retained landings, discards, and removals
#' (landings + discards) respectively.
#'
#' When called on a [hist-class] or [mse-class] object, the functions return
#' simulated catch arrays or tidy data frames as described below.
#'
#' When called on an [obs-class] object, `Landings()`/`Discards()` always
#' return the corresponding [catchobs-class] slot directly (the observation
#' error structure, not simulated values), regardless of any other argument.
#'
#' When called on a [data-class] object, `Landings()`/`Discards()` return the
#' corresponding [catchdata-class] slot directly (the observed or simulated
#' values) when neither `byAge` nor `bySize` is `TRUE`. When `byAge = TRUE`
#' or `bySize = TRUE`, they instead return a tidy `data.frame` built from the
#' corresponding `*AtAge`/`*AtSize` [compdata-class] slot, with columns
#' `Year`, `Fleet`, `Age` or `Class`, `Value`, and `Variable` (no `Sim`,
#' `Stock`, `Period`, or `MP`, since a `data` object is a single
#' realization); `df` is not meaningful in that case and is ignored. For
#' `*AtSize`, fleets are not required to share a size-class grid (see
#' [compdata-class]); `byFleet = FALSE` raises an error if the object's
#' fleets don't all share the exact same classes, the same as for
#' [hist-class]/[mse-class] objects.
#'
#' The assignment forms `Landings<-` and `Discards<-` replace the
#' corresponding slot of an [obs-class] or [data-class] object.
#'
#' When `byAge = FALSE` or `bySize = FALSE` for [hist-class] or [mse-class]
#' objects, the catch data are in units of biomass (`N x Weight`), where
#' `Weight` is the fleet-specific weight-at-age schedule from [Fleet()].
#' When `byAge = TRUE` or `bySize = TRUE` the values are in units of numbers.
#' When `byArea = TRUE` the values are always in units of numbers.
#'
#' When applied to an [mse-class] object the historical and projection periods
#' are row-bound and labelled via the `Period` column; historical rows carry
#' `MP = "Historical"`.
#'
#' @param object A [hist-class], [mse-class], [obs-class], or [data-class]
#'   object.
#' @param df Logical. Applies to [hist-class] and [mse-class] objects only.
#'   If `FALSE` the raw array slot is returned. If `TRUE` (default) a tidy
#'   `data.frame` is returned. Ignored for [obs-class] and [data-class]
#'   objects.
#' @param byAge Logical. If `TRUE` the data frame retains the `Age` dimension.
#'   Otherwise values are summed over ages. Mutually exclusive with `bySize`.
#'   Applies to [hist-class] and [mse-class] objects only.
#' @param bySize Logical. If `TRUE` the data frame retains the `Size`
#'   (length-bin) dimension using the `*AtSize` slots. Otherwise values are
#'   summed over size classes. Mutually exclusive with `byAge`. Not available
#'   for `Interactions()`. Applies to [hist-class] and [mse-class] objects
#'   only.
#' @param byArea Logical. If `TRUE` the data frame retains the `Area`
#'   dimension. Otherwise values are summed over areas. Applies to
#'   [hist-class] and [mse-class] objects only.
#' @param byFleet Logical. If `TRUE` (default for `Landings()` and
#'   `Discards()`) the data frame retains the `Fleet` dimension. Otherwise
#'   values are summed over fleets. Applies to [hist-class] and [mse-class]
#'   objects only. When `bySize = TRUE`, fleets are not required to share a
#'   size-class grid (see [compdata-class]); `byFleet = FALSE` raises an
#'   error if a stock's fleets don't all share the exact same classes,
#'   rather than silently summing incompatible bins.
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using [ReduceDims()] before conversion to a data frame. Applies
#'   to [hist-class] and [mse-class] objects only.
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the year
#'   dimension is retained during reduction. Default `FALSE`. Applies to
#'   [hist-class] and [mse-class] objects only.
#'
#' @return
#' - For [obs-class]: the [catchobs-class] object stored in the `Landings` or
#'   `Discards` slot.
#' - For [data-class] with `byAge = FALSE` and `bySize = FALSE`: the
#'   [catchdata-class] object stored in the `Landings` or `Discards` slot.
#' - For [data-class] with `byAge = TRUE` or `bySize = TRUE`: a tidy
#'   `data.frame` with columns `Year`, `Fleet`, `Age` or `Class`, `Value`,
#'   and `Variable`.
#' - For [hist-class] or [mse-class] with `df = FALSE`: the raw array slot
#'   (`Interactions`, `Landings`, or `Discards`).
#' - For [hist-class] or [mse-class] with `df = TRUE`: a tidy `data.frame`
#'   with columns `Sim`, `Stock`, `Year`, `Period`, `MP` (MSE only), and
#'   optionally `Age`, `Size`, `Area`, and/or `Fleet`, plus `Value` and
#'   `Variable`.
#' - Assignment forms return `x` with the named slot replaced by `value`.
#'
#' @example man-examples/catch_timeseries.R
#'
#' @name catch_timeseries
#' @export
Interactions <- function(object,
                         df      = TRUE,
                         byAge   = FALSE,
                         byArea  = FALSE,
                         byFleet = FALSE,
                         Reduce  = TRUE,
                         IncYear = FALSE) {
  .ExtractCatchTimeseries(object,
                           df        = df,
                           slot_name = 'Interactions',
                           byAge     = byAge,
                           bySize    = FALSE,
                           byArea    = byArea,
                           byFleet   = byFleet,
                           Reduce    = Reduce,
                           IncYear   = IncYear)
}

#' @rdname catch_timeseries
#' @export
Landings <- function(object,
                     df      = TRUE,
                     byAge   = FALSE,
                     bySize  = FALSE,
                     byArea  = FALSE,
                     byFleet = TRUE,
                     Reduce  = TRUE,
                     IncYear = FALSE) {

  if (inherits(object, 'obs'))
    return(object@Landings)

  if (byAge)  bySize <- FALSE
  if (bySize) byAge  <- FALSE

  if (inherits(object, 'data')) {
    if (!byAge && !bySize)
      return(object@Landings)
    return(.ExtractDataCompTimeseries(
      object, slot_name = if (byAge) 'LandingsAtAge' else 'LandingsAtSize',
      byFleet = byFleet
    ))
  }

  .ExtractCatchTimeseries(object,
                           df        = df,
                           slot_name = 'Landings',
                           byAge     = byAge,
                           bySize    = bySize,
                           byArea    = byArea,
                           byFleet   = byFleet,
                           Reduce    = Reduce,
                           IncYear   = IncYear)
}

#' @rdname catch_timeseries
#' @param x An [obs-class] or [data-class] object.
#' @param value A [catchobs-class] object (when `x` is [obs-class]) or a
#'   [catchdata-class] object (when `x` is [data-class]) to assign.
#' @export
`Landings<-` <- function(x, value) {
  .CheckClass(x, c('obs', 'data', 'hist', 'mse'), 'x')
  x@Landings <- value
  methods::validObject(x)
  x
}

#' @rdname catch_timeseries
#' @export
Discards <- function(object,
                     df      = TRUE,
                     byAge   = FALSE,
                     bySize  = FALSE,
                     byArea  = FALSE,
                     byFleet = TRUE,
                     Reduce  = TRUE,
                     IncYear = FALSE) {

  if (inherits(object, 'obs'))
    return(object@Discards)

  if (byAge)  bySize <- FALSE
  if (bySize) byAge  <- FALSE

  if (inherits(object, 'data')) {
    if (!byAge && !bySize)
      return(object@Discards)
    return(.ExtractDataCompTimeseries(
      object, slot_name = if (byAge) 'DiscardsAtAge' else 'DiscardsAtSize',
      byFleet = byFleet
    ))
  }

  .ExtractCatchTimeseries(object,
                           df        = df,
                           slot_name = 'Discards',
                           byAge     = byAge,
                           bySize    = bySize,
                           byArea    = byArea,
                           byFleet   = byFleet,
                           Reduce    = Reduce,
                           IncYear   = IncYear)
}


#' @rdname catch_timeseries
#' @export
`Discards<-` <- function(x, value) {
  .CheckClass(x, c('obs', 'data', 'hist', 'mse'), 'x')
  x@Discards <- value
  methods::validObject(x)
  x
}

#' @rdname catch_timeseries
#' @export
Removals <- function(object,
                     df      = TRUE,
                     byAge   = FALSE,
                     bySize  = FALSE,
                     byArea  = FALSE,
                     byFleet = TRUE,
                     Reduce  = TRUE,
                     IncYear = FALSE) {
  if (byAge)  bySize <- FALSE
  if (bySize) byAge  <- FALSE
  
  L <- .ExtractCatchTimeseries(object,
                           df        = df,
                           slot_name = 'Landings',
                           byAge     = byAge,
                           bySize    = bySize,
                           byArea    = byArea,
                           byFleet   = byFleet,
                           Reduce    = Reduce,
                           IncYear   = IncYear)
  
  D <- .ExtractCatchTimeseries(object,
                                df        = df,
                                slot_name = 'Discards',
                                byAge     = byAge,
                                bySize    = bySize,
                                byArea    = byArea,
                                byFleet   = byFleet,
                                Reduce    = Reduce,
                                IncYear   = IncYear)
  
  if (!df)
    return(ArraySum(D,L))
  
 R <- dplyr::bind_rows(L, D) 
 cnames <- colnames(R)
 cnames <- cnames[!cnames=='Variable']
 cnames <- cnames[!cnames=='Value']
 
 R |> dplyr::group_by(dplyr::across(dplyr::all_of(cnames))) |>
   dplyr::summarise(Value = sum(Value, na.rm=TRUE), .groups='drop') |>
   dplyr::mutate(Variable = 'Removals')
 
}

.ExtractCatchTimeseries <- function(object,
                                     slot_name = 'Interactions',
                                     df        = FALSE,
                                     byAge     = FALSE,
                                     bySize    = FALSE,
                                     byArea    = FALSE,
                                     byFleet   = FALSE,
                                     Reduce    = TRUE,
                                     IncYear   = FALSE) {
  .CheckClass(object, c('hist', 'mse'), 'object')
  
  if (!df)
    return(slot(object, slot_name))
  
  if (inherits(object, 'hist')) {
    return(
      .ExtractCatchTimeseriesCore(object,
                                OM        = object@OM,
                                slot_name = slot_name,
                                byAge     = byAge,
                                bySize    = bySize,
                                byArea    = byArea,
                                byFleet   = byFleet,
                                Reduce    = Reduce,
                                IncYear   = IncYear)
    )
  }
  
  # MSE object: bind historical + projection
  hist <- .ExtractCatchTimeseriesCore(object@Hist,
                                    OM        = object@OM,
                                    slot_name = slot_name,
                                    byAge     = byAge,
                                    bySize    = bySize,
                                    byArea    = byArea,
                                    byFleet   = byFleet,
                                    Reduce    = Reduce,
                                    IncYear   = IncYear) |>
    dplyr::mutate(MP = 'Historical')
  
  proj <- .ExtractCatchTimeseriesCore(object,
                                    OM        = object@OM,
                                    slot_name = slot_name,
                                    byAge     = byAge,
                                    bySize    = bySize,
                                    byArea    = byArea,
                                    byFleet   = byFleet,
                                    Reduce    = Reduce,
                                    IncYear   = IncYear)
  
  out <- dplyr::bind_rows(hist, proj)
  class(out) <- c(paste0(tolower(slot_name), '.df'), class(out))
  out
}

.ExtractCatchTimeseriesCore <- function(object,
                                      OM        = NULL,
                                      slot_name = 'Interactions',
                                      byAge     = FALSE,
                                      bySize    = FALSE,
                                      byArea    = FALSE,
                                      byFleet   = TRUE,
                                      Reduce    = TRUE,
                                      IncYear   = FALSE) {
  isMSE <- inherits(object, 'mse')
  
  if (byAge || byArea || bySize) {
    
    if (bySize) {
      # Size-structured
      arraySizeList <- switch(slot_name,
                              Landings = object@LandingsAtSize,
                              Discards = object@DiscardsAtSize
      )
      
      if (!byArea)
        arraySizeList <- purrr::map(arraySizeList, \(stock)
                                    purrr::map(stock, SumOverArea))

      if (!byFleet)
        arraySizeList <- purrr::imap(arraySizeList, \(fleetList, stock_name)
                                     stats::setNames(
                                       list(.SumFleetSizeArrays(fleetList, stock_name, slot_name)),
                                       'Total'
                                     ))
      df <- purrr::map(arraySizeList, \(stock)
                       purrr::map(stock, Array2DF) |> 
                         dplyr::bind_rows(.id = "Fleet")
                       ) |> 
        dplyr::bind_rows(.id = "Stock")
      
    } else {
      # Age-structured path 
      arrayAgeList <- switch(slot_name,
                             Interactions = object@InteractAtAge,
                             Landings     = object@LandingsAtAge,
                             Discards     = object@DiscardsAtAge
      )
      
      if (!byArea)
        arrayAgeList <- purrr::map(arrayAgeList, SumOverArea)
      
      if (!byFleet)
        arrayAgeList <- purrr::map(arrayAgeList, SumOverFleet)
      
      if (!byAge)
        arrayAgeList <- purrr::map(arrayAgeList, SumOverAge)
      
      if (Reduce) 
        arrayAgeList <- purrr::map(arrayAgeList, \(array)
                                   ReduceDims(array, IncYear = IncYear)
        )
      
      df <- purrr::map(arrayAgeList, Array2DF) |>
        dplyr::bind_rows(.id = "Stock")
    }
    
    return(
      df |>
        dplyr::mutate(Variable = slot_name,
                      Period   = ifelse(isMSE, 'Projection', 'Historical')) |>
        dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |>
        dplyr::arrange(Sim, Stock, Year)
    )
  }
  
  array <- slot(object, slot_name)
  if (!byFleet) array <- SumOverFleet(array)
  if (Reduce)   array <- ReduceDims(array, IncYear = IncYear)
  
  Array2DF(array) |>
    dplyr::mutate(Variable = slot_name,
                  Period   = ifelse(isMSE, 'Projection', 'Historical')) |>
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period')
}

# Sums a stock's per-fleet size-structured arrays (`Sim x Class x Year x
# Area`) together for `byFleet = FALSE`. Fleets are not required to share a
# size-class grid (see compdata-class), so this is only valid when every
# fleet in `fleetList` happens to share the exact same `Class` dimnames;
# otherwise summing would silently combine incompatible bins, so this
# raises a clear error instead.
.SumFleetSizeArrays <- function(fleetList, stock_name, slot_name) {
  classGrids <- purrr::map(fleetList, \(a) dimnames(a)$Class)
  ref        <- classGrids[[1]]
  matches    <- purrr::map_lgl(classGrids, identical, ref)

  if (!all(matches))
    cli::cli_abort(c(
      "Cannot sum {.field {slot_name}} across fleets for stock {.val {stock_name}} with `byFleet = FALSE`.",
      "x" = "Fleets do not share the same size-class grid.",
      "i" = "Set `byFleet = TRUE` to keep fleets separate instead."
    ), call = NULL)

  Reduce(`+`, fleetList)
}
