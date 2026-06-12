#' Extract or Assign Catch Time Series
#'
#' `Interactions()`, `Landings()`, `Discards()`, and `Removals()` extract
#' total interactions (encounters), retained landings, discards, and removals
#' (landings + discards) respectively.
#'
#' When called on a [hist-class] or [mse-class] object, the functions return
#' simulated catch arrays or tidy data frames as described below.
#'
#' When called on an [obs-class] object, the functions return the
#' corresponding [catchobs-class] slot directly (the observation error
#' structure, not simulated values). When called on a [data-class] object,
#' they return the corresponding [catchdata-class] slot directly (the observed 
#' or simulated values). In both cases no data frame conversion is performed,
#' regardless of any `df` argument.
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
#'   objects only.
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
#' - For [data-class]: the [catchdata-class] object stored in the `Landings`
#'   or `Discards` slot.
#' - For [hist-class] or [mse-class] with `df = FALSE`: the raw array slot
#'   (`Interactions`, `Landings`, or `Discards`).
#' - For [hist-class] or [mse-class] with `df = TRUE`: a tidy `data.frame`
#'   with columns `Sim`, `Stock`, `Year`, `Period`, `MP` (MSE only), and
#'   optionally `Age`, `Size`, `Area`, and/or `Fleet`, plus `Value` and
#'   `Variable`.
#' - Assignment forms return `x` with the named slot replaced by `value`.
#'
#' @examples
#' Hist <- Simulate(SingleStockOM)
#' MSE <- Project(Hist, 'CurrentEffort')
#'
#' # Raw arrays from Hist / MSE
#' Interactions(Hist, df = FALSE)
#' Landings(MSE, df = FALSE)
#'
#' # Tidy data frames
#' Interactions(Hist)
#' Landings(MSE)
#' Discards(MSE)
#' Removals(MSE)
#'
#' # Retain fleet, age, and area structure
#' Landings(MSE, byFleet = TRUE, byAge = TRUE, byArea = TRUE)
#' Discards(MSE, byFleet = TRUE, bySize = TRUE)
#'
#' # Direct slot access for obs and data objects
#' obs <- Obs(Landings = CatchObs(CV = 0.2))
#' Landings(obs)
#' Landings(obs) <- CatchObs(CV = 0.3)
#'
#' dat <- Data(Landings = CatchData(Value = matrix(100, 1, 1)))
#' Landings(dat)
#' Landings(dat) <- CatchData()
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
  extract_catch_timeseries(object,
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
  
  if (inherits(object, c('obs', 'data')))
    return(object@Landings)
  
  if (byAge)  bySize <- FALSE
  if (bySize) byAge  <- FALSE
  
  extract_catch_timeseries(object,
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
  CheckClass(x, c('obs', 'data', 'hist', 'mse'), 'x')
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
  
  if (inherits(object, c('obs', 'data')))
    return(object@Discards)
  
  if (byAge)  bySize <- FALSE
  if (bySize) byAge  <- FALSE
  
  extract_catch_timeseries(object,
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
  CheckClass(x, c('obs', 'data', 'hist', 'mse'), 'x')
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
  
  L <- extract_catch_timeseries(object,
                           df        = df,
                           slot_name = 'Landings',
                           byAge     = byAge,
                           bySize    = bySize,
                           byArea    = byArea,
                           byFleet   = byFleet,
                           Reduce    = Reduce,
                           IncYear   = IncYear)
  
  D <- extract_catch_timeseries(object,
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

extract_catch_timeseries <- function(object,
                                     slot_name = 'Interactions',
                                     df        = FALSE,
                                     byAge     = FALSE,
                                     bySize    = FALSE,
                                     byArea    = FALSE,
                                     byFleet   = FALSE,
                                     Reduce    = TRUE,
                                     IncYear   = FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  
  if (!df)
    return(slot(object, slot_name))
  
  if (inherits(object, 'hist')) {
    return(
      .extract_catch_timeseries(object,
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
  hist <- .extract_catch_timeseries(object@Hist,
                                    OM        = object@OM,
                                    slot_name = slot_name,
                                    byAge     = byAge,
                                    bySize    = bySize,
                                    byArea    = byArea,
                                    byFleet   = byFleet,
                                    Reduce    = Reduce,
                                    IncYear   = IncYear) |>
    dplyr::mutate(MP = 'Historical')
  
  proj <- .extract_catch_timeseries(object,
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

.extract_catch_timeseries <- function(object,
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
        arraySizeList <- purrr::map(arraySizeList, \(stock)
                                    list(SumOverFleet(stock)))   
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
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |>
    dplyr::arrange(Sim, Stock, Year)
}

