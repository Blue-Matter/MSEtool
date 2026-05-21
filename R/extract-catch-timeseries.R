#' Extract Catch Time Series
#'
#' `Interactions()`, `Landings()`, `Discards()`, and `Removals` extract total
#' interactions (encounters), retained landings, discards, and removals 
#' (landings + discards) respectively, from a [hist-class] or [mse-class] object.
#' 
#' When `byAge = FALSE` or `bySize = FALSE`, the catch data are in units of 
#' biomass, i.e `N` x `Weight`, where `Weight` is the fleet-specific 
#' weight-at-age schedules provided in `WeightFleet` in the [Fleet()] object. 
#' 
#' When `byAge = TRUE` or `bySize = TRUE` the values are in units of 
#' numbers (abundance). 
#' 
#' When `byArea = TRUE` the values are always in units of numbers. 
#' 
#' Future versions of this function may allow users to specify the units (i.e `N`
#' or `B` for the catch data).
#' 
#' When applied to an [mse-class] object the historical and projection
#' periods are row-bound and labelled via the `Period` column; historical
#' rows carry `MP = "Historical"`.
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param df Logical. If `FALSE` the raw array slot is returned.
#'   If `TRUE`  (default) a tidy `data.frame` is returned.
#' @param byAge Logical. If `TRUE` the data frame retains the `Age`
#'   dimension. Otherwise values are summed over ages. 
#'   Mutually exclusive with `bySize`. Ignored when `df = FALSE`.
#' @param bySize Logical. If `TRUE` the data frame retains the `Size`
#'   (length-bin) dimension using the `*AtSize` slots. Otherwise values are
#'   summed over size classes. Mutually exclusive with `byAge`. 
#'   Not available for `Interactions()`. Ignored when `df = FALSE`.
#' @param byArea Logical. If `TRUE` the data frame retains the `Area`
#'   dimension. Otherwise values are summed over areas. Ignored when `df = FALSE`.
#' @param byFleet Logical. If `TRUE` (default for `Landings` and
#'   `Discards`) the data frame retains the `Fleet` dimension. Otherwise
#'   values are summed over fleets. Ignored when `df = FALSE`.
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using  [ReduceDims()] before conversion to a data frame.
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the
#'   year dimension is retained during reduction. Default `FALSE`.
#'
#' @return
#' * `df = FALSE` — the raw array slot (`Interactions`, `Landings`, or
#'   `Discards`).
#' * `df = TRUE` — a tidy `data.frame` with columns `Sim`, `Stock`,
#'   `Year`, `Period`, `MP` (MSE only), and optionally `Age`, `Size`,
#'   `Area`, and/or `Fleet`, plus `Value` and `Variable`.
#'
#' @examples
#' Hist <- Simulate(ExampleOM)
#' MSE <- Project(Hist, 'CurrentEffort')
#' 
#' # Raw arrays
#' Interactions(Hist, df = FALSE)
#' Landings(MSE, df = FALSE)
#'
#' # Tidy data frames — total across fleets, ages, areas
#' Interactions(Hist)
#' Landings(MSE)
#' Discards(MSE)
#' Removals(MSE)
#'
#' # Retain fleet, age, and area structure
#' Interactions(Hist, df = TRUE, byFleet = TRUE, byAge = TRUE, byArea = TRUE)
#' Landings(MSE, df = TRUE, byFleet = TRUE, byAge = TRUE,  byArea = TRUE)
#' Landings(MSE, df = TRUE, byFleet = TRUE, bySize = TRUE, byArea = TRUE)
#' Discards(MSE, df = TRUE, byFleet = TRUE, byAge = TRUE,  byArea = TRUE)
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
#' @export
Discards <- function(object,
                     df      = TRUE,
                     byAge   = FALSE,
                     bySize  = FALSE,
                     byArea  = FALSE,
                     byFleet = TRUE,
                     Reduce  = TRUE,
                     IncYear = FALSE) {
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

