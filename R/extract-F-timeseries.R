#' Extract Fishing Mortality Time Series
#'
#' `FInteract()`, `FDead()`, and `FRetain()` extract fishing mortality for
#' interactions (encounters), killed, and retained fish
#' respectively, from a [hist-class] or [mse-class] object.
#' 
#' When `byAge = FALSE` the apical (maximum over ages) fishing mortality
#' rate is returned. When `byAge = TRUE` the full age-structured array is
#' retained; `byArea` is always included when `byAge = TRUE`. 
#' 
#' When applied to an [mse-class] object the historical and projection
#' periods are row-bound and labelled via the `Period` column; historical
#' rows carry `MP = "Historical"`.
#' 
#' @param object A [hist-class] or [mse-class] object.
#' @param df Logical. If `FALSE` the raw array slot is returned.
#'   If `TRUE` (default)  a tidy `data.frame` is returned.
#'   
#' @param byAge Logical. If `TRUE` the data frame retains the `Age`
#'   dimension (sourced from the `*Area` slots, which always include
#'   `Area`). If `FALSE` the apical fishing mortality — the maximum over
#'   ages — is returned. Ignored when `df = FALSE`.
#'    
#' @param byArea Logical. If `TRUE` the `Area` dimension is retained.
#'   When `byAge = TRUE` area is always included regardless of this
#'   argument. Ignored when `df = FALSE`.
#' 
#' @param byFleet Logical. If `TRUE` the `Fleet` dimension is retained.
#'   If `FALSE` values are summed over fleets before the apical or
#'   age-structured extraction. Ignored when `df = FALSE`.
#'   
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using  [ReduceDims()] before conversion to a data frame.
#'   
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the
#'   year dimension is retained during reduction. Default `FALSE`.
#'
#' @return
#' * `df = FALSE` — the raw array slot (`FInteract`, `FDead`, or
#'   `FRetain`).
#' * `df = TRUE` — a tidy `data.frame` with columns `Sim`, `Stock`,
#'   `Year`, `Period`, `MP` (MSE only), and optionally `Age`, `Area`,
#'   and/or `Fleet`, plus `Value` and `Variable`.
#'
#' @examples
#' Hist <- Simulate(ExampleOM)
#' MSE <- Project(Hist, 'CurrentEffort')
#' 
#' # Raw array slots
#' FInteract(Hist)
#' FDead(Hist)
#' FRetain(MSE)
#'
#' # Age-structured F (area always included)
#' FInteract(Hist, df = TRUE)
#' FDead(MSE, df = TRUE)
#' FRetain(MSE, df = TRUE)
#'
#' # Retain fleet dimension
#' FDead(MSE, df = TRUE, byFleet = TRUE)
#'
#' # Apical F (max over ages) as a tidy data frame
#' FInteract(Hist, df = TRUE, byAge = FALSE)
#' FDead(MSE,      df = TRUE, byAge = FALSE)
#' FRetain(MSE,    df = TRUE, byAge = FALSE)
#'
#' # Age + Fleet
#' FDead(MSE, df = TRUE, byFleet = TRUE)
#'
#' # Area only (apical F per area)
#' FDead(MSE, df = TRUE, byAge = FALSE, byArea = TRUE)
#' FDead(MSE, df = TRUE, byAge = FALSE, byArea = TRUE, byFleet = TRUE)
#' 
#' @rdname F_timeseries
#' @export
FInteract <- function(object,
                      df = TRUE,
                      byAge = TRUE,
                      byArea = FALSE,
                      byFleet = FALSE,
                      Reduce  = TRUE,
                      IncYear = FALSE) {
  
  extract_F_timeseries(object,
                       df        = df,
                       slot_name = 'FInteract',
                       byAge     = byAge,
                       byArea    = byArea,
                       byFleet   = byFleet,
                       Reduce    = Reduce,
                       IncYear   = IncYear)
}

#' @name F_timeseries
#' @export
FDead <- function(object,
                  df = TRUE,
                  byAge = TRUE,
                  byArea = FALSE,
                  byFleet = FALSE,
                  Reduce  = TRUE,
                  IncYear = FALSE) {
  
  extract_F_timeseries(object,
                       df        = df,
                       slot_name = 'FDead',
                       byAge     = byAge,
                       byArea    = byArea,
                       byFleet   = byFleet,
                       Reduce    = Reduce,
                       IncYear   = IncYear)
}

#' @name F_timeseries
#' @export
FRetain <- function(object,
                    df = TRUE,
                    byAge = TRUE,
                    byArea = FALSE,
                    byFleet = FALSE,
                    Reduce  = TRUE,
                    IncYear = FALSE) {
  
  extract_F_timeseries(object,
                       df        = df,
                       slot_name = 'FRetain',
                       byAge     = byAge,
                       byArea    = byArea,
                       byFleet   = byFleet,
                       Reduce    = Reduce,
                       IncYear   = IncYear)
}

extract_F_timeseries <- function(object,
                                 slot_name = 'FDead',
                                 df        = FALSE,
                                 byAge     = FALSE,
                                 byArea    = FALSE,
                                 byFleet   = FALSE,
                                 Reduce    = TRUE,
                                 IncYear   = FALSE) {
  
  CheckClass(object, c('hist', 'mse'), 'object')
  
  if (!df)
    return(slot(object, slot_name))
  
  if (inherits(object, 'hist')) {
    return(
      .extract_F_timeseries(object,
                            OM        = object@OM,
                            slot_name = slot_name,
                            byAge     = byAge,
                            byArea    = byArea,
                            byFleet   = byFleet,
                            Reduce    = Reduce,
                            IncYear   = IncYear)
    )
  }
  
  # MSE object: bind historical + projection
  hist <- .extract_F_timeseries(object@Hist,
                                OM        = object@OM,
                                slot_name = slot_name,
                                byAge     = byAge,
                                byArea    = byArea,
                                byFleet   = byFleet,
                                Reduce    = Reduce,
                                IncYear   = IncYear) |>
    dplyr::mutate(MP = 'Historical')
  
  proj <- .extract_F_timeseries(object,
                                OM        = object@OM,
                                slot_name = slot_name,
                                byAge     = byAge,
                                byArea    = byArea,
                                byFleet   = byFleet,
                                Reduce    = Reduce,
                                IncYear   = IncYear)
  
  out <- dplyr::bind_rows(hist, proj)
  class(out) <- c(paste0(tolower(slot_name), '.df'), class(out))
  out
}

.extract_F_timeseries <- function(object,
                                  OM        = NULL,
                                  slot_name = 'FDead',
                                  byAge     = FALSE,
                                  byArea    = FALSE,
                                  byFleet   = TRUE,
                                  Reduce    = TRUE,
                                  IncYear   = FALSE) {
  isMSE <- inherits(object, 'mse')
  
  if (byArea || byAge) {
    
    arrayAgeList <- switch(slot_name,
                           FInteract = object@FInteractArea,
                           FDead     = object@FDeadArea,
                           FRetain   = object@FRetainArea
    )
    
    if (!byFleet)
      arrayAgeList <- purrr::map(arrayAgeList, SumOverFleet)
    
    if (!byAge)
      arrayAgeList <- purrr::map(arrayAgeList, \(array) {
        dn <- dimnames(array)
        apply(array, names(dn)[names(dn) !='Age'], max)
      })
    
    if (Reduce) 
      arrayAgeList <- purrr::map(arrayAgeList, \(array)
                                 ReduceDims(array, IncYear = IncYear)
      )
    
    df <- purrr::map(arrayAgeList, Array2DF) |>
      dplyr::bind_rows(.id = "Stock")
    
    
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

