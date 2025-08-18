
#' Get Values from OM, Hist, and MSE Objects
#' 
#' Returned as a data frame
#' @name Get
#' 
NULL

# ---- Get At-Age/At-Length Schedules ----

GetSchedule <- function(OM, Variable='Length', Slot='MeanAtAge', df=TRUE) {
  CheckClass(OM, c('om', 'hist', 'mse'))
  
  isStock <- Variable %in% slotNames('stock')
  isFleet <- Variable %in% slotNames('fleet')
  
  if (!isStock & !isFleet)
    cli::cli_abort('{.val {Variable}} is not a slot in `stock` or `fleet` class objects')
  
  if (inherits(OM, 'om')) {
    Array <- GetScheduleOM(OM, Variable, Slot, isStock)
  } else {
    Array <- GetScheduleHist(OM, Variable, Slot, isStock)
  }
  if (!df)
    return(Array)
  
  DF <- Array |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable=Variable)
  
  ColOrder <- c('Sim', 'Stock', 'Age', 'Class', 'TimeStep', 'Fleet', 'Value', 'Variable')
  ColOrder <- ColOrder[ColOrder %in% colnames(DF)]
  
  DF <- DF |> dplyr::select(dplyr::all_of(ColOrder))
  class(DF) <- c('Schedule', class(DF))
  DF
  
}

GetAtLength <- function(OM, Variable, df=TRUE) {
  GetSchedule(OM, Variable, Slot='MeanAtLength', df=TRUE)
}

GetAtAge <- function(OM, slotName, df=TRUE) {
  GetSchedule(OM, Variable, Slot='MeanAtAge', df=TRUE)
}

GetScheduleOM <- function(OM, slotName, Slot, isStock) {
  CheckClass(OM, c('om', 'hist', 'mse'))
  OM <- PopulateOM(OM, silent=TRUE)
  if (isStock)
    return(
      purrr::map(OM@Stock, \(Stock)
                 Stock |> slot(slotName) |> slot(Slot) |>
                   ArrayReduceDims()) |>
        List2Array("Stock")
    )
  purrr::map(OM@Fleet, \(Stock) {
    purrr::map(Stock, \(Fleet) Fleet |> slot(slotName) |> slot(Slot) |>
                 ArrayReduceDims()) |>
      List2Array("Fleet")
  }) |> List2Array("Stock")
}

GetScheduleHist <- function(Hist, slotName, Slot,isStock) {
  CheckClass(Hist, c('hist', 'mse'))
  OM <- Hist@OM
  if (isStock)
    return(
      purrr::map(OM@Stock, \(Stock)
                 Stock |> slot(slotName) |> slot(Slot) |>
                   ArrayReduceDims())|>
        List2Array("Stock")
    )
  
  purrr::map(OM@Fleet, \(Stock)
             Stock |> slot(slotName) |> slot(Slot) |>
               ArrayReduceDims())|>
    List2Array("Stock")
}

#' @rdname Get
#' @export
GetNaturalMortalityAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "NaturalMortality", df=df)
}

#' @rdname Get
#' @export
GetLengthAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Length", df=df)
}

#' @rdname Get
#' @export
GetWeightAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Weight", df=df)
}

#' @rdname Get
#' @export
GetMaturityAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Maturity", df=df)
}

#' @rdname Get
#' @export
GetFecundityAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Fecundity", df=df)
}


#' @rdname Get
#' @export
GetSelectivityAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Selectivity", df=df)
}

#' @rdname Get
#' @export
GetRetentionAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Retention", df=df)
}

#' @rdname Get
#' @export
GetSelectivityAtLength <- function(OM, df=TRUE) {
  GetSchedule(OM, "Selectivity", 'MeanAtLength', df=df)
}

#' @rdname Get
#' @export
GetRetentionAtLength <- function(OM, df=TRUE) {
  GetSchedule(OM, "Retention", 'MeanAtLength', df=df)
}

