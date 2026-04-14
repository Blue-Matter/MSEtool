#' Extract Numbers at Age
#'
#' @description
#' Extracts numbers-at-age from a [hist-class] or [mse-class] object. When `df = FALSE`
#' (the default), the raw array slot is returned. When `df = TRUE`, a tidy
#' `data.frame` of class `number` is returned, with optional aggregation over
#' ages and/or areas.
#'
#' When `df=TRUE`, both the historical and projection periods of an `mse` object 
#' are returned, with a `Period` column distinguishing them. Historical rows
#' will have `NA` in the `MP` column.
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param df `logical`. If `FALSE` (default) the raw list of arrays from the `Number` 
#' slot is returned. If `TRUE` a tidy `data.frame` is returned.
#' @param byAge `logical`. If `TRUE` results are not summed over age
#'   classes. Default `FALSE`.
#' @param byArea `logical`. If `TRUE` results are not summed over areas.
#'   Default `FALSE`.
#'
#' @return
#' - When `df = FALSE`: the raw `Number` list.
#' - When `df = TRUE`: a `data.frame` with columns:
#'   - `Sim` — simulation index
#'   - `Year` — calendar year
#'   - `Stock` — stock name
#'   - `Period` — `"Historical"` or `"Projection"`
#'   - `MP` — management procedure name (`NA` for historical rows; for `mse` objects only)
#'   - `Variable` — always `"Number"`
#'   - `Units` — units inherited from `Stock@SRR@Units`
#'   - `Age` — age class (only present when `byAge = TRUE`)
#'   - `Area` — area index (only present when `byArea = TRUE`)
#'   - `Value` — numbers at age (or summed over age/area when aggregated)
#'
#' @examples
#' \dontrun{
#' # Return raw slot
#' Number(hist_obj)
#'
#' # Tidy data.frame, summed over ages and areas
#' Number(hist_obj, df = TRUE)
#'
#' # Retain age structure, sum over areas
#' Number(hist_obj, df = TRUE, byAge = TRUE)
#'
#' # Full historical + projection output from an MSE run
#' Number(mse_obj, df = TRUE, byAge = TRUE, byArea = TRUE)
#' }
#'
#' @export
Number <- function(object, df = FALSE, byAge = FALSE, byArea = FALSE) {
  CheckClass(object, c('hist', 'mse'), 'object')
  
  if (!df)
    return(object@Number)
  
  if (inherits(object, 'hist')) {
    return(Number_Hist(object, df = df, byAge = byAge, byArea = byArea))
  }
  
  proj <- Number_MSE(object, df = df, byAge = byAge, byArea = byArea)
  
  if (!df) 
    return(proj)

  hist <- number_to_df(
    number_slot    = object@Hist@Number,
    OM             = object@OM,
    period         = "Historical",
    extra_group_vars = character(0),
    byAge          = byAge,
    byArea         = byArea
  )
  
  out <- dplyr::bind_rows(hist, proj) |>
    dplyr::arrange(Sim, Year, Stock, Period, MP)
  
  class(out) <- c("number", class(out))
  out
}


number_to_df <- function(number_slot, OM, period, extra_group_vars,
                          byAge, byArea) {
  Areas       <- seq_len(nArea(OM))
  years       <- Years(OM, period)
  n_stocks    <- length(number_slot)
  stock_names <- names(number_slot)
  
  group_vars <- c("Sim", "Year", extra_group_vars,
                  if (byAge)  "Age",
                  if (byArea) "Area")
  
  stock_list <- vector("list", n_stocks)
  
  for (i in seq_len(n_stocks)) {
    stock <- OM@Stock[[i]]
    
    n <- number_slot[[i]] |>
      ArraySubsetYear(years) |>
      Extend(
        nSim       = OM@nSim,
        AgeClasses = stock@Ages@Classes,
        Years      = years,
        Areas      = Areas
      ) |>
      array2DF() |>
      dplyr::mutate(
        Age  = as.numeric(Age),
        Area = as.numeric(Area)
      ) |>
      dplyr::arrange(Sim, Year, Age, Area) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(Value = sum(Value), .groups = "drop") |>
      dplyr::mutate(
        Stock    = stock_names[i],
        Period   = period,
        Variable = "Number",
        Units    = stock@SRR@Units
      )
    
    stock_list[[i]] <- n
  }
  
  out <- do.call(rbind, stock_list)
  ConvertDF(out)
}



Number_Hist <- function(Hist, df = FALSE, byAge = FALSE, byArea = FALSE) {
  
  if (!df) return(Hist@Number)
  
  out <- number_to_df(
    number_slot    = Hist@Number,
    OM             = Hist@OM,
    period         = "Historical",
    extra_group_vars = character(0),
    byAge          = byAge,
    byArea         = byArea
  )
  
  class(out) <- c("number", class(out))
  out
}


Number_MSE <- function(MSE, df = FALSE, byAge = FALSE, byArea = FALSE) {
  CheckClass(MSE, 'mse', 'MSE')
  
  out <- number_to_df(
    number_slot      = MSE@Number,
    OM               = MSE@OM,
    period           = "Projection",
    extra_group_vars = "MP",
    byAge            = byAge,
    byArea           = byArea
  )
  
  class(out) <- c("number", class(out))
  out
}
