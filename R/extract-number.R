#' Extract Number-at-Age time series
#'
#' Returns the number of individuals (abundance) from a [hist-class] or
#' [mse-class] object, either as a list of length [nStock()] or 
#' a `data.frame`.  
#' 
#' @param object A [hist-class] or [mse-class] object.
#' @param df Logical. If `FALSE` (default) the raw array slot is returned.
#'   If `TRUE` a tidy `data.frame` is returned.
#' @param byAge Logical. If `TRUE` the data frame retains the `Age`
#'   dimension rather than summing over ages. Ignored when `df = FALSE`.
#' @param byArea Logical. If `TRUE` the data frame retains the `Area`
#'   dimension rather than summing over areas. Ignored when `df = FALSE`.
#'
#' @return
#' * `df = FALSE` — the raw `Number` array slot (list of arrays, one per
#'   stock).
#' * `df = TRUE` — a `data.frame` (subclass `"number.df"`) with columns
#'   `Sim`, `Stock`, `Year`, `Period`, `MP` (MSE only), and optionally
#'   `Age` / `Area`, plus `Value`, `Variable`, and `Units`.
#'
#' @export
#' @seealso [Biomass()], [SBiomass()], [SProduction()]
#' @examples
#' Hist <- Simulate(ExampleOM)
#' Number(Hist, df = TRUE)
#' Number(Hist, df = TRUE, byAge = TRUE)
#' Number(Hist, df = TRUE, byAge = TRUE, byArea = TRUE)
Number <- function(object, df = FALSE, byAge = FALSE, byArea = FALSE) {
  CheckClass(object, c('hist', 'mse', 'timeseries'), 'object')
  
  if (!df)
    return(object@Number)
  
  if (inherits(object, 'hist'))
    return(extract_number_hist(object, df = df, byAge = byAge, byArea = byArea))
  
  proj <- extract_number_proj(object, df = df, byAge = byAge, byArea = byArea)
  
  hist <- number_to_df(
    number_slot      = object@Hist@Number,
    OM               = object@OM,
    period           = "Historical",
    extra_group_vars = character(0),
    byAge            = byAge,
    byArea           = byArea
  ) |>
    dplyr::mutate(MP = 'Historical')
  
  out <- dplyr::bind_rows(hist, proj)
  class(out) <- c("number.df", class(out))
  out
}

extract_number_hist <- function(Hist, df = FALSE, byAge = FALSE, byArea = FALSE) {
  
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

extract_number_proj <- function(MSE, df = FALSE, byAge = FALSE, byArea = FALSE) {
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
  
  do.call(rbind, stock_list) |> 
    ConvertDF() |> 
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |>
    dplyr::arrange(Sim, Stock, Year)
  
}

