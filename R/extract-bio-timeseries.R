#' Extract Biological Time Series
#'
#' `Biomass()`, `SBiomass()`, and `SProduction()` extract total biomass,
#' spawning biomass, and spawning production, respectively, from a
#' [hist-class] or [mse-class] object.
#'
#' When applied to an [mse-class] object the historical and projection
#' periods are row-bound and labelled via the `Period` column; historical
#' rows carry `MP = "Historical"`.
#' 
#' @param object A [hist-class] or [mse-class] object.
#' @param df Logical. If `FALSE` the raw array slot is returned.
#'   If `TRUE` (default)  a tidy `data.frame` is returned.
#' @param byAge Logical. If `TRUE` the data frame retains the `Age`
#'   dimension. Values are derived by multiplying numbers-at-age by the
#'   relevant biological schedule before optionally summing over age.
#'   Ignored when `df = FALSE`.
#' @param byArea Logical. If `TRUE` the data frame retains the `Area`
#'   dimension. Ignored when `df = FALSE`.
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using  [ReduceDims()] before conversion to a data frame.
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the
#'   year dimension is retained during reduction. Default `FALSE`.
#'   
#' @return
#' * `df = FALSE` — the raw array slot (`Biomass`, `SBiomass`, or
#'   `SProduction`).
#' * `df = TRUE` — a tidy `data.frame` with columns `Sim`, `Stock`,
#'   `Year`, `Period`, `MP` (MSE only), and optionally `Age` / `Area`,
#'   plus `Value`, `Variable`, and `Units`.
#'
#' @example man-examples/bio_timeseries.R
#'
#' @name bio_timeseries
#' @seealso [Number()]
#' @export
Biomass <- function(object, 
                    df = TRUE,
                    byAge = FALSE, 
                    byArea = FALSE,
                    Reduce  = TRUE,
                    IncYear = FALSE
                    ) {
  
  .ExtractBioTimeseries(object,
                     df = df, 
                     slot_name = 'Biomass', 
                     byAge = byAge,
                     byArea = byArea,
                     Reduce  = Reduce,
                     IncYear = IncYear)
}


#' @rdname bio_timeseries
#' @export
SBiomass <- function(object, 
                     df = TRUE,
                     byAge = FALSE, 
                     byArea = FALSE,
                     Reduce  = TRUE,
                     IncYear = FALSE) {
  
  .ExtractBioTimeseries(object,
                     df = df, 
                     slot_name = 'SBiomass', 
                     byAge = byAge,
                     byArea = byArea,
                     Reduce  = Reduce,
                     IncYear = IncYear)
}

#' @rdname bio_timeseries
#' @export
SProduction <- function(object, 
                        df = TRUE,
                        byAge = FALSE, 
                        byArea = FALSE,
                        Reduce  = TRUE,
                        IncYear = FALSE) {
  
  .ExtractBioTimeseries(object,
                     df = df, 
                     slot_name = 'SProduction', 
                     byAge = byAge,
                     byArea = byArea,
                     Reduce  = Reduce,
                     IncYear = IncYear)
}

.GetAtAge <- function(OM, slot_name = 'Weight', byArea, isMSE, MP_Names) {
  purrr::map(OM@Stock, \(stock) {
    
    out <- slot(stock,slot_name)@MeanAtAge
    if (byArea)
      out <- AddDimension(out, 'Area', pos=4)
    
    if (isMSE)
      out <- AddDimension(out, 'MP', val = MP_Names)
    
    out
  })
}

.GetUnits <- function(OM, slot_name = 'Weight') {

  base_slot <- switch(slot_name,
    Biomass     = 'Weight',
    SBiomass    = 'Weight',
    SProduction = 'Fecundity'
  )

  # Combines each stock's base unit (Weight@Units / Fecundity@Units) with its
  # SRR@Units scaling factor - e.g. Weight@Units = "kg", SRR@Units = 1000
  # (R0 in thousands of fish) -> "t", since raw Biomass is already
  # Number(in SRR@Units scale) x WeightAtAge.
  labels <- purrr::map_chr(OM@Stock, \(stock) {
    info <- .CombineScaledUnit(.mass_units_g, slot(stock, base_slot)@Units, stock@SRR@Units)
    if (is.null(info)) NA_character_ else info$label
  })

  data.frame(Stock = names(labels), Units = unname(labels), stringsAsFactors = FALSE)
}



.ExtractBioTimeseries <- function(object, 
                                   slot_name = 'Biomass', 
                                   df = TRUE, 
                                   byAge = FALSE, 
                                   byArea = FALSE,
                                   Reduce  = TRUE,
                                   IncYear = FALSE) {
  
  .CheckClass(object, c('hist', 'mse'), 'object')
  
  if (!df)
    return(
      slot(object, slot_name)
    )
  
  if (inherits(object, 'hist')) {
    return(
      .ExtractBioTimeseriesCore(object, 
                              OM = object@OM,
                              slot_name = slot_name,
                              byAge = byAge,
                              byArea = byArea,
                              Reduce = Reduce,
                              IncYear = IncYear)
    )
  }
  
  # MSE object 
  hist <- .ExtractBioTimeseriesCore(object@Hist,
                                  OM = object@OM,
                                  slot_name = slot_name,
                                  byAge = byAge,
                                  byArea = byArea,
                                  Reduce = Reduce,
                                  IncYear = IncYear) |> 
    dplyr::mutate(MP = 'Historical')
  
  proj <- .ExtractBioTimeseriesCore(object,
                                  OM = object@OM,
                                  slot_name = slot_name,
                                  byAge = byAge,
                                  byArea = byArea,
                                  Reduce = Reduce,
                                  IncYear = IncYear)
  
  out <- dplyr::bind_rows(hist, proj) 
  
  class(out) <- c(paste0(tolower(slot_name), '.df'), class(out))
  out 
}


.ExtractBioTimeseriesCore <- function(object, 
                                    OM = NULL,
                                    slot_name = 'Biomass',
                                    byAge = FALSE, 
                                    byArea = FALSE,
                                    Reduce = TRUE,
                                    IncYear = FALSE) {
  
  isMSE <- inherits(object, 'mse')
  if (isMSE) {
    MP_Names <- names(object@MPs)
  } else {
    MP_Names <- NULL
  }
  units <- .GetUnits(OM, slot_name)

  if (byAge || byArea) {
    
    number <- Number(object)
    
    if (!byArea) 
      number <- purrr::map(number, SumOverArea)
    
    weight <- .GetAtAge(OM, 'Weight', byArea, isMSE, MP_Names)
    maturity <- .GetAtAge(OM, 'Maturity', byArea, isMSE, MP_Names)
    fecundity <- .GetAtAge(OM, 'Fecundity', byArea, isMSE, MP_Names)
    
    if (slot_name == 'Biomass') {
      arrayList <- purrr::map2(number, weight, ArrayMultiply) 
    } else if (slot_name == 'SBiomass') {
      arrayList <- purrr::map2(number, weight, ArrayMultiply) |>
        purrr::map2(maturity, ArrayMultiply)
    } else if (slot_name == 'SProduction') {
      arrayList <- purrr::map2(number, fecundity, ArrayMultiply)
    }
    
    if (Reduce)
      arrayList <- ReduceDims(arrayList, IncYear = IncYear)
    
    if (!byAge)
      arrayList <- purrr::map(arrayList, SumOverAge)
    
    return(
      purrr::map(arrayList, Array2DF) |>
        dplyr::bind_rows(.id = "Stock") |>
        dplyr::mutate(Variable = slot_name,
                      Period = ifelse(isMSE, 'Projection', 'Historical')) |>
        dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |>
        dplyr::left_join(units, by = 'Stock') |>
        .ConvertDF() |>
        dplyr::arrange(Sim, Stock, Year)
    )
  }
  
  array <- slot(object, slot_name)
  if (Reduce)
    array <- ReduceDims(array, IncYear = IncYear)
  
  Array2DF(array) |>
    dplyr::mutate(Variable = slot_name,
                  Period = ifelse(isMSE, 'Projection', 'Historical'),
                  Stock = as.character(Stock)) |>
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |>
    dplyr::left_join(units, by = 'Stock') |>
    dplyr::mutate(Stock = .MakeFactor(Stock))
}

# ---- Fleet ----


# # Fleet 
# Interactions(Hist)
# Interactions(Hist, df=TRUE)
# Interactions(Hist, df=TRUE, byFleet = TRUE)
# Interactions(Hist, df=TRUE, byFleet = TRUE, byAge = TRUE)
# Interactions(Hist, df=TRUE, byFleet = TRUE, byAge = TRUE, byArea=TRUE)
# 
# Interactions(MSE, df=TRUE) 
# Interactions(MSE, df=TRUE, byFleet = TRUE, byAge = TRUE, byArea=TRUE) 
# 
# 
# Landings(MSE, df=TRUE)
# Landings(MSE, df=TRUE, byFleet = TRUE, byAge = TRUE, byArea=TRUE) 
# Landings(MSE, df=TRUE, byFleet = TRUE, bySize = TRUE, byArea=TRUE) 
# Landings(Hist, df=TRUE, byFleet = TRUE, bySize = TRUE, byArea=TRUE) 

# Discards(MSE, df=TRUE)
# Discards(MSE, df=TRUE, byFleet = TRUE, byAge = TRUE, byArea=TRUE) 
# 





# Relative reference point functions (B_B0, SB_SB0, etc.) live in extract-relative.R








