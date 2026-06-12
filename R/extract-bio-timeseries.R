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
#' @examples
#' Hist <- Simulate(SingleStockOM)
#' MSE <- Project(Hist, 'CurrentEffort')
#' 
#' # Raw array
#' Biomass(Hist)
#' Biomass(MSE)
#'
#' # Tidy data frames
#' Biomass(Hist, df = TRUE)
#' Biomass(MSE,  df = TRUE)
#'
#' # Retain age and area structure
#' Biomass(MSE, df = TRUE, byAge = TRUE)
#' Biomass(MSE, df = TRUE, byArea = TRUE)
#' Biomass(MSE, df = TRUE, byAge = TRUE, byArea = TRUE)
#'
#' # Spawning biomass and production follow the same structure
#' SBiomass(MSE, df = TRUE, byAge = TRUE)
#' SProduction(MSE, df = TRUE, byArea = TRUE)
#' 
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
  
  extract_bio_timeseries(object,
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
  
  extract_bio_timeseries(object,
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
  
  extract_bio_timeseries(object,
                     df = df, 
                     slot_name = 'SProduction', 
                     byAge = byAge,
                     byArea = byArea,
                     Reduce  = Reduce,
                     IncYear = IncYear)
}

.get_at_age <- function(OM, slot_name = 'Weight', byArea, isMSE) {
  purrr::map(OM@Stock, \(stock) {
    
    out <- slot(stock,slot_name)@MeanAtAge
    if (byArea)
      out <- AddDimension(out, 'Area', pos=4)
    
    if (isMSE)
      out <- AddDimension(out, 'MP')
    
    out
  })
}

.get_units <- function(OM, slot_name = 'Weight') {
  
  # number_units <- purrr::map(OM@Stock, \(stock) {
  #   stock@SRR@Units
  # }) |> List2Array('Stock') |> DropDimension('Sim') |> Array2DF() |>
  #   dplyr::rename(NumberUnits=Value)
  
  if (slot_name == 'Biomass') {
    unit_slot_name <- 'Weight'
  } else if (slot_name == 'SBiomass') {
    unit_slot_name <- 'Weight'
  } else if (slot_name == 'SProduction') {
    unit_slot_name <- 'Fecundity'
  }
  
  units <- purrr::map(OM@Stock, \(stock) slot(stock, unit_slot_name)@Units) |> 
    List2Array('Stock') |> DropDimension('Sim', warn=FALSE) |> Array2DF() |>
    dplyr::rename(Units=Value)
  
  # units <- left_join(units, number_units, by = dplyr::join_by(Stock))
  units
  
}



extract_bio_timeseries <- function(object, 
                                   slot_name = 'Biomass', 
                                   df = TRUE, 
                                   byAge = FALSE, 
                                   byArea = FALSE,
                                   Reduce  = TRUE,
                                   IncYear = FALSE) {
  
  CheckClass(object, c('hist', 'mse'), 'object')
  
  if (!df)
    return(
      slot(object, slot_name)
    )
  
  if (inherits(object, 'hist')) {
    return(
      .extract_bio_timeseries(object, 
                              OM = object@OM,
                              slot_name = slot_name,
                              byAge = byAge,
                              byArea = byArea,
                              Reduce = Reduce,
                              IncYear = IncYear)
    )
  }
  
  # MSE object 
  hist <- .extract_bio_timeseries(object@Hist,
                                  OM = object@OM,
                                  slot_name = slot_name,
                                  byAge = byAge,
                                  byArea = byArea,
                                  Reduce = Reduce,
                                  IncYear = IncYear) |> 
    dplyr::mutate(MP = 'Historical')
  
  proj <- .extract_bio_timeseries(object,
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


.extract_bio_timeseries <- function(object, 
                                    OM = NULL,
                                    slot_name = 'Biomass',
                                    byAge = FALSE, 
                                    byArea = FALSE,
                                    Reduce = TRUE,
                                    IncYear = FALSE) {
  
  isMSE <- inherits(object, 'mse')
  # units <- .get_units(OM, slot_name)
  
  if (byAge || byArea) {
    
    number <- Number(object)
    
    if (!byArea) 
      number <- purrr::map(number, SumOverArea)
    
    weight <- .get_at_age(OM, 'Weight', byArea, isMSE)
    maturity <- .get_at_age(OM, 'Maturity', byArea, isMSE)
    fecundity <- .get_at_age(OM, 'Fecundity', byArea, isMSE)
    
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
       #  dplyr::left_join(units, by='Stock') |>
        ConvertDF() |>
        dplyr::arrange(Sim, Stock, Year) 
      
      
    )
  } 
  
  array <- slot(object, slot_name)
  if (Reduce)
    array <- ReduceDims(array, IncYear = IncYear)
  
  Array2DF(array) |>
    dplyr::mutate(Variable = slot_name,
                  Period = ifelse(isMSE, 'Projection', 'Historical')) |>
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |> 
    dplyr::arrange(Sim, Stock, Year)
  # |> 
   # dplyr::left_join(units, by='Stock')
  
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





# ---- Relative Values ----
# 
# B_B0
# 
# B_BMSY
# 
# SB_SB0
# 
# SB_SBMSY
# 
# SP_SP0
# 
# SP_SPMSY 










