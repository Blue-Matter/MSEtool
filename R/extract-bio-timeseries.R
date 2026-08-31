#' Extract Biological Time Series
#'
#' `Biomass()`, `SBiomass()`, and `SProduction()` extract total biomass,
#' spawning biomass, and spawning production, respectively, from a
#' [hist-class] or [mse-class] object. `VBiomass()` extracts vulnerable
#' biomass (biomass weighted by fleet selectivity/retention).
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
#'   Ignored when `df = FALSE`. Not applicable to `VBiomass()`.
#' @param byArea Logical. If `TRUE` the data frame retains the `Area`
#'   dimension. Ignored when `df = FALSE`. Not applicable to `VBiomass()`.
#' @param byFleet Logical. `VBiomass()` only. If `TRUE` (default) the data
#'   frame retains the `Fleet` dimension. Otherwise values are summed over
#'   fleets.
#' @param type Character. `VBiomass()` only. `"Removals"` (default) weights
#'   biomass by `Selectivity x (Retention + (1 - Retention) x
#'   DiscardMortality)` (i.e. fish that die from the encounter, whether
#'   landed or discarded). `"Landings"` weights by `Selectivity x Retention`
#'   only.
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using  [ReduceDims()] before conversion to a data frame.
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the
#'   year dimension is retained during reduction. Default `FALSE`.
#' @param Extend Logical. If `TRUE`, rows sharing one value across every
#'   simulation (e.g. the historical period, or any deterministic quantity --
#'   see `Reduce`/[ReduceDims()]) are broadcast to all `nSim` simulations via
#'   [ExtendSims()], rather than appearing once with `Sim = 1`. Default
#'   `FALSE`. Ignored when `df = FALSE`.
#' @param silent Logical. If `FALSE` and `Extend = FALSE`, emits a message
#'   when the returned data frame has rows sharing one value across every
#'   simulation, pointing at `Extend = TRUE`. Default `TRUE`. Ignored when
#'   `df = FALSE`.
#'
#' @return
#' * `df = FALSE` — the raw array slot (`Biomass`, `SBiomass`, or
#'   `SProduction`); for `VBiomass()`, for [hist-class] a numeric array `Sim x
#'   Stock x Fleet x Year`, or for [mse-class] a named list (`Historical`
#'   plus one element per MP) of such arrays.
#' * `df = TRUE` — a tidy `data.frame` with columns `Sim`, `Stock`,
#'   `Year`, `Period`, `MP` (MSE only), and optionally `Age` / `Area`
#'   (`Fleet` for `VBiomass()`), plus `Value`, `Variable`, and `Units`
#'   (`VBiomass()` omits `Units`).
#'
#' @example man-examples/bio_timeseries.R
#'
#' @name bio_timeseries
#' @seealso [Number()], [ExtendSims()]
#' @export
Biomass <- function(object,
                    df = TRUE,
                    byAge = FALSE,
                    byArea = FALSE,
                    Reduce  = TRUE,
                    IncYear = FALSE,
                    Extend  = FALSE,
                    silent  = TRUE
                    ) {

  .ExtractBioTimeseries(object,
                     df = df,
                     slot_name = 'Biomass',
                     byAge = byAge,
                     byArea = byArea,
                     Reduce  = Reduce,
                     IncYear = IncYear,
                     Extend  = Extend,
                     silent  = silent)
}


#' @rdname bio_timeseries
#' @export
SBiomass <- function(object,
                     df = TRUE,
                     byAge = FALSE,
                     byArea = FALSE,
                     Reduce  = TRUE,
                     IncYear = FALSE,
                     Extend  = FALSE,
                     silent  = TRUE) {

  .ExtractBioTimeseries(object,
                     df = df,
                     slot_name = 'SBiomass',
                     byAge = byAge,
                     byArea = byArea,
                     Reduce  = Reduce,
                     IncYear = IncYear,
                     Extend  = Extend,
                     silent  = silent)
}

#' @rdname bio_timeseries
#' @export
SProduction <- function(object,
                        df = TRUE,
                        byAge = FALSE,
                        byArea = FALSE,
                        Reduce  = TRUE,
                        IncYear = FALSE,
                        Extend  = FALSE,
                        silent  = TRUE) {

  .ExtractBioTimeseries(object,
                     df = df,
                     slot_name = 'SProduction',
                     byAge = byAge,
                     byArea = byArea,
                     Reduce  = Reduce,
                     IncYear = IncYear,
                     Extend  = Extend,
                     silent  = silent)
}

#' @rdname bio_timeseries
#' @export
VBiomass <- function(object,
                     df      = TRUE,
                     byFleet = TRUE,
                     type    = c('Removals', 'Landings'),
                     Reduce  = TRUE,
                     IncYear = FALSE,
                     Extend  = FALSE,
                     silent  = TRUE) {
  type <- match.arg(type)
  .CheckClass(object, c('hist', 'mse'), 'object')

  if (inherits(object, 'hist')) {
    arr <- .CalcVBiomass(object, object@OM, Years = Years(object, 'Historical'), type = type)
    if (!byFleet) arr <- SumOverFleet(arr)
    if (!df) return(arr)
    if (Reduce) arr <- ReduceDims(arr, IncYear = IncYear)
    out <- .VbiomassArrayToDf(arr) |> dplyr::mutate(Period = 'Historical')
    out <- .FinalizeTimeseriesDF(out, object@OM@nSim, Extend, silent)
    class(out) <- c('vbiomass.df', class(out))
    return(out)
  }

  HistYears <- Years(object, 'Historical')
  ProjYears <- Years(object, 'Projection')
  mpNames   <- names(object@MPs)

  histArr  <- .CalcVBiomass(object@Hist, object@OM, Years = HistYears, type = type)
  projArrs <- purrr::map(mpNames, \(mp)
    .CalcVBiomass(object, object@OM, Years = ProjYears, type = type, MPName = mp)
  ) |> stats::setNames(mpNames)

  if (!byFleet) {
    histArr  <- SumOverFleet(histArr)
    projArrs <- purrr::map(projArrs, SumOverFleet)
  }

  if (!df)
    return(c(list(Historical = histArr), projArrs))

  if (Reduce) {
    histArr  <- ReduceDims(histArr, IncYear = IncYear)
    projArrs <- purrr::map(projArrs, ReduceDims, IncYear = IncYear)
  }

  histDF <- .VbiomassArrayToDf(histArr) |>
    dplyr::mutate(Period = 'Historical', MP = 'Historical')
  projDF <- purrr::imap(projArrs, \(arr, mp)
    .VbiomassArrayToDf(arr) |> dplyr::mutate(Period = 'Projection', MP = mp)
  ) |> dplyr::bind_rows()

  out <- dplyr::bind_rows(histDF, projDF) |>
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period')
  out <- .FinalizeTimeseriesDF(out, object@OM@nSim, Extend, silent)
  class(out) <- c('vbiomass.df', class(out))
  out
}

.VbiomassArrayToDf <- function(arr) {
  Array2DF(arr) |>
    dplyr::mutate(Variable = 'VBiomass')
}


.EffectiveGearCurve <- function(object, OM, what, stock, fleet, MPName, Years, x = 'Age') {
  schedObj <- slot(OM@Fleet[[stock]][[fleet]], what)
  base     <- schedObj@MeanAtAge |> .SubsetYear(Years)

  overridden <- FALSE
  if (inherits(object, 'mse') && !is.null(MPName)) {
    override <- object@Misc[[what]][[MPName]][[stock]][[fleet]]$MeanAtAge
    if (!is.null(override)) {
      ArrayFill(base) <- override
      overridden <- TRUE
    }
  }

  if (identical(x, 'Age'))
    return(base)

  schedObj@MeanAtAge <- base
  slotName <- if (identical(x, 'Weight')) 'MeanAtWeight' else 'MeanAtLength'
  sizeObj  <- if (identical(x, 'Weight')) OM@Stock[[stock]]@Weight else OM@Stock[[stock]]@Length

  if (overridden) slot(schedObj, slotName) <- NULL

  fn  <- if (identical(x, 'Weight')) .MeanAtAge2MeanAtWeight else .MeanAtAge2MeanAtLength
  schedObj <- fn(schedObj, sizeObj, Years = Years)
  out <- slot(schedObj, slotName)
  names(dimnames(out))[names(dimnames(out)) == 'Class'] <- 'Age'
  out
}


.CalcVBiomass <- function(object, OM, Years, type = c('Removals', 'Landings'), MPName = NULL) {
  type  <- match.arg(type)
  isMSE <- inherits(object, 'mse')
  if (isMSE && is.null(MPName))
    cli::cli_abort("`MPName` is required when `object` is an `mse` object.", .internal = TRUE)

  fleet_names <- FleetNames(OM)
  n_fleet     <- length(fleet_names)
  n_sim       <- nSim(OM)
  n_area      <- nArea(OM)
  Complexes   <- OM@Complexes
  Allocation  <- OM@Allocation
  n_complex   <- length(Complexes)
  n_year      <- length(Years)

  NumberAtAge <- purrr::map(object@Number, \(stock) {
    arr <- if (isMSE) .SubsetMP(stock, MPs = MPName) |> DropDimension('MP') else stock
    .SubsetYear(arr, Years)
  })

  eff_sel_fn <- if (type == 'Landings') {
    function(sel, ret, dm) ArrayMultiply(sel, ret)
  } else {
    function(sel, ret, dm) ArrayMultiply(sel, ArraySum(ret, ArrayMultiply(1 - ret, dm)))
  }

  out <- array(NA_real_, dim = c(n_sim, n_complex, n_fleet, n_year),
              dimnames = list(Sim = seq_len(n_sim), Stock = names(Complexes),
                             Fleet = fleet_names, Year = Years))

  for (i in seq_len(n_complex)) {
    stocks <- Complexes[[i]]
    alloc  <- Allocation[[i]]
    if (is.null(alloc))
      alloc <- matrix(1, nrow = n_sim, ncol = n_fleet)

    stock_vb <- array(NA_real_, dim = c(n_sim, length(stocks), n_fleet, n_year),
                      dimnames = list(Sim = seq_len(n_sim), Stock = seq_along(stocks),
                                     Fleet = fleet_names, Year = Years))

    for (si in seq_along(stocks)) {
      st   <- stocks[si]
      n_st <- NumberAtAge[[st]]

      for (fl in seq_len(n_fleet)) {
        w_fl <- OM@Stock[[st]]@Weight@MeanAtAge |> .SubsetYear(Years) |>
          AddDimension('Area', pos = 4) |> ExtendAreas(Areas = seq_len(n_area))

        sel <- .EffectiveGearCurve(object, OM, 'Selectivity',      st, fl, MPName, Years)
        ret <- .EffectiveGearCurve(object, OM, 'Retention',        st, fl, MPName, Years)
        dm  <- .EffectiveGearCurve(object, OM, 'DiscardMortality', st, fl, MPName, Years)
        eff_sel <- eff_sel_fn(sel, ret, dm)

        b_fl <- ArrayMultiply(n_st, w_fl)
        vb   <- SumOverAge(ArrayMultiply(b_fl, eff_sel)) |> SumOverArea()

        stock_vb[, si, fl, ] <- vb * alloc[, fl]
      }
    }
    out[, i, , ] <- SumOverStock(stock_vb)
  }
  out
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
                                   IncYear = FALSE,
                                   Extend  = FALSE,
                                   silent  = TRUE) {

  .CheckClass(object, c('hist', 'mse'), 'object')

  if (!df)
    return(
      slot(object, slot_name)
    )

  if (inherits(object, 'hist')) {
    out <- .ExtractBioTimeseriesCore(object,
                              OM = object@OM,
                              slot_name = slot_name,
                              byAge = byAge,
                              byArea = byArea,
                              Reduce = Reduce,
                              IncYear = IncYear)
    return(.FinalizeTimeseriesDF(out, object@OM@nSim, Extend, silent))
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
  out <- .FinalizeTimeseriesDF(out, object@OM@nSim, Extend, silent)

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
    
    number <- Number(object, df = FALSE)

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








