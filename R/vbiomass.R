#' Vulnerable Biomass
#'
#' `VBiomass()` returns vulnerable biomass as a tidy
#' `data.frame` or raw array, for a [hist-class] or [mse-class] object.
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param df Logical. If `FALSE` the raw array is returned. If `TRUE`
#'   (default) a tidy `data.frame` is returned.
#' @param byFleet Logical. If `TRUE` (default) the data frame retains the
#'   `Fleet` dimension. Otherwise values are summed over fleets.
#' @param type Character. `"Removals"` (default) weights biomass by
#'   `Selectivity x (Retention + (1 - Retention) x DiscardMortality)` (i.e.
#'   fish that die from the encounter, whether landed or discarded).
#'   `"Landings"` weights by `Selectivity x Retention` only.
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using [ReduceDims()] before conversion to a data frame.
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the
#'   year dimension is retained during reduction. Default `FALSE`.
#'
#'
#' @return
#' * `df = FALSE`: for [hist-class], a numeric array `Sim x Stock x Fleet x
#'   Year`; for [mse-class], a named list (`Historical` plus one element per
#'   MP) of such arrays.
#' * `df = TRUE`: a tidy `data.frame` with columns `Sim`, `Stock`, `Year`,
#'   `Period`, `MP` (MSE only), `Fleet`, `Value`, `Variable`.
#'
#' @seealso [Biomass()], [Advice()]
#' @export
VBiomass <- function(object,
                     df      = TRUE,
                     byFleet = TRUE,
                     type    = c('Removals', 'Landings'),
                     Reduce  = TRUE,
                     IncYear = FALSE) {
  type <- match.arg(type)
  .CheckClass(object, c('hist', 'mse'), 'object')

  if (inherits(object, 'hist')) {
    arr <- .CalcVBiomass(object, object@OM, Years = Years(object, 'Historical'), type = type)
    if (!byFleet) arr <- SumOverFleet(arr)
    if (!df) return(arr)
    if (Reduce) arr <- ReduceDims(arr, IncYear = IncYear)
    out <- .VbiomassArrayToDf(arr) |> dplyr::mutate(Period = 'Historical')
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

  if (inherits(object, 'mse') && !is.null(MPName)) {
    override <- object@Misc[[what]][[MPName]][[stock]][[fleet]]$MeanAtAge
    if (!is.null(override))
      ArrayFill(base) <- override
  }

  if (identical(x, 'Age'))
    return(base)

  schedObj@MeanAtAge    <- base
  schedObj@MeanAtLength <- NULL
  schedObj <- .MeanAtAge2MeanAtLength(schedObj, OM@Stock[[stock]]@Length, replace = TRUE, Years = Years)
  out <- schedObj@MeanAtLength
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
