#' Biomass and Fishing Mortality Relative to Reference Points
#'
#' Extract time series of biomass, spawning biomass, spawning production, or
#' fishing mortality expressed as a fraction of an unfished or MSY reference
#' level.
#'
#' `B_B0()`, `SB_SB0()`, and `SP_SP0()` divide the time series by the
#' corresponding unfished level (`type = 'Equilibrium'` or `'Dynamic'`).
#' `B_BMSY()`, `SB_SBMSY()`, and `SP_SPMSY()` divide by the corresponding
#' MSY reference point. `F_FMSY()` divides total apical fishing mortality
#' (summed over fleets) by `FMSY`.
#'
#' When applied to an [mse-class] object the historical and projection periods
#' are row-bound and labelled via the `Period` column; historical rows carry
#' `MP = "Historical"`.
#'
#' For the projection period of an [mse-class] object the denominator is
#' extended to cover projection years by repeating the last available
#' historical reference value (forward fill).
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param type Character. One of `'Equilibrium'` or `'Dynamic'`. Controls
#'   which unfished baseline is used. See [B0()] for details.
#'   Only applies to the `*_*0` family.
#' @param df Logical. If `FALSE` returns the raw ratio array.
#'   If `TRUE` (default) returns a tidy `data.frame`.
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using [ReduceDims()] before conversion to a data frame.
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the
#'   year dimension is retained during reduction. Default `FALSE`.
#'
#' @return
#' * `df = FALSE`: a numeric array of ratios with dimensions `Sim × Stock × Year`.
#' * `df = TRUE`: a tidy `data.frame` with columns `Sim`, `Stock`, `Year`,
#'   `Period`, `MP` (MSE only), `Value`, `Variable`.
#'
#' @examples
#' Hist <- Simulate(SingleStockOM)
#' B_B0(Hist)
#' SB_SB0(Hist, type = 'Dynamic')
#' F_FMSY(Hist)
#'
#' MSE <- Project(Hist, 'CurrentEffort')
#' B_BMSY(MSE, df = TRUE)
#' SB_SBMSY(MSE, df = TRUE)
#' SP_SPMSY(MSE, df = TRUE)
#' F_FMSY(MSE, df = TRUE)
#'
#' @name relative_ref
#' @seealso [Biomass()], [B0()], [BMSY()], [FDead()], [FMSY()]
#' @export
B_B0 <- function(object,
                 type    = c('Equilibrium', 'Dynamic'),
                 df      = TRUE,
                 Reduce  = TRUE,
                 IncYear = FALSE) {
  .ExtractRelative(object,
                   num_slot   = 'Biomass',
                   denom_slot = 'Biomass',
                   ref        = 'Unfished',
                   type       = match.arg(type),
                   df = df, Reduce = Reduce, IncYear = IncYear,
                   var_name   = 'B_B0')
}

#' @rdname relative_ref
#' @export
SB_SB0 <- function(object,
                   type    = c('Equilibrium', 'Dynamic'),
                   df      = TRUE,
                   Reduce  = TRUE,
                   IncYear = FALSE) {
  .ExtractRelative(object,
                   num_slot   = 'SBiomass',
                   denom_slot = 'SBiomass',
                   ref        = 'Unfished',
                   type       = match.arg(type),
                   df = df, Reduce = Reduce, IncYear = IncYear,
                   var_name   = 'SB_SB0')
}

#' @rdname relative_ref
#' @export
SP_SP0 <- function(object,
                   type    = c('Equilibrium', 'Dynamic'),
                   df      = TRUE,
                   Reduce  = TRUE,
                   IncYear = FALSE) {
  .ExtractRelative(object,
                   num_slot   = 'SProduction',
                   denom_slot = 'SProduction',
                   ref        = 'Unfished',
                   type       = match.arg(type),
                   df = df, Reduce = Reduce, IncYear = IncYear,
                   var_name   = 'SP_SP0')
}

#' @rdname relative_ref
#' @export
B_BMSY <- function(object,
                   df      = TRUE,
                   Reduce  = TRUE,
                   IncYear = FALSE) {
  .ExtractRelative(object,
                   num_slot   = 'Biomass',
                   denom_slot = 'BMSY',
                   ref        = 'MSY',
                   type       = NULL,
                   df = df, Reduce = Reduce, IncYear = IncYear,
                   var_name   = 'B_BMSY')
}

#' @rdname relative_ref
#' @export
SB_SBMSY <- function(object,
                     df      = TRUE,
                     Reduce  = TRUE,
                     IncYear = FALSE) {
  .ExtractRelative(object,
                   num_slot   = 'SBiomass',
                   denom_slot = 'SBMSY',
                   ref        = 'MSY',
                   type       = NULL,
                   df = df, Reduce = Reduce, IncYear = IncYear,
                   var_name   = 'SB_SBMSY')
}

#' @rdname relative_ref
#' @export
SP_SPMSY <- function(object,
                     df      = TRUE,
                     Reduce  = TRUE,
                     IncYear = FALSE) {
  .ExtractRelative(object,
                   num_slot   = 'SProduction',
                   denom_slot = 'SPMSY',
                   ref        = 'MSY',
                   type       = NULL,
                   df = df, Reduce = Reduce, IncYear = IncYear,
                   var_name   = 'SP_SPMSY')
}

#' @rdname relative_ref
#'
#' @details
#' `FMSY` is a single "apical fishing mortality" optimised jointly across all
#' stocks in a complex (see [CalcMSY()]) -- there is no separate `FMSY` for an
#' individual stock within a multi-stock complex. `F_FMSY()` therefore reports
#' its ratio at the *complex* level (`OM@Complexes`), not the stock level: the
#' numerator is the controlling (maximum) realised apical F across the stocks
#' in each complex -- the same quantity [CalcMSY()] itself optimises against
#' -- divided by that complex's `FMSY`. For a complex containing a single
#' stock this is identical to the stock's own F/FMSY. The `Stock` column of
#' the returned `data.frame` (and dimension of the `df = FALSE` array) holds
#' complex names, matching the convention already used for `FMSY` in
#' [refpointsMSY-class].
#' @export
F_FMSY <- function(object,
                   df      = TRUE,
                   Reduce  = TRUE,
                   IncYear = FALSE) {
  .CheckClass(object, c('hist', 'mse'), 'object')

  fmsy_arr <- object@Reference@MSY@FMSY
  .CheckRefPopulated(fmsy_arr, 'MSY', 'Reference@MSY@FMSY', 'F_FMSY')

  if (!df) {
    f_arr <- SumOverFleet(slot(object, 'FDead')) |> .AggregateFToComplex(object@OM)

    if (inherits(object, 'mse')) {
      MP_names <- dimnames(f_arr)[['MP']]
      fmsy_arr <- AddDimension(fmsy_arr, name = 'MP', val = MP_names)
    }

    target_years  <- dimnames(f_arr)[['Year']]
    denom_aligned <- .AlignDenomYears(fmsy_arr, target_years)
    return(ArrayDivide(f_arr, denom_aligned))
  }

  if (inherits(object, 'hist')) {
    out <- .ComputeFFMSY(object, fmsy_arr, Reduce, IncYear, object@OM)
    class(out) <- c('ffmsy.df', class(out))
    return(out)
  }

  hist_df <- .ComputeFFMSY(object@Hist, fmsy_arr, Reduce, IncYear, object@OM) |>
    dplyr::mutate(MP = 'Historical')
  proj_df <- .ComputeFFMSY(object, fmsy_arr, Reduce, IncYear, object@OM)

  out <- dplyr::bind_rows(hist_df, proj_df)
  class(out) <- c('ffmsy.df', class(out))
  out
}

# Reduce a per-stock array to one value per complex in `OM@Complexes` via
# `FUN`. `strict = FALSE` aggregates over whichever member stocks are
# present (dropping a complex if none are), for use when `arr` has already
# been filtered to a subset of stocks before aggregating.
.AggregateStockToComplex <- function(arr, OM, FUN, strict = TRUE) {
  dn        <- dimnames(arr)
  stock_pos <- match('Stock', names(dn))
  if (is.na(stock_pos))
    cli::cli_abort("`arr` has no `Stock` dimension.", .internal = TRUE)

  stockNms  <- StockNames(OM)
  complexes <- OM@Complexes
  if (!length(complexes))
    complexes <- stats::setNames(as.list(seq_along(stockNms)), stockNms)

  margin <- setdiff(names(dn), 'Stock')

  out_list <- purrr::map(complexes, \(idx) {
    nms     <- stockNms[idx]
    sub_idx <- match(nms, dn[[stock_pos]])

    if (!strict) {
      sub_idx <- sub_idx[!is.na(sub_idx)]
      if (!length(sub_idx)) return(NULL)
    } else if (anyNA(sub_idx)) {
      cli::cli_abort("Cannot locate stock(s) {.val {nms[is.na(sub_idx)]}} in the array.")
    }

    idx_list              <- rep(list(quote(expr = )), length(dn))
    idx_list[[stock_pos]] <- sub_idx
    sub_arr <- do.call('[', c(list(arr), idx_list, list(drop = FALSE)))

    apply(sub_arr, margin, FUN)
  })

  List2Array(purrr::compact(out_list), 'Stock', pos = stock_pos)
}

# F is not additive across stocks with different selectivity -- the complex's
# realised apical F is the *maximum* across member stocks, the same
# "controlling stock" logic CalcMSY()/.OptCalcRefMSYSims() use to define
# ActualApicalF for the complex.
.AggregateFToComplex <- function(f_arr, OM) {
  .AggregateStockToComplex(f_arr, OM, max)
}

.ComputeFFMSY <- function(object, fmsy_arr, Reduce, IncYear, OM) {
  isMSE <- inherits(object, 'mse')

  f_arr <- SumOverFleet(slot(object, 'FDead')) |> .AggregateFToComplex(OM)

  if (isMSE) {
    MP_names <- dimnames(f_arr)[['MP']]
    fmsy_arr <- AddDimension(fmsy_arr, name = 'MP', val = MP_names)
  }

  target_years  <- dimnames(f_arr)[['Year']]
  denom_aligned <- .AlignDenomYears(fmsy_arr, target_years)
  ratio         <- ArrayDivide(f_arr, denom_aligned)

  if (Reduce)
    ratio <- ReduceDims(ratio, IncYear = IncYear)

  Array2DF(ratio) |>
    dplyr::mutate(Variable = 'F_FMSY',
                  Period   = ifelse(isMSE, 'Projection', 'Historical')) |>
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period')
}


.CheckRefPopulated <- function(arr, ref, slot_label, fn_name) {
  if (!is.null(arr)) return(invisible(arr))
  if (ref == 'Unfished') {
    cli::cli_abort(c(
      "{.fn {fn_name}}: unfished slot {.field {slot_label}} has not been calculated.",
      "i" = "Run {.fn Simulate} with unfished reference points enabled."
    ), call = NULL)
  } else {
    cli::cli_abort(c(
      "{.fn {fn_name}}: MSY reference point {.field {slot_label}} has not been calculated.",
      "i" = "Run {.fn CalcMSY} on the object before calling {.fn {fn_name}}."
    ), call = NULL)
  }
}

.GetDenomArr <- function(object, ref, type, denom_slot) {
  if (ref == 'Unfished') {
    slot(slot(object@Unfished, type), denom_slot)
  } else {
    slot(object@Reference@MSY, denom_slot)
  }
}


.AlignDenomYears <- function(denom_arr, target_years) {
  denom_years <- dimnames(denom_arr)[['Year']]
  if (is.null(denom_years) || is.null(target_years)) return(denom_arr)
  if (identical(denom_years, target_years))            return(denom_arr)

  year_pos    <- which(names(dimnames(denom_arr)) == 'Year')
  denom_nums  <- as.numeric(denom_years)
  target_nums <- as.numeric(target_years)

  year_idx <- vapply(target_nums, function(ty) {
    m <- which(denom_nums == ty)
    if (length(m)) m else length(denom_nums)
  }, integer(1))

  nd  <- length(dim(denom_arr))
  idx <- vector('list', nd)
  for (j in seq_len(nd)) idx[[j]] <- if (j == year_pos) year_idx else TRUE

  out <- do.call('[', c(list(denom_arr), idx, list(drop = FALSE)))
  dn <- dimnames(out)
  dn[['Year']] <- target_years
  dimnames(out) <- dn
  out
}

.ComputeRelative <- function(object, OM, num_slot, denom_arr, var_name,
                               Reduce, IncYear) {
  isMSE <- inherits(object, 'mse')

  arr          <- slot(object, num_slot)
  target_years <- dimnames(arr)[['Year']]
  denom_aligned <- .AlignDenomYears(denom_arr, target_years)

  if (isMSE) {
    MP_names <- dimnames(arr)[['MP']]
    denom_aligned <- AddDimension(denom_aligned, name = 'MP', val = MP_names)
  }

  ratio <- ArrayDivide(arr, denom_aligned)

  if (Reduce)
    ratio <- ReduceDims(ratio, IncYear = IncYear)

  Array2DF(ratio) |>
    dplyr::mutate(Variable = var_name,
                  Period   = ifelse(isMSE, 'Projection', 'Historical')) |>
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period')
}

.ExtractRelative <- function(object, num_slot, denom_slot, ref, var_name,
                              type    = NULL,
                              df      = TRUE,
                              Reduce  = TRUE,
                              IncYear = FALSE) {

  .CheckClass(object, c('hist', 'mse'), 'object')

  denom_arr <- .GetDenomArr(object, ref, type, denom_slot)
  slot_label <- if (ref == 'Unfished') {
    paste0('Unfished@', type, '@', denom_slot)
  } else {
    paste0('Reference@MSY@', denom_slot)
  }
  .CheckRefPopulated(denom_arr, ref, slot_label, var_name)

  if (!df) {
    arr          <- slot(object, num_slot)
    target_years <- dimnames(arr)[['Year']]
    denom_aligned <- .AlignDenomYears(denom_arr, target_years)
    return(ArrayDivide(arr, denom_aligned))
  }

  OM <- object@OM

  if (inherits(object, 'hist')) {
    out <- .ComputeRelative(object, OM, num_slot, denom_arr, var_name,
                              Reduce, IncYear)
    class(out) <- c(paste0(tolower(gsub('_', '', var_name)), '.df'), class(out))
    return(out)
  }

  # MSE: bind historical and projection periods
  hist_df <- .ComputeRelative(object@Hist, OM, num_slot, denom_arr, var_name,
                                Reduce, IncYear) |>
    dplyr::mutate(MP = 'Historical')

  proj_df <- .ComputeRelative(object, OM, num_slot, denom_arr, var_name,
                                Reduce, IncYear)

  out <- dplyr::bind_rows(hist_df, proj_df)
  class(out) <- c(paste0(tolower(gsub('_', '', var_name)), '.df'), class(out))
  out
}
