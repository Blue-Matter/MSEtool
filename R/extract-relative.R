#' Biomass Relative to Reference Points
#'
#' Extract time series of biomass, spawning biomass, or spawning production
#' expressed as a fraction of an unfished or MSY reference level.
#'
#' `B_B0()`, `SB_SB0()`, and `SP_SP0()` divide the time series by the
#' corresponding unfished level (`type = 'Equilibrium'` or `'Dynamic'`).
#' `B_BMSY()`, `SB_SBMSY()`, and `SP_SPMSY()` divide by the corresponding
#' MSY reference point.
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
#'
#' MSE <- Project(Hist, 'CurrentEffort')
#' B_BMSY(MSE, df = TRUE)
#' SB_SBMSY(MSE, df = TRUE)
#' SP_SPMSY(MSE, df = TRUE)
#'
#' @name relative_ref
#' @seealso [Biomass()], [B0()], [BMSY()]
#' @export
B_B0 <- function(object,
                 type    = c('Equilibrium', 'Dynamic'),
                 df      = TRUE,
                 Reduce  = TRUE,
                 IncYear = FALSE) {
  extract_relative(object,
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
  extract_relative(object,
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
  extract_relative(object,
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
  extract_relative(object,
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
  extract_relative(object,
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
  extract_relative(object,
                   num_slot   = 'SProduction',
                   denom_slot = 'SPMSY',
                   ref        = 'MSY',
                   type       = NULL,
                   df = df, Reduce = Reduce, IncYear = IncYear,
                   var_name   = 'SP_SPMSY')
}


# ---- Internals ---------------------------------------------------------------

.check_ref_populated <- function(arr, ref, slot_label, fn_name) {
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

.get_denom_arr <- function(object, ref, type, denom_slot) {
  if (ref == 'Unfished') {
    slot(slot(object@Unfished, type), denom_slot)
  } else {
    slot(object@Reference@MSY, denom_slot)
  }
}

# Align the Year dimension of denom_arr to target_years.
# Years present in both: matched exactly.
# Target years absent from denom: the last available denom year is used
# (forward fill for projection period).
.align_denom_years <- function(denom_arr, target_years) {
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

# Worker: compute ratio data frame for one period/object (no byAge/byArea)
.compute_relative <- function(object, OM, num_slot, denom_arr, var_name,
                               Reduce, IncYear) {
  isMSE <- inherits(object, 'mse')

  arr          <- slot(object, num_slot)
  target_years <- dimnames(arr)[['Year']]
  denom_aligned <- .align_denom_years(denom_arr, target_years)

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
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |>
    dplyr::arrange(Sim, Stock, Year)
}

extract_relative <- function(object, num_slot, denom_slot, ref, var_name,
                              type    = NULL,
                              df      = TRUE,
                              Reduce  = TRUE,
                              IncYear = FALSE) {

  CheckClass(object, c('hist', 'mse'), 'object')

  denom_arr <- .get_denom_arr(object, ref, type, denom_slot)
  slot_label <- if (ref == 'Unfished') {
    paste0('Unfished@', type, '@', denom_slot)
  } else {
    paste0('Reference@MSY@', denom_slot)
  }
  .check_ref_populated(denom_arr, ref, slot_label, var_name)

  if (!df) {
    arr          <- slot(object, num_slot)
    target_years <- dimnames(arr)[['Year']]
    denom_aligned <- .align_denom_years(denom_arr, target_years)
    return(ArrayDivide(arr, denom_aligned))
  }

  OM <- object@OM

  if (inherits(object, 'hist')) {
    out <- .compute_relative(object, OM, num_slot, denom_arr, var_name,
                              Reduce, IncYear)
    class(out) <- c(paste0(tolower(gsub('_', '', var_name)), '.df'), class(out))
    return(out)
  }

  # MSE: bind historical and projection periods
  hist_df <- .compute_relative(object@Hist, OM, num_slot, denom_arr, var_name,
                                Reduce, IncYear) |>
    dplyr::mutate(MP = 'Historical')

  proj_df <- .compute_relative(object, OM, num_slot, denom_arr, var_name,
                                Reduce, IncYear)

  out <- dplyr::bind_rows(hist_df, proj_df)
  class(out) <- c(paste0(tolower(gsub('_', '', var_name)), '.df'), class(out))
  out
}
