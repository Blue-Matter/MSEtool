
.mass_units_g <- c(
  g          = 1,
  kg         = 1e3,
  lb         = 453.59237,
  `1000 lb`   = 453592.37,
  t          = 1e6,
  tonne      = 1e6,
  kt         = 1e9,
  `long ton`  = 1016046.9088,
  `short ton` = 907184.74
)

.length_units_mm <- c(
  mm   = 1,
  cm   = 10,
  inch = 25.4,
  m    = 1000
)

.count_units <- c("Number", "n", "1000 n", "fish", "1000 fish")

#' Catch quantity type from catch units
#'
#' Classifies catch units as `"Biomass"` or `"Number"`. `units` may be the
#' generic `"Biomass"` or `"Number"`, a mass unit (a name of `.mass_units_g`),
#' or a count unit (`.count_units`). Matching is case-insensitive. Values are
#' never rescaled.
#'
#' @param units `character` vector of catch units, or `NULL`.
#' @param default Type returned for `NA` elements of `units`.
#' @param arg Argument name used in the error message.
#'
#' @return `character` vector of `"Biomass"`/`"Number"` the same length as
#'   `units`, or `NULL` if `units` is `NULL`. Errors on an unrecognized unit.
#' @keywords internal
.CatchUnitType <- function(units, default = "Biomass", arg = "Units") {
  if (is.null(units)) return(NULL)
  u   <- tolower(trimws(units))
  out <- rep(NA_character_, length(u))
  out[is.na(u)] <- default
  out[u %in% tolower(c("Biomass", names(.mass_units_g)))] <- "Biomass"
  out[u %in% tolower(.count_units)] <- "Number"
  bad <- is.na(out)
  if (any(bad))
    cli::cli_abort(c(
      "x" = "Unrecognized catch {.arg {arg}}: {.val {unique(units[bad])}}.",
      "i" = "Biomass units: {.val {c('Biomass', names(.mass_units_g))}}.",
      "i" = "Number units: {.val {(.count_units)}}."
    ))
  out
}

.IsCatchUnit <- function(units) {
  u <- tolower(trimws(units))
  is.na(u) | u %in% tolower(c("Biomass", names(.mass_units_g), .count_units))
}

.NiceUnitName <- function(table, value, tol = 1e-6) {
  if (!is.finite(value) || value <= 0)
    return(NULL)
  hit <- names(table)[abs(table - value) / value < tol]
  if (!length(hit))
    return(NULL)
  if ('t' %in% hit) return('t')
  hit[1]
}

.CombineScaledUnit <- function(table, base_unit, scale = 1) {
  if (is.null(base_unit) || length(base_unit) != 1 || is.na(base_unit) || !nzchar(base_unit))
    return(NULL)
  if (is.null(scale) || !is.numeric(scale) || length(scale) != 1 || is.na(scale))
    scale <- 1

  if (!base_unit %in% names(table)) {
    label <- if (isTRUE(scale == 1)) base_unit else
      paste0(base_unit, ' × ', format(scale, big.mark = ',', scientific = FALSE))
    return(list(label = label, per_unit = NA_real_, convertible = FALSE))
  }

  per_unit <- unname(table[[base_unit]]) * scale
  nice  <- .NiceUnitName(table, per_unit)
  label <- if (!is.null(nice)) nice else if (isTRUE(scale == 1)) base_unit else
    paste0(base_unit, ' × ', format(scale, big.mark = ',', scientific = FALSE))

  list(label = label, per_unit = per_unit, convertible = TRUE)
}


.UnitRescaleFactor <- function(table, info, target, what = 'value') {
  if (is.null(info) || !isTRUE(info$convertible))
    cli::cli_abort(c(
      "x" = "Can't convert {.field {what}} to `units = '{target}'`.",
      "i" = "The stored units aren't expressed in a recognized unit ({.val {names(table)}})."
    ))
  if (!target %in% names(table))
    cli::cli_abort(c(
      "x" = "`units = '{target}'` is not a recognized unit for {.field {what}}.",
      "i" = "Supported units: {.val {names(table)}}."
    ))
  info$per_unit / unname(table[[target]])
}

.CountScaleLabel <- function(scale) {
  if (is.null(scale) || !is.numeric(scale) || length(scale) != 1 || is.na(scale))
    return(NULL)
  if (isTRUE(all.equal(scale, 1)))
    return(NULL)
  known_vals  <- c(1e3, 1e6, 1e9)
  known_names <- c('thousands', 'millions', 'billions')
  hit <- known_names[abs(known_vals - scale) / scale < 1e-6]
  if (length(hit))
    return(hit[1])
  paste0('× ', format(scale, big.mark = ',', scientific = FALSE))
}

.AppendUnits <- function(label, units) {
  if (is.null(units) || is.na(units) || !nzchar(units))
    return(label)
  paste0(label, ' (', units, ')')
}

.ResolveUnitInfo <- function(table, base_unit, scale, units, what = 'value') {
  if (isFALSE(units))
    return(list(label = NULL, factor = 1))
  if (!isTRUE(units) && !(is.character(units) && length(units) == 1))
    cli::cli_abort("`units` must be `TRUE`, `FALSE`, or a single unit string.")

  info <- .CombineScaledUnit(table, base_unit, scale)

  if (isTRUE(units))
    return(list(label = if (is.null(info)) NULL else info$label, factor = 1))

  factor <- .UnitRescaleFactor(table, info, units, what)
  list(label = units, factor = factor)
}

.GetStockUnits <- function(OM, what, stockNames = NULL) {
  stocks <- OM@Stock
  if (!is.null(stockNames))
    stocks <- stocks[names(stocks) %in% stockNames]
  vals <- purrr::map(stocks, \(stock) slot(stock, what)@Units)
  vals <- purrr::compact(vals)
  if (!length(vals))
    return(NULL)
  uniq <- unique(vals)
  if (length(uniq) != 1)
    return(NULL)
  uniq[[1]]
}
