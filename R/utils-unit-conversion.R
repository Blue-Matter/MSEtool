
.mass_units_g <- c(
  g          = 1,
  kg         = 1e3,
  lb         = 453.59237,
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

# Suffix describing a pure count scaling factor with no base unit involved
# (e.g. Number()/SRR@Units), e.g. 1000 -> "thousands", 500 -> "× 500".
# NULL when scale is 1 (no suffix needed) or unset.
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

# Appends a (possibly NULL/NA/empty) unit string to a plain axis label, e.g.
# .AppendUnits('Biomass', 't') -> "Biomass (t)"; .AppendUnits('Biomass', NULL) -> "Biomass".
.AppendUnits <- function(label, units) {
  if (is.null(units) || is.na(units) || !nzchar(units))
    return(label)
  paste0(label, ' (', units, ')')
}

# Core TRUE/FALSE/character `units` dispatch shared by every Plot*() unit
# helper: OM-level quantities (Biomass etc, `scale` = SRR@Units) and
# per-individual schedules (Length/Weight-at-age, `scale` = 1) both resolve
# `base_unit`/`scale` themselves and delegate here. `table` is
# `.mass_units_g` or `.length_units_mm`. Returns `list(label, factor)`:
# `factor` rescales the plotted values (1 unless a target unit is requested).
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

# Single consistent value of `slot(stock, what)@Units` across every selected
# stock in `OM` (or all stocks when `stockNames` is NULL), or NULL if stocks
# disagree or none has it set. `what` is one of "Weight", "Length", "Ages",
# "Fecundity", "NaturalMortality", "SRR".
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
