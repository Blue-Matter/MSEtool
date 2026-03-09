#' Copy Pre-Populated Slots from `OM@Misc` into a `hist` Object
#'
#' Attempts to copy each recognised time-series slot from `Hist@OM@Misc` into
#' the corresponding slot of `Hist`. Slots not present in `Hist@OM@Misc` are
#' left unchanged. Returns `Hist` unchanged if `Hist@OM@Misc` is empty.
#'
#' This allows users to pre-populate specific historical time-series arrays
#' (e.g. `Biomass`, `Landings`) via `OM@Misc` before running a simulation,
#' overriding the values that would otherwise be computed. Pre-populated values
#' are not overwritten by the model.
#'
#' @param Hist A [hist-class] object with a populated `OM` slot.
#'
#' @return `Hist` with any recognised slots overwritten by values found in
#'   `Hist@OM@Misc`.
#' @keywords internal
FillFromMisc <- function(Hist) {
  if (!length(Hist@OM@Misc))
    return(Hist)
  
  ts_slots <- c(
    'Number', 'Biomass', 'SBiomass', 'SProduction',
    'Landings', 'Discards', 'Effort', 'Distribution',
    'Catchability', 'qArea',
    'FDead', 'FRetain', 'FDeadArea', 'FRetainArea'
  )
  
  for (sl in ts_slots) {
    Hist <- Misc2Hist(Hist, sl)
  }
  Hist
}

#' Copy a Single Slot from `OM@Misc` into a `hist` Object
#'
#' If `Hist@OM@Misc[[sl]]` is non-`NULL`, validates that it has the required
#' named dimensions (or columns, if a `data.frame`) and copies it into
#' `slot(Hist, sl)` via `ArrayFill`, extended to match `nSim`. Returns
#' `Hist` unchanged if `Hist@OM@Misc[[sl]]` is `NULL`.
#'
#' Input may be supplied as either:
#' - A named array with dimension names matching those of `slot(Hist, sl)`,
#'   plus a `Value` column.
#' - A `data.frame` with columns matching the required dimension names of
#'   `slot(Hist, sl)`, converted to an array via [DF2Array()].
#'
#' @param Hist A [hist-class] object.
#' @param sl Character string. Name of the slot to populate. Must be a valid
#'   slot of both `Hist` and a key in `Hist@OM@Misc`.
#'
#' @return `Hist` with `slot(Hist, sl)` overwritten if a matching entry was
#'   found in `Hist@OM@Misc`, otherwise `Hist` unchanged.
#' @keywords internal
Misc2Hist <- function(Hist, sl='Biomass') {
  value <- Hist@OM@Misc[[sl]]
  if (is.null(value))
    return(Hist)
  
  if (inherits(value, 'data.frame')) {
    req  <- c(slot(Hist, sl) |> dimnames() |> names(), 'Value')
    provided <- colnames(value)
    missing_cols <- req[!req %in% provided]
    if (length(missing_cols))
      cli::cli_abort(c(
        "Missing columns in {.code OM@Misc${sl}}.",
        "i" = "Expected: {.val {req}}",
        "x" = "Missing:  {.val {missing_cols}}"
      ))
    value <- DF2Array(value)
  }
  
  req      <- slot(Hist, sl) |> dimnames() |> names()
  provided <- value |> dimnames() |> names()
  missing_dims <- req[!req %in% provided]
  if (length(missing_dims))
    cli::cli_abort(c(
      "Missing named dimensions in array: {.code OM@Misc${sl}}.",
      "i" = "Expected: {.val {req}}",
      "x" = "Missing:  {.val {missing_dims}}"
    ))
  
  value <- ExtendSims(value, nSim(Hist))
  ArrayFill(slot(Hist, sl)) <- value
  Hist
}

