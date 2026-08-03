methods::setClassUnion(
  name    = "selectivity.list",
  members = c("selectivity", "list", 'NULL')
)

methods::setClassUnion(
  name    = "retention.list",
  members = c("retention", "list", 'NULL')
)

methods::setClassUnion(
  name    = "discardmortality.list",
  members = c("discardmortality", "list", 'NULL')
)

#' The `advice` S4 Class
#'
#' The `advice` class defines management advice produced by a management
#' procedure. Advice may include output controls (e.g. total allowable catch),
#' input controls (e.g. effort), spatial controls (closures), gear effects,
#' direct fishing mortality rates, or bag-limit regulations. See [Advice()]
#' for details on valid entries and options for each slot.
#' 
#' @slot TAC Numeric vector length `1` or numeric length `nFleet` specifying 
#' total allowable catch in units of `TACUnit`.
#'
#' @slot TACType Character. Does the TAC refer to `"Removals"` (default) or
#'   `"Landings"`. Either length `1` (applied to all fleets) or a character
#'   vector of length `nFleet`.
#'
#' @slot TACUnit Character. Units of the TAC: `"Biomass"` (default) or
#'   `"Number"`. Either length 1 (applied to all fleets) or a character
#'   vector of length `nFleet`.
#' 
#' @slot Effort Numeric vector length `1` or numeric length `nFleet` specifying 
#' relative or absolute fishing effort (in units of `Fleet@Effort@Effort`). Can
#' also be a matrix with dimensions `nFleet` x `nArea` to set area-specific 
#' effort limits.
#'
#' @slot EffType Character. Are effort regulations relative to last historical
#'   year (`"Rel"`) or absolute (`"Abs"`; in units of [Effort()]). Either
#'   length 1 (applied to all fleets) or a character vector of length `nFleet`.
#'   Default is `"Rel"`.
#'
#' @slot Closure Numeric vector length `nArea` or array with dimensions `nFleet` 
#' x `nArea` specifyin an area open (`1`; default) or closed (`0`) to fishing.
#'
#' @slot Selectivity A [Selectivity()] object or an `nFleet` long list 
#' of [Selectivity()] objects defining gear selectivity prescribed by the `MP`
#'
#' @slot Retention A [Retention()] object or an `nFleet` long list 
#' of [Retention()] objects  defining retention prescribed by the `MP`
#'
#' @slot DiscardMortality A [DiscardMortality()] object or an `nFleet` long 
#'  list of [DiscardMortality()] objects defining discard mortality set in the `MP`
#'
#' @slot BagLimit Numeric vector or `NULL`. Bag limit in fish per angler per
#'   trip (when `LimitType = "angler"`) or fish per vessel per trip (when
#'   `LimitType = "boat"`), for this stock's catch by each fleet. Either
#'   length 1 (applied to all fleets) or a numeric vector of length `nFleet`
#'   for fleet-specific limits. `NULL` (default) means no bag limit
#'   regulation is active. `NA` for a given fleet position means no limit
#'   applies to that fleet. An aggregate bag limit pooling several stocks
#'   under one fleet is set separately via `AggregateBagLimit()` (`mmp`
#'   management procedures only); a stock's own `BagLimit` then acts as an
#'   optional species-specific sub-cap within that pooled limit. See
#'   [Advice()].
#'
#' @slot LimitType Character or `NULL`. Specifies whether `BagLimit` is
#'   per-angler (`"angler"`; default) or per-vessel (`"boat"`) regulations.
#'   Either length 1 (applied to all fleets) or a character vector of length
#'   `nFleet`. See [Advice()].
#'
#' @slot ClosureMode Character or `NULL`. Determines how the OM handles catch
#'   that exceeds the bag limit: `"discard"` (default) converts excess catch
#'   to discards, with discard mortality applied via the fleet's
#'   [DiscardMortality()] object; `"stop"` reduces effort to prevent the
#'   limit from being exceeded. Either length 1 (applied to all fleets) or a
#'   character vector of length `nFleet`. See [Advice()].
#'   
#' @slot Misc Miscellaneous list. Will be passed to `Data@Misc` in following time steps.
#'
#' @slot Log `list`. Internal named list storing diagnostics, warnings, and
#'   assumptions recorded during processing. See [Log()]. Not intended for
#'   direct user access.
#' 
#' @seealso [Advice()], [Selectivity()], [Retention()], [DiscardMortality()]
#'
#' @name advice-class
#' @include class-unions.R
#' @include class-selectivity.R
#' @include class-retention.R
#' @include class-discardmortality.R
setClass("advice",
         slots = c(
           TAC              = "num.array.null",
           TACType          = "char.null",
           TACUnit          = "char.null",
           Effort           = "num.array.null",
           EffType          = "char.null",
           Closure          = "num.array.null",
           Selectivity      = "selectivity.list",
           Retention        = "retention.list",
           DiscardMortality = "discardmortality.list",
           BagLimit         = "num.array.null",
           LimitType        = "char.null",
           ClosureMode      = "char.null",
           Misc             = "list",
           Log              = "list"
         )
)


setValidity("advice", function(object) {
  errors <- character()
  
  valid_TACType <- c("Removals", "Landings")
  if (!is.null(object@TACType) && !all(object@TACType %in% valid_TACType))
    errors <- c(errors,
                paste0("`TACType` must contain only: ",
                       paste(valid_TACType, collapse = ", ")))
  
  valid_TACUnit <- c("Biomass", "Number")
  if (!is.null(object@TACUnit) && !all(object@TACUnit %in% valid_TACUnit))
    errors <- c(errors,
                paste0("`TACUnit` must contain only: ",
                       paste(valid_TACUnit, collapse = ", ")))
  
  valid_EffType <- c("Rel", "Abs")
  if (!is.null(object@EffType) && !all(object@EffType %in% valid_EffType))
    errors <- c(errors,
                paste0("`EffType` must contain only: ",
                       paste(valid_EffType, collapse = ", ")))
  
  valid_LimitType <- c("angler", "boat")
  if (!is.null(object@LimitType) && !all(object@LimitType %in% valid_LimitType))
    errors <- c(errors,
                paste0("`LimitType` must contain only: ",
                       paste(valid_LimitType, collapse = ", ")))
  
  valid_ClosureMode <- c("discard", "stop")
  if (!is.null(object@ClosureMode) && !all(object@ClosureMode %in% valid_ClosureMode))
    errors <- c(errors,
                paste0("`ClosureMode` must contain only: ",
                       paste(valid_ClosureMode, collapse = ", ")))
  
  if (!is.null(object@BagLimit) && any(object@BagLimit < 0, na.rm = TRUE))
    errors <- c(errors, "`BagLimit` must be non-negative")

  if (length(errors)) errors else TRUE
})
