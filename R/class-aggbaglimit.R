#' The `aggbaglimit` S4 Class
#'
#' The `aggbaglimit` class declares an aggregate bag limit pooling catch
#' across several independently-modelled stocks for a single fleet -
#' management advice that does not belong to any one stock's [advice-class]
#' object. Returned by `mmp`-class management procedures alongside a
#' per-stock `Advice` list. See [AggregateBagLimit()] for the constructor
#' and full parameter documentation.
#'
#' @slot Fleet Character or numeric, length 1. Identifies the fleet the
#'   aggregate limit applies to, by name or position.
#' @slot Stocks Character or numeric, length 2 or more. Identifies the
#'   stocks pooled under this limit, by name or position.
#' @slot BagLimit Numeric, length 1. The aggregate limit, in fish per trip.
#' @slot LimitType Character. `"boat"` or `"angler"` - see [Advice()] for
#'   the shared definition. Governs enforcement for the aggregate limit and
#'   for any per-stock `BagLimit` set (as a species-specific sub-cap) on a
#'   stock included in `Stocks`.
#' @slot ClosureMode Character. `"discard"` or `"stop"` - see [Advice()]
#'   for the shared definition. Governs enforcement for the aggregate limit
#'   and for any per-stock `BagLimit` sub-cap on a stock included in
#'   `Stocks`.
#' @slot Misc Miscellaneous list.
#'
#' @seealso [AggregateBagLimit()], [advice-class]
#'
#' @include class-unions.R
#' @name aggbaglimit-class
setClass(
  "aggbaglimit",
  slots = c(
    Fleet       = "char.num",
    Stocks      = "char.num",
    BagLimit    = "numeric",
    LimitType   = "char.null",
    ClosureMode = "char.null",
    Misc        = "list"
  )
)

setValidity("aggbaglimit", function(object) {
  errors <- character()

  if (length(object@Fleet) != 1)
    errors <- c(errors, "`Fleet` must identify a single fleet")

  if (length(object@Stocks) < 2)
    errors <- c(errors, "`Stocks` must identify two or more stocks to pool")

  if (length(object@BagLimit) != 1)
    errors <- c(errors, "`BagLimit` must be a single numeric value")

  if (length(object@BagLimit) == 1 && !is.na(object@BagLimit) && object@BagLimit < 0)
    errors <- c(errors, "`BagLimit` must be non-negative")

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

  if (length(errors)) errors else TRUE
})
