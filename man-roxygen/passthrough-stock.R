#' @section Pass-Through Access:
#' When `<%= arg %>` is an object that contains `<%= slot %>` objects,
#' `<%= slot %>()` returns them instead of constructing a new object:
#'
#' - a [stock-class] object: its `<%= slot %>` slot.
#' - an [om-class], [hist-class], or [mse-class] object, or a list of
#'   [stock-class] objects: a list with the `<%= slot %>` slot of each stock.
#' - a [data-class] or [obs-class] object, or a [lifehistorydata-class] or
#'   [lifehistoryobs-class] object or a list of them: the `<%= slot %>` slot
#'   of the life-history information.
#'
#' `<%= slot %>(x) <- value` replaces the slot of a [stock-class],
#' [om-class], [data-class], [obs-class], [lifehistorydata-class], or
#' [lifehistoryobs-class] object, or of each object in a list of them. When
#' `x` has more than one stock, `value` must be a list named by stock (see
#' [StockNames()]). The slot cannot be replaced in a [hist-class] or
#' [mse-class] object, because it was used to simulate the historical
#' dynamics.
