#' The `discardmortality` S4 Class
#'
#' Defines the proportion of discarded catch that dies, at age or at length,
#' for use in a [fleet-class] object. Discard mortality is optional; if not
#' specified, all discarded fish are assumed to survive. Objects are typically
#' created via the [DiscardMortality()] constructor, which documents all
#' parameters in detail.
#'
#' @slot MeanAtAge `numeric` array or `NULL`. Mean discard mortality-at-age
#'   (`Sim x Age x Year x Area`). Values between 0 (all discards survive) and
#'   1 (all discards die). See [DiscardMortality()].
#' @slot MeanAtLength `numeric` array or `NULL`. Mean discard
#'   mortality-at-length (`Sim x Length x Year x Area`). Values are evaluated
#'   at the midpoint of each bin (halfway between consecutive lower bounds);
#'   the `Class` dimension is labelled by bin lower bounds (see `Classes`).
#'   See [DiscardMortality()].
#' @slot Classes `numeric` or `NULL`. Lower bounds of length bins corresponding
#'   to the second dimension of `MeanAtLength`. Bin `k` spans
#'   `[Classes[k], Classes[k+1])`; the final bin is open-ended. Values in
#'   `MeanAtLength` are evaluated at bin midpoints, not at these lower bounds.
#' @slot Misc `list`. Miscellaneous additional inputs. Used internally.
#'
#' @seealso 
#'  - [DiscardMortality()] for the constructor and full parameter
#'   documentation.
#'  -  [fleet-class] for the enclosing fleet object.
#'  - [Selectivity()], [Retention()] for related fleet components.
#'
#' @family fleet
#'
#' @include class-unions.R
#' @name discardmortality-class
setClass(
  "discardmortality",
  slots = c(
    MeanAtAge    = "num.array.null",
    MeanAtLength = "num.array.null",
    Classes      = "num.null",
    Misc         = "list"
  )
)

setValidity("discardmortality", function(object) {
  # TODO
  TRUE
})

