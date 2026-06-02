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
#'   mortality-at-length (`Sim x Length x Year x Area`). See
#'   [DiscardMortality()].
#' @slot Classes `numeric` or `NULL`. Length class midpoints corresponding to
#'   the second dimension of `MeanAtLength`.
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

