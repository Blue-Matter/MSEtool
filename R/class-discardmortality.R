#' DiscardMortality Object
#'
#' Discard mortality describes the proportion of catch that is discarded and dies,
#' defined either at age or at length.
#' 
#' See [DiscardMortality()] for details.
#' 
#' @slot MeanAtAge Mean selectivity-at-age array.
#' @slot MeanAtLength Mean selectivity-at-length array.
#' @slot Classes Length class mid-points
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
  TRUE
})

