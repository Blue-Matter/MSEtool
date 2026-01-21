#' DiscardMortality Object
#'
#' Discard mortality describes the proportion of catch that is discarded,
#' defined either at age or at length.
#'
#' @include class-unions.R
#'
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

