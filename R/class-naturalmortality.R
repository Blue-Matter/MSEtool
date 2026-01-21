#' NaturalMortality Class
#'
#' An S4 class representing natural mortality assumptions associated
#' with a [Stock()] object.
#'
#' @slot Pars Named list of natural mortality parameters.
#' @slot Model Model identifier associated with `Pars`.
#' @slot Units Time units (e.g. `"year"`).
#' @slot MeanAtAge Mean natural mortality-at-age array (optional).
#' @slot MeanAtLength Mean natural mortality-at-length array (optional).
#' @slot Random Random effects array (optional).
#' @slot Classes Age or length class boundaries (optional).
#' @slot Misc Miscellaneous list.
#'
#' @export
#' @include class-unions.R
setClass(
  "naturalmortality",
  slots = c(
    Pars          = "list",
    Model         = "fun.char",
    Units         = "char.null",
    MeanAtAge     = "num.array.null",
    MeanAtLength  = "num.array.null",
    Random        = "num.array.null",
    Classes       = "num.null",
    Misc          = "list"
  )
)

setValidity("naturalmortality", function(object) {
  # TODO: add structural and dimensional checks
  TRUE
})
