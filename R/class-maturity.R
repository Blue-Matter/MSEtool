#' Maturity Class
#'
#' An S4 class defining maturity-at-age, length, or weight relationships
#' for a stock. The class supports parametric maturity models as well as
#' fully specified maturity schedules.
#'
#' @slot Pars Named list of model parameters (e.g. L50, L50_95)
#' @slot Model Character or function identifying the maturity model
#' @slot MeanAtAge Numeric array of maturity-at-age
#' @slot MeanAtLength Numeric array of maturity-at-length (optional).
#' @slot MeanAtWeight Numeric array of maturity-at-weight (optional).
#' @slot Classes Optional numeric vector defining maturity classes
#' @slot Semelparous Logical or array indicating semelparity
#' @param Misc Miscellaneous list.
#'
#' @export
#' @include class-unions.R
setClass(
  "maturity",
  slots = c(
    Pars          = "list",
    Model         = "fun.char",
    MeanAtAge     = "num.array.null",
    MeanAtLength  = "num.array.null",
    MeanAtWeight  = "num.array.null",
    Classes       = "num.null",
    Semelparous   = "array.log.null",
    Misc          = "list"
  )
)


setValidity("maturity", function(object) {
  # TODO
  TRUE
})

