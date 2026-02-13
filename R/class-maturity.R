#' Maturity Class
#'
#' An S4 class representing the maturity schedule for a [Stock()] object.
#' 
#' See [Maturity()] for details.
#'
#' @slot Pars Named list of model parameters (e.g. L50, L50_95)
#' @slot Model Character or function identifying the maturity model
#' @slot MeanAtAge Numeric array of maturity-at-age
#' @slot MeanAtLength Numeric array of maturity-at-length (optional).
#' @slot MeanAtWeight Numeric array of maturity-at-weight (optional).
#' @slot Classes Optional numeric vector defining maturity classes
#' @slot Semelparous Logical or array indicating semelparity
#' @slot Misc Miscellaneous list.
#'
#' @export
#' @include class-unions.R
#' @name maturity-class
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

