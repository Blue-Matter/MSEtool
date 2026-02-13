#' Retention Class
#'
#' The `retention` class defines how fishing or survey retention
#' varies with age, length, or weight.
#' 
#' See [Retention()] for details.
#'
#' @slot Pars Named list of retention parameters.
#' @slot Model Retention model identifier.
#' @slot isRel Logical or numeric indicator of relative retention.
#' @slot MeanAtAge Mean retention-at-age array.
#' @slot MeanAtLength Mean retention-at-length array.
#' @slot MeanAtWeight Mean retention-at-weight array.
#' @slot Classes Class boundaries (age, length, or weight).
#' @slot Misc Miscellaneous list.
#'
#' @include class-unions.R
#' @name retention-class
setClass(
  "retention",
  slots = c(
    Pars = "list",
    Model = "fun.char",
    isRel = "char.log.num",
    MeanAtAge = "num.array.null",
    MeanAtLength = "num.array.null",
    MeanAtWeight = "num.array.null",
    Classes = "num.null",
    Misc = "list"
  )
)

setValidity("retention", function(object) {
  # TODO: consistency checks between Pars / Model / arrays
  TRUE
})
