#' Selectivity Class
#'
#' The `selectivity` class defines how fishing or survey selectivity
#' varies with age, length, or weight.
#'
#' @slot Pars Named list of selectivity parameters.
#' @slot Model Selectivity model identifier.
#' @slot isRel Logical or numeric indicator of relative selectivity.
#' @slot MeanAtAge Mean selectivity-at-age array.
#' @slot MeanAtLength Mean selectivity-at-length array.
#' @slot MeanAtWeight Mean selectivity-at-weight array.
#' @slot Classes Class boundaries (age, length, or weight).
#' @slot Misc Miscellaneous list.
#'
#' @include class-unions.R
setClass(
  "selectivity",
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

setValidity("selectivity", function(object) {
  # TODO: consistency checks between Pars / Model / arrays
  TRUE
})
