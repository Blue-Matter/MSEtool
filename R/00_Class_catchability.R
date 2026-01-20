#' Catchability Object
#'
#' Historical Catchability
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Efficiency Gear efficiency (q)
#' @slot Misc `r Misc_param()`
#'
#' @name Catchability
#' @export
setClass("catchability",
  slots = c(
    Efficiency = "num.array",
    qCV = "num.array",
    qInc = "num.array",
    Misc = 'list'
  )
)


setValidity('catchability', function(object) {
  TRUE
})


setMethod("initialize", "catchability", function(.Object,
                                                 Efficiency = NULL,
                                                 qCV = NULL,
                                                 qInc = NULL,
                                                 Misc = list()) {
  .Object@Efficiency <- Efficiency
  .Object@qCV <- qCV
  .Object@qInc <- qInc
  .Object@Misc <- Misc
  .Object
})

#' @rdname Catchability
#' @export
Catchability <- function(Efficiency = NULL,
                         qCV = NULL,
                         qInc = NULL,
                         Misc = list()) {
  methods::new("catchability",
    Efficiency = Efficiency,
    qCV = qCV,
    qInc = qInc,
    Misc = Misc
  )
}

#' @rdname Catchability
#' @export
#'
Efficiency <- function(object) {
  object@Efficiency
}
