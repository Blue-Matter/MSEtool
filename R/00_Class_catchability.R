

#' Catchability Object
#'
#' Historical fishing effort
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Q Numeric array.
#' @slot QArea Numeric array. 
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('catchability')`
#'
#' @name Catchability
#' @rdname Catchability
#' @docType class
#' @example man-examples/Catchability-class.R
#' @export
setClass('catchability',
         slots=c(Q='num.array',
                 qCV='num.array',
                 qInc='num.array',
                 qArea='num.array'
         ),
         contains = c('MiscClass')
)
