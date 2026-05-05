#' Effort Class
#'
#' The `effort` class stores historical fishing effort and associated
#' spatial effort structure for a [Fleet()] object.
#' 
#' See [Effort()] for details.
#' 
#' @slot Effort Numeric array of fishing effort 
#' @slot Units Optional character string describing the units of effort
#' @slot Distribution Optional numeric array with dimensions `nSim x nYear x nArea`
#'     giving the fraction of total effort allocated to each spatial area
#'  @slot Targeting Numeric array or scalar controlling spatial targeting behavior
#'  @slot Maximum Numeric array giving the maximum possible fishing effort. 
#'  @slot Mode Character. Mode for calculation of spatial utility: `Density` (default)
#'  or `Biomass`. 
#'  @slot Misc Miscellaneous list
#' @include class-unions.R
#' @name effort-class
setClass(
  "effort",
  slots = c(
    Effort       = "num.array.df",
    Units        = "char.null",
    Distribution = "num.array.null",
    Targeting    = "num.array.null",
    Maximum      = "num.array.null",
    Mode         = 'char.null',
    Misc         = "list"
  )
)

setValidity("effort", function(object) {
  # TODO
  TRUE
})


