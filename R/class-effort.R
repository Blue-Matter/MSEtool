#' The `effort` S4 Class
#'
#' Defines historical fishing effort and associated spatial structure for use
#' in a [fleet-class] object. Objects are typically created via the [Effort()]
#' constructor, which documents all parameters in detail.
#'
#' @slot Effort `numeric` array, data frame, or `NULL`. Historical fishing
#'   effort. See [Effort()] for accepted formats.
#' @slot Units `character` or `NULL`. Units of fishing effort (e.g.,
#'   `"hours"`, `"trips"`). See [Effort()].
#' @slot Distribution `numeric` array or `NULL`. Fraction of total effort
#'   allocated to each spatial area (`Sim x Year x Area`). See [Effort()].
#' @slot Targeting `numeric` array or `NULL`. Fleet targeting parameter
#'   (`Sim x Year`). See [Effort()].
#' @slot Maximum `numeric` array or `NULL`. Maximum allowable effort. See
#'   [Effort()].
#' @slot Mode `character` or `NULL`. Spatial utility calculation mode:
#'   `"Density"` (default) or `"Biomass"`. See [Effort()].
#' @slot Misc `list`. Miscellaneous additional inputs.
#'
#' @seealso [Effort()] for the constructor and full parameter documentation.
#'   [fleet-class] for the enclosing fleet object.
#'
#' @family fleet
#'
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


