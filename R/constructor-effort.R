#' Effort 
#' 
#' Create an `Effort` object
#' 
#' The [Effort()] class stores historical fishing effort and
#' associated spatial structure for [Fleet()] object. Effort may be supplied
#' directly as an array or generated stochastically from a data frame.
#' 
#' @details
#' Effort represents total fishing activity prior to spatial allocation.
#' 
#' It is only required for spatial operating models.
#'
#' When `Distribution` is supplied, effort is partitioned explicitly
#' across areas. Otherwise, spatial allocation is derived internally
#' using spatial utility calculations and fleet behaviour.
#'
#' If `Effort` is supplied as a correctly structured `data.frame`,
#' it will be used by [GenerateHistoricalEffort()] to generate stochastic
#' historical effort.
#'
#' @seealso [Fleet()], [GenerateHistoricalEffort()]
#'
#' @include class-unions.R
#'
#' @name Effort
#' @export
Effort <- function(Effort       = NULL,
                   Units        = NULL,
                   Distribution = NULL,
                   Targeting    = NULL,
                   Maximum      = NULL,
                   Misc         = list()) {
  
  methods::new(
    "effort",
    Effort       = Effort,
    Units        = Units,
    Distribution = Distribution,
    Targeting    = Targeting,
    Maximum      = Maximum,
    Misc         = Misc
  )
}

#' Effort accessors and assignment functions
#'
#' Functions for accessing and modifying an [Effort()] object, and for
#' attaching or retrieving an `Effort` object from a [Fleet()].
#'
#' @param Fleet A [Fleet()] object.
#' @param x An [Effort()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetEffort()` and `SetEffort()` retrieve or assign the `Effort`
#'   component of a [Fleet()] object.
#' - Accessors such as `Effort()`, `Units()`, and `Distribution()` retrieve
#'   individual components of an [Effort()] object.
#' - Replacement functions (e.g. `Effort<-`) update the corresponding
#'   component and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Effort()].
#'
#' @name Effort-accessors
NULL

#' @rdname Effort-accessors
#' @export
GetEffort <- function(Fleet) {
  CheckClass(Fleet, "fleet", "Fleet")
  Fleet@Effort
}

#' @rdname Effort-accessors
#' @export
SetEffort <- function(Fleet, Effort) {
  CheckClass(Fleet, "fleet", "Fleet")
  CheckClass(Effort, "effort", "Effort")
  Fleet@Effort <- Effort
  methods::validObject(Fleet)
  Fleet
}

#' @rdname Effort-accessors
#' @export
Effort <- function(x) {
  CheckClass(x, "effort", "x")
  x@Effort
}

#' @rdname Effort-accessors
#' @export
`Effort<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Effort <- value
  methods::validObject(x)
  x
}



#' @rdname Effort-accessors
#' @export
Distribution <- function(x) {
  CheckClass(x, "effort", "x")
  x@Distribution
}

#' @rdname Effort-accessors
#' @export
`Distribution<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Distribution <- value
  methods::validObject(x)
  x
}

#' @rdname Effort-accessors
#' @export
Targeting <- function(x) {
  CheckClass(x, "effort", "x")
  x@Targeting
}

#' @rdname Effort-accessors
#' @export
`Targeting<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Targeting <- value
  methods::validObject(x)
  x
}

#' @rdname Effort-accessors
#' @export
Maximum <- function(x) {
  CheckClass(x, "effort", "x")
  x@Maximum
}

#' @rdname Effort-accessors
#' @export
`Maximum<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Maximum <- value
  methods::validObject(x)
  x
}


