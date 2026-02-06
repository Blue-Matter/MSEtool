#' Bioeconomic
#'
#' Create a `Bioeconomic` object.
#'
#' A `Bioeconomic` object stores revenue, cost, and investment dynamics
#' used in fleet-level or stock-level bioeconomic analyses.
#'
#' @param Revenue Revenue array.
#' @param Cost Operating cost per unit of effort.
#' @param Investment Cost of adding effort.
#' @param Disinvestment Cost of removing effort.
#' @param Depreciation Depreciation rate of effort units.
#' @param Discount Discount factor.
#' @param Misc Miscellaneous list.
#' 
#' The `Bioeconomic` object is not currently used 
#'
#' @return A `Bioeconomic` object.
#'
#' @seealso [Fleet()]
#'
#' @export
Bioeconomic <- function(Revenue = NULL,
                        Cost = NULL,
                        Investment = NULL,
                        Disinvestment = NULL,
                        Depreciation = NULL,
                        Discount = NULL,
                        Misc = list()) {
  
  methods::new(
    "bioeconomic",
    Revenue = Revenue,
    Cost = Cost,
    Investment = Investment,
    Disinvestment = Disinvestment,
    Depreciation = Depreciation,
    Discount = Discount,
    Misc = Misc
  )
}

#' Bioeconomic accessors and assignment functions
#'
#' Functions for accessing and modifying a [Bioeconomic()] object, and for
#' attaching or retrieving a `Bioeconomic` object from a [Fleet()].
#'
#' @param Fleet A [Fleet()] object.
#' @param x A [Bioeconomic()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetBioeconomic()` and `SetBioeconomic()` retrieve or assign the
#'   `Bioeconomic` component of a [Fleet()] object.
#' - Accessors such as `Revenue()` and `Cost()` retrieve individual
#'   components of a [Bioeconomic()] object.
#' - Replacement functions (e.g. `Revenue<-`) update the corresponding
#'   component and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Bioeconomic()].
#'
#' @name Bioeconomic-accessors
NULL

#' @rdname Bioeconomic-accessors
#' @export
GetBioeconomic <- function(Fleet) {
  CheckClass(Fleet, "fleet", "Fleet")
  Fleet@Bioeconomic
}

#' @rdname Bioeconomic-accessors
#' @export
SetBioeconomic <- function(Fleet, Bioeconomic) {
  CheckClass(Fleet, "fleet", "Fleet")
  CheckClass(Bioeconomic, "bioeconomic", "Bioeconomic")
  Fleet@Bioeconomic <- Bioeconomic
  methods::validObject(Fleet)
  Fleet
}

#' @rdname Bioeconomic-accessors
#' @export
Revenue <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Revenue
}

#' @rdname Bioeconomic-accessors
#' @export
`Revenue<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Revenue <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic-accessors
#' @export
Cost <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Cost
}

#' @rdname Bioeconomic-accessors
#' @export
`Cost<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Cost <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic-accessors
#' @export
Investment <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Investment
}

#' @rdname Bioeconomic-accessors
#' @export
`Investment<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Investment <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic-accessors
#' @export
Disinvestment <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Disinvestment
}

#' @rdname Bioeconomic-accessors
#' @export
`Disinvestment<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Disinvestment <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic-accessors
#' @export
Depreciation <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Depreciation
}

#' @rdname Bioeconomic-accessors
#' @export
`Depreciation<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Depreciation <- value
  methods::validObject(x)
  x
}


#' @rdname Bioeconomic-accessors
#' @export
Discount <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Discount
}

#' @rdname Bioeconomic-accessors
#' @export
`Discount<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Discount <- value
  methods::validObject(x)
  x
}




