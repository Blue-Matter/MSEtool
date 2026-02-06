#' Catchability
#'
#' Construct a [Catchability()] object defining (potentially time-varying)
#' catchability assumptions.
#'
#' @param Efficiency Gear efficiency (q).
#' @param qCV Coefficient of variation for catchability.
#' @param qInc Incremental changes in catchability.
#' @param Misc Miscellaneous metadata.
#'
#' @details
#' The `Catchability` class represents assumptions about the
#' efficiency of fishing gear or surveys in capturing fish.
#'
#' Catchability may be specified as fixed values, time-varying
#' arrays, or stochastic processes depending on model structure.
#'
#' Individual components may be accessed or modified using
#' accessor and replacement functions such as [Efficiency()],
#' [qCV()], and [qInc()].
#'
#' @return A [Catchability()] object.
#'
#' @seealso
#' [Efficiency()], [qCV()], [qInc()]
#'
#'
#' @export
Catchability <- function(Efficiency = NULL,
                         qCV = NULL,
                         qInc = NULL,
                         Misc = list()) {
  
  ## ---- Empty constructor ----
  if (missing(Efficiency) &&
      missing(qCV) &&
      missing(qInc) &&
      missing(Misc)) {
    
    object <- methods::new("catchability")
    methods::validObject(object)
    return(object)
  }
  
  object <- methods::new(
    "catchability",
    Efficiency = Efficiency,
    qCV = qCV,
    qInc = qInc,
    Misc = Misc
  )
  
  methods::validObject(object)
  object
}

#' Catchability accessors and assignment functions
#'
#' Functions for accessing and modifying a [Catchability()] object.
#'
#' @param x A [Catchability()] object.
#' @param value Replacement value.
#'
#' @details
#' Accessors retrieve individual components of a `Catchability`
#' object. Replacement functions update components and validate
#' the object.
#'
#' Conceptual details and valid inputs are documented in
#' [Catchability()].
#'
#' @name Catchability-accessors
NULL

#' @rdname Catchability-accessors
#' @export
Efficiency <- function(x) {
  CheckClass(x, "catchability", "x")
  x@Efficiency
}

#' @rdname Catchability-accessors
#' @export
`Efficiency<-` <- function(x, value) {
  CheckClass(x, "catchability", "x")
  x@Efficiency <- value
  methods::validObject(x)
  x
}

#' @rdname Catchability-accessors
#' @export
qCV <- function(x) {
  CheckClass(x, "catchability", "x")
  x@qCV
}

#' @rdname Catchability-accessors
#' @export
`qCV<-` <- function(x, value) {
  CheckClass(x, "catchability", "x")
  x@qCV <- value
  methods::validObject(x)
  x
}

#' @rdname Catchability-accessors
#' @export
qInc <- function(x) {
  CheckClass(x, "catchability", "x")
  x@qInc
}

#' @rdname Catchability-accessors
#' @export
`qInc<-` <- function(x, value) {
  CheckClass(x, "catchability", "x")
  x@qInc <- value
  methods::validObject(x)
  x
}



