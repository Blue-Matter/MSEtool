#' Catchability
#'
#' Construct a [catchability-class] object defining fishing gear/vessel efficiency
#'
#' @param Efficiency Gear efficiency (q).
#' @param qCV Coefficient of variation for catchability.
#' @param qInc Incremental changes in catchability.
#' @param Misc Miscellaneous metadata.
#'
#' @details
#' The `Catchability` objects represents assumptions about the
#' efficiency of fishing gear or surveys in capturing fish.
#'
#' Catchability may be specified as fixed values, time-varying
#' arrays, or stochastic processes depending on model structure.
#'
#' Individual components may be accessed or modified using
#' accessor and replacement functions such as [Efficiency()],
#' [qCV()], and [qInc()].
#'
#' A `Catchability` object can be attached to a [Fleet()] using `Catchability(Fleet) <- MyCatchability` and
#' retrieved using `MyCatchability <- Catchability(Fleet)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Efficiency()], [qCV()], and [qInc()].
#' 
#' `r TechManLink()`
#' 
#' @return An [catchability-class] object
#'
#' @seealso
#' [Fleet()]
#'
#'
#' @export
Catchability <- function(Efficiency = NULL,
                         qCV = NULL,
                         qInc = NULL,
                         Misc = list()) {
  
  

  
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


#' @rdname Catchability 
#' @export
Efficiency <- function(x) {
  CheckClass(x, "catchability", "x")
  x@Efficiency
}

#' @rdname Catchability 
#' @export
`Efficiency<-` <- function(x, value) {
  CheckClass(x, "catchability", "x")
  x@Efficiency <- value
  methods::validObject(x)
  x
}

#' @rdname Catchability 
#' @export
qCV <- function(x) {
  CheckClass(x, "catchability", "x")
  x@qCV
}

#' @rdname Catchability 
#' @export
`qCV<-` <- function(x, value) {
  CheckClass(x, "catchability", "x")
  x@qCV <- value
  methods::validObject(x)
  x
}

#' @rdname Catchability 
#' @export
qInc <- function(x) {
  CheckClass(x, "catchability", "x")
  x@qInc
}

#' @rdname Catchability 
#' @export
`qInc<-` <- function(x, value) {
  CheckClass(x, "catchability", "x")
  x@qInc <- value
  methods::validObject(x)
  x
}



