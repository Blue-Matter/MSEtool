#' Catchability
#'
#' Construct and manipulate a [catchability-class] object defining fishing
#' gear or vessel efficiency.
#'
#' @param Efficiency Numeric or array. Gear or vessel catchability coefficient
#'   (`q`) relating fishing effort to fishing mortality.
#' @param qCV Numeric. Coefficient of variation for catchability, used to
#'   generate stochastic variation in `q` across simulations (projection only).
#' @param qInc Numeric. Incremental annual change in catchability, used to
#'   model trends in gear efficiency over time (projection only).
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [catchability-class] object, or a [fleet-class] object for
#'   `Catchability<-`.
#' @param value For `Catchability<-`: a [catchability-class] object. For slot
#'   replacement functions: the new value for the corresponding slot.
#'
#' @details
#' A [catchability-class] object represents assumptions about the efficiency
#' of fishing gear or surveys in capturing fish. Catchability may be specified
#' as fixed values, time-varying arrays, or stochastic processes depending on
#' model structure.
#'
#' A `Catchability` object can be attached to a [Fleet()] with
#' `Catchability(Fleet) <- MyCatchability` and retrieved with
#' `Catchability(Fleet)`.
#'
#' Individual slots may be accessed or modified using the accessor and
#' replacement functions documented here: [Efficiency()], [qCV()], [qInc()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Catchability()` returns a [catchability-class] object. If `Efficiency`
#'   is a [fleet-class] object, the `Catchability` slot of that fleet is
#'   returned.
#' - `Catchability<-` returns `x` with the `Catchability` slot replaced.
#' - `Efficiency()`, `qCV()`, `qInc()` return the corresponding slot from `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso [catchability-class], [Fleet()]
#'
#' @examples
#' c <- Catchability(Efficiency = 0.01, qCV = 0.1)
#' Efficiency(c)
#' Efficiency(c) <- 0.02
#' qCV(c)
#' qInc(c)
#'
#' @export
Catchability <- function(Efficiency = NULL,
                         qCV = NULL,
                         qInc = NULL,
                         Misc = list()) {
  

  if (inherits(Efficiency, 'fleet'))
    return(Efficiency@Catchability) 

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
`Catchability<-` <- function(x, value) {
  AssignSlot(x, value, 'Catchability')
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



