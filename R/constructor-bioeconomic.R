#' Bioeconomic
#'
#' Construct and manipulate a [bioeconomic-class] object storing revenue, cost,
#' and investment dynamics for fleet-level or stock-level bioeconomic analyses.
#'
#' @param Revenue Array. Revenue by simulation, fleet, and year.
#' @param Cost Numeric. Operating cost per unit of effort.
#' @param Investment Numeric. Cost of adding a unit of effort.
#' @param Disinvestment Numeric. Cost of removing a unit of effort.
#' @param Depreciation Numeric. Depreciation rate of effort units.
#' @param Discount Numeric. Discount factor applied to future values.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [bioeconomic-class] object, or a [fleet-class] object for
#'   `Bioeconomic<-`.
#' @param value For `Bioeconomic<-`: a [bioeconomic-class] object. For slot
#'   replacement functions: the new value for the corresponding slot.
#'
#' @details
#' A [bioeconomic-class] object is not currently used by the MSE framework but
#' is available for custom analyses.
#'
#' A `Bioeconomic` object can be attached to a [Fleet()] object with
#' `Bioeconomic(Fleet) <- MyBioeconomic` and retrieved with
#' `Bioeconomic(Fleet)`.
#'
#' Individual slots may be accessed or modified using the accessor and
#' replacement functions documented here: [Revenue()], [Cost()],
#' [Investment()], [Disinvestment()], [Depreciation()], [Discount()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Bioeconomic()` returns a [bioeconomic-class] object. If `Revenue` is a
#'   [fleet-class] object, the `Bioeconomic` slot of that fleet is returned.
#' - `Bioeconomic<-` returns `x` with the `Bioeconomic` slot replaced.
#' - `Revenue()`, `Cost()`, `Investment()`, `Disinvestment()`,
#'   `Depreciation()`, `Discount()` return the corresponding slot from `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso [bioeconomic-class], [Fleet()]
#'
#' @examples
#' b <- Bioeconomic(Cost = 100, Discount = 0.05)
#' Cost(b)
#' Cost(b) <- 200
#' Discount(b)
#'
#' @export
Bioeconomic <- function(Revenue = NULL,
                        Cost = NULL,
                        Investment = NULL,
                        Disinvestment = NULL,
                        Depreciation = NULL,
                        Discount = NULL,
                        Misc = list()) {
  
  if (methods::is(Revenue, "fleet"))
    return(Revenue@Bioeconomic)
  
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

#' @rdname Bioeconomic
#' @export
`Bioeconomic<-` <- function(x, value) {
  AssignSlot(x, value, 'BioEconomic')
}

#' @rdname Bioeconomic
#' @export
Revenue <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Revenue
}

#' @rdname Bioeconomic
#' @export
`Revenue<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Revenue <- value
  methods::validObject(x)
  x
}

#' @rdname Bioeconomic
#' @export
Cost <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Cost
}

#' @rdname Bioeconomic
#' @export
`Cost<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Cost <- value
  methods::validObject(x)
  x
}

#' @rdname Bioeconomic
#' @export
Investment <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Investment
}

#' @rdname Bioeconomic
#' @export
`Investment<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Investment <- value
  methods::validObject(x)
  x
}

#' @rdname Bioeconomic
#' @export
Disinvestment <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Disinvestment
}

#' @rdname Bioeconomic
#' @export
`Disinvestment<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Disinvestment <- value
  methods::validObject(x)
  x
}

#' @rdname Bioeconomic
#' @export
Depreciation <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Depreciation
}

#' @rdname Bioeconomic
#' @export
`Depreciation<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Depreciation <- value
  methods::validObject(x)
  x
}

#' @rdname Bioeconomic
#' @export
Discount <- function(x) {
  CheckClass(x, "bioeconomic", "x")
  x@Discount
}

#' @rdname Bioeconomic
#' @export
`Discount<-` <- function(x, value) {
  CheckClass(x, "bioeconomic", "x")
  x@Discount <- value
  methods::validObject(x)
  x
}