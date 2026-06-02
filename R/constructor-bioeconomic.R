#' Bioeconomic
#'
#' Construct and manipulate a [bioeconomic-class] object storing revenue, cost,
#' and investment dynamics for fleet-level or stock-level bioeconomic analyses.
#'
#' **Note:** The [bioeconomic-class] is not currently used by the MSE
#' framework. It is included for future development and is available for
#' custom analyses outside the standard model loop.
#'
#' @param Revenue Numeric array or `NULL`. Revenue by simulation, fleet, and
#'   year. Default `NULL`.
#' @param Cost Numeric or `NULL`. Operating cost per unit of effort. Default
#'   `NULL`.
#' @param Investment Numeric or `NULL`. Cost of adding a unit of effort.
#'   Default `NULL`.
#' @param Disinvestment Numeric or `NULL`. Cost of removing a unit of effort.
#'   Default `NULL`.
#' @param Depreciation Numeric or `NULL`. Depreciation rate of effort units.
#'   Default `NULL`.
#' @param Discount Numeric or `NULL`. Discount factor applied to future values.
#'   Default `NULL`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [bioeconomic-class] object, or a [fleet-class] object for
#'   `Bioeconomic<-`.
#' @param value For `Bioeconomic<-`: a [bioeconomic-class] object. For slot
#'   replacement functions: the new value for the corresponding slot.
#'
#' @details
#' A [bioeconomic-class] object stores parameters and outputs for fleet-level
#' bioeconomic modelling, including revenue, effort costs, and discounting.
#' All slots are optional and default to `NULL`.
#'
#' This class is not currently used by the MSE framework. It is included for
#' future development.
#'
#' ## Attaching to a Fleet
#'
#' A [bioeconomic-class] object can be attached to a [Fleet()] with
#' `Bioeconomic(Fleet) <- MyBioeconomic` and retrieved with
#' `Bioeconomic(Fleet)`.
#'
#' Individual slots may be accessed or modified using [Revenue()], [Cost()],
#' [Investment()], [Disinvestment()], [Depreciation()], and [Discount()].
#'
#' @return
#' - `Bioeconomic()` returns a [bioeconomic-class] object. If `Revenue` is a
#'   [fleet-class] object, the `Bioeconomic` slot of that fleet is returned.
#' - `Bioeconomic<-` returns `x` with the `Bioeconomic` slot replaced by
#'   `value`.
#' - `Revenue()`, `Cost()`, `Investment()`, `Disinvestment()`,
#'   `Depreciation()`, `Discount()` return the corresponding slot from `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso
#' - [bioeconomic-class] for the class definition and slot-level
#'   documentation.
#' - [Fleet()] for the enclosing fleet constructor.
#'
#' @family fleet
#'
#' @examples
#' b <- Bioeconomic(Cost = 100, Discount = 0.05)
#' Cost(b)
#' Cost(b) <- 200
#' Discount(b)
#'
#' # Attach to a Fleet
#' f <- Fleet(Name = "Trawl")
#' Bioeconomic(f) <- Bioeconomic(Cost = 100, Investment = 500, Discount = 0.05)
#' Cost(Bioeconomic(f))
#' Discount(Bioeconomic(f))
#'
#' @export
Bioeconomic <- function(Revenue       = NULL,
                        Cost          = NULL,
                        Investment    = NULL,
                        Disinvestment = NULL,
                        Depreciation  = NULL,
                        Discount      = NULL,
                        Misc          = list()) {
  
  if (methods::is(Revenue, "fleet"))
    return(Revenue@Bioeconomic)
  
  methods::new(
    "bioeconomic",
    Revenue       = Revenue,
    Cost          = Cost,
    Investment    = Investment,
    Disinvestment = Disinvestment,
    Depreciation  = Depreciation,
    Discount      = Discount,
    Misc          = Misc
  )
}

#' @rdname Bioeconomic
#' @export
`Bioeconomic<-` <- function(x, value) {
  AssignSlot(x, value, 'Bioeconomic')
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