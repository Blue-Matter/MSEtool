#' ImpSlot Constructor and Accessors
#'
#' Construct an [impslot-class] object representing implementation uncertainty
#' for a single management control, or access and replace individual slots.
#'
#' @param Mean Numeric array or list of arrays, or `NULL` (default). Mean
#'   implemented fraction of the management recommendation (e.g. `1` = perfect
#'   compliance with the TAC). When `NULL`, the slot is left at its class
#'   default.
#' @param SD Numeric array or list of arrays, or `NULL` (default). Standard
#'   deviation of implementation error around `Mean`. When `NULL`, the slot is
#'   left at its class default.
#' @param Compliance Numeric array or list of arrays, or `NULL` (default).
#'   Compliance rate, i.e. the proportion of the fleet that adheres to the
#'   management control. Values should be in \eqn{[0, 1]}. When `NULL`, the
#'   slot is left at its class default.
#' @param Error Numeric array or list of arrays, or `NULL` (default). Realised
#'   implementation error. When supplied directly, `Mean`, `SD`, and
#'   `Compliance` are stored but `Error` is not re-derived from them during
#'   simulation — the user-supplied values are used as-is. When `NULL`, `Error`
#'   is stochastically generated from `Mean`, `SD`, and `Compliance` at
#'   simulation time.
#' @param Misc List. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Mean, SD, and Compliance
#' `Mean` and `SD` together parameterise a lognormal (or other distributional)
#' implementation error applied to the management recommendation each
#' simulation year. `Compliance` scales the effective error by the fraction of
#' the fleet that actually adheres to the control; the remainder is assumed to
#' operate without constraint.
#'
#' ## Error
#' If `Error` is supplied, it is stored directly in the slot and used during
#' simulation without further stochastic derivation. This is useful for
#' conditioning on historically observed deviations or for deterministic
#' sensitivity runs. If `Error` is `NULL` (the default), it is generated from
#' `Mean`, `SD`, and `Compliance` at simulation time.
#'
#' ## Placeholder status
#' The [impslot-class] and its enclosing [imp-class] are currently placeholders.
#' Dimensional requirements and validation rules will be formalised in a future
#' release.
#'
#' @return
#' - `ImpSlot()` returns a new [impslot-class] object.
#' - `Mean()`, `SD()`, `Compliance()`, `Error()` return the corresponding slot
#'   from an [impslot-class] object `x`.
#' - `Mean<-`, `SD<-`, `Compliance<-`, `Error<-` return `x` with the named
#'   slot replaced.
#'
#' @seealso
#' - [impslot-class] for the class definition.
#' - [Imp()] for the enclosing constructor and accessor.
#' - [TAC()], [Effort()], [Size()] for slot-level accessors on
#'   [imp-class] objects.
#'
#' @family imp
#'
#' @examples
#' # Empty impslot
#' sl <- ImpSlot()
#'
#' # With Mean and SD specified
#' sl <- ImpSlot(Mean = 1, SD = 0.1, Compliance = 0.9)
#'
#' # Supplying Error directly (bypasses stochastic generation)
#' sl <- ImpSlot(Error = array(rnorm(100, 1, 0.05), dim = c(10, 10)))
#'
#' @name ImpSlot
#' @export
ImpSlot <- function(Mean       = NULL,
                    SD         = NULL,
                    Compliance = NULL,
                    Error      = NULL,
                    Misc       = list()) {
  .Object <- methods::new("impslot")
  if (!is.null(Mean))       .Object@Mean       <- Mean
  if (!is.null(SD))         .Object@SD         <- SD
  if (!is.null(Compliance)) .Object@Compliance <- Compliance
  if (!is.null(Error))      .Object@Error      <- Error
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}
