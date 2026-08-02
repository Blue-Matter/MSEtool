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
#' @param Compliance Scalar, length-2 uniform bounds `c(lower, upper)`, a
#'   `[Sim x Year]` array, or `NULL` (default). Meaning depends on which
#'   control (`TAC`, `Effort`, or `Size`) this `impslot` is attached to (see
#'   Details). Values should be in \eqn{[0, 1]}. 
#' @param Error Numeric array or list of arrays, or `NULL` (default). Realised
#'   implementation error, a per simulation/year multiplicative factor
#'   applied to the advised TAC or effort before effort-solving. When
#'   supplied directly, `Mean` and `SD` are stored but `Error` is not
#'   re-derived from them during simulation. When `NULL`, `Error` is 
#'   stochastically generated from
#'   `Mean` and `SD` at simulation time (see [PopulateImpSlot()]).
#' @param Misc List. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Mean, SD, and Error
#' `Mean` and `SD` together parameterise a lognormal distribution for
#' `Error`, a general implementation-error multiplier applied to the
#' advised TAC/effort regardless of how many stocks are in the OM (e.g.
#' `Mean = 0.9` models a fleet that implements 90% of the recommended TAC
#' on average). See [PopulateImpSlot()] for exactly how `Mean`/`SD` expand
#' to `Error`.
#'
#' ## Compliance
#'
#' For `TAC`/`Effort`, only meaningful for multi-stock/multi-complex OMs,
#' where it governs how a fleet reconciles competing TAC or effort
#' recommendations across stocks.
#'
#' For `TAC` it sets how heavily exceeding this complex's TAC is penalised
#' relative to falling short of it: `0` makes exceeding it free, `0.5` weights
#' the two equally, and values approaching `1` act as a hard choke. Unset
#' defaults to `0.7`, penalising overshoot about 2.3 times more than
#' undershoot, since a TAC is a cap rather than a target. See [Imp()].
#'
#' Values close to `1` are not recommended: the penalty on falling short is
#' bounded while the penalty on exceeding is not, so a sufficiently large
#' weight makes zero effort the cheapest option and the fleet stops fishing
#' altogether.
#'
#' For `Size`, it is the fraction of the fleet adopting a newly-advised size
#' regulation.
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
#' - [TACImp()], [Effort()], [Size()] for slot-level accessors on
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
