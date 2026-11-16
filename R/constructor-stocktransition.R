#' Stock Transition Constructor
#'
#' Construct a [stocktransition-class] object defining an age- or
#' length-dependent reclassification of individuals from one stock's
#' numbers-at-age into another's.
#'
#' @param From `character(1)` or `numeric(1)`. Source stock, specified by
#'   stock name or 1-based integer index.
#' @param To `character(1)` or `numeric(1)`. Destination stock, specified by
#'   stock name or 1-based integer index. Must differ from `From`.
#' @param Frac `array` or `NULL`. Cumulative "fraction remaining in `From` by
#'   age" curve - `[sim, age]` or `[sim, age, year]`, values in `[0, 1]`.
#'   Starts near `1` at the youngest ages (almost no one has transitioned
#'   yet) and decreases toward `0` at older ages (nearly everyone has
#'   transitioned out of `From`). Default `NULL`. See *Transition in the
#'   Plus Group* in Details.
#' @param FracAtLength `numeric`, `array`, or `NULL`. Cumulative "fraction
#'   remaining in `From` by length" curve - a numeric vector of length
#'   `length(Classes)`, or an array with named dimensions `Sim`, `Class`,
#'   and `Year`, values in `[0, 1]`. Alternative to `Frac`; only one of
#'   `Frac` and `FracAtLength` can be supplied. Default `NULL`. See
#'   *Length-Based Transitions* in Details.
#' @param Classes `numeric` or `NULL`. Lower bounds of the length bins for
#'   `FracAtLength`. Default `NULL` uses `Length@Classes` of the `From`
#'   stock.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x An [om-class] object, for `Herm<-`.
#' @param value A list of [stocktransition-class] objects, or `NULL`, for
#'   `Herm<-`.
#'
#' @return A [stocktransition-class] object. When `From` is an [om-class],
#'   [hist-class], or [mse-class] object (and `To` is not supplied), `Herm()`
#'   instead acts as a pass-through accessor and returns that object's
#'   `Herm` slot (a list of `stocktransition` objects, or `NULL`). `Herm<-`
#'   returns `x` with the `Herm` slot replaced; it can only be replaced in
#'   [om-class] objects.
#'
#' @details
#' 
#' `Herm()` is the general mechanism, not exclusive to biological
#' hermaphroditism, but it is named and documented around its primary
#' motivating use case, sequential hermaphroditism (e.g. protogynous wrasse
#' or grouper changing sex from female to male at a given age). The same
#' mechanism applies to any scenario shaped as "individuals move from one
#' `stock` object's bookkeeping into another's as they age" (ontogenetic
#' stage transitions modelled as separate stocks).
#' 
#' `om@Herm` takes a list of `stocktransition` objects (one per `From`/`To`
#' pair), supporting multiple simultaneous transition relationships.
#'
#' ## Transition in the Plus Group
#'
#' `Frac` is converted to a per-age hazard, the fraction of `From` at age
#' `a - 1` that moves to `To` on reaching age `a`:
#' `1 - Frac[, a] / Frac[, a - 1]`. When the oldest age of `From` is a plus
#' group, its hazard `1 - Frac[, maxage] / Frac[, maxage - 1]` is applied
#' every year to the whole plus group, including fish already in it.
#'
#' With a plus-group hazard strictly between 0 and 1, fish in the plus group
#' keep moving from `From` to `To` every year they survive, so the split of
#' the plus group between the two stocks depends on how long fish survive
#' in it, and changes with fishing mortality. Age-structured models that set
#' the sex ratio at age from a fixed schedule do
#' not have this behaviour, and cannot be reproduced exactly. [Simulate()]
#' gives a warning in this case when more than 0.1% of `From` remains at the
#' plus group (`Frac[, maxage] > 0.001`), except for length-based transitions
#' (`FracAtLength`).
#'
#' To stop transitions within the plus group, set `Frac[, maxage]` equal to
#' `Frac[, maxage - 1]` (no transition in the plus group), or to `0`
#' (transition complete by the plus group).
#'
#' ## Length-Based Transitions
#'
#' When `FracAtLength` is supplied, [Populate()] converts it to `Frac` with
#' the age-length key (`Length@ALK`) of the `From` stock, as for other
#' at-length schedules (e.g. [Maturity()]):
#' `Frac[, a, ] = sum_L ALK[, a, L, ] * FracAtLength[, L, ]`. Time-varying
#' growth gives a time-varying `Frac`.
#'
#' The model does not track length within an age class, so `Frac` is the
#' expected fraction remaining in `From` at age given the unfished length
#' distribution at age. Size-selective fishing, which removes the larger
#' individuals within an age class, does not change the transition.
#'
#' Individuals cannot move back from `To` to `From`, so ages where the
#' converted `Frac` increases (e.g. when the CV of length-at-age increases
#' steeply with age) have a hazard of 0. [Populate()] gives a warning in
#' this case.
#'
#' The converted `Frac` is usually above 0 at the plus group, so the plus
#' group keeps transitioning every year (see *Transition in the Plus Group*).
#' This is expected for length-based transitions, as individuals in the
#' plus group continue to grow, and [Simulate()] does not give the plus group
#' warning.
#'
#' @examples
#' # Protogynous transition: female stock transitions into male stock,
#' # onset around age 5 with a step-function fraction-remaining curve
#' Frac <- array(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0), dim = c(1, 10))
#' herm <- Herm(From = "Female", To = "Male", Frac = Frac)
#'
#' # equivalent, specified by stock index
#' herm <- Herm(From = 1, To = 2, Frac = Frac)
#'
#' # length-based transition: logistic decline in the fraction remaining
#' # female, centred on 60 cm
#' Classes <- seq(0, 120, by = 5)
#' herm <- Herm(From = "Female", To = "Male",
#'              FracAtLength = 1 / (1 + exp((Classes - 60) / 4)),
#'              Classes = Classes)
#'
#' @seealso [stocktransition-class]
#' @family om
#'
#' @export
Herm <- function(From, To, Frac = NULL, FracAtLength = NULL, Classes = NULL,
                 Misc = list()) {

  if (missing(From))
    cli::cli_abort("{.arg From} is required")

  if (missing(To) && (inherits(From, "om") || inherits(From, "hist") || inherits(From, "mse")))
    return(.IsHist(From, "Herm"))

  if (missing(To))
    cli::cli_abort("{.arg From} and {.arg To} are both required")

  if (identical(From, To))
    cli::cli_abort(c(
      "x" = "{.arg From} and {.arg To} must be different stocks.",
      "i" = "Both were {.val {From}}."
    ))

  if (!is.null(Frac) && !is.null(FracAtLength))
    cli::cli_abort(c(
      "x" = "Only one of {.arg Frac} and {.arg FracAtLength} can be supplied.",
      "i" = "{.arg FracAtLength} is converted to {.arg Frac} during {.fn Populate}."
    ))

  if (!is.null(Classes) && is.null(FracAtLength))
    cli::cli_abort("{.arg Classes} is only used with {.arg FracAtLength}.")

  obj <- methods::new(
    "stocktransition",
    From         = From,
    To           = To,
    Frac         = Frac,
    FracAtLength = FracAtLength,
    Classes      = Classes,
    Misc         = Misc
  )

  methods::validObject(obj)
  obj
}

#' @rdname Herm
#' @export
`Herm<-` <- function(x, value) .AssignOMSlot(x, value, "Herm", "struct")
