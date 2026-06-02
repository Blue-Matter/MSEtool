#' The `compobs` S4 Class
#'
#' Defines the observation error structure for age or length composition data
#' (catch-at-age, catch-at-length). Used in the `LandingsAtAge`,
#' `DiscardsAtAge`, `LandingsAtSize`, and `DiscardsAtSize` slots of an
#' [obs-class] object. Objects are typically created via [CompObs()], which
#' documents all parameters and accepted input forms in detail.
#'
#' @slot SampleSize `numeric` or `NULL`. Nominal sample size (number of fish
#'   aged or measured) applied when drawing the final multinomial sample.
#'   Accepted input forms are documented in [CompObs()]. After population by
#'   [PopulateCompObs()], stored as a named `[nSim x nYear]` array.
#'   `NULL` (default) suppresses composition data generation for this data
#'   type; [EmptyObject()] returns `TRUE` when `SampleSize` is `NULL`.
#'
#' @slot ESS `numeric` or `NULL`. Effective sample size, which scales the
#'   Dirichlet-Multinomial concentration vector and therefore controls
#'   stochastic variability in the composition draw independently of
#'   `SampleSize`. Accepted input forms are the same as for `SampleSize`; see
#'   [CompObs()]. After population, stored as a named `[nSim x nYear]` array.
#'   `NULL` (default) uses `SampleSize` as the effective sample size.
#'
#' @slot Theta `numeric` or `NULL`. Dirichlet-Multinomial dispersion parameter
#'   in `(0, 1]`. Values less than 1 produce overdispersed compositions
#'   relative to a standard multinomial; `Theta = 1` (default) recovers the
#'   standard multinomial. Accepted input forms are the same as for
#'   `SampleSize`; see [CompObs()]. After population, stored as a named
#'   `[nSim x nYear]` array.
#'
#' @slot Years `numeric` vector or `NULL`. Calendar years of observed
#'   composition data to be used during the conditioning step (see
#'   [ConditionObs_Comp()]). `NULL` (default) uses all available historical years during
#'   conditioning.
#'
#' @slot Shift `numeric` or `NULL`. Systematic per-bin offset on the
#'   log-concentration scale applied to the Dirichlet concentration vector
#'   before drawing, capturing directional bias between observed and
#'   OM-predicted compositions. After population by [PopulateCompObs()],
#'   stored as a named `[nSim x nYear x nBin]` array. `NULL` (default) applies
#'   no shift. In conditioning mode, populated internally by [ConditionObs_Comp()]
#'   from the mean per-bin log-concentration residual across historical years.
#'
#' @slot Misc `list`. Reserved for internal use.
#'
#' @details
#' ## Composition generation model
#'
#' During simulation, [PopulateCompObs()] applies the following steps:
#'
#' 1. **`Shift` non-`NULL`**: user-specified (or conditioned) per-bin
#'    log-concentration offsets are exponentiated and applied to the
#'    OM-predicted concentration vector:
#'    \deqn{\alpha_b' = \mathrm{ESS} \times q_b \times \exp(\mathrm{Shift}_b)}
#' 2. **`Shift` `NULL`** (default): pure Dirichlet-Multinomial draw from the
#'    OM-predicted composition with concentration scaled by `ESS` and
#'    dispersion `Theta`:
#'    \deqn{\alpha' = \mathrm{ESS} \times \mathbf{q}}
#'
#' ## Note on large compositional shifts
#'
#' When observed compositions differ substantially from OM-predicted
#' compositions (e.g. a length distribution shifted far left or right, or
#' with markedly different spread), [ConditionObs_Comp()] will produce large
#' values in `Shift`. These are treated as observation error rather than model
#' mis-specification — an assumption that should be evaluated carefully.
#' See the technical manual for a full discussion of this assumption and its
#' implications for simulation performance.
#'
#'
#' @seealso
#' - [CompObs()] for the constructor and full parameter documentation.
#' - [PopulateCompObs()] for the population function.
#' - [ConditionObs_Comp()] for the conditioning function.
#' - [obs-class] for the enclosing observation model object.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name compobs-class
#' @aliases CompObs-class
setClass(
  "compobs",
  slots = c(
    SampleSize = "num.array.null",
    ESS        = "num.array.null",
    Theta      = "num.array.null",
    Years      = "num.null",
    Shift      = "num.array.null",
    Misc       = "list"
  )
)

## Deprecated alias — keeps existing CompObs objects valid
#' @export
setClass("CompObs", contains = "compobs")