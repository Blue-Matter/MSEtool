#' CompObs Constructor and Accessors
#'
#' Construct a [compobs-class] object defining the observation error structure
#' for age or length composition data, or access and replace composition
#' observation slots of an [obs-class] object.
#'
#' @param SampleSize `numeric` or `NULL`. Nominal sample size (number of fish
#'   aged or measured per year), used as the count argument of the final
#'   multinomial draw. `NULL` (default) suppresses composition data generation
#'   for this data type entirely. Accepted input forms:
#'   - Scalar: constant across all simulations and years.
#'   - Length-2 vector `c(lower, upper)`: bounds of a Uniform distribution
#'     from which one value per simulation is drawn, constant across years.
#'   - Named matrix with dimensions `Sim` and/or `Year`: specifying values at
#'     change-point years, expanded to `[nSim x nYear]` by
#'     [PopulateCompObs()]. See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) 
#'     for the full matrix input convention.
#'
#' @param ESS `numeric` or `NULL`. Effective sample size, which scales the
#'   Dirichlet-Multinomial concentration vector (see Details). Controls
#'   stochastic variability in the composition draw independently of
#'   `SampleSize`. Accepted input forms are identical to those for `SampleSize`
#'   above. `NULL` (default) uses `SampleSize` as the effective sample size.
#'   Typically `ESS <= SampleSize`; values of `ESS < SampleSize` produce
#'   overdispersion relative to a pure multinomial with `SampleSize` draws.
#'
#' @param Theta `numeric` or `NULL`. Dirichlet-Multinomial dispersion
#'   parameter in `(0, 1]`. `Theta = 1` (default) recovers the standard
#'   multinomial. Values less than 1 produce overdispersed compositions, with
#'   `Theta -> 0` giving maximum overdispersion. Accepted input forms are
#'   identical to those for `SampleSize` above.
#'
#' @param Years `numeric` vector or `NULL`. Calendar years of observed
#'   composition data to use during conditioning via [ConditionObs_Comp()]. Only
#'   relevant when real composition data are supplied in `OM@Data`; has no
#'   effect during simulation-only runs. `NULL` (default) uses all available
#'   historical years during conditioning.
#'
#' @param Shift `numeric` or `NULL`. Systematic per-bin offset on the
#'   log-concentration scale applied to the Dirichlet concentration vector
#'   before drawing, capturing directional bias between observed and
#'   OM-predicted compositions. The concentration for bin \eqn{b} is scaled as
#'   \eqn{\alpha_b' = \mathrm{ESS} \times q_b \times \exp(\mathrm{Shift}_b)},
#'   so `Shift = 0` leaves a bin unchanged, positive values inflate its
#'   concentration, and negative values deflate it. Because the transformation
#'   is log-multiplicative, `Shift` cannot produce invalid (negative)
#'   concentrations regardless of magnitude. `NULL` (default) applies no
#'   shift. Accepted input forms:
#'   - Scalar: constant offset across all simulations, years, and bins.
#'   - Vector of length `nBin`: bin-specific offset, constant across
#'     simulations and years.
#'   - Named matrix with any subset of `Sim`, `Year`, and `Bin` dimensions,
#'     with change-point years: expanded to `[nSim x nYear x nBin]` by
#'     [PopulateCompObs()].
#'   - Full `[nSim x nYear x nBin]` array.
#'   In conditioning mode, `Shift` is populated internally by
#'   [ConditionObs_Comp()] from the mean per-bin log-concentration residual across
#'   historical years and should not be set by the user.
#'
#' @param Misc `list`. Reserved for internal use. Default `list()`.
#'
#' @details
#' ## Composition generation model
#'
#' Let **q** be the OM-predicted composition vector (length `nBin`, sums to 1)
#' for a given simulation, year, and fleet. The observation error model
#' proceeds as follows:
#'
#' **Step 1 — Base concentration vector**
#'
#' \deqn{\alpha = \mathrm{ESS} \times \mathbf{q}}
#'
#' **Step 2 — Apply log-concentration offset (if `Shift` non-`NULL`)**
#'
#' \deqn{\alpha_b' = \alpha_b \times \exp(\mathrm{Shift}[\mathrm{sim}, \mathrm{year}, b])}
#'
#' When `Shift` is `NULL`, \eqn{\alpha' = \alpha}.
#'
#' **Step 3 — Dirichlet draw**
#'
#' \deqn{\mathbf{p}^* \sim \mathrm{Dirichlet}(\alpha' / \mathrm{Theta})}
#'
#' where `Theta` \eqn{\in (0, 1]} controls overdispersion. `Theta = 1`
#' recovers the standard Dirichlet with concentration \eqn{\alpha'}.
#' Smaller values of `Theta` shrink the total concentration and increase
#' variance.
#'
#' **Step 4 — Multinomial draw**
#'
#' \deqn{\mathrm{obs} \sim \mathrm{Multinomial}(\mathrm{SampleSize},\, \mathbf{p}^*)}
#'
#' ## Conditioning mode
#'
#' When real composition data are supplied via `OM@Data`, [ConditionObs_Comp()]
#' compares observed compositions with OM-predicted compositions across the
#' historical years specified in `Years`, and populates `Shift` (mean per-bin
#' log-concentration residual) as well as estimating an appropriate `ESS` and
#' `Theta`. Users should not set these slots manually when conditioning on
#' real data.
#'
#' Note: when observed compositions differ substantially from OM-predicted
#' compositions (e.g. a length distribution shifted far left or right, or
#' with markedly different spread), the resulting `Shift` values will be
#' large. This is treated as observation error rather than model
#' mis-specification — an assumption that should be evaluated carefully. See
#' the technical manual for a full discussion.
#'
#' ## Attaching to an Obs object
#'
#' ```r
#' obs <- Obs()
#' LandingsAtAge(obs)  <- CompObs(SampleSize = 200, ESS = 50)
#' LandingsAtSize(obs) <- CompObs(SampleSize = 150, ESS = 40, Theta = 0.5)
#' ```
#'
#' @return
#' - `CompObs()` returns a [compobs-class] object.
#' - `LandingsAtAge()`, `DiscardsAtAge()`, `LandingsAtSize()`,
#'   `DiscardsAtSize()` return the corresponding [compobs-class] slot from
#'   an [obs-class] object `x`.
#' - Their replacement forms return `x` with the corresponding slot updated
#'   and the object revalidated.
#'
#' @seealso
#' - [compobs-class] for the class definition and slot descriptions.
#' - [PopulateCompObs()] for the population function.
#' - [ConditionObs_Comp()] for the conditioning function.
#' - [Obs()] for the enclosing observation model constructor.
#'
#' @family obs
#'
#' @examples
#' # Default multinomial draw: 200 fish sampled, ESS = 50
#' co <- CompObs(SampleSize = 200, ESS = 50)
#'
#' # With Dirichlet-Multinomial overdispersion (Theta = 0.5)
#' co_dm <- CompObs(SampleSize = 200, ESS = 50, Theta = 0.5)
#'
#' # Stochastic sample size drawn from Uniform(150, 250) across simulations
#' co_stoch <- CompObs(SampleSize = c(150, 250), ESS = 50)
#'
#' # Bin-specific shift (e.g. 7 age classes): inflate older ages
#' co_shift <- CompObs(
#'   SampleSize = 200,
#'   ESS        = 50,
#'   Shift      = c(-2, -1, 0, 0, 1, 2, 3)
#' )
#'
#' # Attach to an obs object
#' obs <- Obs(
#'   LandingsAtAge  = CompObs(SampleSize = 200, ESS = 50),
#'   LandingsAtSize = CompObs(SampleSize = 150, ESS = 40, Theta = 0.5)
#' )
#'
#' @include class-unions.R
#' @name CompObs
#' @export
CompObs <- function(SampleSize = NULL,
                    ESS        = NULL,
                    Theta      = NULL,
                    Years      = NULL,
                    Shift      = NULL,
                    Misc       = list()) {
  .Object <- methods::new("compobs")
  if (!is.null(SampleSize)) .Object@SampleSize <- SampleSize
  if (!is.null(ESS))        .Object@ESS        <- ESS
  if (!is.null(Theta))      .Object@Theta      <- Theta
  if (!is.null(Years))      .Object@Years      <- Years
  if (!is.null(Shift))      .Object@Shift      <- Shift
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}