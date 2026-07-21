#' The `srr` S4 Class
#'
#' Defines the stock-recruitment relationship (SRR) and recruitment variability
#' for a [stock-class] object. Objects are typically created via [SRR()], which
#' documents all parameters and validates inputs. An `srr` object is required
#' for all [stock-class] objects.
#'
#' @slot Pars `list`. Named list containing the steepness parameter for the
#'   chosen SRR model. For `BevertonHolt`: `list(h = ...)`. For `Ricker`:
#'   `list(hR = ...)`. For `HockeyStick`: `list(Shinge = ...)`. `R0` is never
#'   placed in `Pars`; it has its own dedicated slot. See [SRRModels()] and
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#'   accepted input formats.
#' @slot Model `function` or `character(1)`. SRR model identifier. Defaults
#'   to `"BevertonHolt"`. See [SRRModels()] for available models.
#' @slot R0 `array` or `numeric`. Unfished equilibrium recruitment. May be a
#'   scalar (same across all simulations), a length-2 bounds vector (sampled
#'   from `Uniform(lower, upper)` once per simulation), a length-`nSim`
#'   vector, or a `Sim x Year` array for a deliberately time-varying R0 (e.g.
#'   to model a regime shift in carrying capacity). Interpreted in units of
#'   `Units` (e.g., `Units = 1000` means R0 is in thousands of fish). A
#'   constant R0 only sets the absolute scale of numbers in [hist-class]
#'   output and does not otherwise affect population dynamics. A
#'   time-varying R0 does affect dynamics: the spawning production per
#'   recruit implied by steepness is fixed at its initial-year value (so
#'   time-varying life-history parameters like `M` don't cause it to drift),
#'   but `R0` itself is read for the current year, so a change in `R0`
#'   immediately rescales the stock-recruitment curve from that year on.
#' @slot SD `array` or `numeric`. Log-space standard deviation of recruitment
#'   deviations. May be a scalar, length-2 bounds vector, or length-`nSim`
#'   vector. Fixed across years (currently not time-varying). When `NULL`,
#'   recruitment is deterministic.
#' @slot AC `array` or `numeric`. Lag-1 autocorrelation of log-space
#'   recruitment deviations. Follows the same length conventions as `SD`.
#'   Fixed across years (currently not time-varying). Defaults to `0`
#'   (no autocorrelation) when `NULL`.
#' @slot SPFrom `character(1)` or `numeric(1)`. Identifies which stock's
#'   spawning production drives this stock's recruitment. Defaults to self
#'   (stock recruits from its own spawning production). For multi-stock models,
#'   may be a stock name (`character`) or 1-based integer index (`numeric`).
#' @slot TruncSD `numeric(1)`. Number of standard deviations at which the
#'   log-normal recruitment deviation distribution is truncated. Default `2`.
#' @slot RecDevInit `matrix`. Recruitment deviations (`Sim × Age`) used to
#'   initialise the historical age structure. Covers all age classes except the
#'   minimum age, whose deviation appears as the first entry of `RecDevHist`.
#'   Generated automatically during [Populate()] from `SD` and `AC` if `NULL`.
#' @slot RecDevHist `matrix`. Recruitment deviations (`Sim × nHistTS`) for
#'   historical time steps. The first column corresponds to the minimum age
#'   class in the initial year. Generated automatically if `NULL`.
#' @slot RecDevProj `matrix`. Recruitment deviations (`Sim × nProjTS`) for
#'   projection time steps. Generated automatically if `NULL`.
#' @slot SpawnTimeFrac `numeric(1)`. Fraction of the time step elapsed before
#'   spawning occurs. Controls how much mortality is applied to the population
#'   before spawning numbers are counted: numbers at spawning are
#'   `N × exp(-Z × SpawnTimeFrac)`. `0` = spawning at the start of the step
#'   (no mortality applied first); `1` = spawning at the end of the step (full
#'   within-step Z applied first). Default `0`.
#' @slot SpawnLag `numeric(1)` or `NULL`. Number of timesteps between the
#'   spawning production evaluation and recruitment (first appearance of the
#'   youngest age class). When `NULL` (default), the lag is derived
#'   automatically from `min(Ages@Classes)` and `Seasons` as
#'   `round(min(Ages@Classes) * Seasons)`. Set explicitly when the spawn season
#'   differs from what `min(Ages@Classes)` implies — for example, in SS3 models
#'   where `Spawn_month` and `birthseas` are decoupled.
#' @slot RelRecFun `function` or `character(1)`. Relative recruitment function
#'   giving equilibrium recruitment relative to `R0` as a function of
#'   spawning-per-recruit (SPR). For built-in models, set automatically to
#'   `paste0(Model, "_RelRec")` (e.g., `"BevertonHolt_RelRec"`) during
#'   [Populate()]. For custom SRR models, must be supplied by the user with
#'   signature `function(Pars, SPR)`.
#' @slot Units `numeric(1)`. Scaling factor for recruitment. `Units = 1`
#'   (default) means `R0` is in absolute numbers of fish. `Units = 1000` means
#'   `R0` is in thousands of fish. Does not affect internal calculations — used
#'   only to set the interpretation of numbers in [hist-class] output.
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use [SRR()]
#' instead, which validates inputs and sets sensible defaults.
#'
#' Recruitment deviations are generated in log-space with standard deviation
#' `SD` and lag-1 autocorrelation `AC`, truncated at `TruncSD` standard
#' deviations. `SD` and `AC` are currently fixed across years; time-varying
#' process variance is not yet supported.
#'
#' `RecDevInit`, `RecDevHist`, and `RecDevProj` may be supplied directly to
#' override the internally generated deviations — for example, to condition
#' the operating model on observed recruitment indices.
#'
#' @seealso
#' - [SRR()] for the constructor and accessor functions.
#' - [SRRModels()] for available stock-recruitment models and their required
#'   parameters.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#' - [Fecundity()] for the spawning production used as input to the SRR.
#' - [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#'   accepted `Pars` input formats.
#'
#' @family srr
#'
#' @include class-unions.R
#' @name srr-class
setClass(
  "srr",
  slots = c(
    Pars          = "list",
    Model         = "fun.char",
    R0            = "num.array.null",
    SD            = "num.array.null",
    AC            = "num.array.null",
    SPFrom        = "char.num",
    TruncSD       = "num.null",
    RecDevInit    = "num.array.list",
    RecDevHist    = "num.array.list",
    RecDevProj    = "num.array.list",
    SpawnTimeFrac = "numeric",
    SpawnLag      = "num.null",
    RelRecFun     = "fun.char",
    Units         = "numeric",
    Misc          = "list"
  )
)


setValidity("srr", function(object) {
  TRUE
})
