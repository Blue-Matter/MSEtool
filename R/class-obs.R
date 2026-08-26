#' The `lifehistoryobs` S4 Class
#'
#' Defines the observation error structure for life-history parameters
#' (growth, maturity, natural mortality, etc.). Used in the `LifeHistory` slot
#' of an [obs-class] object. Objects are created via [LifeHistoryObs()].
#'
#' @slot Ages `list`. Observation error specification for age structure
#'   parameters. See [LifeHistoryObs()].
#' @slot Length `list`. Observation error specification for growth parameters.
#'   See [LifeHistoryObs()].
#' @slot Weight `list`. Observation error specification for weight-at-age or
#'   length-weight parameters. See [LifeHistoryObs()].
#' @slot NaturalMortality `list`. Observation error specification for natural
#'   mortality. See [LifeHistoryObs()].
#' @slot Maturity `list`. Observation error specification for maturity
#'   schedules. See [LifeHistoryObs()].
#' @slot Fecundity `list`. Observation error specification for fecundity
#'   parameters. See [LifeHistoryObs()].
#' @slot SRR `list`. Observation error specification for stock-recruitment
#'   parameters. See [LifeHistoryObs()].
#' @slot Spatial `list`. Observation error specification for spatial
#'   parameters. See [LifeHistoryObs()].
#' @slot Depletion `list`. Observation error specification for initial
#'   depletion. See [LifeHistoryObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' This class is a placeholder. Each slot mirrors the corresponding sub-object
#' of [lifehistorydata-class] and will eventually hold an error structure
#' (bias, CV) for the associated life-history parameter. Slots are currently
#' untyped lists and are not populated during model runs. See [LifeHistoryObs()]
#' for further details.
#'
#' @seealso
#' - [LifeHistoryObs()] for the constructor.
#' - [obs-class] for the enclosing object.
#' - [lifehistorydata-class] for the complementary observed-values class.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name lifehistoryobs-class
setClass(
  "lifehistoryobs",
  slots = c(
    Ages             = "list",
    Length           = "list",
    Weight           = "list",
    NaturalMortality = "list",
    Maturity         = "list",
    Fecundity        = "list",
    SRR              = "list",
    Spatial          = "list",
    Depletion        = "list",
    Misc             = "list"
  )
)


#' The `exploitationobs` S4 Class
#'
#' Defines the observation error structure for exploitation process parameters
#' (selectivity, retention, discard mortality). Used in the `Exploitation` slot
#' of an [obs-class] object. Objects are created via [ExploitationObs()].
#'
#' @slot Selectivity `list`. Observation error specification for selectivity.
#'   See [ExploitationObs()].
#' @slot Retention `list`. Observation error specification for retention.
#'   See [ExploitationObs()].
#' @slot DiscardMortality `list`. Observation error specification for discard
#'   mortality. See [ExploitationObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @details
#' This class is a placeholder. Each slot mirrors the corresponding sub-object
#' of [exploitationdata-class] and will eventually hold an error structure for
#' the associated exploitation parameter. Slots are currently untyped lists and
#' are not populated during model runs. See [ExploitationObs()] for further
#' details.
#'
#' @seealso
#'  - [ExploitationObs()] for the constructor.
#'  - [obs-class] for the enclosing object.
#'  - [exploitationdata-class] for the complementary observed-values class.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name exploitationobs-class
setClass(
  "exploitationobs",
  slots = c(
    Selectivity      = "list",
    Retention        = "list",
    DiscardMortality = "list",
    Misc             = "list"
  )
)


#' The `effortobs` S4 Class
#'
#' Defines the observation error structure for fishing effort data. Used in the
#' `Effort` slot of an [obs-class] object. Objects are typically created via
#' [EffortObs()], which documents all parameters in detail.
#'
#' @slot CV `numeric` or `NULL`. Coefficient of variation of effort observation
#'   error. See [EffortObs()].
#' @slot Error `numeric` array or `NULL`. Realised lognormal observation error
#'   multipliers (`nSim x nYear`). Populated internally during simulation;
#'   see [EffortObs()].
#' @slot Bias `numeric` or `NULL`. Multiplicative observation bias. See
#'   [EffortObs()].
#' @slot Years `numeric` or `NULL`. Calendar years over which the observation
#'   error applies. See [EffortObs()].
#' @slot Units `character` or `NULL`. Units of fishing effort
#'   (e.g., `"hours"`, `"trips"`). See [EffortObs()].
#' @slot Ref `numeric` array or `NULL`. Reference effort values, one per
#'   simulation. See [EffortObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @seealso [EffortObs()] for the constructor and full parameter documentation.
#'   [obs-class] for the enclosing observation model object.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name effortobs-class
setClass(
  "effortobs",
  slots = c(
    CV    = "num.array.null",
    Error = "num.array.null",
    Bias  = "num.array.null",
    Years = "num.null",
    Units = "char.null",
    Ref   = "num.array.null",
    Misc  = "list"
  )
)

setValidity("effortobs", function(object) {
  errors <- character()

  if (!is.null(object@CV) && any(object@CV < 0, na.rm = TRUE))
    errors <- c(errors, "`CV` must be non-negative")

  if (!is.null(object@Bias) && any(object@Bias <= 0, na.rm = TRUE))
    errors <- c(errors, "`Bias` must be positive")

  if (!is.null(object@Error) && any(object@Error <= 0, na.rm = TRUE))
    errors <- c(errors, "`Error` must be positive")

  if (length(errors)) errors else TRUE
})


#' The `catchobs` S4 Class
#'
#' Defines the observation error structure for landed or discarded catch data.
#' Used in the `Landings` and `Discards` slots of an [obs-class] object.
#' Objects are typically created via [CatchObs()], which documents all
#' parameters in detail.
#'
#' @slot CV `numeric` or `NULL`. Coefficient of variation of catch observation
#'   error. See [CatchObs()].
#' @slot Error `numeric` array or `NULL`. Realised lognormal observation error
#'   multipliers (`nSim x nYear`). Populated internally during conditioning or
#'   simulation; see [CatchObs()].
#' @slot Bias `numeric` or `NULL`. Multiplicative observation bias. See
#'   [CatchObs()].
#' @slot Years `numeric` or `NULL`. Calendar years over which the observation
#'   error is conditioned. See [CatchObs()].
#' @slot Units `character` or `NULL`. Units of catch (`"Biomass"` or
#'   `"Number"`). See [CatchObs()].
#' @slot Ref `numeric` array or `NULL`. Reference catch values, one per
#'   simulation. See [CatchObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @seealso
#' - [CatchObs()] for the constructor and full parameter documentation.
#' - [obs-class] for the enclosing observation model object.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name catchobs-class
setClass(
  "catchobs",
  slots = c(
    CV    = "num.array.null",
    Error = "num.array.null",
    Bias  = "num.array.null",
    Years = "num.null",
    Units = "char.null",
    Ref   = "num.array.null",
    Misc  = "list"
  )
)

setValidity("catchobs", function(object) {
  errors <- character()

  if (!is.null(object@CV) && any(object@CV < 0, na.rm = TRUE))
    errors <- c(errors, "`CV` must be non-negative")

  if (!is.null(object@Bias) && any(object@Bias <= 0, na.rm = TRUE))
    errors <- c(errors, "`Bias` must be positive")

  if (!is.null(object@Error) && any(object@Error <= 0, na.rm = TRUE))
    errors <- c(errors, "`Error` must be positive")

  valid_Units <- c("Biomass", "Number")
  if (!is.null(object@Units) && !all(object@Units %in% valid_Units))
    errors <- c(errors,
                paste0("`Units` must be one of: ", paste(valid_Units, collapse = ", ")))

  if (length(errors)) errors else TRUE
})


#' The `indicesobs` S4 Class
#'
#' Defines the observation error structure for abundance or biomass indices
#' (CPUE or survey). Used in the `CPUE` and `Survey` slots of an [obs-class]
#' object. Objects are typically created via [IndicesObs()], which documents
#' all parameters in detail.
#'
#' @slot CV `numeric` or `NULL`. Coefficient of variation of index observation
#'   error. See [IndicesObs()].
#' @slot Error `numeric` array or `NULL`. Lognormal observation error
#'   multipliers (`nSim x nYear`). May be supplied directly via [IndicesObs()],
#'   generated stochastically from `CV`, or
#'   estimated from real data during `.ConditionObs()`. See [IndicesObs()].
#' @slot Beta `numeric` array or `NULL`. Hyperstability/hyperdepletion
#'   parameter: `Observed_t = Efficiency * NomIndex_t^Beta * Error_t`. `NULL`
#'   (default) is `Beta = 1` (proportional); otherwise estimated per
#'   simulation when conditioned on real data and
#'   `SimControl(EstimateBeta = TRUE)`, unless already supplied. See
#'   [IndicesObs()].
#' @slot AC `numeric` array or `NULL`. Lag-1 autocorrelation of index
#'   residuals, one value per simulation. When supplied by the user, overrides
#'   the value estimated during conditioning. See [IndicesObs()].
#' @slot Years `numeric` or `NULL`. Calendar years over which the observation
#'   error is conditioned. See [IndicesObs()].
#' @slot Areas `numeric` or `NULL`. Integer indices of the spatial areas
#'   contributing to this index. See [IndicesObs()].
#' @slot Units `character` or `NULL`. Units of the index (`"Biomass"`,
#'   `"Number"`, or `"Recruitment"`). See [IndicesObs()].
#' @slot Selectivity `array`, `character`, `numeric`, or `list`. Selectivity
#'   specification for this index. See [IndicesObs()].
#' @slot Type `character`. Index type identifier. See [IndicesObs()].
#' @slot Ref `numeric` array or `NULL`. Reference index value, one per
#'   simulation. See [IndicesObs()].
#' @slot Efficiency `numeric` array or `NULL`. Catchability coefficient `q`
#'   relating the nominal index to the population quantity. Calculated
#'   internally during conditioning as the ratio of mean observed index to
#'   mean simulated index; not set by the user.
#' @slot TruncSD `numeric(1)` or `NULL`. Number of standard deviations at
#'   which to truncate the lognormal residual distribution when generating
#'   projection errors. Default `2`. See [IndicesObs()].
#' @slot Stats `data.frame` or `NULL`. Residual statistics computed during
#'   conditioning by [CalcResidualStats()]. Contains columns `Sim`, `AC`
#'   (weighted lag-1 autocorrelation), `SD` (standard deviation), and
#'   `NA_Season` (a list-column of seasons with no observations). Populated
#'   internally; not set by the user.
#' @slot Misc `list`. Miscellaneous additional objects. When `Beta` is
#'   conditioned on real data, `Misc$BetaFit` holds the per-simulation fit
#'   diagnostics from [EstimateBeta()]: `SE_Beta`, `CI_Lower`, `CI_Upper`,
#'   `R2`, `PValue`, `nPoints`, and `Status`. See [IndexFitTable()].
#'
#' @seealso
#'  - [IndicesObs()] for the constructor and full parameter
#'   documentation.
#'  - [obs-class] for the enclosing observation model object.
#'  - [CalcResidualStats()] for how `Stats` is computed.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name indicesobs-class
setClass(
  "indicesobs",
  slots = c(
    CV          = "num.array.null",
    Error       = "num.array.null",
    Beta        = "num.array.null",
    AC          = "num.array.null",
    Years       = "num.array.null",
    Areas       = "num.null",
    Units       = "char.null",
    Selectivity = "array.char.num.list",
    Type        = "character",
    Ref         = "num.array.null",
    Efficiency  = "num.array.null",
    TruncSD     = "num.null",
    Stats       = "df.null",
    Misc        = "list"
  )
)

setValidity("indicesobs", function(object) {
  errors <- character()

  if (!is.null(object@CV) && any(object@CV < 0, na.rm = TRUE))
    errors <- c(errors, "`CV` must be non-negative")

  if (!is.null(object@Error) && any(object@Error <= 0, na.rm = TRUE))
    errors <- c(errors, "`Error` must be positive")

  valid_Units <- c("Biomass", "Number", "Recruitment")
  if (!is.null(object@Units) && !all(object@Units %in% valid_Units))
    errors <- c(errors,
                paste0("`Units` must be one of: ", paste(valid_Units, collapse = ", ")))

  if (!is.null(object@TruncSD) && object@TruncSD <= 0)
    errors <- c(errors, "`TruncSD` must be positive")

  if (!is.null(object@Beta) && any(object@Beta <= 0, na.rm = TRUE))
    errors <- c(errors, "`Beta` must be positive")

  if (length(errors)) errors else TRUE
})


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
#'   `.ConditionObsComp()`). `NULL` (default) uses all available historical years during
#'   conditioning.
#'
#' @slot Shift `numeric` or `NULL`. Systematic per-bin offset on the
#'   log-concentration scale applied to the Dirichlet concentration vector
#'   before drawing, capturing directional bias between observed and
#'   OM-predicted compositions. After population by [PopulateCompObs()],
#'   stored as a named `[nSim x nYear x nBin]` array. `NULL` (default) applies
#'   no shift. In conditioning mode, populated internally by `.ConditionObsComp()`
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
#' with markedly different spread), `.ConditionObsComp()` will produce large
#' values in `Shift`. These are treated as observation error rather than model
#' mis-specification — an assumption that should be evaluated carefully.
#' See the technical manual for a full discussion of this assumption and its
#' implications for simulation performance.
#'
#'
#' @seealso
#' - [CompObs()] for the constructor and full parameter documentation.
#' - [PopulateCompObs()] for the population function.
#' - `.ConditionObsComp()` for the conditioning function.
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

setValidity("compobs", function(object) {
  errors <- character()

  if (!is.null(object@SampleSize) && any(object@SampleSize <= 0, na.rm = TRUE))
    errors <- c(errors, "`SampleSize` must be positive")

  if (!is.null(object@ESS) && any(object@ESS <= 0, na.rm = TRUE))
    errors <- c(errors, "`ESS` must be positive")

  if (!is.null(object@Theta) && any(object@Theta <= 0 | object@Theta > 1, na.rm = TRUE))
    errors <- c(errors, "`Theta` must be in (0, 1]")

  if (length(errors)) errors else TRUE
})

## Deprecated alias — keeps existing CompObs objects valid
#' @export
setClass("CompObs", contains = "compobs")


methods::setClassUnion(name="compobs.CompObs", members=c('compobs', 'CompObs'))

#' The `obs` S4 Class
#'
#' Defines the observation error structure applied to each data type in the
#' operating model. Each slot contains a sub-object specifying bias, CV, and
#' sampling structure for one category of observed data. Objects are typically
#' created via the [Obs()] constructor, which documents all parameters in
#' detail.
#'
#' @slot Name `character` or `NULL`. Unique identifier for this observation
#'   model. See [Obs()].
#' @slot LifeHistory A [lifehistoryobs-class] object. Observation error on
#'   life-history parameters (growth, maturity, natural mortality, etc.).
#'   See [LifeHistoryObs()].
#' @slot Exploitation An [exploitationobs-class] object. Observation error on
#'   exploitation processes (selectivity, retention, discard mortality).
#'   See [ExploitationObs()].
#' @slot Effort An [effortobs-class] object. Observation error on fishing
#'   effort. See [EffortObs()].
#' @slot Landings A [catchobs-class] object. Observation error on landed
#'   catch. See [CatchObs()].
#' @slot Discards A [catchobs-class] object. Observation error on discarded
#'   catch. See [CatchObs()].
#' @slot CPUE An [indicesobs-class] object. Observation error on
#'   catch-per-unit-effort indices. See [IndicesObs()].
#' @slot Survey An [indicesobs-class] object. Observation error on
#'   fishery-independent survey indices. See [IndicesObs()].
#' @slot LandingsAtAge A [compobs-class] object. Observation error on
#'   landed catch-at-age composition. See [CompObs()].
#' @slot DiscardsAtAge A [compobs-class] object. Observation error on
#'   discarded catch-at-age composition. See [CompObs()].
#' @slot LandingsAtSize A [compobs-class] object. Observation error on
#'   landed catch-at-length composition. See [CompObs()].
#' @slot DiscardsAtSize A [compobs-class] object. Observation error on
#'   discarded catch-at-length composition. See [CompObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @details
#' An `obs` object defines how each data type is observed, i.e., the error
#' structure (CV, bias, selectivity).
#' Observed values are held in [data-class] objects stored in `OM@Data`, `Hist@Data`,
#' and `MSE@PPD`.
#'
#' `OM@Obs` is a two-level named list indexed first by stock complex, then by
#' fleet or index name:
#' ```
#' OM@Obs[[stock_complex]][[fleet_name]]  →  obs-class object
#' ```
#' Stock complexes aggregate stocks whose data are reported together (e.g.,
#' combined landings across species). This indexing mirrors `OM@Data`.
#'
#' The slots `Effort`, `Landings`, `Discards`, `CPUE`, `Survey`,
#' `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, and `DiscardsAtSize`
#' are populated with empty sub-objects by [Obs()] when not supplied. The
#' `LifeHistory` and `Exploitation` slots are reserved for future use and are
#' not currently populated during model runs; see [LifeHistoryObs()] and
#' [ExploitationObs()].
#'
#' Direct construction via [methods::new()] is not recommended; use [Obs()]
#' instead, which initialises all sub-objects automatically.
#'
#' @seealso
#' - [Obs()] for the constructor and accessor.
#' - [CatchObs()], [EffortObs()], [IndicesObs()], [CompObs()] for
#'   sub-object constructors.
#' - [LifeHistoryObs()], [ExploitationObs()] for placeholder sub-objects.
#' - [data-class] and [Data()] for the complementary observed-values object.
#' - [OM()] for the operating model constructor.
#' - [ConvertObs()] for converting legacy [Obs-legacy-class] objects.
#'
#' @family obs
#'
#' @include class-unions.R
#' @name obs-class
#' @rdname obs-class
setClass(
  "obs",
  slots = c(
    Name           = "char.null",
    LifeHistory    = "lifehistoryobs",
    Exploitation   = "exploitationobs",
    Effort         = "effortobs",
    Landings       = "catchobs",
    Discards       = "catchobs",
    CPUE           = "indicesobs",
    Survey         = "indicesobs",
    LandingsAtAge  = "compobs.CompObs",
    DiscardsAtAge  = "compobs.CompObs",
    LandingsAtSize = "compobs.CompObs",
    DiscardsAtSize = "compobs.CompObs",
    Misc           = "list"
  )
)

setValidity("obs", function(object) {
  # Content validation lives on each sub-object's own class (catchobs,
  # effortobs, indicesobs, compobs); nothing further to check here.
  TRUE
})
