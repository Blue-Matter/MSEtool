#' Specifying Biological and Fleet Schedules
#'
#' Several objects in this package — [length-class], [weight-class],
#' [maturity-class], [naturalmortality-class], [fecundity-class], and the fleet
#' sub-objects [selectivity-class], and [retention-class] — share a common structure for specifying
#' schedules (growth, mortality, maturity, selectivity, etc.) across
#' simulations and years. This page describes that structure and the rules that
#' govern how `Pars`, `Model`, and `MeanAt*` arrays interact.
#'
#' @section Two Ways to Specify a Schedule:
#'
#' **Option 1 — Model-based (recommended)**
#'
#' Supply `Pars`, a named list whose element names correspond to the arguments
#' of a built-in model function (see, e.g., [LengthModels()],
#' [MaturityModels()], [NaturalMortalityModels()]). The model is then resolved
#' automatically by [FindModel()], which matches the parameter names in `Pars`
#' to the formal arguments of candidate model functions. You may also set
#' `Model` explicitly to a character string naming the model or to a custom R
#' function — see *Custom Models* below.
#'
#' When model-based, `Pars` are expanded to `Sim × Year` arrays by
#' [Populate()], the model function is called to produce `MeanAtAge` (or
#' `MeanAtLength`/`MeanAtWeight` for size-based models), and any existing
#' values in `MeanAt*` slots are **overwritten**.
#'
#' **Option 2 — Direct array**
#'
#' Leave `Pars = list()` (the default) and supply one of `MeanAtAge`,
#' `MeanAtLength`, or `MeanAtWeight` directly as a numeric array. No model is
#' inferred and no overwriting occurs.
#'
#' These two options are mutually exclusive: if `Pars` contains valid,
#' non-`NA` named values that match a model, it takes precedence and any
#' values supplied in `MeanAt*` will be replaced.
#'
#' @section Populating `Pars` — Input Formats:
#'
#' Each element of `Pars` may be supplied in any of the following forms. All
#' are converted internally to `Sim × Year` (or `Sim × Year × Area`) arrays
#' before the model function is called.
#'
#' **Scalar** — a single value applied to all simulations and years:
#' ```r
#' Pars = list(M = 0.2)
#' ```
#'
#' **Length-2 bounds vector** — values are sampled independently for each
#' simulation from `Uniform(lower, upper)`. When `nSim = 1`, the midpoint is
#' used:
#' ```r
#' Pars = list(M = c(0.1, 0.3))   # U(0.1, 0.3) across simulations
#' ```
#'
#' **Length-`nSim` vector** — one value per simulation, constant across years:
#' ```r
#' Pars = list(M = c(0.15, 0.20, 0.25, ...))
#' ```
#'
#' **`Sim × Year` array with named dimensions** — full stochastic,
#' time-varying specification. Only the years where the value *changes* need
#' to be included; [Extend()] fills intermediate and future years
#' automatically by forward-filling from the most recent supplied year:
#' ```r
#' m_arr <- array(
#'   c(0.2, 0.4),              # two change points
#'   dim      = c(1, 2),
#'   dimnames = list(Sim = 1, Year = c(1990, 2010))
#' )
#' Pars = list(M = m_arr)
#' ```
#'
#' **Temporal random walk** — append a partner entry named `<ParName>SD` to
#' `Pars`. A mean-preserving log-normal random walk is then applied to the
#' base parameter across years, and the `SD` entry is removed from
#' `Pars` after use:
#' ```r
#' Pars = list(M = 0.2, MSD = 0.1)   # M with log-normal inter-annual variation
#' ```
#'
#' @section `MeanAt*` Array Formats and Dimension Conventions:
#'
#' All `MeanAt*` arrays use named dimensions. The expected dimension names
#' depend on the array type:
#'
#' | Array | Dimensions |
#' |---|---|
#' | `MeanAtAge` | `Sim × Age × Year` |
#' | `MeanAtLength` | `Sim × Class × Year` |
#' | `MeanAtWeight` | `Sim × Class × Year` |
#'
#' As with `Pars`, you only need to supply the years at which the schedule
#' *changes*. [Extend()] forward-fills all other years automatically.
#' Similarly, a single simulation (`Sim` dimension of length 1) is replicated
#' to all `nSim` simulations.
#'
#' A numeric vector of length `nAge` is also accepted for `MeanAtAge` and is
#' promoted to a `1 × nAge × 1` array automatically.
#'
#' @section How `MeanAtLength` and `MeanAtWeight` Relate to `MeanAtAge`:
#'
#' `openMSE` uses age-based accounting, so `MeanAtAge`
#' is ultimately what is needed. `MeanAtLength` and `MeanAtWeight` serve two
#' roles:
#'
#' 1. **Generating length- or weight- data** (e.g., observed size
#'    distributions, length-based data).
#' 2. **Conversion to `MeanAtAge`** when a schedule is naturally expressed on
#'    a size axis (e.g., length-based maturity ogives, length-based
#'    selectivity).
#'
#' The conversion from size to age uses the Age-Length Key (`ALK`) stored in
#' the [length-class] object, or the Age-Weight Key (`AWK`) from the
#' [weight-class] object:
#'
#' - If `MeanAtLength` is populated and `MeanAtAge` is not, [Populate()]
#'   converts `MeanAtLength` → `MeanAtAge` via the `ALK`.
#' - If `MeanAtWeight` is populated and `MeanAtAge` is not, [Populate()]
#'   converts `MeanAtWeight` → `MeanAtAge` via the `AWK`.
#' - If `MeanAtAge` is already populated it is **not** overwritten by this
#'   conversion.
#' - If both `MeanAtLength` and `MeanAtAge` are populated, `MeanAtAge` takes
#'   precedence and `MeanAtLength` is used only for size-indexed output.
#'
#' For **model-based** specification, whether the model populates `MeanAtAge`
#' directly or via `MeanAtLength` depends on the model's function signature:
#' models that accept a `Length` argument produce `MeanAtLength` first and then
#' convert to `MeanAtAge` via the `ALK`; age-based models populate `MeanAtAge`
#' directly.
#'
#' @section Model Resolution — `Pars`, `Model`, and `FindModel()`:
#'
#' When `Pars` is non-empty and `Model` is `NULL`, [FindModel()] is called
#' automatically. It scans the candidate model functions for the object class
#' (e.g., all functions with class `"LengthModel"`) and finds the one whose
#' formal argument names match the names in `Pars` exactly (excluding
#' auxiliary arguments such as `Ages`, `Length`, `Weight`). SD-suffixed
#' entries are ignored during matching.
#'
#' If no unique match is found, an informative error is thrown directing the
#' user to the relevant `*Models()` function.
#'
#' You may bypass automatic inference by setting `Model` explicitly:
#' - **Character string**: the name of a built-in model (must exist as an
#'   exported function of the appropriate class).
#' - **R function**: a custom function whose formal arguments match the names
#'   in `Pars`. Auxiliary arguments (`Ages`, `Length`, `Weight`, etc.) may
#'   also be present.
#'
#' If `object@Model` is already a function object, [FindModel()] returns it
#' unchanged — no further matching is attempted.
#'
#' @section Custom Models:
#'
#' Any R function can be used as a model by passing it to `Model`. The
#' function must accept arguments whose names match the elements of `Pars`,
#' plus any required auxiliary arguments (`Ages`, `Length`, `Weight`, etc.
#' as appropriate). It must return a named `Sim × Age × Year` array
#' (or `Sim × Class × Year` for size-based outputs):
#'
#' ```r
#' my_M <- function(M, Ages) {
#'   # age-varying M: decline from M to M/2 over age classes
#'   m_vec <- seq(M, M / 2, length.out = length(Ages))
#'   array(m_vec, dim = c(1, length(Ages), 1),
#'         dimnames = list(Sim = 1, Age = Ages, Year = 1))
#' }
#'
#' NaturalMortality(Pars = list(M = 0.3), Model = my_M)
#' ```
#'
#' @seealso 
#'  - [Length()], [Weight()], [Maturity()], [NaturalMortality()], [Fecundity()]
#' for the stock sub-object constructors that follow this pattern.
#'  - [Selectivity()], [Retention()] for the fleet 
#'  sub-object constructors that follow this pattern.
#'  - [LengthModels()], [WeightModels()], [MaturityModels()],
#' [NaturalMortalityModels()], [SelectivityModels()], [RetentionModels()],
#'  for the available built-in models and their
#' required parameter sets. 
#' - [Extend()] for the array extension rules.
#' - [Populate()] for the top-level population dispatch function.
#'
#' @name populating-schedules
#' @aliases schedule-population pars-model-meanatage
NULL