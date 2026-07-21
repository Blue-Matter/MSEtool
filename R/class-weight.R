#' The `weight` S4 Class
#'
#' Defines the weight-at-age and weight-at-length schedules for a
#' [stock-class] object. Objects are typically created via [Weight()], which
#' documents all parameters, validates inputs, and infers the model
#' automatically when possible.
#'
#' @slot Pars `list`. Named list of parameters whose names
#'   correspond to those expected by `Model`. See [WeightModels()] for
#'   parameter sets and
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#'   accepted input formats.
#' @slot Model `function` or `character(1)`. Weight model identifier, matched
#'   to one of [WeightModels()]. Set automatically by `.FindModel()` when `Pars`
#'   is supplied without an explicit model.
#' @slot Units `character(1)`. Physical unit of the weight measurements (e.g.,
#'   `"g"`, `"kg"`). Must be accepted by [ValidUnits()].
#' @slot MeanAtAge `array`. Mean weight at age with named dimensions `Sim`,
#'   `Age`, and `Year`. Populated automatically by [Populate()] when `Pars`
#'   and `Model` are set; may also be supplied directly when `Pars` is empty.
#'   When `Pars` contains a matched model, any existing values are overwritten.
#'   See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @slot MeanAtLength `array`. Mean weight at length with named dimensions
#'   `Sim`, `Length`, and `Year`. Populated automatically when an at-length
#'   weight model is used, or may be supplied directly. Values are evaluated at
#'   the midpoint of each bin (halfway between consecutive lower bounds); the
#'   `Class` dimension is labelled by bin lower bounds (see `Classes`).
#'   Converted to `MeanAtAge` via the `ALK` during [Populate()] when `MeanAtAge`
#'   is not already populated. See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @slot CVatAge `array` or `numeric`. Coefficient of variation of
#'   weight-at-age, used to generate the within-age-class weight distribution
#'   and to build the age-weight key (`AWK`). `NULL` by default; the `AWK` is
#'   only constructed when this slot is populated.
#' @slot Dist `character(1)`. Parametric distribution used for weight-at-age
#'   variability (e.g., `"lognormal"`).
#' @slot TruncSD `array` or `numeric`. Number of standard deviations at which
#'   the weight distribution is truncated.
#' @slot Timing `array` or `numeric`. Fractional position within the time step
#'   at which weight is measured (`0` = start, `1` = end).
#' @slot Random `array`. Random effects on weight parameters across simulations
#'   or years.
#' @slot AWK `array`. Age-weight key: the probability of belonging to each
#'   weight class given age, derived from `MeanAtAge`, `CVatAge`, `Dist`, and
#'   `TruncSD`. Only populated when `CVatAge` is non-`NULL`; use [AWK()] to
#'   retrieve.
#' @slot Classes `numeric`. Lower bounds of weight bins in units of `Units`.
#'   Bin `k` spans `[Classes[k], Classes[k+1])`; the final bin is open-ended.
#'   Values in `MeanAtLength` are evaluated at bin midpoints, not at these
#'   lower bounds.
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use [Weight()]
#' instead, which handles model inference and object validation.
#'
#' The `AWK` slot is only populated during [Populate()] when `CVatAge` is
#' non-`NULL`. For most applications weight variability is propagated through
#' the age-length key (`ALK`, see [Length()]) and a length-weight relationship
#' rather than directly through an age-weight distribution.
#'
#' @seealso 
#'  - [Weight()] for the constructor and accessor functions.
#'  - [WeightModels()] for available models and their required parameters.
#'  - [ValidUnits()] for accepted unit strings. 
#'  - [Populate()] for array population. 
#'  - `.FindModel()` for automatic model inference. 
#'  - [AWK()] to retrieve the age-weight key. 
#'  - [Length()] for the companion length schedule, required when using at-length weight models.
#'  - [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for the
#'   full description of how `Pars`, `Model`, and `MeanAt*` arrays interact.
#'
#' @family weight
#'
#' @export
#' @include class-unions.R
#' @name weight-class
setClass(
  "weight",
  slots = c(
    Pars         = "list",
    Model        = "fun.char",
    Units        = "char.null",
    MeanAtAge    = "num.array.null",
    MeanAtLength = "num.array.null",
    CVatAge      = "num.array.null",
    Dist         = "character",
    TruncSD      = "num.array.null",
    Timing       = "num.array.null",
    Random       = "num.array.null",
    AWK          = "array.null",
    Classes      = "num.null",
    Misc         = "list"
  )
)

setValidity("weight", function(object) {
  chk <- tryCatch(.CheckPars(object@Pars), error=function(e) e)
  if (inherits(chk, "error")) return(conditionMessage(chk))
  TRUE
})
