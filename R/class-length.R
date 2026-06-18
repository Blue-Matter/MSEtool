#' The `length` S4 Class
#'
#' Defines the length-at-age schedule for a [stock-class] object. Objects are
#' typically created via [Length()], which documents all parameters, validates
#' inputs, and infers the growth model automatically when possible.
#'
#' @slot Pars `list`. Named list of growth parameters whose names correspond to
#'   those expected by `Model`. See [LengthModels()] for parameter sets and
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#'   accepted input formats.
#' @slot Model `function` or `character(1)`. Growth model identifier, matched
#'   to one of [LengthModels()]. Set automatically by [FindModel()] when `Pars`
#'   is supplied without an explicit model.
#' @slot Units `character(1)`. Physical unit of the length measurements (e.g.,
#'   `"mm"`, `"cm"`). Must be accepted by [ValidUnits()].
#' @slot MeanAtAge `array`. Mean length at age with named dimensions `Sim`,
#'   `Age`, and `Year`. Populated automatically by [Populate()] when `Pars`
#'   and `Model` are set; may also be supplied directly when `Pars` is empty.
#'   When `Pars` contains a matched model, any existing values are overwritten.
#'   See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @slot CVatAge `array` or `numeric`. Coefficient of variation of
#'   length-at-age, used to generate the within-age-class length distribution.
#' @slot Dist `character(1)`. Parametric distribution used for length-at-age
#'   variability (e.g., `"normal"`).
#' @slot TruncSD `array` or `numeric`. Number of standard deviations at which
#'   the length distribution is truncated.
#' @slot Timing `array` or `numeric`. Fractional position within the time step
#'   at which length is measured (`0` = start, `1` = end).
#' @slot Random `array`. Random effects on growth parameters across simulations
#'   or years.
#' @slot ALK `array`. Age-length key: the probability of belonging to each
#'   length class given age, derived from `MeanAtAge`, `CVatAge`, `Dist`, and
#'   `TruncSD`. Populated automatically; use [ALK()] to retrieve.
#' @slot Classes `numeric`. Lower bounds of length bins in units of `Units`.
#'   Bin `k` spans `[Classes[k], Classes[k+1])`; the final bin is open-ended.
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use [Length()]
#' instead, which handles model inference and object validation.
#'
#' The `ALK` slot is derived from `MeanAtAge`, `CVatAge`, `Dist`, and
#' `TruncSD` during [Populate()] and should not be set manually.
#'
#' @seealso
#'  - [Length()] for the constructor and accessor functions.
#'  - [LengthModels()] for available growth models and their required #'   parameters. 
#'  - [ValidUnits()] for accepted unit strings. 
#'  - [Populate()] for array population. 
#'  - [FindModel()] for automatic model inference.
#'   -  [ALK()] to retrieve the age-length key.
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for the
#'   full description of how `Pars`, `Model`, and `MeanAtAge` interact.
#'
#' @family length
#'
#' @include class-unions.R
#' @name length-class
setClass(
  "length",
  slots = c(
    Pars      = "list",
    Model     = "fun.char",
    Units     = "char.null",
    MeanAtAge = "num.array.null",
    CVatAge   = "num.array.null",
    Dist      = "character",
    TruncSD   = "num.array.null",
    Timing    = "num.array.null",
    Random    = "num.array.null",
    ALK       = "array.null",
    Classes   = "num.null",
    Misc      = "list"
  )
)

setValidity("length", function(object) {
  # TODO
  TRUE
})

