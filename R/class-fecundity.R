#' The `fecundity` S4 Class
#'
#' Defines the fecundity schedule for a [stock-class] object, expressed as
#' reproductive output per individual as a function of age or length. This
#' object is optional — see *Default Behaviour* in [Fecundity()] for what
#' happens when it is omitted. Objects are typically created via [Fecundity()],
#' which documents all parameters, validates inputs, and infers the fecundity
#' model automatically when possible.
#'
#' @slot Pars `list`. Named list of fecundity model parameters whose names
#'   correspond to those expected by `Model`. See [FecundityModels()] for
#'   parameter sets and
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#'   accepted input formats.
#' @slot Model `function` or `character(1)`. Fecundity model identifier,
#'   matched to one of [FecundityModels()]. Set automatically by `.FindModel()`
#'   when `Pars` is supplied without an explicit model.
#' @slot Units `character(1)`. Unit of reproductive output (e.g., `"eggs"`).
#'   Determines the unit of the `SProduction` slot in [hist-class] objects.
#'   When this object is empty, `SProduction` defaults to spawning biomass
#'   and `Units` is irrelevant.
#' @slot MeanAtAge `array`. Mean fecundity-at-age with named dimensions `Sim`,
#'   `Age`, and `Year`. Populated automatically by [Populate()] when `Pars`
#'   and `Model` are set; may also be supplied directly when `Pars` is empty.
#'   When `Pars` contains a matched model, any existing values are overwritten.
#'   See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @slot MeanAtLength `array`. Mean fecundity-at-length with named dimensions
#'   `Sim`, `Length`, and `Year`. Populated automatically when an at-length
#'   fecundity model is used, or may be supplied directly. Values are evaluated
#'   at the midpoint of each bin (halfway between consecutive lower bounds);
#'   the `Class` dimension is labelled by bin lower bounds (see `Classes`).
#'   Converted to `MeanAtAge` via the `ALK` during [Populate()] when `MeanAtAge`
#'   is not already populated.
#'   See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @slot Classes `numeric`. Age classes (in years) or lower bounds of length
#'   bins, corresponding to the `MeanAt*` array in use. For length bins, bin
#'   `k` spans `[Classes[k], Classes[k+1])`; the final bin is open-ended.
#'   Values in `MeanAtLength` are evaluated at bin midpoints, not at these
#'   lower bounds.
#' @slot Timing `array` or `numeric`. Reserved for future use. Intended to
#'   represent the timing of spawning within the time step. Currently stored
#'   but not applied; spawning timing is controlled by [SRR()].
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use
#' [Fecundity()] instead, which handles model inference and object validation.
#'
#' This object is optional. When empty, [Populate()] computes spawning
#' production (`SProduction`) as mature weight-at-age (i.e. identical to
#' spawning biomass, `SBiomass`). When populated, `SProduction` is in the
#' units of this object (e.g. eggs per individual), allowing reproductive
#' output to differ from biomass — relevant for species where egg production
#' is not proportional to weight.
#'
#' @seealso
#' - [Fecundity()] for the constructor and accessor functions.
#' - [FecundityModels()] for available models and their required parameters.
#' - [ValidUnits()] for accepted unit strings.
#' - [Populate()] for array population.
#' - `.FindModel()` for automatic model inference.
#' - [Length()] and [Weight()] for the companion schedules used in the
#'   default spawning biomass calculation and at-length fecundity models.
#' - [Maturity()] for the maturity schedule multiplied through fecundity
#'   during population.
#' - [SRR()] for spawning timing within the time step.
#' - [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for the
#'   full description of how `Pars`, `Model`, and `MeanAt*` arrays interact.
#'
#' @family fecundity
#'
#' @include class-unions.R
#' @name fecundity-class
setClass(
  "fecundity",
  slots = c(
    Pars         = "list",
    Model        = "fun.char",
    Units        = "char.null",
    MeanAtAge    = "num.array.null",
    MeanAtLength = "num.array.null",
    Classes      = "num.null",
    Timing       = "num.array.null",
    Misc         = "list"
  )
)


setValidity("fecundity", function(object) {
  chk <- tryCatch(.CheckPars(object@Pars), error=function(e) e)
  if (inherits(chk, "error")) return(conditionMessage(chk))
  TRUE
})
