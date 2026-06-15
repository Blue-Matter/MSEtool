#' The `naturalmortality` S4 Class
#'
#' Defines the natural mortality schedule for a [stock-class] object. Mortality
#' may be expressed as a function of age or length, and may vary across
#' simulations and years. Objects are typically created via
#' [NaturalMortality()], which documents all parameters, validates inputs, and
#' infers the mortality model automatically when possible.
#'
#' @slot Pars `list`. Named list of natural mortality parameters whose names
#'   correspond to those expected by `Model`. See [NaturalMortalityModels()]
#'   for parameter sets and
#'   [Specifying Biological and Fleet Schedules][populating-schedules] for
#'   accepted input formats.
#' @slot Model `function` or `character(1)`. Natural mortality model
#'   identifier, matched to one of [NaturalMortalityModels()]. Set
#'   automatically by [FindModel()] when `Pars` is supplied without an
#'   explicit model.
#' @slot Units `character(1)`. Time unit in which mortality rates are
#'   expressed (e.g., `"year"` for instantaneous annual mortality). Must be
#'   accepted by [ValidUnits()] and match those used in [Ages()].
#' @slot MeanAtAge `array`. Mean instantaneous natural mortality at age with
#'   named dimensions `Sim`, `Age`, and `Year`. Populated automatically by
#'   [Populate()] when `Pars` and `Model` are set; may also be supplied
#'   directly when `Pars` is empty. When `Pars` contains a matched model, any
#'   existing values are overwritten.
#'   See [Specifying Biological and Fleet Schedules][populating-schedules].
#' @slot MeanAtLength `array`. Mean natural mortality at length with named
#'   dimensions `Sim`, `Length`, and `Year`. Populated automatically when an
#'   at-length mortality model is used, or may be supplied directly. Values are
#'   evaluated at the midpoint of each bin (halfway between consecutive lower
#'   bounds); the `Class` dimension is labelled by bin lower bounds (see
#'   `Classes`). Converted to `MeanAtAge` via the `ALK` during [Populate()]
#'   when `MeanAtAge` is not already populated.
#'   See [Specifying Biological and Fleet Schedules][populating-schedules].
#' @slot Random `array`. Reserved for future use. Intended to hold
#'   simulation- and year-specific multipliers that add stochastic variation
#'   around the average mortality schedule in `MeanAtAge`. Currently stored
#'   but not applied during [Populate()].
#' @slot Classes `numeric`. Age classes (in years) or lower bounds of length
#'   bins, corresponding to the `MeanAt*` array in use. For length bins, bin
#'   `k` spans `[Classes[k], Classes[k+1])`; the final bin is open-ended.
#'   Values in `MeanAtLength` are evaluated at bin midpoints, not at these
#'   lower bounds.
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use
#' [NaturalMortality()] instead, which handles model inference and object
#' validation.
#'
#' Rates in `MeanAtAge` are instantaneous mortality values (*M*) expressed
#' over the period defined by `Units`. For seasonal models, set `Units` to
#' match the season length so that within-step rates are scaled correctly
#' during [Populate()].
#'
#' @seealso
#' - [NaturalMortality()] for the constructor and accessor functions.
#' - [NaturalMortalityModels()] for available models and their required
#'   parameters.
#' - [ValidUnits()] for accepted unit strings.
#' - [Populate()] for array population.
#' - [FindModel()] for automatic model inference.
#' - [Length()] for the companion length schedule, required when using
#'   at-length mortality models.
#' - [Specifying Biological and Fleet Schedules][populating-schedules] for the
#'   full description of how `Pars`, `Model`, and `MeanAt*` arrays interact.
#'
#' @family naturalmortality
#'
#' @export
#' @include class-unions.R
#' @name naturalmortality-class
setClass(
  "naturalmortality",
  slots = c(
    Pars          = "list",
    Model         = "fun.char",
    Units         = "char.null",
    MeanAtAge     = "num.array.null",
    MeanAtLength  = "num.array.null",
    Random        = "num.array.null",
    Classes       = "num.null",
    Misc          = "list"
  )
)

setValidity("naturalmortality", function(object) {
  # TODO: add structural and dimensional checks
  TRUE
})
