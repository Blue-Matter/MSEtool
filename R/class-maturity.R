#' The `maturity` S4 Class
#'
#' Defines the maturity schedule for a [stock-class] object. Maturity may be
#' expressed as a function of age, length, or weight. Objects are typically
#' created via [Maturity()], which documents all parameters, validates inputs,
#' and infers the maturity model automatically when possible.
#'
#' @slot Pars `list`. Named list of maturity model parameters whose names
#'   correspond to those expected by `Model`. See [MaturityModels()] for
#'   parameter sets and
#'   [Specifying Biological and Fleet Schedules][populating-schedules] for
#'   accepted input formats.
#' @slot Model `function` or `character(1)`. Maturity model identifier,
#'   matched to one of [MaturityModels()]. Set automatically by [FindModel()]
#'   when `Pars` is supplied without an explicit model.
#' @slot MeanAtAge `array`. Mean maturity-at-age with named dimensions `Sim`,
#'   `Age`, and `Year`. Values range from 0 (immature) to 1 (fully mature).
#'   Populated automatically by [Populate()] when `Pars` and `Model` are set;
#'   may also be supplied directly when `Pars` is empty. When `Pars` contains
#'   a matched model, any existing values are overwritten.
#'   See [Specifying Biological and Fleet Schedules][populating-schedules].
#' @slot MeanAtLength `array`. Mean maturity-at-length with named dimensions
#'   `Sim`, `Length`, and `Year`. Populated automatically when an at-length
#'   maturity model is used, or may be supplied directly. Converted to
#'   `MeanAtAge` via the `ALK` during [Populate()] when `MeanAtAge` is not
#'   already populated.
#'   See [Specifying Biological and Fleet Schedules][populating-schedules].
#' @slot MeanAtWeight `array`. Mean maturity-at-weight with named dimensions
#'   `Sim`, `Weight`, and `Year`. Populated automatically when an at-weight
#'   maturity model is used, or may be supplied directly. Converted to
#'   `MeanAtAge` via the `AWK` during [Populate()] when `MeanAtAge` is not
#'   already populated. Requires a populated [weight-class] object with a
#'   non-`NULL` `CVatAge` slot (so that the `AWK` exists).
#'   See [Specifying Biological and Fleet Schedules][populating-schedules].
#' @slot Classes `numeric`. Age, length, or weight class midpoints
#'   corresponding to the `MeanAt*` array in use.
#' @slot Semelparous `logical` or `array`. Controls post-spawning mortality.
#'   Before [Populate()], a scalar `TRUE` or `FALSE`. After [Populate()], always
#'   a `Sim × Age × Year` array where each cell gives the fraction of
#'   individuals at that age dying immediately after spawning. The default
#'   (`FALSE`) sets all cells to 0 (no post-spawn mortality). `TRUE` sets
#'   the array equal to `MeanAtAge`, so that post-spawn mortality tracks the
#'   maturity ogive — fully mature fish die with probability 1, fish on the
#'   ogive die proportionally, and immature fish are unaffected. A custom
#'   array may also be supplied directly. See [Maturity()] for details.
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use
#' [Maturity()] instead, which handles model inference and object validation.
#'
#' Only one of `MeanAtAge`, `MeanAtLength`, or `MeanAtWeight` need be
#' populated; `MeanAtAge` takes precedence if more than one is present.
#'
#' After [Populate()], `Semelparous` is always a `Sim × Age × Year` array.
#' Do not test `Semelparous(mat) == TRUE` on a populated object; check
#' `any(Semelparous(mat) > 0)` instead.
#'
#' @seealso 
#' - [Maturity()] for the constructor and accessor functions.
#' - [MaturityModels()] for available maturity models and their required
#'   parameters. 
#' - [Populate()] for array population.
#' - [FindModel()] for automatic model inference. 
#' - [Semelparous()] to retrieve or set the post-spawn mortality array. 
#' - [Length()] and [Weight()] for the companion schedules required by at-length and at-weight maturity models.
#' - [Specifying Biological and Fleet Schedules][populating-schedules] for the
#'   full description of how `Pars`, `Model`, and `MeanAt*` arrays interact.
#'
#' @family maturity
#'
#' @export
#' @include class-unions.R
#' @name maturity-class
setClass(
  "maturity",
  slots = c(
    Pars         = "list",
    Model        = "fun.char",
    MeanAtAge    = "num.array.null",
    MeanAtLength = "num.array.null",
    MeanAtWeight = "num.array.null",
    Classes      = "num.null",
    Semelparous  = "array.log.null",
    Misc         = "list"
  )
)

setValidity("maturity", function(object) {
  # TODO
  TRUE
})