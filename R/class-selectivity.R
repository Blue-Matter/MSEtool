#' The `selectivity` S4 Class
#'
#' Defines selectivity-at-age, selectivity-at-length, or
#' selectivity-at-weight for use in a [fleet-class] object. Selectivity is
#' required for all fleets. Objects are typically created via the
#' [Selectivity()] constructor, which documents all parameters in detail.
#'
#' @slot Pars `list`. Named list of selectivity parameters passed to `Model`.
#'   See [Selectivity()] and [SelectivityModels()].
#' @slot Model `function` or `character` or `NULL`. Selectivity model
#'   identifier or function. See [SelectivityModels()].
#' @slot isRel `logical`, `numeric`, or `character`. Whether length-based
#'   parameters are expressed relative to maturity `L50`. See [Selectivity()].
#' @slot MeanAtAge `numeric` array or `NULL`. Mean selectivity-at-age
#'   (`Sim x Age x Year x Area`). See [Selectivity()].
#' @slot MeanAtLength `numeric` array or `NULL`. Mean selectivity-at-length
#'   (`Sim x Length x Year x Area`). Values are evaluated at the midpoint of
#'   each bin (halfway between consecutive lower bounds); the `Class` dimension
#'   is labelled by bin lower bounds (see `Classes`). See [Selectivity()].
#' @slot MeanAtWeight `numeric` array or `NULL`. Mean selectivity-at-weight
#'   (`Sim x Weight x Year x Area`). Values are evaluated at the midpoint of
#'   each bin; the `Class` dimension is labelled by bin lower bounds (see
#'   `Classes`). See [Selectivity()].
#' @slot Classes `numeric` or `NULL`. Lower bounds of length or weight bins
#'   corresponding to the second dimension of `MeanAtLength` or `MeanAtWeight`.
#'   Bin `k` spans `[Classes[k], Classes[k+1])`; the final bin is open-ended.
#'   Values in `MeanAtLength`/`MeanAtWeight` are evaluated at bin midpoints,
#'   not at these lower bounds.
#' @slot isAtLength `logical`. Whether `MeanAtLength`/`MeanAtWeight` reflect a
#'   genuinely length- (or weight-) selectivity schedule, as opposed to
#'   one derived from an age-only schedule (e.g. via the age-length
#'   key). Controls whether fleet weight-at-age calculations
#'   (`WeightFleetRetained`/`WeightFleetSelected`) weight by this object's
#'   at-length schedule or fall back to the stock's plain weight-at-age.
#'   Default `TRUE`. 
#' @slot Misc `list`. Miscellaneous additional inputs. Used internally.
#'
#' @seealso
#' - [Selectivity()] for the constructor and full parameter
#'   documentation.
#'  - [SelectivityModels()] for available model functions.
#'  - [fleet-class] for the enclosing fleet object.
#'
#' @family fleet
#'
#' @include class-unions.R
#' @name selectivity-class
setClass(
  "selectivity",
  slots = c(
    Pars         = "list",
    Model        = "fun.char",
    isRel        = "char.log.num",
    MeanAtAge    = "num.array.null",
    MeanAtLength = "num.array.null",
    MeanAtWeight = "num.array.null",
    Classes      = "num.null",
    isAtLength   = "logical",
    Misc         = "list"
  )
)

setValidity("selectivity", function(object) {
  # TODO
  TRUE
})
