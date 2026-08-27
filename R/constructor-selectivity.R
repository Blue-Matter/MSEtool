#' Selectivity
#'
#' Construct and manipulate a [selectivity-class] object defining
#' selectivity-at-age, selectivity-at-length, or selectivity-at-weight for a
#' [Fleet()] object. Selectivity is required for all fleets.
#'
#' @param Pars Named list of selectivity parameters passed to the selectivity
#'   model function. Parameter names must match the arguments of the chosen
#'   model (see [SelectivityModels()]). Each element may be a scalar, vector,
#'   or array; see [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for accepted formats. Default `list()`
#'   (empty).
#'
#'   If `Pars` and `Model` are both provided, `MeanAtAge` is computed from the
#'   model and any values already in `MeanAtAge` are overwritten. To preserve
#'   a user-specified `MeanAtAge`, leave `Pars = list()` (the default).
#'
#'   If `Pars` is a [fleet-class] object, the `Selectivity` slot of that
#'   fleet is returned.
#'
#' @param Model Character or function or `NULL`. Selectivity model identifier.
#'   If `NULL` (default), the model is inferred automatically from the names
#'   in `Pars` via `.FindModel()`. May also be supplied as a custom R function
#'   with arguments matching those in `Pars`. See [SelectivityModels()] for
#'   built-in options.
#' @param MeanAtAge Numeric array or `NULL`. Mean selectivity-at-age with
#'   dimensions `Sim x Age x Year` (area dimension added during population).
#'   Used directly when `Pars` is empty and `MeanAtLength` is not supplied.
#'   See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#'   accepted array formats. **Note:** if `Pars` and `Model` are both
#'   provided, any values supplied here will be overwritten during population.
#' @param MeanAtLength Numeric array or `NULL`. Mean selectivity-at-length
#'   with dimensions `Sim x Class x Year`. If provided and `Pars` is empty,
#'   takes precedence over `MeanAtAge`; `MeanAtAge` is derived from it via the
#'   age-length key. A numeric vector is also accepted and promoted to a
#'   `1 x nClass x 1` array automatically, using `Classes` if supplied or the
#'   [length-class] object's own `Classes` otherwise; if `Classes` differs
#'   from the [length-class] object's, the `ALK` is recalculated on `Classes`.
#' @param MeanAtWeight Numeric array or `NULL`. Mean selectivity-at-weight
#'   with dimensions `Sim x Class x Year`. A numeric vector is also accepted
#'   and promoted the same way as `MeanAtLength`, using the [weight-class]
#'   object's `AWK`. Default `NULL`.
#' @param Classes Numeric vector or `NULL`. Length or weight class midpoints
#'   corresponding to the second dimension of `MeanAtLength` or
#'   `MeanAtWeight`. Default `NULL`.
#' @param isRel Logical. If `TRUE`, length-based parameters (e.g., `L5`,
#'   `LFS`) are interpreted as multiples of the length-at-50%-maturity
#'   (`L50`) of the paired stock rather than absolute length values. A
#'   [Maturity()] object must be available to [PopulateSelectivity()] for
#'   scaling to occur. Default `FALSE`.
#' @param isAtLength Logical. Whether `MeanAtLength`/`MeanAtWeight` (or an
#'   at-length/at-weight `Model`) reflect a genuinely length- or weight-
#'   selectivity schedule. Set `FALSE` when selectivity is only meaningfully
#'   defined at age (e.g. imported from an age-structured assessment) so that
#'   fleet weight-at-age calculations skip weighting by this object's at-length
#'   schedule and use the stock's plain weight-at-age
#'   instead. Default `TRUE`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [selectivity-class] object, or a [fleet-class] object for
#'   `Selectivity<-`.
#' @param value For `Selectivity<-`: a [selectivity-class] object. For
#'   `isRel<-`: a logical value.
#'
#' @details
#' Selectivity is required for all [fleet-class] objects. It defines the
#' probability that a fish of a given age, length, or weight is caught by the
#' gear, on a scale from 0 to 1.
#'
#' ## Specifying Selectivity
#'
#' There are two ways to specify selectivity:
#'
#' **Model-based** (recommended): supply `Pars` as a named list whose element
#' names match the arguments of a built-in or custom model function. The model
#' is resolved automatically unless `Model` is specified explicitly. See
#' [SelectivityModels()] for available models and their required parameters,
#' and [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for
#' how parameter values are structured across simulations and years.
#'
#' Available model families are: logistic (at-age, at-length, at-weight),
#' knife-edge (at-age, at-length), and double-normal (at-length, at-weight).
#' At-length and at-weight schedules are converted to at-age internally using
#' the age-length or age-weight key.
#'
#' **Direct array**: leave `Pars = list()` and supply `MeanAtAge`,
#' `MeanAtLength`, or `MeanAtWeight` directly. If `MeanAtLength` is provided,
#' it takes precedence and `MeanAtAge` is derived from it. If only `MeanAtAge`
#' is provided, `MeanAtLength` is calculated from it unless already populated.
#'
#' These two approaches are mutually exclusive: if `Pars` is non-empty and
#' `Model` can be resolved, any values in `MeanAtAge` will be overwritten.
#'
#' ## Relative Parameters
#'
#' When `isRel = TRUE`, length-based parameters (e.g., `L5`, `LFS`) are
#' scaled by the maturity `L50` of the paired stock before the selectivity
#' curve is computed. This allows selectivity to be expressed as a fraction of
#' the length at 50% maturity rather than an absolute length, which is useful
#' when the same fleet configuration is applied across stocks with different
#' growth characteristics.
#'
#' ## Attaching to a Fleet
#'
#' A [selectivity-class] object can be attached to a [Fleet()] with
#' `Selectivity(Fleet) <- MySelectivity` and retrieved with
#' `Selectivity(Fleet)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [MeanAtAge()], [MeanAtLength()], [MeanAtWeight()], [Classes()], and
#' [isRel()].
#'
#' @return
#' - `Selectivity()` returns a [selectivity-class] object. If `Pars` is a
#'   [fleet-class] object, the `Selectivity` slot of that fleet is returned.
#' - `Selectivity<-` returns `x` with the `Selectivity` slot replaced by
#'   `value`.
#' - `isRel()` returns the `isRel` slot from `x`.
#' - `isRel<-` returns `x` with the `isRel` slot updated.
#'
#' @seealso
#' - [selectivity-class] for the class definition and slot-level
#'   documentation.
#' - [SelectivityModels()] for available model functions and their parameters.
#' - [Fleet()] for the enclosing fleet constructor.
#' - [Retention()], [DiscardMortality()] for related fleet components.
#' - [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for how
#'   `Pars`, `Model`, and `MeanAt*` arrays are structured.
#' - [PopulateSelectivity()] for population details.
#'
#' @family fleet
#'
#' @example man-examples/class-Selectivity.R
#'
#' @export
Selectivity <- function(Pars         = list(),
                        Model        = NULL,
                        MeanAtAge    = NULL,
                        MeanAtLength = NULL,
                        MeanAtWeight = NULL,
                        Classes      = NULL,
                        isRel        = FALSE,
                        isAtLength   = TRUE,
                        Misc         = list()) {

  if (.IsFleetOrList(Pars))
    return(.ExtractFleetSlot(Pars, 'Selectivity'))

  if (inherits(Pars, 'advice'))
    return(.AccessSlot(Pars, 'Selectivity'))

  if (!inherits(Pars, 'list'))
    cli::cli_abort(c(
      'x' = '`Pars` must be a list',
      'i' = 'Currently as {.cls {class(Pars)}} object'
    ))
    
  methods::new(
    "selectivity",
    Pars         = Pars,
    Model        = Model,
    isRel        = isRel,
    isAtLength   = isAtLength,
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes      = Classes,
    Misc         = Misc
  )
}


#' @rdname Selectivity
#' @export
isRel <- function(x) {
  .CheckClass(x, "selectivity", "x")
  x@isRel
}

#' @rdname Selectivity
#' @export
`isRel<-` <- function(x, value) {
  .CheckClass(x, "selectivity", "x")
  x@isRel <- value
  methods::validObject(x)
  x
}

#' @rdname Selectivity
#' @export
`Selectivity<-`<- function(x,value) {
  .AssignFleetSlot(x, value, 'Selectivity')
}











