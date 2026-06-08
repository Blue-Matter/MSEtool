#' Retention
#'
#' Construct and manipulate a [retention-class] object defining
#' retention-at-age, retention-at-length, or retention-at-weight for a
#' [Fleet()] object. Retention is optional; if not specified, all age and
#' length classes are assumed fully retained.
#'
#' @param Pars Named list of retention parameters passed to the retention
#'   model function. Parameter names must match the arguments of the chosen
#'   model (see [RetentionModels()]). Each element may be a scalar, vector, or
#'   array; see [Specifying Biological and Fleet
#'   Schedules][populating-schedules] for accepted formats. Default `list()`
#'   (empty).
#'
#'   If `Pars` and `Model` are both provided, `MeanAtAge` is computed from the
#'   model and any values already in `MeanAtAge` are overwritten. To preserve
#'   a user-specified `MeanAtAge`, leave `Pars = list()` (the default).
#'
#'   If `Pars` is a [fleet-class] object, the `Retention` slot of that fleet
#'   is returned.
#'
#' @param Model Character or function or `NULL`. Retention model identifier.
#'   If `NULL` (default), the model is inferred automatically from the names
#'   in `Pars` via [FindModel()]. May also be supplied as a custom R function
#'   with arguments matching those in `Pars`. See [RetentionModels()] for
#'   built-in options.
#' @param MeanAtAge Numeric array or `NULL`. Mean retention-at-age with
#'   dimensions `Sim x Age x Year` (area dimension added during population).
#'   Used directly when `Pars` is empty and `MeanAtLength` is not supplied.
#'   See [Specifying Biological and Fleet Schedules][populating-schedules] for
#'   accepted array formats. **Note:** if `Pars` and `Model` are both
#'   provided, any values supplied here will be overwritten during population.
#' @param MeanAtLength Numeric array or `NULL`. Mean retention-at-length with
#'   dimensions `Sim x Length x Year`. If provided and `Pars` is empty, takes
#'   precedence over `MeanAtAge`; `MeanAtAge` is derived from it via the
#'   age-length key.
#' @param MeanAtWeight Numeric array or `NULL`. Mean retention-at-weight with
#'   dimensions `Sim x Weight x Year`. Default `NULL`.
#' @param Classes Numeric vector or `NULL`. Length or weight class midpoints
#'   corresponding to the second dimension of `MeanAtLength` or
#'   `MeanAtWeight`. Default `NULL`.
#' @param isRel Logical. If `TRUE`, length-based parameters (e.g., `LR5`,
#'   `LFR`) are interpreted as multiples of the length-at-50%-maturity
#'   (`L50`) of the paired stock rather than absolute length values. A
#'   [Maturity()] object must be available to [PopulateRetention()] for
#'   scaling to occur. Default `FALSE`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [retention-class] object, or a [fleet-class] object for
#'   `Retention<-`.
#' @param value For `Retention<-`: a [retention-class] object.
#'
#' @details
#' Retention is optional for all [fleet-class] objects. It defines the
#' probability that a selected fish is retained, on a scale from 0 to 1.
#' Fish that are selected but not retained are treated as discards, and their
#' fate is determined by [DiscardMortality()].
#'
#' When no [retention-class] object is supplied, [PopulateRetention()] sets
#' full retention (1) for all age and length classes.
#'
#' ## Specifying Retention
#'
#' There are two ways to specify retention:
#'
#' **Model-based** (recommended): supply `Pars` as a named list whose element
#' names match the arguments of a built-in or custom model function. The model
#' is resolved automatically unless `Model` is specified explicitly. See
#' [RetentionModels()] for available models and their required parameters, and
#' [Specifying Biological and Fleet Schedules][populating-schedules] for how
#' parameter values are structured across simulations and years.
#'
#' Available model families are: logistic (at-age, at-length, at-weight, with
#' optional `MaxRet` asymptote), knife-edge (at-age, at-length), and
#' double-normal (at-length, at-weight). At-length and at-weight schedules are
#' converted to at-age internally using the age-length or age-weight key.
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
#' When `isRel = TRUE`, length-based parameters (e.g., `LR5`, `LFR`) are
#' scaled by the maturity `L50` of the paired stock before the retention curve
#' is computed.
#'
#' ## Attaching to a Fleet
#'
#' A [retention-class] object can be attached to a [Fleet()] with
#' `Retention(Fleet) <- MyRetention` and retrieved with `Retention(Fleet)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [MeanAtAge()], [MeanAtLength()], [MeanAtWeight()], and [Classes()].
#'
#' @return
#' - `Retention()` returns a [retention-class] object. If `Pars` is a
#'   [fleet-class] object, the `Retention` slot of that fleet is returned.
#' - `Retention<-` returns `x` with the `Retention` slot replaced by `value`.
#'
#' @seealso
#' - [retention-class] for the class definition and slot-level documentation.
#' - [RetentionModels()] for available model functions and their parameters.
#' - [Fleet()] for the enclosing fleet constructor.
#' - [Selectivity()], [DiscardMortality()] for related fleet components.
#' - [Specifying Biological and Fleet Schedules][populating-schedules] for how
#'   `Pars`, `Model`, and `MeanAt*` arrays are structured.
#' - [PopulateRetention()] for population details.
#'
#' @family fleet
#'
#' @examples
#' # See man-examples/class-Retention.R
#'
#' @export
Retention <- function(Pars         = list(),
                      Model        = NULL,
                      MeanAtAge    = NULL,
                      MeanAtLength = NULL,
                      MeanAtWeight = NULL,
                      Classes      = NULL,
                      isRel        = FALSE,
                      Misc         = list()) {
  
  if (inherits(Pars, "fleet"))
    return(Pars@Retention)
  
  if (inherits(Pars, "om"))
    return(purrr::map(Pars@Fleet, \(FleetList)
                      purrr::map(FleetList, \(fleet) fleet@Retention)
    ))
  
  if (inherits(Pars, "StockFleetList"))
    return(purrr::map(Pars, \(FleetList)
                      purrr::map(FleetList, \(fleet) fleet@Retention)
    ))
  
  if (inherits(Pars, "FleetList"))
    return(purrr::map(Pars, \(fleet) fleet@Retention))
  
  if (!inherits(Pars, 'list'))
    cli::cli_abort(c(
      'x' = '`Pars` must be a list',
      'i' = 'Currently as {.cls {class(Pars)}} object'
    ))
  
  object <- methods::new(
    "retention",
    Pars         = Pars,
    Model        = Model,
    isRel        = isRel,
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes      = Classes,
    Misc         = Misc
  )
  object
}

#' @rdname Retention
#' @export
`Retention<-` <- function(x,value) {
  assign_fleet_slot(x, value, "Retention", "retention")
}




