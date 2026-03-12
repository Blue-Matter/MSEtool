#' NaturalMortality
#'
#' Construct and manipulate a [naturalmortality-class] object defining the
#' natural mortality schedule associated with a [Stock()] object. A
#' `NaturalMortality` object is required for all [stock-class] objects.
#'
#' @param Pars Named list of natural mortality parameters corresponding to
#'   `Model`. See [NaturalMortalityModels()] for available models and required
#'   parameters. If `Pars` is a [stock-class] object, the `NaturalMortality`
#'   slot of that stock is returned. If `Pars` is non-empty, `Model` is
#'   `NULL`, and all values are non-`NA`, the model is inferred automatically
#'   via [FindModel()].
#' @param Model Character or function. Natural mortality model identifier. If
#'   `NULL` (default), the model is inferred from `Pars` where possible. See
#'   [NaturalMortalityModels()] for available models.
#' @param Units Character. Time units for the mortality rate (e.g., `"year"`).
#'   See [ValidUnits()] for valid options. Default `"year"`.
#' @param MeanAtAge Numeric array. Mean natural mortality at age, with named
#'   dimensions `Sim`, `Age`, and `Year`. Populated automatically during
#'   [Populate()] if `Pars` and `Model` are provided. Default `NULL`.
#' @param MeanAtLength Numeric array. Mean natural mortality at length, with
#'   named dimensions `Sim`, `Length`, and `Year`. Default `NULL`.
#' @param Random Numeric array. Random effects on natural mortality parameters.
#'   Default `NULL`.
#' @param Classes Numeric vector. Age or length class midpoints. Default
#'   `NULL`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [naturalmortality-class] object for slot accessors, or a
#'   [stock-class] object for `NaturalMortality<-`.
#' @param value A [naturalmortality-class] object.
#'
#' @details
#' A [naturalmortality-class] object is required for all [stock-class]
#' objects. It defines how natural mortality varies with age and/or length,
#' which is used throughout the operating model for population dynamics
#' calculations.
#'
#' ## Specifying Natural Mortality
#'
#' Natural mortality schedules may be specified in two ways:
#'
#' 1. **Model-based**: provide `Pars` and optionally `Model`. `MeanAtAge`
#'    is populated automatically during [Populate()]. If `Model` is `NULL`
#'    and `Pars` is non-empty with no `NA` values, the model is inferred via
#'    [FindModel()].
#' 2. **Direct array**: provide `MeanAtAge` or `MeanAtLength` directly with
#'    `Pars = list()` (default).
#'
#' ## Attaching to a Stock
#'
#' A `NaturalMortality` object can be attached to a [Stock()] with
#' `NaturalMortality(Stock) <- MyNaturalMortality` and retrieved with
#' `NaturalMortality(Stock)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [Units()], [MeanAtAge()], [MeanAtLength()], [Random()], and [Classes()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `NaturalMortality()` returns a [naturalmortality-class] object. If
#'   `Pars` is a [stock-class] object, the `NaturalMortality` slot of that
#'   stock is returned.
#' - `NaturalMortality<-` returns `x` with the `NaturalMortality` slot
#'   replaced.
#'
#' @seealso [naturalmortality-class], [Stock()], [NaturalMortalityModels()],
#'   [Populate()], [FindModel()], [ValidUnits()], [Length()], [Maturity()],
#'   [Pars()], [MeanAtAge()], [MeanAtLength()], [Units()], [Classes()]
#'
#' @export
NaturalMortality <- function(Pars = list(),
                             Model = NULL,
                             Units = "year",
                             MeanAtAge = NULL,
                             MeanAtLength = NULL,
                             Random = NULL,
                             Classes = NULL,
                             Misc = list()) {
  
  if (inherits(Pars, 'stock'))
    return(Pars@NaturalMortality)
  
  object <- methods::new(
    "naturalmortality",
    Pars          = Pars,
    Model         = Model,
    Units         = Units,
    MeanAtAge     = MeanAtAge,
    MeanAtLength  = MeanAtLength,
    Random        = Random,
    Classes       = Classes,
    Misc          = Misc
  )
  
  if (length(Pars) > 0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars))) &&
      is.null(Model)) {
    object@Model <- FindModel(object)
  }
  
  methods::validObject(object)
  object
}


#' @rdname NaturalMortality
#' @export
`NaturalMortality<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  AssignSlot(x, value, 'NaturalMortality')
}
