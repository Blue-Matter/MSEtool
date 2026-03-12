#' Weight
#'
#' Construct and manipulate a [weight-class] object defining weight-at-age
#' and weight-at-length schedules associated with a [Stock()] object. A
#' `Weight` object is required for all [stock-class] objects.
#'
#' @param Pars Named list of growth parameters corresponding to `Model`. See
#'   [WeightModels()] for available models and required parameters. If `Pars`
#'   is a [stock-class] object, the `Weight` slot of that stock is returned.
#'   If `Pars` is non-empty, `Model` is `NULL`, and all values are non-`NA`,
#'   the model is inferred automatically via [FindModel()].
#' @param Model Character or function. Weight model identifier. If `NULL`
#'   (default), the model is inferred from `Pars` where possible. See
#'   [WeightModels()] for available models.
#' @param Units Character. Weight units (e.g., `"g"`, `"kg"`). See
#'   [ValidUnits()] for valid options. Default `"g"`.
#' @param MeanAtAge Numeric array. Mean weight at age, with named dimensions
#'   `Sim`, `Age`, and `Year`. Populated automatically during [Populate()] if
#'   `Pars` and `Model` are provided. Default `NULL`.
#' @param MeanAtLength Numeric array. Mean weight at length, with named
#'   dimensions `Sim`, `Length`, and `Year`. Default `NULL`.
#' @param CVatAge Numeric. Coefficient of variation of weight at age, used to
#'   generate the weight distribution within each age class. Default `NULL` as
#'   it is not typically used.
#' @param Dist Character. Distribution of weight-at-age
#'   (e.g., `"lognormal"`). Default `"lognormal"`.
#' @param TruncSD Numeric. Truncation of the weight-at-age distribution in
#'   standard deviation units. Default `2`.
#' @param Timing Numeric. Timing of weight measurement within the time step
#'   (0 = start, 1 = end). Default `0`.
#' @param Random Numeric array. Random effects on weight parameters. Default
#'   `NULL`.
#' @param AWK Numeric array. Age-weight key. Default `NULL`. Only used if 
#' `CVatAge` is not `NULL`.
#' @param Classes Numeric vector. Weight class midpoints. Default `NULL`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [weight-class] object for slot accessors, or a [stock-class]
#'   object for `Weight<-`.
#' @param value For `Weight<-`: a [weight-class] object. For `AWK<-`: the new
#'   age-weight key array.
#'
#' @details
#' A [weight-class] object is required for all [stock-class] objects. It
#' defines how weight varies with age and/or length, and is used throughout
#' the operating model for biomass calculations and selectivity conversions.
#'
#' ## Specifying Weight-at-Age
#'
#' Weight schedules may be specified in two ways:
#'
#' 1. **Model-based**: provide `Pars` and optionally `Model`. `MeanAtAge`
#'    is populated automatically during [Populate()]. If `Model` is `NULL`
#'    and `Pars` is non-empty with no `NA` values, the model is inferred via
#'    [FindModel()].
#' 2. **Direct array**: provide `MeanAtAge` or `MeanAtLength` directly with
#'    `Pars = list()` (default).
#'
#' ## Weight Distribution
#'
#' Within each age class, weights are assumed to follow the distribution
#' specified by `Dist`, with coefficient of variation `CVatAge`, truncated at
#' `TruncSD` standard deviations. This distribution is used to construct the
#' age-weight key (`AWK`).
#'
#' ## Attaching to a Stock
#'
#' A `Weight` object can be attached to a [Stock()] with
#' `Weight(Stock) <- MyWeight` and retrieved with `Weight(Stock)`.
#'
#' Individual slots may be accessed or modified using [Pars()], [Model()],
#' [Units()], [MeanAtAge()], [MeanAtLength()], [CVatAge()], [Dist()],
#' [TruncSD()], [Timing()], [AWK()], and [Classes()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Weight()` returns a [weight-class] object. If `Pars` is a [stock-class]
#'   object, the `Weight` slot of that stock is returned.
#' - `Weight<-` returns `x` with the `Weight` slot replaced.
#' - `AWK()` returns the age-weight key from `x`.
#' - `AWK<-` returns `x` with the `AWK` slot updated.
#'
#' @seealso [weight-class], [Stock()], [WeightModels()], [ValidUnits()],
#'   [Populate()], [Length()], [Maturity()], [Pars()]
#'   
#' @export
Weight <- function(Pars = list(),
                   Model = NULL,
                   Units = "g",
                   MeanAtAge = NULL,
                   MeanAtLength = NULL,
                   CVatAge = NULL,
                   Dist = "lognormal",
                   TruncSD = 2,
                   Timing = 0,
                   Random = NULL,
                   AWK = NULL,
                   Classes = NULL,
                   Misc = list()) {
  
  if (inherits(Pars, 'stock'))
    return(Pars@Weight)
  
  object <- methods::new(
    "weight",
    Pars          = Pars,
    Model         = Model,
    Units         = Units,
    MeanAtAge     = MeanAtAge,
    MeanAtLength  = MeanAtLength,
    CVatAge       = CVatAge,
    Dist          = Dist,
    TruncSD       = TruncSD,
    Timing        = Timing,
    Random        = Random,
    AWK           = AWK,
    Classes       = Classes,
    Misc          = Misc
  )
  
  if (length(Pars)>0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars))) &&
      is.null(Model))
    object@Model <- FindModel(object)
  
  methods::validObject(object)
  object
}


#' @rdname Weight
#' @export
AWK <- function(x) {
  CheckClass(x, "weight", "x")
  x@AWK
}

#' @rdname Weight
#' @export
`AWK<-` <- function(x, value) {
  CheckClass(x, "weight", "x")
  x@AWK <- value
  methods::validObject(x)
  x
}

#' @rdname Weight
#' @export
`Weight<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  AssignSlot(x, value, 'Weight')
}



