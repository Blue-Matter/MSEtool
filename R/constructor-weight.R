#' Weight Constructor and Accessors
#'
#' Construct a [weight-class] object defining the weight-at-age and
#' weight-at-length schedules for a [stock-class], or access and replace the
#' `Weight` slot of a [stock-class] and its individual slots. A `Weight`
#' object is required for all [stock-class] objects.
#'
#' @param Pars `list`. Named list of allometric parameters. Element names must
#'   match the arguments of a built-in weight model (see [WeightModels()]).
#'   When `Pars` is non-empty and `Model` is `NULL`, the model is inferred
#'   automatically by `.FindModel()`. When `Pars` is a [stock-class] object,
#'   `Weight()` acts as a pass-through accessor and returns `x@Weight`.
#'   See *Specifying Weight-at-Age* and
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for the
#'   full set of accepted input formats. Default `list()`.
#' @param Model `character(1)` or `function`. Weight model identifier. When
#'   `NULL` (default), the model is inferred from `Pars` via `.FindModel()`.
#'   May be set to a character string naming a built-in model or to a custom
#'   R function — see [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @param Units `character(1)`. Physical unit of weight measurements. Must be
#'   one of the strings returned by [ValidUnits()] (e.g., `"g"`, `"kg"`).
#'   Default `"kg"`.
#' @param MeanAtAge `array` or `NULL`. Mean weight at age with named dimensions
#'   `Sim × Age × Year`. Supply directly when bypassing the model-based
#'   approach (`Pars = list()`). Only the years at which values *change* need
#'   to be included; [Extend()] fills the remainder. When `Pars` contains a
#'   matched model, any values here are **overwritten** during [Populate()].
#'   A numeric vector of length `nAge` is also accepted and is promoted to a
#'   `1 × nAge × 1` array automatically. Default `NULL`.
#' @param MeanAtLength `array` or `NULL`. Mean weight at length with named
#'   dimensions `Sim × Length × Year`. Populated automatically during
#'   [Populate()] when an at-length weight model is used (see *At-Length
#'   Models*); may also be supplied directly. When `MeanAtLength` is populated
#'   and `MeanAtAge` is not, [Populate()] converts it to `MeanAtAge` via the
#'   age-length key (`ALK`) from a [length-class] object. Default `NULL`.
#' @param CVatAge `numeric(1)`, length-2 bounds vector, `array`, or `NULL`.
#'   Coefficient of variation of weight-at-age, used to parameterise the
#'   within-age-class weight distribution and to build the age-weight key
#'   (`AWK`). A length-2 vector is treated as `Uniform(lower, upper)` bounds
#'   sampled once per simulation. When `NULL` (default), no `AWK` is
#'   constructed; for most models weight variability is propagated through the
#'   `ALK` and a length-weight relationship rather than through `CVatAge`
#'   directly.
#' @param Dist `character(1)`. Parametric distribution for weight-at-age
#'   variability. Options are `"lognormal"` (default) and `"normal"`.
#' @param TruncSD `numeric(1)`. Number of standard deviations at which the
#'   weight distribution is truncated. Default `2`.
#' @param Timing `numeric(1)`. Fractional position within the time step at
#'   which weight is measured: `0` = start of step, `1` = end of step.
#'   Default `0`.
#' @param Random `array` or `NULL`. Simulation- and year-specific random
#'   multipliers applied to weight parameters. Currently stored but not
#'   applied. Default `NULL`.
#' @param AWK `array` or `NULL`. Age-weight key giving the probability that an
#'   individual of a given age belongs to each weight class
#'   (`Sim × Age × Class × Year`). Only populated during [Populate()] when
#'   `CVatAge` is non-`NULL`; supply directly only when overriding the
#'   computed key. Default `NULL`.
#' @param Classes `numeric` or `NULL`. Weight class midpoints in units of
#'   `Units`. Populated automatically during [Populate()] when `CVatAge` is
#'   non-`NULL` and `Classes` is `NULL`. Default `NULL`.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x A [weight-class] object for slot accessors, or a [stock-class]
#'   object for `Weight<-`.
#' @param value For `Weight<-`: a [weight-class] object. For `AWK<-`: a
#'   replacement age-weight key array.
#'
#' @details
#' ## Specifying Weight-at-Age
#'
#' There are two ways to define the weight schedule:
#'
#' **Model-based** (recommended): supply `Pars` with named parameters matching
#' a built-in model (see [WeightModels()]). If `Model = NULL` and the parameter
#' names uniquely match a model, `.FindModel()` resolves the model
#' automatically. `MeanAtAge` is then populated by [Populate()] when the stock
#' is added to an [OM()]:
#'
#' ```r
#' # Power-law length-weight — model inferred from Pars names
#' wt <- Weight(Pars = list(a = 0.01, b = 3.0))
#'
#' # Stochastic a across simulations
#' wt <- Weight(Pars = list(a = c(0.008, 0.012), b = 3.0))
#'
#' # Time-varying a (increases in 2010; Extend() fills the rest)
#' a_arr <- array(c(0.01, 0.012), dim = c(1, 2),
#'                dimnames = list(Sim = 1, Year = c(1990, 2010)))
#' wt <- Weight(Pars = list(a = a_arr, b = 3.0))
#'
#' # Inter-annual random walk on a (log-normal, SD = 0.1)
#' wt <- Weight(Pars = list(a = 0.01, aSD = 0.1, b = 3.0))
#' ```
#'
#' **Direct array**: supply `MeanAtAge` (or `MeanAtLength`) with
#' `Pars = list()`. Values are preserved during [Populate()]:
#'
#' ```r
#' ages  <- 0:20
#' wt_aa <- Weight(MeanAtAge = 0.01 * (100 * (1 - exp(-0.2 * ages)))^3)
#' ```
#'
#' See [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for full
#' details on input formats and the rules that govern how `Pars`, `Model`, and
#' `MeanAt*` arrays interact.
#' 
#' ## At-Length Weight Models
#'
#' Some built-in weight models (those whose class contains `"at-Length"`) take
#' a [length-class] object as input and produce `MeanAtLength` rather than
#' `MeanAtAge` directly. In this case a populated [Length()] object must be
#' passed to [Populate()]. The resulting `MeanAtLength` is then converted to
#' `MeanAtAge` automatically via the age-length key (`ALK`). The same
#' conversion applies when `MeanAtLength` is supplied directly by the user.
#'
#' ## The Age-Weight Key
#'
#' The `AWK` is only constructed when `CVatAge` is non-`NULL`. For most
#' operating models weight variability is captured through the `ALK` and a
#' length-weight relationship, so `CVatAge` is typically left at its default
#' of `NULL`.
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Pars` is a [stock-class] object, `Weight()` returns the `Weight`
#' slot directly:
#'
#' ```r
#' Weight(my_stock)           # returns my_stock@Weight
#' Weight(my_stock) <- my_wt  # replaces my_stock@Weight
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' Pars(wt)          <- list(a = 0.01, b = 3.0)
#' Model(wt)         <- "LengthWeight"
#' Units(wt)         <- "kg"
#' MeanAtAge(wt)     <- my_array
#' MeanAtLength(wt)  <- my_length_array
#' CVatAge(wt)       <- 0.1
#' Dist(wt)          <- "lognormal"
#' TruncSD(wt)       <- 3
#' Timing(wt)        <- 0
#' AWK(wt)           <- my_awk
#' Classes(wt)       <- seq(0, 5, by = 0.5)
#' ```
#'
#' @return
#' - `Weight()` returns a [weight-class] object. If `Pars` is a [stock-class],
#'   returns `x@Weight`.
#' - `Weight<-` returns the [stock-class] `x` with the `Weight` slot replaced
#'   and the object re-validated.
#' - `AWK()` returns the age-weight key array from `x`.
#' - `AWK<-` returns `x` with the `AWK` slot updated and the object
#'   re-validated.
#'
#' @seealso
#' - [weight-class] for the class definition and slot-level documentation.
#' - [WeightModels()] for available weight models and required parameter
#'   sets.
#' - [ValidUnits()] for accepted unit strings. 
#' - `.FindModel()` for automatic model inference.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#' - [Length()] for the companion length schedule, required when using
#'   at-length weight models.
#'
#' @family weight
#'
#' @example man-examples/class-Weight.R
#'
#' @export
Weight <- function(Pars = list(),
                   Model = NULL,
                   Units = "kg",
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
  
  if (.IsStockOrList(Pars)) 
    return(.ExtractStockSlot(Pars, "Weight"))
  
  if (is.null(Pars))
    return(NULL)
  
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
    object@Model <- .FindModel(object)
  
  methods::validObject(object)
  object
}

#' @rdname Weight
#' @export
`Weight<-` <- function(x, value) {
  .AssignSlotRecursive(x, value, 'Weight')
}



#' @rdname Weight
#' @export
AWK <- function(x) {
  .CheckClass(x, "weight", "x")
  x@AWK
}

#' @rdname Weight
#' @export
`AWK<-` <- function(x, value) {
  .CheckClass(x, "weight", "x")
  x@AWK <- value
  methods::validObject(x)
  x
}


