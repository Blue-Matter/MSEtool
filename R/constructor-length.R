#' Length Constructor and Accessors
#'
#' Construct a [length-class] object defining the length-at-age schedule for a
#' [stock-class], or access and replace the `Length` slot of a [stock-class]
#' and its individual slots. A `Length` object is required for all
#' [stock-class] objects.
#'
#' @param Pars `list`. Named list of growth parameters. Element names must
#'   match the arguments of a built-in growth model (see [LengthModels()]).
#'   When `Pars` is non-empty and `Model` is `NULL`, the model is inferred
#'   automatically by [FindModel()]. When `Pars` is a [stock-class] object,
#'   `Length()` acts as a pass-through accessor and returns `x@Length`.
#'   See *Specifying the Length-at-Age* and 
#'   [Specifying Biological and Fleet Schedules][populating-schedules] for the full
#'   set of accepted input formats. Default `list()`.
#' @param Model `character(1)` or `function`. Growth model identifier. When
#'   `NULL` (default), the model is inferred from `Pars` via [FindModel()].
#'   May be set to a character string naming a built-in model or to a custom
#'   R function — see [Specifying Biological and Fleet Schedules][populating-schedules].
#' @param Units `character(1)`. Physical unit of length measurements. Must be
#'   one of the strings returned by [ValidUnits()] (e.g., `"mm"`, `"cm"`).
#'   Default `"mm"`.
#' @param MeanAtAge `array` or `NULL`. Mean length at age with named dimensions
#'   `Sim × Age × Year`. Supply directly when bypassing the model-based
#'   approach (`Pars = list()`). Only the years at which values *change* need
#'   to be included; [Extend()] fills the remainder. When `Pars` contains a
#'   matched model, any values here are **overwritten** during [Populate()].
#'   A numeric vector of length `nAge` is also accepted and is promoted to a
#'   `1 × nAge × 1` array automatically. Default `NULL`.
#' @param CVatAge `numeric(1)`, length-2 bounds vector, or `array`. Coefficient
#'   of variation of length-at-age within each age class, used to parameterise
#'   the within-age-class length distribution and to build the age-length key
#'   (`ALK`). A length-2 vector is treated as `Uniform(lower, upper)` bounds
#'   sampled once per simulation. Default `0.1`.
#' @param Dist `character(1)`. Parametric distribution for length-at-age
#'   variability. Options are: `"normal"` (default) and `"lognormal"`).
#' @param TruncSD `numeric(1)`. Number of standard deviations at which the
#'   length distribution is truncated, preventing unrealistic tails in the
#'   `ALK`. Default `2`.
#' @param Timing `numeric(1)`. Fractional position within the time step at
#'   which length is measured: `0` = start of step, `1` = end of step.
#'   Shifts age class midpoints before computing the `ALK`. Relevant for
#'   seasonal models where growth occurs within a step. Default `0`.
#' @param Random `array` or `NULL`. Simulation- and year-specific random
#'   multipliers applied to growth parameters, enabling inter-annual or
#'   among-simulation variation in growth. Currently stored but not applied.
#'   Default `NULL`.
#' @param ALK `array` or `NULL`. Age-length key giving the probability that an
#'   individual of a given age belongs to each length class
#'   (`Sim × Age × Class × Year`). Populated automatically during [Populate()]
#'   from `MeanAtAge`, `CVatAge`, `Dist`, and `TruncSD`; supply directly only
#'   when overriding the computed key. Default `NULL`.
#' @param Classes `numeric` or `NULL`. Length class midpoints in units of
#'   `Units`. Populated automatically during [Populate()] if `NULL`. Default
#'   `NULL`.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x A [length-class] object for slot accessors, or a [stock-class]
#'   object for `Length<-`.
#' @param value For `Length<-`: a [length-class] object. For `ALK<-`: a
#'   replacement age-length key array.
#'
#' @details
#' ## Specifying Length-at-Age
#'
#' There are two ways to define the length schedule:
#'
#' 1. **Model-based** (recommended): supply `Pars` with the named parameters
#'    for your chosen growth model (e.g., `list(Linf = 80, K = 0.2, t0 =
#'    -0.5)` for von Bertalanffy). If `Model` is `NULL` and the parameter
#'    names uniquely identify a model, [FindModel()] sets `Model`
#'    automatically. The `MeanAtAge` array and `ALK` are then populated by
#'    [Populate()] when the stock is added to an [OM()].
#'
#' 2. **Direct array**: supply `MeanAtAge` with `Pars = list()`. Any values
#'    already present in `MeanAtAge` are preserved and are not overwritten
#'    during [Populate()].
#'
#' See [Specifying Biological and Fleet Schedules][populating-schedules] for more details.
#'   
#' ## Length Distribution and the ALK
#'
#' Within each age class, lengths follow the distribution given by `Dist`,
#' centred on `MeanAtAge` with spread `CVatAge`, truncated at `TruncSD`
#' standard deviations. These parameters are used to build the age-length key
#' (`ALK`), which maps age-structured abundance to length-structured
#' quantities throughout the operating model. Retrieve the computed key with
#' `ALK(x)`.
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Pars` is a [stock-class] object, `Length()` returns the `Length`
#' slot directly:
#'
#' ```r
#' Length(my_stock)           # returns my_stock@Length
#' Length(my_stock) <- my_L   # replaces my_stock@Length
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' Pars(len)         <- list(Linf = 80, K = 0.2, t0 = -0.5)
#' Model(len)        <- "vonBert"
#' Units(len)        <- "cm"
#' MeanAtAge(len)    <- my_array
#' CVatAge(len)      <- 0.08
#' Dist(len)         <- "normal"
#' TruncSD(len)      <- 3
#' Timing(len)       <- 0
#' ALK(len)          <- my_alk
#' Classes(len)      <- seq(5, 80, by = 5)
#' ```
#'
#' @return
#' - `Length()` returns a [length-class] object. If `Pars` is a [stock-class],
#' returns `x@Length`.
#' - `Length<-` returns the [stock-class] `x` with the `Length` slot replaced by
#' `value` and the object re-validated.
#' - `ALK()` returns the age-length key array from `x`.
#' - `ALK<-` returns `x` with the `ALK` slot updated and the object
#' re-validated.
#'
#' @seealso
#' - [length-class] for the class definition and slot-level documentation.
#' - [LengthModels()] for available growth models and required parameter sets.
#' - [ValidUnits()] for accepted unit strings.
#' - [FindModel()] for automatic model inference. 
#' - [Populate()] for array population. 
#' - [Stock()] for the enclosing stock constructor. 
#'
#' @family length
#'
#' @example man-examples/class-Length.R
#'
#' @export
Length <- function(Pars = list(),
                   Model = NULL,
                   Units = "mm",
                   MeanAtAge = NULL,
                   CVatAge = 0.1,
                   Dist = "normal",
                   TruncSD = 2,
                   Timing = 0,
                   Random = NULL,
                   ALK = NULL,
                   Classes = NULL,
                   Misc = list()) {
  
  if (isStockOrList(Pars)) 
    return(ExtractStockSlot(Pars, "Length"))
  
  if (is.null(Pars))
    return(NULL)
  
  object <- methods::new(
    "length",
    Pars      = Pars,
    Model     = Model,
    Units     = Units,
    MeanAtAge = MeanAtAge,
    CVatAge   = CVatAge,
    Dist      = Dist,
    TruncSD   = TruncSD,
    Timing    = Timing,
    Random    = Random,
    ALK       = ALK,
    Classes   = Classes,
    Misc      = Misc
  )
  
  if (length(Pars) > 0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars))) &&
      is.null(Model))
    object@Model <- FindModel(object)
  
  methods::validObject(object)
  object
}

#' @rdname Length
#' @export
`Length<-` <- function(x, value) {
  AssignSlotRecursive(x, value, 'Length')
}



#' @rdname Length
#' @export
ALK <- function(x) {
  CheckClass(x, "length", "x")
  x@ALK
}

#' @rdname Length
#' @export
`ALK<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@ALK <- value
  methods::validObject(x)
  x
}




