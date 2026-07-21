#' NaturalMortality Constructor and Accessors
#'
#' Construct a [naturalmortality-class] object defining the natural mortality
#' schedule for a [stock-class], or access and replace the `NaturalMortality`
#' slot of a [stock-class] and its individual slots. A `NaturalMortality`
#' object is required for all [stock-class] objects.
#'
#' @param Pars `list`. Named list of natural mortality parameters. Element
#'   names must match the arguments of a built-in mortality model (see
#'   [NaturalMortalityModels()]). When `Pars` is non-empty and `Model` is
#'   `NULL`, the model is inferred automatically by `.FindModel()`. When `Pars`
#'   is a [stock-class] object, `NaturalMortality()` acts as a pass-through
#'   accessor and returns `x@NaturalMortality`. See also
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for the
#'   full set of accepted input formats. Default `list()`.
#' @param Model `character(1)` or `function`. Natural mortality model
#'   identifier. When `NULL` (default), the model is inferred from `Pars` via
#'   `.FindModel()`. May be set to a character string naming a built-in model or
#'   to a custom R function — see
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @param Units `character(1)`. Time unit in which mortality rates are
#'   expressed. Must be one of the strings returned by [ValidUnits()] (e.g.,
#'   `"year"` for instantaneous annual mortality). For seasonal models, set
#'   `Units` to match the season length so that within-step rates are scaled
#'   correctly during [Populate()]. Default `"year"`.
#' @param MeanAtAge `array` or `NULL`. Mean instantaneous natural mortality at
#'   age with named dimensions `Sim × Age × Year`. Supply directly when
#'   bypassing the model-based approach (`Pars = list()`). Only the years at
#'   which values *change* need to be included; [Extend()] fills the remainder.
#'   When `Pars` contains a matched model, any values here are **overwritten**
#'   during [Populate()]. A numeric vector of length `nAge` is also accepted
#'   and promoted to a `1 × nAge × 1` array automatically. Default `NULL`.
#' @param MeanAtLength `array` or `NULL`. Mean natural mortality at length with
#'   named dimensions `Sim × Length × Year`. Populated automatically during
#'   [Populate()] when an at-length mortality model is used; may also be
#'   supplied directly. When `MeanAtLength` is populated and `MeanAtAge` is
#'   not, [Populate()] converts it to `MeanAtAge` via the age-length key
#'   (`ALK`) from a [length-class] object. Default `NULL`.
#' @param Random `array` or `NULL`. Reserved for future use. Intended to hold
#'   simulation- and year-specific multipliers that add stochastic variation
#'   around the average mortality schedule in `MeanAtAge`. Currently stored
#'   but not applied during [Populate()]. Default `NULL`.
#' @param Classes `numeric` or `NULL`. Age or length class midpoints
#'   corresponding to the `MeanAt*` array in use. Default `NULL`.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x A [naturalmortality-class] object for slot accessors, or a
#'   [stock-class] object for `NaturalMortality<-`.
#' @param value For `NaturalMortality<-`: a [naturalmortality-class] object.
#'
#' @details
#' ## Specifying Natural Mortality
#'
#' There are two ways to define the mortality schedule; see
#' [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for full
#' details on input formats and the rules that govern how `Pars`, `Model`, and
#' `MeanAt*` arrays interact.
#'
#' **Model-based** (recommended): supply `Pars` with named parameters matching
#' a built-in model (see [NaturalMortalityModels()]). If `Model = NULL` and
#' the parameter names uniquely match a model, `.FindModel()` resolves the
#' model automatically. `MeanAtAge` is then populated by [Populate()] when the
#' stock is added to an [OM()]:
#'
#' ```r
#' # Constant M — model inferred from Pars names
#' nm <- NaturalMortality(Pars = list(M = 0.2))
#'
#' # Stochastic M across simulations
#' nm <- NaturalMortality(Pars = list(M = c(0.1, 0.3)))
#'
#' # Time-varying M (increases in 2010; Extend() fills the rest)
#' M_arr <- array(c(0.2, 0.3), dim = c(1, 2),
#'                dimnames = list(Sim = 1, Year = c(1990, 2010)))
#' nm <- NaturalMortality(Pars = list(M = M_arr))
#'
#' # Inter-annual random walk on M (log-normal, SD = 0.1)
#' nm <- NaturalMortality(Pars = list(M = c(0.1, 0.3), MSD = 0.1))
#' ```
#'
#' **Direct array**: supply `MeanAtAge` (or `MeanAtLength`) with
#' `Pars = list()`. Values are preserved during [Populate()]:
#'
#' ```r
#' ages  <- 0:20
#' # Age-varying M declining from 0.5 at age 0 to 0.1 at the plus group
#' nm_aa <- NaturalMortality(
#'   MeanAtAge = array(
#'     seq(0.5, 0.1, length.out = length(ages)),
#'     dim      = c(1, length(ages), 1),
#'     dimnames = list(Sim = 1, Age = ages, Year = 1990)
#'   )
#' )
#' ```
#'
#' ## Units and Seasonal Models
#'
#' Rates in `MeanAtAge` are instantaneous mortality values (*M*) expressed
#' over the period defined by `Units`. The default `Units = "year"` means
#' annual *M*. For seasonal models, set `Ages@Units` to the season length (e.g.,
#' `"quarter"` for quarterly steps) and make sure the rate parameters (e.g., M and K)
#' are in the same temporal units.
#'
#' ## At-Length Models
#'
#' Models whose class contains `"at-Length"` produce `MeanAtLength`, which is
#' converted to `MeanAtAge` via the `ALK` — a populated [Length()] object must
#' be passed to [Populate()]. The same conversion applies when `MeanAtLength`
#' is supplied directly by the user.
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Pars` is a [stock-class] object, `NaturalMortality()` returns the
#' `NaturalMortality` slot directly:
#'
#' ```r
#' NaturalMortality(my_stock)           # returns my_stock@NaturalMortality
#' NaturalMortality(my_stock) <- my_M   # replaces it
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' Pars(nm)          <- list(M = 0.2)
#' Model(nm)         <- "Constant"
#' Units(nm)         <- "year"
#' MeanAtAge(nm)     <- my_array
#' MeanAtLength(nm)  <- my_length_array
#' Classes(nm)       <- 0:20
#' ```
#'
#' @return
#' - `NaturalMortality()` returns a [naturalmortality-class] object. If `Pars`
#'   is a [stock-class], returns `x@NaturalMortality`.
#' - `NaturalMortality<-` returns the [stock-class] `x` with the
#'   `NaturalMortality` slot replaced and the object re-validated.
#'
#' @seealso
#' - [naturalmortality-class] for the class definition and slot-level
#'   documentation.
#' - [NaturalMortalityModels()] for available models and required parameter
#'   sets.
#' - [ValidUnits()] for accepted unit strings.
#' - `.FindModel()` for automatic model inference.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#' - [Length()] for the companion length schedule, required when using
#'   at-length mortality models.
#'
#' @family naturalmortality
#'
#' @example man-examples/class-NaturalMortality.R
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
  
  if (.IsStockOrList(Pars)) 
    return(.ExtractStockSlot(Pars, "NaturalMortality"))
  
  if (is.null(Pars))
    return(NULL)
  
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
    object@Model <- .FindModel(object)
  }
  
  methods::validObject(object)
  object
}


#' @rdname NaturalMortality
#' @export
`NaturalMortality<-` <- function(x, value) {
  .AssignSlotRecursive(x, value, 'NaturalMortality')
}
