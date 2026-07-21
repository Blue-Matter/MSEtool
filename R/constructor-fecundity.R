#' Fecundity Constructor and Accessors
#'
#' Construct a [fecundity-class] object defining the fecundity schedule for a
#' [stock-class], or access and replace the `Fecundity` slot of a [stock-class]
#' and its individual slots. `Fecundity` is optional — see *Default Behaviour*
#' below.
#'
#' @param Pars `list`. Named list of fecundity parameters. Element names must
#'   match the arguments of a built-in fecundity model (see [FecundityModels()]).
#'   When `Pars` is non-empty and `Model` is `NULL`, the model is inferred
#'   automatically by `.FindModel()`. When `Pars` is a [stock-class] object,
#'   `Fecundity()` acts as a pass-through accessor and returns `x@Fecundity`.
#'   See also
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for the
#'   full set of accepted input formats. Default `list()`.
#' @param Model `character(1)` or `function`. Fecundity model identifier. When
#'   `NULL` (default), the model is inferred from `Pars` via `.FindModel()`.
#'   May be set to a character string naming a built-in model or to a custom
#'   R function — see
#'   [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html).
#' @param Units `character(1)`. Unit of reproductive output (e.g., `"eggs"`).
#'   Sets the unit of the `SProduction` slot in [hist-class] output objects.
#'   Default `"eggs"`.
#' @param MeanAtAge `array` or `NULL`. Mean fecundity-at-age with named
#'   dimensions `Sim × Age × Year`. Supply directly when bypassing the
#'   model-based approach (`Pars = list()`). Only the years at which values
#'   *change* need to be included; [Extend()] fills the remainder. When `Pars`
#'   contains a matched model, any values here are **overwritten** during
#'   [Populate()]. A numeric vector of length `nAge` is also accepted and
#'   promoted to a `1 × nAge × 1` array automatically. Default `NULL`.
#' @param MeanAtLength `array` or `NULL`. Mean fecundity-at-length with named
#'   dimensions `Sim × Length × Year`. Populated automatically during
#'   [Populate()] when an at-length fecundity model is used; may also be
#'   supplied directly. When `MeanAtLength` is populated and `MeanAtAge` is
#'   not, [Populate()] converts it to `MeanAtAge` via the age-length key
#'   (`ALK`) from a [length-class] object. Default `NULL`.
#' @param Classes `numeric` or `NULL`. Age or length class midpoints
#'   corresponding to the `MeanAt*` array in use. Default `NULL`.
#' @param Timing `array` or `NULL`. Reserved for future use. Intended to
#'   represent the timing of spawning within the time step. Currently stored
#'   but not applied; spawning timing is controlled by [SRR()]. Default `NULL`.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x A [fecundity-class] object for slot accessors, or a [stock-class]
#'   object for `Fecundity<-`.
#' @param value For `Fecundity<-`: a [fecundity-class] object.
#'
#' @details
#' ## Default Behaviour When Omitted
#'
#' `Fecundity` is optional. When this object is empty (the default), [Populate()]
#' computes spawning production (`SProduction` in [hist-class] output) as
#' mature weight-at-age — the product of `Weight@MeanAtAge` and
#' `Maturity@MeanAtAge`. In this case `SProduction` equals spawning biomass
#' (`SBiomass`) and both slots carry the same values.
#'
#' When this object is populated, `SProduction` is computed in the units of
#' `Units` (e.g. eggs per individual multiplied through maturity), allowing
#' reproductive output to differ from biomass. This is relevant for species
#' where egg production is not proportional to body weight — for example,
#' batch-spawning fish or species with strongly size-dependent clutch size.
#'
#' ## Specifying Fecundity
#'
#' There are two ways to define the fecundity schedule; see
#' [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for full
#' details on input formats and the rules that govern how `Pars`, `Model`, and
#' `MeanAt*` arrays interact.
#'
#' **Model-based** (recommended): supply `Pars` with named parameters matching
#' a built-in model (see [FecundityModels()]). If `Model = NULL` and the
#' parameter names uniquely match a model, `.FindModel()` resolves the model
#' automatically. `MeanAtAge` is then populated by [Populate()] when the stock
#' is added to an [OM()]:
#'
#' ```r
#' # Egg production proportional to weight — model inferred from Pars names
#' fec <- Fecundity(Pars = list(a = 1000, b = 3.0))
#'
#' # Stochastic a across simulations
#' fec <- Fecundity(Pars = list(a = c(800, 1200), b = 3.0))
#'
#' # Time-varying a (increases in 2010; Extend() fills the rest)
#' a_arr <- array(c(1000, 1200), dim = c(1, 2),
#'                dimnames = list(Sim = 1, Year = c(1990, 2010)))
#' fec <- Fecundity(Pars = list(a = a_arr, b = 3.0))
#'
#' # Inter-annual random walk on a (log-normal, SD = 0.1)
#' fec <- Fecundity(Pars = list(a = c(800, 1200), aSD = 0.1, b = 3.0))
#' ```
#'
#' **Direct array**: supply `MeanAtAge` (or `MeanAtLength`) with
#' `Pars = list()`. Values are preserved during [Populate()]:
#'
#' ```r
#' ages  <- 0:20
#' fec_aa <- Fecundity(
#'   MeanAtAge = array(
#'     1000 * seq(0, 1, length.out = length(ages))^2,
#'     dim      = c(1, length(ages), 1),
#'     dimnames = list(Sim = 1, Age = ages, Year = 1990)
#'   )
#' )
#' ```
#'
#' ## At-Length Models
#'
#' Models whose class contains `"at-Length"` produce `MeanAtLength`, which is
#' converted to `MeanAtAge` via the `ALK` — a populated [Length()] object must
#' be passed to [Populate()].
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Pars` is a [stock-class] object, `Fecundity()` returns the `Fecundity`
#' slot directly:
#'
#' ```r
#' Fecundity(my_stock)             # returns my_stock@Fecundity
#' Fecundity(my_stock) <- my_fec   # replaces my_stock@Fecundity
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' Pars(fec)          <- list(a = 1000, b = 3.0)
#' Model(fec)         <- "EggProduction"
#' Units(fec)         <- "eggs"
#' MeanAtAge(fec)     <- my_array
#' MeanAtLength(fec)  <- my_length_array
#' Classes(fec)       <- 0:20
#' ```
#'
#' @return
#' - `Fecundity()` returns a [fecundity-class] object. If `Pars` is a
#'   [stock-class], returns `x@Fecundity`.
#' - `Fecundity<-` returns the [stock-class] `x` with the `Fecundity` slot
#'   replaced and the object re-validated.
#'
#' @seealso
#' - [fecundity-class] for the class definition and slot-level documentation.
#' - [FecundityModels()] for available models and required parameter sets.
#' - [ValidUnits()] for accepted unit strings.
#' - `.FindModel()` for automatic model inference.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#' - [Length()] for the companion length schedule, required when using
#'   at-length fecundity models.
#' - [Weight()] and [Maturity()] for the schedules used in the default
#'   spawning biomass calculation.
#' - [SRR()] for spawning timing within the time step.
#'
#' @family fecundity
#'
#' @example man-examples/class-Fecundity.R
#'
#' @export
Fecundity <- function(Pars = list(),
                      Model = NULL,
                      Units = "eggs",
                      MeanAtAge = NULL,
                      MeanAtLength = NULL,
                      Classes = NULL,
                      Timing = NULL,
                      Misc = list()) {
  
  if (.IsStockOrList(Pars)) 
    return(.ExtractStockSlot(Pars, "Fecundity"))
  
  if (is.null(Pars))
    return(NULL)
  
  if (is.null(Pars))
    return(NULL)
  
  object <- methods::new(
    "fecundity",
    Pars         = Pars,
    Model        = Model,
    Units        = Units,
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    Classes      = Classes,
    Timing       = Timing,
    Misc         = Misc
  )
  
  if (length(Pars) > 0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars))) &&
      is.null(Model))
    object@Model <- .FindModel(object)
  
  methods::validObject(object)
  object
  
}

#' @rdname Fecundity
#' @export
`Fecundity<-` <- function(x, value) {
  .AssignSlotRecursive(x, value, 'Fecundity')
}
