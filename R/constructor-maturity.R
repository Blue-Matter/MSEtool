#' Maturity Constructor and Accessors
#'
#' Construct a [maturity-class] object defining the maturity schedule for a
#' [stock-class], or access and replace the `Maturity` slot of a [stock-class]
#' and its individual slots. A `Maturity` object is required for all
#' [stock-class] objects.
#'
#' @param Pars `list`. Named list of maturity parameters. Element names must
#'   match the arguments of a built-in maturity model (see [MaturityModels()]).
#'   When `Pars` is non-empty and `Model` is `NULL`, the model is inferred
#'   automatically by [FindModel()]. When `Pars` is a [stock-class] object,
#'   `Maturity()` acts as a pass-through accessor and returns `x@Maturity`;
#'   when `Pars` is a list of [stock-class] objects, `Model` is treated as an
#'   integer index selecting which stock's `Maturity` slot to return — see
#'   *Pass-Through Access*. See also
#'   [Specifying Biological and Fleet Schedules][populating-schedules] for the
#'   full set of accepted input formats. Default `list()`.
#' @param Model `character(1)`, `function`, or `integer(1)`. Maturity model
#'   identifier. When `NULL` (default), the model is inferred from `Pars` via
#'   [FindModel()]. May be set to a character string naming a built-in model or
#'   to a custom R function. When `Pars` is a list of [stock-class] objects,
#'   `Model` is an integer index — see *Pass-Through Access* and
#'   [Specifying Biological and Fleet Schedules][populating-schedules].
#' @param MeanAtAge `array` or `NULL`. Mean maturity-at-age with named
#'   dimensions `Sim × Age × Year`. Values should range from 0 (immature) to 1
#'   (fully mature). Supply directly when bypassing the model-based approach
#'   (`Pars = list()`). Only the years at which values *change* need to be
#'   included; [Extend()] fills the remainder. When `Pars` contains a matched
#'   model, any values here are **overwritten** during [Populate()]. A numeric
#'   vector of length `nAge` is also accepted and promoted to a `1 × nAge × 1`
#'   array automatically. Default `NULL`.
#' @param MeanAtLength `array` or `NULL`. Mean maturity-at-length with named
#'   dimensions `Sim × Length × Year`. Populated automatically during
#'   [Populate()] when an at-length maturity model is used; may also be
#'   supplied directly. When `MeanAtLength` is populated and `MeanAtAge` is
#'   not, [Populate()] converts it to `MeanAtAge` via the age-length key
#'   (`ALK`) from a [length-class] object. Default `NULL`.
#' @param MeanAtWeight `array` or `NULL`. Mean maturity-at-weight with named
#'   dimensions `Sim × Weight × Year`. Populated automatically during
#'   [Populate()] when an at-weight maturity model is used; may also be
#'   supplied directly. When `MeanAtWeight` is populated and `MeanAtAge` is
#'   not, [Populate()] converts it to `MeanAtAge` via the age-weight key
#'   (`AWK`) from a [weight-class] object. Requires a populated [weight-class]
#'   with a non-`NULL` `CVatAge` slot so that the `AWK` exists. Default `NULL`.
#' @param Classes `numeric` or `NULL`. Age, length, or weight class midpoints
#'   corresponding to the `MeanAt*` array in use. Default `NULL`.
#' @param Semelparous `logical(1)` or `array`. Controls post-spawning
#'   mortality. `FALSE` (default) disables post-spawn mortality. `TRUE` sets
#'   post-spawn mortality equal to the maturity ogive after [Populate()] — see
#'   *Semelparity* for full details. A custom `Sim × Age × Year` array may
#'   also be supplied directly to specify age- and time-varying post-spawn
#'   mortality that differs from the maturity schedule.
#' @param Misc `list`. Named list of optional user-defined inputs stored on the
#'   object and passed through to custom model functions. Default `list()`.
#' @param x A [maturity-class] object for slot accessors, or a [stock-class]
#'   object for `Maturity<-`.
#' @param value For `Maturity<-`: a [maturity-class] object. For
#'   `Semelparous<-`: a `logical(1)` or a `Sim × Age × Year` array.
#'
#' @details
#' ## Specifying Maturity
#'
#' There are two ways to define the maturity schedule; see
#' [Specifying Biological and Fleet Schedules][populating-schedules] for full
#' details on input formats and the rules that govern how `Pars`, `Model`, and
#' `MeanAt*` arrays interact.
#'
#' **Model-based** (recommended): supply `Pars` with named parameters matching
#' a built-in model (see [MaturityModels()]). If `Model = NULL` and the
#' parameter names uniquely match a model, [FindModel()] resolves the model
#' automatically. The relevant `MeanAt*` array is then populated by
#' [Populate()] when the stock is added to an [OM()]:
#'
#' ```r
#' # Logistic length-based ogive — model inferred from Pars names
#' mat <- Maturity(Pars = list(L50 = 40, L50_95 = 8))
#'
#' # Stochastic L50 across simulations
#' mat <- Maturity(Pars = list(L50 = c(35, 45), L50_95 = 8))
#'
#' # Time-varying L50 (shifts in 2010; Extend() fills the rest)
#' L50_arr <- array(c(40, 45), dim = c(1, 2),
#'                  dimnames = list(Sim = 1, Year = c(1990, 2010)))
#' mat <- Maturity(Pars = list(L50 = L50_arr, L50_95 = 8))
#'
#' # Inter-annual random walk on L50 (log-normal, SD = 0.05)
#' mat <- Maturity(Pars = list(L50 = c(35, 45), L50SD = 0.05, L50_95 = 8))
#' ```
#'
#' **Direct array**: supply any of `MeanAtAge`, `MeanAtLength`, or
#' `MeanAtWeight` with `Pars = list()`. `MeanAtAge` takes precedence if more
#' than one is populated:
#'
#' ```r
#' ages   <- 0:20
#' mat_aa <- Maturity(MeanAtAge = 1 / (1 + exp(-0.5 * (ages - 5))))
#'
#' lens   <- seq(5, 120, by = 5)
#' mat_al <- Maturity(
#'   MeanAtLength = array(
#'     1 / (1 + exp(-log(19) * (lens - 40) / 8)),
#'     dim      = c(1, length(lens), 1),
#'     dimnames = list(Sim = 1, Length = lens, Year = 1990)
#'   )
#' )
#' ```
#'
#' ## Semelparity
#'
#' Post-spawning mortality is controlled by the `Semelparous` slot, which
#' after [Populate()] is always a `Sim × Age × Year` array. Each cell gives
#' the fraction of individuals at that age dying immediately after spawning.
#' This mortality is applied as a multiplier `(1 - Semelparous)` on top of
#' standard `exp(-Z)` survival — it is an *additional* source of mortality,
#' not a substitute for natural mortality.
#'
#' The default (`Semelparous = FALSE`) sets all cells to 0: no post-spawn
#' mortality, appropriate for iteroparous species.
#'
#' Setting `Semelparous = TRUE` sets the array equal to `MeanAtAge` after
#' population:
#'
#' - Fully mature ages (maturity ≈ 1): post-spawn survival ≈ 0 — complete
#'   die-off.
#' - Ages on the maturity ogive (0 < maturity < 1): proportional post-spawn
#'   mortality.
#' - Immature ages (maturity ≈ 0): post-spawn survival ≈ 1 — unaffected.
#'
#' This assumption — that the post-spawn mortality schedule equals the maturity
#' ogive — is appropriate for obligate semelparous species such as Pacific
#' salmon, where all maturing individuals spawn once and die. For species with
#' partial semelparity, or where post-spawn mortality affects only older or
#' larger mature fish, supply a custom `Sim × Age × Year` array directly:
#'
#' ```r
#' ages <- 0:20
#' # Post-spawn mortality applies only to ages >= 5, ramping from 0 to 0.8
#' sem_arr <- array(
#'   pmin(pmax((ages - 5) / 10, 0), 0.8),
#'   dim      = c(1, length(ages), 1),
#'   dimnames = list(Sim = 1, Age = ages, Year = 1990)
#' )
#' mat <- Maturity(Pars = list(L50 = 40, L50_95 = 8), Semelparous = sem_arr)
#' ```
#'
#' Note: after [Populate()], `Semelparous` is always an array. 
#' 
#' ## At-Length and At-Weight Models
#'
#' Models whose class contains `"at-Length"` produce `MeanAtLength`, which is
#' converted to `MeanAtAge` via the `ALK` — a populated [Length()] object must
#' be passed to [Populate()]. Models whose class contains `"at-Weight"` produce
#' `MeanAtWeight`, converted via the `AWK` — a populated [Weight()] object
#' with non-`NULL` `CVatAge` (so the `AWK` exists) must be passed to
#' [Populate()]. At-weight maturity models are rare in practice.
#'
#' The same conversions apply when `MeanAtLength` or `MeanAtWeight` are
#' supplied directly by the user.
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Pars` is a [stock-class] object, `Maturity()` returns the `Maturity`
#' slot directly:
#'
#' ```r
#' Maturity(my_stock)             # returns my_stock@Maturity
#' Maturity(my_stock) <- my_mat   # replaces my_stock@Maturity
#' ```
#'
#' When `Pars` is a list of [stock-class] objects, `Model` is an integer index
#' selecting which stock's slot to return:
#'
#' ```r
#' Maturity(stock_list, 2)   # returns stock_list[[2]]@Maturity
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' Pars(mat)            <- list(L50 = 40, L50_95 = 8)
#' Model(mat)           <- "LogisticLength"
#' MeanAtAge(mat)       <- my_array
#' MeanAtLength(mat)    <- my_length_array
#' MeanAtWeight(mat)    <- my_weight_array
#' Semelparous(mat)     <- TRUE
#' Classes(mat)         <- seq(0, 80, by = 5)
#' ```
#'
#' @return
#' - `Maturity()` returns a [maturity-class] object. If `Pars` is a
#'   [stock-class], returns `x@Maturity`.
#' - `Maturity<-` returns the [stock-class] `x` with the `Maturity` slot
#'   replaced
#' - `Semelparous()` returns the `Semelparous` slot from `x` (a scalar before
#'   [Populate()], a `Sim × Age × Year` array after).
#' - `Semelparous<-` returns `x` with the `Semelparous` slot updated and the
#'   object re-validated.
#'
#' @seealso
#' - [maturity-class] for the class definition and slot-level documentation.
#' - [MaturityModels()] for available maturity models and required parameter
#'   sets.
#' - [FindModel()] for automatic model inference.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#' - [Length()] for the companion length schedule, required when using
#'   at-length maturity models.
#' - [Weight()] for the companion weight schedule, required when using
#'   at-weight maturity models.
#'
#' @family maturity
#'
#' @example man-examples/class-Maturity.R
#'
#' @export
Maturity <- function(Pars = list(),
                     Model = NULL,
                     MeanAtAge = NULL,
                     MeanAtLength = NULL,
                     MeanAtWeight = NULL,
                     Classes = NULL,
                     Semelparous = FALSE,
                     Misc = list()) {
  
  if (isStockOrList(Pars)) 
    return(ExtractStockSlot(Pars, "Maturity"))
  
  if (is.null(Pars))
    return(NULL)
  
  object <- methods::new(
    "maturity",
    Pars         = Pars,
    Model        = Model,
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes      = Classes,
    Semelparous  = Semelparous,
    Misc         = Misc
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



#' @rdname Maturity
#' @export
Semelparous <- function(x) {
  CheckClass(x, "maturity", "x")
  x@Semelparous
}

#' @rdname Maturity
#' @export
`Semelparous<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@Semelparous <- value
  methods::validObject(x)
  x
}

#' @rdname Maturity
#' @export
`Maturity<-` <- function(x, value) {
  AssignSlotRecursive(x, value, 'Maturity')
}
