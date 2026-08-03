#' Management Advice Constructor and Accessors
#'
#' Construct an [advice-class] object defining management advice returned by a
#' management procedure.
#'
#' @param TAC Numeric vector or matrix specifying total allowable catch.
#'  See `Details`
#'
#' @param TACType Character. Does the TAC refer to `"Removals"` (default) or
#'   `"Landings"`. Either length 1 (applied to all fleets) or a character
#'   vector of length `nFleet`.
#'
#' @param TACUnit Character. Units in which the TAC is expressed: `"Biomass"`
#'   (default) or `"Number"`. Either length 1 (applied to all fleets) or a
#'   character vector of length `nFleet`.
#'    
#' @param Effort Numeric vector or matrix specifying relative or absolute 
#' fishing effort. See `Details`
#'
#' @param EffType Character. Effort interpretation: `"Rel"` for relative to
#'   the last historical year, or `"Abs"` for absolute effort units (in units
#'   of [Effort()]). Either length 1 (applied to all fleets) or a character
#'   vector of length `nFleet`. Default is `"Rel"`.
#'
#' @param Closure Numeric vector or array specifying spatial closures. See `Details`
#'
#' @param Selectivity A [Selectivity()] object or an `nFleet`-length list of such objects. See `Details`
#'
#' @param Retention A [Retention()] object or an `nFleet`-length list of such objects. See `Details`
#'
#' @param DiscardMortality A [DiscardMortality()] object or an `nFleet`-length list
#' of such objects. See `Details`
#'
#'
#' @param BagLimit Numeric vector or `NULL`. Bag limit in fish per angler per
#'   trip (when `LimitType = "angler"`) or fish per vessel per trip (when
#'   `LimitType = "boat"`), for this stock's catch by each fleet. Accepted
#'   forms:
#'   - `NULL` (default): no bag limit regulation is active for any fleet.
#'   - Single numeric value: the same limit applied to all fleets.
#'   - Numeric vector of length `nFleet`: fleet-specific bag limits. `NA`
#'     for a given fleet position means no limit applies to that fleet.
#'
#'   The bag limit regulation persists across time steps until a new
#'   [Advice()] object is returned by the MP. See Details.
#'
#'   An aggregate bag limit pooling catch across several stocks for one
#'   fleet is declared separately via `AggregateBagLimit()`, available to
#'   `mmp`-class management procedures. A stock's own `BagLimit` then acts
#'   as an optional species-specific sub-cap within that pooled limit for
#'   any stock included in the group.
#'
#' @param LimitType Character or `NULL`. Whether `BagLimit` is a per-angler
#'   or per-vessel regulation. Either length 1 (applied to all
#'   fleets) or a character vector of length `nFleet`. Valid values:
#'   - `"angler"` (default): limits are per angler per trip; the fleet-level
#'     retention cap is \eqn{B_f \cdot A_{f,t} \cdot T_{f,t}}, where
#'     \eqn{A_{f,t}} is mean anglers per trip ([AnglerPerTrip()]) and
#'     \eqn{T_{f,t}} is the number of trips ([TripsScalar()]).
#'   - `"boat"`: limits are per vessel per trip; the fleet-level retention cap
#'     is \eqn{B_f \cdot T_{f,t)}}, and [AnglerPerTrip()] is not used.
#'
#' @param ClosureMode Character or `NULL`. Determines how the OM handles catch
#'   that exceeds the bag limit. Either length 1 (applied to all fleets) or a
#'   character vector of length `nFleet`. Valid values:
#'   - `"discard"` (default): catch exceeding the bag limit is converted to
#'     discards; discard mortality is applied via the fleet's
#'     [DiscardMortality()] object.
#'   - `"stop"`: effort is reduced so that projected catch does not exceed the
#'     bag limit retention cap; no additional discards are generated beyond the
#'     baseline discard rate from non-retention of undersized fish.
#'
#' @param Misc Miscellaneous list. Will be passed to `Data@Misc` in following
#'   time steps.
#' 
#' @details
#'
#' `Advice()` constructs a container for management regulations returned by a
#' management procedure. All slots are optional; any regulation not supplied
#' will remain unchanged from the previous time step.
#' 
#' The management regulations in an `Advice` object will be applied in the 
#' year/time-step that the `Advice` object is produced (i.e., when the MP is run)
#' and will apply to all future time-steps until a new `Advice` object is 
#' returned by an MP.
#' 
#' The `Closure`, `Selectivity`, `Retention`, and `DiscardMortality` regulations
#' (if any) are applied first before calculating the fishing mortality corresponding
#' with `TAC`.
#' 
#' If both `TAC` and `Effort` regulations are returned, the expected 
#' fishing effort will be calculated to achieve a catch
#' equal to the `TAC`. If the expected effort is greater than that set in 
#' `Effort`, the catch will be constrained to that corresponding with `Effort`. 
#' Otherwise, the actual fishing effort will be lower than `Effort`. 
#' 
#' ## Valid Configurations for Management Regulations
#'
#' There are several options for populating the slots in an `Advice` object:
#'
#' ## TAC
#'
#' Total allowable catch in units of `"Biomass"` (default) or `"Number"`
#' (see `TACUnit`). `TACType` and `TACUnit` are each either length 1
#' (applied uniformly across all fleets) or a character vector of length
#' `nFleet` specifying per-fleet values.
#'
#'  * single numeric value: global TAC distributed across
#' the stocks and fleets in the [Data()] object according to `Allocation(OM)`.
#'
#'  * numeric vector length `nFleet`: fleet-specific TAC.
#'
#' ## Effort
#'
#' Either in absolute units corresponding with fleet-specific effort
#' (`EffType = "Abs"`), or relative to the effort in the last historical year
#' (`EffType = "Rel"`; default). `EffType` is either length 1 (applied
#' uniformly across all fleets) or a character vector of length `nFleet`
#' specifying per-fleet values.
#'
#' Note that when `Effort` is supplied as a matrix (fleet x area), it is
#' always treated as absolute regardless of `EffType`.
#'
#' * single numeric value: 
#' 
#'    * If `EffType`=`"Rel"`: Effort relative to last historical year, applied to all
#'    fleets. E.g., if `Effort=0.5`, effort for all fleets will be set to half 
#'    the effort in the last historical year. 
#'  
#'    * If `EffType`=`"Abs"`: the total effort (summed over fleets unless provide
#'    as a length `nFleet` vector); Note that in this case all fleets in the OM must 
#'    have the same units for `Effort`).
#'
#' * numeric vector length `nFleet`: Fleet-specific relative or absolute Effort. 
#'
#' * numeric matrix: Fleet- and Area-specific absolute Effort.
#' Must have `nFleet` rows and `nArea` columns.  
#' 
#' ## Closure
#' 
#' Can only be `0` (closed) or `1` (open). Otherwise will be converted to 
#' `0` (`Closure`<0.5) or `1`
#' 
#' * vector length `nArea`
#' 
#' * matrix with `nFleet` rows and `nArea` columns: fleet-specific closures.
#' 
#' ## Selectivity
#' 
#' Either a [Selectivity()] object, or a list length `nFleet` of
#' `Selectivity` objects. 
#' 
#' If a single `Selectivity` object, the prescribed selectivity schedule
#' is applied to all fleets.  
#' 
#' See section below for more details.
#'
#' ## Retention
#' 
#' Same as described for `Selectivity`, but for [Retention()] objects.
#' 
#' ## Discard Mortality
#' 
#' Same as described for `Selectivity`, but for [DiscardMortality()] objects.
#' 
#' ## Bag Limit
#'
#' `BagLimit`, `LimitType`, and `ClosureMode` together define a bag-limit
#' regulation for this stock's catch by each fleet: the number of fish an
#' angler (or vessel) may retain per trip. `NA` in a fleet position of
#' `BagLimit` means no limit applies for that fleet.
#'
#' When `ClosureMode = "discard"`, catch exceeding the retention cap is
#' converted to discards and discard mortality is applied via the fleet's
#' [DiscardMortality()] object. When `ClosureMode = "stop"`, effort is
#' reduced to prevent exceeding the bag limit and no additional discards are
#' generated.
#'
#' A bag limit pooling catch across several stocks for one fleet (rather
#' than a single stock's own catch) is declared separately via
#' `AggregateBagLimit()`, available to `mmp`-class management procedures. A
#' stock's own `BagLimit` may still be set for any stock included in such a
#' group, acting as a species-specific sub-cap within the pooled limit.
#'
#' ## Misc
#'
#' A list of miscellaneous information that needs to be stored and accessed by
#' the MP in future timesteps. Contents of `Advice@Misc` will be available in
#' the `Data@Misc` slot in subsequent time steps.
#' 
#' ## Populating Selectivity, Retention, and Discard Mortality Objects
#' 
#'  The `Selectivity`, `Retention`, and `DiscardMortality` objects are used to 
#'  set the selectivity-, retention-, and discard mortality- at-age or -at-size
#'  schedules. 
#'  
#'  If values are provided for these slots in the `Advice` object,
#'  the new regulations/specifications will apply to all future time steps
#'  until modified again by the `MP`.
#'  
#'  Values can be set for these objects in the following ways:
#'  
#'  * `MeanAtAge`: 
#'      * A numeric vector length `nAge`, where `nAge` is the number of 
#'      age classes for the stock (or stocks) that are being managed by the `MP`.
#'      
#'      * For area-specific schedules, a numeric matrix with `nAge` rows and
#'       `nArea` columns.
#'  
#'  * `MeanAtLength`: 
#'      * A numeric vector length `nClass`, where `nClass` is the number of 
#'      length classes for the stock (or stocks) that are being managed by the `MP`. 
#'      The `Classes` slot can be used to set the size classes corresponding with
#'      `MeanAtLength`. Otherwise, the classes will be taken 
#'      from the corresponding `Length` object in the `OM`(if they exist, otherwise
#'      an error).
#'            
#'      * For area-specific schedules, a numeric matrix with `nClass` rows and 
#'      `nArea` columns.
#'      
#'  * `MeanAtWeight`: 
#'      * A numeric vector length `nClass`, where `nClass` is the number of 
#'      weight classes for the stock (or stocks) that are being managed by the `MP`. 
#'      The `Classes` slot can be used to set the size classes corresponding with
#'      `MeanAtWeight`. Otherwise, the classes will be taken 
#'      from the corresponding `Weight` object in the `OM` (if they exist, otherwise
#'      an error).
#'            
#'      * For area-specific schedules, a numeric matrix with `nClass` rows and 
#'      `nArea` columns. 
#'      
#'   * `Pars` and `Model`: for `Selectivity` and `Retention` only.
#'   
#'   These slots can be used to set the age- or size-schedules using parameters and 
#'   a model. See [SelectivityModels()] and [RetentionModels()] for a details 
#'   of built-in models.
#'   
#'   `Pars` should be a named list of parameters, where each parameter can be either
#'   a single numeric value, or a numeric vector length `nArea` for area-specific schedules.
#'  
#' @param x An [advice-class] object, for the accessor and replacement
#'   functions below.
#' @param value The replacement value, for the replacement functions below.
#'
#' @return
#' - `Advice()` returns an [advice-class] object.
#' - `TAC()`, `TACType()`, `TACUnit()`, `Effort()`, `EffType()`, `Closure()`,
#'   `Selectivity()`, `Retention()`, `DiscardMortality()`,
#'   `BagLimit()`, `LimitType()`, `ClosureMode()`, `Misc()` return the
#'   corresponding slot from an [advice-class] object `x`.
#' - Their replacement forms (e.g. `TAC<-`) return `x` with the corresponding
#'   slot updated.
#'
#' @seealso
#' - [advice-class] for the class definition and slot-level documentation.
#' - [Selectivity()], [Retention()], [DiscardMortality()] for gear regulation
#'   sub-objects.
#' - [TripsScalar()], [AnglerPerTrip()] for the effort-to-trips and
#'   angler-scaling parameters consumed by the bag limit model.
#' - [Theta()] for the within-trip catch overdispersion parameter.
#'
#' @rdname Advice
#' @export
#' @example man-examples/class-Advice.R
Advice <- function(TAC              = NULL,
                   TACType          = 'Removals',
                   TACUnit          = 'Biomass',
                   Effort           = NULL,
                   EffType          = 'Rel',
                   Closure          = NULL,
                   Selectivity      = NULL,
                   Retention        = NULL,
                   DiscardMortality = NULL,
                   BagLimit         = NULL,
                   LimitType        = 'angler',
                   ClosureMode      = 'discard',
                   Misc             = list()) {
  
  match_arg_vec <- function(x, choices, arg_name) {
    bad <- !x %in% choices
    if (any(bad))
      cli::cli_abort(
        c("{.arg {arg_name}} must be one of {.val {choices}}.",
          x = "Invalid value{?s}: {.val {unique(x[bad])}}")
      )
    x
  }
  
  TACType     <- match_arg_vec(TACType,     c('Removals', 'Landings'),  'TACType')
  TACUnit     <- match_arg_vec(TACUnit,     c('Biomass',  'Number'),    'TACUnit')
  EffType     <- match_arg_vec(EffType,     c('Rel',      'Abs'),       'EffType')
  LimitType   <- match_arg_vec(LimitType,   c('angler',   'boat'),      'LimitType')
  ClosureMode <- match_arg_vec(ClosureMode, c('discard',  'stop'),      'ClosureMode')
  
  if (!is.null(TAC) && !is.numeric(TAC))
    cli::cli_abort("`TAC` must be numeric")
  
  if (!is.null(Effort) && !is.numeric(Effort))
    cli::cli_abort("`Effort` must be numeric")
  
  if (!is.null(Closure) && !is.numeric(Closure))
    cli::cli_abort("`Closure` must be numeric")
  
  if (!is.null(BagLimit) && !is.numeric(BagLimit))
    cli::cli_abort("`BagLimit` must be numeric")
  
  if (!is.null(BagLimit) && any(BagLimit < 0, na.rm = TRUE))
    cli::cli_abort("`BagLimit` must be non-negative")

  if (!is.null(Selectivity) && !is.list(Selectivity) &&
      !inherits(Selectivity, 'selectivity'))
    cli::cli_abort(
      "`Selectivity` must be a {.help MSEtool::Selectivity} object or a list of `Selectivity` objects"
    )
  
  if (!is.null(Retention) && !is.list(Retention) &&
      !inherits(Retention, 'retention'))
    cli::cli_abort(
      "`Retention` must be a {.help MSEtool::Retention} object or a list of `Retention` objects"
    )
  
  if (!is.null(DiscardMortality) && !is.list(DiscardMortality) &&
      !inherits(DiscardMortality, 'discardmortality'))
    cli::cli_abort(
      "`DiscardMortality` must be a {.help MSEtool::DiscardMortality} object or a list of `DiscardMortality` objects"
    )
  
  if (!is.null(Misc) && !is.list(Misc))
    cli::cli_abort("`Misc` must be a list")
  
  methods::new(
    "advice",
    TAC              = TAC,
    TACType          = TACType,
    TACUnit          = TACUnit,
    Effort           = Effort,
    EffType          = EffType,
    Closure          = Closure,
    Selectivity      = Selectivity,
    Retention        = Retention,
    DiscardMortality = DiscardMortality,
    BagLimit         = BagLimit,
    LimitType        = LimitType,
    ClosureMode      = ClosureMode,
    Misc             = Misc,
    Log              = list()
  )
}

#' @rdname Advice
#' @export
`TAC<-` <- function(x, value) {
  .AssignSlot(x, value, 'TAC')
}

#' @rdname Advice
#' @export
TACType <- function(x) {
  .AccessSlot(x, 'TACType')
}

#' @rdname Advice
#' @export
`TACType<-` <- function(x, value) {
  .AssignSlot(x, value, 'TACType')
}

#' @rdname Advice
#' @export
TACUnit <- function(x) {
  .AccessSlot(x, 'TACUnit')
}

#' @rdname Advice
#' @export
`TACUnit<-` <- function(x, value) {
  .AssignSlot(x, value, 'TACUnit')
}

#' @rdname Advice
#' @export
EffType <- function(x) {
  .AccessSlot(x, 'EffType')
}

#' @rdname Advice
#' @export
`EffType<-` <- function(x, value) {
  .AssignSlot(x, value, 'EffType')
}

#' @rdname Advice
#' @export
BagLimit <- function(x) {
  .AccessSlot(x, 'BagLimit')
}

#' @rdname Advice
#' @export
`BagLimit<-` <- function(x, value) {
  .AssignSlot(x, value, 'BagLimit')
}

#' @rdname Advice
#' @export
LimitType <- function(x) {
  .AccessSlot(x, 'LimitType')
}

#' @rdname Advice
#' @export
`LimitType<-` <- function(x, value) {
  .AssignSlot(x, value, 'LimitType')
}

#' @rdname Advice
#' @export
ClosureMode <- function(x) {
  .AccessSlot(x, 'ClosureMode')
}

#' @rdname Advice
#' @export
`ClosureMode<-` <- function(x, value) {
  .AssignSlot(x, value, 'ClosureMode')
}
