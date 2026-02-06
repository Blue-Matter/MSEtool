#' Management Advice 
#'
#' Construct an [advice-class] object defining management advice returned by a
#' management procedure.
#'
#' @param TAC Numeric vector or matrix specifying total allowable catch. See `Details`
#'
#' @param Effort Numeric vector or matrix specifying relative or absolute fishing effort. See `Details`
#'
#' @param EffType Character string specifying effort interpretation:
#' `"Rel"` for relative to the last historical year, or `"Abs"` for absolute
#' effort units (in units of [Effort()]). Default is `"Rel"`.
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
#' @param ApicalF Numeric array specifying target apical fishing mortality. Not currently used
#'
#' @param Misc Miscellaneous list.
#' 
#' @details
#'
#' `Advice()` constructs a container for management regulations returned by a
#' management procedure. All slots are optional; any regulation not supplied
#' will remain unchanged from the previous time step.
#' 
#' The management regulations in an `Advice` object will be applied in the year/time-step
#' that the `Advice` object is produced (i.e., when the MP is run) and will 
#' apply to all future time-steps until a new `Advice` object is returned by an
#' MP.
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
#' Total allowable catch units of `Biomass`.
#'
#'  * single numeric value: global TAC distributed across
#' the stocks and fleets in the [Data()] object according to `Allocation(OM)`.
#'
#'  * numeric vector length `nFleet`: fleet-specific TAC.
#'
#'  * numeric matrix: Fleet- and Area-specific TAC. Must have `nFleet`
#' rows and `nArea` columns.
#'    
#' ## Effort
#' 
#' Either in absolute units corresponding with fleet-specific effort
#' (`EffType`=`"Abs"`), or relative to the effort in the last historical year
#' (`EffType`=`"Rel"`; default).
#'
#' Unless `Effort` is specified by `Area`, the spatial distribution of fishing
#' effort is calculated internally (see [Technical
#' Manual](https://docs.openmse.com/)) 
#' 
#' * single numeric value: 
#' 
#'    * If `EffType`=`"Rel"`: Effort relative to last historical year, applied to all
#'    fleets. E.g., if `Effort=0.5`, effort for all fleets will be set to half 
#'    the effort in the last historical year. 
#'  
#'    * If `EffType`=`"Abs"`: the total effort (summed over fleets; Note all
#'    fleets in the OM must have the same units for `Effort`) and 
#'    distributed over fleets following the same distribution as the last 
#'    historical year   
#'
#' * numeric vector length `nFleet`: Fleet-specific relative or absolute Effort. 
#'
#' * numeric matrix: Fleet- and Area-specific relative or absolute Effort. Must have `nFleet`
#' rows and `nArea` columns.  
#' 
#'   If `Effort` is a matrix, it will be treated as absolute; i.e., `EffType`  
#'   will be ignored.
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
#' ## ApicalF
#' 
#' Not currently used. 
#' 
#' ## Misc
#' 
#' A list of miscellaneous information that needs to be stored and accessed by 
#' the MP in future timesteps. 
#' 
# 'Contents of Advice@Misc will be available in the Data@Misc slot in subsequent time steps
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
#'
#' @return An [advice-class] object.
#'
#' @seealso [Selectivity()], [Retention()], [DiscardMortality()]
#'
#' @rdname Advice
#' @export
#' 
#' @examples
#' Advice(TAC = 1000)
#'
#' Advice(
#'   Effort = c(1, 0.8),
#'   EffType = "Rel"
#' )
Advice <- function(TAC = NULL,
                   Effort = NULL,
                   EffType = c('Rel', 'Abs'),
                   Closure = NULL,
                   Selectivity = NULL,
                   Retention = NULL,
                   DiscardMortality = NULL,
                   ApicalF = NULL,
                   Misc = list()) {
  
  EffType <- match.arg(EffType, c('Rel', 'Abs'))
  
  
  if (!is.null(TAC) && !is.numeric(TAC))
    cli::cli_abort("`TAC` must be numeric")
  
  if (!is.null(Effort) && !is.numeric(Effort))
    cli::cli_abort("`Effort` must be numeric")
  
  if (!is.null(Closure) && !is.numeric(Closure))
    cli::cli_abort("`Closure` must be numeric")
  
  if (!is.null(Selectivity) && !is.list(Selectivity) &&
      !inherits(Selectivity, 'selectivity'))
    cli::cli_abort("`Selectivity` must be {.help MSEtool::Selectivity} object or a list of `Selectivity` objects ")
  
  if (!is.null(Retention) && !is.list(Retention) &&
      !inherits(Retention, 'retention'))
    cli::cli_abort("`Retention` must be {.help MSEtool::Retention} object or a list of `Retention` objects ")
  
  if (!is.null(DiscardMortality) && !is.list(DiscardMortality) &&
      !inherits(DiscardMortality, 'DiscardMortality'))
    cli::cli_abort("`DiscardMortality` must be {.help MSEtool::DiscardMortality} object or a list of `DiscardMortality` objects ")
  
  if (!is.null(Misc) && !is.list(Misc))
    cli::cli_abort("`Misc` must be a list")
  
  
  
  methods::new("advice",
               TAC = TAC,
               Effort = Effort,
               EffType = EffType,
               Closure = Closure,
               Selectivity = Selectivity,
               Retention = Retention,
               DiscardMortality = DiscardMortality,
               ApicalF = ApicalF,
               Misc = Misc,
               Log = list())
}


