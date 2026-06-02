#' Obs Constructor and Accessor
#'
#' Construct an [obs-class] object defining the observation error structure for
#' each data type in the operating model, or extract the `Obs` slot from an
#' enclosing object.
#'
#' @param Name `character(1)` or an S4 object. Unique identifier for this
#'   observation model. Default `NULL`.
#'
#'   If `Name` is an [om-class] object, `Obs()` returns `Name@Obs` (the
#'   top-level observation model list) rather than constructing a new object.
#'   If `Name` is a [hist-class] or [mse-class] object, `Obs()` returns
#'   the `Obs` slot of the embedded OM (`Name@OM@Obs`). See *Pass-Through
#'   Access* in Details.
#'   
#' @param LifeHistory A [lifehistoryobs-class] object, or `NULL` (default).
#'   When `NULL`, an empty [lifehistoryobs-class] is created via
#'   [LifeHistoryObs()]. Currently a placeholder; see [LifeHistoryObs()].
#'   
#' @param Exploitation An [exploitationobs-class] object, or `NULL` (default).
#'   When `NULL`, an empty [exploitationobs-class] is created via
#'   [ExploitationObs()]. Currently a placeholder; see [ExploitationObs()].
#'   
#' @param Effort An [effortobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [effortobs-class] is created via [EffortObs()].
#'   
#' @param Landings A [catchobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [catchobs-class] is created via [CatchObs()]. Defines
#'   observation error for landed catch.
#'   
#' @param Discards A [catchobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [catchobs-class] is created via [CatchObs()]. Defines
#'   observation error for discarded catch.
#'   
#' @param CPUE An [indicesobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [indicesobs-class] is created via [IndicesObs()].
#'   Defines observation error for CPUE indices.
#'   
#' @param Survey An [indicesobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [indicesobs-class] is created via [IndicesObs()].
#'   Defines observation error for fishery-independent survey indices.
#'   
#' @param LandingsAtAge A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for landed catch-at-age composition.
#'   
#' @param DiscardsAtAge A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for discarded catch-at-age composition.
#'   
#' @param LandingsAtSize A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for landed catch-at-length composition.
#'   
#' @param DiscardsAtSize A [compobs-class] object, or `NULL` (default). When
#'   `NULL`, an empty [compobs-class] is created via [CompObs()]. Defines
#'   observation error for discarded catch-at-length composition.
#'   
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' ## Sub-Object Initialisation
#'
#' All sub-object slots are initialised to empty objects of the appropriate
#' class when not supplied. Empty sub-objects cause the model to skip
#' observation error for that data type (e.g., an empty `Landings` slot means
#' catch is reported without observation error). To activate observation error
#' for a data type, supply a sub-object with at least `CV` specified.
#'
#' ## Pass-Through Access
#'
#' When `Name` is an [om-class], [hist-class], or [mse-class] object, `Obs()`
#' extracts the `Obs` slot of the embedded OM rather than constructing a new
#' object:
#'
#' - `Obs(om)` → `om@Obs`
#' - `Obs(hist)` → `hist@OM@Obs`
#' - `Obs(mse)` → `mse@OM@Obs`
#'
#' The result in all cases is the two-level named list
#' `[[stock_complex]][[fleet_name]]` of [obs-class] objects stored in the OM.
#'
#' ## Attaching to an OM
#'
#' A single [obs-class] object can be assigned to a specific stock complex and
#' fleet slot with:
#' ```r
#' Obs(om) <- MyObs           # replaces the full Obs list
#' ```
#' Individual sub-objects are more commonly set directly on the `obs` object
#' before it is inserted into the OM.
#'
#' @return
#' - `Obs()` returns an [obs-class] object. When `Name` is an [om-class],
#'   [hist-class], or [mse-class] object, returns the `Obs` slot of the
#'   embedded OM (a two-level named list of [obs-class] objects).
#' - `Obs<-` returns `x` with the `Obs` slot replaced by `value`.
#'
#' @seealso
#' - [obs-class] for the class definition and slot-level documentation.
#' - [CatchObs()], [EffortObs()], [IndicesObs()], [CompObs()] for
#'   sub-object constructors.
#' - [LifeHistoryObs()], [ExploitationObs()] for placeholder sub-objects.
#' - [data-class] and [Data()] for the complementary observed-values object.
#' - [OM()] for the operating model constructor.
#' - [ConvertObs()] for converting legacy [Obs-legacy-class] objects.
#'
#' @family obs
#'
#' @examples
#' # Empty obs object
#' obs <- Obs()
#' obs
#'
#' # Obs with catch observation error specified
#' obs <- Obs(
#'   Name     = "MyObs",
#'   Landings = CatchObs(CV = 0.2, Bias = 1.0),
#'   CPUE     = IndicesObs(CV = 0.3)
#' )
#'
#' # Pass-through: extract Obs from an OM
#' # Obs(my_om)
#' 
#' 
#' @name Obs
#' @rdname Obs
#' 
#' @export
Obs <- function(Name           = NULL,
                LifeHistory    = NULL,
                Exploitation   = NULL,
                Effort         = NULL,
                Landings       = NULL,
                Discards       = NULL,
                CPUE           = NULL,
                Survey         = NULL,
                LandingsAtAge  = NULL,
                DiscardsAtAge  = NULL,
                LandingsAtSize = NULL,
                DiscardsAtSize = NULL,
                Misc           = list()) {
  
  if (inherits(Name, "om"))
    return(Name@Obs)
  
  if (inherits(Name, c("hist", "mse")))
    return(Name@OM@Obs)
  
  if (is.null(LifeHistory))    LifeHistory    <- new("lifehistoryobs")
  if (is.null(Exploitation))   Exploitation   <- new("exploitationobs")
  if (is.null(Effort))         Effort         <- new("effortobs")
  if (is.null(Landings))       Landings       <- new("catchobs")
  if (is.null(Discards))       Discards       <- new("catchobs")
  if (is.null(CPUE))           CPUE           <- new("indicesobs")
  if (is.null(Survey))         Survey         <- new("indicesobs")
  if (is.null(LandingsAtAge))  LandingsAtAge  <- new("compobs")
  if (is.null(DiscardsAtAge))  DiscardsAtAge  <- new("compobs")
  if (is.null(LandingsAtSize)) LandingsAtSize <- new("compobs")
  if (is.null(DiscardsAtSize)) DiscardsAtSize <- new("compobs")
  
  .Object <- methods::new("obs")
  
  if (!is.null(Name))      
    .Object@Name <- Name
  
  .Object@LifeHistory    <- LifeHistory
  .Object@Exploitation   <- Exploitation
  .Object@Effort         <- Effort
  .Object@Landings       <- Landings
  .Object@Discards       <- Discards
  .Object@CPUE           <- CPUE
  .Object@Survey         <- Survey
  .Object@LandingsAtAge  <- LandingsAtAge
  .Object@DiscardsAtAge  <- DiscardsAtAge
  .Object@LandingsAtSize <- LandingsAtSize
  .Object@DiscardsAtSize <- DiscardsAtSize
  .Object@Misc           <- Misc
  
  methods::validObject(.Object)
  .Object
}

#' @rdname Obs
#' @param x An [om-class] object.
#' @param value A two-level named list of [obs-class] objects, or a single
#'   [obs-class] object, to assign to the `Obs` slot.
#' @export
`Obs<-` <- function(x, value) {
  CheckClass(x, "om", "x")
  
  OM <- x
  Obs <- value
  
  Complexes    <- Complexes(OM)
  nComplex     <- length(Complexes)
  ComplexNames <- names(Complexes)
  
  if (is.null(ComplexNames) || nComplex < 1) {
    Complexes    <- MakeNamedList(StockNames(OM))
    for (i in seq_along(Complexes))
      Complexes[[i]] <- i
    ComplexNames <- StockNames(OM)
    nComplex     <- length(Complexes)
  }
  
  if (is.null(ComplexNames) || nComplex < 1)
    cli::cli_abort("Add `Stock` object(s) to `OM` first")
  
  FleetNames <- FleetNames(OM)
  nFleet     <- length(FleetNames)
  
  # validate and name a flat list of obs objects.
  check_and_name_obs <- function(obs_list) {
    cls <- purrr::map_chr(obs_list, class)
    if (any(cls != "obs"))
      cli::cli_abort(c(
        'x' = 'All elements of `value` must be a {.help MSEtool::Obs} object',
        'i' = 'Current classes of `value` are: {.val {cls}}'
      ))
    
    if (length(obs_list) < nFleet)
      cli::cli_abort(c(
        'x' = 'Each complex must have at least one `Obs` object per fleet',
        'i' = 'Expected {.val {nFleet}} fleet{?s}, got {.val {length(obs_list)}}'
      ))
    
    # Name the fleet elements positionally
    names(obs_list)[seq_len(nFleet)] <- FleetNames
    
    # Validate survey elements (beyond nFleet)
    nSurvey <- length(obs_list) - nFleet
    if (nSurvey > 0) {
      survey_names <- names(obs_list)[seq(nFleet + 1, length(obs_list))]
      
      if (any(is.null(survey_names)) || any(nchar(survey_names) == 0))
        cli::cli_abort(c(
          'x' = 'Survey `Obs` objects (beyond fleet elements) must be explicitly named',
          'i' = 'Provide unique names for elements {nFleet + 1} to {length(obs_list)}'
        ))
      
      all_names <- names(obs_list)
      if (anyDuplicated(all_names))
        cli::cli_abort(c(
          'x' = 'All names in `value` must be unique across fleets and surveys',
          'i' = 'Duplicated name{?s}: {.val {all_names[duplicated(all_names)]}}'
        ))
    }
    
    obs_list
  }
  
  # Case 1: single obs object — replicate across all complexes and fleets
  if (inherits(Obs, "obs")) {
    OM@Obs <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, Obs))
    return(OM)
  }
  
  if (inherits(Obs, "list")) {
    is_nested <- purrr::every(Obs, is.list)
    
    # Case 2: nested list [complex][fleet + surveys]
    if (is_nested) {
      if (length(Obs) != nComplex)
        cli::cli_abort(c(
          'x' = 'Nested `value` must have one element per complex',
          'i' = 'Expected {.val {nComplex}} complex{?es}, got {.val {length(Obs)}}'
        ))
      
      Obs <- purrr::map(Obs, check_and_name_obs)
      names(Obs) <- ComplexNames
      OM@Obs <- Obs
      return(OM)
    }
    
    # Case 3: flat list of obs objects — replicate across all complexes
    named_obs <- check_and_name_obs(Obs)
    OM@Obs <- MakeNamedList(ComplexNames, named_obs)
    return(OM)
  }
  
  AssignSlot(OM, Obs, 'Obs')
}