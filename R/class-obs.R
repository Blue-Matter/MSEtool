#' Observation Model
#'
#' The [obs-class] defines the observation model used to generate observed data
#' from the operating model, including observation error, bias, and sampling
#' structure for each data type.
#'
#' @slot Name Character. Name of the observation model. Or an [om-class] object,
#' to return the contents of the `Obs` slot. 
#' @slot LifeHistory An [lifehistoryobs-class] object. Observation error on
#'   life-history parameters (e.g., growth, maturity).
#' @slot Exploitation An [exploitationobs-class] object. Observation error on
#'   exploitation processes (e.g., fishing mortality, selectivity).
#' @slot Effort An [effortobs-class] object. Observation error on effort data.
#' @slot Landings An [catchobs-class] object. Observation error on landed
#'   catch.
#' @slot Discards An [catchobs-class] object. Observation error on discarded
#'   catch.
#' @slot CPUE An [indicesobs-class] object. Observation error on
#'   catch-per-unit-effort indices.
#' @slot Survey An [indicesobs-class] object. Observation error on fishery-
#'   independent survey indices.
#' @slot LandingsAtAge An [CompObs-class] object. Observation error on
#'   landed catch-at-age composition.
#' @slot DiscardsAtAge An [CompObs-class] object. Observation error on
#'   discarded catch-at-age composition.
#' @slot LandingsAtSize An [CompObs-class] object. Observation error on
#'   landed catch-at-length composition.
#' @slot DiscardsAtSize An [CompObs-class] object. Observation error on
#'   discarded catch-at-length composition.
#' @slot Misc List. Miscellaneous additional objects.
#'
#' @details
#' Each slot corresponds to a distinct data type and contains an observation
#' model sub-object defining the error structure (bias, CV, sample size, etc.)
#' for that data type. Sub-objects are defined in their respective class files.
#'
#' An [obs-class] object can be attached to an [om-class] object with
#' `Obs(om) <- MyObs` and retrieved with `Obs(om)`.
#'
#' @seealso [Obs()], [OM()], [ConvertObs()]
#' 
#' @include class-unions.R
#' @include class-obs-lifehistory.R
#' @include class-obs-exploitation.R
#' @include class-obs-effort.R
#' @include class-obs-catch.R
#' @include class-obs-indices.R
#' @include class-obs-comp.R
#' @name obs-class
#' @export
setClass(
  "obs",
  slots = c(
    Name           = "char.null",
    LifeHistory    = "lifehistoryobs",
    Exploitation   = "exploitationobs",
    Effort         = "effortobs",
    Landings       = "catchobs",
    Discards       = "catchobs",
    CPUE           = "indicesobs",
    Survey         = "indicesobs",
    LandingsAtAge  = "CompObs",
    DiscardsAtAge  = "CompObs",
    LandingsAtSize = "CompObs",
    DiscardsAtSize = "CompObs",
    Misc           = "list"
  )
)


#' @rdname obs-class
#' @param object An [om-class] object, or `NULL` (default) to create a new
#'   empty [obs-class] object.
#' @return
#' - `Obs()`: if `Name` is an [om-class] object, returns `Name@Obs`.
#'   Otherwise returns a new [obs-class] object.
#' - `Obs<-`: returns `x` with the `Obs` slot replaced.
#' @export
Obs <- function(Name = NULL,
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
  
  .Object <- methods::new("obs")
  
  if (!is.null(Name))           .Object@Name           <- Name
  if (!is.null(LifeHistory))    .Object@LifeHistory    <- LifeHistory
  if (!is.null(Exploitation))   .Object@Exploitation   <- Exploitation
  if (!is.null(Effort))         .Object@Effort         <- Effort
  if (!is.null(Landings))       .Object@Landings       <- Landings
  if (!is.null(Discards))       .Object@Discards       <- Discards
  if (!is.null(CPUE))           .Object@CPUE           <- CPUE
  if (!is.null(Survey))         .Object@Survey         <- Survey
  if (!is.null(LandingsAtAge))  .Object@LandingsAtAge  <- LandingsAtAge
  if (!is.null(DiscardsAtAge))  .Object@DiscardsAtAge  <- DiscardsAtAge
  if (!is.null(LandingsAtSize)) .Object@LandingsAtSize <- LandingsAtSize
  if (!is.null(DiscardsAtSize)) .Object@DiscardsAtSize <- DiscardsAtSize
  
  methods::validObject(.Object)
  .Object
}


setValidity("obs", function(object) {
  # TODO: structural checks
  TRUE
})


#' @rdname obs-class
#' @param x An [om-class] object.
#' @param value An [obs-class] object to assign.
#' @export
`Obs<-` <- function(x, value) {
  x@Obs <- value
  x
}
