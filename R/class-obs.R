#' Observation Model
#'
#' The [obs-class] defines the observation model used to generate observed data
#' from the operating model, including observation error, bias, and sampling
#' structure for each data type.
#'
#' @slot Name Character. Name of the observation model.
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
#' @slot LandingsAtAge An [obs-comp-class] object. Observation error on
#'   landed catch-at-age composition.
#' @slot DiscardsAtAge An [obs-comp-class] object. Observation error on
#'   discarded catch-at-age composition.
#' @slot LandingsAtSize An [obs-comp-class] object. Observation error on
#'   landed catch-at-length composition.
#' @slot DiscardsAtSize An [obs-comp-class] object. Observation error on
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
    Name           = "character",
    LifeHistory    = "lifehistoryobs",
    Exploitation   = "exploitationobs",
    Effort         = "effortobs",
    Landings       = "catchobs",
    Discards       = "catchobs",
    CPUE           = "indicesobs",
    Survey         = "indicesobs",
    LandingsAtAge  = "obs-comp",
    DiscardsAtAge  = "obs-comp",
    LandingsAtSize = "obs-comp",
    DiscardsAtSize = "obs-comp",
    Misc           = "list"
  )
)


#' @rdname obs-class
#' @param object An [om-class] object, or `NULL` (default) to create a new
#'   empty [obs-class] object.
#' @return
#' - `Obs()`: if `object` is an [om-class] object, returns `object@Obs`.
#'   Otherwise returns a new empty [obs-class] object.
#' - `Obs<-`: returns `x` with the `Obs` slot replaced.
#' @export
Obs <- function(object = NULL) {
  if (inherits(object, "om"))
    return(object@Obs)
  
  .Object <- methods::new("obs")
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
