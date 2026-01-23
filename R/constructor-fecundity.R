#' Fecundity
#'
#' Construct or access a [Fecundity()] object.
#'
#' @param Pars List of fecundity model parameters.
#' @param Model Character string or function identifying the fecundity model.
#' @param Units Character string giving fecundity units.
#' @param MeanAtAge Numeric array giving fecundity-at-age.
#' @param MeanAtLength Numeric array giving fecundity-at-length.
#' @param Classes Numeric vector of age or length classes.
#' @param Timing Numeric array giving timing of fecundity.
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `Fecundity()` constructor creates a [Fecundity()] object describing
#' reproductive output as a function of age or length.
#' 
#' Fecundity is optional. 
#'
#' Fecundity may be specified directly using `MeanAtAge` or `MeanAtLength`,
#' or indirectly using a parametric model defined by `Pars` and `Model`.
#'
#' A `fecundity` object can be attached to a [Stock()] using
#' [SetFecundity()] and retrieved using [GetFecundity()].
#' 
#'
#' @return A valid [fecundity()] object.
#'
#' @seealso
#' [GetFecundity()], [SetFecundity()], [FecundityModels()]
#'
#' @example man-examples/Fecundity-class.R
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
  
  object <- new(
    "fecundity",
    Pars = Pars,
    Model = Model,
    Units = Units,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    Classes = Classes,
    Timing = Timing,
    Misc = Misc
  )
  
  object
}

#' @rdname Fecundity
#' @export
GetFecundity <- function(object) {
  object@Fecundity
}

#' @rdname Fecundity
#' @export
SetFecundity <- function(object, value) {
  object@Fecundity <- value
  validObject(object)
  object
}


