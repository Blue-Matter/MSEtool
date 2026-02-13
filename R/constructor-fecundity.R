#' Fecundity
#'
#' Construct a [fecundity-class] object defining the length-at-age structure
#' associated with a [Stock()].
#'
#' @param Pars List of fecundity model parameters (see [FecundityModels()])..
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
#' A `Fecundity` object can be attached to a [Stock()] using `Fecundity(Stock) <- MyFecundity` and
#' retrieved using `MyFecundity <- Fecundity(Stock)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [Units()].
#' 
#' `r TechManLink()`
#' 
#'
#' @return A valid [fecundity-class] object.
#'
#' @seealso [Populate()], [FecundityModels()]
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
  
  
  if (inherits(Pars, 'stock'))
    return(Pars@Fecundity)
  
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

`Fecundity<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  AssignSlot(x, value, 'Fecundity')
}


