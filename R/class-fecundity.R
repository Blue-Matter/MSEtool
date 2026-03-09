#' Fecundity Class
#'
#' An S4 class defining fecundity-at-age or fecundity-at-length relationships
#' associated with a [Stock()] object.
#'
#' @slot Pars List of model parameters defining fecundity.
#' @slot Model Character string or function identifying the fecundity model.
#' @slot Units Character string giving units of fecundity (e.g. "eggs").
#' @slot MeanAtAge Numeric array giving mean fecundity-at-age.
#' @slot MeanAtLength Numeric array giving mean fecundity-at-length.
#' @slot Classes Numeric vector giving age or length classes.
#' @slot Timing Numeric array giving timing of fecundity within the year.
#' @slot Misc Miscellaneous list 
#'
#' @include class-unions.R
#' 
#' @details
#' See [Fecundity()] for details.
#' 
#' @name fecundity-class
setClass(
  "fecundity",
  slots = c(
    Pars = "list",
    Model = "fun.char",
    Units = "char.null",
    MeanAtAge = "num.array.null",
    MeanAtLength = "num.array.null",
    Classes = "num.null",
    Timing = "num.array.null",
    Misc = "list"
  )
)


setValidity("fecundity", function(object) {
  TRUE
})
