#' Length Class
#' 
#' An S4 class representing the length-at-age structure for a [Stock()] object.
#' 
#' See [Length()] for details.
#' 
#' @slot Pars Named list of growth parameters for models in [LengthModels()]
#' @slot Model Model associated with `Pars`
#' @slot Units Length units
#' @slot MeanAtAge Mean length-at-age array
#' @slot CVatAge Coefficient of variation at age
#' @slot Dist Distribution name
#' @slot TruncSD Truncation in SD units
#' @slot Timing Timing within time step
#' @slot Random Random effects
#' @slot ALK Age–length key
#' @slot Classes Length classes
#' @slot Misc Miscellaneous list
#' 
#' @include class-unions.R
#' @name length-class
setClass("length",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null',
                 MeanAtAge='num.array.null',
                 CVatAge='num.array.null',
                 Dist='character',
                 TruncSD='num.array.null',
                 Timing='num.array.null',
                 Random='num.array.null',
                 ALK='array.null',
                 Classes='num.null',
                 Misc='list'
         )
)

setValidity("length", function(object) {
  # TODO
  TRUE
})
