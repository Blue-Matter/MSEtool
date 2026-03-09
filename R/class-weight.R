#' Weight Class
#' 
#' An S4 class representing the weight-at-age and -at-length structure for a [Stock()] object.
#' 
#' See [Weight()] for details.
#' 
#' @slot Pars Named list of parameters for [WeightModels()]
#' @slot Model Model associated with `Pars`
#' @slot Units Weight units
#' @slot MeanAtAge Mean weight-at-age array
#' @slot MeanAtLength Mean weight-at-length array
#' @slot CVatAge Coefficient of variation at age
#' @slot Dist Distribution name
#' @slot TruncSD Truncation in SD units
#' @slot Timing Timing within time step
#' @slot Random Random effects
#' @slot AWK Age–weight key
#' @slot Classes Weight classes
#' @slot Misc Miscellaneous list
#' @export
#' @include class-unions.R
#' @name weight-class
setClass("weight",
         slots=c(Pars='list',
                 Model='fun.char',
                 Units='char.null',
                 MeanAtAge='num.array.null',
                 MeanAtLength='num.array.null',
                 CVatAge='num.array.null',
                 Dist='character',
                 TruncSD='num.array.null',
                 Timing='num.array.null',
                 Random='num.array.null',
                 AWK='array.null',
                 Classes='num.null',
                 Misc='list'
         )
)

setValidity("weight", function(object) {
  
  # TODO - update for all legitimate cases
  
  TRUE
})


