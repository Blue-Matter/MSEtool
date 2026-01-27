#' Weight Class
#' 
#' The `Weight` class defines the weight-at-age and weight-at-length 
#' schedules  associated with a [Stock()] object. It stores the parameters and model used to generate mean weight-at-age, along with variability and assumptions regarding the distribution of weight-at-age.
#' @param Pars Named list of parameters for [WeightModels()]
#' @param Model Model associated with `Pars`
#' @param Units Weight units
#' @param MeanAtAge Mean weight-at-age array
#' @param MeanAtLength Mean weight-at-length array
#' @param CVatAge Coefficient of variation at age
#' @param Dist Distribution name
#' @param TruncSD Truncation in SD units
#' @param Timing Timing within time step
#' @param Random Random effects
#' @param AWK Age–weight key
#' @param Classes Weight classes
#' @param `Misc` Miscellaneous list
#' @export
#' @include class-unions.R
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


