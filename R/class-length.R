#' Length Class
#' 
#' The `Length` class defines the length-at-age schedule associated with a
#' [Stock()] object It stores the parameters and model used to generate
#' mean length-at-age, along with variability and assumptions regarding the
#' distribution of length-at-age.
#' @param Pars Named list of growth parameters for [LengthModels()]
#' @param Model Model associated with `Pars`
#' @param Units Length units
#' @param MeanAtAge Mean length-at-age array
#' @param CVatAge Coefficient of variation at age
#' @param Dist Distribution name
#' @param TruncSD Truncation in SD units
#' @param Timing Timing within time step
#' @param Random Random effects
#' @param ASK Age–length key
#' @param Classes Length classes
#' @param `Misc` Miscellaneous list
#' 
#' @include 00_Class_unions.R
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
                 ASK='array.null',
                 Classes='num.null',
                 Misc='list'
         )
)

setValidity("length", function(object) {
  # TODO
  TRUE
})
