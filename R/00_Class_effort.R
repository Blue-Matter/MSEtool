#' Effort Object
#'
#' Historical Fishing Effort
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' @slot Misc `r Misc_param()`
#'
#' @name Effort
#' @export
setClass("effort",
         slots=c(
           Effort='num.array.df',
           Units='char.null',
           Distribution='num.array'  
         ),
         contains='MiscClass'
)

setValidity('effort', isValidObject)


setMethod("initialize", "effort", function(.Object,
                                           Effort=NULL,
                                           Units='',
                                           Distribution=NULL,
                                           Misc=list()) {
  
  .Object@Effort <- Effort
  .Object@Units <- Units
  .Object@Distribution <- Distribution
  .Object@Misc <- Misc
  .Object
})



#' @rdname Effort
#' @export
Effort <- function(Effort=NULL,
                   Units='',
                   Distribution=NULL,
                   Misc=list()) {
  
  if (inherits(Effort,'fleet'))
    return(
      Effort@Effort
    )
  
  methods::new('effort',
               Effort=Effort,
               Units=Units,
               Distribution=Distribution,
               Misc=Misc)
  

}


#' @rdname Effort
#' @param x A [Fleet()] class object
#' @param value A `Effort` object to assign to `x`
#' @export
`Effort<-` <- function(x, value) {
  assignSlot(x, value, 'Effort')
}
