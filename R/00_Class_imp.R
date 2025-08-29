setClass('impslot',
         slots=c(
           Mean='num.array.list', 
           SD='num.array.list', 
           Compliance='num.array.list',
           Error='num.array.list'),
         contains='MiscClass'
)


#' Imp Object
#' @include 00_Class_unions.R
#' 
#' @name ImpClass
#'
#' @export
setClass('imp',
         slots=c(Name='character',
                 TAC='impslot',
                 Effort='impslot',
                 Size='impslot',
                 Misc='MiscClass' 
         ))

#' @describeIn ImpClass description
#' @export
Imp <- function(object=NULL) {
  if (inherits(object, 'om'))
    return(object@Imp)
  
  .Object <- methods::new('imp')
  
  validObject(.Object)
  .Object
}

validImpObject <- function(object) {
  TRUE
}
setValidity('imp', validImpObject)

setMethod("initialize", "imp", function(.Object) {
  .Object
})

#' @describeIn ImpClass Assign an [Imp()] object to a [OM()] object
#' @param x An [OM()] class object
#' @param value An `imp` class object to assign to `x`
#' @export
`Imp<-` <- function(x, value) {
  assignSlot(x, value, 'Imp')
}