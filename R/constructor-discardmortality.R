#' Discard Mortality
#'
#' Construct a [discardmortality-class] object for a [Fleet()] object.
#'
#' Discard mortality describes the proportion of catch that is discarded and dies,
#' defined either at age or at length.
#' 
#' @param MeanAtAge A numeric array of discard mortality at age
#' @param MeanAtLength A numeric array of discard mortality at length
#' @param Classes Optional class vector (ages or lengths)
#' @param Misc A list of miscellaneous parameters
#'
#'
#' A `DiscardMortality` object can be attached to a [Fleet()] using `DiscardMortality(Fleet) <- MyDiscardMortality` and
#' retrieved using `MyDiscardMortality <- DiscardMortality(Fleet)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [MeanAtAge()], [MeanAtLength()], and [Classes()].
#' 
#' `r TechManLink()`
#' 
#' @return A [discardmortality-class] object
#'
#' @export
DiscardMortality <- function(MeanAtAge    = NULL,
                             MeanAtLength = NULL,
                             Classes      = NULL,
                             Misc         = list()) {
  
  if (methods::is(MeanAtAge, "fleet"))
    return(MeanAtAge@DiscardMortality)
  
  methods::new(
    "discardmortality",
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    Classes      = Classes,
    Misc         = Misc
  )
}


#' @rdname DiscardMortality
#' @export
`DiscardMortality<-`<- function(x,value) {
  AssignSlot(x, value, 'DiscardMortality')
}
