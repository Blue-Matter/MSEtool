
#' @export
Effort <- function(object) {
  if (inherits(object, 'fleet'))
    return(object@Effort)
  
}

#' @describeIn Effort Assign an `Effort` object to a [Fleet()] object
#' @param x A [Fleet()] class object
#' @param value A `Effort` object to assign to `x`
#' @export
`Effort<-` <- function(x, value) {
  assignSlot(x, value, 'Effort')
}
