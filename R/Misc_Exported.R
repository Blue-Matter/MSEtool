#' Is a value NA or zero.
#' 
#' As title
#' 
#' 
#' @param x A numeric value.
#' @return TRUE or FALSE 
#' @author T. Carruthers
#' @keywords internal
#' @export
NAor0 <- function(x) {
  if (length(x) == 0) 
    return(TRUE)
  if (length(x) > 0) 
    return(is.na(x[1]))
}
