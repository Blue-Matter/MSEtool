#' Convert old style and new style object classes
#' @include MOM_object.r
#' @export
Convert <- function(x, ...) {
  
  if (inherits(x, 'OM'))
    return(ConvertOM(x, ...))
  if (inherits(x, 'MOM'))
    return(ConvertMOM(x, ...))
  
} 

