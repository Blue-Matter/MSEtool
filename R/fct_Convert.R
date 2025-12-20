#' Convert legacy S4 objects to new S4 object classes
#' 
#' @include MOM_object.r
#' @param x A legacy S4 object of class [OM-class], [MOM-class], [Stock-class], 
#' [Fleet-class], [Obs-class], [Imp-class], or [Data-class]
#' @param ... Additional named arguments to pass to the other `Convert` functions.
#' 
#' @details
#' 
#' Note: not all of the slots from [Obs-class] objects exist in the new [Obs()] objects
#' 
#' Note: Legacy `Data` objects do not contain data on `Discards`. It is assumed that data in `Data@Cat` is 
#' `Landings` and there are no `Discards`. This will be wrong if `Data@Cat` represents total removals. 
#' In this case, users need to update the new  [Data()] object manually.
#' 
#' 
#' @return An updated S4 object
#' @example man-examples/Convert.R
#' @export 
Convert <- function(x, ...) {
  
  if (inherits(x, 'OM'))
    return(ConvertOM(x, ...))
  
  if (inherits(x, 'MOM'))
    return(ConvertMOM(x, ...))
  
  if (inherits(x, 'Stock'))
    return(ConvertStock(x, ...))
  
  if (inherits(x, 'Fleet'))
    return(ConvertFleet(x, ...))
  
  if (inherits(x, 'Obs'))
    return(ConvertObs(x, ...))
  
  if (inherits(x, 'Imp'))
    return(ConvertImp(x, ...))
  
  if (inherits(x, 'Data'))
    return(ConvertData(x, ...))
  
} 

