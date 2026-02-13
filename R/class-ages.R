#' Ages class
#'
#' An S4 class representing the age structure for a [Stock()] object.
#' 
#' See [Ages()] for details.
#'
#' @slot MaxAge Numeric scalar giving the maximum age. If `PlusGroup == TRUE`,
#'   this represents the plus group age.
#' @slot MinAge  Numeric scalar giving the minimum age.
#' @slot Units Character string describing the time units used in the definition
#' of `MaxAge` and `MinAge` (e.g. `"year"`).
#' @slot PlusGroup Logical; indicates whether the maximum age is treated
#'   as a plus group.
#' @slot Classes Numeric vector of age classes expressed in years
#' @export
#' @include class-unions.R
#' @rdname ages-class
#' @seealso [Ages()]
setClass('ages',
         slots=c(MaxAge='numeric',
                 MinAge='numeric',
                 Units='character',
                 PlusGroup='logical',
                 Classes='num.null')
         
)

setValidity('ages', function(object) {
  
  if (!length(object@MaxAge) || !length(object@MinAge))
    return(TRUE)
  
  if (length(object@MaxAge) != 1)
    return("`MaxAge` must be a numeric scalar")
  
  if (length(object@MinAge) != 1)
    return("`MinAge` must be a numeric scalar")
  
  if (!is.finite(object@MaxAge) || !is.finite(object@MinAge))
    return("`MaxAge` and `MinAge` must be finite numeric values")
  
  if (object@MinAge < 0)
    return("`MinAge` must be non-negative")
  
  if (object@MaxAge < object@MinAge)
    return("`MaxAge` must be greater than or equal to `MinAge`")
  
  if (length(object@PlusGroup) && length(object@PlusGroup) != 1)
    return("`PlusGroup` must be a logical scalar")
  
  if (length(object@Units)) {
    if (length(object@Units) != 1)
      return("`Units` must be a character scalar")
    if (!is.character(object@Units))
      return("`Units` must be a character string")
    
    if (!object@Units %in%  ValidUnits())
      return("Invalid `Units`. See `ValidUnits()`")
  }
  TRUE
})
