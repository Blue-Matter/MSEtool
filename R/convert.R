#' Convert legacy S4 objects to new S4 object classes
#' 
#' Converts legacy openMSE S4 objects to their equivalent new S4 class
#' representations. Dispatches to the appropriate conversion function based on
#' the class of `x`.
#' 
#' @param x A legacy S4 object of class [OM-legacy-class], [MOM-legacy-class],
#'   [Stock-legacy-class], [Fleet-legacy-class], [Obs-legacy-class],
#'   [Imp-legacy-class], or [Data-legacy-class].
#' @param ... Additional named arguments passed to the underlying conversion
#'   function ([ConvertOM()], [ConvertMOM()], [ConvertStock()],
#'   [ConvertFleet()], [ConvertObs()], [ConvertImp()], or [ConvertData()]).
#'   
#' @details
#' Not all slots from [Obs-legacy-class] objects exist in the new [obs-class]
#' objects. Slots without a direct equivalent are silently dropped.
#'
#' Legacy `Data` objects do not contain discard data. It is assumed that
#' `Data@Cat` represents landings only, with no discards. If `Data@Cat`
#' represents total removals, the resulting [data-class] object must be
#' updated manually after conversion.
#'
#' @return A new S4 object of the corresponding updated class:
#'   - [OM-legacy-class] → [om-class]
#'   - [MOM-legacy-class] → [om-class]
#'   - [Stock-legacy-class] → [stock-class]
#'   - [Fleet-legacy-class] → [fleet-class]
#'   - [Obs-legacy-class] → [obs-class]
#'   - [Imp-legacy-class] → [imp-class]
#'   - [Data-legacy-class] → [data-class]
#'
#' @seealso [ConvertOM()], [ConvertMOM()], [ConvertStock()], [ConvertFleet()],
#'   [ConvertObs()], [ConvertImp()], [ConvertData()]
#'
#' @include zz_MOM_object.r
#' @include zz_Class_definitions.R
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
  
  cli::cli_alert_info('No `Convert*` function found for object of class {.val {class(x)}}')
  cli::cli_alert('Returning object unchanged')
  x
} 

