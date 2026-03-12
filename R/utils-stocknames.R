#' Access Stock or Fleet Names 
#'
#' @details
#' These functions extract stock or fleet names from objects used in the MSE
#' framework, including [om-class], [hist-class], [stock-class], [fleet-class], and [mse-class]
#' objects
#'
#' **StockNames**
#'
#' Returns the names of stocks contained in the object.
#'
#' **FleetNames**
#'
#' Returns the names of fleets contained in the object. For objects containing
#' multiple stocks, a list of character vectors is returned.
#'
#' @param object An [om-class], [hist-class], [stock-class], [fleet-class], or [mse-class] object.
#'
#' @return
#' * `StockNames()`: a character vector of stock names
#' * `FleetNames()`: a character vector of fleet names (always taken from the first stock)
#'
#'
#' @examples
#' StockNames(ExampleOM)
#' FleetNames(ExampleOM)
#'
#' @name name-accessors
#' @rdname name-accessors
#' @export
StockNames <- function(object) {
  if (inherits(object, c("hist", "mse"))) {
    return(names(object@OM@Stock))
  }
  
  if (inherits(object, "om")) {
    if (inherits(object@Stock, 'stock')) 
      return(object@Stock@Name)
      
    return(names(object@Stock))
  }
  
  if (inherits(object, "StockList")) {
    return(names(object))
  }
  
  NULL
}

#' @rdname name-accessors
#' @export
FleetNames <- function(object) {
  if (inherits(object, c("hist", "mse"))) {
    return(lapply(object@OM@Fleet, names)[[1]])
  }
  
  if (inherits(object, "om")) {
    if (inherits(object@Fleet, 'fleet')) {
      return(object@Fleet@Name)
    }
    if (is.list(object@Fleet)) {
      return(names(object@Fleet[[1]]))
    }
  }
  
  if (inherits(object, "StockList")) {
    return(lapply(object, names))
  }
  
  if (inherits(object, "FleetList")) {
    return(names(object))
  }
  NULL
}