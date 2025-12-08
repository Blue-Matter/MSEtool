#' Access or assign the maximum or minimum age
#' 
#' 
#' 
#' @name MaxAge
NULL 

#' @describeIn MaxAge Return the value from the `MaxAge` slot
#' @param x An [Ages()], [OM()], or [Stock()] x
#' @export
MaxAge <- function(x) {
  AccessAge(x)
}

#' @describeIn MaxAge Return the value from the `MinAge` slot
#' @export
MinAge <- function(x) {
  AccessAge(x, 'MinAge')
}

AccessAge <- function(x, slot='MaxAge') {
  CheckClass(x, c('ages', 'stock', 'om', 'StockList'), 'x')
  
  if (inherits(x, 'ages'))
    return(slot(x,slot))
  
  if (inherits(x, 'stock'))
    return(slot(x@Ages,slot))
  
  if (inherits(x, 'StockList')) {
    x <- purrr::map(x, \(stock) slot(stock@Ages, slot))
    class(x) <- 'StockList'
    return(x)
  }

  if (inherits(x, 'om')) 
    Recall(x@Stock, slot)
}

#' @describeIn MaxAge Assign a value to the `MaxAge` slot
#' @param value Either a numeric value for [Ages()] or [Stock()] objects or a list of 
#' length [nStock()] for [OM()] objects
#' @export
`MaxAge<-` <- function(x, value) {
  AssignAge(x, value)
}

#' @describeIn MaxAge Assign a value to the `MinAge` slot
#' @export
`MinAge<-` <- function(x, value) {
  AssignAge(x, value, 'MinAge')
}


AssignAge <- function(x, value, slot='MaxAge') {
  
  CheckClass(x, c('ages', 'stock', 'om', 'StockList'), 'x')
  
  if (inherits(x, 'ages')) {
    CheckClass(value, 'numeric', 'value')
    CheckValue(value)
    slot(x, slot) <- value
  } else if (inherits(x, 'stock')) {
    CheckClass(value, 'numeric', 'value')
    CheckValue(value)
    slot(x@Ages, slot) <- value
    
  } else if (inherits(x, 'StockList')) {
    CheckClass(value, 'list', 'value')
    if (length(value)!=length(x))
      cli::cli_abort('`length(value)` must equal {.fun nStock}')
    
    x <- purrr::map2(x, value, \(stock, val) {
      slot(stock@Ages, slot) <- val
      stock
    })
  } else if (inherits(x, 'om')) {
    return(Recall(x@Stock, value, slot))
  }
  
  methods::validObject(x)
  x
}