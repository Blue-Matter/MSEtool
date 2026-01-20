#' @rdname Stock
#' @export
setGeneric(
  "Stock",
  function(x, ...) standardGeneric("Stock")
)

#' @rdname Stock
#' @export
setGeneric(
  "Stock<-",
  function(x, value) standardGeneric("Stock<-")
)

#' @rdname Stock
#' @export
setReplaceMethod(
  "Stock",
  signature(x = "om", value = "stock"),
  function(x, value) {
    
    x@Stock <- list(value)
    names(x@Stock) <- value@Name
    class(x@Stock) <- "StockList"
    
    x
  }
)
