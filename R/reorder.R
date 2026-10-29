#' Reorder Stocks and Fleets in an OM
#'
#' Reorders stocks and/or fleets throughout an operating model (OM) object,
#' updating all list elements and array dimension orders recursively across S4
#' slots.
#'
#' @param object An object of class `om`.
#' @param Stocks Optional character vector of all stock names in the desired
#'   order. Must contain exactly the same names as currently in `OM`. If
#'   `NULL`, stock order is unchanged.
#' @param Fleets Optional character vector of all fleet names in the desired
#'   order. Must contain exactly the same names as currently in `OM`. If
#'   `NULL`, fleet order is unchanged.
#'
#' @return The input `om` object with stocks and/or fleets reordered
#'   throughout.
#'
#' @details
#' Reordering is applied recursively: list element names, S4 slot names, and
#' the `Stock` and `Fleet` dimensions of arrays are all reordered wherever a
#' match is found. The full set of names must be provided — partial reordering
#' is not supported.
#'
#' @examples
#' OM <- AddFleet(SingleStockOM, 'DummyFleet')
#' FleetNames(OM)
#' 
#' OM2 <- Reorder(OM, Fleets = rev(FleetNames(OM)))
#' FleetNames(OM2)
#' 
#' @seealso [Rename()]
#' @export
Reorder <- function(object, Stocks=NULL, Fleets=NULL) {
  
  .CheckClass(object, 'om', 'OM')
  
  if (!is.null(Stocks))
    object <- .ReorderStock(object, Stocks)
  
  if (!is.null(Fleets))
    object <- .ReorderFleet(object, Fleets)
  
  object
}

.ReorderCheck <- function(provided, current, label) {
  missing  <- setdiff(provided, current)
  extra    <- setdiff(current, provided)
  
  if (length(missing))
    cli::cli_abort(c('x'='{label} names not found in OM: {.val {missing}}'))
  if (length(extra))
    cli::cli_abort(c('x'='All {label} names must be provided. Missing from input: {.val {extra}}'))
}

.ReorderRecursive <- function(object, order, dim_name) {
  
  if (isS4(object)) {
    for (s in slotNames(object)) {
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- .ReorderRecursive(val, order, dim_name)
    }
    return(object)
  }
  
  if (is.list(object)) {
    if (!length(object))
      return(object)
    if (!is.null(names(object)) && all(order %in% names(object))) {
      # reorder matching elements; preserve any others (e.g. non-stock/fleet slots)
      other <- setdiff(names(object), order)
      object <- object[c(order, other)]
    }
    for (i in seq_along(object)) {
      if (!is.null(object[[i]]))
        object[[i]] <- .ReorderRecursive(object[[i]], order, dim_name)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (dim_name %in% names(dnames)) {
      current  <- dnames[[dim_name]]
      if (any(order %in% current)) {
        order <- order[order %in% current]
        new_order <- c(order, setdiff(current, order))
        object <- abind::asub(object, idx=match(new_order, current), dims=which(names(dnames) == dim_name), drop=FALSE)
        dnames[[dim_name]] <- new_order
        dimnames(object)   <- dnames
      }
    }
    return(object)
  }
  
  if (is.character(object)) {
    if (any(order %in% object)) {
      other  <- setdiff(object, order[order%in% object])
      object <- c(order[order%in% object], other)
    }
  }
  
  object
}

.ReorderStock <- function(object, Stocks) {
  .ReorderCheck(Stocks, StockNames(object), 'Stock')
  .ReorderRecursive(object, Stocks, "Stock")
}

.ReorderFleet <- function(object, Fleets) {
  .ReorderCheck(Fleets, FleetNames(object), 'Fleet')
  .ReorderRecursive(object, Fleets, "Fleet")
}
