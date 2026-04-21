#' Rename Stocks and Fleets in an OM
#'
#' Renames stocks and/or fleets throughout an operating model (OM) object,
#' updating all names recursively across S4 slots, lists, and array dimension
#' names.
#'
#' @param object An object of class `om`.
#' @param Stocks Optional named character vector or named list mapping new
#'   stock names to current names (e.g. `c('new_name' = 'old_name')`). If `NULL`,
#'   stock names are unchanged.
#' @param Fleets Optional named character vector or named list mapping new
#'   fleet names to current names. If `NULL`, fleet names are unchanged.
#'
#' @return The input `OM` object with stock and/or fleet names updated
#'   throughout.
#'
#' Renaming is applied recursively: list element names, S4 slot names, and the
#' `Stock` and `Fleet` dimensions of arrays are all updated wherever a match is
#' found. Names not present in `Stocks` or `Fleets` are left unchanged, so
#' partial lookups are safe.
#' 
#' The lookup arguments use the convention `NewName = OldName`, e.g.:
#' `Fleets = list(NewFleet = 'OldFleet')`.
#' 
#' @examples
#' OM <- ExampleOM
#' StockNames(OM)
#' FleetNames(OM)
#' 
#' OM2 <- Rename(OM, 
#'               Stocks = list('New Stock Name' = 'Example Stock'),
#'               Fleets = list('New Fleet Name' = 'Example Fleet')
#' )
#' 
#' StockNames(OM2)
#' FleetNames(OM2)
#' 
#' @seealso [Reorder()]
#' @export
Rename <- function(object, Stocks=NULL, Fleets=NULL) {
  
  # Currently on supports OM objects
  CheckClass(object, 'om', 'OM')
  
  if (!is.null(Stocks)) 
    object <- Rename_Stock(object, Stocks)
  
  if (!is.null(Fleets)) 
    object <- Rename_Fleet(object, Fleets)
  
  object
}

rename_matched <- function(nms, lookup) {
  
  if (all(lengths(lookup) != 1))
    cli::cli_abort(c("x"='New names must be length 1'))
  
  old_names <- as.character(unlist(lookup))
  new_names <- names(lookup)
  
  ind <- match(nms, old_names)
  hit <- !is.na(ind)
  nms[hit] <- new_names[ind[hit]]
  nms
}

rename_recursive <- function(object, lookup, dim_name) {
  if (isS4(object)) {
    for (s in slotNames(object)) {
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- rename_recursive(val, lookup, dim_name)
    }
    return(object)
  }
  
  if (is.list(object)) {
    if (!length(object))
      return(object)
    if (!is.null(names(object)))
      names(object) <- rename_matched(names(object), lookup)
    for (i in seq_along(object)) {
      if (!is.null(object[[i]]))
        object[[i]] <- rename_recursive(object[[i]], lookup, dim_name)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (dim_name %in% names(dnames)) {
      dnames[[dim_name]] <- rename_matched(dnames[[dim_name]], lookup)
      dimnames(object) <- dnames
    }
    return(object)
  }
  
  if (is.character(object)) {
    object <- rename_matched(object, lookup)
  }
  
  object
}

Rename_Stock <- function(object, Stocks) rename_recursive(object, Stocks, "Stock")
Rename_Fleet <- function(object, Fleets) rename_recursive(object, Fleets, "Fleet")
# Rename_Year <- function(object, Years) rename_recursive(object, Years, "Year")
# Rename_Age <- function(object, Years) rename_recursive(object, Ages, "Age")
