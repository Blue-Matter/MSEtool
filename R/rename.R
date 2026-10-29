#' Rename Stocks and Fleets in an OM
#'
#' Renames stocks and/or fleets throughout an operating model (OM) object,
#' updating all names recursively across S4 slots, lists, and array dimension
#' names.
#'
#' @param object An object of class `om`.
#' @param Stocks Optional stock names. Either an unnamed character vector of new
#'   names with length `nStock(object)`, matched to the current names in order,
#'   or a named character vector mapping new stock names to current names
#'   (e.g. `c('new_name' = 'old_name')`). If `NULL`, stock names are unchanged.
#' @param Fleets Optional fleet names, in the same form as `Stocks`. An unnamed
#'   vector must have length `nFleet(object)`. If `NULL`, fleet names are
#'   unchanged.
#'
#' @return The input `OM` object with stock and/or fleet names updated
#'   throughout.
#'
#' @details
#' Renaming is applied recursively: list element names, S4 slot names, and the
#' `Stock` and `Fleet` dimensions of arrays are all updated wherever a match is
#' found. A named lookup can cover a subset of the names; names not included
#' are left unchanged.
#'
#' A named lookup uses the convention `NewName = OldName`, e.g.:
#' `Fleets = c(NewFleet = 'OldFleet')`.
#'
#' An error is raised if any new or old name is empty or `NA`, if new or old
#' names are duplicated, if an old name is not found in the OM, or if the
#' renaming would leave two stocks (or fleets) with the same name.
#'
#' @examples
#' OM <- SingleStockOM
#' StockNames(OM)
#' FleetNames(OM)
#' 
#' OM2 <- Rename(OM, 
#'               Stocks = c('New Stock Name' = 'Example Albacore Stock'),
#'               Fleets = c('New Fleet Name' = 'AsympExFleet')
#' )
#'
#' StockNames(OM2)
#' FleetNames(OM2)
#'
#' OM3 <- Rename(OM, Stocks = 'New Stock Name', Fleets = 'New Fleet Name')
#' StockNames(OM3)
#' FleetNames(OM3)
#'
#' @seealso [Reorder()]
#' @export
Rename <- function(object, Stocks=NULL, Fleets=NULL) {
  
  # Currently on supports OM objects
  .CheckClass(object, 'om', 'OM')
  object <- UpdateObject(object)

  if (!is.null(Stocks))
    object <- .RenameStock(object, .CheckRenameLookup(Stocks, StockNames(object), 'Stocks'))

  if (!is.null(Fleets))
    object <- .RenameFleet(object, .CheckRenameLookup(Fleets, FleetNames(object), 'Fleets'))

  object
}

.CheckRenameLookup <- function(lookup, current, arg) {

  if (!is.character(lookup) && !is.list(lookup))
    cli::cli_abort(c("x"="{.arg {arg}} must be a character vector"))

  if (!length(lookup))
    cli::cli_abort(c("x"="{.arg {arg}} is empty"))

  if (any(lengths(lookup) != 1))
    cli::cli_abort(c("x"="Each element of {.arg {arg}} must be a single name"))

  if (is.list(lookup) && !all(vapply(lookup, is.character, logical(1))))
    cli::cli_abort(c("x"="Elements of {.arg {arg}} must be character"))

  values <- as.character(unlist(lookup, use.names = FALSE))

  if (is.null(names(lookup))) {
    if (length(values) != length(current))
      cli::cli_abort(c("x"="Unnamed {.arg {arg}} must have length {length(current)} (one new name for each of {.val {current}}), not {length(values)}"))
    lookup <- stats::setNames(current, values)
  } else {
    lookup <- stats::setNames(values, names(lookup))
  }

  new_names <- names(lookup)
  old_names <- unname(lookup)

  if (anyNA(new_names) || any(!nzchar(trimws(new_names))))
    cli::cli_abort(c("x"="New names in {.arg {arg}} cannot be empty or {.val NA}",
                     "i"="Use either a fully named lookup {.code c(NewName = 'OldName')} or an unnamed vector of all new names"))

  if (anyNA(old_names) || any(!nzchar(trimws(old_names))))
    cli::cli_abort(c("x"="Old names in {.arg {arg}} cannot be empty or {.val NA}"))

  if (anyDuplicated(new_names))
    cli::cli_abort(c("x"="Duplicated new names in {.arg {arg}}: {.val {unique(new_names[duplicated(new_names)])}}"))

  if (anyDuplicated(old_names))
    cli::cli_abort(c("x"="Duplicated old names in {.arg {arg}}: {.val {unique(old_names[duplicated(old_names)])}}"))

  missing <- setdiff(old_names, current)
  if (length(missing))
    cli::cli_abort(c("x"="Names in {.arg {arg}} not found in the OM: {.val {missing}}",
                     "i"="Current names: {.val {current}}"))

  renamed <- .RenameMatched(current, lookup)
  if (anyDuplicated(renamed))
    cli::cli_abort(c("x"="Renaming {.arg {arg}} would create duplicate names: {.val {unique(renamed[duplicated(renamed)])}}"))

  lookup
}

.RenameMatched <- function(nms, lookup) {

  old_names <- as.character(unlist(lookup))
  new_names <- names(lookup)
  
  ind <- match(nms, old_names)
  hit <- !is.na(ind)
  nms[hit] <- new_names[ind[hit]]
  nms
}

.RenameRecursive <- function(object, lookup, dim_name) {
  if (isS4(object)) {
    for (s in slotNames(object)) {
      val <- slot(object, s)
      if (!is.null(val))
        slot(object, s) <- .RenameRecursive(val, lookup, dim_name)
    }
    return(object)
  }
  
  if (is.list(object)) {
    if (!length(object))
      return(object)
    if (!is.null(names(object)))
      names(object) <- .RenameMatched(names(object), lookup)
    for (i in seq_along(object)) {
      if (!is.null(object[[i]]))
        object[[i]] <- .RenameRecursive(object[[i]], lookup, dim_name)
    }
    return(object)
  }
  
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (dim_name %in% names(dnames)) {
      dnames[[dim_name]] <- .RenameMatched(dnames[[dim_name]], lookup)
      dimnames(object) <- dnames
    }
    return(object)
  }
  
  if (is.character(object)) {
    object <- .RenameMatched(object, lookup)
  }
  
  object
}

.RenameStock <- function(object, Stocks) .RenameRecursive(object, Stocks, "Stock")
.RenameFleet <- function(object, Fleets) .RenameRecursive(object, Fleets, "Fleet")
# Rename_Year <- function(object, Years) .RenameRecursive(object, Years, "Year")
# Rename_Age <- function(object, Years) .RenameRecursive(object, Ages, "Age")
