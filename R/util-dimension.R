#' Add or Drop Named Dimensions of an Array
#'
#' Utilities to add or drop named dimensions from arrays  
#'
#' **AddDimension**
#'
#' Adds a new dimension of length `val` to an array. The new dimension is inserted
#' at position `pos` and optionally given a name and value. If the dimension
#' name already exists, the array is returned unchanged.
#'
#' **DropDimension**
#'
#' Drops one or more named dimensions from an array.
#' 
#' * All requested dimensions must exist
#' * At least one dimension must remain
#' * Dimensions of length one are removed by reshaping only
#' * Dimensions of length greater than one are sliced at their first index,
#'   with an optional warning
#'
#' Other dimensions are preserved.
#'
#' @param array An array with named dimensions
#' @param name Character vector of dimension names to add or drop.
#'   For `AddDimension`, a single name.
#'   For `DropDimension`, one or more existing dimension names.
#' @param val Value to assign to the new dimension name in `AddDimension`.
#' @param pos Integer giving the position at which to insert the new dimension
#'   in `AddDimension`. Defaults to 1.
#' @param warn Logical; if `TRUE`, warn when dropping a dimension of length
#'   greater than one in `DropDimension`.
#'
#' @return
#' An array with modified dimensions and updated `dimnames`.
#'
#' @example man-examples/AddDimension.R
#'
#' @name dimension-utils
#' @rdname dimension-utils
#' @export
AddDimension <- function(array, name, val = 1, pos = NULL) {
  if (is.null(array)) {
    return(NULL)
  }
  
  CheckClass(array, 'array', 'array')
  CheckClass(name, 'character', 'name')
  
  d  <- dim(array)
  dn <- dimnames(array)
  nms <- names(dn)
  
  # If dimension already exists, return unchanged
  if (!is.null(nms) && name %in% nms) {
    return(array)
  }
  
  nd <- length(d)
  if (is.null(pos)) {
    pos <- nd + 1
  }
  
  if (pos < 1 || pos > nd + 1) {
    cli::cli_abort(
      "Argument {.arg pos} must be between 1 and {nd + 1}"
      )
  }
  
  # size of new dimension
  new_len <- if (is.null(val)) 1L else length(val)
  
  array <- array(
    data = array,
    dim  = append(d, new_len, after = pos - 1)
  )
  
  # update dimensions
  new_dim <- append(d, new_len, after = pos - 1)
  dim(array) <- new_dim
  
  # update dimnames
  if (!is.null(dn)) {
    new_dn <- append(dn, list(val), after = pos - 1)
    names(new_dn)[pos] <- name
    dimnames(array) <- new_dn
  }
  
  array
}


#' @rdname dimension-utils
#' @export
DropDimension <- function(array, name, warn = TRUE) {
  if (is.null(array)) {
    return(array)
  }
  
  CheckClass(array, 'array', 'array')
  CheckClass(name, 'character', 'name')
  
  d  <- dim(array)
  dn <- dimnames(array)
  nms <- names(dn)
  
  if (is.null(nms)) {
    cli::cli_abort("Array has no named dimensions", .internal = TRUE)
  }
  
  # Ensure all requested dimensions exist
  ind <- match(name, nms)
  if (anyNA(ind)) {
    missing <- name[is.na(ind)]
    cli::cli_abort("Dimension(s) not found: {.val {missing}}")
  }
  
  ind <- unique(ind)
  
  # Ensure at least one dimension remains
  if (length(ind) >= length(d)) {
    cli::cli_abort("Cannot drop all dimensions; at least one must remain")
  }
  
  # Warn for dimensions with length > 1
  if (warn) {
    big <- ind[d[ind] > 1]
    if (length(big)) {
      cli::cli_alert_warning(
        "Note: dimension(s) {.val {nms[big]}} have length > 1. Use `warn=FALSE` to suppress this message"
      )
    }
  }
  
  if (all(d[ind] == 1)) {
    dim(array) <- d[-ind]
    dimnames(array) <- dn[-ind]
    return(array)
  }
  
  idx <- vector("list", length(d))
  for (i in seq_along(d)) {
    idx[[i]] <- if (i %in% ind && d[i] > 1) 1 else seq_len(d[i])
  }
  
  out <- do.call(`[`, c(list(array), idx, list(drop = FALSE)))
  
  dim(out) <- d[-ind]
  dimnames(out) <- dn[-ind]
  
  out
}


