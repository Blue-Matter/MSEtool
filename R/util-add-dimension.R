#' Add or Drop Named Dimensions of an Array
#'
#' Utilities to add or drop named dimensions from arrays.
#'
#' **AddDimension**
#'
#' Adds a new dimension to an array at position `pos` with the given `name`
#' and `val` as its dimname values. If the dimension name already exists,
#' the array is returned unchanged.
#'
#' **DropDimension**
#'
#' Drops one or more named dimensions from an array. All requested dimensions
#' must exist and at least one dimension must remain. Dimensions of length > 1
#' are sliced at their first index, with an optional warning.
#'
#' @param array An array with named dimensions.
#' @param name A single character string naming the dimension to add
#'   (`AddDimension`) or a character vector of dimension names to drop
#'   (`DropDimension`).
#' @param val Value(s) to assign as the dimname of the new dimension in
#'   `AddDimension`. Defaults to `1`.
#' @param pos Integer giving the position at which to insert the new dimension
#'   in `AddDimension`. Defaults to the last position.
#' @param warn Logical; if `TRUE`, warn when dropping a dimension of length > 1.
#'
#' @return An array with modified dimensions and updated `dimnames`.
#'
#' @example man-examples/AddDimension.R
#'
#' @name dimension-utils
#' @rdname dimension-utils
#' @export
AddDimension <- function(array, name, val = 1, pos = NULL) {
  if (is.null(array)) return(NULL)
  
  CheckClass(array, 'array', 'array')
  CheckClass(name, 'character', 'name')
  
  if (length(name) != 1)
    cli::cli_abort("`name` must be a single character string, not a vector of length {length(name)}.")
  
  d   <- dim(array)
  dn  <- dimnames(array)
  nms <- names(dn)
  
  if (!is.null(nms) && name %in% nms) return(array)
  
  nd  <- length(d)
  pos <- if (is.null(pos)) nd + 1L else as.integer(pos)
  
  if (pos < 1L || pos > nd + 1L)
    cli::cli_abort("`pos` must be between 1 and {nd + 1}.")
  
  new_len <- length(val)
  new_dim <- append(d, new_len, after = pos - 1L)
  
  out <- array(data = array, dim = new_dim)
  
  if (!is.null(dn)) {
    new_dn         <- append(dn, list(val), after = pos - 1L)
    names(new_dn)[pos] <- name
    dimnames(out)  <- new_dn
  }
  
  out
}


#' @rdname dimension-utils
#' @export
DropDimension <- function(array, name, warn = TRUE) {
  if (is.null(array)) return(NULL)
  
  CheckClass(array, 'array', 'array')
  CheckClass(name, 'character', 'name')
  
  d   <- dim(array)
  dn  <- dimnames(array)
  nms <- names(dn)
  
  if (is.null(nms))
    cli::cli_abort("Array has no named dimensions.", .internal = TRUE)
  
  ind <- match(name, nms)
  if (anyNA(ind)) {
    missing_dims <- name[is.na(ind)]
    cli::cli_abort("Dimension(s) not found: {.val {missing_dims}}.")
  }
  
  ind <- unique(ind)
  
  if (length(ind) >= length(d))
    cli::cli_abort("Cannot drop all dimensions; at least one must remain.")
  
  if (warn && any(d[ind] > 1)) {
    big <- nms[ind[d[ind] > 1]]
    cli::cli_alert_warning(
      "Dimension(s) {.val {big}} have length > 1 and will be sliced at index 1. Use `warn = FALSE` to suppress."
    )
  }
  
  idx <- lapply(seq_along(d), function(i) if (i %in% ind) 1L else seq_len(d[i]))
  out <- do.call(`[`, c(list(array), idx, list(drop = FALSE)))
  
  dim(out)      <- d[-ind]
  dimnames(out) <- dn[-ind]
  
  out
}