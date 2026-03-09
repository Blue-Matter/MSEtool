#' Convert Between Lists and Arrays
#'
#' Convert between lists of arrays and higher-dimensional arrays in a
#' predictable and efficient way.
#'
#' @details
#' These functions provide inverse operations for reshaping data between list
#' and array representations while preserving dimension structure and
#' dimnames.
#'
#'  **List2Array**
#'
#' Combines a list of vectors or arrays into a single array by adding a new
#' dimension corresponding to list elements. All list elements must have
#' compatible dimensions. The new dimension has length equal to the length of
#' the list.
#'
#' **Array2List**
#'
#' Splits an array into a list by extracting slices along a specified dimension.
#' Each list element is an array with the remaining dimensions preserved.
#'
#' @param x A list (for `List2Array`) or an array (for `Array2List`).
#' @param name Character string giving the name of the new dimension created by
#'   `List2Array`.
#' @param dim1 Character string giving the name of the first dimension when
#'   combining a list of vectors. Defaults to `"Sim"`.
#' @param pos Integer or character specifying the position or name of the
#'   dimension to add (`List2Array`) or split (`Array2List`).
#' @param index Optional integer vector specifying which slices to extract in
#'   `Array2List`. Defaults to all slices.
#'
#' @return
#' * `List2Array()`: an array with one additional dimension
#' * `Array2List()`: a named list of arrays
#'
#' @example man-examples/List2Array.R
#'
#' @name list-array-conversion
#' @rdname list-array-conversion
#' @export
List2Array <- function(x, name = "Fleet", dim1='Sim', pos = NULL) {
  # Pass-through
  if (is.array(x)) {
    return(x)
  }
  
  if (!length(x)) {
    return(NULL)
  }
  
  # Ensure names exist for new dimension
  if (is.null(names(x))) {
    names(x) <- seq_along(x)
  }
  
  first <- x[[1]]
  
  ## list of vectors
  if (is.null(dim(first))) {
    out <- array(
      unlist(x, use.names = FALSE),
      dim = c(length(first), length(x))
    )
    
    dimnames(out) <- list(
      seq_along(first),
      names(x)
    )
    
    names(dimnames(out)) <- c(dim1, name)
  } else {  ## list of arrays 
    d  <- dim(first)
    dn <- dimnames(first)
    if (is.null(dn)) {
      cli::cli_abort("arrays must have named dimensions")
    }
    
    out <- array(
      unlist(x, use.names = FALSE),
      dim = c(d, length(x))
    )
    
    dn <- c(dn, list(names(x)))
    names(dn)[length(dn)] <- name
    dimnames(out) <- dn
  }
  
  ##  reordering 
  if (!is.null(pos)) {
    nd <- length(dim(out))
    if (pos < 1 || pos > nd) {
      cli::cli_abort("`pos` must be between 1 and number of dimensions ({.val {nd}})")
    }
    
    cur <- seq_len(nd)
    perm <- append(cur[-nd], nd, after = pos - 1)
    out <- aperm(out, perm)
  }
  
  out
}

#' @rdname list-array-conversion
#' @export
Array2List <- function(x, pos=2, index = NULL) {
  if (is.null(x)) {
    return(NULL)
  }
  
  if (!is.array(x)) {
    cli::cli_abort("`x` must be an array")
  }
  
  dn <- dimnames(x)
  d  <- dim(x)
  nms <- names(dn)
  
  if (is.character(pos)) {
    pos <- match(pos, nms)
    if (is.na(pos)) {
      cli::cli_abort("`pos` does not match any dimension name")
    }
  }
  
  if (pos < 1 || pos > length(d)) {
    cli::cli_abort("`pos` out of bounds")
  }
  
  if (is.null(index)) {
    index <- seq_len(d[pos])
  }
  
  out <- vector("list", length(index))
  names(out) <- dn[[pos]][index]
  
  idx <- lapply(d, seq_len)
  
  for (i in seq_along(index)) {
    idx[[pos]] <- index[i]
    val <- do.call(`[`, c(list(x), idx, list(drop = FALSE)))
    dim(val) <- d[-pos]
    dimnames(val) <- dn[-pos]
    out[[i]] <- val
  }
  
  out
}

