#' Check if all 'Sim' slices in an object are identical
#'
#' Recursively checks S4 objects, lists, and arrays to determine if all
#' values along the "Sim" dimension are identical. Returns `FALSE` immediately
#' if a discrepancy is found.
#'
#' @param object An S4 object, list, or array to check
#' @param ignore Character vector of slots to ignore (for S4 objects).
#' @param debug Logical; if `TRUE`, prints debug information for each slot or element.
#'
#' @return Logical `TRUE` if all 'Sim' slices are identical, `FALSE` otherwise.
#' 
#' @keywords internal
.IdenticalSims <- function(object, ignore=NULL, debug=FALSE) {
  
  if (debug) 
    print(class(object))
  
  # S4 objects 
  if (isS4(object)) {
    for (s in slotNames(object)) {
      
      if (!is.null(ignore) && s %in% ignore) {
        next()
      }
        
      val <- slot(object, s)
      if (debug) {
        print(paste("Slot:", s))
      }
      
      if (!is.null(val)) {
        if (!Recall(val, ignore=ignore, debug=debug)) return(FALSE)
      }
    }
  } 
  
  # Lists
  if (is.list(object)) {
    if (length(object) == 0) return(TRUE)
    for (el in object) {
      if (!is.null(el)) {
        if (!Recall(el,  ignore=ignore, debug=debug)) return(FALSE)
      }
    }
  }
  
  ## Arrays 
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Sim" %in% names(dnames)) {
      if (all(is.na(object))) return(TRUE)
      
      sim_dim <- which(names(dnames) == "Sim")
      dims <- dim(object)
      if (dims[sim_dim]==1)
        return(TRUE)
      
      perm    <- c(sim_dim, setdiff(seq_along(dims), sim_dim))
      mat     <- matrix(.Aperm(object, perm), nrow = dims[sim_dim])
      
      ref <- mat[1, , drop = TRUE]
      
      col_na    <- colSums(!is.na(mat)) == 0L          
      col_equal <- colSums(mat != matrix(ref, nrow = nrow(mat),
                                         ncol = ncol(mat),
                                         byrow = TRUE),
                           na.rm = TRUE) == 0L         
      
      if (!all(col_na | col_equal))
        return(FALSE)
    }
  }
  
  TRUE
}


#' Identify slots in an S4 object that vary across 'Sim' slices
#'
#' Recursively inspects S4 objects, lists, and arrays to find all slots
#' (or named list elements) that contain values varying along the "Sim"
#' dimension. Complements `.IdenticalSims()`.
#'
#' @param object An S4 object, list, or array to inspect.
#' @param ignore Character vector of slot names to skip.
#' @param path Character; internal use — tracks the slot path for reporting.
#'
#' @return A character vector of dot-separated slot paths that vary across
#'   simulations (e.g. `"Fleet@Effort"`, `"Stock@Growth@Linf"`).
#'   Returns an empty character vector if nothing varies.
#'
#' @keywords internal
.VaryingSims <- function(object, ignore = NULL, path = NULL) {
  
  varying <- character(0)
  
  # S4 objects
  if (isS4(object)) {
    for (s in slotNames(object)) {
      if (!is.null(ignore) && s %in% ignore) next
      
      val      <- slot(object, s)
      new_path <- if (is.null(path)) s else paste(path, s, sep = "@")
      
      if (!is.null(val)) {
        varying <- c(varying, .VaryingSims(val, ignore = ignore, path = new_path))
      }
    }
    return(varying)
  }
  
  # Lists
  if (is.list(object)) {
    if (length(object) == 0) return(varying)
    nms <- names(object)
    for (i in seq_along(object)) {
      el       <- object[[i]]
      tag      <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
      new_path <- if (is.null(path)) tag else paste(path, tag, sep = "$")
      if (!is.null(el)) {
        varying <- c(varying, .VaryingSims(el, ignore = ignore, path = new_path))
      }
    }
    return(varying)
  }
  
  # Arrays
  if (is.array(object)) {
    dnames  <- dimnames(object)
    if (!is.null(dnames) && "Sim" %in% names(dnames)) {
      if (all(is.na(object))) return(varying)
      
      sim_dim <- which(names(dnames) == "Sim")
      dims    <- dim(object)
      if (dims[sim_dim] == 1) return(varying)
      
      perm <- c(sim_dim, setdiff(seq_along(dims), sim_dim))
      mat  <- matrix(.Aperm(object, perm), nrow = dims[sim_dim])
      ref  <- mat[1, , drop = TRUE]
      
      col_na    <- colSums(!is.na(mat)) == 0L
      col_equal <- colSums(
        mat != matrix(ref, nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE),
        na.rm = TRUE
      ) == 0L
      
      if (!all(col_na | col_equal)) {
        varying <- c(varying, path)
      }
    }
    return(varying)
  }
  
  varying
}




