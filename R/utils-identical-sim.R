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
IdenticalSims <- function(object, ignore=NULL, debug=FALSE) {
  
  if (debug) {
    print(class(object))
  }

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
      
      perm <- c(sim_dim, setdiff(seq_along(dims), sim_dim))
      arr_perm <- aperm(object, perm)
      
      mat <- matrix(arr_perm, nrow = dims[sim_dim])
      
      ref <- mat[1, , drop=TRUE]
      ind <- any(mat != matrix(ref, nrow=nrow(mat), ncol=ncol(mat), byrow=TRUE))
      if (is.na(ind) || ind)
        return(FALSE)
      
    }
  }
  
  TRUE
}







