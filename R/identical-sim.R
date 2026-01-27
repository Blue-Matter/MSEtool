#' Check if all 'Sim' slices in an object are identical
#'
#' Recursively checks S4 objects, lists, and arrays to determine if all
#' values along the "Sim" dimension are identical. Returns `FALSE` immediately
#' if a discrepancy is found.
#'
#' @param object An S4 object, list, or array to check.
#' @param debug Logical; if `TRUE`, prints debug information for each slot or element.
#'
#' @return Logical `TRUE` if all 'Sim' slices are identical, `FALSE` otherwise.
#' 
#' @keywords internal
Identical_Sim <- function(object, ignore=NULL, debug=FALSE) {
  
  if (debug) {
    print(class(object))
  }

  ## ---- S4 objects ----
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
  
  ## ---- Lists ----
  if (is.list(object)) {
    if (length(object) == 0) return(TRUE)
    for (el in object) {
      if (!is.null(el)) {
        if (!Recall(el,  ignore=ignore, debug=debug)) return(FALSE)
      }
    }
  }
  
  ## ---- Arrays ----
  if (is.array(object)) {
    dnames <- dimnames(object)
    if (!is.null(dnames) && "Sim" %in% names(dnames)) {
      if (all(is.na(object))) return(TRUE)
      
      sim_dim <- which(names(dnames) == "Sim")
      dims <- dim(object)
      
      # Permute Sim dimension to the first position
      perm <- c(sim_dim, setdiff(seq_along(dims), sim_dim))
      arr_perm <- aperm(object, perm)
      
      # Flatten remaining dimensions
      mat <- matrix(arr_perm, nrow = dims[sim_dim])
      
      # Compare each column to the first row
      ref <- mat[1, , drop=TRUE]
      if (any(mat != matrix(ref, nrow=nrow(mat), ncol=ncol(mat), byrow=TRUE))) {
        return(FALSE)
      }
    }
  }
  
  TRUE
}







