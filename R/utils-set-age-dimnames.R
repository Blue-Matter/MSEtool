SetAgeDimnames <- function(array, Ages) {
  
  # Recall for S4
  if (isS4(array)) {
    for (sl in slotNames(array)) {
      slot(array, sl) <- Recall(slot(array, sl), Ages)
    }
    
    return(array)
  }
  
  # Recall for list
  if (is.list(array)) {
    for (i in seq_along(array)) {
      tmp <- Recall(array[[i]], Ages)
      if (!is.null(tmp)) {
        array[[i]] <- tmp
      }
    }
    return(array)
  }
  
  dnames <- dimnames(array)
  ind <- which(names(dnames) =='Age')
  if (!length(ind))
    return(array)
  
  dd <- dim(array)
  AgeClasses <- CalcAgeClasses(Ages)
  dnames[[ind]] <- AgeClasses[1:dd[ind]]
  dimnames(array) <- dnames
  array
}