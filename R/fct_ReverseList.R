# https://stackoverflow.com/questions/15263146/revert-list-structure
ReverseList <- function(ls) {
  if (all(lapply(ls, is.null) |> unlist()))
    return(ls)
  x <- lapply(ls, `[`, names(ls[[1]]))
  apply(do.call(rbind, x), 2, as.list) 
}
