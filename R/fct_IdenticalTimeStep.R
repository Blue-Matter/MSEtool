
IdenticalYears <- function(array, logical=TRUE) {
  if (!is.array(array))
    return(TRUE)
  
  unique <- UniqueYears(array)
  
  if (!logical) 
    return(unique)
  
  if (is.null(unique))
    return(TRUE)
  
  length(unique)==1
}

UniqueYears <- function(array) {
  if (!is.array(array))
    cli::cli_abort('`array` is not an array')
  
  dnames <- dimnames(array)
  TSInd <- which(names(dnames) == 'Year')
  if (!length(TSInd))
    return(NULL)
  
  dd <- dim(array)    
  if (dd[TSInd]==1)
    return(1)
  
  Ref <- abind::asub(array, 1, TSInd)
  nTS <- dimnames(array)[[TSInd]] |> length()
  logVec <- rep(TRUE, nTS)

  for (i in seq_along(logVec)[-1]) {
    Comp1 <- abind::asub(array, i, TSInd)
    Comp2 <- abind::asub(array, i-1, TSInd)
    logVec[i] <- !any(round(Comp1, 4) != round(Comp2, 4))
  }
  
  which(logVec)
}
