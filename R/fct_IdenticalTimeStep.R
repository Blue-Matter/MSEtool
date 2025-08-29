
IdenticalTimeSteps <- function(array, logical=TRUE) {
  if (!is.array(array))
    return(TRUE)
  
  unique <- UniqueTimeSteps(array)
  
  if (!logical) 
    return(unique)
  
  if (is.null(unique))
    return(TRUE)
  
  length(unique)==1
}

UniqueTimeSteps <- function(array) {
  if (!is.array(array))
    cli::cli_abort('`array` is not an array')
  
  dnames <- dimnames(array)
  TSInd <- which(names(dnames) == 'TimeStep')
  if (!length(TSInd))
    return(NULL)
  
  dd <- dim(array)    
  if (dd[TSInd]==1)
    return(1)
  
  meanTS <- apply(array, TSInd, mean)
  match(unique(meanTS), meanTS)
}
