
IdenticalTimeSteps <- function(array) {
  if (!is.array(array))
    return(TRUE)
  
  dnames <- dimnames(array)
  TSInd <- which(names(dnames) == 'TimeStep')
  
  if (!length(TSInd))
    return(TRUE)

  dd <- dim(array)    
  if (dd[TSInd]==1)
    return(TRUE)
  meanTS <- apply(array, TSInd, mean)
  
  meanmeanTS <- mean(meanTS)
  if (meanmeanTS<=0)
    return(TRUE)
  
  sum(meanTS/meanmeanTS - 1) < 1E-6
}
