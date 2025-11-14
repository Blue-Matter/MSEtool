# AgeOpt = 1 fills all additional age classes with 1e-16
# AgeOpt = 2 fills all additional age classes with 1
# AgeOpt = 3 fills all additional age classes with same as last age 

ExtendAges <- function(array, nAges, AgeOpt=3) {
  if (is.null(nAges))
    return(array)
  ind <- which(names(dimnames(array))=='Age')
  if (length(ind)<1)
    return(array)
  
  d <- dim(array)
  dnames <- dimnames(array)
  existing <- as.numeric(dnames[[ind]])
  
  if (length(existing)==nAges)
    return(array)
  
  AddDim <- nAges - length(existing)
  
  OutDim <- d
  OutDim[ind] <- AddDim
  
  fillvalue <- tiny/2
  if (AgeOpt==2)
    fillvalue <- 1
  if (AgeOpt==3) {
    fillvalue <- abind::asub(array, 1, ind)
  }
  existingNames <- dnames[[ind]]
  Last <- existingNames[length(existingNames)] |> as.numeric()
  AddNames <- seq(Last+1, length.out=AddDim)
  AddDimNames <- dnames
  AddDimNames[[ind]] <- AddNames
  empty <- array(fillvalue, dim=OutDim, 
                 dimnames=AddDimNames)
  
  abind::abind(array, empty, along=ind,
               use.dnns=TRUE)
}