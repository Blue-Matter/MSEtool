# AgeOpt = 1 fills all additional age classes with 1e-16 - NOT WORKING
# AgeOpt = 2 fills all additional age classes with 1 - NOT WORKING
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
  
  OutList <- replicate(nAges, array, simplify = FALSE)
  OutArray <- abind::abind(OutList, along=ind)

  existingNames <- dnames[[ind]]
  Last <- existingNames[length(existingNames)] |> as.numeric()
  AddNames <- c(Last, seq(Last+1, length.out=AddDim))
  AddDimNames <- dnames
  AddDimNames[[ind]] <- AddNames
  
  dimnames(OutArray) <- AddDimNames
  
  OutArray

}