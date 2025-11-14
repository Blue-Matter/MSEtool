ExtendSims <- function(array, nSim) {
  if (is.null(nSim))
    return(array)
  
  ind <- which(names(dimnames(array))=='Sim')
  if (length(ind)<1)
    return(array)
  
  d <- dim(array)
  dnames <- dimnames(array)
  existing <- as.numeric(dnames[[ind]])
  if (length(existing)==nSim)
    return(array)
  
  if (length(existing)>nSim) {
    return(array)
    # return(abind::asub(array, ind, 1:nSim, drop=FALSE))
  }
  
  if (length(existing)>1) 
    cli::cli_abort(c('The `Sim` dimension must be either length `nSim` ({.val {nSim}}) or length 1',
                     'x' ='The `Sim` dimension of this array has length {.val {d[ind]}}')
    )
  
  
  AddDim <- nSim - length(existing)
  
  OutDim <- d
  OutDim[ind] <- AddDim
  OutDimNames <- dnames
  OutDimNames[[ind]] <- 1:nSim
  
  out <- RepeatArrayDim(array, ind, nSim)
  dimnames(out) <- OutDimNames
  out
  
}