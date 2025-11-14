
# fills all time step values
ExtendYears <- function(array, Years, default=NULL) {
  if (is.null(Years))
    return(array)
  # array1 <<- array
  # Years1 <<- Years
  # 
  # Years <- Years1
  # array = Array1
  
  ind <- which(names(dimnames(array))=='Year')
  if (length(ind)<1)
    return(array)
  
  d <- dim(array)
  dnames <- dimnames(array)
  
  ArrayTS <- dnames[[ind]] |> as.numeric()
  
  # Years <- Years[Years>=ArrayTS]
  # Years <- Years[!is.na(Years)]
  
  if (prod(Years %in% ArrayTS))
    return(array)
  
  namematch <- match(ArrayTS, Years)
  adddim <- length(Years) - length(namematch)
  d[ind] <- length(Years)
  dnames$Year <- Years
  OutArray <- array(NA, dim=d, dimnames=dnames)
  
  TSmatch <- which(ArrayTS %in% Years)
  if (!length(TSmatch))
    return(array)
  YearsKeep <- Years[TSmatch]
  KeepValues <- abind::asub(array, TSmatch, ind, drop=FALSE)
  abind::afill(OutArray) <- KeepValues
  
  TSfill <- which(!Years %in% ArrayTS)
  
  if (length(TSfill)<1)
    return(OutArray)
  
  YearsFill <- Years[TSfill]
  
  nTSFill <- dim(OutArray)[ind] - dim(array)[ind]
  d2 <- d
  d2[ind] <- nTSFill
  dnames[[ind]] <- YearsFill
  FillArray <- array(NA, dim=d2, dimnames = dnames)
  
  if (length(ArrayTS)==1) {
    vals <- abind::adrop(abind::asub(array, 1,ind, drop=FALSE), ind)
    if (inherits(vals, 'numeric')) {
      vals <- array(vals, length(vals), dimnames = list(Sim=1:length(vals)))
      dimnames(vals) <- dnames[-ind]
    }
    
    vals <- replicate(nTSFill, vals) 
    if (!inherits(vals, 'array')) {
      
      outDim <- rep(1, length(dnames))
      outDim[ind] <- length(vals)
      vals <- array(vals, dim=outDim)
      dimnames(vals) <- dnames
    }
    
    dnames2 <- vals |> dimnames() |> names()
    tsInd <- which(dnames2=='Year')
    
    if (length(tsInd)<1) 
      tsInd <- which(nchar(dnames2)==0)  
    
    dimnames(vals)[[tsInd]] <- YearsFill
    names(dimnames(vals))[tsInd] <- 'Year'
    vals <- aperm(vals, names(dnames))
    abind::afill(OutArray) <- vals
    return(OutArray)
  }
  
  TimeBlocks <- split(YearsFill, YearsFill - seq_along(YearsFill)) |> unname()
  
  for (i in seq_along(TimeBlocks)) {
    Block <- TimeBlocks[[i]]
    
    ValueIndForward <- which(ArrayTS <=min(Block)) 
    ValueIndBack  <- which(ArrayTS >=min(Block)) 
    
    # forward 
    if (length(ValueIndForward)) {
      ValueInd <- max(ValueIndForward)
    } else if ((length(ValueIndBack))) {
      ValueInd <- min(ValueIndBack)
    }
    
    if (!is.finite(ValueInd))
      cli::cli_abort("Non-finite value", .internal=TRUE)
    
    ValueYear <- abind::asub(array, ValueInd, ind, drop=FALSE)
    if (!is.null(default))
      ValueYear[] <- default
    
    dd <- dim(ValueYear)
    dd[ind] <- length(Block)
    
    ValueExpanded <- array(NA, dim = dd)
    dnames <- dimnames(ValueYear)
    dnames[[ind]] <- Block
    dimnames(ValueExpanded) <- dnames
    
    for (j in seq_along(Block)) {
      dimnames(ValueYear)[[ind]] <- Block[j]
      abind::afill(ValueExpanded) <- ValueYear  
    }
    
    abind::afill(FillArray) <- ValueExpanded
    
  }
  
  abind::afill(OutArray) <- FillArray
  OutArray
}
