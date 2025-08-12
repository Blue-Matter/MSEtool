
#' Perform Operations on Two Arrays
#' 
#' Multiply, divide, subtract, or add two arrays together. 
#' The arrays must have the same named dimensions, but do not need to have
#' the same length for each dimension
#' 
#' @param array1 A 2D, 3D, or 4D array
#' @param array2 A 2D, 3D, or 4D array
#' @name ArrayMultiply
NULL


CheckArrays <- function(ArrayList) {
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  d1 <- dim(array1)
  d2 <- dim(array2)
  nm1 <- array1 |> dimnames() |> names()
  nm2 <- array2 |> dimnames() |> names()
  
  if (length(d1)!=length(d2))
    cli::cli_abort(c(' `array1` and `array2` must have same number of dimensions',
                   'x'='`array1` has {.val {length(d1)}} dimensions  while `array2` has {.val {length(d2)}} dimensions '))
  
  if (!(all(nm1==nm2)))
    cli::cli_abort(c('`array1` and `array2` must have same dimension names',
                     'x'='Dimension names are {.val {nm1}} for `array1` and {.val {nm2}} for `array2`'))
}

RepeatArrayDim <- function(array, dim, n) {
  ArrayDim <- dim(array)
  nDim <- length(ArrayDim)
  OutDim <- ArrayDim
  OutDim[dim] <- OutDim[dim] * n

  perm <- (1:nDim)[-dim]
  perm <- c(perm, dim)
  OutArray <- aperm(array, perm)
  OutArray <- rep(OutArray, n)
  dim(OutArray) <- OutDim[perm]
  perm <- append((1:(nDim - 1)), nDim, dim - 1)
  if (nDim==1) {
    return(OutArray)
  } 
  aperm(OutArray, perm) 
}

ExpandTS <- function(Array, Dims, TimeSteps) {
  ind <- which(names(dimnames(Array))=='TimeStep')
  if (length(ind)<1) 
    return(Array)
  
  ArrayTS <- dimnames(Array)[[ind]] |> as.numeric()
  
  OutArray <- array(NA, dim=Dims) |>
    AddDimNames(names(dimnames(Array)), TimeSteps=TimeSteps)
  for (i in seq_along(TimeSteps)) {
    j <- which(ArrayTS <= TimeSteps[i])
    if (any(!is.finite(j)) || length(j)<1)
      next()
    j <- max(j)
    OutArray[,,i] <- Array[,,j]
  }
  OutArray
}

MatchArrayTimeSteps <- function(ArrayList) {
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  
  nm1 <- names(dimnames(array1))
  ind <- which(nm1=='TimeStep')
  
  nm2 <- names(dimnames(array2))
  ind2 <- which(nm2=='TimeStep')
  
  if (length(ind)<1)
    return(list(array1, array2))
  
  TSarray1 <- dimnames(array1)[[ind]] |> as.numeric()
  TSarray2 <- dimnames(array2)[[ind2]] |> as.numeric()
  
  TSout <- c(TSarray1, TSarray2) |> unique() |> sort()
  
  if (all(TSout %in% TSarray1) & all(TSout %in% TSarray2)) 
    return(list(array1, array2))
  
  d1 <- dim(array1)
  d2 <- dim(array2)
  d1[ind] <- length(TSout)
  d2[ind2] <- length(TSout)
  
  if (!all(TSout %in% TSarray1) & length(TSarray1)>1) 
    array1 <- ExpandTS(Array=array1, Dims=d1, TimeSteps=TSout)
  if (!all(TSout %in% TSarray2)& length(TSarray2)>1) 
    array2 <- ExpandTS(Array=array2, Dims=d2, TimeSteps=TSout)
  
  list(array1=array1, array2=array2)
}


ExpandArrays <- function(ArrayList, array2=NULL) {
  if (inherits(ArrayList, 'array') & !is.null(array2))
    ArrayList <- list(ArrayList, array2)
   
  ArrayList <- MatchArrayTimeSteps(ArrayList) # match time-steps
  
  AllDims <- rbind(dim(ArrayList[[1]]), dim(ArrayList[[2]]))
  MatchDims <- AllDims[1,] == AllDims[2,]
  if (all(MatchDims)) {
    return(ArrayList)
  }
  DimNames1 <- dimnames(ArrayList[[1]])
  DimNames2 <- dimnames(ArrayList[[2]])
  Names <- names(DimNames1)
  
  if (!any(MatchDims)) {
    if (sum(apply(AllDims == 1, 1, prod))>2)
      cli::cli_abort(c('Array dimensions must be length {.val 1} or equal lengths in both arrays.',
                     'x'='Dimensions {.val {Names}} are length {.val {AllDims[1,]}} for `array1` and length {.val {AllDims[2,]}} for `array2`. '))
  }
  
  OutDims <- apply(AllDims, 2, max)
  for (a in 1:2) {
    for (dim in 1:ncol(AllDims)) {
      if (AllDims[a,dim] != OutDims[dim])
        ArrayList[[a]] <- RepeatArrayDim(ArrayList[[a]], dim, OutDims[dim])
    }
  }
  
  # set dimnames
  l <- list()
  
  for (i in seq_along(DimNames1)) {
    ind <- which.max(AllDims[,i])
    nm <- list(DimNames1[[i]], DimNames2[[i]]) 
    l[[i]] <- nm[[ind]]
  }
  names(l) <- Names
  dimnames(ArrayList[[1]]) <- l
  dimnames(ArrayList[[2]]) <- l
  ArrayList
} 

ArrayOperation <- function(array1, array2, operation=`*`) {
  if (inherits(array1, 'list') && length(array1)==2) {
    ArrayList <- array1 
  } else {
    ArrayList <- list(array1, array2)
  }
  CheckArrays(ArrayList)
  
  ArrayList <- ArrayList |> 
    ExpandArrays() 
  
  operation(ArrayList[[1]], ArrayList[[2]])
}

#' @rdname ArrayMultiply
#' @export
ArrayMultiply <- function(array1, array2=NULL) {
  ArrayOperation(array1, array2)
}

#' @rdname ArrayMultiply
#' @export
ArrayAdd <- function(array1, array2=NULL) {
  ArrayOperation(array1, array2, `+`)
}

#' @rdname ArrayMultiply
#' @export
ArrayDivide <- function(array1, array2=NULL) {
  ArrayOperation(array1, array2, `/`)
}

#' @rdname ArrayMultiply
#' @export
ArraySubtract <- function(array1, array2=NULL) {
  ArrayOperation(array1, array2, `-`)
}

#' @rdname ArrayMultiply
#' @export
`ArrayFill<-` <- function(object, value) {
  if (is.null(object)) {
    object <- value
    return(object)
  }
  
  objectDims <- dimnames(object)
  valueDims <- dimnames(value)
  
  if (length(objectDims) != length(valueDims))
    stop('`object` and `value` must have same dimension names')
  
  chk <- rep(TRUE, length(objectDims))
  for (i in seq_along(objectDims)) {
    chk[i] <- all(valueDims[[i]] %in% objectDims[[i]])
  }
 
   if (!all(chk)) {
    # Update object size if required
    ind <- which(chk!=TRUE)
    l <- objectDims 
    for (i in seq_along(objectDims)) {
      dd <- c(objectDims[[i]],  valueDims[[i]]) |> unique() 
      d1 <- suppressWarnings(as.numeric(dd))
      if (any(is.na(d1))) {
        d1 <- sort(dd)
      } else {
        d1 <- sort(d1)
      }
      l[[i]] <- d1
    }
    
    match <- !valueDims[[ind]] %in% objectDims[[ind]]
    if (any(match)) {
      value <- abind::asub(value, match, ind, drop=FALSE)
    }
    
    object <- abind::abind(object, value, along=ind)  
    
    dimnames(object) <- l
  } else {
    abind::afill(object) <- value
  }
  object

}

ArraySubsetTimeStep <- function(object, TimeSteps=NULL, AddPast=TRUE) {
  if (is.null(TimeSteps))
    return(object)
  
  TimeSteps <- as.numeric(TimeSteps)
  
  DN <- dimnames(object)
  if (is.null(DN))
    return(object)
  
  DN$TimeStep <- as.numeric(DN$TimeStep)
  TSind <- which(names(DN) == 'TimeStep')
  if (length(TSind)==0)
    cli::cli_abort("`TimeStep` dimension not found in this array", .internal=TRUE)
  
  TSexist <- TimeSteps[TimeSteps %in% DN$TimeStep]
  TSimpute <- TimeSteps[!TimeSteps %in% DN$TimeStep]
  
  if (!AddPast) {
    TSimpute <- TSimpute[!TSimpute<max(TSexist)]
  }
  
  if (length(TSimpute)) {
    # need to impute some values
    matchTS <- rep(NA, length(TSimpute))
    for (i in seq_along(TSimpute)) {
      
      FutureImpute <- which(DN$TimeStep < TSimpute[i]) 
      if (length(FutureImpute)>0)
        matchTS[i] <- DN$TimeStep[min(FutureImpute)]
      
      PastImpute <- which(DN$TimeStep > TSimpute[i]) 
      if (length(PastImpute)>0)
        matchTS[i] <- DN$TimeStep[min(PastImpute)]
    }
    
    TimeStepsMod <- c(TSexist, matchTS) |> as.character() 
    array <- abind::asub(object, TimeStepsMod, TSind, drop=FALSE)
    dimnames(array)$TimeStep <- TimeSteps
    return(array)
  }
  
  abind::asub(object, (DN[[TSind]] %in% TimeSteps), TSind, drop=FALSE)
}

ArraySubsetSim <- function(object, Sims=NULL, drop=FALSE) {
  if (is.null(Sims))
    return(object)
  
  Sims <- Sims |> as.numeric() 
  
  DN <- dimnames(object)
  DN$Sim <- as.numeric(DN$Sim)
  TSind <- which(names(DN) == 'Sim')
  
  
  if (length(TSind)==0)
    cli::cli_abort("`Sim` dimension not found in this array", .internal=TRUE)
  
  if (any(Sims > max(DN$Sim))) {
    TSexist <- Sims[Sims %in% DN$Sim]
    TSimpute <- Sims[!Sims %in% DN$Sim]
    if (length(TSimpute)) {
      matchTS <- rep(NA, length(TSimpute))
      for (i in seq_along(TSimpute)) {
        matchTS[i] <- DN$Sim[DN$Sim < TSimpute[i]] |> max()
      }
    }
    TimeStepsMod <- c(TSexist, matchTS)
    array <- abind::asub(object, TimeStepsMod, TSind, drop=drop)

    if (!is.null( dimnames(array)))
      dimnames(array)$`Sim` <- Sims
    return(array)
  } 
  if (drop) {
    out <- abind::adrop(abind::asub(object, (DN[[TSind]] %in% Sims), TSind, drop=FALSE), TSind)
    if (is.null(dim(out))) {
      out <- array(out, dim=length(out), dimnames=list(TimeStep=names(out)))
    }
    return(out)  
  } else {
    out <- abind::asub(object, (DN[[TSind]] %in% Sims), TSind, drop=FALSE)  
    return(out)
  }
  
}

# ----- Array Expand ----

ArrayExpand <- function(Array, nSim, nAges, TimeSteps, AgeOpt=3) {
  
  out <- Array |>
    ExpandSims(nSim) |>
    ExpandAges(nAges, AgeOpt) |>
    ExpandTimeSteps(TimeSteps)
  out
}

# AgeOpt = 1 fills all additional age classes with 1e-16
# AgeOpt = 2 fills all additional age classes with 1
# AgeOpt = 3 fills all additional age classes with same as last age 
ExpandAges <- function(Array, nAges, AgeOpt=3) {
  ind <- which(names(dimnames(Array))=='Age')
  if (length(ind)<1)
    return(Array)
  
  d <- dim(Array)
  dnames <- dimnames(Array)
  existing <- as.numeric(dnames[[ind]])
  
  if (length(existing)==nAges)
    return(Array)
  
  AddDim <- nAges - length(existing)
  
  OutDim <- d
  OutDim[ind] <- AddDim
  
  fillvalue <- tiny/2
  if (AgeOpt==2)
    fillvalue <- 1
  if (AgeOpt==3) {
    fillvalue <- abind::asub(Array, 1, ind)
    
  }
    
  
  existingNames <- dnames[[ind]]
  Last <- existingNames[length(existingNames)] |> as.numeric()
  AddNames <- seq(Last+1, length.out=AddDim)
  AddDimNames <- dnames
  AddDimNames[[ind]] <- AddNames
  empty <- array(fillvalue, dim=OutDim, 
                 dimnames=AddDimNames)
  
  abind::abind(Array, empty, along=ind,
               use.dnns=TRUE)
  
}

# replicates to `nsim`
ExpandSims <- function(Array, nSim) {
  ind <- which(names(dimnames(Array))=='Sim')
  if (length(ind)<1)
    return(Array)
  
  d <- dim(Array)
  dnames <- dimnames(Array)
  existing <- as.numeric(dnames[[ind]])
  if (length(existing)==nSim)
    return(Array)
  
  if (length(existing)>nSim) {
    list <- as.list(d)
    list[[ind]] <- 1:nSim
    return(abind::asub(Array, list, drop=FALSE))
  }
    
    
  
  if (length(existing)>1)
    cli::cli_abort('`Sim` dimension must be either length `nSim` or length 1', .internal=TRUE)
  
  
  AddDim <- nSim - length(existing)
 
  OutDim <- d
  OutDim[ind] <- AddDim
  OutDimNames <- dnames
  OutDimNames[[ind]] <- 1:nSim
  
  out <- RepeatArrayDim(Array, ind, nSim)
  dimnames(out) <- OutDimNames
  out
  
}


# fills all time step values
ExpandTimeSteps <- function(Array, TimeSteps, default=NULL) {
  # Array1 <<- Array
  # TimeSteps1 <<- TimeSteps
  # 
  # TimeSteps <- TimeSteps1
  # Array = Array1
  
  ind <- which(names(dimnames(Array))=='TimeStep')
  if (length(ind)<1)
    return(Array)
  
  d <- dim(Array)
  dnames <- dimnames(Array)
  
  ArrayTS <- dnames[[ind]] |> as.numeric()
  
  # TimeSteps <- TimeSteps[TimeSteps>=ArrayTS]
  # TimeSteps <- TimeSteps[!is.na(TimeSteps)]
  
  if (prod(TimeSteps %in% ArrayTS))
    return(Array)
  
  namematch <- match(ArrayTS, TimeSteps)
  adddim <- length(TimeSteps) - length(namematch)
  d[ind] <- length(TimeSteps)
  dnames$TimeStep <- TimeSteps
  OutArray <- array(NA, dim=d, dimnames=dnames)

  TSmatch <- which(ArrayTS %in% TimeSteps)
  TimeStepsKeep <- TimeSteps[TSmatch]
  KeepValues <- abind::asub(Array, TSmatch, ind, drop=FALSE)
  abind::afill(OutArray) <- KeepValues
  
  TSfill <- which(!TimeSteps %in% ArrayTS)
  
  if (length(TSfill)<1)
    return(OutArray)
  
  TimeStepsFill <- TimeSteps[TSfill]
  
  nTSFill <- dim(OutArray)[ind] - dim(Array)[ind]
  d2 <- d
  d2[ind] <- nTSFill
  dnames[[ind]] <- TimeStepsFill
  FillArray <- array(NA, dim=d2, dimnames = dnames)
  
  
  if (length(ArrayTS)==1) {
    vals <- abind::adrop(abind::asub(Array, 1,ind, drop=FALSE), ind)
    if (inherits(vals, 'numeric'))
      vals <- array(vals, length(vals), dimnames = list(Sim=1:length(vals)))
    vals <- replicate(nTSFill, vals) 
    if (!inherits(vals, 'array')) {
      vals <- array(vals, dim=c(1, length(vals)))
      dimnames(vals) <- dnames
    }
    
    dnames2 <- vals |> dimnames() |> names()
    tsInd <- which(dnames2=='TimeStep')
    
    if (length(tsInd)<1) 
      tsInd <- which(nchar(dnames2)==0)  
    
    dimnames(vals)[[tsInd]] <- TimeStepsFill
    names(dimnames(vals))[tsInd] <- 'TimeStep'
    vals <- aperm(vals, names(dnames))
    abind::afill(OutArray) <- vals
    return(OutArray)
    
  }
  
  for (i in TimeStepsFill) {
    if (all(i < ArrayTS))
      next()
    ValueInd <- which(ArrayTS <=i) |> max()
    if (!is.finite(ValueInd))
      stop()
    
    TSexpanded <- abind::asub(Array, ValueInd, ind, drop=FALSE)
    if (!is.null(default) && i > max(ArrayTS) )
       TSexpanded[] <- default
    
    dd <- dim(TSexpanded) |> length()
    
    if (dd==0) {
      stop("need to fix!")
      # dnames <- dimnames(Array)
      # d1 <- dim(Array)
      # d1[ind] <- length(TimeSteps[TSind])
      # TSexpanded <- array(TSexpanded, dim=d1)
      # dnames[[ind]] <- TimeSteps[TSind]
      # dimnames(TSexpanded) <- dnames
    } else {
      dimnames(TSexpanded)[[ind]] <- i
    }
    
    TSexpanded <- TSexpanded |>
      aperm(names(dimnames(Array)))
    
    abind::afill(FillArray) <- TSexpanded
  }
  
  abind::afill(OutArray) <- FillArray
  OutArray
}
