
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
  aperm(OutArray, perm) 
}

ExpandTS <- function(Array, Dims, TimeSteps) {
  ind <- which(names(dimnames(Array))=='Time Step')
  if (length(ind)<1) 
    return(Array)
  
  ArrayTS <- dimnames(Array)[[ind]] |> as.numeric()
  
  OutArray <- array(NA, dim=Dims) |>
    AddDimNames(names(dimnames(Array)), TimeSteps=TimeSteps)
  for (i in seq_along(TimeSteps)) {
    j <- which(ArrayTS <= TimeSteps[i]) |> max()
    OutArray[,,i] <- Array[,,j]
  }
  OutArray
}

MatchArrayTimeSteps <- function(ArrayList) {
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  
  nm1 <- names(dimnames(array1))
  ind <- which(nm1=='Time Step')
  
  nm2 <- names(dimnames(array2))
  ind2 <- which(nm2=='Time Step')
  
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
    nm <- list(DimNames1[[i]], DimNames2[[i]])
    ind <- purrr::map(nm, length) |> unlist() |> which.max()
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
  
  
  # Array <- object
  # FillValue <- value
  # 
  # if (is.null(Array))
  #   return(value)
  # 
  # 
  # ArrayList <- list(Array, FillValue)
  # CheckArrays(ArrayList)
  # ArrayList <- ArrayList |> ExpandArrays()
  # 
  # Array <- ArrayList[[1]]
  # FillValue <- ArrayList[[2]]
  # Array[] <- FillValue
  # Array
}
