
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

ExpandTS <- function(array, Dims, Years) {
  ind <- which(names(dimnames(array))=='Year')
  if (length(ind)<1) 
    return(array)
  
  ArrayTS <- dimnames(array)[[ind]] |> as.numeric()
  
  OutArray <- array(NA, dim=Dims) |>
    AddDimNames(names(dimnames(array)), Years=Years)
  for (i in seq_along(Years)) {
    j <- which(ArrayTS <= Years[i])
    if (any(!is.finite(j)) || length(j)<1)
      next()
    j <- max(j)
    
    dimnames(array)[[ind]][j] <-  Years[i]
    
    ArrayFill(OutArray) <- abind::asub(array, j, ind, drop=FALSE)
    # dimnames(Array[[,,j]])
    # dimnames(OutArray)
    # OutArray[,,i] <- Array[,,j]
  }
  OutArray
}

MatchArrayYears <- function(ArrayList) {
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  
  nm1 <- names(dimnames(array1))
  ind <- which(nm1=='Year')
  
  nm2 <- names(dimnames(array2))
  ind2 <- which(nm2=='Year')
  
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
    array1 <- ExpandTS(array=array1, Dims=d1, Years=TSout)
  if (!all(TSout %in% TSarray2)& length(TSarray2)>1) 
    array2 <- ExpandTS(array=array2, Dims=d2, Years=TSout)
  
  list(array1=array1, array2=array2)
}


ExpandArrays <- function(ArrayList, array2=NULL) {
  if (inherits(ArrayList, 'array') & !is.null(array2))
    ArrayList <- list(ArrayList, array2)
   
  ArrayList <- MatchArrayYears(ArrayList) # match time-steps
  
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

ArraySubsetYear <- function(object, Years=NULL, AddPast=TRUE) {
  
  
  if (is.null(Years))
    return(object)
  
  Years <- as.numeric(Years)
  
  DN <- dimnames(object)
  if (is.null(DN))
    return(object)
  
  DN$Year <- as.numeric(DN$Year)
  TSind <- which(names(DN) == 'Year')
  if (length(TSind)==0)
    cli::cli_abort("`Year` dimension not found in this array", .internal=TRUE)
  
  TSexist <- Years[Years %in% DN$Year]
  TSimpute <- Years[!Years %in% DN$Year]
  
  if (!AddPast & length(TSexist)) {
    TSimpute <- TSimpute[!TSimpute<min(DN$Year)]
  }
  
  if (length(TSimpute)) {
    # need to impute some values
    matchTS <- rep(NA, length(TSimpute))
    for (i in seq_along(TSimpute)) {
      
      FutureImpute <- which(DN$Year < TSimpute[i]) 
      if (length(FutureImpute)>0)
        matchTS[i] <- DN$Year[max(FutureImpute)]
      
      PastImpute <- which(DN$Year >TSimpute[i]) 
      if (length(PastImpute)>0)
        matchTS[i] <- DN$Year[min(PastImpute)]
    }
    
    YearsMod <- c(TSexist, matchTS) |> as.character() 
    array <- abind::asub(object, YearsMod, TSind, drop=FALSE)
    dimnames(array)$Year <- Years
    return(array)
  }
  
  abind::asub(object, (DN[[TSind]] %in% Years), TSind, drop=FALSE)
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
    YearsMod <- c(TSexist, matchTS)
    array <- abind::asub(object, YearsMod, TSind, drop=drop)

    if (!is.null( dimnames(array)))
      dimnames(array)$`Sim` <- Sims
    return(array)
  } 
  if (drop) {
    out <- abind::adrop(abind::asub(object, (DN[[TSind]] %in% Sims), TSind, drop=FALSE), TSind)
    if (is.null(dim(out))) {
      nms <- names(out)
      numericNames <- suppressWarnings(as.numeric(nms))
      if (is.null(nms)) {
        return(out)
      } else  if (any(is.na(numericNames))) {
        out <- array(out, dim=length(out), dimnames=list(Fleet=names(out)))
      } else {
        out <- array(out, dim=length(out), dimnames=list(Year=names(out)))
      }
    }
    return(out)  
  } else {
    out <- abind::asub(object, (DN[[TSind]] %in% Sims), TSind, drop=FALSE)  
    return(out)
  }
  
}

ArraySubsetAge <- function(object, Ages=NULL, drop=FALSE) {
  if (is.null(Ages))
    return(object)
  
  Ages <- Ages |> as.numeric() 
  
  DN <- dimnames(object)
  DN$Age <- as.numeric(DN$Age)
  AgeInd <- which(names(DN) == 'Age')
  
  if (length(AgeInd)==0)
    cli::cli_abort("`Age` dimension not found in this array", .internal=TRUE)
  
  if (any(Ages > max(DN$Age))) {
    cli::cli_abort("`Ages` greater than ages in this array", .internal=TRUE)
  } 
  
  abind::asub(object, (DN[[AgeInd]] %in% Ages), AgeInd, drop=FALSE) 
}

# ----- Array Expand ----

#' @export
ArrayExpand <- function(array, nSim, nAges, Years, AgeOpt=3, debug=FALSE) {
  
  if (debug)
    print(class(array))
  
  if (!is.array(array)) {
    if (isS4(array)) {
      if (inherits(array, 'data'))
        return(array)
      slots <- slotNames(array)
      
      for (sl in slots) {
        if (debug)
          print(sl)
        slot(array, sl) <- Recall(slot(array, sl), nSim, nAges, Years, AgeOpt, debug)
      }
      return(array)
    }
    if (is.list(array)) {
      if (length(array)) {
        for (i in 1:length(array)) {
          temp <- Recall(array[[i]], nSim, nAges, Years, AgeOpt, debug)
          if (!is.null(temp))
            array[[i]] <- temp 
        }
        return(array)
      }
    }

  }
  
  array |>
    ExpandSims(nSim) |>
    ExpandAges(nAges, AgeOpt) |>
    ExpandYears(Years)
  
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
ExpandSims <- function(array, nSim) {
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


# fills all time step values
ExpandYears <- function(array, Years, default=NULL) {
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




#' @export
ArrayReduceDims <- function(array, 
                            IncSim=TRUE, 
                            IncAge=FALSE, 
                            IncYear=TRUE,
                            debug=FALSE, 
                            silent=FALSE, 
                            id=NULL) {
  
  if (debug)
    print(class(array))
  
  if (!IncSim & !IncYear & !IncAge) 
    return(array)
  
  if (!length(array))
    return(array)
  
  if (!is.array(array)) {
    if (isS4(array)) {
      if (!silent) {
        if (is.null(id)) {
          id <- cli::cli_progress_bar('Reducing dimensions to minimum size', type='tasks')
        } else {
          cli::cli_progress_update(id=id) 
        }
      }
      
      if (inherits(array, 'data'))
        return(array)
      slots <- slotNames(array)
      for (sl in slots) {
        slot(array, sl) <- Recall(slot(array, sl), IncSim, IncAge, IncYear, debug, silent, id)
      }
      return(array)
    }
    if (is.list(array)) {
      if (length(array)) {
        for (i in 1:length(array)) {
          temp <- Recall(array[[i]], IncSim, IncAge, IncYear, debug, silent, id)
          if (!is.null(temp))
            array[[i]] <- temp 
        }
        return(array)
      }
    }
  }
  
  dnames <- array |> dimnames() |> names()
  
  indSim <- which(dnames=='Sim')
  indAge <- which(dnames=='Age')
  indYear <- which(dnames=='Year')
  
  incSim <- length(indSim)
  incAge <- length(indAge)
  incYear <- length(indYear)
  
  if (!IncSim)
    incSim <- 0
  
  if (!IncAge)
    incAge <- 0
  
  if (!IncYear)
    incYear <- 0
  
  if (incSim & incYear & incAge) {
    idenSim <- IdenticalSims(array) 
    idenTime <- IdenticalYears(array)
    idenAge <- IdenticalAge(array)
    
    if (idenSim & idenTime & idenAge) 
      return(abind::asub(array, list(1,1,1), c(indSim, indAge, indYear), drop=FALSE))
    
    if (!idenSim & idenTime & idenAge) 
      return(abind::asub(array, list(1,1), c(indAge, indYear), drop=FALSE))
    
    if (idenSim & !idenTime & idenAge) {
      return(abind::asub(array, list(1,1,UniqueYears(array)), c(indSim, indAge, indYear), drop=FALSE))
    }
    
    if (!idenSim & !idenTime & idenAge) 
      return(abind::asub(array, list(1,UniqueYears(array)), c(indAge, idenTime), drop=FALSE))
    
    if (idenSim & idenTime & !idenAge)
      return(abind::asub(array, list(1,1), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & idenTime & !idenAge) 
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (idenSim & !idenTime & !idenAge) 
      return(abind::asub(array, list(1, UniqueYears(array)), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & !idenTime & !idenAge) {
      return(abind::asub(array, list(UniqueYears(array)), c(indYear), drop=FALSE))
    }
    
  }
  
  if (!incSim & incYear & incAge) {
    idenTime <- IdenticalYears(array)
    idenAge <- IdenticalAge(array)
    
    if (idenTime & idenAge) 
      return(abind::asub(array, list(1,1), c(indAge, indYear), drop=FALSE))
    if (!idenTime & idenAge) 
      return(abind::asub(array, list(1, UniqueYears(array)), c(indAge, indYear), drop=FALSE))
    
    if (idenTime & !idenAge)
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (!idenTime & !idenAge) 
      return(abind::asub(array, list(UniqueYears(array)), c(indYear), drop=FALSE))
  }
  
  if (incSim & !incYear & incAge) {
    idenSim <- IdenticalSims(array) 
    idenAge <- IdenticalAge(array)
    
    if (idenSim  & idenAge) 
      return(abind::asub(array, list(1,1), c(indSim, indAge), drop=FALSE))
    
    if (!idenSim & idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (idenSim & idenAge) 
      return(abind::asub(array, list(1,1), c(indSim, indAge), drop=FALSE))
    
    if (!idenSim & idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (idenSim & !idenAge)
      return(abind::asub(array, list(1,1), c(indSim), drop=FALSE))
    
    if (!idenSim & !idenAge) 
      return(array)
    
  }
  
  if (!incSim & !incYear & incAge) {
    idenAge <- IdenticalAge(array)
    
    if (idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (!idenAge) 
      return(array)
  }
  
  if (incSim & incYear & !incAge) {
    idenSim <- IdenticalSims(array) 
    idenTime <- IdenticalYears(array)
    
    if (idenSim & idenTime) 
      return(abind::asub(array, list(1,1), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & idenTime) 
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (idenSim & !idenTime) 
      return(abind::asub(array, list(1, UniqueYears(array)), c(indSim, indYear), drop=FALSE))
    
    if (!idenSim & !idenTime) 
      return(abind::asub(array, list(UniqueYears(array)), c(indYear), drop=FALSE))
    
  }
  
  if (!incSim & incYear & !incAge) {
    idenTime <- IdenticalYears(array)
    if (idenTime) 
      return(abind::asub(array, list(1), c(indYear), drop=FALSE))
    
    if (!idenTime) 
      return(abind::asub(array, UniqueYears(array), indYear, drop=FALSE))
  }
  
  if (incSim & !incYear & !incAge) {
    idenSim <- IdenticalSims(array) 
    if (idenSim) 
      return(abind::asub(array, list(1), c(indSim), drop=FALSE))
    if (!idenSim) 
      return(array)
  }
  
  
  
  array
}

