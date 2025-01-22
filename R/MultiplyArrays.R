
#' Perform Operations on Two Arrays
#' 
#' Multiply, divide, subtract, or add two arrays together. 
#' The arrays must have the same named dimensions, but do not need to have
#' the same length for each dimension
#' 
#' @param array1 A 2D, 3D, or 4D array
#' @param array2 A 2D, 3D, or 4D array
#' @name MultiplyArrays
NULL


CheckArrays <- function(ArrayList) {
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  
  if (length(dim(array1))!=length(dim(array2)))
    cli::cli_abort('`array1` and `array2` must have number of dimensions')
  
  if (!(all(names(dimnames(array1))==names(dimnames(array2)))))
    cli::cli_abort('`array1` and `array2` must have same named dimensions')
}

ExpandArray <- function(Array, Dims, TimeSteps) {
  ind <- which(names(dimnames(Array))=='Time Step')
  if (length(ind)<1) 
    return(Array)
  
  ArrayTS <- dimnames(Array)[[ind]] |> as.numeric()
  
  OutArray <- array(NA, dim=Dims) |>
    AddDimNames(TimeSteps=TimeSteps)
  for (i in seq_along(TimeSteps)) {
    j <- which(ArrayTS <= TimeSteps[i]) |> max()
    OutArray[,,i] <- Array[,,j]
  }
  OutArray
}

MatchTimeSteps <- function(ArrayList) {
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  
  nm1 <- names(dimnames(array1))
  ind <- which(nm1=='Time Step')
  if (length(ind)<1)
    return(list(array1, array2))
  
  TSarray1 <- dimnames(array1)[[ind]] |> as.numeric()
  TSarray2 <- dimnames(array2)[[ind]] |> as.numeric()

  TSout <- c(TSarray1, TSarray2) |> unique() |> sort()
  
  if (all(TSout %in% TSarray1) & all(TSout %in% TSarray2)) 
    return(list(array1, array2))
  
  d1 <- dim(array1)
  d2 <- dim(array2)
  d1[ind] <- length(TSout)
  d2[ind] <- length(TSout)
  
  if (!all(TSout %in% TSarray1)) 
    array1 <- ExpandArray(Array=array1, Dims=d1, TimeSteps=TSout)
  if (!all(TSout %in% TSarray2)) 
    array2 <- ExpandArray(Array=array2, Dims=d2, TimeSteps=TSout)
  
  list(array1=array1, array2=array2)
}

CheckDims <- function(ArrayList, dimname='Age') {
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  
  names(dimnames(array1))
  ind <- which(nm1==dimname)
  if (length(ind)<1)
    return(ArrayList)
  
  dims1 <- dimnames(array1)[[ind]] |> as.numeric()
  dims2 <- dimnames(array2)[[ind]] |> as.numeric()
  
  if (
    (length(dims1)>1 & length(dims1) != length(dims2)) |
    (length(dims2)>1 & length(dims1) != length(dims2))
  ) {
    cli::cli_abort('{.code {dimname}} dimension must either be length 1 or equal lengths in both arrays')
  }
  ArrayList
  
}



ArrayOperation <- function(array1, array2, operation=`*`) {
  ArrayList <- list(array1, array2)
  CheckArrays(ArrayList)
  
  ArrayList <- ArrayList |> 
    MatchTimeSteps() |>
    CheckDims('Sim') |>
    CheckDims('Age') 
  
  array1 <- ArrayList[[1]]
  array2 <- ArrayList[[2]]
  
  d1 <- dim(array1)
  d2 <- dim(array2)
  alldims <- rbind(d1, d2)
  outdims <- apply(alldims, 2, max)
  out <- array(NA, dim=outdims) 
  ind <- which(nm1=='Time Step')
  if (length(ind)>0) {
    out <- out |> AddDimNames(names=nm1, TimeSteps = dimnames(array1)$`Time Step`)
  } else {
    out <- out |> AddDimNames(names=nm1)
  }
  
  if (all(d1==d2)) {
    out[] <- operation(array1, array2)
    return(out)
  }
  
  # do the operation in a loop - TODO C++
  
  if (length(outdims)==2) {
    for (s in 1:outdims[1]) {
      for (age in 1:outdims[2]) {
        out[s,age,ts] <- operation(array1[GetIndex(s, d1[1]), 
                                          GetIndex(age, d1[2])],
                                   array2[GetIndex(s, d2[1]),
                                          GetIndex(age, d2[2])]
        )
      }
    }
  }
  
  
  if (length(outdims)==3) {
    for (s in 1:outdims[1]) {
      for (age in 1:outdims[2]) {
        for (ts in 1:outdims[3]) {
          out[s,age,ts] <- operation(array1[GetIndex(s, d1[1]), 
                                            GetIndex(age, d1[2]),
                                            GetIndex(ts, d1[3])],
                                     array2[GetIndex(s, d2[1]),
                                            GetIndex(age, d2[2]),
                                            GetIndex(ts, d2[3])]
          )
        }
      }
    }
  }
  
  if (length(outdims)==4) {
    for (s in 1:outdims[1]) {
      for (age in 1:outdims[2]) {
        for (ts in 1:outdims[3]) {
          for (area in 1:outdims[4]) {
            out[s,age,ts,area] <- operation(array1[GetIndex(s, d1[1]), 
                                                   GetIndex(age, d1[2]),
                                                   GetIndex(ts, d1[3]),
                                                   GetIndex(area, d1[4])],
                                            array2[GetIndex(s, d2[1]),
                                                   GetIndex(age, d2[2]),
                                                   GetIndex(ts, d2[3]),
                                                   GetIndex(area, d2[4])]
            )
          }
        }
      }
    }
  }
  out
}

#' @rdname MultiplyArrays
#' @export
MultiplyArrays <- function(array1, array2) {
  ArrayOperation(array1, array2)
}

#' @rdname MultiplyArrays
#' @export
AddArrays <- function(array1, array2) {
  ArrayOperation(array1, array2, `+`)
}

#' @rdname MultiplyArrays
#' @export
DivideArrays <- function(array1, array2) {
  ArrayOperation(array1, array2, `/`)
}

#' @rdname MultiplyArrays
#' @export
SubtractArrays <- function(array1, array2) {
  ArrayOperation(array1, array2, `-`)
}
