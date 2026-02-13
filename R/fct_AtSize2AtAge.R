

#' Calculates object@MeanAtAge from object@MeanAtLength and Length@ASK
#' 
#' @param object Any populated object with slots: `MeanAtAge` and `MeanAtLength` or `MeanAtWeight`
#' @param Length A [Length()] or [Weight()] object
#' @param max1 Logical. Standardize so that the maximum value is 1? 
#' 
#' @details
#' `max1` argument:
#' 
#' Sometimes SelectivityAtLength can result in maximum values for SelectivityAtAge < 1 
#' While this is probably 'correct' for the given SelectivityAtLength schedule,
#' it can result in some weirdness when max(SelectivityAtAge) < 1
#' This argument forces max(SelectivityAtAge) == 1
AtSize2AtAge <- function(object, Length, max1=FALSE) {

  if (inherits(Length, 'length')) {
    MeanAtSize <- object@MeanAtLength
  } else if (inherits(Length, 'weight')) {
    MeanAtSize <- object@MeanAtWeight
  } else {
    cli::abort("`Length` must be an object of class `length` or `weight`")
  }
  
  if (hasSlot(Length, 'ALK')) {
    ASK <- Length@ALK 
    if (is.null(ASK)) {
      cli::cli_abort("`Length@ALK` is not populated", .internal=TRUE)
    }
    
  }
  if (hasSlot(Length, 'AWK')) {
    ASK <- Length@AWK 
    if (is.null(ASK)) {
      cli::cli_abort("`Weight@AWK` is not populated", .internal=TRUE)
    }
    
  }

  dNames_MeanAtSize <- dimnames(MeanAtSize)
  dNames_ASK <- dimnames(ASK)
  
  if ("Sim" %in% names(dNames_MeanAtSize)) {
    bySim <- TRUE
    if (length(dNames_ASK[['Sim']])==1 &&
      length(dNames_ASK[['Sim']]==1)) {
        nSim <- 1
        sims <- as.numeric(dNames_ASK[['Sim']])
      } else {
        nSim <- c(dNames_MeanAtSize[['Sim']], dNames_ASK[['Sim']]) |>
          as.numeric() |>
          unique() |> 
          max()
        sims <- 1:nSim
      }
   
    
  } else {
    bySim <- FALSE
    nSim <- 1
  }
  
  if ("Area" %in% names(dNames_MeanAtSize)) {
    byArea <- TRUE
    nArea <- dNames_MeanAtSize[['Area']] |> length()
  } else {
    byArea <- FALSE
  }
  
  AgeClasses <- dNames_ASK[['Age']] |> as.numeric()
  nAge <- length(AgeClasses)
  
  Years <- c(dNames_MeanAtSize[['Year']], dNames_ASK[['Year']]) |>
    as.numeric() |>
    sort() |>
    unique()
  
  Years <- Years[Years>=min(dNames_MeanAtSize[['Year']])]
  
  
  nTS <- length(Years)

  if (byArea) {
    MeanAtAge <- array(0, dim=c(nSim, nAge, nTS, nArea),
                       dimnames = list(
                         Sim=sims,
                         Age=AgeClasses,
                         Year=Years,
                         Area=1:nArea
                       )
    )
  } else {
    MeanAtAge <- array(0, dim=c(nSim, nAge, nTS),
                       dimnames = list(
                         Sim=1:nSim,
                         Age=AgeClasses,
                         Year=Years
                       )
    )
  }

  if (all(MeanAtSize>0.99)) {
    MeanAtAge[] <- 1
    object@MeanAtAge <- MeanAtAge
    return(object)
  }
  
  if (all(MeanAtSize<0.01)) {
    MeanAtAge[] <- tiny
    object@MeanAtAge <- MeanAtAge
    return(object)
  }
  
  MeanAtSize <- MeanAtSize |> ExtendSims(nSim) |> ExtendYears(Years)
  ASK <- ASK |> ExtendSims(nSim) |> ExtendYears(Years)
  
  if (byArea) {
    object@MeanAtAge <- AtSize2AtAge_sim_area(MeanAtAge, MeanAtSize, ASK, nSim, nAge, nTS, nArea, bySim)
  } else {
    object@MeanAtAge <- AtSize2AtAge_sim(MeanAtAge, MeanAtSize, ASK, nSim, nAge, nTS, bySim)
  }
  
  if (max1) {
    object@MeanAtAge <- CheckSelectivityMaximum(object@MeanAtAge, alert=FALSE)
  }

  object
}

AtSize2AtAge_sim_area <- function(MeanAtAge, MeanAtSize, ASK, nSim, nAge, nTS, nArea, bySim) {
 
  for (sim in 1:nSim) {
    for (year in 1:nTS) {
      for (area in 1:nArea) {
        meanatsize <- MeanAtSize[sim,,year, area]
        if (all(meanatsize>0.99)) {
          MeanAtAge[sim,,year, area] <- 1
        } else {
          if (bySim) {
            ASK_ts <- ASK[sim,,,year]
            MeanAtAge[sim,,year, area] <- meanatsize %*%t(ASK_ts)
          } else {
            ASK_ts <- ASK[,,year]
            MeanAtAge[sim,,year, area] <- (MeanAtSize_ts %*%t(ASK_ts))[1,]
          }
        }
      }
    }
  }
  MeanAtAge
}

AtSize2AtAge_sim <- function(MeanAtAge, MeanAtSize, ASK, nSim, nAge, nTS, bySim=TRUE) {
  
  for (sim in 1:nSim) {
    for (year in 1:nTS) {
      meanatsize <- MeanAtSize[sim,,year]
      if (all(meanatsize>0.99)) {
        MeanAtAge[sim,,year] <- 1
      } else {
        if (bySim) {
          ASK_ts <- ASK[sim,,,year]
          MeanAtAge[sim,,year] <- meanatsize %*%t(ASK_ts)
        } else {
          ASK_ts <- ASK[,,year]
          MeanAtAge[sim,,year] <- (MeanAtSize_ts %*%t(ASK_ts))[1,]
        }
      }
    }
  }
  MeanAtAge
}

MeanAtLength2MeanAtAge <- function(object,
                                   Length,
                                   max1=FALSE) {
  
  if (is.null(object@MeanAtLength)) {
    return(object)
  }
  if (!is.null(object@MeanAtAge)) {
    return(object)
  }
  
  CheckRequiredObject(Length, 'length')
  AtSize2AtAge(object, Length, max1)
}

MeanAtWeight2MeanAtAge <- function(object,
                                   Weight,
                                   max1=FALSE) {
  if (is.null(object@MeanAtWeight)) {
    return(object)
  }
  if (!is.null(object@MeanAtAge)) {
    return(object)
  }
  
  CheckRequiredObject(Weight, 'weight')
  AtSize2AtAge(object, Weight, max1)
}
