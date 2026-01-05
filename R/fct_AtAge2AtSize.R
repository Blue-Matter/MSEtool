# Increases the temporal resolution of `ObjectMeanAtAge` and `ASK`
# by linear interpolate Mean length-at-age and CV length-at-age
#

IncreaseTemporalResolution <- function(ObjectMeanAtAge,
                                       LengthMeanAtAge,
                                       LengthCVatAge,
                                       ASK,
                                       bySim,
                                       byArea,
                                       nArea) {
  
  AgeClasses <- dimnames(ObjectMeanAtAge)[['Age']] |> as.numeric()
  nAges <- length(AgeClasses)
  nSubAges <- 12 # eg months in an annual model 
  nAgesOut <- (nAges*nSubAges)-(nSubAges-1)
  
  
  dimLengthMeanAtAge <- dim(LengthMeanAtAge)
  dimLengthMeanAtAge[2] <- nAgesOut
  dimLengthCVatAgeOut <- dim(LengthCVatAge)
  dimLengthCVatAgeOut[2] <- nAgesOut
  
  LengthMeanAtAgeOut <- array(0, dim=dimLengthMeanAtAge)
  LengthCVatAgeOut <- array(0, dim=dimLengthCVatAgeOut)
  
  # up to here
  stop()
  
  # TODO 
  # - finish this code - make it smarter/cleaner
  # - then finish `AtAge2AtSize`
  # - then Selectivity Examples
  
  
  
  # generate a new age-size key with finer temporal resolution
  ASKout <- CalcAgeSizeKey(MeanAtAge=LengthMeanAtAgeOut,
                           CVatAge=LengthCVatAgeOut,
                           Classes=Length@Classes,
                           TruncSD=Length@TruncSD,
                           Dist=Length@Dist,
                           AgeClasses=NULL,
                           silent=TRUE)
  
  
  
  
  ObjectMeanAtAgeOut <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
  
  
  list(ObjectMeanAtAge=ObjectMeanAtAgeOut,
       ASK=ASKout) 
  
}



#' Calculates object@MeanAtLength from object@MeanAtAge and Length@ASK
#' 
#' @param object Any populated object with slots: `MeanAtAge` and `MeanAtLength` or `MeanAtWeight`
#' @param Length A [Length()] or [Weight()] object
#' @param max1 Logical. Standardize so that the maximum value is 1? 
#' 
#' Converting from at-age to at-size requires a sufficiently high temporal resolution
#' to ensure that the `MeanAtAge` schedule can fill all the `MeanAtLength` classes
#' 
AtAge2AtSize <- function(object, Length, max1=TRUE) {
  
  ObjectMeanAtAge <- object@MeanAtAge
  LengthMeanAtAge <- Length@MeanAtAge
  LengthCVatAge <- Length@CVatAge
  
  ASK <- Length@ASK 
  if (is.null(ASK)) {
    cli::cli_abort("`Length@ASK` is not populated", .internal=TRUE)
  }
  
  dNames_MeanAtAge <- dimnames(ObjectMeanAtAge)
  dNames_ASK <- dimnames(ASK)
  
  
  if ("Sim" %in% names(dNames_MeanAtAge)) {
    bySim <- TRUE
    nSim <- c(dNames_MeanAtAge[['Sim']], dNames_ASK[['Sim']]) |>
      as.numeric() |>
      unique() |> 
      max()
    
  } else {
    bySim <- FALSE
    nSim <- 1
  }
  
  if ("Area" %in% names(dNames_MeanAtAge)) {
    byArea <- TRUE
    nArea <- dNames_MeanAtAge[['Area']] |> length()
  } else {
    byArea <- FALSE
    nArea <- 0
  }
  
  Classes <- dNames_ASK[['Class']] |> as.numeric()
  nClasses <- length(Classes)
  
  Years <- c(dNames_MeanAtSize[['Year']], dNames_ASK[['Year']]) |>
    as.numeric() |>
    sort() |>
    unique()
  nTS <- length(Years)
  
  AgeClasses <- dNames_ASK[['Age']] |> as.numeric()
  nAge <- length(AgeClasses)
  
  LengthCVatAge <- LengthCVatAge |> ExtendAges(AgeClasses)
  
  if (nAge < 50) { # arbitrary number!
    tempList <- IncreaseTemporalResolution(ObjectMeanAtAge,
                                           LengthMeanAtAge,
                                           LengthCVatAge,
                                           ASK,
                                           bySim,
                                           byArea,
                                           nArea)
    ObjectMeanAtAge <- tempList$ObjectMeanAtAge
    ASK <- tempList$ASK
  }
  
######################################
  
  if (dim(MeanAtAge)[AgeDim]<30) { # arbitrary number!
    # Generate higher resolution length-at-age
    # linear interpolate Mean length-at-age and CV length-at-age
    # for 11 time-steps in-between (e.g., months)
    dims  <- rbind(dim(Length@MeanAtAge),
                   dim(Length@CVatAge),
                   dim(ObjectMeanAtAge)
    )
    dd <- apply(dims, 2, max)
    
    nSubAges <- 12
    SubAgeDim <- (dd[2]*nSubAges)-(nSubAges-1)
    
    objectMeanAtAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    LengthMeanAtAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    LengthCVatAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    
    dname1 <- dimnames(Length@MeanAtAge)
    ages <- as.numeric(dname1[["Age"]])
    dname1[["Age"]] <- seq(from=ages[1], to=ages[length(ages)], length.out=SubAgeDim)
    
    TSteps <- c(dimnames(MeanAtAge)[[3]], dimnames(Length@MeanAtAge)[[3]]) |> unique() |> sort()
    dname1[["Year"]] <- TSteps
    
    dname1$Sim <- 1:dd[1]
    dimnames(LengthMeanAtAge) <- dname1
    
    Length@CVatAge <- ArrayExpand(Length@CVatAge, dd[1], ages, dname1[["Year"]])
    dname1 <- dimnames(Length@CVatAge)
    dname1[["Age"]] <- seq(from=ages[1], to=ages[length(ages)], length.out=SubAgeDim)
    TSteps <- c(dimnames(MeanAtAge)[[3]], dimnames(Length@CVatAge)[[3]]) |> unique() |> sort()
    dname1[["Year"]] <- TSteps
    
    dimnames(LengthCVatAge) <- dname1
    
    ind <- seq(from=1, by=nSubAges, to=dim(LengthMeanAtAge)[2])
    
    MeanAtAge <- ArrayExpand(MeanAtAge, dd[1], ages, Years=TSteps)
    
    objectMeanAtAge[,ind,] <- MeanAtAge[]
    LengthMeanAtAge[,ind,] <- Length@MeanAtAge[]
    LengthCVatAge[,ind,] <- Length@CVatAge[]
    
    dims  <- rbind(dim(Length@MeanAtAge),
                   dim(Length@CVatAge),
                   dim(MeanAtAge)
    )
    
    for (i in 1:(length(ind)-1)) {
      ind2 <- c(ind[i], ind[i]+12)
      ind3 <- (ind2[1]+1): (ind2[2]-1)
      
      # silly loop for now!
      for (s in 1:max(dims[,1])) {
        for (ts in 1:max(dims[,3])) {
          
          # Linear interpolate at-Age schedule
          temp <- approx(ind2, 
                         MeanAtAge[GetIndex(s, dims[1,1]),i:(i+1), GetIndex(s, dims[1,3])],
                         xout=ind3)
          objectMeanAtAge[s,ind3,ts] <- temp$y
          
          # Linear interpolate growth curve
          temp <- approx(ind2, Length@MeanAtAge[GetIndex(s, dims[1,1]),
                                                i:(i+1),
                                                GetIndex(s, dims[1,3])],
                         xout=ind3)
          
          LengthMeanAtAge[s,ind3,ts] <- temp$y
          
          
          
          if (dims[2,2]==1) {
            LengthCVatAge[s,,ts] <- Length@CVatAge[GetIndex(s, dims[2,1]),,
                                                   GetIndex(s, dims[2,3])]
          } else {
            temp <- approx(ind2, Length@CVatAge[GetIndex(s, dims[2,1]),
                                                i:(i+1),
                                                GetIndex(s, dims[2,3])],
                           xout=ind3)
            LengthCVatAge[s,ind3,ts] <- temp$y
          }
        }
      }
    }
    
    # generate a new age-size key with finer temporal resolution
    ASK <- CalcAgeSizeKey(MeanAtAge=LengthMeanAtAge,
                          CVatAge=LengthCVatAge,
                          Classes=Length@Classes,
                          TruncSD=Length@TruncSD,
                          Dist=Length@Dist,
                          AgeClasses=NULL,
                          silent=TRUE)
    
    MeanAtAge <- objectMeanAtAge
    dimnames(MeanAtAge) <- dname1
    
  }
  
  #############################
  
  if (byArea) {
    MeanAtSize <- array(0, dim=c(nSim, nClasses, nTS, nArea),
                        dimnames = list(
                          Sim=1:nSim,
                          Class=Classes,
                          Year=Years,
                          Area=1:nArea
                        ))
  } else {
    MeanAtSize <- array(0, dim=c(nSim, nClasses, nTS),
                        dimnames = list(
                          Sim=1:nSim,
                          Class=Classes,
                          Year=Years
                        ))
  }

  if (all(ObjectMeanAtAge>0.99)) {
    MeanAtSize[] <- 1
    if (inherits(Length, 'length')) {
      object@MeanAtLength <- MeanAtSize
    }
    if (inherits(Length, 'weight')) {
      object@MeanAtWeight <- MeanAtSize
    }
    return(object)
  }
  
  if (all(ObjectMeanAtAge<0.01)) {
    MeanAtSize[] <- tiny
    if (inherits(Length, 'length')) {
      object@MeanAtLength <- MeanAtSize
    }
    if (inherits(Length, 'weight')) {
      object@MeanAtWeight <- MeanAtSize
    }
    return(object)
  }
  
  if (byArea) {
    MeanAtSize <- AtAge2AtSize_sim_area(MeanAtSize, MeanAtSize, ASK, nSim, nAge, nTS, nArea, bySim)
  } else {
    MeanAtSize <- AtAge2AtSize_sim(MeanAtAge, MeanAtSize, ASK, nSim, nAge, nTS, bySim)
  }
  
  if (inherits(Length, 'length')) {
    object@MeanAtLength <- MeanAtSize
  }
  if (inherits(Length, 'weight')) {
    object@MeanAtWeight <- MeanAtSize
  }
  
  object@Classes <- Length@Classes
  object
  
  for (s in 1:nsim) {
    for (t in 1:nTS) {
      MeanAtAge_ts <- MeanAtAge[GetIndex(s, nsim_MeanAtAge), ,GetIndex(t, nTS_MeanAtAge)]
      ASK_ts <- ASK[GetIndex(s, nsim_ASK),,,GetIndex(t, nTS_ASK)]
      ASK_tsvec <- apply(ASK_ts, 2, sum)
      ind <- max(which(ASK_tsvec>0))
      
      
      sums <- matrix(apply(ASK_ts, 2, sum), nage, nClasses, byrow=TRUE)
      ASK_ts_stand <- ASK_ts/sums
      ASK_ts_stand[!is.finite(ASK_ts_stand)] <- 0
      AtSize[s,,t] <- MeanAtAge_ts %*% ASK_ts_stand
      if (max1)
        AtSize[s,ind:ncol(ASK_ts),t] <- 1
    }
  }
  
}

AtAge2AtSize_sim_area <- function() {
  
  
}



