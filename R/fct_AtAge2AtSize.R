
LinearInterpolate_Age <- function(array, nSubAges=12) {
  dnames <- dimnames(array)
  AgeClasses <- dnames[['Age']] |> as.numeric()
  by <- AgeClasses[2] -AgeClasses[1]
  nAge <- length(AgeClasses)
  nAgesOut <- (nAge*nSubAges)-(nSubAges-1)
  newAgeClasses <- seq(AgeClasses[1], by=by/nSubAges, length.out=nAgesOut) |> round(5)
  
  dnames <- names(dnames)
  dnames <- dnames[!dnames=='Age']
  array_inter <- apply(array, dnames, function(y, x, xout) {
    approx(x, y=y,xout=xout)$y
  }, x=AgeClasses, xout=newAgeClasses) 
  
  # update dim names 
  dnames <- dimnames(array_inter)
  dnames[[1]] <- newAgeClasses
  names(dnames)[1] <- 'Age'
  dimnames(array_inter) <- dnames
  array_inter |> aperm(names(dimnames(array)))
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
  # TODO extend Years and Sim dimensions as needed 
  
  if (!is.null(object@MeanAtLength)) {
    return(object)
  }
  
  ObjectMeanAtAge <- object@MeanAtAge
  LengthMeanAtAge <- Length@MeanAtAge
  LengthCVatAge <- Length@CVatAge
  
  ASK <- Length@ASK 
  if (is.null(ASK)) {
    cli::cli_abort("`Length@ASK` is not populated", .internal=TRUE)
  }
  
  AgeClasses <- dimnames(ASK)[['Age']] |> as.numeric()
  nAge <- length(AgeClasses)
  
  LengthCVatAge <- ExtendAges(LengthCVatAge, AgeClasses)
  
  if (nAge < 50) {  
    # Increases the temporal resolution of `ObjectMeanAtAge` and `ASK`
    # by linear interpolate Mean length-at-age and CV length-at-age
    ObjectMeanAtAge <- LinearInterpolate_Age(object@MeanAtAge)
    ASK <- CalcAgeSizeKey(MeanAtAge=LinearInterpolate_Age(LengthMeanAtAge),
                          CVatAge=LinearInterpolate_Age(LengthCVatAge),
                          Classes=Length@Classes,
                          TruncSD=Length@TruncSD,
                          Dist=Length@Dist,
                          AgeClasses=NULL,
                          silent=TRUE)
    
  }

  # Sims 
  Sims <- c(dimnames(object@MeanAtAge)[['Sim']], dimnames(Length@ASK)[['Sim']]) |>
    as.numeric() |> unique() |> sort()
  
  # Years
  Years <- c(dimnames(object@MeanAtAge)[['Year']], dimnames(Length@ASK)[['Year']]) |>
    as.numeric() |> unique() |> sort()
  
  
  
  ASK <- ExtendYears(ASK, Years)
  object@MeanAtAge <- ExtendYears(object@MeanAtAge, Years)
  
  
  dnames <- dimnames(object@MeanAtAge)
  names(dnames)[2] <- 'Class'
  dnames[['Class']] <- Length@Classes
  dnames[['Sim']] <- Sims
  dnames[['Year']] <- Years
  
  ASK_dim <- dim(ASK)
  OMA_dim <- dim(ObjectMeanAtAge)
  
  nSim   <- length(Sims)
  nAge   <- ASK_dim[2]
  nClass <- ASK_dim[3]
  nYear  <- length(Years)
  
  hasArea <- length(OMA_dim) == 4
  
  if (hasArea) {
    nArea <- OMA_dim[4]
  } else {
    nArea <- 1
  }
 
  out_dim <- if (hasArea) {
    c(nSim, nClass, nYear, nArea)
  } else {
    c(nSim, nClass, nYear)
  }
  
  MeanAtLength <- array(NA, dim=out_dim,
                        dimnames=dnames)
  
  
  for (s in seq_len(nSim)) {
    for (y in seq_len(nYear)) {
      for (a in seq_len(nArea)) { 
        if (hasArea) {
          MeanAtAge <- ObjectMeanAtAge[s, , y, a]
        } else {
          MeanAtAge <-  ObjectMeanAtAge[s, , y]
        }
        
        ASK_sim_ts <- ASK[s, , , y]
        sums <- matrix(apply(ASK_sim_ts, 2, sum), nAge, nClass, byrow=TRUE)
        ASK_stand <- ASK_sim_ts/sums
        ASK_stand[!is.finite(ASK_stand)] <- 0
        
        atlength <- as.numeric(MeanAtAge %*% ASK_stand)
        
        
        if (hasArea) {
          MeanAtLength[s, , y, a] <- atlength
        } else {
          MeanAtLength[s, , y] <- atlength
        }
        
        
      }
    }
  }
  # TODO - MeanAtSize = 0 for classes greater than truncSD in Length - ie ASK = 0 
  
  object@Classes <- Length@Classes
  object@MeanAtLength <- MeanAtLength
  object
}



