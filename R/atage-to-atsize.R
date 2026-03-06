
LinearInterpolate_Age <- function(array, nSubAges=12) {
  dnames <- dimnames(array)
  AgeClasses <- dnames[['Age']] |> as.numeric()
  by <- AgeClasses[2] -AgeClasses[1]
  to <- max(AgeClasses)
  nAge <- length(AgeClasses)
  nAgesOut <- (nAge*nSubAges)-(nSubAges-1)
  newAgeClasses <- seq(AgeClasses[1], by=by/nSubAges, to=to) |> round(5)
  
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
#' @param Years Numeric vector of years. Optional
#' @param ASKOverride Optional Age-Size Key. Otherwise it uses the one from Length or Weight object. 
#' Only used to avoid repeated calls to `CalcAgeSizeKey`
#' 
#' Converting from at-age to at-size requires a sufficiently high temporal resolution
#' to ensure that the `MeanAtAge` schedule can fill all the `MeanAtLength` classes
#' @keywords internal
AtAge2AtSize <- function(object, Length, max1=TRUE, Years=NULL, ASKOverride=NULL) {
  
  if (!is.null(object@MeanAtLength)) {
    return(object)
  }
  
  ObjectMeanAtAge <- object@MeanAtAge |> ReduceDims()
  LengthMeanAtAge <- Length@MeanAtAge |> ReduceDims()
  LengthCVatAge <- Length@CVatAge |> ReduceDims()
  
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
  
  AgeClasses <- dimnames(ASK)[['Age']] |> as.numeric()
  nAge <- length(AgeClasses)
  
  LengthCVatAge <- ExtendAges(LengthCVatAge, AgeClasses)
  
  if (nAge < 50 ) {  
    # Increases the temporal resolution of `ObjectMeanAtAge` and `ASK`
    # by linear interpolate Mean length-at-age and CV length-at-age
    ObjectMeanAtAge <- LinearInterpolate_Age(array=object@MeanAtAge)
    if (is.null(ASKOverride)) {
      ASK <- CalcAgeSizeKey(MeanAtAge=LinearInterpolate_Age(LengthMeanAtAge),
                            CVatAge=LinearInterpolate_Age(LengthCVatAge),
                            Classes=Length@Classes,
                            TruncSD=Length@TruncSD,
                            Dist=Length@Dist,
                            silent=TRUE)
      
    } else {
      ASK <- ASKOverride
    }
      
  }

  # Sims 
  MeanAtAgeSim <- dimnames(object@MeanAtAge)[['Sim']]
  ASKSim <- dimnames(ASK)[['Sim']]
  
  if (length(MeanAtAgeSim) == 1 && length(ASKSim) == 1) {
    Sims <- max( c(MeanAtAgeSim, ASKSim)) |> as.numeric()
  } else {
    Sims <- c(MeanAtAgeSim, ASKSim) |>
      as.numeric() |> unique() |> sort()
  }
 
  # Years
  if (is.null(Years)) 
    Years <- c(dimnames(object@MeanAtAge)[['Year']], dimnames(ASK)[['Year']]) |>
    as.numeric() |> unique() |> sort()
  
  ASK <- ExtendYears(ASK, Years) 
  ObjectMeanAtAge <- ExtendYears(array=ObjectMeanAtAge, Years) 
  
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
    object_sim <- min(s, dim(ObjectMeanAtAge)[1])
    for (y in seq_len(nYear)) {
      for (a in seq_len(nArea)) { 
        if (hasArea) {
          MeanAtAge <- ObjectMeanAtAge[object_sim, , y, a]
        } else {
          MeanAtAge <-  ObjectMeanAtAge[object_sim, , y]
        }
        
        if (all(MeanAtAge>=0.99)) {
          atlength <- 1
        } else {
          ASK_s <- min(ASK_dim[1], s)
          ASK_y <- min(ASK_dim[3], y)
          ASK_sim_ts <- ASK[ASK_s, , , ASK_y]
          sums <- matrix(apply(ASK_sim_ts, 2, sum), nAge, nClass, byrow=TRUE)
          ASK_stand <- ASK_sim_ts/sums
          ASK_stand[!is.finite(ASK_stand)] <- 0
          
          atlength <- as.numeric(MeanAtAge %*% ASK_stand)
        }

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



