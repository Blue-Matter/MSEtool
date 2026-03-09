
#' Linearly Interpolate an Array Along the Age Dimension
#'
#' Increases the resolution of the `Age` dimension of `array` by linearly
#' interpolating between existing age classes using [stats::approx()]. All
#' other dimensions are preserved. The output is permuted back to match the
#' original dimension order.
#'
#' @param array A named-dimnames array with an `"Age"` dimension whose values
#'   are equally spaced numeric age classes.
#' @param nSubAges Integer. Number of sub-age intervals to insert between each
#'   pair of adjacent age classes. Default `12`.
#'
#' @return An array with the same dimensions as `array` except that the `Age`
#'   dimension is expanded from `n` classes to `(n - 1) * nSubAges + 1`
#'   classes, with age labels rounded to 5 decimal places.
#' @keywords internal
LinearInterpolate_Age <- function(array, nSubAges=12) {
  dnames        <- dimnames(array)
  AgeClasses    <- as.numeric(dnames[['Age']])
  by            <- AgeClasses[2] - AgeClasses[1]
  newAgeClasses <- round(seq(AgeClasses[1], to=max(AgeClasses), by=by/nSubAges), 5)
  
  other_dims  <- names(dnames)[names(dnames) != 'Age']
  array_inter <- apply(array, other_dims, function(y, x, xout) {
    approx(x, y=y, xout=xout)$y
  }, x=AgeClasses, xout=newAgeClasses)
  
  dn           <- dimnames(array_inter)
  dn[[1]]      <- newAgeClasses
  names(dn)[1] <- 'Age'
  dimnames(array_inter) <- dn
  
  aperm(array_inter, names(dimnames(array)))
}


#' Interpolate MeanAtAge and Recalculate Age-Size Key at Higher Age Resolution
#'
#' When the number of age classes is below `minAgeRes`, linearly interpolates
#' `ObjectMeanAtAge` to a finer age resolution via [LinearInterpolate_Age()]
#' and recomputes the Age-Size Key at the same resolution via
#' [CalcAgeSizeKey()]. Returns both arrays unchanged if resolution is already
#' sufficient or `nAge >= minAgeRes`.
#'
#' @param ObjectMeanAtAge Numeric array of mean-at-age values
#'   (`Sim × Age × Year` or `Sim × Age × Year × Area`).
#' @param LengthMeanAtAge Numeric array of length mean-at-age from the
#'   [Length()] or [Weight()] object.
#' @param LengthCVatAge Numeric array of CV-at-age from the [Length()] or
#'   [Weight()] object, already extended to match `AgeClasses`.
#' @param ASK Age-Size Key array (`Sim × Age × Class × Year`). Returned
#'   unchanged if `nAge >= minAgeRes` or `ASKOverride` is supplied.
#' @param Length A [Length()] or [Weight()] object supplying `Classes`,
#'   `TruncSD`, and `Dist` for [CalcAgeSizeKey()].
#' @param nAge Integer. Current number of age classes.
#' @param minAgeRes Integer. Minimum age classes required before interpolation
#'   is triggered.
#' @param ASKOverride Optional pre-computed Age-Size Key. If non-`NULL` and
#'   interpolation is triggered, this is used instead of calling
#'   [CalcAgeSizeKey()].
#'
#' @return A named list with elements `ObjectMeanAtAge` and `ASK`, both at the
#'   resolved age resolution.
#' @keywords internal
ResolveAgeResolution <- function(ObjectMeanAtAge, LengthMeanAtAge,
                                 LengthCVatAge, ASK, Length,
                                 nAge, minAgeRes, ASKOverride) {
  if (nAge >= minAgeRes)
    return(list(ObjectMeanAtAge=ObjectMeanAtAge, ASK=ASK))
  
  ObjectMeanAtAge <- LinearInterpolate_Age(ObjectMeanAtAge)
  
  ASK <- if (!is.null(ASKOverride)) {
    ASKOverride
  } else {
    CalcAgeSizeKey(
      MeanAtAge = LinearInterpolate_Age(LengthMeanAtAge),
      CVatAge   = LinearInterpolate_Age(LengthCVatAge),
      Classes   = Length@Classes,
      TruncSD   = Length@TruncSD,
      Dist      = Length@Dist,
      silent    = TRUE
    )
  }
  
  list(ObjectMeanAtAge=ObjectMeanAtAge, ASK=ASK)
}

#' Convert Mean-at-Age to Mean-at-Size
#'
#' Populates `object@MeanAtLength` (or `object@MeanAtWeight`) by projecting
#' `object@MeanAtAge` through a column-standardised Age-Size Key (ALK or AWK)
#' from a [Length()] or [Weight()] object. If the number of age classes is
#' below `minAgeRes`, both `MeanAtAge` and the Age-Size Key are linearly
#' interpolated to a finer age resolution before conversion, ensuring the
#' at-age schedule has sufficient resolution to fill all size classes. Returns
#' `object` unchanged if `MeanAtLength` is already populated.
#'
#' @param object An S4 object with slots `MeanAtAge` (array:
#'   `Sim × Age × Year` or `Sim × Age × Year × Area`) and `MeanAtLength`
#'   (`NULL` if not yet populated).
#' @param Length A [Length()] or [Weight()] object supplying `MeanAtAge`,
#'   `CVatAge`, `Classes`, `TruncSD`, `Dist`, and either an `ALK` or `AWK`
#'   slot (array: `Sim × Age × Class × Year`).
#' @param max1 Logical. If `TRUE`, standardises output so the maximum value
#'   across size classes is 1. Default `TRUE`.
#' @param Years Numeric vector of years. If `NULL` (default), the union of
#'   years in `object@MeanAtAge` and the Age-Size Key is used.
#' @param ASKOverride Optional pre-computed Age-Size Key array, used in place
#'   of calling [CalcAgeSizeKey()] when interpolation is triggered. Avoids
#'   redundant computation in repeated calls.
#' @param minAgeRes Integer. Minimum number of age classes required before
#'   linear interpolation to a finer age resolution is triggered. Default `50`.
#'
#' @return `object` with `object@MeanAtLength` and `object@Classes` populated.
#' @keywords internal
AtAge2AtSize <- function(object, Length, max1=TRUE, Years=NULL,
                         ASKOverride=NULL, minAgeRes=50) {
  
  if (!is.null(object@MeanAtLength))
    return(object)
  
  ObjectMeanAtAge <- object@MeanAtAge |> ReduceDims()
  LengthMeanAtAge <- Length@MeanAtAge |> ReduceDims()
  LengthCVatAge   <- Length@CVatAge   |> ReduceDims()
  
  ASK           <- GetASK(Length)
  AgeClasses    <- as.numeric(dimnames(ASK)[['Age']])
  nAge          <- length(AgeClasses)
  LengthCVatAge <- ExtendAges(LengthCVatAge, AgeClasses)
  
  # Interpolate to finer age resolution if needed 
  resolved <- ResolveAgeResolution(ObjectMeanAtAge, LengthMeanAtAge,
                                   LengthCVatAge, ASK, Length,
                                   nAge, minAgeRes, ASKOverride)
  
  ObjectMeanAtAge <- resolved$ObjectMeanAtAge
  ASK             <- resolved$ASK
  
  # Resolve Sim and Year dimensions
  MeanAtAgeSim <- dimnames(object@MeanAtAge)[['Sim']]
  ASKSim <- dimnames(ASK)[['Sim']]
  
  Sims <- if (length(MeanAtAgeSim) == 1L && length(ASKSim) == 1L) {
    as.numeric(max(c(MeanAtAgeSim, ASKSim)))
  } else {
    sort(unique(as.numeric(c(MeanAtAgeSim, ASKSim))))
  }
  
  if (is.null(Years))
    Years <- sort(unique(as.numeric(c(
      dimnames(object@MeanAtAge)[['Year']],
      dimnames(ASK)[['Year']]
    ))))
  
  ASK             <- ExtendYears(ASK, Years)
  ObjectMeanAtAge <- ExtendYears(ObjectMeanAtAge, Years)
  
  # Dimensions 
  ASK_dim <- dim(ASK)
  OMA_dim <- dim(ObjectMeanAtAge)
  nSim    <- length(Sims)
  nAge    <- ASK_dim[2]
  nClass  <- ASK_dim[3]
  nYear   <- length(Years)
  hasArea <- length(OMA_dim) == 4
  nArea   <- if (hasArea) OMA_dim[4] else 1L
  
  # Allocate output 
  dn           <- dimnames(object@MeanAtAge)
  names(dn)[2] <- 'Class'
  dn[['Class']] <- Length@Classes
  dn[['Sim']]   <- Sims
  dn[['Year']]  <- Years
  if (hasArea) dn[['Area']] <- seq_len(nArea)
  
  out_dim      <- if (hasArea) c(nSim, nClass, nYear, nArea) else
    c(nSim, nClass, nYear)
  MeanAtLength <- array(NA, dim=out_dim, dimnames=dn)
  
  for (s in seq_len(nSim)) {
    obj_s      <- min(s, OMA_dim[1])
    ASK_s      <- min(ASK_dim[1], s)
    for (y in seq_len(nYear)) {
      ASK_y      <- min(ASK_dim[4], y)        
      ASK_sim_ts <- ASK[ASK_s, , , ASK_y]
      col_sums   <- matrix(colSums(ASK_sim_ts), nAge, nClass, byrow=TRUE)
      ASK_stand  <- ASK_sim_ts / col_sums
      ASK_stand[!is.finite(ASK_stand)] <- 0
      
      for (a in seq_len(nArea)) {
        MeanAtAge_vec <- if (hasArea) ObjectMeanAtAge[obj_s, , y, a] else
          ObjectMeanAtAge[obj_s, , y]
        
        atlength <- if (all(MeanAtAge_vec >= 0.99)) 1 else
          as.numeric(MeanAtAge_vec %*% ASK_stand)
        
        if (hasArea) MeanAtLength[s, , y, a] <- atlength else
          MeanAtLength[s, , y]    <- atlength
      }
    }
  }
  # TODO: MeanAtSize = 0 for classes beyond truncSD in Length (ASK = 0)
  
  object@Classes      <- Length@Classes
  object@MeanAtLength <- MeanAtLength
  object
}

#' Populate `MeanAtLength` from `MeanAtAge`
#'
#' Convenience wrapper around [AtAge2AtSize()] for the length case. Returns
#' `object` unchanged if `MeanAtLength` is already populated and
#' `replace = FALSE`.
#'
#' @param object An S4 object with slots `MeanAtAge` and `MeanAtLength`.
#' @param Length A [Length()] object.
#' @param replace Logical. If `TRUE`, recomputes `MeanAtLength` even if
#'   already populated. Default `FALSE`.
#' @param max1 Logical. Passed to [AtAge2AtSize()]. Default `FALSE`.
#' @param Years Numeric vector of years. Passed to [AtAge2AtSize()].
#' @param ASK Optional pre-computed Age-Size Key. Passed to [AtAge2AtSize()]
#'   as `ASKOverride`.
#'
#' @return `object` with `object@MeanAtLength` populated.
#' @keywords internal
MeanAtAge2MeanAtLength <- function(object, Length, replace=FALSE, max1=FALSE,
                                   Years=NULL, ASK=NULL) {
  if (!is.null(object@MeanAtLength) && !replace)
    return(object)
  CheckRequiredObject(Length, 'length')
  AtAge2AtSize(object, Length, max1, Years, ASK)
}

#' Populate `MeanAtWeight` from `MeanAtAge`
#'
#' Convenience wrapper around [AtAge2AtSize()] for the weight case. Returns
#' `object` unchanged if `MeanAtWeight` is already populated and
#' `replace = FALSE`.
#'
#' @param object An S4 object with slots `MeanAtAge` and `MeanAtWeight`.
#' @param Weight A [Weight()] object.
#' @param replace Logical. If `TRUE`, recomputes `MeanAtWeight` even if
#'   already populated. Default `FALSE`.
#' @param max1 Logical. Passed to [AtAge2AtSize()]. Default `FALSE`.
#' @param Years Numeric vector of years. Passed to [AtAge2AtSize()].
#' @param ASK Optional pre-computed Age-Size Key. Passed to [AtAge2AtSize()]
#'   as `ASKOverride`.
#'
#' @return `object` with `object@MeanAtWeight` populated.
#' @keywords internal
MeanAtAge2MeanAtWeight <- function(object, Weight, replace=FALSE, max1=FALSE,
                                   Years=NULL, ASK=NULL) {
  if (!is.null(object@MeanAtWeight) && !replace)
    return(object)
  CheckRequiredObject(Weight, 'weight')
  AtAge2AtSize(object, Weight, max1, Years, ASK)
}

