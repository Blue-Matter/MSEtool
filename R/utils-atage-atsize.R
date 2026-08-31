
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
.LinearInterpolateAge <- function(array, nSubAges=12) {
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
  
  .Aperm(array_inter, names(dimnames(array)))
}


#' Interpolate MeanAtAge and Recalculate Age-Size Key at Higher Age Resolution
#'
#' When the number of age classes is below `minAgeRes`, linearly interpolates
#' `ObjectMeanAtAge` to a finer age resolution via `.LinearInterpolateAge()`
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
.ResolveAgeResolution <- function(ObjectMeanAtAge, LengthMeanAtAge,
                                 LengthCVatAge, ASK, Length,
                                 nAge, minAgeRes, ASKOverride) {
  if (nAge >= minAgeRes)
    return(list(ObjectMeanAtAge=ObjectMeanAtAge, ASK=ASK))
  
  ObjectMeanAtAge <- .LinearInterpolateAge(ObjectMeanAtAge)
  
  ASK <- if (!is.null(ASKOverride)) {
    ASKOverride
  } else {
    CalcAgeSizeKey(
      MeanAtAge = .LinearInterpolateAge(LengthMeanAtAge),
      CVatAge   = .LinearInterpolateAge(LengthCVatAge),
      Classes   = Length@Classes,
      TruncSD   = Length@TruncSD,
      Dist      = Length@Dist,
      silent    = TRUE
    )
  }
  
  list(ObjectMeanAtAge=ObjectMeanAtAge, ASK=ASK)
}

#' Last-Observation-Carried-Forward
#'
#' Fills `NA` values in a numeric vector with the nearest preceding non-`NA`
#' value. Leading `NA`s (no preceding value) are left as `NA`.
#'
#' @param x Numeric vector, possibly containing `NA`.
#'
#' @return `x` with internal and trailing `NA` runs filled by carrying the
#'   last valid value forward.
#' @keywords internal
.CarryForwardLOCF <- function(x) {
  fill <- cummax(seq_along(x) * !is.na(x))
  x[ifelse(fill == 0, seq_along(x), fill)]
}

#' Convert Mean-at-Age to Mean-at-Size
#'
#' Populates `object@MeanAtLength` (or `object@MeanAtWeight`) by projecting
#' `object@MeanAtAge` through a column-standardised Age-Size Key (ALK or AWK)
#' from a [Length()] or [Weight()] object. If the number of age classes is
#' below `minAgeRes`, both `MeanAtAge` and the Age-Size Key are linearly
#' interpolated to a finer age resolution before conversion, ensuring the
#' at-age schedule has sufficient resolution to fill all size classes. Returns
#' `object` unchanged if `MeanAtLength`/`MeanAtWeight` is already populated.
#'
#' @param object An S4 object with slots `MeanAtAge` (array:
#'   `Sim × Age × Year` or `Sim × Age × Year × Area`) and `MeanAtLength`/
#'   `MeanAtWeight` (`NULL` if not yet populated, or a plain numeric vector
#'   of length `nClass`).
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
.AtAge2AtSize <- function(object, Length, max1=TRUE, Years=NULL,
                         ASKOverride=NULL, minAgeRes=50) {

  slotName   <- if (inherits(Length, 'weight')) 'MeanAtWeight' else 'MeanAtLength'
  MeanAtSize <- slot(object, slotName)

  if (!is.null(MeanAtSize)) {
    if (!is.array(MeanAtSize)) {
      Classes <- if (!EmptyObject(object@Classes)) object@Classes else Length@Classes
      if (length(MeanAtSize) != length(Classes))
        cli::cli_abort(c(
          "x" = "If `{slotName}` is a numeric vector it must have length matching `Classes` ({.val {length(Classes)}}).",
          "i" = "Error occurred on object of class {.cls {class(object)}}."
        ))
      Year <- dimnames(.GetASK(Length))[['Year']][1]
      MeanAtSize <- array(
        MeanAtSize,
        dim      = c(1L, length(Classes), 1L),
        dimnames = list(Sim=1, Class=Classes, Year=Year)
      )
      slot(object, slotName) <- MeanAtSize
      object@Classes <- Classes
    }
    return(object)
  }

  ObjectMeanAtAge <- object@MeanAtAge |> ReduceDims()
  LengthMeanAtAge <- Length@MeanAtAge |> ReduceDims()
  LengthCVatAge   <- Length@CVatAge   |> ReduceDims()
  
  ASK           <- .GetASK(Length)
  AgeClasses    <- as.numeric(dimnames(ASK)[['Age']])
  nAge          <- length(AgeClasses)
  LengthCVatAge <- ExtendAges(LengthCVatAge, AgeClasses)
  
  if (is.null(ASK) && is.null(ASKOverride))
    return(object)
  
  # Interpolate to finer age resolution if needed 
  resolved <- .ResolveAgeResolution(ObjectMeanAtAge, LengthMeanAtAge,
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
      class_sums <- colSums(ASK_sim_ts)
      col_sums   <- matrix(class_sums, nAge, nClass, byrow=TRUE)
      ASK_stand  <- ASK_sim_ts / col_sums
      ASK_stand[!is.finite(ASK_stand)] <- 0
      no_mass    <- class_sums == 0

      for (a in seq_len(nArea)) {
        MeanAtAge_vec <- if (hasArea) ObjectMeanAtAge[obj_s, , y, a] else
          ObjectMeanAtAge[obj_s, , y]

        atlength <- as.numeric(MeanAtAge_vec %*% ASK_stand)
        atlength[no_mass] <- NA
        atlength <- .CarryForwardLOCF(atlength)
        atlength <- rev(.CarryForwardLOCF(rev(atlength)))
        atlength[is.na(atlength)] <- 0

        if (hasArea) MeanAtLength[s, , y, a] <- atlength else
          MeanAtLength[s, , y]    <- atlength
      }
    }
  }
  
  if (!inherits(object, 'weight'))
    object@Classes <- Length@Classes
  slot(object, slotName) <- MeanAtLength
  object
}

#' Populate `MeanAtLength` from `MeanAtAge`
#'
#' Convenience wrapper around `.AtAge2AtSize()` for the length case. Returns
#' `object` unchanged if `MeanAtLength` is already populated and
#' `replace = FALSE`.
#'
#' @param object An S4 object with slots `MeanAtAge` and `MeanAtLength`.
#' @param Length A [Length()] object.
#' @param replace Logical. If `TRUE`, recomputes `MeanAtLength` even if
#'   already populated. Default `FALSE`.
#' @param max1 Logical. Passed to `.AtAge2AtSize()`. Default `FALSE`.
#' @param Years Numeric vector of years. Passed to `.AtAge2AtSize()`.
#' @param ASK Optional pre-computed Age-Size Key. Passed to `.AtAge2AtSize()`
#'   as `ASKOverride`.
#'
#' @return `object` with `object@MeanAtLength` populated.
#' @keywords internal
.MeanAtAge2MeanAtLength <- function(object, Length, replace=FALSE, max1=FALSE,
                                   Years=NULL, ASK=NULL) {
  if (!is.null(object@MeanAtLength) && !replace)
    return(object)
  .CheckRequiredObject(Length, 'length')
  .AtAge2AtSize(object, Length, max1, Years, ASK)
}

#' Populate `MeanAtWeight` from `MeanAtAge`
#'
#' Convenience wrapper around `.AtAge2AtSize()` for the weight case. Returns
#' `object` unchanged if `MeanAtWeight` is already populated and
#' `replace = FALSE`.
#'
#' @param object An S4 object with slots `MeanAtAge` and `MeanAtWeight`.
#' @param Weight A [Weight()] object.
#' @param replace Logical. If `TRUE`, recomputes `MeanAtWeight` even if
#'   already populated. Default `FALSE`.
#' @param max1 Logical. Passed to `.AtAge2AtSize()`. Default `FALSE`.
#' @param Years Numeric vector of years. Passed to `.AtAge2AtSize()`.
#' @param ASK Optional pre-computed Age-Size Key. Passed to `.AtAge2AtSize()`
#'   as `ASKOverride`.
#'
#' @return `object` with `object@MeanAtWeight` populated.
#' @keywords internal
.MeanAtAge2MeanAtWeight <- function(object, Weight, replace=FALSE, max1=FALSE,
                                   Years=NULL, ASK=NULL) {
  if (!is.null(object@MeanAtWeight) && !replace)
    return(object)
  .CheckRequiredObject(Weight, 'weight')
  .AtAge2AtSize(object, Weight, max1, Years, ASK)
}


#' Core At-Size to At-Age Conversion Loop
#'
#' Internal function called by `.AtSize2AtAge`. Multiplies `MeanAtSize` through
#' the transpose of the Age-Size Key for each simulation, year, and optionally
#' area. By the time this is called, `MeanAtSize` and `ASK` are already
#' extended to consistent `nSim` and `Years` dimensions.
#'
#' @param MeanAtAge Pre-allocated output array initialised to zero.
#'   Dimensions: `Sim × Age × Year` or `Sim × Age × Year × Area`.
#' @param MeanAtSize Numeric array of mean-at-size values.
#'   Dimensions: `Sim × Class × Year` or `Sim × Class × Year × Area`.
#' @param ASK Age-Size Key array. Dimensions: `Sim × Age × Class × Year`.
#' @param nSim Integer. Number of simulations.
#' @param nAge Integer. Number of age classes.
#' @param nTS Integer. Number of time steps.
#' @param nArea Integer. Number of areas. Use `1` when `byArea = FALSE`.
#' @param bySim Logical. If `TRUE`, uses a per-simulation ASK slice.
#'   If `FALSE`, uses slice 1 for all simulations.
#' @param byArea Logical. If `TRUE`, loops over areas using a 4-D
#'   `MeanAtSize`. Default `FALSE`.
#' @param allow_shortcut Logical. If `TRUE` (default), collapses a
#'   fully-saturated (all `> 0.99`) at-size slice to a constant `1`
#'   at-age value instead of computing the ALK-weighted sum
#'
#' @return `MeanAtAge` array with all values filled.
#' @keywords internal
.AtSize2AtAgeCore <- function(MeanAtAge, MeanAtSize, ASK,
                              nSim, nAge, nTS, nArea=1,
                              bySim=TRUE, byArea=FALSE, allow_shortcut=TRUE) {
  ask_sim <- function(sim) if (bySim) sim else 1L

  for (sim in seq_len(nSim)) {
    s <- ask_sim(sim)
    for (year in seq_len(nTS)) {
      ASK_ts <- ASK[s, , , year]
      if (byArea) {
        for (area in seq_len(nArea)) {
          mas <- MeanAtSize[sim, , year, area]
          MeanAtAge[sim, , year, area] <- if (allow_shortcut && all(mas > 0.99)) 1 else
            as.numeric(mas %*% t(ASK_ts))
        }
      } else {
        mas <- MeanAtSize[sim, , year]
        MeanAtAge[sim, , year] <- if (allow_shortcut && all(mas > 0.99)) 1 else
          as.numeric(mas %*% t(ASK_ts))
      }
    }
  }
  MeanAtAge
}

#' Resolve Consistent Sim and Year Dimensions
#'
#' Determines the union of simulation indices and years across two arrays,
#' then extends both to match. Used by `.AtSize2AtAge()` to align `MeanAtSize`
#' and the Age-Size Key before the conversion loop.
#'
#' @param MeanAtSize Numeric array with a `Sim` dimension name.
#' @param ASK Age-Size Key array with `Sim` and `Year` dimension names.
#'
#' @return A named list:
#'   - `MeanAtSize`: extended array.
#'   - `ASK`: extended array.
#'   - `nSim`: resolved integer number of simulations.
#'   - `sims`: integer vector of simulation indices.
#'   - `Years`: sorted numeric vector of the year union.
#' @keywords internal
.ResolveSimYearDims <- function(MeanAtSize, ASK) {
  dn_mas <- dimnames(MeanAtSize)
  dn_ask <- dimnames(ASK)
  
  sims_mas <- as.integer(dn_mas[['Sim']])
  sims_ask <- as.integer(dn_ask[['Sim']])
  
  if (length(sims_mas) == 1L && length(sims_ask) == 1L) {
    sims <- sims_ask
    nSim <- 1L
  } else {
    sims <- sort(unique(c(sims_mas, sims_ask)))
    nSim <- max(sims)
  }
  
  Years <- sort(unique(c(
    as.numeric(dn_mas[['Year']]),
    as.numeric(dn_ask[['Year']])
  )))
  Years <- Years[Years >= min(as.numeric(dn_mas[['Year']]))]
  
  list(
    MeanAtSize = MeanAtSize |> ExtendSims(nSim) |> ExtendYears(Years),
    ASK        = ASK        |> ExtendSims(nSim) |> ExtendYears(Years),
    nSim       = nSim,
    sims       = sims,
    Years      = Years
  )
}

#' Extract and Validate the Age-Size Key from a Length or Weight Object
#'
#' Retrieves the `ALK` slot from a [Length()] object or the `AWK` slot from a
#' [Weight()] object and aborts with an informative message if the slot is
#' unpopulated.
#'
#' @param Length A [Length()] or [Weight()] object.
#'
#' @return The Age-Size Key array.
#' @keywords internal
.GetASK <- function(Length) {
  if (.HasSlot(Length, 'ALK')) {
    ASK <- Length@ALK
    if (is.null(ASK))
      cli::cli_abort("`Length@ALK` is not populated", .internal=TRUE)
    return(ASK)
  }
  if (.HasSlot(Length, 'AWK')) {
    ASK <- Length@AWK
    if (is.null(ASK))
      cli::cli_abort("`Weight@AWK` is not populated", .internal=TRUE)
    return(ASK)
  }
  cli::cli_abort("`Length` has neither an `ALK` nor an `AWK` slot",
                 .internal=TRUE)
}

#' Convert Mean-at-Size to Mean-at-Age
#'
#' Populates `object@MeanAtAge` by passing `object@MeanAtLength` (or
#' `object@MeanAtWeight`) through the Age-Size Key stored in `Length`.
#' Handles optional simulation and area dimensions. Early-exits when all
#' values are effectively 1 or 0. Returns `object` unchanged if `MeanAtAge`
#' is already populated. 
#'
#' @param object An S4 object with slots `MeanAtLength` or `MeanAtWeight`
#'   (array: `Sim × Class × Year` or `Sim × Class × Year × Area`, or a plain
#'   numeric vector of length `nClass`) and `MeanAtAge` (`NULL` if not yet
#'   populated).
#' @param Length A [Length()] or [Weight()] object supplying an `ALK` or `AWK`
#'   slot (array: `Sim × Age × Class × Year`).
#' @param max1 Logical. If `TRUE`, forces `max(MeanAtAge) == 1` via
#'   `.CheckSelectivityMaximum`, correcting cases where selectivity-at-length
#'   produces an apical selectivity-at-age below 1. Default `FALSE`.
#' @param allow_shortcut Logical. If `TRUE` (default), skips the age-length-key
#'   math and fills `MeanAtAge` with a constant `1` (or `tiny`) when
#'   `MeanAtSize` is uniformly ~1 (or ~0). Valid only for proportion values
#'   like selectivity/retention. 
#'
#' @details
#' When `max1 = TRUE`, selectivity-at-age values are rescaled so the maximum
#' across age classes equals 1. This is sometimes necessary because a
#' selectivity-at-length schedule can produce a maximum selectivity-at-age
#' below 1, which is technically correct but can cause unexpected behaviour in
#' downstream calculations.
#'
#' @return `object` with `object@MeanAtAge` populated.
#' @keywords internal
.AtSize2AtAge <- function(object, Length, max1=FALSE, allow_shortcut=TRUE) {

  slotName <- if (inherits(Length, 'length')) {
    'MeanAtLength'
  } else if (inherits(Length, 'weight')) {
    'MeanAtWeight'
  } else {
    cli::cli_abort("`Length` must be an object of class `length` or `weight`")
  }

  MeanAtSize <- slot(object, slotName)

  Classes <- if (is.array(MeanAtSize) && !is.null(dimnames(MeanAtSize)[['Class']])) {
    as.numeric(dimnames(MeanAtSize)[['Class']])
  } else if (!EmptyObject(object@Classes)) {
    object@Classes
  } else {
    Length@Classes
  }

  # If MeanAtSize is defined on different classes than Length, the ALK/AWK
  # must be recalculated on those classes rather than using Length's own key.
  ASK <- if (isTRUE(all.equal(Classes, Length@Classes))) {
    .GetASK(Length)
  } else {
    CalcAgeSizeKey(
      MeanAtAge = Length@MeanAtAge,
      CVatAge   = Length@CVatAge,
      Classes   = Classes,
      TruncSD   = Length@TruncSD,
      Dist      = Length@Dist,
      silent    = TRUE
    )
  }

  if (!is.array(MeanAtSize)) {
    if (length(MeanAtSize) != length(Classes))
      cli::cli_abort(c(
        "x" = "If `{slotName}` is a numeric vector it must have length matching `Classes` ({.val {length(Classes)}}).",
        "i" = "Error occurred on object of class {.cls {class(object)}}."
      ))
    Year <- dimnames(ASK)[['Year']][1]
    MeanAtSize <- array(
      MeanAtSize,
      dim      = c(1L, length(Classes), 1L),
      dimnames = list(Sim=1, Class=Classes, Year=Year)
    )
    slot(object, slotName) <- MeanAtSize
    object@Classes <- Classes
  }

  byArea <- "Area" %in% names(dimnames(MeanAtSize))
  nArea  <- if (byArea) dim(MeanAtSize)[4] else 1L

  #  Early exits
  alloc_and_fill <- function(val) {
    dims <- .ResolveSimYearDims(MeanAtSize, ASK)
    AgeClasses <- as.numeric(dimnames(dims$ASK)[['Age']])
    dn <- list(Sim=dims$sims, Age=AgeClasses, Year=dims$Years)
    if (byArea) dn$Area <- seq_len(nArea)
    out <- array(val, dim=lengths(dn), dimnames=dn)
    object@MeanAtAge <<- out
  }

  if (allow_shortcut) {
    if (all(MeanAtSize > 0.99)) { alloc_and_fill(1);    return(object) }
    if (all(MeanAtSize < 0.01)) { alloc_and_fill(tiny); return(object) }
  }
  
  # Resolve dims 
  dims       <- .ResolveSimYearDims(MeanAtSize, ASK)
  MeanAtSize <- dims$MeanAtSize
  ASK        <- dims$ASK
  nSim       <- dims$nSim
  sims       <- dims$sims
  Years      <- dims$Years
  
  AgeClasses <- as.numeric(dimnames(ASK)[['Age']])
  nAge       <- length(AgeClasses)
  nTS        <- length(Years)
  
  # Allocate output 
  dn <- list(Sim=sims, Age=AgeClasses, Year=Years)
  if (byArea) dn$Area <- seq_len(nArea)
  MeanAtAge <- array(0, dim=lengths(dn), dimnames=dn)
  
  object@MeanAtAge <- .AtSize2AtAgeCore(
    MeanAtAge, MeanAtSize, ASK,
    nSim=nSim, nAge=nAge, nTS=nTS, nArea=nArea,
    bySim="Sim" %in% names(dimnames(MeanAtSize)),
    byArea=byArea,
    allow_shortcut=allow_shortcut
  )
  
  if (max1)
    object <- .CheckSelectivityMaximum(object)
  
  object
}

#' Convert Mean-at-Size to Mean-at-Age, Weighted by Selectivity
#'
#' Like `.AtSize2AtAge()`, but weights the age-length key by a second object's
#' at-size schedule (typically selectivity) before collapsing to age, i.e.
#' `object(age) = sum_L[ P(L|age) Weighting(L) object(L) ] / sum_L[ P(L|age)
#' Weighting(L) ]`. This avoids biasing the at-age result toward the
#' unconditional population length distribution at age when `object`
#' (e.g. retention) is only meaningful conditional on capture, and capture is
#' itself size-selective. Falls back to the unweighted `.AtSize2AtAge()` result
#' wherever the weighted ratio is non-finite (e.g. no weight in that age
#' class) or `Weighting`'s at-size schedule is unavailable.
#'
#' @param object An S4 object with `MeanAtLength`/`MeanAtWeight` and
#'   `MeanAtAge` slots (e.g. a [retention-class] or [weight-class] object).
#' @param Weighting An S4 object supplying the weighting at-size schedule
#'   (e.g. a populated [selectivity-class] object).
#' @param Length A [Length()] or [Weight()] object supplying the ALK/AWK.
#'
#' @return `object` with `object@MeanAtAge` populated.
#' @keywords internal
.WeightedAtSize2AtAge <- function(object, Weighting, Length) {
  slotName <- if (inherits(Length, 'weight')) 'MeanAtWeight' else 'MeanAtLength'

  wt_at_size  <- slot(Weighting, slotName)
  obj_at_size <- slot(object, slotName)

  fallback <- .AtSize2AtAge(object, Length, allow_shortcut=FALSE)

  if (is.null(wt_at_size) || is.null(obj_at_size))
    return(fallback)

  if ('Area' %in% names(dimnames(wt_at_size)))
    wt_at_size <- DropDimension(wt_at_size, 'Area', warn = FALSE)

  # constant input: skip division to avoid floating-point rounding outside range
  const_val <- obj_at_size[1]
  if (all(abs(obj_at_size - const_val) < 1e-9, na.rm = TRUE)) {
    object@MeanAtAge <- fallback@MeanAtAge
    object@MeanAtAge[] <- const_val
    return(object)
  }

  NumObj <- object
  slot(NumObj, slotName) <- ArrayMultiply(wt_at_size, obj_at_size)

  DenObj <- object
  slot(DenObj, slotName) <- wt_at_size

  num_at_age <- .AtSize2AtAge(NumObj, Length, allow_shortcut=FALSE)@MeanAtAge
  den_at_age <- .AtSize2AtAge(DenObj, Length, allow_shortcut=FALSE)@MeanAtAge

  eff <- num_at_age / den_at_age
  negligible <- abs(den_at_age) < 1e-8 * max(abs(den_at_age), na.rm = TRUE)
  bad <- !is.finite(eff) | negligible

  fallbackAtAge <- Extend(
    fallback@MeanAtAge,
    nSim  = dim(eff)[names(dimnames(eff)) == 'Sim'],
    Years = as.numeric(dimnames(eff)[['Year']]),
    backfill = TRUE
  )
  eff[bad] <- fallbackAtAge[bad]

  object@MeanAtAge <- eff
  object
}

#' Populate `MeanAtAge` from `MeanAtLength`
#'
#' Convenience wrapper around `.AtSize2AtAge()` for the length case. Returns
#' `object` unchanged if `MeanAtLength` is `NULL` or `MeanAtAge` is already
#' populated.
#'
#' @param object An S4 object with slots `MeanAtLength` and `MeanAtAge`.
#' @param Length A [Length()] object.
#' @param max1 Logical. Passed to `.AtSize2AtAge()`. Default `FALSE`.
#'
#' @return `object` with `object@MeanAtAge` populated.
#' @keywords internal
.MeanAtLength2MeanAtAge <- function(object, Length, max1=FALSE) {
  if (is.null(object@MeanAtLength)) return(object)
  if (!is.null(object@MeanAtAge))   return(object)
  .CheckRequiredObject(Length, 'length')
  .AtSize2AtAge(object, Length, max1)
}

#' Populate `MeanAtAge` from `MeanAtWeight`
#'
#' Convenience wrapper around `.AtSize2AtAge()` for the weight case. Returns
#' `object` unchanged if `MeanAtWeight` is `NULL` or `MeanAtAge` is already
#' populated.
#'
#' @param object An S4 object with slots `MeanAtWeight` and `MeanAtAge`.
#' @param Weight A [Weight()] object.
#' @param max1 Logical. Passed to `.AtSize2AtAge()`. Default `FALSE`.
#'
#' @return `object` with `object@MeanAtAge` populated.
#' @keywords internal
.MeanAtWeight2MeanAtAge <- function(object, Weight, max1=FALSE) {
  if (is.null(object@MeanAtWeight)) return(object)
  if (!is.null(object@MeanAtAge))   return(object)
  .CheckRequiredObject(Weight, 'weight')
  .AtSize2AtAge(object, Weight, max1)
}
