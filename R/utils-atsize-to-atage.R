#' Core At-Size to At-Age Conversion Loop
#'
#' Internal function called by `AtSize2AtAge`. Multiplies `MeanAtSize` through
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
#'
#' @return `MeanAtAge` array with all values filled.
#' @keywords internal
AtSize2AtAge_core <- function(MeanAtAge, MeanAtSize, ASK,
                              nSim, nAge, nTS, nArea=1,
                              bySim=TRUE, byArea=FALSE) {
  ask_sim <- function(sim) if (bySim) sim else 1L
  
  for (sim in seq_len(nSim)) {
    s <- ask_sim(sim)
    for (year in seq_len(nTS)) {
      ASK_ts <- ASK[s, , , year]
      if (byArea) {
        for (area in seq_len(nArea)) {
          mas <- MeanAtSize[sim, , year, area]
          MeanAtAge[sim, , year, area] <- if (all(mas > 0.99)) 1 else
            as.numeric(mas %*% t(ASK_ts))
        }
      } else {
        mas <- MeanAtSize[sim, , year]
        MeanAtAge[sim, , year] <- if (all(mas > 0.99)) 1 else
          as.numeric(mas %*% t(ASK_ts))
      }
    }
  }
  MeanAtAge
}

#' Resolve Consistent Sim and Year Dimensions
#'
#' Determines the union of simulation indices and years across two arrays,
#' then extends both to match. Used by [AtSize2AtAge()] to align `MeanAtSize`
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
ResolveSimYearDims <- function(MeanAtSize, ASK) {
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
GetASK <- function(Length) {
  if (hasSlot(Length, 'ALK')) {
    ASK <- Length@ALK
    if (is.null(ASK))
      cli::cli_abort("`Length@ALK` is not populated", .internal=TRUE)
    return(ASK)
  }
  if (hasSlot(Length, 'AWK')) {
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
#'   (array: `Sim × Class × Year` or `Sim × Class × Year × Area`) and
#'   `MeanAtAge` (`NULL` if not yet populated).
#' @param Length A [Length()] or [Weight()] object supplying an `ALK` or `AWK`
#'   slot (array: `Sim × Age × Class × Year`).
#' @param max1 Logical. If `TRUE`, forces `max(MeanAtAge) == 1` via
#'   `CheckSelectivityMaximum`, correcting cases where selectivity-at-length
#'   produces an apical selectivity-at-age below 1. Default `FALSE`.
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
AtSize2AtAge <- function(object, Length, max1=FALSE) {
  
  MeanAtSize <- if (inherits(Length, 'length')) {
    object@MeanAtLength
  } else if (inherits(Length, 'weight')) {
    object@MeanAtWeight
  } else {
    cli::cli_abort("`Length` must be an object of class `length` or `weight`")
  }
  
  ASK    <- GetASK(Length)
  byArea <- "Area" %in% names(dimnames(MeanAtSize))
  nArea  <- if (byArea) dim(MeanAtSize)[4] else 1L
  
  #  Early exits 
  alloc_and_fill <- function(val) {
    dims <- ResolveSimYearDims(MeanAtSize, ASK)
    AgeClasses <- as.numeric(dimnames(dims$ASK)[['Age']])
    dn <- list(Sim=dims$sims, Age=AgeClasses, Year=dims$Years)
    if (byArea) dn$Area <- seq_len(nArea)
    out <- array(val, dim=lengths(dn), dimnames=dn)
    object@MeanAtAge <<- out
  }
  
  if (all(MeanAtSize > 0.99)) { alloc_and_fill(1);    return(object) }
  if (all(MeanAtSize < 0.01)) { alloc_and_fill(tiny); return(object) }
  
  # Resolve dims 
  dims       <- ResolveSimYearDims(MeanAtSize, ASK)
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
  
  object@MeanAtAge <- AtSize2AtAge_core(
    MeanAtAge, MeanAtSize, ASK,
    nSim=nSim, nAge=nAge, nTS=nTS, nArea=nArea,
    bySim="Sim" %in% names(dimnames(MeanAtSize)),
    byArea=byArea
  )
  
  if (max1)
    object <- CheckSelectivityMaximum(object)
  
  object
}

#' Populate `MeanAtAge` from `MeanAtLength`
#'
#' Convenience wrapper around [AtSize2AtAge()] for the length case. Returns
#' `object` unchanged if `MeanAtLength` is `NULL` or `MeanAtAge` is already
#' populated.
#'
#' @param object An S4 object with slots `MeanAtLength` and `MeanAtAge`.
#' @param Length A [Length()] object.
#' @param max1 Logical. Passed to [AtSize2AtAge()]. Default `FALSE`.
#'
#' @return `object` with `object@MeanAtAge` populated.
#' @keywords internal
MeanAtLength2MeanAtAge <- function(object, Length, max1=FALSE) {
  if (is.null(object@MeanAtLength)) return(object)
  if (!is.null(object@MeanAtAge))   return(object)
  CheckRequiredObject(Length, 'length')
  AtSize2AtAge(object, Length, max1)
}

#' Populate `MeanAtAge` from `MeanAtWeight`
#'
#' Convenience wrapper around [AtSize2AtAge()] for the weight case. Returns
#' `object` unchanged if `MeanAtWeight` is `NULL` or `MeanAtAge` is already
#' populated.
#'
#' @param object An S4 object with slots `MeanAtWeight` and `MeanAtAge`.
#' @param Weight A [Weight()] object.
#' @param max1 Logical. Passed to [AtSize2AtAge()]. Default `FALSE`.
#'
#' @return `object` with `object@MeanAtAge` populated.
#' @keywords internal
MeanAtWeight2MeanAtAge <- function(object, Weight, max1=FALSE) {
  if (is.null(object@MeanAtWeight)) return(object)
  if (!is.null(object@MeanAtAge))   return(object)
  CheckRequiredObject(Weight, 'weight')
  AtSize2AtAge(object, Weight, max1)
}

