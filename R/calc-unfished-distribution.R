#' Calculate the Unfished Spatial Distribution
#'
#' Computes the asymptotic unfished spatial distribution for each simulation,
#' age class, and year by applying [CalcAsymDist()] to each `nArea × nArea`
#' slice of `Spatial@Movement`. The result is stored in
#' `Spatial@UnfishedDist`.
#'
#' For a single-area model, all values are set to 1. For multi-area models,
#' the stationary distribution of the movement matrix is computed for each
#' `Sim × Age × Year` combination using [CalcAsymDist()].
#'
#' @param Spatial A [Spatial()] object with a populated `Movement` slot. The
#'   `Movement` array must have dimensions `Sim × FromArea × ToArea × Age × Year`.
#' @param Ages An [ages-class] object supplying age class labels. If `NULL`
#'   (default), [DefaultAges()] is used.
#' @param Years Numeric vector of year labels. If `NULL` (default),
#'   [DefaultYears()] is used.
#'
#' @return `Spatial` with `Spatial@UnfishedDist` populated as a named
#'   `Sim × Area × Age × Year` array, where each `Area` vector gives the
#'   stationary probability of occupying each area under unfished conditions.
#'
#' @seealso [CalcAsymDist()], [Spatial()]
#' @export
CalcUnfishedDist <- function(Spatial, Ages=NULL, Years=NULL) {
  Ages  <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  dims <- dim(Spatial@Movement)
  if (is.null(dims))
    return(Spatial)
  
  nSim  <- dims[1]
  nArea <- dims[2]
  nAge  <- dims[4]
  nYear <- dims[5]
  
  UnfishedDist <- array(
    1,
    dim      = c(nSim, nArea, nAge, nYear),
    dimnames = list(
      Sim  = seq_len(nSim),
      Area = seq_len(nArea),
      Age  = Ages@Classes[seq_len(nAge)],
      Year = Years[seq_len(nYear)]
    )
  )
  
  if (nArea == 1L) {
    Spatial@UnfishedDist <- UnfishedDist
    return(Spatial)
  }
  
  # For each Sim, apply CalcAsymDist across the Age x Year slices of the
  # nArea x nArea movement sub-matrix. aperm() restores Area to dimension 1
  # after apply() collapses it to the last position.
  for (s in seq_len(nSim)) {
    UnfishedDist[s, , , ] <- apply(
      Spatial@Movement[s, , , , ],
      MARGIN  = c(3, 4),   # Age x Year
      FUN     = CalcAsymDist
    ) |> aperm(c(3, 1, 2))  # Area x Age x Year -> Area first
  }
  
  Spatial@UnfishedDist <- UnfishedDist
  Spatial
}