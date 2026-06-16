#' Calculate the Unfished Spatial Distribution
#'
#' Computes the unfished spatial distribution for each simulation, age class,
#' and year from `Spatial@Movement`. The result is stored in
#' `Spatial@UnfishedDist`.
#'
#' For a single-area model, all values are set to 1. For multi-area models,
#' two modes are available:
#'
#' - **Age-invariant movement** (`Movement` Age dimension = 1): the stationary
#'   (asymptotic) distribution of the single movement matrix is computed and
#'   applied uniformly across all age classes using [CalcAsymDist()].
#'
#' - **Age-varying movement** (`Movement` Age dimension > 1): a sequential
#'   chain propagates the analyst-specified recruit distribution forward through
#'   age classes. For age $a$:
#'   \deqn{\boldsymbol{\pi}_a = \boldsymbol{\pi}_{a-1} \mathbf{M}_{a-1}}
#'   The plus-group distribution is computed as the stationary distribution of
#'   `M[nAge]`, since fish remain in that age class across multiple time steps.
#'   The starting point $\boldsymbol{\pi}_1$ is taken from the recruit
#'   distribution already stored in `Spatial@UnfishedDist[, , 1, ]`.
#'
#' @param Spatial A [Spatial()] object with a populated `Movement` slot. The
#'   `Movement` array must have dimensions `Sim × FromArea × ToArea × Age × Year`.
#' @param Ages An [ages-class] object supplying age class labels. If `NULL`
#'   (default), [DefaultAges()] is used.
#' @param Years Numeric vector of year labels. If `NULL` (default),
#'   [DefaultYears()] is used.
#'
#' @return `Spatial` with `Spatial@UnfishedDist` populated as a named
#'   `Sim × Area × Age × Year` array.
#'
#' @seealso [CalcAsymDist()], [FitMovement()], [Spatial()]
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

  if (nAge == 1L) {
    # Age-invariant movement: stationary distribution applies to all age classes.
    for (s in seq_len(nSim)) {
      UnfishedDist[s, , , ] <- apply(
        abind::adrop(Spatial@Movement[s, , , , , drop=FALSE], 1),
        MARGIN = c(3, 4),   # Age x Year
        FUN = function(x) CalcAsymDist(matrix(x, nrow=nArea, ncol=nArea))
      ) # Area x Age x Year
    }
  } else {
    # Age-varying movement: sequential chain.
    # π_1 from analyst spec (already in UnfishedDist[,, 1,]);
    # π_a = π_{a-1} %*% M_{a-1} for a = 2..(nAge-1);
    # π_nAge = stationary of M[nAge] (plus group applies same matrix repeatedly).
    for (s in seq_len(nSim)) {
      for (yr in seq_len(nYear)) {
        # Age 1: keep analyst-specified recruit distribution from existing slot
        pi_prev <- Spatial@UnfishedDist[s, , 1L, yr]

        UnfishedDist[s, , 1L, yr] <- pi_prev

        # Ages 2..(nAge-1): one-step propagation
        for (a in seq_len(nAge - 1L) + 1L) {
          M_a_prev <- matrix(Spatial@Movement[s, , , a - 1L, yr], nrow=nArea)
          pi_curr  <- as.vector(pi_prev %*% M_a_prev)
          pi_curr  <- pi_curr / sum(pi_curr)   # normalise for numerical safety
          UnfishedDist[s, , a, yr] <- pi_curr
          pi_prev  <- pi_curr
        }

        # Plus group: stationary distribution of M[nAge]
        M_plus <- matrix(Spatial@Movement[s, , , nAge, yr], nrow=nArea)
        UnfishedDist[s, , nAge, yr] <- CalcAsymDist(M_plus)
      }
    }
  }

  Spatial@UnfishedDist <- UnfishedDist
  Spatial
}