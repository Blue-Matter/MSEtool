#' Calculate the Asymptotic Spatial Distribution of a Movement Matrix
#'
#' Computes the stationary (asymptotic) distribution of a Markov chain
#' movement matrix — the long-run proportion of the population expected to
#' occupy each area. For a 2-area matrix a closed-form solution is used. For
#' larger matrices, either a linear-algebra or iterative Markov chain approach
#' is available.
#'
#' @details
#' The movement matrix is treated as a row-stochastic transition matrix, where
#' `Movement[i, j]` is the probability of moving from area `i` to area `j`.
#' Diagonal entries represent the probability of remaining in an area. All
#' rows must sum to 1.
#'
#' Two solution methods are available for matrices larger than 2 × 2 via the
#' `method` argument:
#'
#' - `"la"` (default): solves the system `pi * P = pi` subject to
#'   `sum(pi) = 1` using least-squares linear algebra (`solve(t(A) %*% A,
#'   t(A) %*% B)`). Exact and fast for well-conditioned matrices.
#' - `"mc"`: iteratively multiplies the current distribution by `Movement`
#'   until convergence. More robust for nearly-reducible chains but slower.
#'
#' For 2-area matrices the stationary distribution is computed analytically
#' regardless of `method`:
#'
#' `pi[1] = P[2,1] / (P[1,2] + P[2,1])`
#'
#' `pi[2] = P[1,2] / (P[1,2] + P[2,1])`
#'
#' @param Movement A square row-stochastic matrix of dimensions `nArea ×
#'   nArea`, where `Movement[i, j]` is the probability of moving from area
#'   `i` to area `j`. All rows must sum to 1.
#' @param method Character. Solution method for matrices larger than 2 × 2.
#'   One of `"la"` (linear algebra; default) or `"mc"` (Markov chain
#'   iteration). Partially matched.
#' @param tol Numeric. Convergence tolerance. Used to check row sums, detect
#'   reducible 2-area chains, clamp near-zero negative probabilities, and
#'   assess convergence of the `"mc"` method. Default `1e-10`.
#' @param maxiter Integer. Maximum number of iterations for the `"mc"` method.
#'   A warning is emitted if convergence is not reached. Default `1e4`.
#'
#' @return A numeric vector of length `nArea` giving the stationary
#'   probability of occupying each area. Values sum to 1.
#'
#' @examples
#' # 2-area movement matrix
#' M <- matrix(c(0.8, 0.2,
#'               0.3, 0.7), nrow=2, byrow=TRUE)
#' CalcAsymDist(M)
#'
#' # 3-area movement matrix using linear algebra (default)
#' M3 <- matrix(c(0.7, 0.2, 0.1,
#'                0.1, 0.8, 0.1,
#'                0.2, 0.2, 0.6), nrow=3, byrow=TRUE)
#' CalcAsymDist(M3)
#'
#' # Using Markov chain iteration
#' CalcAsymDist(M3, method="mc")
#'
#' @seealso [Spatial()], [CalcUnfishedDist()]
#' @export
CalcAsymDist <- function(Movement, method=c('la', 'mc'), tol=1e-10, maxiter=1e4) {
  if (!is.matrix(Movement) || nrow(Movement) != ncol(Movement))
    cli::cli_abort("{.arg Movement} must be a square matrix.")
  
  if (any(abs(rowSums(Movement) - 1) > tol))
    cli::cli_abort("Each row of {.arg Movement} must sum to 1.")
  
  if (nrow(Movement) == 2L)
    return(.CalcAsymDist2Area(Movement, tol))
  
  .CalcAsymDistMultiArea(Movement, method, tol, maxiter)
}

#' Closed-Form Asymptotic Distribution for a 2-Area Movement Matrix
#'
#' Computes the stationary distribution analytically for a 2 × 2
#' row-stochastic movement matrix. Called by [CalcAsymDist()] when
#' `nrow(Movement) == 2`.
#'
#' @param Movement A 2 × 2 row-stochastic matrix.
#' @param tol Numeric. Tolerance for detecting a reducible chain (both
#'   off-diagonal entries near zero). Default `1e-10`.
#'
#' @return A numeric vector of length 2.
#' @keywords internal
.CalcAsymDist2Area <- function(Movement, tol=1e-10) {
  if (!is.matrix(Movement) || any(dim(Movement) != c(2L, 2L)))
    cli::cli_abort("{.arg Movement} must be a 2 \u00d7 2 matrix.")
  
  P_1_2 <- Movement[1, 2]   # probability of moving from area 1 to area 2
  P_2_1 <- Movement[2, 1]   # probability of moving from area 2 to area 1
  
  if (P_1_2 + P_2_1 < tol)
    cli::cli_abort("Chain is reducible: no movement between areas.")
  
  c(P_2_1 / (P_1_2 + P_2_1),
    P_1_2 / (P_1_2 + P_2_1))
}

#' Asymptotic Distribution for a Multi-Area Movement Matrix
#'
#' Computes the stationary distribution of a row-stochastic movement matrix
#' with 3 or more areas using either a linear-algebra or iterative Markov
#' chain approach. Called by [CalcAsymDist()] when `nrow(Movement) > 2`.
#'
#' @param Movement A square row-stochastic matrix with `nArea >= 3` rows.
#' @param method Character. One of `"la"` (linear algebra; default) or `"mc"`
#'   (Markov chain iteration). Partially matched.
#' @param tol Numeric. Convergence tolerance and threshold for clamping
#'   near-zero negative values. Default `1e-10`.
#' @param maxiter Integer. Maximum iterations for `method = "mc"`. Default
#'   `1e4`.
#'
#' @return A numeric vector of length `nArea` summing to 1. Emits a warning
#'   if `method = "mc"` does not converge within `maxiter`.
#' @keywords internal
.CalcAsymDistMultiArea <- function(Movement, method=c('la', 'mc'),
                                   tol=1e-10, maxiter=1e4) {
  method <- match.arg(method)
  nArea  <- nrow(Movement)
  
  if (method == 'la') {
    A         <- rbind(t(Movement) - diag(nArea), rep(1, nArea))
    B         <- c(rep(0, nArea), 1)
    Prob_Area <- as.numeric(solve(t(A) %*% A, t(A) %*% B))
    Prob_Area[Prob_Area < 0 & abs(Prob_Area) < tol] <- 0
    return(Prob_Area / sum(Prob_Area))
  }
  
  Prob_Area <- rep(1 / nArea, nArea)
  for (i in seq_len(maxiter)) {
    Prob_Area_New <- as.numeric(Prob_Area %*% Movement)
    if (max(abs(Prob_Area_New - Prob_Area)) < tol)
      return(Prob_Area_New)
    Prob_Area <- Prob_Area_New
  }
  
  cli::cli_alert_warning(
    "`CalcAsymDist` did not converge within {.val {as.integer(maxiter)}} iterations."
  )
  Prob_Area
}
