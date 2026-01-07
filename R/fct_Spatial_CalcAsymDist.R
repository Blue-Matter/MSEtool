#' Calculate Asymptotic Spatial Distribution
#'
#' @param Movement A movement matrix with dimensions `c(nArea, nArea)`  where the
#' diagonals are the probability of staying in an area, and off-diagonals are the
#' probability of moving from area in row and column `i` to area in row `i` and column `j`
#'
#' @return A numeric vector of length `nArea` with the asymptotic distribution
#' @rdname CalcMovement
#' @export
CalcAsymDist <- function(Movement, method=c('la', 'mc'), tol=1E-10, maxiter=1E4) {

  if (!is.matrix(Movement) || nrow(Movement) != ncol(Movement)) {
    cli::cli_abort("`Movement` must be a square matrix")
  }
  
  if (any(abs(rowSums(Movement) - 1) > tol)) {
    cli::cli_abort("Each row of `Movement` must sum to 1")
  }
  
  if (dim(Movement)[1]==2) {
    return(CalcAsymDist_2_Area(Movement, tol))
  }
  
  CalcAsymDist_Multi_Area(Movement, method, tol, maxiter)
}

CalcAsymDist_2_Area <- function(Movement, tol=1E-10) {
  
  if (!is.matrix(Movement) || any(dim(Movement) != c(2, 2))) {
    cli::cli_abort("`Movement` must be a 2x2 matrix")
  }
  
  # Closed form solution
  P_1_2 <- Movement[1,2] # prob moving from area 1 to area 2
  P_2_1 <- Movement[2,1] # prob moving from area 2 to area 1
  
  if (P_1_2 + P_2_1 < tol) {
    cli::cli_abort("Chain is reducible: no movement between areas")
  }
  
  Area_1 <- P_2_1 / (P_1_2 + P_2_1)
  Area_2 <- P_1_2 / (P_1_2 + P_2_1)
  
  c(Area_1, Area_2)
}

CalcAsymDist_Multi_Area <- function(Movement, method=c('la', 'mc'), tol=1E-10, maxiter=1E4) {
  method <- match.arg(method, c('la', 'mc'))
  nArea <- nrow(Movement)
  
  
  if (method=='la') {
    # Linear-algebra solution 
    A <- rbind(t(Movement) - diag(nArea), rep(1, nArea))
    B <- c(rep(0, nArea), 1)
    Prob_Area <- solve(t(A) %*% A, t(A) %*% B) |> as.numeric()
    Prob_Area[Prob_Area < 0 & abs(Prob_Area) < tol] <- 0
    return(Prob_Area / sum(Prob_Area))
  }

  # Markov Chain iteration approach
  Prob_Area <- rep(1/nArea, nArea)
  
  for (i in seq_len(maxiter)) {
    Prob_Area_New <- as.numeric(Prob_Area %*% Movement)
    if (max(abs(Prob_Area_New - Prob_Area)) < tol) {
      return(Prob_Area_New)
    }
    Prob_Area <- Prob_Area_New
  }
  
  cli::cli_alert_warning("`CalcAsymDist` did not converge within `maxiter` ({.val {maxiter}})")
  Prob_Area
  
}

