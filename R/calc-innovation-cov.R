#' Calculate Innovation Covariance Matrix for Multivariate AR(1)
#'
#' Computes the innovation covariance matrix (\eqn{\Sigma_\epsilon}) for a
#' multivariate AR(1) process given the stationary covariance matrix
#' (\eqn{\Sigma_Z}) and lag-1 autocorrelation coefficients (\eqn{\phi}).
#'
#'
#' @param Sigma_Z Numeric covariance matrix (`nStock` x `nStock`) representing the
#'   stationary covariance of the process.
#' @param phi Numeric vector of length `nStock` containing lag-1 autocorrelation
#'   coefficients for each stock.
#'
#' @return A numeric covariance matrix (`nStock` x `nStock`) representing the
#'   innovation covariance (\eqn{\Sigma_\epsilon}). Rows and columns
#'   corresponding to inactive stocks (zero diagonal in `Sigma_Z`) are set
#'   to zero.
#'
#' @details
#' 
#' The relationship used is:
#'
#' \deqn{
#'   \Sigma_\epsilon = \Sigma_Z - \Phi \Sigma_Z \Phi^T
#' }
#'
#' where \eqn{\Phi} is a diagonal matrix of autocorrelation coefficients.
#'
#' Only stocks with positive stationary variance (active stocks) are included
#' in the computation. The result is embedded back into a full zero matrix,
#' so inactive stocks contribute no innovation variance.
#'
#' The active submatrix is forced to be symmetric and positive semi-definite.
#' If negative eigenvalues are detected, [Matrix::nearPD()] is used to find
#' the nearest PSD matrix. After correction, the diagonal is capped at the
#' theoretical upper bound \eqn{\sigma^2_{Z,i}(1 - \phi_i^2)} to prevent
#' [Matrix::nearPD()] from inflating innovation variance beyond what is
#' consistent with the stationary distribution.
#' 
#'
#' @examples
#' Sigma_Z <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
#' phi <- c(0.7, 0.6)
#'
#' CalcInnovationCov(Sigma_Z, phi)
#' 
#'
#' @seealso [GenerateStockTargeting()], [FitStockTargeting()]
#' 
#' @export
CalcInnovationCov <- function(Sigma_Z, phi) {
  
  CheckPackage('MASS')
  
  n <- length(phi)
  phi <- pmin(pmax(phi, -0.999), 0.999)
  Phi <- diag(phi, n)
  
  active <- diag(Sigma_Z) > 0
  Sigma_eps <- matrix(0, n, n, dimnames = dimnames(Sigma_Z))
  
  if (!any(active)) return(Sigma_eps)
  
  phi_a   <- phi[active]
  Phi_a   <- diag(phi_a, sum(active))
  Sigma_a <- Sigma_Z[active, active, drop = FALSE]
  
  Sigma_eps_a <- Sigma_a - Phi_a %*% Sigma_a %*% t(Phi_a)
  
  # force symmetry
  Sigma_eps_a <- (Sigma_eps_a + t(Sigma_eps_a)) / 2
  
  # eigenvalue correction 
  eig <- eigen(Sigma_eps_a, symmetric = TRUE)
  if (any(eig$values <= 0)) {
    Sigma_eps_a <- as.matrix(Matrix::nearPD(
      Sigma_eps_a,
      corr    = FALSE,
      keepDiag = FALSE      
    )$mat)
    
    expected_diag <- diag(Sigma_a) * (1 - phi_a^2)
    diag(Sigma_eps_a) <- pmin(diag(Sigma_eps_a), expected_diag)
    # re-symmetrise after diagonal adjustment
    Sigma_eps_a <- (Sigma_eps_a + t(Sigma_eps_a)) / 2
    
    # ensure strict PD after all corrections
    min_eig <- min(eigen(Sigma_eps_a, symmetric = TRUE)$values)
    if (min_eig <= 0) 
      Sigma_eps_a <- Sigma_eps_a + diag(abs(min_eig) + 1e-8, nrow(Sigma_eps_a))
    
  }
  Sigma_eps[active, active] <- Sigma_eps_a
  Sigma_eps
}
