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
#' @return A numeric covariance matrix(`nStock` x `nStock`) representing the
#'   innovation covariance (\eqn{\Sigma_\epsilon}).
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
#' The resulting matrix is forced to be symmetric and positive semi-definite
#' using eigenvalue truncation.
#' 
#'
#' @examples
#' Sigma_Z <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
#' phi <- c(0.7, 0.6)
#'
#' CalcInnovationCov(Sigma_Z, phi)
#'
#' @export
CalcInnovationCov <- function(Sigma_Z, phi) {
  n <- length(phi)
  phi <- pmin(pmax(phi, -0.999), 0.999)
  Phi <- diag(phi, n)
  
  Sigma_eps <- Sigma_Z - Phi %*% Sigma_Z %*% Phi
  
  # force symmetry
  Sigma_eps <- (Sigma_eps + t(Sigma_eps)) / 2
  
  # eigenvalue correction 
  eig <- eigen(Sigma_eps, symmetric = TRUE)
  
  if (any(eig$values <= 0)) {
    Sigma_eps <- as.matrix(Matrix::nearPD(
      Sigma_eps,
      corr = FALSE,
      keepDiag = TRUE
    )$mat)
  }
  
  dimnames(Sigma_eps) <- dimnames(Sigma_Z)
  Sigma_eps
}
