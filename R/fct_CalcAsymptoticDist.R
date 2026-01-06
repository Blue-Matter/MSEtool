#' Calculate Asymptotic Spatial Distribution
#'
#' @param Movement A movement matrix with dimensions `c(nArea, nArea)`  where the
#' diagonals are the probability of staying in an area, and off-diagonals are the
#' probability of moving from area in row and column `i` to area in row `i` and column `j`
#' @param ExpectedDist Optional. A numeric vector of length `nArea` describing the expected
#' unfished distribution.
#' @param nits Number of iterations
#' @param plot Logical. Produce the plot?
#'
#' @return A numeric vector of length `nArea` with the asymptotic distribution
#' @rdname CalcMovement
#' @export
CalcAsymptoticDist <- function(Movement, ExpectedDist=NULL, nits=100, plot=FALSE) {
  if (!is.null(ExpectedDist) && !all(length(ExpectedDist)==dim(Movement))) {
    cli::cli_abort("Length of the distribution vector `ExpectedDist` is not the same as the dimensions of the square movement matrix `Movement`")
  }
  
  # attempt to calculate eigen value
  
  
  
      
  if (is.null(ExpectedDist))
    ExpectedDist <- rep(1/dim(Movement)[1],dim(Movement)[1])
  
  eig <- eigen(t(Movement))
  
  # Find eigenvalue closest to 1
  idx <- which.min(abs(eig$values - 1))
  
  # Corresponding eigenvector
  v <- Re(eig$vectors[, idx])
  
  # Normalize to sum to 1
  v <- v / sum(v)
  
  
  AsymptoticDist <- ExpectedDist
  tol <- rep(NA,nits)
  for(i in 1:nits) {
    temp <- AsymptoticDist%*%Movement
    tol[i] <- mean(abs(temp-AsymptoticDist))
    AsymptoticDist <- temp
  }
  
  if(plot){
    graphics::par(mfrow=c(1,2),mai=c(0.3,0.3,0.01,0.01),omi=c(0.05,0.05,0.3,0.05))
    plot(tol,pch=19,col='blue')
    graphics::lines(tol,col ="blue")
    graphics::grid()
    plot(1:length(AsymptoticDist),AsymptoticDist,col='#0000ff90',pch=1,lwd=2,cex=1.3,ylim=c(0,max(AsymptoticDist,ExpectedDist)))
    graphics::grid()
    graphics::points(1:length(AsymptoticDist),ExpectedDist,col="#ff000090",lwd=2,pch=3,cex=1.3)
    graphics::legend('topright',legend=c("Specified","Achieved"),text.col=c("red","blue"),bty='n')
  }
  AsymptoticDist
}
