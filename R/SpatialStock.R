






#' @rdname CalcMovement
#' @export
FitMovement <- function(LogitProbs,
                        UnfishedDist,
                        ProbStaying,
                        FracOther=NULL,
                        CVDist=0.1,
                        CVStay=1,
                        nits = 100) {

  nareas <- max(2, length(UnfishedDist))

  if (nareas==2) {
    Movement <- matrix(0, 2,2)
    diag(Movement) <- ilogit(LogitProbs)
    Movement[1,2] <- 1- Movement[1,1]
    Movement[2,1] <- 1- Movement[2,2]

    if (UnfishedDist==0)
      UnfishedDist <- 1e-3
    ExpDistribution <- c(UnfishedDist, 1-UnfishedDist)
    Distribution <- CalcAsymptoticDist(Movement, ExpDistribution, nits=nits)
    NLL <- (log(Movement[1,1]) - log(ProbStaying))^2 +
      (log(UnfishedDist) - log(Distribution[1]))^2

    return(NLL)
  }

  Movement <- MarkovFrac(LogitProbs, FracOther)

  outdist <- CalcAsymptoticDist(Movement, UnfishedDist, nits=nits)
  nll_dist <- dnorm(log(outdist), log(UnfishedDist), CVDist, TRUE)
  nll_stay <- dnorm(LogitProbs, logit(ProbStaying), CVStay, TRUE)
  -sum(c(nll_dist, nll_stay))
}





#' @rdname CalcMovement
#' @export
MarkovFrac <- function(LogitProbs, FracOther=NULL){
  probs <- ilogit(LogitProbs)
  left <- 1-probs
  
  if (!is.null(FracOther)) {
    diag(FracOther) <- NA
    mov <- FracOther/apply(FracOther,1,sum,na.rm=T)*left
  } else {
    mov <- matrix(left, 2,2)
  }
  diag(mov) <- probs
  if (!all(rowSums(mov)==1))
    cli::cli_abort('Movement matrix does not sum to 1 across areas')
  
  mov
}




