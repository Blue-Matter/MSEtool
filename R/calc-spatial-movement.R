FitMovement <-  function(Spatial, sim=1, age=1, year=1) {
  nArea <- dim(Spatial@Movement)[2]
  if (nArea==2) {
    movement <- FitMovement_2_Area(Spatial,
                                   sim,
                                   age,
                                   year)
  } else {
    movement <- FitMovement_Multi_Area(Spatial,
                                       sim,
                                       age,
                                       year)
  }
  movement
}

FitMovement_2_Area <- function(Spatial, sim=1, age=1, year=1) {
  
  PS_dim <- dim(Spatial@ProbStaying)
  PS_sim <- min(sim, PS_dim[1])
  PS_age <- min(age, PS_dim[3])
  PS_year <- min(year, PS_dim[4])
  
  UD_dim <- dim(Spatial@UnfishedDist)
  UD_sim <- min(sim, UD_dim[1])
  UD_age <- min(age, UD_dim[3])
  UD_year <- min(year, UD_dim[4])
  
  optMovement <- stats::optim(logit(rep(Spatial@ProbStaying[PS_sim, 1, PS_age, PS_year], 2)),
                              SolveMovement_2_Area,
                              UnfishedDist = Spatial@UnfishedDist[UD_sim,1,UD_age,UD_year],
                              ProbStaying= Spatial@ProbStaying[PS_sim, 1, PS_age, PS_year],
                              method = "L-BFGS-B")
  
  MarkovFrac(LogitProbs=optMovement$par)
}

SolveMovement_2_Area <- function(LogitProbs,
                               UnfishedDist,
                               ProbStaying) {
  Movement <- matrix(0, 2,2)
  diag(Movement) <- ilogit(LogitProbs)
  Movement[1,2] <- 1- Movement[1,1]
  Movement[2,1] <- 1- Movement[2,2]
  
  Distribution <- CalcAsymDist_2Area(Movement)
  
  NLL <- (log(Movement[1,1]) - log(ProbStaying))^2 +
    (log(UnfishedDist) - log(Distribution[1]))^2
  NLL
}

MarkovFrac <- function(LogitProbs, FracOther=NULL, tol = 1e-10){
  probs <- ilogit(LogitProbs)
  left <- 1-probs
  
  if (!is.null(FracOther)) {
    diag(FracOther) <- NA
    mov <- FracOther/apply(FracOther,1,sum,na.rm=T)*left
  } else {
    mov <- matrix(left, 2,2)
  }
  diag(mov) <- probs
  rs <- rowSums(mov, na.rm = FALSE)
  if (any(abs(rs - 1) > tol)) {
    cli::cli_abort('Movement matrix does not sum to 1 across areas')
  }
  mov
}


FitMovement_Multi_Area <- function(Spatial, sim=1, age=1, year=1) {
  
  PS_dim <- dim(Spatial@ProbStaying)
  PS_sim <- min(sim, PS_dim[1])
  PS_age <- min(age, PS_dim[3])
  PS_year <- min(year, PS_dim[4])
  
  UD_dim <- dim(Spatial@UnfishedDist)
  UD_sim <- min(sim, UD_dim[1])
  UD_age <- min(age, UD_dim[3])
  UD_year <- min(year, UD_dim[4])
  
  FO_dim <- dim(Spatial@FracOther)
  FO_sim <- min(sim, FO_dim[1])
  FO_age <- min(age, FO_dim[4])
  FO_year <- min(year, FO_dim[5])
  
  nArea <- UD_dim[2]
  
  optMovement <- stats::nlminb(rep(0, nArea),
                               SolveMovement_Multi_Area,
                               UnfishedDist = Spatial@UnfishedDist[UD_sim, , UD_age, UD_year],
                               ProbStaying = Spatial@ProbStaying[PS_sim,, PS_age, PS_year],
                               FracOther = Spatial@FracOther[FO_sim,,, FO_age, FO_year],
                               CVDist = Spatial@CVDist,
                               CVStay=Spatial@CVStay,
                               control = list(iter.max = 5e3, eval.max = 1e4))
  
  MarkovFrac(LogitProbs=optMovement$par,
             FracOther = Spatial@FracOther[FO_sim,,, FO_age, FO_year])
  
}

SolveMovement_Multi_Area <- function(LogitProbs, 
                                     UnfishedDist, 
                                     ProbStaying, 
                                     FracOther, 
                                     CVDist=0.1, 
                                     CVStay=1) {
  Movement <- MarkovFrac(LogitProbs, FracOther)
  Distribution <- CalcAsymDist(Movement)
  nll_dist <- dnorm(log(Distribution), log(UnfishedDist), CVDist, TRUE)
  nll_stay <- dnorm(LogitProbs, logit(ProbStaying), CVStay, TRUE)
  -sum(c(nll_dist, nll_stay))
}
