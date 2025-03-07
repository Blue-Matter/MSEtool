
#' Calculate the Movement Matrix
#'
#' @export
CalcMovement <- function(Spatial,
                         TimeSteps=NULL,
                         nsim=NULL,
                         seed=NULL,
                         nits=100,
                         plot=FALSE,
                         silent=FALSE,
                         control = list(iter.max = 5e3, eval.max = 1e4)) {

  argList <- list(nsim, seed, nits, control)
  if (CheckDigest(argList, Spatial))
    return(Spatial)

  SetSeed(Spatial, seed)

  # generate stochastic values if needed
  Spatial@UnfishedDist <- GenerateStochasticValues(Spatial@UnfishedDist, nsim)

  # add age and time-step dimensions
  Spatial@UnfishedDist  <- Spatial@UnfishedDist |>
    AddAgeTimeStepDimensions() |>
    AddDimNames(c('Sim', 'Area', 'Age', 'Time Step'), TimeSteps=TimeSteps)

  nareas <- max(2,dim(Spatial@UnfishedDist)[2])

  if (nareas>2) {
    dd <- dim(Spatial@FracOther)
    if (length(dd)==0)
      cli::cli_abort(c('`FracOther` required for more than 2 areas.',
                     'i'='Must be an array with dimensions: `c(nSim, nArea, nArea)`')
      )
    if (length(dd)==2)
      cli::cli_abort('`FracOther` must be an array with dimensions: `c(nSim, nArea, nArea)`')

    if (any(dd[2:3] != nareas))
      cli::cli_abort('Second and third dimensions of `FracOther` must length `nArea`')

    Spatial@FracOther  <- Spatial@FracOther |>
      AddAgeTimeStepDimensions(outdim=5) |>
      AddDimNames(c('Sim', 'Area', 'Area', 'Age', 'Time Step'), TimeSteps=TimeSteps)
  }

  # generate stochastic values if needed
  Spatial@ProbStaying <- GenerateStochasticValues(Spatial@ProbStaying, nsim)

  if (is.null(Spatial@ProbStaying))
    cli::cli_abort('`ProbStaying` is not populated.')

  if (is.null(dim(Spatial@ProbStaying))) # when ProbStaying is a single value
    Spatial@ProbStaying <- array(Spatial@ProbStaying, dim=c(1, nareas))

  # add age and time-step dimensions
  Spatial@ProbStaying  <- Spatial@ProbStaying |>
    AddAgeTimeStepDimensions() |>
    AddDimNames(c('Sim', 'Area', 'Age', 'Time Step'), TimeSteps=TimeSteps)

  dims <- lapply(list(Spatial@UnfishedDist, Spatial@ProbStaying), dim)
  if (!is.null(Spatial@FracOther)) {
    dims <- c(dims, list(dim(Spatial@FracOther)[c(1,2,4,5)]))
    names(dims) <- c('UnfishedDist', 'ProbStaying', 'FracOther')
    outdims <- do.call('rbind',dims) |> apply(2, max)
  } else {
    names(dims) <- c('UnfishedDist', 'ProbStaying')
    outdims <- do.call('rbind',dims) |> apply(2, max)
  }

  Spatial@Movement <- AddDimNames(array(NA, dim=c(outdims[1],
                                                  nareas,
                                                  nareas,
                                                  outdims[3],
                                                  outdims[4])),
                                  c('Sim', 'FromArea', 'ToArea', 'Age', 'Time Step'),
                                  TimeSteps=TimeSteps)


  # nasty loop for now

  if (!silent)
    sb <- cli::cli_progress_bar('Calculating Movement Matrix',
                          total=outdims[1]*outdims[3]*outdims[4])

  UnfishedDist <- AddDimNames(array(NA, dim=c(outdims[1],
                                              nareas,
                                              outdims[3],
                                              outdims[4])),
                              c('Sim', 'Area', 'Age', 'Time Step'),
                              TimeSteps=TimeSteps)



  for (s in 1:outdims[1]) {

    FracArea_s <- FracOther_s <- ProbStaying_s <- s
    if (dims$ProbStaying[1] < s) ProbStaying_s <- 1
    if (dims$UnfishedDist[1] < s) FracArea_s <- 1
    if (nareas>2)
      if (dims$FracOther[1] < s) FracArea_s <- 1

    for (ts in 1:outdims[4]) {
      FracArea_ts <- ProbStaying_ts <- FracOther_ts <- ts
      if (dims$ProbStaying[4] < ts) ProbStaying_ts <- 1
      if (dims$UnfishedDist[4] < ts) FracArea_ts <- 1
      if (nareas>2)
        if (dims$FracOther[4] < ts) FracOther_ts <- 1

      for (age in 1:outdims[3]) {

        if (!silent)
          cli::cli_progress_update(id=sb)

        FracArea_age <- ProbStaying_age <-FracOther_age <-  age
        if (dims$ProbStaying[3] < age) ProbStaying_age <- 1
        if (dims$UnfishedDist[3] < age) FracArea_age <- 1
        if (nareas>2)
          if (dims$FracOther[3] < age) FracOther_age <- 1

        if (nareas==2) {
          optMovement <- stats::optim(logit(rep(Spatial@ProbStaying[ProbStaying_s,1,ProbStaying_age,ProbStaying_ts],2)),
                                      FitMovement,
                                      UnfishedDist = Spatial@UnfishedDist[FracArea_s,1,FracArea_age,FracArea_ts],
                                      ProbStaying= Spatial@ProbStaying[ProbStaying_s,1,ProbStaying_age,ProbStaying_ts],
                                      method = "L-BFGS-B")#,
                                      #lower = rep(-8, 2), upper = rep(8, 2))

          Movement <- MarkovFrac(LogitProbs=optMovement$par)
          Spatial@Movement[s,,,age,ts] <- Movement

          ExpectedDist <- c(Spatial@UnfishedDist[FracArea_s,1,FracArea_age,FracArea_ts],
                            1-Spatial@UnfishedDist[FracArea_s,1,FracArea_age,FracArea_ts])

          UnfishedDist[FracArea_s,,FracArea_age,FracArea_ts] <- CalcAsymptoticDist(Movement, ExpectedDist, plot=plot, nits=nits)

          
        } else {
          opt <- stats::nlminb(rep(0,nareas),
                               FitMovement,
                               UnfishedDist = Spatial@UnfishedDist[FracArea_s, , FracArea_age,FracArea_ts],
                               ProbStaying = Spatial@ProbStaying[ProbStaying_s,,ProbStaying_age,ProbStaying_ts],
                               FracOther = Spatial@FracOther[FracArea_s,,,FracOther_age,FracOther_ts],
                               CVDist = Spatial@CVDist,
                               CVStay=Spatial@CVStay,
                               nits=nits,
                               control = list(iter.max = 5e3, eval.max = 1e4))

          Spatial@Movement[s,,,age,ts] <- MarkovFrac(LogitProbs=opt$par,
                                                     FracOther = Spatial@FracOther[FracArea_s,,,FracOther_age,FracOther_ts])

          UnfishedDist[FracArea_s,,FracArea_age,FracArea_ts] <- CalcAsymptoticDist(Movement=Spatial@Movement[s,,,age,ts],
                                                                                           ExpectedDist=Spatial@UnfishedDist[FracArea_s,,FracArea_age,FracArea_ts],
                                                                                           plot=plot, nits=nits)
        }
      }
    }
  }
  
  # if (nareas==2) {
  #   ProbStaying <- abind::abind(Spatial@ProbStaying, 
  #                                1-Spatial@ProbStaying, 
  #                                along=2, 
  #                                use.dnns = TRUE)
  #   dimnames(ProbStaying)[['Area']] <- 1:2
  #   Spatial@ProbStaying <- ProbStaying
  # }

  if (!silent)
    cli::cli_progress_done()

  Spatial@UnfishedDist <- UnfishedDist

  SetDigest(argList, Spatial)
}



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



#' Calculate Asymptotic Spatial Distribution
#'
#' @param Movement A movement matrix with dimensions `c(narea, narea)`  where the
#' diagonals are the probability of staying in an area, and off-diagonals are the
#' probability of moving from area in row and column `i` to area in row `i` and column `j`
#' @param ExpectedDist Optional. A numeric vector of length `narea` describing the expected
#' unfished distribution.
#' @param nits Number of iterations
#' @param plot Logical. Produce the plot?
#'
#' @return A numeric vector of length `nareas` with the asymptotic distribution
#' @rdname CalcMovement
#' @export
CalcAsymptoticDist <- function(Movement, ExpectedDist=NULL,nits=100,plot=F){
  if(!is.null(ExpectedDist))
    if(!all(length(ExpectedDist)==dim(Movement)))
      cli::cli_abort("Error in CalcAsymptoticDist(): the length of the distribution vector `ExpectedDist` is not the same as the dimensions of the square movement matrix `Movement`")

  if (is.null(ExpectedDist))
    ExpectedDist <- rep(1/dim(Movement)[1],dim(Movement)[1])

  AsymptoticDist <- ExpectedDist
  tol <- rep(NA,nits)
  for(i in 1:nits){
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



#' @rdname CalcMovement
#' @export
MarkovFrac <- function(LogitProbs, FracOther=NULL){
  probs <- ilogit(LogitProbs)
  left <- 1-probs
  if (!is.null(FracOther)) {
    mov <- FracOther/apply(FracOther,1,sum,na.rm=T)*left
  } else {
    mov <- matrix(left, 2,2)
  }
  diag(mov) <- probs
  mov
}




