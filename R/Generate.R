
GenerateMeanAtAge <- function(Model, Pars, Ages) {

  if (inherits(Model, 'function')) {
    return(Model(Ages, Pars))
  }

  fun_args <- names(formals(Model))
  fun <- get(Model)
  arg_ind <- match(names(Pars), fun_args)
  val_ind <- 1:max(min(arg_ind-1), 1)

  l <- list()
  for (i in seq_along(val_ind)) {
    l[[fun_args[[val_ind[i]]]]] <- get(fun_args[[i]])
  }

  for (i in seq_along(arg_ind)) {
    l[[fun_args[[arg_ind[i]]]]] <-  Pars[[i]]
  }
  do.call(fun, l)
}

GenerateMeanatLength <- function(Model, Pars, Length) {

  if (inherits(Model, 'function')) {
    return(Model(Length, Pars))
  }

  fun_args <- names(formals(Model))
  fun <- get(Model)
  arg_ind <- match(names(Pars), fun_args)
  val_ind <- 1:max(min(arg_ind-1), 1)

  l <- list()
  for (i in seq_along(val_ind)) {
    l[[fun_args[[val_ind[i]]]]] <- get(fun_args[[i]])
  }

  for (i in seq_along(arg_ind)) {
    l[[fun_args[[arg_ind[i]]]]] <-  Pars[[i]]
  }
  do.call(fun, l)
}

GenerateSRR <- function(Model, Pars, S=NULL, S0=NULL) {

  fun_args <- names(formals(Model))
  fun <- get(Model)
  arg_ind <- match(names(Pars), fun_args)
  val_ind <- 1:max(min(arg_ind-1), 1)

  l <- list()
  for (i in seq_along(val_ind)) {
    l[[fun_args[[val_ind[i]]]]] <- get(fun_args[[i]])
  }

  for (i in seq_along(arg_ind)) {
    l[[fun_args[[arg_ind[i]]]]] <-  Pars[[i]]
  }
  do.call(fun, l)
}


GenerateStochasticnVessels <- function(nVessels, nsim, Timesteps) {
  nms <- names(nVessels)
  if (!all(c('EffLower', 'EffUpper', 'EffYears') %in% nms)) {
    cli::cli_abort(paste('If `nVessels` is a dataframe, it must have names:', paste(c('EffLower', 'EffUpper', 'EffYears'), collapse=', ')))
  }
  ind <- which(Timesteps@Period=='Historical')
  HistTimesteps <- Timesteps@Timestep[ind]
  refTimeSteps <- seq_along(HistTimesteps)
  nts <- length(refTimeSteps)

  EffLower <- nVessels$EffLower
  EffUpper <- nVessels$EffUpper
  EffYears <- range01(nVessels$EffYears)


  if (any(EffLower > EffUpper)) {
    ind <- which(EffLower > EffUpper)
    tt <- cbind(EffLower, EffUpper)
    EffLower <- apply(tt, 1, min)
    EffUpper <- apply(tt, 1, max)
  }

  nVesselsRange <- mapply(stats::runif, n = nsim, min = EffLower, max = EffUpper)

  if (nsim > 1) {
    if (ncol(nVesselsRange) == 1) {
      nVesselsStochastic <- matrix(nVesselsRange, nrow=nsim, ncol=nts)
    } else {
      nVesselsStochastic <- t(sapply(1:nsim, function(x)
        stats::approx(x = EffYears,
               y = nVesselsRange[x, ],
               method = "linear", n = nts)$y))
    }
  }
  if (nsim == 1) {
    if (length(nVesselsRange) == 1) {
      nVesselsStochastic <- matrix(nVesselsRange, nrow=nsim, ncol=nts)
    } else {
      nVesselsStochastic <- matrix(stats::approx(x = EffYears,
                                          y = nVesselsRange,
                                          method = "linear",
                                          n = nts)$y, nrow = 1)
    }
  }

  if (!all(nVesselsStochastic == mean(nVesselsStochastic)))
    nVesselsStochastic <- range01(nVesselsStochastic)

  nVesselsStochastic[nVesselsStochastic == 0] <- 1E-15
  nVesselsStochastic
}


GenerateRecruitmentDeviations <- function(SD=0.2, AC=0, TruncSD=2,
                                          MaxAge=5, nHistTS=15, nProjTS=15,
                                          nsim=48,
                                          RecDevInit=NULL,
                                          RecDevHist=NULL,
                                          RecDevProj=NULL) {

  if (is.null(MaxAge))
    cli::cli_abort('`MaxAge` cannot be NULL')

  if (is.null(nHistTS))
    cli::cli_abort('`nHistTS` cannot be NULL')

  if (is.null(nProjTS))
    cli::cli_abort('`nProjTS` cannot be NULL')

  if (is.null(nsim) | nsim==1) {
    cli::cli_alert_info('`nsim` not specified. Assuming `nsim=1` and no process error ')
    return(
      list(RecDevInit=rep(1, MaxAge),
           RecDevHist=rep(1, nHistTS),
           RecDevProj=rep(1, nProjTS)
      )
    )
  }

  genInit <- TRUE
  genHist <- TRUE
  genProj <- TRUE

  if (!is.null(RecDevInit) & all(!is.na(RecDevInit)) & (!is.array(RecDevInit))) {
    RecDevInit <- array(RecDevInit, dim=c(1, MaxAge))
    logRecDevInit <- log(RecDevInit)
    genInit <- FALSE
  }

  if (!is.null(RecDevHist) & all(!is.na(RecDevHist)) & !is.array(RecDevHist)) {
      RecDevHist <- array(RecDevHist, dim=c(1, nHistTS))
      logRecDevHist <- log(RecDevHist)
      genHist <- FALSE
  }
  if (!is.null(RecDevProj) & all(!is.na(RecDevProj)) & !is.array(RecDevProj)) {
      RecDevProj <- array(RecDevProj, dim=c(1,nProjTS))
      logRecDevProj <- log(RecDevProj)
      genProj <- FALSE
  }

  if (!genInit & !genHist & !genProj) {
    return(
      list(RecDevInit=RecDevInit,
           RecDevHist=RecDevHist,
           RecDevProj=RecDevProj
      )
    )
  }

  nsimSD <- length(SD)
  nsimAC <- length(AC)

  if (nsimSD!=nsim & nsimSD!=1) {
    cli::cli_alert_warning('`SRR@SD` is not length `nsim` or length `1`. Recycling')
  }
  SD <- rep(SD, nsim)[1:nsim]

  if (nsimAC!=nsim & nsimAC!=1) {
    cli::cli_alert_warning('`SRR@AC` is not length `nsim` or length `1`. Recycling')
  }

  AC <- rep(AC, nsim)[1:nsim]
  AC[!is.finite(AC)] <- 0

  mu <- -0.5 * SD^2  * (1 - AC)/sqrt(1 - AC^2)
  lower <- mu-TruncSD*SD
  upper <- mu+TruncSD*SD

  if (genInit)
    logRecDevInit <- array(rtnorm(nsim*MaxAge, mu, SD, lower, upper), dim=c(nsim, MaxAge))

  if (genHist)
    logRecDevHist <- array(rtnorm(nsim*nHistTS, mu, SD, lower, upper), dim=c(nsim, nHistTS))

  if (genProj)
    logRecDevProj <- array(rtnorm(nsim*nProjTS, mu, SD, lower, upper), dim=c(nsim, nProjTS))

  # Apply auto-correlation
  timesteps <- 1:(MaxAge+nHistTS+nProjTS)
  period <- c(rep('Init', MaxAge), rep('Hist',nHistTS), rep('Proj', nProjTS))
  required <- c(rep(genInit, MaxAge), rep(genHist,nHistTS), rep(genProj, nProjTS))

  timesteps <- timesteps[required]

  for (i in 1:nsim) {
    logRecDeviations <- c(logRecDevInit[GetIndex(i, nrow(logRecDevInit)),],
                          logRecDevHist[GetIndex(i, nrow(logRecDevHist)),],
                          logRecDevProj[GetIndex(i, nrow(logRecDevProj)),]
                          )

    for (ts in seq_along(timesteps)[-1]) {
      logRecDeviations[timesteps[ts]] <- AC[i] * logRecDeviations[timesteps[ts]-1] +
        logRecDeviations[timesteps[ts]] * (1 - AC[i] * AC[i])^0.5
    }

    # rescale to SD
    logRecDeviations[timesteps] <- (scale(logRecDeviations[timesteps]) + mean(logRecDeviations[timesteps])) * SD[i]

    if (genInit)
      logRecDevInit[GetIndex(i, nrow(logRecDevInit)), ] <- logRecDeviations[period =='Init']

    if (genHist)
      logRecDevHist[GetIndex(i, nrow(logRecDevHist)), ] <- logRecDeviations[period =='Hist']

    if (genProj)
      logRecDevProj[GetIndex(i, nrow(logRecDevProj)), ] <- logRecDeviations[period =='Proj']

  }

  list(RecDevInit=exp(logRecDevInit),
       RecDevHist=exp(logRecDevHist),
       RecDevProj=exp(logRecDevProj)
  )

}


