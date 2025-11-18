
ApplyCustomAtAgeModel <- function(Model, Pars, Ages) {
  nSimnTS <- cbind(unlist(lapply(Pars, nSim)),    
                   unlist(lapply(Pars, nTS)))
  
  nsim <- max(nSimnTS[,1])
  nTS <- max(nSimnTS[,2])
  
  tsind <- which.max(nSimnTS[,2])
  TSnames <- dimnames(Pars[[tsind]])
  
  out <- array(0, dim=c(nsim, length(Ages), nTS))
  l <- Pars
  l$Ages <- Ages
  
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      for (arg in 1:nrow(nSimnTS)) {
        l[[arg]] <- Pars[[arg]][GetIndex(s, nSimnTS[arg,1]), GetIndex(ts, nSimnTS[arg,2])]
      }
      out[s,,ts] <- do.call(Model, l)
    }
  }
  dimnames(out) <- list(Sim=1:nsim,
                        Age=Ages,
                        Year=TSnames$Year)
  out
}

ApplyCustomAtLengthModel <- function(Model, Pars, Length) {
  nSimnTS <- cbind(unlist(lapply(Pars, nSim)),    
                   unlist(lapply(Pars, nTS)))
  
  nsim <- max(nSimnTS[,1])
  nTS <- max(nSimnTS[,2])
  
  tsind <- which.max(nSimnTS[,2])
  TSnames <- dimnames(Pars[[tsind]])
  
  out <- array(0, dim=c(nsim, length(Length), nTS))
  l <- Pars
  l$Length <- Length
  
  for (s in 1:nsim) {
    for (ts in 1:nTS) {
      for (arg in 1:nrow(nSimnTS)) {
        l[[arg]] <- Pars[[arg]][GetIndex(s, nSimnTS[arg,1]), GetIndex(ts, nSimnTS[arg,2])]
      }
      out[s,,ts] <- do.call(Model, l)
    }
  }
  dimnames(out) <- list(Sim=1:nsim,
                        Class=Length,
                        Year=TSnames$Year)
  out
}


GenerateMeanAtAge <- function(Model, Pars, Ages) {

  if (inherits(Model, 'function')) {
    return(ApplyCustomAtAgeModel(Model, Pars, Ages))
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
    return(ApplyCustomAtLengthModel(Model, Pars, Length))
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
    l[[fun_args[[arg_ind[i]]]]] <- Pars[[i]]
  }
  do.call(fun, l)
}

GenerateMeanatWeight <- function(Model, Pars, Weight) {
  
  if (inherits(Model, 'function')) {
    stop("R functions not currently supported for MeanAtWeight")
    # return(ApplyCustomAtLengthModel(Model, Pars, Length))
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
    l[[fun_args[[arg_ind[i]]]]] <- Pars[[i]]
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


GenerateStochasticnVessels <- function(nVessels, nsim, Years) {
  nms <- names(nVessels)
  if (!all(c('EffLower', 'EffUpper', 'EffYears') %in% nms)) {
    cli::cli_abort(paste('If `nVessels` is a dataframe, it must have names:', paste(c('EffLower', 'EffUpper', 'EffYears'), collapse=', ')))
  }
  ind <- which(Years@Period=='Historical')
  HistYears <- Years@Year[ind]
  refYears <- seq_along(HistYears)
  nts <- length(refYears)

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


GenerateRecruitmentDeviations <- function(SD=0.2, 
                                          AC=0,
                                          TruncSD=2,
                                          Ages=NULL,
                                          HistTS=NULL, 
                                          ProjTS=NULL,
                                          nsim=48,
                                          RecDevInit=NULL,
                                          RecDevHist=NULL,
                                          RecDevProj=NULL) {
  
  if (is.null(HistTS))
    cli::cli_abort('`nHistTS` cannot be NULL')
  
  if (is.null(ProjTS))
    cli::cli_abort('`nProjTS` cannot be NULL')
  
  nInitRecDev <- length(Ages@Classes)-1
  nHistTS <- length(HistTS)
  nProjTS <- length(ProjTS)
  
  if (!is.null(nsim) && nsim==1) {
    cli::cli_alert_info('`nsim=1`. Assuming no process error ')
    return(
      list(RecDevInit=array(1, dim=c(1, nInitRecDev)),
           RecDevHist=array(1, dim=c(1,nHistTS)),
           RecDevProj=array(1, dim=c(1,nProjTS))
      )
    )
  }

  genInit <- TRUE
  genHist <- TRUE
  genProj <- TRUE

  if (!is.null(RecDevInit) & all(!is.na(RecDevInit)) & is.array(RecDevInit)) {
    RecDevInit <- array(RecDevInit, dim=c(1, nInitRecDev))
    logRecDevInit <- log(RecDevInit)
    genInit <- FALSE
  }

  if (!is.null(RecDevHist) & all(!is.na(RecDevHist)) & is.array(RecDevHist)) {
      RecDevHist <- array(RecDevHist, dim=c(1, nHistTS))
      logRecDevHist <- log(RecDevHist)
      genHist <- FALSE
  }
  if (!is.null(RecDevProj) & all(!is.na(RecDevProj)) & is.array(RecDevProj)) {
      RecDevProj <- array(RecDevProj, dim=c(1,nProjTS))
      logRecDevProj <- log(RecDevProj)
      genProj <- FALSE
  }

  if (!genInit & !genHist & !genProj) {
    
    dd <- dim(RecDevInit)
    dimnames(RecDevInit) <- list(
      Sim=1:dd[1],
      Age=Ages@Classes[-1]
    )
    
    dd <- dim(RecDevHist)
    dimnames(RecDevHist) <- list(
      Sim=1:dd[1],
      Year=HistTS
    )
    
    dd <- dim(RecDevProj)
    dimnames(RecDevProj) <- list(
      Sim=1:dd[1],
      Year=ProjTS
    )
    
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
    logRecDevInit <- array(rtnorm(nsim*nInitRecDev, mu, SD, lower, upper), dim=c(nsim, nInitRecDev))

  if (genHist)
    logRecDevHist <- array(rtnorm(nsim*nHistTS, mu, SD, lower, upper), dim=c(nsim, nHistTS))

  if (genProj)
    logRecDevProj <- array(rtnorm(nsim*nProjTS, mu, SD, lower, upper), dim=c(nsim, nProjTS))

  # Apply auto-correlation
  Years <- 1:(nInitRecDev+nHistTS+nProjTS)
  period <- c(rep('Init', nInitRecDev), rep('Hist',nHistTS), rep('Proj', nProjTS))
  required <- c(rep(genInit, nInitRecDev), rep(genHist,nHistTS), rep(genProj, nProjTS))

  Years <- Years[required]

  for (i in 1:nsim) {
    logRecDeviations <- c(logRecDevInit[GetIndex(i, nrow(logRecDevInit)),],
                          logRecDevHist[GetIndex(i, nrow(logRecDevHist)),],
                          logRecDevProj[GetIndex(i, nrow(logRecDevProj)),]
                          )

    for (ts in seq_along(Years)[-1]) {
      logRecDeviations[Years[ts]] <- AC[i] * logRecDeviations[Years[ts]-1] +
        logRecDeviations[Years[ts]] * (1 - AC[i] * AC[i])^0.5
    }

    if (genInit)
      logRecDevInit[GetIndex(i, nrow(logRecDevInit)), ] <- logRecDeviations[period =='Init']

    if (genHist)
      logRecDevHist[GetIndex(i, nrow(logRecDevHist)), ] <- logRecDeviations[period =='Hist']

    if (genProj)
      logRecDevProj[GetIndex(i, nrow(logRecDevProj)), ] <- logRecDeviations[period =='Proj']

  }
  
  dd <- dim(RecDevInit)
  dimnames(RecDevInit) <- list(
    Sim=1:dd[1],
    Age=Ages@Classes[-1]
  )
  
  dd <- dim(RecDevHist)
  dimnames(RecDevHist) <- list(
    Sim=1:dd[1],
    Year=HistTS
  )
  
  dd <- dim(RecDevProj)
  dimnames(RecDevProj) <- list(
    Sim=1:dd[1],
    Year=ProjTS
  )

  list(RecDevInit=exp(logRecDevInit),
       RecDevHist=exp(logRecDevHist),
       RecDevProj=exp(logRecDevProj)
  )

}


