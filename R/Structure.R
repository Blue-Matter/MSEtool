
StructurePars <- function(Pars, nsim=NULL, nTS=NULL) {
  atts <- attributes(Pars)
  Pars <- lapply(Pars, StructurePars_, nsim=nsim)
  Pars <- ApplyRandomWalk(Pars, nsim, nTS)
  attributes(Pars)$timesteps <- atts$timesteps
  Pars
}


StructurePars_ <- function(Pars, nsim=NULL) {
  # returns an array - nsim by nTS

  # Par already an array
  if (length(dim(Pars))>=2)
    return(Pars)

  # length 1 - return
  if (length(Pars)==1)
    return(array(Pars, dim=c(1,1)))

  # length 2 = sample from uniform distribution
  if (length(Pars)==2) {
    Pars <- sort(Pars)
    if (is.null(nsim))
      cli::cli_abort(c('`nsim` required to generate stochastic values',
                       'i'='Provide number of simulations to `nsim` argument')
      )
    if (nsim==1)
      return(array(mean(c(Pars[1], Pars[2])),
             dim=c(nsim, 1)))

    return(array(stats::runif(nsim, Pars[1], Pars[2]), dim=c(nsim, 1)))
  }

  # Pars are `nsim` long
  array(Pars, dim=c(1, length(Pars)))
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

RandomWalk <- function(targ, targsd, nTS, nsim) {
  targ <- matrix(targ, nsim, nTS)
  mutemp <- -0.5 * targsd^2
  temp <- array(exp(rnorm(nsim*nTS, mutemp, targsd)),dim = c(nsim, nTS))
  if (nsim >1) {
    return(targ * temp/apply(temp, 1, mean))
  } else {
    return(targ * temp/mean(temp))
  }
}

ApplyRandomWalk <- function(Pars, nsim, nTS) {
  detect_sd <- which(tolower(names(Pars)) |> substrRight(2) == 'sd') 
  if (length(detect_sd)==0)
    return(Pars)
  for (i in detect_sd) {
    nm_sd <- names(Pars)[i]
    nm_par <- strsplit(nm_sd, split="(?<=.)(?=.{2}$)", perl=T)[[1]][1]
    par_ind <- match(nm_par, names(Pars))
    if (is.null(nTS))
      cli::cli_abort(c('`nTS` required to generate stochastic time-varying values',
                       'i'='Add time steps to the `TimeSteps` argument')
      )

    Pars[[par_ind]] <- RandomWalk(targ=Pars[[par_ind]],
                                  targsd=Pars[[i]],
                                  nTS=nTS,
                                  nsim=nsim)
    Pars[[i]] <- NA
  }
  tt <- lapply(lapply(Pars, is.na), prod)
  ind <- which(tt==1)
  Pars[ind] <- NULL
  Pars
}



Structure <- function(value, out=c('nsim', 'nage', 'nTS'), req='nage') {

  if (is.null(value))
    return(NULL)

  array_str <- data.frame(name=out, size=1)

  if (!is.array(value)) {
    array_str$size[match(req, array_str$name)] <- length(value)
    return(array(value, dim=array_str$size))
  }

  dim_value <- dim(value)

  if (length(dim_value)==length(out))
    return(value)

  if (length(dim_value)>length(out))
    cli::cli_abort('`length(dim(value))>length(out)`')

  i <- seq_along(dim_value)
  array_str$size[i] <- dim_value[i]
  array(value, dim=array_str$size)
}







