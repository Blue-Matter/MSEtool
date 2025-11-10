
StructurePars <- function(Pars, nsim=NULL, Years=NULL) {
  OutPars <- lapply(Pars, StructurePars_, nsim=nsim, Years)
  OutPars <- ApplyRandomWalk(OutPars, nsim, Years)
  OutPars
}


StructurePars_ <- function(Par, nsim=NULL, Years=NULL) {
  # returns an array - nsim by nTS

  # Par already an array
  if (length(dim(Par))>=2) {
    if (is.null(dimnames(Par)))
      cli::cli_abort('array must have dimnames', .internal=TRUE)
    return(Par)
  }
    

  # length 1 - return
  if (length(Par)==1) {
    out <- array(Par, dim=c(1,1))
    dimnames(out) <- list(Sim=1,
                          Year=Years[1])
    return(out)
  }
   
  # length 2 = sample from uniform distribution
  if (length(Par)==2 && nsim!=2) {
    Par <- sort(Par)
    if (is.null(nsim))
      cli::cli_abort(c('`nsim` required to generate stochastic values',
                       'i'='Provide number of simulations to `nsim` argument')
      )
    if (nsim==1) {
      out <- array(mean(c(Par[1], Par[2])), dim=c(1, 1))
      dimnames(out) <- list(Sim=1,
                            Year=Years[1])
      return(out)
    }
      
    out <- array(stats::runif(nsim, Par[1], Par[2]), dim=c(nsim, 1))
    dimnames(out) <- list(Sim=1:nsim,
                          Year=Years[1])
    return(out)
  }

  if (length(Par) > nsim) {
    Par <- Par[1:nsim]
  }
  
  
  # Par are `nsim` long
  out <- array(Par, dim=c(length(Par), 1))
  dimnames(out) <- list(Sim=1:nsim,
                        Year=Years[1])
  out
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

RandomWalk <- function(targ, targsd, nsim, Years) {
  nTS <- length(Years)
  targ <- matrix(targ, nsim, nTS)
  mutemp <- -0.5 * targsd^2
  temp <- array(exp(rnorm(nsim*nTS, mutemp, targsd)),dim = c(nsim, nTS))
  dimnames(temp) <- list(Sim=1:nsim,
                        Year=Years)
  if (nsim >1) {
    return(targ * temp/apply(temp, 1, mean))
  } else {
    return(targ * temp/mean(temp))
  }
}

ApplyRandomWalk <- function(Pars, nsim, Years) {
  detect_sd <- which(tolower(names(Pars)) |> substrRight(2) == 'sd') 
  if (length(detect_sd)==0)
    return(Pars)
  for (i in detect_sd) {
    nm_sd <- names(Pars)[i]
    nm_par <- strsplit(nm_sd, split="(?<=.)(?=.{2}$)", perl=T)[[1]][1]
    par_ind <- match(nm_par, names(Pars))
    if (is.null(Years))
      cli::cli_abort(c('`Years` required to generate stochastic time-varying values',
                       'i'='Add time steps to the `Years` argument')
      )

    Pars[[par_ind]] <- RandomWalk(targ=Pars[[par_ind]],
                                  targsd=Pars[[i]],
                                  nsim=nsim,
                                  Years=Years
                                  )
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







