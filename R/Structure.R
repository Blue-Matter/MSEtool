
StructurePars <- function(Pars, nSim=NULL, Years=NULL, nArea=NULL) {
  Pars <- purrr::map(Pars, \(Par) 
                        StructurePars_(Par, nSim, Years, nArea)
  )

  if (is.null(nArea)) {
    Pars <- ApplyRandomWalk(Pars)  
  }

  ExtendPars(Pars)
}


unique_dimname_values <- function(ParsList) {
  nD <- length(dim(ParsList[[1]]))  # assume all arrays have same rank
  lapply(seq_len(nD), function(d) {
    unique(unlist(lapply(ParsList, function(x) {
      dn <- dimnames(x)
      if (is.null(dn)) character(0) else dn[[d]]
    }), use.names = FALSE))
  })
}

# make sure Par arrays are the same for all
ExtendPars <- function(Pars) {
  dnames <- unique_dimname_values(Pars)
  
  nSim <- as.numeric(dnames[[1]]) |> max()
  Years <- as.numeric(dnames[[2]])
  if (length(dnames)==3) {
    Areas <- as.numeric(dnames[[3]]) 
  } else {
    Areas <- NULL
  }

 purrr::map(Pars, \(par) {
    Extend(par, nSim, NULL, Years, Areas)
  })
  
}


NameParDimensions <- function(Par, nSim=NULL, Years=NULL, nArea=NULL) {
  if (!is.null(dimnames(Par)))
    return(Par)
  
  dd <- dim(Par)
  
  if (dd[2]>1) {
    cli::cli_abort('`Year` dimensions must be named if dimension length > 1' )
  }
  
  if (length(dd)<3) {
    dimnames(Par) <- list(Sim=(1:nSim)[1:dd[1]],
                          Year=Years[1])
  } else {
    dimnames(Par) <- list(Sim=(1:nSim)[1:dd[1]],
                          Year=Years[1],
                          Area=(1:nArea)[1:dd[3]])
  }
  Par
}

StructurePars_ <- function(Par, nSim=NULL, Years=NULL, nArea=NULL) {

  # Par already an array
  if (inherits(Par, 'array')) {
    return(NameParDimensions(Par, nSim, Years, nArea))
  }
  
  # length 2 = sample from uniform distribution
  if (length(Par)==2 && nSim!=2) {
    Par <- sort(Par)
    if (is.null(nSim))
      cli::cli_abort(c('`nSim` required to generate stochastic values',
                       'i'='Provide number of simulations to `nSim` argument')
      )
    if (nSim==1) {
      out <- array(mean(c(Par[1], Par[2])), dim=c(1, 1))
      return(NameParDimensions(Par, nSim, Years, nArea))
    }
      
    Par <- array(stats::runif(nSim, Par[1], Par[2]), dim=c(nSim, 1))
    return(NameParDimensions(Par, nSim, Years, nArea))
  }


  if (length(Par) > nSim) {
    Par <- Par[1:nSim]
  }
  
  # Par are `nSim` long
  if (is.null(nArea)) {
    Par <- array(Par, dim=c(length(Par), 1))  
  } else {
    Par <- array(Par, dim=c(length(Par), 1,1))
  }
  NameParDimensions(Par, nSim, Years, nArea)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

RandomWalk <- function(targ, targsd, nSim, Years) {
  nTS <- length(Years)

  targ <- matrix(targ, nSim, nTS)
  mutemp <- -0.5 * targsd^2
  temp <- array(exp(rnorm(nSim*nTS, mutemp, targsd)),dim = c(nSim, nTS))
  dimnames(temp) <- list(Sim=1:nSim,
                        Year=Years)
  if (nSim >1) {
    return(targ * temp/apply(temp, 1, mean))
  } else {
    return(targ * temp/mean(temp))
  }
}

ApplyRandomWalk <- function(Pars) {
  detect_sd <- which(tolower(names(Pars)) |> substrRight(2) == 'sd') 
  if (length(detect_sd)==0)
    return(Pars)
  
  for (i in detect_sd) {
    nm_sd <- names(Pars)[i]
    nm_par <- strsplit(nm_sd, split="(?<=.)(?=.{2}$)", perl=T)[[1]][1]
    par_ind <- match(nm_par, names(Pars))
    dnames <- dimnames(Pars[[par_ind]])
    
    Years <- dnames[["Year"]] |> as.numeric()
    Sims <- dnames[["Sim"]] |> as.numeric()
    nSim <- length(Sims)
    Pars[[par_ind]] <- RandomWalk(targ=Pars[[par_ind]],
                                  targsd=Pars[[i]],
                                  nSim,
                                  Years
                                  )
    Pars[[i]] <- NA
  }
  tt <- lapply(lapply(Pars, is.na), prod)
  ind <- which(tt==1)
  Pars[ind] <- NULL
  Pars
}



Structure <- function(value, out=c('nSim', 'nage', 'nTS'), req='nage') {

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







