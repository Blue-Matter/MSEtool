
AtSize2AtAge <- function(object, Length) {
  # OBJ <<- object
  # LEN <<- Length
  
  if (inherits(Length, 'length')) {
    MeanAtSize <- object@MeanAtLength
  } else if (inherits(Length, 'weight')) {
    MeanAtSize <- object@MeanAtWeight
  } else {
    cli::abort("`Length` must be an object of class `length` or `weight`")
  }
  
  ASK <- Length@ASK 
  if (is.null(ASK)) 
    cli::cli_abort("`Length@ASK` is not populated", .internal=TRUE)
  dim_MeanAtSize <- dim(MeanAtSize)
  dim_ASK <- dim(ASK)
  
  DNames <- names(dimnames(Length@MeanAtAge))
  bySim <- TRUE
  if ("Sim" %in% DNames) {
    nage <- dim_ASK[2]
    nClasses <- dim_ASK[3]
    
    nsim_MeanAtSize <- dim_MeanAtSize[1]
    nTS_MeanAtSize <- dim_MeanAtSize[3]
    
    nsim_ASK <- dim_ASK[1]
    nTS_ASK <- dim_ASK[4]
    
    nsim <- max(nsim_MeanAtSize, nsim_ASK) # maximum number of simulations
    nTS <- max(nTS_MeanAtSize, nTS_ASK) # maximum number of time-steps
  } else {
    bySim <- FALSE
    
    nage <- dim_ASK[1]
    nClasses <- dim_ASK[2]
    
    nsim_ASK <- 1
    nsim_MeanAtSize <- 1
    nsim <- 1
    
    nTS_ASK <- 1
    nTS_MeanAtSize <- 1
    nTS <- dim_MeanAtSize[3]
  }
  
  AtAge <- array(0, dim=c(nsim, nage, nTS))
  for (s in 1:nsim) {
    for (t in 1:nTS) {
      MeanAtSize_ts <- MeanAtSize[GetIndex(s, nsim_MeanAtSize), ,GetIndex(t, nTS_MeanAtSize)]
      if (all(MeanAtSize_ts>0.99)) {
        AtAge[s,,t] <- 1
      } else {
        if (bySim) {
          ASK_ts <- ASK[GetIndex(s, nsim_ASK),,,GetIndex(t, nTS_ASK)]
          AtAge[s,,t] <- MeanAtSize_ts %*%t(ASK_ts)
        } else {
          ASK_ts <- ASK[,,GetIndex(t, nTS_ASK)]
          AtAge[s,,t] <- (MeanAtSize_ts %*%t(ASK_ts))[1,]
        }
      }
    }
  }
  dd <- dim(AtAge)
  
  MeanAtSizeDNAmes <- MeanAtSize |> dimnames() |> names()
  LengthASKDNAmes <- Length@ASK |> dimnames() |> names()
  
  ind1 <- which(MeanAtSizeDNAmes == 'Year')
  ind2 <- which(LengthASKDNAmes == 'Year')
  
  TSnames <- c(dimnames(MeanAtSize)[[ind1]],
               dimnames(Length@ASK)[[ind2]]) |>
    unique() |>
    sort()
  
  AgeInd <- which(names(dimnames(Length@MeanAtAge))=='Age')
  dimnames(AtAge) <- list(Sim=1:dd[1],
                          Age=dimnames(Length@MeanAtAge)[[AgeInd]],
                          Year=TSnames)
  AtAge
}


