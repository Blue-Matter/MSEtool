AtAge2AtSize <- function(object, Length, max1=TRUE) {
  
  MeanAtAge <- object@MeanAtAge
  ASK <- Length@ASK 
  AgeDim <- which(names(dimnames(MeanAtAge)) == "Age")
  nAgeClasses <- dim(MeanAtAge)[AgeDim]
  
  BySim <- 'Sim' %in% names(dimnames(ASK))
  
  
  if (dim(MeanAtAge)[AgeDim]<30) { # arbitrary number!
    # Generate higher resolution length-at-age
    # linear interpolate Mean length-at-age and CV length-at-age
    # for 11 time-steps in-between (e.g., months)
    dims  <- rbind(dim(Length@MeanAtAge),
                   dim(Length@CVatAge),
                   dim(MeanAtAge)
    )
    dd <- apply(dims, 2, max)
    
    nSubAges <- 12
    SubAgeDim <- (dd[2]*nSubAges)-(nSubAges-1)
    
    objectMeanAtAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    LengthMeanAtAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    LengthCVatAge <- array(0, dim=c(dd[1], SubAgeDim, dd[3]))
    
    dname1 <- dimnames(Length@MeanAtAge)
    ages <- as.numeric(dname1[["Age"]])
    dname1[["Age"]] <- seq(from=ages[1], to=ages[length(ages)], length.out=SubAgeDim)
    
    TSteps <- c(dimnames(MeanAtAge)[[3]], dimnames(Length@MeanAtAge)[[3]]) |> unique() |> sort()
    dname1[["Year"]] <- TSteps
    
    dname1$Sim <- 1:dd[1]
    dimnames(LengthMeanAtAge) <- dname1
    
    Length@CVatAge <- ArrayExpand(Length@CVatAge, dd[1], dd[2], dname1[["Year"]])
    dname1 <- dimnames(Length@CVatAge)
    dname1[["Age"]] <- seq(from=ages[1], to=ages[length(ages)], length.out=SubAgeDim)
    TSteps <- c(dimnames(MeanAtAge)[[3]], dimnames(Length@CVatAge)[[3]]) |> unique() |> sort()
    dname1[["Year"]] <- TSteps
    
    dimnames(LengthCVatAge) <- dname1
    
    ind <- seq(from=1, by=nSubAges, to=dim(LengthMeanAtAge)[2])
    
    MeanAtAge <-ArrayExpand(MeanAtAge, dd[1], nAges=dd[2], Years=TSteps)
    
    objectMeanAtAge[,ind,] <- MeanAtAge[]
    LengthMeanAtAge[,ind,] <- Length@MeanAtAge[]
    LengthCVatAge[,ind,] <- Length@CVatAge[]
    
    dims  <- rbind(dim(Length@MeanAtAge),
                   dim(Length@CVatAge),
                   dim(MeanAtAge)
    )
    
    for (i in 1:(length(ind)-1)) {
      ind2 <- c(ind[i], ind[i]+12)
      ind3 <- (ind2[1]+1): (ind2[2]-1)
      
      # silly loop for now!
      for (s in 1:max(dims[,1])) {
        for (ts in 1:max(dims[,3])) {
          
          # Linear interpolate at-Age schedule
          temp <- approx(ind2, 
                         MeanAtAge[GetIndex(s, dims[1,1]),i:(i+1), GetIndex(s, dims[1,3])],
                         xout=ind3)
          objectMeanAtAge[s,ind3,ts] <- temp$y
          
          # Linear interpolate growth curve
          temp <- approx(ind2, Length@MeanAtAge[GetIndex(s, dims[1,1]),
                                                i:(i+1),
                                                GetIndex(s, dims[1,3])],
                         xout=ind3)
          
          LengthMeanAtAge[s,ind3,ts] <- temp$y
          
          
          
          if (dims[2,2]==1) {
            LengthCVatAge[s,,ts] <- Length@CVatAge[GetIndex(s, dims[2,1]),,
                                                   GetIndex(s, dims[2,3])]
          } else {
            temp <- approx(ind2, Length@CVatAge[GetIndex(s, dims[2,1]),
                                                i:(i+1),
                                                GetIndex(s, dims[2,3])],
                           xout=ind3)
            LengthCVatAge[s,ind3,ts] <- temp$y
          }
        }
      }
    }
    
    # generate a new age-size key with finer temporal resolution
    ASK <- CalcAgeSizeKey(MeanAtAge=LengthMeanAtAge,
                          CVatAge=LengthCVatAge,
                          Classes=Length@Classes,
                          TruncSD=Length@TruncSD,
                          Dist=Length@Dist,
                          AgeClasses=NULL,
                          silent=TRUE)
    
    MeanAtAge <- objectMeanAtAge
    dimnames(MeanAtAge) <- dname1
    
  }
  
  dim_MeanAtAge <- dim(MeanAtAge)
  dim_ASK <- dim(ASK)
  nage <- dim_ASK[2]
  nClasses <- dim_ASK[3]
  
  if (dim_MeanAtAge[2] != dim_ASK[2])
    cli::cli_abort('`dim(MeanAtAge)[2] != dim(ASK)[2]`')
  
  nsim_MeanAtAge <- dim_MeanAtAge[1]
  nTS_MeanAtAge <- dim_MeanAtAge[3]
  
  nsim_ASK <- dim_ASK[1]
  nTS_ASK <- dim_ASK[4]
  
  nsim <- max(nsim_MeanAtAge, nsim_ASK) # maximum number of simulations
  nTS <- max(nTS_MeanAtAge, nTS_ASK) # maximum number of time-steps
  
  AtSize <- array(0, dim=c(nsim, nClasses, nTS))
  
  for (s in 1:nsim) {
    for (t in 1:nTS) {
      MeanAtAge_ts <- MeanAtAge[GetIndex(s, nsim_MeanAtAge), ,GetIndex(t, nTS_MeanAtAge)]
      ASK_ts <- ASK[GetIndex(s, nsim_ASK),,,GetIndex(t, nTS_ASK)]
      ASK_tsvec <- apply(ASK_ts, 2, sum)
      ind <- max(which(ASK_tsvec>0))
      
      
      sums <- matrix(apply(ASK_ts, 2, sum), nage, nClasses, byrow=TRUE)
      ASK_ts_stand <- ASK_ts/sums
      ASK_ts_stand[!is.finite(ASK_ts_stand)] <- 0
      AtSize[s,,t] <- MeanAtAge_ts %*% ASK_ts_stand
      if (max1)
        AtSize[s,ind:ncol(ASK_ts),t] <- 1
    }
  }
  
  dimnames(AtSize) <- list(Sim=1:nsim,
                           Class=Length@Classes,
                           Year=dimnames(MeanAtAge)[["Year"]][1:nTS])
  AtSize 
}

