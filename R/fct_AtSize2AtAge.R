
AtAge2AtSize <- function(object, Length, max1=TRUE) {
  MeanAtAge <- object@MeanAtAge
  AgeDim <- which(names(dimnames(MeanAtAge)) == "Age")
  BySim <- 'Sim' %in% names(dimnames(MeanAtAge))
  
  List <- InterpolateAtAge(object, Length)
  object <- List$object
  Length <- List$Length
  
  if (!BySim)
    cli::cli_abort("!BySim not done yet")
  
  
  d1 <- dim(Length@MeanAtAge)
  dnames <- names(dimnames(Length@MeanAtAge))
  SimInd <- which(dnames=='Sim')
  AgeInd <- which(dnames=='Age')
  TSInd <- which(dnames=='TimeStep')
  
  d2 <- dim(object@MeanAtLength)
  dnames <- names(dimnames(object@MeanAtLength))
  ClassInd <-  which(dnames=='Class')
  
  AtSize <- array(0, dim=c(d1[SimInd], d2[ClassInd], d1[TSInd]),
                  dimnames=list(Sim=dimnames(Length@MeanAtAge)[[SimInd]],
                                Class=dimnames(object@MeanAtLength)[[ClassInd]],
                                TimeStep=dimnames(Length@MeanAtAge)[[TSInd]])
  )
  dd <- dim(AtSize)
  nAge <- d1[AgeInd]
  for (Sim in 1:dd[1]) {
    for (TS in 1:dd[3]) {
      MeanAtAge_ts <- object@MeanAtAge[Sim, ,TS]
      ASK_ts <- Length@ASK[Sim,,,TS]
      ASK_tsvec <- apply(ASK_ts, 2, sum)
      ind <- max(which(ASK_tsvec>0))
      sums <- matrix(apply(ASK_ts, 2, sum), nAge, nClasses, byrow=TRUE)
      ASK_ts_stand <- ASK_ts/sums
      ASK_ts_stand[!is.finite(ASK_ts_stand)] <- 0
      AtSize[Sim,,TS] <- MeanAtAge_ts %*% ASK_ts_stand
      if (max1)
        AtSize[Sim,ind:ncol(ASK_ts),TS] <- 1
      
    }
  }
  
  par(mfrow=c(2,2))
  object@MeanAtAge[1,,1] |> plot(type='b')
  
  plot(object@MeanAtLength[1, ,1])
  lines(AtSize[1,,1])
  
}


GetTimeSteps <- function(List) {
  purrr::map(List, \(array){
    dnames <- dimnames(array)
    TSInd <- which(names(dnames)=='TimeStep')
    dnames[[TSInd]]
  }) |> unlist() |> unique() |> sort() |> as.numeric()
}

GetSims <- function(List) {
  purrr::map(List, \(array){
    dnames <- dimnames(array)
    TSInd <- which(names(dnames)=='Sim')
    dnames[[TSInd]]
  }) |> unlist() |> unique() |> sort() |> as.numeric()
}


InterpolateAtAge <- function(object, Length) {
  MeanAtAge <- object@MeanAtAge
  AgeDim <- which(names(dimnames(MeanAtAge)) == "Age")
  BySim <- 'Sim' %in% names(dimnames(MeanAtAge))
  
  if (dim(MeanAtAge)[AgeDim]>30)  # arbitrary number!
    return(list(object=object, Length=Length))
    
  # Generate higher resolution length-at-age
  # linear interpolate Mean length-at-age and CV length-at-age
  # for 11 time-steps in-between (e.g., months)
  
  Length@Classes <- object@Classes
  Length@ASK <- CalcAgeSizeKey(Length@MeanAtAge, Length@CVatAge, Length@Classes, Length@TruncSD, Length@Dist)
  
  Ages <- dimnames(MeanAtAge)[[AgeDim]] |> as.numeric()
  nAge <- length(Ages)
  SubAges <- seq(Ages[1], by=1/12, length.out=(nAge * 12)-11)
  nSubAges <- length(SubAges)
  RealAges <- seq(from=1, by=12, to=nSubAges)
  
  TSteps <- GetTimeSteps(list(MeanAtAge, Length@MeanAtAge, Length@CVatAge))
  if (BySim) {
    Sims <- GetSims(list(MeanAtAge, Length@MeanAtAge, Length@CVatAge))  
    MeanAtAge <- ArrayReduceDims(MeanAtAge) |> ArrayExpand(nSim=length(Sims), nAge, TSteps)
    Length@MeanAtAge <- ArrayReduceDims(Length@MeanAtAge) |> ArrayExpand(nSim=length(Sims), nAge, TSteps)
    Length@CVatAge <- ArrayReduceDims(Length@CVatAge) |> ArrayExpand(nSim=length(Sims), nAge, TSteps)
    
    dMeanAtAge <- dim(MeanAtAge)
    dLengthMeanAtAge <- dim(Length@MeanAtAge)
    dLengthCVAtAge <- dim(Length@CVatAge)
    
    objectMeanAtAge <- array(0, dim=c(dMeanAtAge[1], nSubAges, dMeanAtAge[3]),
                             dimnames=list(Sim=1:dMeanAtAge[1],
                                           Age=SubAges,
                                           TimeStep=TSteps))
    
    LengthMeanAtAge <- array(0, dim=c(dLengthMeanAtAge[1], nSubAges, dLengthMeanAtAge[3]),
                             dimnames=list(Sim=1:dLengthMeanAtAge[1],
                                           Age=SubAges,
                                           TimeStep=TSteps))
    
    LengthCVatAge <- array(0, dim=c(dLengthCVAtAge[1], nSubAges, dLengthCVAtAge[3]),
                           dimnames=list(Sim=1:dLengthCVAtAge[1],
                                         Age=SubAges,
                                         TimeStep=TSteps))
    
   
    
    objectMeanAtAge[,RealAges,] <- MeanAtAge[]
    LengthMeanAtAge[,RealAges,] <- Length@MeanAtAge[]
    LengthCVatAge[,RealAges,] <- Length@CVatAge[]
    
  } else {
   cli::cli_abort("!BySim not done yet") 
  }
  
  nSim <- length(Sims)
  nTS <- length(TSteps)
  for (Age in seq_along(RealAges)[-length(RealAges)]) {
    ind2 <- c(RealAges[Age], RealAges[Age]+12)
    ind3 <- (ind2[1]+1): (ind2[2]-1)
    
    for (Sim in 1:nSim) {
      for (TS in 1:nTS) {
        objectMeanAtAge[Sim,ind3,TS] <- approx(ind2, MeanAtAge[Sim,Age:(Age+1),Sim], xout=ind3)$y
        LengthMeanAtAge[Sim,ind3,TS] <- approx(ind2, Length@MeanAtAge[Sim,Age:(Age+1),Sim], xout=ind3)$y
        LengthCVatAge[Sim,ind3,TS] <- approx(ind2, Length@CVatAge[Sim,Age:(Age+1),Sim], xout=ind3)$y
      }
    }
  }
  
  Length@MeanAtAge <- LengthMeanAtAge
  Length@CVatAge <- LengthCVatAge
  # generate a new age-size key with finer temporal resolution
  Length@ASK <- CalcAgeSizeKey(MeanAtAge=LengthMeanAtAge,
                        CVatAge=LengthCVatAge,
                        Classes=Length@Classes,
                        TruncSD=Length@TruncSD,
                        Dist=Length@Dist,
                        Ages=NULL,
                        silent=TRUE)
  
  object@MeanAtAge <- objectMeanAtAge |> ArrayExpand(nSim, nSubAges, TSteps)
  return(list(object=object, Length=Length))
}



