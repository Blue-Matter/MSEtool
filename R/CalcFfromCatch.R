# Calculate apical F from Total Removals (retainted + dead discarded)
# by fleet

CalcFfromCatch <- function(CatchAtAge, 
                           PopatAge, 
                           NMortalityAtAge, 
                           SelectAtAge,
                           RetainAtAge=NULL,
                           DiscardAtAge=NULL,
                           control=NULL) {
  
  # Calculates apicalF for Dead fish 
  
  # CatchAtAge is CatchDead (retained and dead discarded fish)
  
  # Dimensions:
  # Catch  
  # 1. Sim, Age, Time Step, Fleet
 
  # PopatAge: same units as Catch
  # 1. Sim, Age, Time Step
  
  # NMortalityAtAge
  # 1. Sim, Age, Time Step
  
  # SelectAtAge, RetainAtAge & DiscardAtAge (or NULL for RetainAtAge & DiscardAtAge)
  # 1. Sim, Age, Time Step, Fleet
  
  # Calculate number of sims 
  CatchAtAgeDim <- dim(CatchAtAge)
  PopatAgeDim <- dim(PopatAge)
  NMortalityAtAgeDim <- dim(NMortalityAtAge)
  SelectAtAgeDim <- dim(SelectAtAge)
  RetainAtAgeDim <- dim(RetainAtAge)
  DiscardAtAgeDim <- dim(DiscardAtAge)
  
  nSims <- c(CatchAtAgeDim[1], 
             PopatAgeDim[1],
             PopatAgeDim[1],
             SelectAtAgeDim[1],
             RetainAtAgeDim[1],
             DiscardAtAgeDim[1]
  )
  nSim <- max(nSims)
  
  nTSs <- c(CatchAtAgeDim[3], 
           PopatAgeDim[3],
           PopatAgeDim[3],
           SelectAtAgeDim[3],
           RetainAtAgeDim[3],
           DiscardAtAgeDim[3]
  ) 
  nTS <- max(nTSs)

  
  nFleet <- SelectAtAgeDim[4]

  ApicalF <- array(NA, dim=c(nSim, nTS, nFleet))
  l <- list()
  l$Sim <- 1:nSim
  l$`Time Step` <- dimnames(CatchAtAge)$`Time Step`
  l$Fleet <- dimnames(CatchAtAge)$`Fleet`
  
  dimnames(ApicalF) <- l
  
  # loop over sims
  for (i in 1:nSim) {
    for (ts in 1:nTS) {
      CatchAtAge_i <- abind::adrop(CatchAtAge[GetIndex(i,nSims[1]),,GetIndex(ts,nTSs[1]),,drop=FALSE],c(1,3))
      PopatAge_i <- abind::adrop(PopatAge[GetIndex(i, nSims[2]),,GetIndex(ts,nTSs[2]),drop=FALSE],c(1,3))
      NMortalityAtAge_i <- abind::adrop(NMortalityAtAge[GetIndex(i, nSims[3]),,GetIndex(ts,nTSs[3]),drop=FALSE],c(1,3))
      SelectAtAge_i <- abind::adrop(SelectAtAge[GetIndex(i,nSims[4]),,GetIndex(ts,nTSs[4]),,drop=FALSE],c(1,3))
      RetainAtAge_i <- abind::adrop(RetainAtAge[GetIndex(i,nSims[5]),,GetIndex(ts,nTSs[5]),,drop=FALSE],c(1,3))
      DiscardAtAge_i <- abind::adrop(DiscardAtAge[GetIndex(i,nSims[6]),,GetIndex(ts,nTSs[6]),,drop=FALSE],c(1,3))
      
      ApicalF[i,ts,] <- CalcFfromCatch_i(CatchAtAge_i,
                                      PopatAge_i,
                                      NMortalityAtAge_i,
                                      SelectAtAge_i,
                                      RetainAtAge_i,
                                      DiscardAtAge_i,
                                      control=control)
      
    }
  }
  ApicalF
}

# calculates by Sim 
CalcFfromCatch_i <- function(CatchAtAge_i, 
                            PopatAge_i, 
                            NMortalityAtAge_i,
                            SelectAtAge_i=NULL,
                            RetainAtAge_i=NULL,
                            DiscardAtAge_i=NULL,
                            control=NULL) {
  
  # Dimensions:
  # Catch  
  # 1. Age, Fleet
  
  # PopatAge: same units as Catch
  # 1. Age
  
  # NMortalityAtAge
  # 1. Age
  
  # SelectAtAge, RetainAtAge & DiscardAtAge 
  # 1. Age, Fleet
  
  if (is.null(control)) 
    control <- list(MaxIt=300, tolF=1e-4)
  
  maxiterF <- control$MaxIt
  tolF <- control$tolF
  
  nAge <- dim(SelectAtAge_i)[1]
  nFleet <- dim(SelectAtAge_i)[2]
  
  AF <- expand.grid(1:nAge, 1:nFleet) |> as.matrix() 
  
  VulnPopAgeFleet <- SelectAtAge_i[]
  VulnPopAgeFleet[] <- NA
  VulnPopAgeFleet[AF] <- PopatAge_i[AF[,1]] * SelectAtAge_i[AF]
  VulnPopFleet <- colSums(VulnPopAgeFleet)
  
  # initial guess at apical F by fleet 
  apicalF <- colSums(CatchAtAge_i)/ colSums(VulnPopAgeFleet)
  apicalF[!is.finite(apicalF)] <- tiny
  

  if (all(CatchAtAge_i <=1E-6) || all(apicalF<=1E-6)) 
    return(rep(tiny, nFleet))
  
  for (i in 1:maxiterF) {
    apicalF_Age<- matrix(apicalF, nAge, nFleet, byrow=TRUE)
    FInteract <- ArrayMultiply(apicalF_Age, SelectAtAge_i)
    FRetain <- ArrayMultiply(FInteract, RetainAtAge_i)
    FDiscard <- ArraySubtract(FInteract, FRetain)
    FDeadDiscard <- ArrayMultiply(FDiscard, DiscardAtAge_i)
    FDead <- ArrayAdd(FRetain, FDeadDiscard)
    
    ZatAge <- ArrayAdd(rowSums(FDead), NMortalityAtAge_i)
    ZatAge[!is.finite(ZatAge)] <- 1E-9
    ZatAge[ZatAge==0] <- 1E-9
    
    PopDead <- ArrayMultiply((1-exp(-ZatAge)), PopatAge_i)
    
    ZatAge <- matrix(ZatAge,  nAge, nFleet, byrow=FALSE)
    PopDead <- matrix(PopDead,  nAge, nFleet, byrow=FALSE)
    PopatAge_mat <- matrix(PopatAge_i,  nAge, nFleet, byrow=FALSE)
   
    predRemoval <- ArrayMultiply(ArrayDivide(FDead,ZatAge), PopDead)
    predRemoval[!is.finite(predRemoval)] <- 0 
    
    # derivative of predCatch wrt apicalF
    dctarray <- array(NA, dim=dim(FDead))
    dctarray[AF] <- PopDead/ZatAge - ((FDead * PopDead)/ZatAge^2) +
      FDead/ZatAge * exp(-ZatAge) * PopatAge_mat
    
    dct <- apply(dctarray,2, sum)  
    
    apicalF <- apicalF - (colSums(predRemoval) -colSums(CatchAtAge_i))/(0.8*dct)
    
    # check for convergence
    converge <- abs(predRemoval - CatchAtAge_i)/CatchAtAge_i
    converge[!is.finite(converge)] <- 0
    if (all(converge < tolF))
      break()
  }
  apicalF
}


