# 
CalcFfromCatch <- function(CatchAtAge, 
                           PopatAge, 
                           NMortalityAtAge, 
                           SelectAtAge,
                           RetainAtAge=NULL,
                           DiscardAtAge=NULL,
                           control=NULL) {
  
  
  
  # CatchAtAge can be either:
  #
  # 1. CatchInteract (all fish caught and some discarded alive)
  # 2. CatchDead (retained and dead discarded fish)
  # 
  # 1. Will calculate apicalF Interact - i.e., proportional to Effort
  # 2. Will calculate apicalF Dead. i.e., the actual fishing mortality for 
  #    dead fish removed from population. 
  #    apicalF Interact and Dead will be the same if there is no discarding, 
  #    or all discards die, but otherwise apicalF Dead will be lower than
  #    apicalF Interact
  #
  # RetainAtAge & DiscardAtAge are only required for calculation of apicalF Dead
  
  # NOTE: most of the time the Area dimension isn't needed. It is only used to 
  # calculate the F on the overall population from area-specific catches
  # i.e., this is different than the F within that area
  
  # Dimensions:
  # Catch  
  # 1. Age
  # 2. Age, Fleet
  # 3. Age, Fleet, Area
  
  # PopatAge
  # 1. Age
  
  # NMortalityAtAge
  # 1. Age
  
  # SelectAtAge, RetainAtAge & DiscardAtAge (or NULL for RetainAtAge & DiscardAtAge)
  # 1. Age 
  # 2. Age, Fleet
  
  if(!is.null(RetainAtAge))
    cli::cli_abort('RetainAtAge not done yet', .internal=TRUE)
  
  if(!is.null(DiscardAtAge))
    cli::cli_abort('DiscardAtAge not done yet', .internal=TRUE)
  
  # ---- Controls -----
  if (is.null(control)) 
    control <- list(MaxIt=300, tolF=1e-4)
  
  maxiterF <- control$MaxIt
  tolF <- control$tolF
  
  # ---- Set up Arrays -----
  CatchAtAgeDim <- dim(CatchAtAge)
  PopatAgeDim <- dim(PopatAge)
  SelectAtAgeDim <- dim(SelectAtAge)
  RetainAtAgeDim <- dim(RetainAtAge)
  DiscardAtAgeDim <- dim(DiscardAtAge)
  
  # Add Dimensions as Needed
  if (is.null(CatchAtAgeDim)) {
    CatchAtAge <- CatchAtAge |> array() |> AddDimension() |> AddDimension()
  } 
  if (length(CatchAtAgeDim)==2) {
    CatchAtAge <- CatchAtAge |> AddDimension() 
  }

  if (is.null(SelectAtAgeDim)) {
    if (length(SelectAtAge)==1) {
      SelectAtAge <- array(SelectAtAge, dim=c(1,1))
    } else {
      SelectAtAge <- SelectAtAge |> array() |> AddDimension()   
    }
    
  } 
  
  if (is.null(RetainAtAge)) {
    RetainAtAge <- SelectAtAge
    RetainAtAge[] <- 1
    RetainAtAgeDim <- dim(RetainAtAge)
  } else {
    if (is.null(RetainAtAgeDim)) {
      RetainAtAge <- RetainAtAge |> array() |> AddDimension() 
    } 
  }

  if (is.null(DiscardAtAge)) {
    DiscardAtAge <- SelectAtAge
    DiscardAtAge[] <- 0
    DiscardAtAgeDim <- dim(DiscardAtAge)
  } else {
    if (is.null(DiscardAtAgeDim)) {
      DiscardAtAge <- DiscardAtAge |> array() |> AddDimension() 
    } 
  }

  # Check Dimensions 
  nAge <- length(NMortalityAtAge)
  CatchAtAgeDim <- dim(CatchAtAge)
  SelectAtAgeDim <- dim(SelectAtAge)
  RetainAtAgeDim <- dim(RetainAtAge)
  DiscardAtAgeDim <- dim(DiscardAtAge)
  
  checkAge <- c(CatchAtAgeDim[1], SelectAtAgeDim[1], RetainAtAgeDim[1],DiscardAtAgeDim[1]) == nAge
  if (!any(checkAge))
    cli::cli_abort('All arrays must have equal number of age classes')
  
  chk <- c(length(CatchAtAgeDim)) == 3
  if (any(!chk))
    cli::cli_abort('`CatchAtAge` must have 3 dimensions')
  
  chk <- c(length(SelectAtAgeDim), length(RetainAtAgeDim), length(DiscardAtAgeDim)) == 2
  if (any(!chk))
    cli::cli_abort('`SelectAtAge`, `RetainAtAge`, and `DiscardAtAge` must have 2 dimensions')
  
  nFleet <- SelectAtAgeDim[2]
  nArea <- CatchAtAgeDim[3]
  
  # ---- Calcs ----
  AFR <- expand.grid(1:nAge, 1:nFleet, 1:nArea) |> as.matrix() # age, fleet, area (R)
  
  CatchFleetArea <- apply(CatchAtAge, 2:3, sum)
  VulnPopAgeFleet <- array(NA, dim=c(nAge, nFleet))
  VulnPopAgeFleet[AFR[,1:2]] <- PopatAge[AFR[,1]] * SelectAtAge[AFR[,1:2]]
  
  VulnPopFleet <- apply(VulnPopAgeFleet, 2, sum)
  
  apicalF <- array(NA, dim=c(nFleet, nArea))
  # initial guess at apical F by fleet area
  apicalF[AFR[,2:3]] <- CatchFleetArea[AFR[,2:3]]/VulnPopFleet[AFR[,2]] 
  apicalF[!is.finite(apicalF)] <- tiny
  
  if (all(CatchFleetArea <=1E-6) || all(apicalF<=1E-6)) {
    out <- array(tiny, dim=c(nFleet, nArea))
    CollapseArray(out)
  }

  for (i in 1:maxiterF) {

    FatAge <- array(NA, dim=c(nAge, nFleet, nArea))
    PopDead <- ZatAge <- array(NA, dim=c(nAge))
    FatAge[AFR] <- apicalF[AFR[,2:3]] * SelectAtAge[AFR[,1:2]]
    
    ZatAge <- apply(FatAge,1, sum) + NMortalityAtAge
    ZatAge[!is.finite(ZatAge)] <- 1E-9
    ZatAge[ZatAge==0] <- 1E-9
    
    PopDead <- (1-exp(-ZatAge)) * PopatAge
    
    predCatch <- FatAge/ZatAge * PopDead
    predCatch[!is.finite(predCatch)] <- 0 
    
    predCatchTotal <- apply(predCatch, 2:3, sum)  
    
    # derivative of predCatch wrt apicalF
    dctarray <- array(NA, dim=dim(FatAge))
    dctarray[AFR] <- PopDead/ZatAge - ((FatAge[AFR] * PopDead)/ZatAge^2) +
      FatAge[AFR]/ZatAge * exp(-ZatAge) * PopatAge
    
    dct <- apply(dctarray, 2:3, sum)  
    
    apicalF <- apicalF - (predCatchTotal - CatchFleetArea)/(0.8*dct)
 
    # check for convergence
    converge <- abs(predCatchTotal - CatchFleetArea)/CatchFleetArea
    converge[!is.finite(converge)] <- 0
    if (all(converge < tolF))
      break()
  }
  CollapseArray(apicalF)
}

CollapseArray <- function(out) {
  d <- dim(out)
  if (all(d==1)) {
    return(out[1,1])
  }
  if (d[1]==1) {
    return(out[1,])
  }
  if (d[2]==1) {
    return(out[,1])
  }
}
# 
# CalcFfromCatch <- function(Catch, PopatAge, MatAge, SelectAtAge) {
#   
#   maxiterF <- 300
#   tolF <- 1e-4
#   
#   # TODO add controls 
#   # if(!is.null(control$maxiterF) && is.numeric(control$maxiterF)) maxiterF <- as.integer(control$maxiterF)
#   # if(!is.null(control$tolF) && is.numeric(control$tolF)) tolF <- control$tolF
#   # Catch = dead ; add option for catch = retained
#   
#   # TODO add retention and discard 
#   
#   
#   apicalF <- Catch/sum(PopatAge * SelectAtAge) # initial guess
#   if (Catch <= 1E-9 || apicalF <= 1E-9) 
#     return(tiny)
#   
#   for (i in 1:maxiterF) {
#     FatAge <- apicalF*SelectAtAge
#     ZatAge <- FatAge + MatAge
#     ZatAge[ZatAge==0] <- tiny
#     
#     PopDead <- (1-exp(-ZatAge)) * PopatAge
#     predCatch <- FatAge/ZatAge * PopDead
#     predCatch[!is.finite(predCatch)] <- 0 
#     predCatchTotal <- sum(predCatch)
#     
#     # derivative of predCatch wrt apicalF
#     dct <- sum(PopDead/ZatAge - ((FatAge * PopDead)/ZatAge^2) + FatAge/ZatAge * exp(-ZatAge) * PopatAge)
#     
#     if (dct<1E-15) break
#     apicalF <- apicalF - (predCatchTotal - Catch)/(0.8*dct)
#     if (abs(predCatchTotal - Catch)/Catch < tolF) break
#   }
#   apicalF
# }