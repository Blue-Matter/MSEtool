# 
CalcFfromCatchArea <- function(Catch, PopatAge, MatAge, SelectAtAge) {
  
  maxiterF <- 300
  tolF <- 1e-4
  
  # TODO add controls
  # Catch should really by CatchInteract - for cases where there is discard mortality < 1
  # This function can calculate FInteract which is directly proportional to Effort
  # while apical F can be lower than FInteract
  
  # if(!is.null(control$maxiterF) && is.numeric(control$maxiterF)) maxiterF <- as.integer(control$maxiterF)
  # if(!is.null(control$tolF) && is.numeric(control$tolF)) tolF <- control$tolF
  # Catch = dead ; add option for catch = retained
  
  # TODO add retention and discard 
  
  CatchAreaFleet <- apply(Catch, 2:3, sum)
  VulnPopAreaFleet <- apply(PopatAge * SelectAtAge, 2:3, sum)
  apicalF <- CatchAreaFleet/VulnPopAreaFleet
  apicalF[!is.finite(apicalF)] <- tiny
  
  # if (Catch <= 1E-9 || apicalF <= 1E-9) 
  #   return(tiny)
  
  nAge <- dim(SelectAtAge)[1]
  
  for (i in 1:maxiterF) {
    FatAge <- aperm(replicate(nAge, apicalF), c(3,1,2)) *SelectAtAge
    ZatAge <- FatAge + MatAge
    ZatAge[ZatAge==0] <- 1E-9
    
    PopDead <- (1-exp(-ZatAge)) * PopatAge
    predCatch <- FatAge/ZatAge * PopDead
    predCatch[!is.finite(predCatch)] <- 0 
   
    predCatchTotal <- apply(predCatch, 2:3, sum)
    # derivative of predCatch wrt apicalF
    dct <- apply(PopDead/ZatAge - ((FatAge * PopDead)/ZatAge^2) + FatAge/ZatAge * exp(-ZatAge) * PopatAge,
                 2:3, sum)
    
    # if (dct<1E-15) break
    apicalF <- apicalF - (predCatchTotal - CatchAreaFleet)/(0.8*dct)
    
    # check for convergence
    converge <- abs(predCatchTotal - CatchAreaFleet)/CatchAreaFleet
    converge[!is.finite(converge)] <- 0
    if (all(converge < tolF))
      break()
    
    # if (abs(predCatchTotal - CatchAreaFleet)/CatchAreaFleet < tolF) break
  }
  apicalF
}


CalcFfromCatch <- function(Catch, PopatAge, MatAge, SelectAtAge) {
  
  maxiterF <- 300
  tolF <- 1e-4
  
  # TODO add controls 
  # if(!is.null(control$maxiterF) && is.numeric(control$maxiterF)) maxiterF <- as.integer(control$maxiterF)
  # if(!is.null(control$tolF) && is.numeric(control$tolF)) tolF <- control$tolF
  # Catch = dead ; add option for catch = retained
  
  # TODO add retention and discard 
  
  
  apicalF <- Catch/sum(PopatAge * SelectAtAge) # initial guess
  if (Catch <= 1E-9 || apicalF <= 1E-9) 
    return(tiny)
  
  for (i in 1:maxiterF) {
    FatAge <- apicalF*SelectAtAge
    ZatAge <- FatAge + MatAge
    ZatAge[ZatAge==0] <- tiny
    
    PopDead <- (1-exp(-ZatAge)) * PopatAge
    predCatch <- FatAge/ZatAge * PopDead
    predCatch[!is.finite(predCatch)] <- 0 
    predCatchTotal <- sum(predCatch)
    
    # derivative of predCatch wrt apicalF
    dct <- sum(PopDead/ZatAge - ((FatAge * PopDead)/ZatAge^2) + FatAge/ZatAge * exp(-ZatAge) * PopatAge)
    
    if (dct<1E-15) break
    apicalF <- apicalF - (predCatchTotal - Catch)/(0.8*dct)
    if (abs(predCatchTotal - Catch)/Catch < tolF) break
  }
  apicalF
}