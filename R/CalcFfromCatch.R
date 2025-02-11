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