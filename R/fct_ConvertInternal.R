OM2stock <- function(OM, cpars=NULL, TimeSteps=NULL) {
  stock <- Stock()
  if (inherits(OM, 'OM')) {
    stock@Name <- SubOM(OM, 'Stock')@Name
  } else {
    stock@Name <- OM@Name
  }
  
  stock@CommonName <- OM@Common_Name
  stock@Species <- OM@Species
  stock@Ages <- Ages(OM@maxage)
  
  Length(stock) <- OM2Length(OM, cpars, TimeSteps)
  Weight(stock) <- OM2Weight(OM, cpars)
  NaturalMortality(stock) <- OM2NaturalMortality(OM, cpars)
  Maturity(stock) <- OM2Maturity(OM, cpars)
  Fecundity(stock) <- OM2Fecundity(OM, cpars)
  SRR(stock) <- OM2SRR(OM, cpars, TimeSteps)
  Spatial(stock) <- OM2Spatial(OM, cpars, TimeSteps)
  Depletion(stock) <- OM2Depletion(OM, cpars)
  stock
}

