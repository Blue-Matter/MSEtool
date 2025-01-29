
# devtools::load_all()

# NOTE: Curves do not account for spatial closures




CalcSPRCurve <- function(OM, 
                         messages='default',
                         nSim=NULL,
                         parallel=FALSE,
                         FSearch=NULL,
                         SPR0=NULL) {
  
  # TODO add option to specify Time Steps to calculate
  # currently does all
  
  # TODO skip for male stocks, modify for herm species
  OM <- StartUp(OM, messages, nSim) 
  if (is.null(FSearch))
    FSearch <- OM@Control$Curves$FSearch
  CalcSPR(OM, FSearch, SPR0)
}
