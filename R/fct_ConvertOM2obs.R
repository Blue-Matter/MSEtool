OM2obs <- function(OM, cpars=NULL) {
  obs <- Obs()
  Obs <- SubOM(OM, 'Obs')
  obs@Name <- Obs@Name
  
  
  obs@Removals@Bias <- Obs@Cbiascv
  obs@Removals@CV <- Obs@Cobs
  
  obs@Landings@Bias <- Obs@Cbiascv
  obs@Landings@CV <- Obs@Cobs
  
  obs@CPUE@CV <- Obs@Iobs
  obs@CPUE@Beta <-  Obs@beta
  
  obs@Survey@CV <- Obs@Iobs
  obs@Survey@Beta <-  Obs@beta 

  Obs@CAA_nsamp
  Obs@CAA_ESS
  
  Obs@CAL_nsamp
  
  Obs@CAL_ESS
  

  Obs@Btobs
  
  Obs@Btbiascv
  
  Obs@LenMbiascv
  
  Obs@Mbiascv
  
  Obs@Kbiascv
  
  Obs@t0biascv
  
  Obs@Linfbiascv
  
  Obs@LFCbiascv
  
  Obs@LFSbiascv
  
  Obs@FMSY_Mbiascv
  
  Obs@BMSY_B0biascv
  
  Obs@Irefbiascv
  
  Obs@Brefbiascv
  
  Obs@Crefbiascv
  
  Obs@Dbiascv
  
  Obs@Dobs
  
  Obs@hbiascv
  
  Obs@Recbiascv
  
  Obs@sigmaRbiascv
  
  Obs@Eobs
  
  Obs@Ebiascv
  
  
  obs
}