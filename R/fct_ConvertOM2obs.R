OM2obs <- function(OM, cpars=NULL) {
  obs <- Obs()
  Obs <- SubOM(OM, 'Obs')
  obs@Name <- Obs@Name
  
  
  obs@Catch@Bias <- Obs@Cbiascv
  obs@Catch@CV <- Obs@Cobs
  
  obs@Index@CV <- Obs@Iobs
  obs@Index@Beta <-  Obs@beta
 
  
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