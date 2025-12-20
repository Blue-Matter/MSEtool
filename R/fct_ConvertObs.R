#' @rdname Convert
#' @param Obs An [Obs-class] object#' 
#' @export
ConvertObs <- function(Obs, silent = FALSE) {
  CheckClass(Obs, "Obs", "Obs")
  
  if (!silent) {
    cli::cli_alert("Converting object of class {.cls Obs} to class {.cls obs}")
  }
  
  obs <- Obs()
  obs@Name <- Obs@Name
  
  obs@Effort@CV <- Obs@Eobs
  obs@Effort@Bias <- Obs@Ebiascv
  
  obs@Landings@CV <- Obs@Cobs
  obs@Landings@Bias <- Obs@Cbiascv 
  
  obs@Discards@CV <- Obs@Cobs
  obs@Discards@Bias <- Obs@Cbiascv 
  
  obs@CPUE@CV <- Obs@Iobs 
  obs@Survey@CV <- Obs@Iobs
  
  obs@CAA@SampleSize <- Obs@CAA_nsamp
  obs@CAA@ESS <- Obs@CAA_ESS
  
  obs@CAL@SampleSize <- Obs@CAL_nsamp
  obs@CAL@ESS <- Obs@CAL_ESS
  
  #   # Life History 
  # TODO 
  # Obs@Linfbiascv
  # Obs@t0biascv
  # Obs@Kbiascv
  # Obs@LenMbiascv
  # Obs@Mbiascv
  # 
  # 
  # Obs@LFCbiascv
  # Obs@LFSbiascv
  # 
  # Obs@FMSY_Mbiascv
  # Obs@BMSY_B0biascv
  
  obs
}

