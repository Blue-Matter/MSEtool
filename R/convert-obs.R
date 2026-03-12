#' Convert a Legacy Obs Object to a New obs Class
#'
#' Converts a legacy [Obs-legacy-class] object to the current [obs-class] by
#' mapping observation error parameters to their corresponding new S4 slots.
#'
#' @param Obs An [Obs-legacy-class] object to convert.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' Not all slots from [Obs-legacy-class] are represented in [obs-class]. The
#' following legacy slots are not currently converted and are silently dropped:
#'
#' - Life history bias CVs: `Linfbiascv`, `t0biascv`, `Kbiascv`,
#'   `LenMbiascv`, `Mbiascv`
#' - Selectivity bias CVs: `LFCbiascv`, `LFSbiascv`
#' - Reference point bias CVs: `FMSY_Mbiascv`, `BMSY_B0biascv`
#'
#' Note that `Discards@CV` and `Discards@Bias` are set to the same values as
#' `Landings` (`Cobs` and `Cbiascv`) as legacy objects do not distinguish
#' between landings and discards observation error.
#'
#' @return An [obs-class] object.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [ConvertImp()]
#'
#' @examples
#' \dontrun{
#' Obslegacy <- readRDS("MyLegacyObs.rds")
#' obs_new <- ConvertObs(Obslegacy)
#' }
#'
#' @export
ConvertObs <- function(Obs, silent = FALSE) {
  CheckClass(Obs, c("Obs",'OM'), "Obs")
  
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
  
  # obs@CAA@SampleSize <- Obs@CAA_nsamp
  # obs@CAA@ESS <- Obs@CAA_ESS
  # 
  # obs@CAL@SampleSize <- Obs@CAL_nsamp
  # obs@CAL@ESS <- Obs@CAL_ESS
  
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

