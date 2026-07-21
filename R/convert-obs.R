#' Convert a Legacy Obs Object to the Current `obs` Class
#'
#' Converts a legacy [Obs-legacy-class] object to the current [obs-class] by
#' mapping observation error parameters to their corresponding sub-object slots.
#'
#' @param Obs An [Obs-legacy-class] object to convert. An [om-class] object
#'   with a legacy `Obs` slot is also accepted, in which case the `Obs` slot
#'   is extracted and converted.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' ## Slot Mapping
#'
#' The following legacy slots are mapped to their current equivalents:
#'
#' | Legacy slot | New slot |
#' |---|---|
#' | `Eobs` | `Effort@CV` |
#' | `Ebiascv` | `Effort@Bias` |
#' | `Cobs` | `Landings@CV` and `Discards@CV` |
#' | `Cbiascv` | `Landings@Bias` and `Discards@Bias` |
#' | `Iobs` | `CPUE@CV` and `Survey@CV` |
#'
#' Note that `Discards@CV` and `Discards@Bias` are set to the same values as
#' `Landings` (`Cobs` and `Cbiascv`) because legacy objects do not distinguish
#' between landings and discards observation error.
#'
#' ## Slots Not Yet Converted
#'
#' The following legacy slots do not have a corresponding slot in the current
#' [obs-class] and are silently dropped. They will be mapped once the
#' [lifehistoryobs-class] and [exploitationobs-class] placeholders are fully
#' implemented:
#'
#' - Life-history bias CVs: `Linfbiascv`, `t0biascv`, `Kbiascv`,
#'   `LenMbiascv`, `Mbiascv`
#' - Selectivity bias CVs: `LFCbiascv`, `LFSbiascv`
#' - Reference point bias CVs: `FMSY_Mbiascv`, `BMSY_B0biascv`
#' - Composition data: `CAA_nsamp`, `CAA_ESS`, `CAL_nsamp`, `CAL_ESS`
#'   (mapped to `LandingsAtAge` and `LandingsAtSize` once implemented)
#'
#' @return An [obs-class] object.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [ConvertImp()] for
#'   related legacy conversion functions.
#'
#' @examples
#' \dontrun{
#' obs_legacy <- readRDS("MyLegacyObs.rds")
#' obs_new <- ConvertObs(obs_legacy)
#' }
#'
#' @export
ConvertObs <- function(Obs, silent = FALSE) {
  .CheckClass(Obs, c("Obs", "OM"), "Obs")
  
  if (!silent)
    cli::cli_alert("Converting object of class {.cls Obs} to class {.cls obs}")
  
  obs <- Obs()
  obs@Name <- Obs@Name
  
  obs@Effort@CV   <- Obs@Eobs
  obs@Effort@Bias <- Obs@Ebiascv
  
  obs@Landings@CV   <- Obs@Cobs
  obs@Landings@Bias <- 1 # Obs@Cbiascv 
  
  # Legacy does not separate landings from discards; apply the same values
  obs@Discards@CV   <- Obs@Cobs
  obs@Discards@Bias <- 1 # Obs@Cbiascv
  
  obs@CPUE@CV   <- Obs@Iobs
  obs@Survey@CV <- Obs@Iobs
  

  # Composition slots 
  obs@LandingsAtAge@SampleSize  <- Obs@CAA_nsamp
  obs@LandingsAtAge@ESS         <- Obs@CAA_ESS
  obs@LandingsAtSize@SampleSize <- Obs@CAL_nsamp
  obs@LandingsAtSize@ESS        <- Obs@CAL_ESS
  
  
  # Life-history bias CVs 
  # Obs@Linfbiascv, Obs@t0biascv, Obs@Kbiascv, Obs@LenMbiascv, Obs@Mbiascv
  #
  # Selectivity bias CVs 
  # Obs@LFCbiascv, Obs@LFSbiascv
  #
  # Reference point bias CVs:
  # Obs@FMSY_Mbiascv, Obs@BMSY_B0biascv
  
  obs
}
