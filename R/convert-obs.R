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
#' The legacy bias CVs of life history and selectivity become the `Bias` of
#' the named parameter in [LifeHistoryObs()] and [ExploitationObs()]. A legacy
#' bias CV is the CV of the lognormal from which one bias per simulation is
#' drawn, which is how a scalar `Bias` is interpreted:
#'
#' | Legacy slot | New slot |
#' |---|---|
#' | `Linfbiascv`, `Kbiascv`, `t0biascv` | `LifeHistory@Length`, `Bias` of `Linf`, `K`, `t0` |
#' | `Mbiascv` | `LifeHistory@NaturalMortality`, `Bias` of `M` |
#' | `LenMbiascv` | `LifeHistory@Maturity`, `Bias` of `L50` |
#' | `hbiascv`, `sigmaRbiascv` | `LifeHistory@SRR`, `Bias` of `h` and `SD` |
#' | `Dbiascv` | `LifeHistory@Depletion`, `Bias` of `Final` |
#' | `LFCbiascv`, `LFSbiascv` | `Exploitation@Selectivity`, `Bias` of `L5` and `LFS` |
#'
#' ## Slots Not Converted
#'
#' The following legacy slots do not have a corresponding slot in the current
#' [obs-class] and are dropped:
#'
#' - Reference point bias CVs: `FMSY_Mbiascv`, `BMSY_B0biascv`
#'
#' @return An [obs-class] object.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [ConvertImp()] for
#'   related legacy conversion functions.
#'
#' @examples
#' obs_new <- ConvertObs(Generic_Obs)
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
  
  
  obs@LifeHistory <- LifeHistoryObs(
    Length           = .LegacyBias(Linf = Obs@Linfbiascv, K = Obs@Kbiascv, t0 = Obs@t0biascv),
    NaturalMortality = .LegacyBias(M = Obs@Mbiascv),
    Maturity         = .LegacyBias(L50 = Obs@LenMbiascv),
    SRR              = .LegacyBias(h = Obs@hbiascv, SD = Obs@sigmaRbiascv),
    Depletion        = .LegacyBias(Final = Obs@Dbiascv)
  )

  obs@Exploitation <- ExploitationObs(
    Selectivity = .LegacyBias(L5 = Obs@LFCbiascv, LFS = Obs@LFSbiascv)
  )

  obs
}

# `Bias` specification from legacy bias CVs; CVs that are missing or not positive are dropped
.LegacyBias <- function(...) {
  Bias <- list(...)
  Bias <- Bias[vapply(Bias, \(x) length(x) == 1 && is.finite(x) && x > 0, logical(1))]
  if (length(Bias)) list(Bias = lapply(Bias, as.numeric)) else list()
}
