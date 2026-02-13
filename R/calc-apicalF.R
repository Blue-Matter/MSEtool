#' Calculate Fleet-Specific Instantaneous F (Population-Level)
#'
#' Compute fleet-specific apical instantaneous F for multiple stocks, simulations,
#' years, and fleets, assuming population abundance is already summed over ages.
#'
#' @param CatchInteract Array of total encounters (Sim × Stock × Year × Fleet)
#' @param CatchDead Array of total dead catch (Sim × Stock × Year × Fleet)
#' @param CatchRetain Array of total retained catch (Sim × Stock × Year × Fleet)
#' @param Pop Population abundance or biomass, summed over ages (Sim × Stock × Year)
#'
#' @return A list of arrays (Sim × Stock × Year × Fleet):
#' * `FInteract` – instantaneous F from all fish interacting with the gear
#' * `FDead` – instantaneous F from all fish killed
#' * `FRetain` – instantaneous F from all fish retained by fishers
#'
#' @keywords internal
CalcApicalF <- function(CatchInteract,
                        CatchDead,
                        CatchRetain,
                        Pop) {
  
  # Dimensions
  nSim   <- dim(Pop)[1]
  nStock <- dim(Pop)[2]
  nYear  <- dim(Pop)[3]
  nFleet <- dim(CatchInteract)[4]
  
  # Pre-allocate F arrays
  FInteract <- FDead <- FRetain <- array(NA, dim = dim(CatchInteract))
  
  # Compute instantaneous F
  for (f in seq_len(nFleet)) {
    for (s in seq_len(nStock)) {
      for (y in seq_len(nYear)) {
        N <- Pop[, s, y] 
        FInteract[, s, y, f] <- -log(pmax(1 - CatchInteract[, s, y, f] / N, 0))
        FDead[, s, y, f]     <- -log(pmax(1 - CatchDead[, s, y, f] / N, 0))
        FRetain[, s, y, f]   <- -log(pmax(1 - CatchRetain[, s, y, f] / N, 0))
      }
    }
  }
  
  list(
    FInteract = FInteract,
    FDead     = FDead,
    FRetain   = FRetain
  )
}