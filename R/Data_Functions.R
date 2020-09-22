#' Make stochastic variables certain for only one rep
#' 
#' As title.
#'
#' @param Data An object of class Data that has been run though TAC()
#' @author T. Carruthers
#' @export 
#' @keywords internal 
OneRep <- function(Data) {
  if (class(Data) != "Data") stop("First argument must be object of class 'Data'", call.=FALSE)
  Data@CV_Cat =  Data@CV_Ind = Data@CV_Rec = matrix(tiny, nrow=1, ncol=1)
  Data@CV_Dt = Data@CV_AvC = Data@CV_Mort = Data@CV_FMSY_M = Data@CV_BMSY_B0 = 
    Data@CV_Cref = Data@CV_Bref = Data@CV_Iref = Data@CV_Dep = Data@CV_Abun = 
    Data@CV_L50 = Data@CV_vbK = Data@CV_vbLinf = Data@CV_vbt0 = Data@CV_LFC = 
    Data@CV_LFS = Data@CV_wla = Data@CV_wlb = Data@CV_steep = tiny
  Data
}


#' Enlarge (replicate) a DLM data object to create an additional dimension for
#' simulation / sensitivity testing
#' 
#' Replicates position 1 data to multiple positions for sensitivity testing etc
#' 
#' 
#' @param Data A data-limited methods data object
#' @param nrep The number of positions to expand the DLM object to
#' @author T. Carruthers
#' @export
replic8 <- function(Data, nrep) {
  
  slotnam <- slotNames(Data)
  slotnam <- slotnam[slotnam != "Ref" & slotnam != "OM" & slotnam != 
                       "MaxAge" & slotnam != "CAL_bins" & slotnam != "Year"]
  
  for (sl in 1:length(slotnam)) {
    slt <- attr(Data, slotnam[sl])
    if (inherits(slt, "matrix")) {
      attr(Data, slotnam[sl]) <- matrix(rep(slt, each = nrep), 
                                        nrow = nrep, ncol = ncol(slt))
    } else if (inherits(slt, "numeric")) {
      attr(Data, slotnam[sl]) <- rep(slt, nrep)
    } else if (inherits(slt, "array")) {
      attr(Data, slotnam[sl]) <- array(rep(slt, each = nrep), 
                                       dim = c(nrep, dim(slt)[2:3]))
    }
  }
  Data
}


#' Apply Management Procedures to an object of class Data
#'
#' @param Data An object of class Data
#' @param MPs Name(s) of the MPs to run
#' @param reps Number of samples
#' @param nsims Optional. Number of simulations. 
#' @param silent Logical. Should messages be suppressed?
#'
#' @return A list with the first element a list of management recommendations,
#' and the second the updated Data object
#' @export
#'
applyMP <- function(Data, MPs = NA, reps = 100, nsims=NA, silent=FALSE) {
  if (class(Data) != "Data") stop("First argument must be object of class 'Data'", call.=FALSE)
  Dataout <- Data
  if (is.na(nsims)) nsims <- nrow(Data@Cat)
  nMPs <- length(MPs)
  
  if (.hasSlot(Data, "nareas")) {
    nareas <- Data@nareas   
  } else {
    nareas <- 2 
  }
  returnList <- list() # a list nMPs long containing MPs recommendations
  recList <- list() # a list containing nsim recommendations from a single MP 
  TACout <- array(NA, dim=c(nMPs, reps, nsims))
  # if (!sfIsRunning() | (nMPs < 8 & nsims < 8)) {
  for (mp in 1:nMPs) {
    temp <- lapply(1:nsims, MPs[mp], Data = Data, reps = reps)  
    slots <- slotNames(temp[[1]])
    for (X in slots) { # sequence along recommendation slots 
      if (X == "Misc") { # convert to a list nsim by nareas
        rec <- lapply(temp, slot, name=X)
      } else {
        rec <- do.call("cbind", lapply(temp, slot, name=X)) # unlist(lapply(temp, slot, name=X))
      }
      if (X == "Spatial") { # convert to a matrix nsim by nareas
        rec <- matrix(rec, nareas, nsims, byrow=FALSE)   
      }
      recList[[X]] <- rec
      for (x in 1:nsims) Dataout@Misc[[x]] <- recList$Misc[[x]]
      recList$Misc <- NULL
    }
    if (length(recList$TAC)>0)  TACout[mp,,] <- recList$TAC 
    returnList[[mp]] <- recList
    if (!silent && any(apply(is.na(recList$TAC), 2, sum) > rep(0.5 * reps, nsims)))
      message("Method ", MPs[mp], " produced greater than 50% NA values")
  }
  
  Dataout@TAC <- TACout
  Dataout@MPs <- MPs
  
  list(returnList, Dataout)
}
