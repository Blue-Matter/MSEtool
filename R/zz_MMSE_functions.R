#' Utility functions for multiMSE objects
#' 
#' `addMMPs()` adds additional management procedures to a MMSE object by combining
#' multiple MMSE objects that have identical historical OM values, and population/ fleet structures.
#' 
#' @param MMSEobjs A list `MMSE` object
#' @return An object of class \code{MMSE}
#' @author Q. Huynh
#' @seealso [addMPs()]
#' @export
addMMPs <- function(MMSEobjs) {
  # join two or more MMSE objects
  if (!inherits(MMSEobjs, "list")) stop("MMSEobjs must be a list")
  if (length(MMSEobjs) < 2) stop("MMSEobjs list doesn't contain multiple MSE objects")
  if (!all(sapply(MMSEobjs, inherits, "MMSE"))) stop('MMSEobjs must be a list of objects of class `MMSE`', call.=FALSE)
  
  slots_identical <- function(slotname, MMSEobjs, is_logical = FALSE) {
    templist <- lapply(MMSEobjs, slot, slotname)
    is_identical <- vapply(templist[-1], identical, logical(1), templist[[1]]) %>% all()
    if (is_logical) {
      return(is_identical)
    } else {
      return(templist %>% unlist() %>% unique())
    }
  }
  
  if(!slots_identical("nsim", MMSEobjs, TRUE)) stop("nsim slot not identical in all MSE objects.")
  if(!slots_identical("nyears", MMSEobjs, TRUE)) stop("nyears slot not identical in all MSE objects.")
  if(!slots_identical("proyears", MMSEobjs, TRUE)) stop("proyears slot not identical in all MSE objects.")
  
  MMSE <- new(
    "MMSE",
    Name = slots_identical("Name", MMSEobjs),
    nyears = slots_identical("nyears", MMSEobjs),
    proyears = slots_identical("proyears", MMSEobjs),
    nMPs = sapply(MMSEobjs, slot, "nMPs") %>% sum(),
    MPs = lapply(MMSEobjs, slot, "MPs") %>% unlist() %>% list(),
    MPcond = lapply(MMSEobjs, slot, "MPcond") %>% unlist(),
    MPrefs = join_arrays(MMSEobjs, "MPrefs", along = 1),
    nsim = slots_identical("nsim", MMSEobjs),
    nstocks = slots_identical("nstocks", MMSEobjs),
    nfleets = slots_identical("nfleets", MMSEobjs),
    Snames = slots_identical("Snames", MMSEobjs),
    Fnames = matrix(),
    Stocks = MMSEobjs[[1]]@Stocks,
    Fleets = MMSEobjs[[1]]@Fleets,
    Obss = MMSEobjs[[1]]@Obss,
    Imps = MMSEobjs[[1]]@Imps,
    OM = MMSEobjs[[1]]@OM,
    Obs = MMSEobjs[[1]]@Obs,
    SB_SBMSY = join_arrays(MMSEobjs, "SB_SBMSY", along = 3),
    F_FMSY = join_arrays(MMSEobjs, "F_FMSY", along = 4),
    N = join_arrays(MMSEobjs, "N", along = 4),
    B = join_arrays(MMSEobjs, "B", along = 3),
    SSB = join_arrays(MMSEobjs, "SSB", along = 3),
    VB = join_arrays(MMSEobjs, "VB", along = 3),
    FM = join_arrays(MMSEobjs, "FM", along = 4),
    SPR = lapply(MMSEobjs, slot, "SPR") %>% join_list_of_arrays(along = 3),
    Catch = join_arrays(MMSEobjs, "Catch", along = 4),
    Removals = join_arrays(MMSEobjs, "Removals", along = 4),
    Effort = join_arrays(MMSEobjs, "Effort", along = 4),
    TAC = join_arrays(MMSEobjs, "TAC", along = 4),
    TAE = join_arrays(MMSEobjs, "TAE", along = 4),
    BioEco = list(),
    RefPoint = list(),
    multiHist = MMSEobjs[[1]]@multiHist,
    PPD = list(),
    Misc = list()
  )
  
  MMSE@Fnames <- slots_identical("Fnames", MMSEobjs) %>% matrix(MMSE@nfleets, MMSE@nstocks)
  
  MMSE@RefPoint <- local({
    Refout <- list()
    Refout$ByYear <- lapply(MMSEobjs, function(x) x@RefPoint$ByYear) %>% join_list_of_arrays(along = 3)
    Refout$Dynamic_Unfished <- MMSEobjs[[1]]@RefPoint$Dynamic_Unfished
    Refout
  })
  
  MMSE@PPD <- lapply(1:MMSE@nstocks, function(p) {
    lapply(1:MMSE@nfleets, function(f) {
      lapply(MMSEobjs, function(i) i@PPD[[p]][[f]]) %>% unlist()
    })
  })
  
  attr(MMSE, "version") <- packageVersion("MSEtool")
  attr(MMSE, "date") <- date()
  attr(MMSE, "R.version") <- R.version
  
  return(MMSE)
}
