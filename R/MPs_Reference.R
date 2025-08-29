## Reference MPs ####

#' Reference management procedures
#'
#' Several reference MPs for your operating model to use in the management strategy
#' evaluation. FMSYref (and related) assume perfect information about FMSY (FMSY
#' is taken from the operating model stored at `Data@Misc$ReferencePoints$ByYear$FMSY`),
#' and set an effort limit (TAE) so that F=FMSY (or some fraction of FMSY) in
#' each year the MP is applied. NFref sets annual catch to zero and is used
#'  for looking at variability in stock with no fishing.
#'
#' @templateVar mp FMSYref
#' @template MPtemplate
#'
#' @details
#' Note that you can out-perform \code{FMSYref} easily. The requirement for fixed
#' F is actually quite strict and is by no means the upper limit in terms of
#' yield. Don't panic if your method beats this one for yield, especially for
#' short-lived species of high temporal variability in productivity!
#'
#' @author T. Carruthers, A. Hordyk
#' @describeIn FMSYref A reference FMSY method that fishes at FMSY
#' @examples
#' FMSYref(1, MSEtool::SimulatedData, plot=TRUE)
#' @export
FMSYref <- function(x, Data, reps = 100, plot=FALSE) {
  y <- max(Data@Year) - Data@LHYear+1
  nyears <- length(Data@Misc$FleetPars$Find[x,])
  FMSY <- Data@Misc$ReferencePoints$ByYear$FMSY[x,nyears+y]
  q <- Data@Misc$FleetPars$qs[x]
  qvar <- Data@Misc$FleetPars$qvar[x,y] # future only
  if (length(qvar)<1) qvar <- 1
  qinc <- Data@Misc$FleetPars$qinc[x] # future only
  qcur <- qvar * q*(1+qinc/100)^y # catchability this year

  HistE <- Data@OM$FinF[x] # Last historical fishing effort
  MSYE <- FMSY/qcur # effort for this year's FMSY

  Rec <- new('Rec')
  Rec@Effort <- MSYE/HistE
  Rec
}
class(FMSYref) <- "MP"

#' @describeIn FMSYref A reference FMSY method that fishes at 50% of FMSY
#' @examples
#' FMSYref50(1, MSEtool::SimulatedData, plot=TRUE)
#' @export
FMSYref50 <- function(x, Data, reps = 100, plot=FALSE) {
  y <- max(Data@Year) - Data@LHYear+1
  nyears <- length(Data@Misc$FleetPars$Find[x,])
  FMSY <- Data@Misc$ReferencePoints$ByYear$FMSY[x,nyears+y]
  q <- Data@Misc$FleetPars$qs[x]
  qvar <- Data@Misc$FleetPars$qvar[x,y] # future only
  if (length(qvar)<1) qvar <- 1
  qinc <- Data@Misc$FleetPars$qinc[x] # future only
  qcur <- qvar * q*(1+qinc/100)^y # catchability this year

  HistE <- Data@OM$FinF[x] # Last historical fishing effort
  MSYE <- FMSY/qcur # effort for this year's FMSY

  Rec <- new('Rec')
  Rec@Effort <- MSYE/HistE *0.5
  Rec
}
class(FMSYref50) <- "MP"

#' @describeIn FMSYref A reference FMSY method that fishes at 75% of FMSY
#' @examples
#' FMSYref75(1, MSEtool::SimulatedData, plot=TRUE)
#' @export
FMSYref75 <- function(x, Data, reps = 100, plot=FALSE) {
  y <- max(Data@Year) - Data@LHYear+1
  nyears <- length(Data@Misc$FleetPars$Find[x,])
  FMSY <- Data@Misc$ReferencePoints$ByYear$FMSY[x,nyears+y]
  q <- Data@Misc$FleetPars$qs[x]
  qvar <- Data@Misc$FleetPars$qvar[x,y] # future only
  if (length(qvar)<1) qvar <- 1
  qinc <- Data@Misc$FleetPars$qinc[x] # future only
  qcur <- qvar * q*(1+qinc/100)^y # catchability this year

  HistE <- Data@OM$FinF[x] # Last historical fishing effort
  MSYE <- FMSY/qcur # effort for this year's FMSY

  Rec <- new('Rec')
  Rec@Effort <- MSYE/HistE * 0.75
  Rec
}
class(FMSYref75) <- "MP"


#' @describeIn FMSYref A reference MP that sets annual catch to almost zero (1e-15)
#' @examples
#' NFref(1, MSEtool::SimulatedData, plot=TRUE)
#' @export
NFref <- function(x, Data, reps = 100, plot=FALSE) {
  rec <- new("Rec") # create recommendation object
  rec@TAC <- rep(tiny, reps)
  if (plot) boxplot(rec@TAC, ylab=paste0("TAC (", Data@Units, ")"))
  rec
}
class(NFref) <- "MP"

#' @describeIn FMSYref A reference MP that keeps fishing effort at the level of the last
#' historical year
#' @examples
#' curEref(1, MSEtool::SimulatedData)
#' @export
curEref <- function(x, Data, reps = 100, plot=FALSE) {
  Rec <- new('Rec')
  Rec@Effort <- 1
  Rec
}
class(curEref) <- 'MP'

