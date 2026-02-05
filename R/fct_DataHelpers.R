
#' Data Helper Functions
#' 
#' @name DataHelpers
NULL

#' @describeIn DataHelpers Get the last TAC 
#' @export
LastTAC <- function(Data) {
  CheckClass(Data, 'data', 'Data')
  LastTAC <- tail(Data@TAC[!is.na(Data@TAC)],1) |> as.numeric()
  if (length(LastTAC)<1)
    LastTAC <- sum(tail(Data@Landings@Value,1) + tail(Data@Discards@Value,1))
  LastTAC
}


#' @describeIn DataHelpers Get the index for the last historical time step
#' @export
GetYearLH <- function(Data) {
  CheckClass(Data, 'data', 'Data')
  which.max(Data@Years[Data@Years<Data@YearLH+1])
}

#' @describeIn DataHelpers Get the index for current time step
#' @export
ProjectionYear <- function(Data) {
  length(Data@Years[Data@Years>Data@YearLH])+1
}


