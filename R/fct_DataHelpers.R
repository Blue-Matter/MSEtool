
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
  which(Data@Years == Data@YearLH)
}

#' @describeIn DataHelpers Get the index for current time step
#' @export
ProjectionYear <- function(Data) {
  length(Data@Years[Data@Years>Data@YearLH])+1
}

#' @describeIn DataHelpers Trim a Data object to a specific Time Step
#' @export
DataTrim <- function(Data, Year) {
  CheckClass(Data, 'data', 'Data')
  saveTAC <- Data@TAC

  if (!is.numeric(Year))
    cli::cli_abort("`Year` must be a numeric value")
  if (!length(Year)==1)
    cli::cli_abort("`Year` must be a numeric value length 1")
  
  Years <- Data@Years
  if (!Year %in% Years)
    cli::cli_abort("{.var Year} {.val {Year}} is not in `Years(Data)`: {.val {Years(Data)}}")
  
  if (Year == max(Data@Years))
    return(Data)
  
  
  OutYears <- Years[Years <= Year]
  
  OutData <- SubsetYear(Data, OutYears, AddPast=FALSE)
  # OutData@Years <- OutYears
  OutData@TAC <- saveTAC
  OutData
}

