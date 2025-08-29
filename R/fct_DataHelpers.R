
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
    LastTAC <- tail(Data@Catch@Value,1) |> sum()
  LastTAC
}


#' @describeIn DataHelpers Get the index for the last historical time step
#' @export
GetTimeStepLH <- function(Data) {
  CheckClass(Data, 'data', 'Data')
  which(Data@TimeSteps == Data@TimeStepLH)
}

#' @describeIn DataHelpers Get the index for current time step
#' @export
ProjectionTimeStep <- function(Data) {
  length(Data@TimeSteps[Data@TimeSteps>Data@TimeStepLH])+1
}

#' @describeIn DataHelpers Trim a Data object to a specific Time Step
#' @export
DataTrim <- function(Data, TimeStep) {
  CheckClass(Data, 'data', 'Data')
  saveTAC <- Data@TAC

  if (!is.numeric(TimeStep))
    cli::cli_abort("`TimeStep` must be a numeric value")
  if (!length(TimeStep)==1)
    cli::cli_abort("`TimeStep` must be a numeric value length 1")
  
  TimeSteps <- Data@TimeSteps
  if (!TimeStep %in% TimeSteps)
    cli::cli_abort("{.var TimeStep} {.val {TimeStep}} is not in `TimeSteps(Data)`: {.val {TimeSteps(Data)}}")
  
  if (TimeStep == max(Data@TimeSteps))
    return(Data)
  
  
  OutTimeSteps <- TimeSteps[TimeSteps <= TimeStep]
  
  OutData <- SubsetTimeStep(Data, OutTimeSteps, AddPast=FALSE)
  # OutData@TimeSteps <- OutTimeSteps
  OutData@TAC <- saveTAC
  OutData
}

