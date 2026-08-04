#' Data Helper Functions
#'
#' A collection of helper functions for extracting key values from a `Data`
#' object, including TAC advice, and year indices for historical and projection
#' time steps.
#'
#' @param Data An object of class [data].
#' 
#' @seealso [DataTrim()]
#'
#' @name DataHelpers
NULL

#' @describeIn DataHelpers Get the last TAC (Total Allowable Catch) from the
#'   advice slot. If no TAC has been set, returns the sum of the most recent
#'   landings and discards.
#'
#' @return
#' - `LastTAC()`: A numeric scalar giving the last TAC, or the sum of the
#'   most recent landings and discards if no TAC is available.
#'
#' @export
LastTAC <- function(Data) {
  .CheckClass(Data, 'data', 'Data')
  LastTAC <- utils::tail(Data@Advice@TAC[!is.na(Data@Advice@TAC)], 1) |> as.numeric()
  if (length(LastTAC) < 1)
    LastTAC <- sum(utils::tail(Data@Landings@Value, 1) + utils::tail(Data@Discards@Value, 1))
  LastTAC
}

#' @describeIn DataHelpers Get the index of the last historical year within
#'   `Data@@Years`. This is the position of the final year that does not exceed
#'   `Data@@YearLH`, and is typically used to subset or index time series
#'   vectors aligned to `Data@@Years`.
#'
#' @return
#' - `LastHistYearInd()`: An integer giving the index of the last historical
#'   year in `Data@@Years`.
#'
#' @export
LastHistYearInd <- function(Data) {
  .CheckClass(Data, 'data', 'Data')
  which.max(Data@Years[Data@Years < Data@YearLH + 1])
}

#' @describeIn DataHelpers Get the index of the current projection time step
#'   relative to `Data@@YearLH`. Returns `1` in the first projection year,
#'   incrementing by one each subsequent timestep
#'
#' @return
#' - `ProjectionYear()`: A positive integer giving the current projection year
#'   index.
#'
#' @export
ProjectionYear <- function(Data) {
  length(Data@Years[Data@Years >= Data@YearLH + 1])
}

#  

#' @describeIn DataHelpers Get the season index for the season this call to an MP is providing advice for.
#'
#' @return
#' - `MPSeasonIndex()`: A list elements `Seasons` the number of seasons and 
#'   `SeasonInd` the season index  
#'   
#' @export
MPSeasonIndex <- function(Data) {
  Seasons <- Data@Seasons
  if (is.null(Seasons) || !length(Seasons))
    Seasons <- 1
  list(Seasons = Seasons,
       SeasonInd = ((length(Data@Years) - 1) %% Seasons) + 1)
}
