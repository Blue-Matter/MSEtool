#' EffortData Constructor and Accessors
#'
#' Construct an [effortdata-class] object storing observed fishing effort and
#' uncertainty, or access and replace individual slots.
#'
#' @param Name `character` or `NULL`. Name of the dataset. Default `NULL`.
#' @param Value `array` or `NULL`. Observed effort values with dimensions
#'   `[nYear x nFleet]`. Default `NULL`.
#' @param CV `array` or `NULL`. Coefficients of variation for the effort
#'   observations, matching the dimensions of `Value`. Default `NULL`.
#' @param Units `character` or `NULL`. Units of effort measurement, one
#'   element per fleet (e.g., `"days"`, `"trips"`, `"unitless"`).
#'   Default `NULL`.
#'
#' @details
#' ## Usage
#'
#' `EffortData()` is the standard constructor. All arguments are optional;
#' an empty object is valid and slots are populated later (e.g., by
#' [GenHistData_Effort()]).
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' Effort(data) <- EffortData(Value = myArray, CV = myCVArray)
#' ```
#'
#' @return
#'
#' - `EffortData()` returns an [effortdata-class] object.
#' - `Effort()` returns the `Effort` slot from a [data-class] object `x`.
#' - `Effort<-` returns `x` with the `Effort` slot replaced.
#'
#' @seealso
#'
#' - [effortdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [EffortObs()] for the observation error structure that generates
#'   [effortdata-class] objects during simulation.
#' - [GenHistData_Effort()] for how `Value`, `CV`, and `Units` are populated
#'   from the operating model history.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' ed <- EffortData()
#'
#' # With values
#' yrs <- 2000:2020
#' nFleet <- 2
#' val <- array(runif(length(yrs) * nFleet),
#'              dim = c(length(yrs), nFleet),
#'              dimnames = list(Year = yrs, Fleet = c("Trawl", "Longline")))
#' ed <- EffortData(Value = val,
#'                  CV    = array(0.2, dim = dim(val), dimnames = dimnames(val)),
#'                  Units = c("hours", "hooks"))
#'
#' @name EffortData
#' @export
EffortData <- function(Name  = NULL,
                       Value = NULL,
                       CV    = NULL,
                       Units = NULL) {
  .Object <- methods::new("effortdata")
  if (!is.null(Name))  .Object@Name  <- Name
  if (!is.null(Value)) .Object@Value <- Value
  if (!is.null(CV))    .Object@CV    <- CV
  if (!is.null(Units)) .Object@Units <- Units
  methods::validObject(.Object)
  .Object
}

