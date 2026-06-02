#' CatchData Constructor
#'
#' Construct a [catchdata-class] object storing observed landed or discarded
#' catch and associated uncertainty.
#'
#' @param Name `character` or `NULL`. Fleet names, length `nFleet`.
#'   Default `NULL`.
#' @param Value `array` or `NULL`. Observed catch values with dimensions
#'   `[nYear x nFleet]`. Default `NULL`.
#' @param CV `array` or `NULL`. Coefficients of variation for the catch
#'   observations, matching the dimensions of `Value`. Default `NULL`.
#' @param Units `character` or `NULL`. Units of catch measurement, one element
#'   per fleet (e.g., `"t"` for tonnes, `"numbers"`). Default `NULL`.
#' @param Ref `array` or `NULL`. Reference catch values (e.g., a historical
#'   baseline), matching the dimensions of `Value`. Default `NULL`.
#' @param RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. Default `NULL`.
#'
#' @details
#' ## Usage
#'
#' All arguments are optional; an empty object is valid and slots are
#' populated later (e.g., by [GenHistData_Catch()]).
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' Landings(data) <- CatchData(Value = myArray, CV = myCVArray)
#' Discards(data) <- CatchData(Value = myArray, CV = myCVArray)
#' ```
#'
#' @return A [catchdata-class] object.
#'
#' @seealso
#'
#' - [catchdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [CatchObs()] for the observation error structure that generates
#'   [catchdata-class] objects during simulation.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' cd <- CatchData()
#'
#' # With values
#' yrs <- 2000:2020
#' fleets <- c("Trawl", "Longline")
#' val <- array(runif(length(yrs) * length(fleets)),
#'              dim = c(length(yrs), length(fleets)),
#'              dimnames = list(Year = yrs, Fleet = fleets))
#' cd <- CatchData(Name  = fleets,
#'                 Value = val,
#'                 CV    = array(0.1, dim = dim(val), dimnames = dimnames(val)),
#'                 Units = c("t", "t"))
#'
#' @export
CatchData <- function(Name  = NULL,
                      Value = NULL,
                      CV    = NULL,
                      Units = NULL,
                      Ref   = NULL,
                      RefCV = NULL) {
  .Object <- methods::new("catchdata")
  if (!is.null(Name))  .Object@Name  <- Name
  if (!is.null(Value)) .Object@Value <- Value
  if (!is.null(CV))    .Object@CV    <- CV
  if (!is.null(Units)) .Object@Units <- Units
  if (!is.null(Ref))   .Object@Ref   <- Ref
  if (!is.null(RefCV)) .Object@RefCV <- RefCV
  methods::validObject(.Object)
  .Object
}

