#' IndicesData Constructor
#'
#' Construct an [indicesdata-class] object storing an abundance or biomass
#' index and associated uncertainty.
#'
#' @param Name `character` or `NULL`. Name of the index. Default `NULL`.
#' @param Value `array` or `NULL`. Observed index values with dimensions
#'   `[nYear x nIndex]`. Default `NULL`.
#' @param CV `array` or `NULL`. Coefficients of variation for the index
#'   observations, matching the dimensions of `Value`. Default `NULL`.
#' @param Units `character` or `NULL`. Units of the index (e.g., `"kg/trip"`,
#'   `"numbers/tow"`). Default `NULL`.
#' @param Ref `numeric` or `NULL`. Reference value for each index (e.g., a
#'   historical mean or target level), length `nIndex`. Default `NULL`.
#' @param RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. Default `NULL`.
#' @param Timing `numeric`. Within-year timing of each observation as a
#'   fraction of the year (0-1), length `nIndex`. Default `0`.
#' @param Selectivity An array or character specification mapping each index
#'   to a fleet selectivity or defining an independent selectivity curve, or
#'   `NULL`. Default `NULL`.
#' @param Misc `list`. Named list for additional metadata. Default `list()`.
#'
#' @details
#' ## Usage
#'
#' All arguments are optional; an empty object is valid and slots are
#' populated later (e.g., by [GenHistData_Indices()]).
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' CPUE(data)   <- IndicesData(Value = myArray, CV = myCVArray)
#' Survey(data) <- IndicesData(Value = myArray, CV = myCVArray)
#' ```
#'
#' @return An [indicesdata-class] object.
#'
#' @seealso
#'
#' - [indicesdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [IndicesObs()] for the observation error structure that generates
#'   [indicesdata-class] objects during simulation.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' id <- IndicesData()
#'
#' # With values
#' yrs <- 2000:2020
#' val <- array(runif(length(yrs)),
#'              dim = c(length(yrs), 1),
#'              dimnames = list(Year = yrs, Index = "Survey1"))
#' id <- IndicesData(Name  = "Survey1",
#'                   Value = val,
#'                   CV    = array(0.2, dim = dim(val), dimnames = dimnames(val)),
#'                   Units = "kg/tow")
#'
#' @export
IndicesData <- function(Name        = NULL,
                        Value       = NULL,
                        CV          = NULL,
                        Units       = NULL,
                        Ref         = NULL,
                        RefCV       = NULL,
                        Timing      = 0,
                        Selectivity = NULL,
                        Misc        = list()) {
  .Object <- methods::new("indicesdata")
  if (!is.null(Name))        .Object@Name        <- Name
  if (!is.null(Value))       .Object@Value       <- Value
  if (!is.null(CV))          .Object@CV          <- CV
  if (!is.null(Units))       .Object@Units       <- Units
  if (!is.null(Ref))         .Object@Ref         <- Ref
  if (!is.null(RefCV))       .Object@RefCV       <- RefCV
  if (!is.null(Timing))      .Object@Timing      <- Timing
  if (!is.null(Selectivity)) .Object@Selectivity <- Selectivity
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}
