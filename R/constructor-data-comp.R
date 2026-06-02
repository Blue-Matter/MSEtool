#' CompData Constructor
#'
#' Construct a [compdata-class] object storing age or size composition
#' observations of catch samples.
#'
#' @param Name `character` or `NULL`. Fleet names, length `nFleet`.
#'   Default `NULL`.
#' @param Value `array` or `NULL`. Composition counts with dimensions
#'   `[nYear x nFleet x nClass]`. Values represent counts scaled to sum to
#'   `SampleSize` within each year and fleet — see [CompObs()] for the
#'   simulation model. Default `NULL`.
#' @param Classes `numeric` or `NULL`. Class midpoints — ages in years for
#'   age compositions, or bin midpoints in the appropriate length unit for
#'   size compositions. Length `nClass`. Default `NULL`.
#' @param Units `character` or `NULL`. Units of the class variable
#'   (e.g., `"years"`, `"cm"`, `"mm"`). Default `NULL`.
#' @param Log `list`. Named list for diagnostic and audit logging.
#'   Default `list()`.
#' @param Misc `list`. Named list for additional metadata. Default `list()`.
#'
#' @include generate-data-hist-sizecomp.R
#' 
#' @details
#' ## Usage
#'
#' All arguments are optional; an empty object is valid and slots are
#' populated later (e.g., by [GenHistData_AgeComp()] or
#' [GenHistData_SizeComp()].
#'
#' ## Value Dimensions and Units
#'
#' `Value` stores counts, compositions are normalised
#' to sum to `SampleSize` within each year-fleet combination. The
#' simulation model that generates `Value` is defined in [CompObs()].
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' LandingsAtAge(data)  <- CompData(Value = myArray, Classes = ages)
#' DiscardsAtAge(data)  <- CompData(Value = myArray, Classes = ages)
#' LandingsAtSize(data) <- CompData(Value = myArray, Classes = bins)
#' DiscardsAtSize(data) <- CompData(Value = myArray, Classes = bins)
#' ```
#'
#' @return A [compdata-class] object.
#'
#' @seealso
#'
#' - [compdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [CompObs()] for the observation error structure that generates
#'   [compdata-class] objects during simulation.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' cd <- CompData()
#'
#' # With values
#' yrs    <- 2000:2020
#' ages   <- 1:10
#' fleets <- c("Trawl", "Longline")
#' val <- array(0L,
#'              dim = c(length(yrs), length(fleets), length(ages)),
#'              dimnames = list(Year = yrs, Fleet = fleets, Age = ages))
#' cd <- CompData(Name    = fleets,
#'                Value   = val,
#'                Classes = ages,
#'                Units   = "years")
#' @name CompData
#' @export
CompData <- function(Name    = NULL,
                     Value   = NULL,
                     Classes = NULL,
                     Units   = NULL,
                     Log     = list(),
                     Misc    = list()) {
  .Object <- methods::new("compdata")
  if (!is.null(Name))    .Object@Name    <- Name
  if (!is.null(Value))   .Object@Value   <- Value
  if (!is.null(Classes)) .Object@Classes <- Classes
  if (!is.null(Units))   .Object@Units   <- Units
  .Object@Log  <- Log
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}

