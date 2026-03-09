#' `compdata` Class
#'
#' Stores age or size composition observations of catch or survey samples.
#' Used in the `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, and
#' `DiscardsAtSize` slots of a [data-class] object.
#'
#' @slot Name Optional character string. Name of the dataset.
#' @slot Value Numeric array of composition proportions or counts, with dimensions
#'   `[nFleet, nYear, nClass]`, where `nYear` represents the total number of time steps.
#' @slot Classes Numeric vector of class midpoints (ages in years, or size bin
#'   midpoints in the appropriate length unit).
#' @slot Units Optional character string. Units of the class variable
#'   (e.g., `"years"`, `"cm"`, `"mm"`).
#' @slot Log Internal named list used for diagnostic and audit logging.
#' @slot Misc A named list for any additional composition-level metadata.
#'
#' @seealso [data-class], [Data()]
#' @include class-unions.R
#' 
#' `CompData()` creates a new `compdata` object. 
#' 
#' @return `CompData()` returns a `compdata` object.
#' @name compdata
#' @export
setClass(
  "compdata",
  slots = c(
    Name    = "char.null",
    Value   = "array.null",
    Classes = "num.null",
    Units   = "char.null",
    Log     = "list",
    Misc    = "list"
  )
)

#' @rdname compdata
#' @export
CompData <- function() {
  new('compdata')
}