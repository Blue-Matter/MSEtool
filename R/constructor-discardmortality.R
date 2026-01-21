#' Create a DiscardMortality object
#'
#' Constructs a [`DiscardMortality`] object describing discard mortality
#' at age or length.
#'
#' If a [`Fleet`] object is supplied, the `DiscardMortality` slot is returned.
#'
#' @param MeanAtAge A numeric array of discard mortality at age
#' @param MeanAtLength A numeric array of discard mortality at length
#' @param Classes Optional class vector (ages or lengths)
#' @param Misc A list of miscellaneous parameters
#'
#' @return A `DiscardMortality` object
#'
#' @export
DiscardMortality <- function(MeanAtAge    = NULL,
                             MeanAtLength = NULL,
                             Classes      = NULL,
                             Misc         = list()) {
  
  if (methods::is(MeanAtAge, "fleet"))
    return(MeanAtAge@DiscardMortality)
  
  methods::new(
    "discardmortality",
    MeanAtAge    = MeanAtAge,
    MeanAtLength = MeanAtLength,
    Classes      = Classes,
    Misc         = Misc
  )
}

#' DiscardMortality accessors and assignment functions
#'
#' Functions for accessing and modifying a [DiscardMortality()] object, and for
#' attaching or retrieving a `DiscardMortality` object from a [Fleet()].
#'
#' @param Fleet A [Fleet()] object.
#' @param x A [DiscardMortality()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetDiscardMortality()` and `SetDiscardMortality()` retrieve or assign the
#'   `DiscardMortality` component of a [Fleet()] object.
#'
#' Conceptual details and valid inputs are documented in [DiscardMortality()].
#'
#' @name DiscardMortality-accessors
NULL

#' @rdname DiscardMortality-accessors
#' @export
GetDiscardMortality <- function(Fleet) {
  CheckClass(Fleet, "fleet", "Fleet")
  Fleet@DiscardMortality
}

#' @rdname DiscardMortality-accessors
#' @export
SetDiscardMortality <- function(Fleet, DiscardMortality) {
  CheckClass(Fleet, "fleet", "Fleet")
  CheckClass(DiscardMortality, "discardmortality", "DiscardMortality")
  Fleet@DiscardMortality <- DiscardMortality
  methods::validObject(Fleet)
  Fleet
}
