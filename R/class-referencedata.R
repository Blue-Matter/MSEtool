#' `referencedata` Class
#'
#' Stores biological and management reference points by extending
#' [refpointsMSY-class]. Used in the `Reference` slot of a [data-class] object.
#'
#' This class inherits all slots from [refpointsMSY-class] (e.g., MSY, FMSY,
#' BMSY) and appends a `Misc` slot for any additional reference point data not
#' covered by the parent class.
#'
#' @slot Misc A named list for any additional reference point metadata not
#'   covered by the inherited [refpointsMSY-class] slots.
#'
#' `ReferenceData()` creates a new `referencedata` object. 
#'
#' @return `ReferenceData()` returns a `referencedata` object.
#' 
#' @seealso [data-class], [Data()], [refpointsMSY-class]
#' @include class-refpointsMSY.R
#' @name referencedata
#' @export
setClass(
  "referencedata",
  slots = c(
    Misc = "list"
  ),
  contains = "refpointsMSY"
)

#' @rdname referencedata
#' @export
ReferenceData <- function() {
  new('referencedata')
}
