#' Retention
#'
#' Create a `Retention` object.
#'
#' A `Retention` object defines retention-at-age, length, or weight
#' relationships.
#'
#' @param Pars A named list of retention parameters.
#' @param Model Optional retention model identifier. If `NULL`, the model
#'   is inferred from `Pars` where possible.
#' @param MeanAtAge Optional numeric array of mean retention-at-age.
#' @param MeanAtLength Optional numeric array of mean retention-at-length.
#' @param MeanAtWeight Optional numeric array of mean retention-at-weight.
#' @param Classes Optional vector of class values associated with the
#'   retention.
#' @param isRel Logical indicating whether retention parameters are relative to maturity.
#' @param Misc Miscellaneous list.
#'
#' @return A `Retention` object.
#'
#' @seealso [Fleet()]
#'
#' @export
Retention <- function(Pars = list(),
                      Model = NULL,
                      MeanAtAge = NULL,
                      MeanAtLength = NULL,
                      MeanAtWeight = NULL,
                      Classes = NULL,
                      isRel = FALSE,
                      Misc = list()) {
  
  ## Fleet pass-through
  if (methods::is(Pars, "fleet"))
    return(Pars@Retention)
  
  object <- methods::new(
    "retention",
    Pars = Pars,
    Model = Model,
    isRel = isRel,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes = Classes,
    Misc = Misc
  )
  object

}


#' Retention accessors and assignment functions
#'
#' Functions for accessing and modifying a [Retention()] object, and for
#' attaching or retrieving a `Retention` object from a [Fleet()].
#'
#' @param Fleet A [Fleet()] object.
#' @param x A [Retention()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetRetention()` and `SetRetention()` retrieve or assign the
#'   `Retention` component of a [Fleet()] object.
#' - Accessors such as `Pars()` and `Model()` retrieve individual
#'   components of a [Retention()] object.
#' - Replacement functions (e.g. `Pars<-`) update the corresponding component
#'   and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Retention()].
#'
#' @name Retention-accessors
NULL

#' @rdname Retention-accessors
#' @export
GetRetention <- function(Fleet) {
  CheckClass(Fleet, "fleet", "Fleet")
  Fleet@Retention
}

#' @rdname Retention-accessors
#' @export
SetRetention <- function(Fleet, Retention) {
  CheckClass(Fleet, "fleet", "Fleet")
  CheckClass(Retention, "retention", "Retention")
  Fleet@Retention <- Retention
  methods::validObject(Fleet)
  Fleet
}


