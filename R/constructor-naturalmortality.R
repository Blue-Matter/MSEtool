#' NaturalMortality
#'
#' Construct a [NaturalMortality()] object defining natural mortality
#' assumptions for a [Stock()].
#'
#' @param Pars Named list of natural mortality parameters.
#' @param Model Model identifier associated with `Pars`.
#' @param Units Time units (e.g. `"year"`).
#' @param MeanAtAge Mean natural mortality-at-age array (optional).
#' @param MeanAtLength Mean natural mortality-at-length array (optional).
#' @param Random Random effects array (optional).
#' @param Classes Age or length class boundaries (optional).
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `NaturalMortality` class defines how natural mortality varies
#' with age and/or length in a [Stock()]. Mortality schedules may be
#' model-based (via `Pars` and `Model`) or supplied directly as arrays.
#'
#' @return A [NaturalMortality()] object.
#'
#' @seealso
#' [GetNaturalMortality()], [SetNaturalMortality()],
#' [Populate()], [NaturalMortalityModels()]
#'
#' @export
NaturalMortality <- function(Pars = list(),
                             Model = NULL,
                             Units = "year",
                             MeanAtAge = NULL,
                             MeanAtLength = NULL,
                             Random = NULL,
                             Classes = NULL,
                             Misc = list()) {
  
  object <- methods::new(
    "naturalmortality",
    Pars          = Pars,
    Model         = Model,
    Units         = Units,
    MeanAtAge     = MeanAtAge,
    MeanAtLength  = MeanAtLength,
    Random        = Random,
    Classes       = Classes,
    Misc          = Misc
  )
  
  if (length(Pars) > 0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars))) &&
      is.null(Model)) {
    object@Model <- FindModel(object)
  }
  
  methods::validObject(object)
  object
}

# -------------------------------------------------------------------------
# Accessors and assignment functions
# -------------------------------------------------------------------------

#' NaturalMortality accessors and assignment functions
#'
#' Functions for accessing and modifying a [NaturalMortality()] object,
#' and for attaching or retrieving a `NaturalMortality` object from a
#' [Stock()].
#'
#' @param Stock A [Stock()] object.
#' @param x A [NaturalMortality()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetNaturalMortality()` and `SetNaturalMortality()` retrieve or assign
#'   the `NaturalMortality` component of a [`Stock`] object.
#' - Slot accessors extract individual components of a
#'   [NaturalMortality()] object.
#' - Replacement functions update the corresponding component and
#'   validate the object.
#'
#' Conceptual details and valid inputs are documented in
#' [NaturalMortality()].
#'
#' @seealso [NaturalMortality()], [Populate()]
#'
#' @name NaturalMortality-accessors
NULL

# ---- Stock-level ---------------------------------------------------------

#' @rdname NaturalMortality-accessors
#' @export
GetNaturalMortality <- function(Stock) {
  CheckClass(Stock, "stock", "Stock")
  Stock@NaturalMortality
}

#' @rdname NaturalMortality-accessors
#' @export
SetNaturalMortality <- function(Stock, NaturalMortality) {
  CheckClass(Stock, "stock", "Stock")
  CheckClass(NaturalMortality, "naturalmortality", "NaturalMortality")
  Stock@NaturalMortality <- NaturalMortality
  methods::validObject(Stock)
  Stock
}

# ---- Slot accessors ------------------------------------------------------

#' @rdname NaturalMortality-accessors
#' @export
Pars <- function(x) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@Pars
}

#' @rdname NaturalMortality-accessors
#' @export
`Pars<-` <- function(x, value) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@Pars <- value
  methods::validObject(x)
  x
}

#' @rdname NaturalMortality-accessors
#' @export
Model <- function(x) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@Model
}

#' @rdname NaturalMortality-accessors
#' @export
`Model<-` <- function(x, value) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@Model <- value
  methods::validObject(x)
  x
}

#' @rdname NaturalMortality-accessors
#' @export
Units <- function(x) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@Units
}

#' @rdname NaturalMortality-accessors
#' @export
`Units<-` <- function(x, value) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@Units <- value
  methods::validObject(x)
  x
}

#' @rdname NaturalMortality-accessors
#' @export
MeanAtAge <- function(x) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@MeanAtAge
}

#' @rdname NaturalMortality-accessors
#' @export
`MeanAtAge<-` <- function(x, value) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@MeanAtAge <- value
  methods::validObject(x)
  x
}

#' @rdname NaturalMortality-accessors
#' @export
MeanAtLength <- function(x) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@MeanAtLength
}

#' @rdname NaturalMortality-accessors
#' @export
`MeanAtLength<-` <- function(x, value) {
  CheckClass(x, "naturalmortality", "NaturalMortality")
  x@MeanAtLength <- value
  methods::validObject(x)
  x
}


#' @rdname NaturalMortality
#' @export
NMortality <- function(...) {
  NaturalMortality(...)
}

#' @rdname NaturalMortality
#' @export
`NMortality<-` <- function(x, value) {
  SetNaturalMortality(x, value)
}
