#' Maturity
#'
#' Construct a [Maturity()] object defining the maturity schedule
#' associated with a [Stock()].
#'
#' @param Pars Named list of maturity parameters.
#' @param Model Maturity model name or function
#' @param MeanAtAge Mean maturity-at-age array (optional).
#' @param MeanAtLength Mean maturity-at-length array (optional).
#' @param MeanAtWeight Mean maturity-at-weight array (optional).
#' @param Classes Maturity class boundaries (optional).
#' @param Semelparous Logical or numeric array indicating semelparity.
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `Maturity` class defines the relationship between age, length, or
#' weight and maturity for a [Stock()] object. Maturity schedules may be
#' model-based (via `Pars` and `Model`) or supplied directly as arrays.
#'
#' Printing a `Maturity` object provides a concise summary of the specified
#' maturity assumptions without printing full arrays.
#'
#' A `Maturity` object can be attached to a [Stock()] using [SetMaturity()]
#' and retrieved using [GetMaturity()].
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()] and [Model()].
#'
#' @return A [Maturity()] object.
#'
#' @seealso
#' [GetMaturity()], [SetMaturity()],
#' [Pars()], [Model()], [MeanAtAge()], [MeanAtLength()],
#' [MeanAtWeight()], [Classes()], [Populate()]
#'
#' @example man-examples/class-Maturity.R
#'
#' @export
Maturity <- function(Pars,
                     Model = NULL,
                     MeanAtAge = NULL,
                     MeanAtLength = NULL,
                     MeanAtWeight = NULL,
                     Classes = NULL,
                     Semelparous = FALSE,
                     Misc = list()) {
  
  if (missing(Pars)) {
    object <- methods::new("maturity")
    methods::validObject(object)
    return(object)
  }
  
  CheckClass(Pars, "list", "Pars")
  
  object <- methods::new(
    "maturity",
    Pars = Pars,
    Model = Model,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes = Classes,
    Semelparous = Semelparous,
    Misc = Misc
  )
  
  if (length(Pars) > 0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model)) {
    object@Model <- FindModel(object)
  }
  
  methods::validObject(object)
  object
}


#' Maturity accessors and assignment functions
#'
#' Functions for accessing and modifying a [Maturity()] object, and for
#' attaching or retrieving a `Maturity` object from a [Stock()].
#'
#' @param Stock A [Stock()] object.
#' @param x A [Maturity()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetMaturity()` and `SetMaturity()` retrieve or assign the `Maturity`
#'   component of a [Stock()] object.
#' - Accessors such as `Pars()` and `Model()` retrieve individual components
#'   of a [Maturity()] object.
#' - Replacement functions (e.g. `Pars<-`) update the corresponding component
#'   and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Maturity()].
#'
#' @name Maturity-accessors
NULL


# ---- Stock attachment ----

#' @rdname Maturity-accessors
#' @export
GetMaturity <- function(Stock) {
  CheckClass(Stock, "stock", "Stock")
  Stock@Maturity
}

#' @rdname Maturity-accessors
#' @export
SetMaturity <- function(Stock, Maturity) {
  CheckClass(Stock, "stock", "Stock")
  CheckClass(Maturity, "maturity", "Maturity")
  Stock@Maturity <- Maturity
  methods::validObject(Stock)
  Stock
}


# ---- Slot accessors ----

#' @rdname Maturity-accessors
#' @export
MeanAtAge <- function(x) {
  CheckClass(x, "maturity", "x")
  x@MeanAtAge
}

#' @rdname Maturity-accessors
#' @export
`MeanAtAge<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@MeanAtAge <- value
  methods::validObject(x)
  x
}

#' @rdname Maturity-accessors
#' @export
MeanAtLength <- function(x) {
  CheckClass(x, "maturity", "x")
  x@MeanAtLength
}

#' @rdname Maturity-accessors
#' @export
`MeanAtLength<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@MeanAtLength <- value
  methods::validObject(x)
  x
}

#' @rdname Maturity-accessors
#' @export
MeanAtWeight <- function(x) {
  CheckClass(x, "maturity", "x")
  x@MeanAtWeight
}

#' @rdname Maturity-accessors
#' @export
`MeanAtWeight<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@MeanAtWeight <- value
  methods::validObject(x)
  x
}

#' @rdname Maturity-accessors
#' @export
Classes <- function(x) {
  CheckClass(x, "maturity", "x")
  x@Classes
}

#' @rdname Maturity-accessors
#' @export
`Classes<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@Classes <- value
  methods::validObject(x)
  x
}

#' @rdname Maturity-accessors
#' @export
Semelparous <- function(x) {
  CheckClass(x, "maturity", "x")
  x@Semelparous
}

#' @rdname Maturity-accessors
#' @export
`Semelparous<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@Semelparous <- value
  methods::validObject(x)
  x
}
