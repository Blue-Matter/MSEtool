#' Length
#'
#' Construct a [Length()] object defining the length-at-age structure
#' associated with a [Stock()].
#'
#' @param Pars Named list of growth parameters.
#' @param Model Growth model identifier.
#' @param Units Character string giving length units (e.g. `"mm"`).
#' @param MeanAtAge Mean length-at-age array (optional).
#' @param CVatAge Coefficient of variation at age.
#' @param Dist Character distribution name (e.g. `"normal"`).
#' @param TruncSD Truncation in standard deviation units.
#' @param Timing Timing within the time step.
#' @param Random Random effects array (optional)
#' @param ALK Age–length key (optional)
#' @param Classes Length class boundaries (optional)
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `Length` class defines the relationship between age and length for a
#' [Stock()] object. Length schedules may be model-based (via `Pars`
#' and `Model`) or supplied directly as arrays.
#' 
#' Printing a `Length` object provides a concise summary of the specified
#' weight assumptions without printing full arrays.
#'
#' A `Length` object can be attached to a [Stock()] using [SetLength()] and
#' retrieved using [GetLength()].
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [Units()].
#'
#' @return A [Length()] object.
#'
#' @seealso
#' [GetLength()], [SetLength()],
#' [Pars()], [Model()], [Units()], [MeanAtAge()], [CVatAge()],
#' [Dist()], [TruncSD()], [Timing()], [Random()], [ALK()],
#' [Classes()], [Populate()], [LengthModels()]
#'
#' @example man-examples/class-Length.R
#'
#' @export
Length <- function(Pars = list(),
                   Model = NULL,
                   Units = "mm",
                   MeanAtAge = NULL,
                   CVatAge = 0.1,
                   Dist = "normal",
                   TruncSD = 2,
                   Timing = 0,
                   Random = NULL,
                   ALK = NULL,
                   Classes = NULL,
                   Misc = list()) {
  
  object <- methods::new("length",
                         Pars = Pars,
                         Model = Model,
                         Units = Units,
                         MeanAtAge = MeanAtAge,
                         CVatAge = CVatAge,
                         Dist = Dist,
                         TruncSD = TruncSD,
                         Timing = Timing,
                         Random = Random,
                         ALK  = ALK,
                         Classes = Classes,
                         Misc = Misc)
  
  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    object@Model <- FindModel(object)
  
  methods::validObject(object)
  object
}


#' Length accessors and assignment functions
#'
#' Functions for accessing and modifying a [Length()] object, and for
#' attaching or retrieving a `Length` object from a [Stock()].
#'
#' @param Stock A [Stock()] object.
#' @param x A [Length()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetLength()` and `SetLength()` retrieve or assign the `Length` component
#'   of a [Stock()] object.
#' -  Accessors such as `Pars()`, `Model()`, and `Units()` retrieve
#'   individual component of a [Length()] object.
#' - Replacement functions (e.g. `Pars<-`) update the corresponding component
#'   and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Length()].
#'
#' @name Length-accessors
NULL


# ---- Stock attachment ----

#' @rdname Length-accessors
#' @export
GetLength <- function(Stock) {
  CheckClass(Stock, "stock", "Stock")
  Stock@Length
}

#' @rdname Length-accessors
#' @export
SetLength <- function(Stock, Length) {
  CheckClass(Stock, "stock", "Stock")
  CheckClass(Length, "length", "Length")
  Stock@Length <- Length
  methods::validObject(Stock)
  Stock
}


# ---- Slot accessors ----

#' @rdname Length-accessors
#' @export
Pars <- function(x) {
  CheckClass(x, "length", "x")
  x@Pars
}

#' @rdname Length-accessors
#' @export
`Pars<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@Pars <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
Model <- function(x) {
  CheckClass(x, "length", "x")
  x@Model
}

#' @rdname Length-accessors
#' @export
`Model<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@Model <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
Units <- function(x) {
  CheckClass(x, "length", "x")
  x@Units
}

#' @rdname Length-accessors
#' @export
`Units<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@Units <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
MeanAtAge <- function(x) {
  CheckClass(x, "length", "x")
  x@MeanAtAge
}

#' @rdname Length-accessors
#' @export
`MeanAtAge<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@MeanAtAge <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
CVatAge <- function(x) {
  CheckClass(x, "length", "x")
  x@CVatAge
}

#' @rdname Length-accessors
#' @export
`CVatAge<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@CVatAge <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
Dist <- function(x) {
  CheckClass(x, "length", "x")
  x@Dist
}

#' @rdname Length-accessors
#' @export
`Dist<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@Dist <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
TruncSD <- function(x) {
  CheckClass(x, "length", "x")
  x@TruncSD
}

#' @rdname Length-accessors
#' @export
`TruncSD<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@TruncSD <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
Timing <- function(x) {
  CheckClass(x, "length", "x")
  x@Timing
}

#' @rdname Length-accessors
#' @export
`Timing<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@Timing <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
Random <- function(x) {
  CheckClass(x, "length", "x")
  x@Random
}

#' @rdname Length-accessors
#' @export
`Random<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@Random <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
ALK <- function(x) {
  CheckClass(x, "length", "x")
  x@ALK
}

#' @rdname Length-accessors
#' @export
`ALK<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@ALK <- value
  methods::validObject(x)
  x
}

#' @rdname Length-accessors
#' @export
Classes <- function(x) {
  CheckClass(x, "length", "x")
  x@Classes
}

#' @rdname Length-accessors
#' @export
`Classes<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@Classes <- value
  methods::validObject(x)
  x
}
