#' Weight
#'
#' Construct a [Weight()] object defining weight-at-age and
#' weight-at-length schedules associated with a [Stock()].
#'
#' @param Pars Named list of parameters for [WeightModels()].
#' @param Model Growth model identifier associated with `Pars`.
#' @param Units Weight units (e.g. `"g"`, `"kg"`).
#' @param MeanAtAge Mean weight-at-age array (optional).
#' @param MeanAtLength Mean weight-at-length array (optional).
#' @param CVatAge Coefficient of variation at age (optional).
#' @param Dist Distribution name (e.g. `"lognormal"`).
#' @param TruncSD Truncation in SD units.
#' @param Timing Timing within the time step.
#' @param Random Random effects array (optional).
#' @param ASK Age–weight key (optional).
#' @param Classes Weight classes (optional).
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `Weight` class defines how weight varies with age and/or length
#' in a [Stock()]. Weight schedules may be model-based (via `Pars`
#' and `Model`) or supplied directly as arrays.
#'
#' Printing a `Weight` object provides a concise summary of the specified
#' weight assumptions without printing full arrays.
#'
#' A `Weight` object can be attached to a [Stock()] using [SetWeight()] and
#' retrieved using [GetWeight()].
#' 
#' @return A [Weight()] object.
#'
#' @seealso
#' [GetWeight()], [SetWeight()],
#' [MeanAtAge()], [MeanAtLength()], [CVatAge()],
#' [Populate()]
#'
#' @export
Weight <- function(Pars,
                   Model = NULL,
                   Units = "g",
                   MeanAtAge = NULL,
                   MeanAtLength = NULL,
                   CVatAge = NULL,
                   Dist = "lognormal",
                   TruncSD = 2,
                   Timing = 0,
                   Random = NULL,
                   ASK = NULL,
                   Classes = NULL,
                   Misc = list()) {
  
  if (missing(Pars)) {
    object <- methods::new("weight")
    methods::validObject(object)
    return(object)
  }
  
  CheckClass(Pars, "list", "Pars")
  
  object <- methods::new(
    "weight",
    Pars          = Pars,
    Model         = Model,
    Units         = Units,
    MeanAtAge     = MeanAtAge,
    MeanAtLength  = MeanAtLength,
    CVatAge       = CVatAge,
    Dist          = Dist,
    TruncSD       = TruncSD,
    Timing        = Timing,
    Random        = Random,
    ASK           = ASK,
    Classes       = Classes,
    Misc          = Misc
  )
  
  if (length(Pars)>0 &
      !is.null(names(Pars)) &
      all(!is.na(unlist(Pars))) &
      is.null(Model))
    object@Model <- FindModel(object)
  
  methods::validObject(object)
  object
}

# -------------------------------------------------------------------------
# Accessors and assignment functions
# -------------------------------------------------------------------------

#' Weight accessors and assignment functions
#'
#' Functions for accessing and modifying a [Weight()] object, and for
#' attaching or retrieving a `Weight` object from a [Stock()].
#'
#' @param Stock A [Stock()] object.
#' @param x A [Weight()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetWeight()` and `SetWeight()` retrieve or assign the `Weight`
#'   component of a [`Stock`] object.
#' - Slot accessors (e.g. `MeanAtAge()`, `CVatAge()`) extract individual
#'   components of a [Weight()] object.
#' - Replacement functions (e.g. `MeanAtAge<-`) update the corresponding
#'   component and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Weight()].
#'
#' @seealso [Weight()], [Populate()]
#'
#' @name Weight-accessors
NULL

# ---- Stock-level ---------------------------------------------------------

#' @rdname Weight-accessors
#' @export
GetWeight <- function(Stock) {
  CheckClass(Stock, "stock", "Stock")
  Stock@Weight
}

#' @rdname Weight-accessors
#' @export
SetWeight <- function(Stock, Weight) {
  CheckClass(Stock, "stock", "Stock")
  CheckClass(Weight, "weight", "Weight")
  Stock@Weight <- Weight
  methods::validObject(Stock)
  Stock
}

# ---- Slot accessors ------------------------------------------------------

#' @rdname Weight-accessors
#' @export
Pars <- function(x) {
  CheckClass(x, "weight", "Weight")
  x@Pars
}

#' @rdname Weight-accessors
#' @export
`Pars<-` <- function(x, value) {
  CheckClass(x, "weight", "Weight")
  x@Pars <- value
  methods::validObject(x)
  x
}

#' @rdname Weight-accessors
#' @export
Model <- function(x) {
  CheckClass(x, "weight", "Weight")
  x@Model
}

#' @rdname Weight-accessors
#' @export
`Model<-` <- function(x, value) {
  CheckClass(x, "weight", "Weight")
  x@Model <- value
  methods::validObject(x)
  x
}

#' @rdname Weight-accessors
#' @export
Units <- function(x) {
  CheckClass(x, "weight", "Weight")
  x@Units
}

#' @rdname Weight-accessors
#' @export
`Units<-` <- function(x, value) {
  CheckClass(x, "weight", "Weight")
  x@Units <- value
  methods::validObject(x)
  x
}

#' @rdname Weight-accessors
#' @export
MeanAtAge <- function(x) {
  CheckClass(x, "weight", "Weight")
  x@MeanAtAge
}

#' @rdname Weight-accessors
#' @export
`MeanAtAge<-` <- function(x, value) {
  CheckClass(x, "weight", "Weight")
  x@MeanAtAge <- value
  methods::validObject(x)
  x
}

#' @rdname Weight-accessors
#' @export
MeanAtLength <- function(x) {
  CheckClass(x, "weight", "Weight")
  x@MeanAtLength
}

#' @rdname Weight-accessors
#' @export
`MeanAtLength<-` <- function(x, value) {
  CheckClass(x, "weight", "Weight")
  x@MeanAtLength <- value
  methods::validObject(x)
  x
}

#' @rdname Weight-accessors
#' @export
CVatAge <- function(x) {
  CheckClass(x, "weight", "Weight")
  x@CVatAge
}

#' @rdname Weight-accessors
#' @export
`CVatAge<-` <- function(x, value) {
  CheckClass(x, "weight", "Weight")
  x@CVatAge <- value
  methods::validObject(x)
  x
}

#' @rdname Weight-accessors
#' @export
Classes <- function(x) {
  CheckClass(x, "weight", "Weight")
  x@Classes
}

#' @rdname Weight-accessors
#' @export
`Classes<-` <- function(x, value) {
  CheckClass(x, "weight", "Weight")
  x@Classes <- value
  methods::validObject(x)
  x
}
