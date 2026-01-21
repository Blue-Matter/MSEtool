#' Selectivity
#'
#' Create a `Selectivity` object.
#'
#' A `Selectivity` object defines selectivity-at-age, length, or weight
#' relationships.
#'
#' @param Pars A named list of selectivity parameters.
#' @param Model Optional selectivity model identifier. If `NULL`, the model
#'   is inferred from `Pars` where possible.
#' @param MeanAtAge Optional numeric array of mean selectivity-at-age.
#' @param MeanAtLength Optional numeric array of mean selectivity-at-length.
#' @param MeanAtWeight Optional numeric array of mean selectivity-at-weight.
#' @param Classes Optional vector of class values associated with the
#'   selectivity.
#' @param isRel Logical indicating whether selectivity parameters are relative to maturity.
#' @param Misc Miscellaneous list
#'
#' @return A `Selectivity` object.
#'
#' @seealso [Fleet()]
#'
#' @export
Selectivity <- function(Pars = list(),
                        Model = NULL,
                        MeanAtAge = NULL,
                        MeanAtLength = NULL,
                        MeanAtWeight = NULL,
                        Classes = NULL,
                        isRel = FALSE,
                        Misc = list()) {
  
  ## Fleet pass-through 
  if (methods::is(Pars, "fleet"))
    return(Pars@Selectivity)
  
  ## Infer model if not supplied
  if (is.null(Model) &&
      length(Pars) > 0 &&
      !is.null(names(Pars)) &&
      all(!is.na(unlist(Pars)))) {
    
    object <- methods::new(
      "selectivity",
      Pars = Pars,
      Model = NA_character_,
      isRel = isRel,
      MeanAtAge = MeanAtAge,
      MeanAtLength = MeanAtLength,
      MeanAtWeight = MeanAtWeight,
      Classes = Classes,
      Misc = Misc
    )
    
    object@Model <- FindModel(object)
    return(object)
  }
  
  methods::new(
    "selectivity",
    Pars = Pars,
    Model = Model,
    isRel = isRel,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes = Classes,
    Misc = Misc
  )
}


#' Selectivity accessors and assignment functions
#'
#' Functions for accessing and modifying a [Selectivity()] object, and for
#' attaching or retrieving a `Selectivity` object from a [Fleet()].
#'
#' @param Fleet A [Fleet()] object.
#' @param x A [Selectivity()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetSelectivity()` and `SetSelectivity()` retrieve or assign the
#'   `Selectivity` component of a [Fleet()] object.
#' - Accessors such as `Pars()` and `Model()` retrieve individual
#'   components of a [Selectivity()] object.
#' - Replacement functions (e.g. `Pars<-`) update the corresponding component
#'   and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Selectivity()].
#'
#' @name Selectivity-accessors
NULL



#' @rdname Selectivity-accessors
#' @export
GetSelectivity <- function(Fleet) {
  CheckClass(Fleet, "fleet", "Fleet")
  Fleet@Selectivity
}

#' @rdname Selectivity-accessors
#' @export
SetSelectivity <- function(Fleet, Selectivity) {
  CheckClass(Fleet, "fleet", "Fleet")
  CheckClass(Selectivity, "selectivity", "Selectivity")
  Fleet@Selectivity <- Selectivity
  methods::validObject(Fleet)
  Fleet
}

#' @rdname Selectivity-accessors
#' @export
Pars <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@Pars
}

#' @rdname Selectivity-accessors
#' @export
`Pars<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@Pars <- value
  methods::validObject(x)
  x
}


#' @rdname Selectivity-accessors
#' @export
Model <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@Model
}

#' @rdname Selectivity-accessors
#' @export
`Model<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@Model <- value
  methods::validObject(x)
  x
}


#' @rdname Selectivity-accessors
#' @export
isRel <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@isRel
}

#' @rdname Selectivity-accessors
#' @export
`isRel<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@isRel <- value
  methods::validObject(x)
  x
}


#' @rdname Selectivity-accessors
#' @export
MeanAtAge <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@MeanAtAge
}

#' @rdname Selectivity-accessors
#' @export
`MeanAtAge<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@MeanAtAge <- value
  methods::validObject(x)
  x
}


#' @rdname Selectivity-accessors
#' @export
MeanAtLength <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@MeanAtLength
}

#' @rdname Selectivity-accessors
#' @export
`MeanAtLength<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@MeanAtLength <- value
  methods::validObject(x)
  x
}


#' @rdname Selectivity-accessors
#' @export
MeanAtWeight <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@MeanAtWeight
}

#' @rdname Selectivity-accessors
#' @export
`MeanAtWeight<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@MeanAtWeight <- value
  methods::validObject(x)
  x
}


#' @rdname Selectivity-accessors
#' @export
Classes <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@Classes
}

#' @rdname Selectivity-accessors
#' @export
`Classes<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@Classes <- value
  methods::validObject(x)
  x
}


#' @rdname Selectivity-accessors
#' @export
Misc <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@Misc
}

#' @rdname Selectivity-accessors
#' @export
`Misc<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@Misc <- value
  methods::validObject(x)
  x
}




