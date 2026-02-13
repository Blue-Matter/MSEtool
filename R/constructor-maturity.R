#' Maturity
#'
#' Construct a [maturity-class] object defining the maturity schedule
#' associated with a [Stock()].
#'
#' @param Pars Named list of maturity parameters.
#' @param Model Maturity model name or function
#' @param MeanAtAge Mean maturity-at-age array (optional).
#' @param MeanAtLength Mean maturity-at-length array (optional).
#' @param MeanAtWeight Mean maturity-at-weight array (optional).
#' @param Classes Maturity class mid-points (optional).
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
#' A `Maturity` object can be attached to a [Stock()] using `Maturity(Stock) <- MyMaturity` and
#' retrieved using `MyMaturity <- Maturity(Stock)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [MeanAtAge()].
#' 
#' `r TechManLink()`
#'
#' @return A [maturity-class] object.
#' 
#' @seealso [Populate()],[MaturityModels()]
#' 
#' @export
Maturity <- function(Pars = list(),
                     Model = NULL,
                     MeanAtAge = NULL,
                     MeanAtLength = NULL,
                     MeanAtWeight = NULL,
                     Classes = NULL,
                     Semelparous = FALSE,
                     Misc = list()) {
  
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



#' @rdname Maturity
#' @export
Semelparous <- function(x) {
  CheckClass(x, "maturity", "x")
  x@Semelparous
}

#' @rdname Maturity
#' @export
`Semelparous<-` <- function(x, value) {
  CheckClass(x, "maturity", "x")
  x@Semelparous <- value
  methods::validObject(x)
  x
}

#' @rdname Maturity
#' @export
`Maturity<-`<- function(x, value) {
  CheckClass(x, "stock", "x")
  x@Maturity <- value
  methods::validObject(x)
  x
}
