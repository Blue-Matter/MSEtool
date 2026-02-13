#' Length
#'
#' Construct a [length-class] object defining the length-at-age structure
#' associated with a [Stock()].
#'
#' @param Pars Named list of growth parameters for models in [LengthModels()]
#' @param Model Model associated with `Pars`
#' @param Units Character string giving length units (e.g. `"mm"`).
#' @param MeanAtAge Mean length-at-age array (optional).
#' @param CVatAge Coefficient of variation at age.
#' @param Dist Character distribution name (e.g. `"normal"`).
#' @param TruncSD Truncation in standard deviation units.
#' @param Timing Timing within the time step.
#' @param Random Random effects array (optional)
#' @param ALK Age–length key (optional)
#' @param Classes Length class mid-points (optional)
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `Length` class defines the relationship between age and length for a
#' [Stock()] object. Length schedules may be model-based (via `Pars`
#' and `Model`) or supplied directly as arrays.
#' 
#' A `Length` object can be attached to a [Stock()] using `Length(Stock) <- MyLength` and
#' retrieved using `MyLength <- Length(Stock)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [Units()].
#' 
#' `r TechManLink()`
#'
#' @return A [length-class] object.
#'
#' @seealso [Populate()], [LengthModels()]
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
  
  if (inherits(Pars, 'stock'))
    return(Pars@Length)
  
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



#' @rdname Length
#' @export
ALK <- function(x) {
  CheckClass(x, "length", "x")
  x@ALK
}

#' @rdname Length
#' @export
`ALK<-` <- function(x, value) {
  CheckClass(x, "length", "x")
  x@ALK <- value
  methods::validObject(x)
  x
}

`Length<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  AssignSlot(x, value, 'Length')
}


