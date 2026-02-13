#' Weight
#'
#' Construct a [weight-class] object defining weight-at-age and
#' weight-at-length schedules associated with a [Stock()].
#'
#' @param Pars Named list of parameters for [WeightModels()].
#' @param Model Model associated with `Pars`.
#' @param Units Weight units (e.g. `"g"`, `"kg"`).
#' @param MeanAtAge Mean weight-at-age array (optional).
#' @param MeanAtLength Mean weight-at-length array (optional).
#' @param CVatAge Coefficient of variation at age (optional).
#' @param Dist Distribution name (e.g. `"lognormal"`).
#' @param TruncSD Truncation in SD units.
#' @param Timing Timing within the time step.
#' @param Random Random effects array (optional).
#' @param AWK Age–weight key (optional).
#' @param Classes Weight classes (optional).
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `Weight` class defines how weight varies with age and/or length
#' in a [Stock()]. Weight schedules may be model-based (via `Pars`
#' and `Model`) or supplied directly as arrays.
#'
#'
#' A `Weight` object can be attached to a [Stock()] using `Weight(Stock) <- MyWeight` and
#' retrieved using `MyWeight <- Weight(Stock)`
#' 
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [Units()].
#' 
#' `r TechManLink()`
#' 
#' @return A [weight-class] object.
#'
#' @seealso [Populate()], [WeightModels()]
#'
#' @export
Weight <- function(Pars = list(),
                   Model = NULL,
                   Units = "g",
                   MeanAtAge = NULL,
                   MeanAtLength = NULL,
                   CVatAge = NULL,
                   Dist = "lognormal",
                   TruncSD = 2,
                   Timing = 0,
                   Random = NULL,
                   AWK = NULL,
                   Classes = NULL,
                   Misc = list()) {
  
  if (inherits(Pars, 'stock'))
    return(Pars@Weight)
  
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
    AWK           = AWK,
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


#' @rdname Weight
#' @export
AWK <- function(x) {
  CheckClass(x, "weight", "x")
  x@AWK
}

#' @rdname Weight
#' @export
`AWK<-` <- function(x, value) {
  CheckClass(x, "weight", "x")
  x@AWK <- value
  methods::validObject(x)
  x
}

`Weight<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  AssignSlot(x, value, 'Weight')
}



