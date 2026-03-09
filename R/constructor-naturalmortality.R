#' NaturalMortality
#'
#' Construct a [naturalmortality-class()] object defining natural mortality
#' assumptions for a [Stock()].
#'
#' @param Pars Named list of natural mortality parameters for models in [NaturalMortalityModels()]
#' @param Model Model associated with `Pars`.
#' @param Units Time units (e.g. `"year"`).
#' @param MeanAtAge Mean natural mortality-at-age array (optional).
#' @param MeanAtLength Mean natural mortality-at-length array (optional).
#' @param Random Random effects array (optional).
#' @param Classes Age or length class mid-points (optional).
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `NaturalMortality` class defines how natural mortality varies
#' with age and/or length in a [Stock()]. Mortality schedules may be
#' model-based (via `Pars` and `Model`) or supplied directly as arrays.
#'
#' A `NaturalMortality` object can be attached to a [Stock()] using `NaturalMortality(Stock) <- MyNaturalMortality` and
#' retrieved using `MyNaturalMortality <- NaturalMortality(Stock)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [Units()].
#' 
#' `r TechManLink()`
#' 
#' @return A [naturalmortality-class] object.
#'
#' @seealso
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
  
  if (inherits(Pars, 'stock'))
    return(Pars@NaturalMortality)
  
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


#' @rdname NaturalMortality
#' @export
`NaturalMortality<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  AssignSlot(x, value, 'NaturalMortality')
}
