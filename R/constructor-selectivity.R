#' Selectivity
#'
#' Construct a [selectivity-class] object for a [Fleet()] object.
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
#' A `Selectivity` object can be attached to a [Fleet()] using `Selectivity(Fleet) <- MySelectivity` and
#' retrieved using `MySelectivity <- Selectivity(Fleet)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [Units()].
#' 
#' `r TechManLink()`
#'
#' @seealso [Fleet()], [SelectivityModels()]
#' 
#' @return An [selectivity-class] object
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
  

  if (methods::is(Pars, "fleet"))
    return(Pars@Selectivity)
  
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



#' @rdname Selectivity
#' @export
isRel <- function(x) {
  CheckClass(x, "selectivity", "x")
  x@isRel
}

#' @rdname Selectivity
#' @export
`isRel<-` <- function(x, value) {
  CheckClass(x, "selectivity", "x")
  x@isRel <- value
  methods::validObject(x)
  x
}

#' @rdname Selectivity
#' @export
`Selectivity<-`<- function(x,value) {
  AssignSlot(x, value, 'Selectivity')
}













