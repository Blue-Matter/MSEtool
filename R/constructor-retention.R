#' Retention
#'
#' Construct a [retention-class] object for a [Fleet()] object.
#'
#' A `Retention` object defines retention-at-age, length, or weight
#' relationships.
#'
#' @param Pars A named list of retention parameters.
#' @param Model Optional retention model identifier. If `NULL`, the model
#'   is inferred from `Pars` where possible.
#' @param MeanAtAge Optional numeric array of mean retention-at-age.
#' @param MeanAtLength Optional numeric array of mean retention-at-length.
#' @param MeanAtWeight Optional numeric array of mean retention-at-weight.
#' @param Classes Optional vector of class values associated with the
#'   retention.
#' @param isRel Logical indicating whether retention parameters are relative to maturity.
#' @param Misc Miscellaneous list.
#'
#' A `Retention` object can be attached to a [Fleet()] using `Retention(Fleet) <- MyRetention` and
#' retrieved using `MyRetention <- Retention(Fleet)`
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [Pars()], [Model()], and [MeanAtAge()].
#' 
#' `r TechManLink()`
#' 
#' @return A [retention-class] object.
#'
#' @seealso [Fleet()], [RetentionModels()]
#'
#' @export
Retention <- function(Pars = list(),
                      Model = NULL,
                      MeanAtAge = NULL,
                      MeanAtLength = NULL,
                      MeanAtWeight = NULL,
                      Classes = NULL,
                      isRel = FALSE,
                      Misc = list()) {
  
  ## Fleet pass-through
  if (methods::is(Pars, "fleet"))
    return(Pars@Retention)
  
  object <- methods::new(
    "retention",
    Pars = Pars,
    Model = Model,
    isRel = isRel,
    MeanAtAge = MeanAtAge,
    MeanAtLength = MeanAtLength,
    MeanAtWeight = MeanAtWeight,
    Classes = Classes,
    Misc = Misc
  )
  object

}

#' @rdname Retention
#' @export
`Retention<-` <- function(x,value) {
  AssignSlot(x, value, 'Retention')
}




