#' Populate the `MeanAtWeight` Slot from a Model and Weight Object
#'
#' Generates `object@MeanAtWeight` by calling`GenMeanAtWeight` with
#' `object@Model`, `object@Pars`, and the size classes from `Weight`. Assigns
#' named `Sim × Class × Year` (or `Sim × Class × Year × Area`) dimnames to
#' the result.
#'
#' Returns `object` unchanged if `object@Model` is `NULL`, `object@Pars` is
#' empty, or if the model function expects `Ages` rather than weight-based
#' inputs (indicating this is an age-based rather than size-based object).
#'
#' @param object An S4 object with slots `MeanAtWeight`, `Model`, `Pars`, and
#'   `Classes`.
#' @param Weight A [Weight()] object supplying `Classes` (lower bounds of weight
#'   bins) used as size class labels. The model is evaluated at bin midpoints
#'   derived from these lower bounds.
#' @param Years Numeric vector of year labels used to assign the `Year`
#'   dimension of the output array.
#' @param Ages An [ages-class] object. Not used directly but checked to
#'   determine whether the model is age-based, in which case the function
#'   returns early.
#' @param seed Integer or `NULL`. Random seed for stochastic models. Reserved
#'   for future use.
#' @param silent Logical. If `TRUE`, suppresses messages. Reserved for future
#'   use. Default `FALSE`.
#'
#' @return `object` with `object@MeanAtWeight` and `object@Classes` populated,
#'   or `object` unchanged if the early-exit conditions are met.
#' @keywords internal
PopulateMeanAtWeight <- function(object, Weight=NULL, Years=NULL,
                                 Ages=NULL, seed=NULL, silent=FALSE) {
  
  if (is.null(object@Model) || ParsEmpty(object@Pars))
    return(object)
  
  object@Model <- FindModel(object)
  args <- names(formals(object@Model))
  
  # Age-based model — MeanAtWeight not applicable
  if ('Ages' %in% args)
    return(object)
  
  if ('Weight' %in% args)
    CheckRequiredObject(Weight, 'weight', 'Weight')
  
  object@MeanAtWeight <- GenMeanAtWeight(
    Model  = object@Model,
    Pars   = object@Pars,
    Weight = ClassMidpoints(Weight@Classes)
  )
  object@Classes <- Weight@Classes

  dd     <- dim(object@MeanAtWeight)
  nDims  <- length(dd)
  Years_ <- Years[seq_len(dd[3])]

  dimnames(object@MeanAtWeight) <- if (nDims == 3) {
    list(
      Sim   = seq_len(dd[1]),
      Class = Weight@Classes,
      Year  = Years_
    )
  } else {
    list(
      Sim   = seq_len(dd[1]),
      Class = Weight@Classes,
      Year  = Years_,
      Area  = seq_len(dd[4])
    )
  }
  
  object
}