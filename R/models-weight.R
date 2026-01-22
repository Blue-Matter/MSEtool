#' Weight-at-Age and Weight-at-Length Models
#'
#' Allometric weight models used to compute weight at age or at length.
#'
#' @param Ages numeric vector of age classes (for Weight-at-Age)
#' @param a scaling parameter for Weight-at-Age in units of [Weight()].
#' @param b exponent for Weight-at-Age. Typically around 3.
#' @param Length numeric vector of length classes (for Weight-at-Length)
#' @param MeanLength numeric vector of mean length-at-age (for Weight-at-Mean-Length)
#' @param Alpha scaling parameter for Weight-at-Length in units of [Weight()].
#' @param Beta exponent for Weight-at-Length. Typically around 3.
#' @param alpha scaling parameter for Weight-at-Mean-Length in units of [Weight()].
#' @param beta exponent for Weight-at-Mean-Length. Typically around 3.
#' @param full logical; provide a complete table of models (TRUE) or just model names (FALSE)
#' @param print logical; print results (TRUE) or return data frame invisibly (FALSE)
#' 
#' `WeightModels()` prints the list of available weight models.
#'  
#' @section Model Equations:
#' - **Weight-at-Age**: \deqn{W(a) = a \cdot Age^b}
#' - **Weight-at-Length**: \deqn{W(L) = \alpha \cdot L^\beta}
#' - **Weight-at-Mean-Length**: \deqn{W(L_{mean}) = \alpha_{mean} \cdot L_{mean}^{\beta_{mean}}}, returning weight-at-age
#'
#' @details
#' Models are identified internally by matching the names in `Pars`.  
#' `WeightatMeanLength` returns weight-at-age corresponding to the mean length at each age, while `WeightatLength` returns weight-at-length for the supplied length classes.
#'
#' @return
#' Each function returns a numeric vector of expected weights for each age or length.  
#' `WeightModels()` invisibly returns a data frame describing available models.
#'
#' @seealso [Ages()], [Length()], [Stock()], [Weight()]
#' @example man-examples/models-Weight.R
#'
#' @name Weight-Models
#' @rdname Weight-Models
NULL


#' @rdname Weight-Models
#' @export
WeightModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Weight-at-Age-Model',
                            'Weight-at-Length-Model'), full, print)
}

allometric_ <- function(x, scale, exponent) {
  scale * x^exponent
}

#' @name Weight-Models
#' @rdname Weight-Models
#' @export
WeightatAge <- function(Ages, a, b) {
  allometric_(Ages, scale = a, exponent = b)
}
class(WeightatAge) <- 'Weight-at-Age-Model'

#' @name Weight-Models
#' @rdname Weight-Models
#' @export
WeightatLength <- function(Length, Alpha, Beta) {
  allometric_(Length, scale = Alpha, exponent = Beta)
}
class(WeightatLength) <- 'Weight-at-Length-Model'

#' @name Weight-Models
#' @rdname Weight-Models
#' @export
WeightatMeanLength <- function(MeanLength, alpha, beta) {
  allometric_(MeanLength, scale = alpha, exponent = beta)
}
class(WeightatMeanLength) <- 'Weight-at-Age-Model'

