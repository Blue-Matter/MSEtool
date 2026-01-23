#' Selectivity-at-Age, -Length, and -Weight Models
#'
#' Functions for generating selectivity curves for age, length, or weight.
#' Includes standard logistic, knife-edge, and double-normal
#'
#' @param Ages Numeric vector of age classes for age-based models.
#' @param SA50 First age at 50% selectivity for logistic selectivity-at-age.
#' @param SA50_95 Interval between 50% and 95% selectivity for logistic selectivity-at-age.
#' @param Length Numeric vector of length classes for length-based models.
#' @param SL50 First length at 50% selectivity for logistic selectivity-at-length.
#' @param SL50_95 Interval between 50% and 95% selectivity for logistic selectivity-at-length.
#' @param SW50 First weight at 50% selectivity for logistic selectivity-at-weight.
#' @param SW50_95 Interval between 50% and 95% selectivity for logistic selectivity-at-weight.
#' @param L5 First length at 5% selection (double-normal).
#' @param LFS Full-selection length (double-normal).
#' @param Vmaxlen Selection value at `max(Length)` (double-normal).
#' @param W5 First weight at 5% selection (double-normal).
#' @param WFS Full-selection weight (double-normal).
#' @param Vmaxweight Selection value at `max(Weight)` (double-normal).
#' @param SL Knife-edge length threshold.
#' @param SA Knife-edge age threshold.
#' @param full Logical; provide a complete table of models (TRUE) or just model names (FALSE).
#' @param print Logical; print results (TRUE) or return data frame invisibly (FALSE).
#'
#' at-length and at-weight schedules are converted internally to at-age using the age-length 
#' age-weight key respectively.
#' 
#' - **Logistic**: \deqn{S(x) = \frac{1}{1 + \exp(-\ln(19) \frac{x - x_{50}}{x_{95} - x_{50}})}}
#' - **Knife-edge**: \deqn{S(x) = 0 \text{ if } x < x_{50}, 1 \text{ if } x \ge x_{50}}
#' - **Double-normal**: combination of ascending and descending half-normal curves.
#'
#' * `SelectivityModels()` prints a list of available selectivity models.
#' * `SelectivityModelsLength()` prints a list of available selectivity-at-length models
#' * `SelectivityModelsAge()` prints a list of available selectivity-at-age models
#' * `SelectivityModelWeight()` prints a list of available selectivity-at-weight models
#' 
#' @return
#' Numeric vector of selectivity at each age, length, or weight.  
#' `SelectivityModels()` invisibly returns a data frame describing available models.
#'
#' @seealso [Ages()], [Length()], [Weight()], [Stock()]
#' @example man-examples/models-selectivity.R
#'
#' @name Selectivity-Models
#' @rdname Selectivity-Models
NULL



#' @rdname Selectivity-Models
#' @export
SelectivityAtAge <- function(Ages, SA50, SA50_95) {
  logistic_50_95(Ages, x50 = SA50, x50_95 = SA50_95)
}
class(SelectivityAtAge) <- 'Selectivity-at-Age-Model'

#' @rdname Selectivity-Models
#' @export
SelectivityKnifeEdgeAge <- function(Ages, SA) {
  as.numeric(Ages >= SA)
}
class(SelectivityKnifeEdgeAge) <- 'Selectivity-at-Age-Model'


#' @rdname Selectivity-Models
#' @export
SelectivityAtLength <- function(Length, SL50, SL50_95) {
  logistic_50_95(Length, x50 = SL50, x50_95 = SL50_95)
}
class(SelectivityAtLength) <- 'Selectivity-at-Length-Model'


#' @rdname Selectivity-Models
#' @export
SelectivityKnifeEdgeLength <- function(Length, SL) {
  as.numeric(Length >= SL)
}
class(SelectivityKnifeEdgeLength) <- 'Selectivity-at-Length-Model'


#' @rdname Selectivity-Models
#' @export
SelectivityAtWeight <- function(Weight, SW50, SW50_95) {
  logistic_50_95(Weight, x50 = SW50, x50_95 = SW50_95)
}
class(SelectivityAtWeight) <- 'Selectivity-at-Weight-Model'



#' @rdname Selectivity-Models
#' @export
DoubleNormal <- function(Length, L5, LFS, Vmaxlen) {
  double_normal(Length, L5, LFS, Vmaxlen)
}
class(DoubleNormal) <- 'Selectivity-at-Length-Model'

#' @rdname Selectivity-Models
#' @export
DoubleNormalWeight <- function(Weight, W5, WFS, Vmaxweight) {
  double_normal(Weight, W5, WFS, Vmaxweight)
}
class(DoubleNormalWeight) <- 'Selectivity-at-Weight-Model'


#' @rdname Selectivity-Models
#' @export
SelectivityModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Age-Model',
                            'Selectivity-at-Length-Model',
                            'Selectivity-at-Weight-Model'),
               full, print)
}

#' @rdname Selectivity-Models
#' @export
SelectivityModelsLength <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Length-Model'),
               full, print)
}

#' @rdname Selectivity-Models
#' @export
SelectivityModelsAge <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Age-Model'),
               full, print)
}

#' @rdname Selectivity-Models
#' @export
SelectivityModelsWeight <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Selectivity-at-Weight-Model'),
               full, print)
}