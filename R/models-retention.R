#' Retention-at-Age, -Length, and -Weight Models
#'
#' Functions for generating retention curves for age, length, or weight.
#' Includes standard logistic, knife-edge, and double-normal models.
#'
#' @param Ages Numeric vector of age classes for age-based models.
#' @param RA50 First age at 50% retention for logistic retention-at-age.
#' @param RA50_95 Interval between 50% and 95% retention for logistic retention-at-age.
#' @param Length Numeric vector of length classes for length-based models.
#' @param RL50 First length at 50% retention for logistic retention-at-length.
#' @param RL50_95 Interval between 50% and 95% retention for logistic retention-at-length.
#' @param RW50 First weight at 50% retention for logistic retention-at-weight.
#' @param RW50_95 Interval between 50% and 95% retention for logistic retention-at-weight.
#' @param LR5 First length at 5% retention (double-normal).
#' @param LFR Full-retention length (double-normal).
#' @param Rmaxlen Retention value at `max(Length)` (double-normal).
#' @param WR5 First weight at 5% retention (double-normal).
#' @param WFR Full-retention weight (double-normal).
#' @param Rmaxweight Retention value at `max(Weight)` (double-normal).
#' @param RL Knife-edge length threshold.
#' @param RA Knife-edge age threshold.
#' @param full Logical; provide a complete table of models (TRUE) or just model names (FALSE).
#' @param print Logical; print results (TRUE) or return data frame invisibly (FALSE).
#'
#' At-length and at-weight schedules are converted internally to at-age using the age-length 
#' and age-weight key respectively.
#' 
#' - **Logistic**: \deqn{R(x) = \frac{1}{1 + \exp(-\ln(19) \frac{x - x_{50}}{x_{95} - x_{50}})}}
#' - **Knife-edge**: \deqn{R(x) = 0 \text{ if } x < x_{50}, 1 \text{ if } x \ge x_{50}}
#' - **Double-normal**: combination of ascending and descending half-normal curves.
#'
#' * `RetentionModels()` prints a list of available retention models.
#' * `RetentionModelsLength()` prints a list of available retention-at-length models.
#' * `RetentionModelsAge()` prints a list of available retention-at-age models.
#' * `RetentionModelsWeight()` prints a list of available retention-at-weight models.
#' 
#' @return
#' Numeric vector of retention at each age, length, or weight.  
#' `RetentionModels()` invisibly returns a data frame describing available models.
#'
#' @seealso [Ages()], [Length()], [Weight()], [Stock()]
#' @example man-examples/models-retention.R
#'
#' @name Retention-Models
#' @rdname Retention-Models
NULL

# Logistic retention
#' @rdname Retention-Models
#' @export
RetentionAtAge <- function(Ages, RA50, RA50_95) {
  logistic_50_95(Ages, x50 = RA50, x50_95 = RA50_95)
}
class(RetentionAtAge) <- 'Retention-at-Age-Model'

# Knife-edge retention
#' @rdname Retention-Models
#' @export
RetentionKnifeEdgeAge <- function(Ages, RA) {
  as.numeric(Ages >= RA)
}
class(RetentionKnifeEdgeAge) <- 'Retention-at-Age-Model'

# Logistic retention at length
#' @rdname Retention-Models
#' @export
RetentionAtLength <- function(Length, RL50, RL50_95) {
  logistic_50_95(Length, x50 = RL50, x50_95 = RL50_95)
}
class(RetentionAtLength) <- 'Retention-at-Length-Model'

# Knife-edge retention at length
#' @rdname Retention-Models
#' @export
RetentionKnifeEdgeLength <- function(Length, RL) {
  as.numeric(Length >= RL)
}
class(RetentionKnifeEdgeLength) <- 'Retention-at-Length-Model'

# Logistic retention at weight
#' @rdname Retention-Models
#' @export
RetentionAtWeight <- function(Weight, RW50, RW50_95) {
  logistic_50_95(Weight, x50 = RW50, x50_95 = RW50_95)
}
class(RetentionAtWeight) <- 'Retention-at-Weight-Model'

# Double-normal retention
#' @rdname Retention-Models
#' @export
DoubleNormalRetention <- function(Length, LR5, LFR, Rmaxlen) {
  double_normal(x=Length, x5=LR5, xF=LFR, xMax=Rmaxlen)
}
class(DoubleNormalRetention) <- 'Retention-at-Length-Model'

# Double-normal retention at weight
#' @rdname Retention-Models
#' @export
DoubleNormalRetentionWeight <- function(Weight, WR5, WFR, Rmaxweight) {
  double_normal(Weight, WR5, WFR, Rmaxweight)
}
class(DoubleNormalRetentionWeight) <- 'Retention-at-Weight-Model'


#' @rdname Retention-Models
#' @export
RetentionModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Retention-at-Age-Model',
                            'Retention-at-Length-Model',
                            'Retention-at-Weight-Model'),
               full, print)
}

#' @rdname Retention-Models
#' @export
RetentionModelsLength <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Retention-at-Length-Model'),
               full, print)
}

#' @rdname Retention-Models
#' @export
RetentionModelsAge <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Retention-at-Age-Model'),
               full, print)
}

#' @rdname Retention-Models
#' @export
RetentionModelsWeight <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('Retention-at-Weight-Model'),
               full, print)
}
