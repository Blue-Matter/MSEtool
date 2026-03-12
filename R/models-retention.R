#' Retention Models
#'
#' Logistic, knife-edge, and double-normal retention-at-age,
#' retention-at-length, and retention-at-weight models for use in a
#' [Retention()] object.
#'
#' @param Ages Numeric vector of age classes.
#' @param Length Numeric vector of length classes.
#' @param Weight Numeric vector of weight classes.
#' @param RA50 Numeric. Age at 50% retention (logistic-at-age).
#' @param RA50_95 Numeric. Interval between `RA50` and age at 95% retention
#'   (logistic-at-age).
#' @param MaxRet Numeric. Maximum (asymptotic) retention. Default `1` (full
#'   retention). Values less than 1 produce a dome-shaped or partially
#'   selective retention curve.
#' @param RL50 Numeric. Length at 50% retention (logistic-at-length).
#' @param RL50_95 Numeric. Interval between `RL50` and length at 95% retention
#'   (logistic-at-length).
#' @param RW50 Numeric. Weight at 50% retention (logistic-at-weight).
#' @param RW50_95 Numeric. Interval between `RW50` and weight at 95% retention
#'   (logistic-at-weight).
#' @param LR5 Numeric. Length at 5% retention (double-normal).
#' @param LFR Numeric. Length at full retention (double-normal).
#' @param Rmaxlen Numeric. Retention at `max(Length)` (double-normal). Values
#'   less than 1 produce a dome-shaped retention curve.
#' @param WR5 Numeric. Weight at 5% retention (double-normal).
#' @param WFR Numeric. Weight at full retention (double-normal).
#' @param Rmaxweight Numeric. Retention at `max(Weight)` (double-normal).
#'   Values less than 1 produce a dome-shaped retention curve.
#' @param RL Numeric. Knife-edge length threshold; fish at or above this
#'   length are fully retained.
#' @param RA Numeric. Knife-edge age threshold; fish at or above this age are
#'   fully retained.
#' @param full Logical. If `TRUE` (default), returns a complete table of
#'   available models. If `FALSE`, returns model names only.
#' @param print Logical. If `TRUE` (default), prints results to the console.
#'   If `FALSE`, returns the data frame invisibly without printing.
#'
#' @details
#' Three families of retention model are available:
#'
#' - **Logistic**: a standard increasing retention curve parameterised by the
#' 50% and 95% retention points, with asymptote `MaxRet`:
#'   \deqn{R(x) = \frac{\texttt{MaxRet}}{1 + \exp\left(-\ln(19) \cdot
#'                                                       \frac{x - x_{50}}{x_{95} - x_{50}}\right)}}
#' - **Knife-edge**: full retention at or above a threshold, zero below:
#'   \deqn{R(x) = \begin{cases} 0 & x < x_t \\ 1 & x \geq x_t \end{cases}}
#' - **Double-normal**: a combination of ascending and descending half-normal
#'   curves, producing either asymptotic (`Rmaxlen = 1`) or dome-shaped
#'   (`Rmaxlen < 1`) retention.
#'
#' At-length and at-weight schedules are converted internally to at-age using
#' the age-length key and age-weight key respectively.
#'
#' The available model functions are:
#' - `RetentionAtAge()`: logistic retention-at-age.
#' - `RetentionKnifeEdgeAge()`: knife-edge retention-at-age.
#' - `RetentionAtLength()`: logistic retention-at-length.
#' - `RetentionKnifeEdgeLength()`: knife-edge retention-at-length.
#' - `RetentionAtWeight()`: logistic retention-at-weight.
#' - `DoubleNormalRetention()`: double-normal retention-at-length.
#' - `DoubleNormalRetentionWeight()`: double-normal retention-at-weight.
#'
#' The `RetentionModels*` functions list available models:
#' - `RetentionModels()`: all retention models.
#' - `RetentionModelsAge()`: retention-at-age models only.
#' - `RetentionModelsLength()`: retention-at-length models only.
#' - `RetentionModelsWeight()`: retention-at-weight models only.
#'
#' @return
#' - `RetentionAtAge()`, `RetentionKnifeEdgeAge()`, `RetentionAtLength()`,
#'   `RetentionKnifeEdgeLength()`, `RetentionAtWeight()`,
#'   `DoubleNormalRetention()`, `DoubleNormalRetentionWeight()`: a numeric
#'   vector of retention values (0–1) at each age, length, or weight class
#'   respectively.
#' - `RetentionModels()`, `RetentionModelsAge()`, `RetentionModelsLength()`,
#'   `RetentionModelsWeight()`: invisibly returns a data frame (if
#'   `full = TRUE`) or character vector (if `full = FALSE`) of available
#'   models. Prints to console if `print = TRUE`.
#'
#' @seealso [Retention()], [Selectivity()], [Ages()], [Length()], [Weight()],
#'   [Fleet()]
#'
#' @example man-examples/models-retention.R
#'
#' @name Retention-Models
#' @rdname Retention-Models
NULL

# Logistic retention
#' @rdname Retention-Models
#' @export
RetentionAtAge <- function(Ages, RA50, RA50_95, MaxRet = 1) {
  logistic_50_95(Ages, x50 = RA50, x50_95 = RA50_95, asymp = MaxRet)
}
class(RetentionAtAge) <- 'Retention-at-Age-Model'

#' Knife-edge retention
#' @rdname Retention-Models
#' @export
RetentionKnifeEdgeAge <- function(Ages, RA) {
  as.numeric(Ages >= RA)
}
class(RetentionKnifeEdgeAge) <- 'Retention-at-Age-Model'

# Logistic retention at length
#' @rdname Retention-Models
#' @export
RetentionAtLength <- function(Length, RL50, RL50_95, MaxRet = 1) {
  logistic_50_95(Length, x50 = RL50, x50_95 = RL50_95, asymp = MaxRet)
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
RetentionAtWeight <- function(Weight, RW50, RW50_95, MaxRet = 1) {
  logistic_50_95(Weight, x50 = RW50, x50_95 = RW50_95, asymp = MaxRet)
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
  double_normal(x    = Weight, 
                x5   = WR5, 
                xF   = WFR, 
                xMax = Rmaxweight)
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
