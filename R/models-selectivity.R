#' Selectivity Models
#'
#' Logistic, knife-edge, and double-normal selectivity-at-age,
#' selectivity-at-length, and selectivity-at-weight models for use in a
#' [Selectivity()] object.
#'
#' @param Ages Numeric vector of age classes.
#' @param Length Numeric vector of length classes.
#' @param Weight Numeric vector of weight classes.
#' @param SA50 Numeric. Age at 50% selectivity (logistic-at-age).
#' @param SA50_95 Numeric. Interval between `SA50` and age at 95% selectivity
#'   (logistic-at-age).
#' @param SL50 Numeric. Length at 50% selectivity (logistic-at-length).
#' @param SL50_95 Numeric. Interval between `SL50` and length at 95%
#'   selectivity (logistic-at-length).
#' @param SW50 Numeric. Weight at 50% selectivity (logistic-at-weight).
#' @param SW50_95 Numeric. Interval between `SW50` and weight at 95%
#'   selectivity (logistic-at-weight).
#' @param L5 Numeric. Length at 5% selectivity (double-normal).
#' @param LFS Numeric. Length at full selectivity (double-normal).
#' @param Vmaxlen Numeric. Selectivity at `max(Length)` (double-normal).
#'   Values less than 1 produce a dome-shaped selectivity curve.
#' @param W5 Numeric. Weight at 5% selectivity (double-normal).
#' @param WFS Numeric. Weight at full selectivity (double-normal).
#' @param Vmaxweight Numeric. Selectivity at `max(Weight)` (double-normal).
#'   Values less than 1 produce a dome-shaped selectivity curve.
#' @param SL Numeric. Knife-edge length threshold; fish at or above this
#'   length are fully selected.
#' @param SA Numeric. Knife-edge age threshold; fish at or above this age are
#'   fully selected.
#' @param full Logical. If `TRUE` (default), returns a complete table of
#'   available models. If `FALSE`, returns model names only.
#' @param print Logical. If `TRUE` (default), prints results to the console.
#'   If `FALSE`, returns the data frame invisibly without printing.
#'
#' @details
#' Three families of selectivity model are available:
#'
#' - **Logistic**: a standard increasing selectivity curve parameterised by
#'   the 50% and 95% selectivity points:
#'   \deqn{S(x) = \frac{1}{1 + \exp\left(-\ln(19) \cdot
#'   \frac{x - x_{50}}{x_{95} - x_{50}}\right)}}
#' - **Knife-edge**: full selectivity at or above a threshold, zero below:
#'   \deqn{S(x) = \begin{cases} 0 & x < x_t \\ 1 & x \geq x_t \end{cases}}
#' - **Double-normal**: a combination of ascending and descending half-normal
#'   curves, producing either asymptotic (`Vmaxlen = 1`) or dome-shaped
#'   (`Vmaxlen < 1`) selectivity.
#'
#' At-length and at-weight schedules are converted internally to at-age using
#' the age-length key and age-weight key respectively.
#'
#' The available model functions are:
#' - `SelectivityAtAge()`: logistic selectivity-at-age.
#' - `SelectivityKnifeEdgeAge()`: knife-edge selectivity-at-age.
#' - `SelectivityAtLength()`: logistic selectivity-at-length.
#' - `SelectivityKnifeEdgeLength()`: knife-edge selectivity-at-length.
#' - `SelectivityAtWeight()`: logistic selectivity-at-weight.
#' - `DoubleNormal()`: double-normal selectivity-at-length.
#' - `DoubleNormalWeight()`: double-normal selectivity-at-weight.
#'
#' The `SelectivityModels*` functions list available models:
#' - `SelectivityModels()`: all selectivity models.
#' - `SelectivityModelsAge()`: selectivity-at-age models only.
#' - `SelectivityModelsLength()`: selectivity-at-length models only.
#' - `SelectivityModelsWeight()`: selectivity-at-weight models only.
#'
#' @return
#' - `SelectivityAtAge()`, `SelectivityKnifeEdgeAge()`,
#'   `SelectivityAtLength()`, `SelectivityKnifeEdgeLength()`,
#'   `SelectivityAtWeight()`, `DoubleNormal()`, `DoubleNormalWeight()`: a
#'   numeric vector of selectivity values (0–1) at each age, length, or weight
#'   class respectively.
#' - `SelectivityModels()`, `SelectivityModelsAge()`,
#'   `SelectivityModelsLength()`, `SelectivityModelsWeight()`: invisibly
#'   returns a data frame (if `full = TRUE`) or character vector (if
#'   `full = FALSE`) of available models. Prints to console if `print = TRUE`.
#'
#' @seealso [Selectivity()], [Retention()], [Ages()], [Length()], [Weight()],
#'   [Fleet()]
#'
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
  double_normal(x = Length, x5 = L5, xF = LFS, xMax = Vmaxlen)
}
class(DoubleNormal) <- 'Selectivity-at-Length-Model'

#' @rdname Selectivity-Models
#' @export
DoubleNormalWeight <- function(Weight, W5, WFS, Vmaxweight) {
  double_normal(x = Weight, x5 = W5, xF = WFS, xMax = Vmaxweight)
}
class(DoubleNormalWeight) <- 'Selectivity-at-Weight-Model'

#' @rdname Selectivity-Models
#' @export
SelectivityModels <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c(
      'Selectivity-at-Age-Model',
      'Selectivity-at-Length-Model',
      'Selectivity-at-Weight-Model'
    ),
    full, print
  )
}

#' @rdname Selectivity-Models
#' @export
SelectivityModelsLength <- function(full = TRUE, print = TRUE) {
  ReturnModels(ModelClass = 'Selectivity-at-Length-Model', full, print)
}

#' @rdname Selectivity-Models
#' @export
SelectivityModelsAge <- function(full = TRUE, print = TRUE) {
  ReturnModels(ModelClass = 'Selectivity-at-Age-Model', full, print)
}

#' @rdname Selectivity-Models
#' @export
SelectivityModelsWeight <- function(full = TRUE, print = TRUE) {
  ReturnModels(ModelClass = 'Selectivity-at-Weight-Model', full, print)
}