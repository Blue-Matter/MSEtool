#' Unit Conversion Functions
#'
#' A family of simple unit conversion functions for weight and length
#' measurements commonly used in fisheries stock assessments.
#'
#' **Weight:**
#' - `lb2kg(x)`: pounds → kilograms
#' - `lb2mt(x)`: pounds → metric tonnes
#' - `kg2lb(x)`: kilograms → pounds
#' - `kg2mt(x)`: kilograms → metric tonnes
#' - `kg2_1000lb(x)`: kilograms → thousands of pounds
#'
#' **Length:**
#' - `inch2mm(x)`: inches → millimetres
#' - `mm2inch(x)`: millimetres → inches
#'
#' @param x Numeric vector of values to convert.
#'
#' @return Numeric vector of the same length as `x` in the target units.
#'
#' @name unit-conversions
#' @export
lb2kg <- function(x) x * 0.453592

#' @rdname unit-conversions
#' @export
lb2mt <- function(x) lb2kg(x) / 1000

#' @rdname unit-conversions
#' @export
kg2lb <- function(x) x / 0.453592

#' @rdname unit-conversions
#' @export
kg2mt <- function(x) x / 1000

#' @rdname unit-conversions
#' @export
kg2_1000lb <- function(x) kg2lb(x) / 1000

#' @rdname unit-conversions
#' @export
inch2mm <- function(x) x * 25.4

#' @rdname unit-conversions
#' @export
mm2inch <- function(x) x / 25.4