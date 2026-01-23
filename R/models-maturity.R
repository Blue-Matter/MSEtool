#' Maturity Models: Age, Length, and Weight
#'
#' Logistic maturity models
#'
#'
#' @param Ages numeric vector of age classes (for age-based models)
#' @param Length numeric vector of length classes (for length-based models)
#' @param Weight numeric vector of weight classes (for weight-based models)
#' @param A50 age at 50% maturity (Logistic-at-Age)
#' @param A50_95 interval between `A50` and age at 95% maturity (Logistic-at-Age)
#' @param L50 length at 50% maturity (Logistic-at-Length)
#' @param L50_95 interval between `L50` length at 95% maturity (Logistic-at-Length)
#' @param W50 weight at 50% maturity (Logistic-at-Weight)
#' @param W50_95 interval between `W50` weight at 95% maturity (Logistic-at-Weight)
#' @param full logical; provide a complete table of models (TRUE) or just model names (FALSE)
#' @param print logical; print results (TRUE) or return data frame invisibly (FALSE)
#'
#' * `MaturityModels()` prints the list of available maturity models
#' * `MaturityModelsLength()` prints the list of available maturity-at-length models
#' * `MaturityModelsAge` prints the list of available maturity-at-age models
#' * `MaturityModelsWeight` prints the list of available maturity-at-weight models
#' 
#' @details
#' Logistic models produce standard increasing maturity curves.
#' - Age-based models return proportion mature by age.
#' - Length-based models return proportion mature by length.
#' - Weight-based models return proportion mature by weight.
#'
#' All maturity models use a logistic 50/95 parameterization:
#' \deqn{M(x) = \frac{1}{1 + \exp\left(-\ln(19) \cdot \frac{x - x_{50}}{x_{95} - x_{50}}\right)}}
#' 
#' at-length and at-weight schedules are converted internally to at-age using the age-length 
#' age-weight key respectively.
#' 
#' @return
#' Each function returns a numeric vector of proportion mature at each age, length, or weight.
#' `MaturityModels()` invisibly returns a data frame describing available models.
#'
#' @seealso [Ages()], [Length()], [Maturity()], [Weight()], [Stock()]
#' @example man-examples/models-maturity.R
#'
#' @name Maturity-Models
#' @rdname Maturity-Models
NULL


#' @rdname Maturity-Models
#' @export
MaturityModelsWeight <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c("Maturity-at-Weight-Model"),
    full, print
  )
}


#' @rdname Maturity-Models
#' @export
MaturityAtAge <- function(Ages, A50, A50_95) {
  logistic_50_95(Ages, x50 = A50, x50_95 = A50_95)
}
class(MaturityAtAge) <- "Maturity-at-Age-Model"


#' @rdname Maturity-Models
#' @export
MaturityAtLength <- function(Length, L50, L50_95) {
  logistic_50_95(Length, x50 = L50, x50_95 = L50_95)
}
class(MaturityAtLength) <- "Maturity-at-Length-Model"


#' @rdname Maturity-Models
#' @export
MaturityAtWeight <- function(Weight, W50, W50_95) {
  logistic_50_95(Weight, x50 = W50, x50_95 = W50_95)
}
class(MaturityAtWeight) <- "Maturity-at-Weight-Model"




#' @rdname Maturity-Models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
MaturityModels <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c(
      "Maturity-at-Age-Model",
      "Maturity-at-Length-Model",
      "Maturity-at-Weight-Model"
    ),
    full, print
  )
}

#' @rdname Maturity-Models
#' @export
MaturityModelsLength <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c("Maturity-at-Length-Model"),
    full, print
  )
}

#' @rdname Maturity-Models
#' @export
MaturityModelsAge <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c("Maturity-at-Age-Model"),
    full, print
  )
}

