#' Fecundity Models: Age, Length, and Weight
#'
#' Logistic fecundity models
#'
#' @param Ages numeric vector of age classes (for age-based models)
#' @param Length numeric vector of length classes (for length-based models)
#' @param Weight numeric vector of weight classes (for weight-based models)
#' @param A50 age at 50% maximum fecundity (Logistic-at-Age)
#' @param A50_95 interval between `A50` and age at 95% maximum fecundity (Logistic-at-Age)
#' @param L50 length at 50% maximum fecundity (Logistic-at-Length)
#' @param L50_95 interval between `L50` length at 95% maximum fecundity (Logistic-at-Length)
#' @param W50 weight at 50% maximum fecundity (Logistic-at-Weight)
#' @param W50_95 interval between `W50` weight at 95% maximum fecundity (Logistic-at-Weight)
#' @param W50_95 weight at 95% maximum fecundity (Logistic-at-Weight)
#' @param full logical; provide a complete table of models (TRUE) or just model names (FALSE)
#' @param print logical; print results (TRUE) or return data frame invisibly (FALSE)
#'
#' * `FecundityModels()` prints the list of available fecundity models
#' * `FecundityModelsLength()` prints the list of available fecundity-at-length models
#' * `FecundityModelsAge` prints the list of available fecundity-at-age models
#' * `FecundityModelsWeight` prints the list of available fecundity-at-weight models
#'
#' `FecundityModels()` prints the list of available fecundity models.
#' 
#' Logistic models produce standard increasing fecundity curves.
#' - Age-based models return fecundity by age.
#' - Length-based models return fecundity by length.
#' - Weight-based models return fecundity by weight.
#' 
#' All fecundity models use a logistic 50/95 parameterization:
#' \deqn{F(x) = \frac{1}{`MaxFec` + \exp\left(-\ln(19) \cdot \frac{x - x_{50}}{x_{95} - x_{50}}\right)}}
#'
#'
#' @return
#' Each function returns a numeric vector of fecundity at each age, length, or weight.
#' `FecundityModels()` invisibly returns a data frame describing available models.
#'
#' @seealso [Ages()], [Length()], [Maturity()], [Weight()], [Stock()]
#' @example man-examples/models-fecundity.R
#'
#' @name Fecundity-Models
#' @rdname Fecundity-Models
NULL

#' @rdname Fecundity-Models
#' @export
FecundityAtAge <- function(Ages, A50, A50_95, MaxFec) {
  logistic_50_95(Ages, x50 = A50, x50_95 = A50_95, asymp=MaxFec)
}
class(FecundityAtAge) <- "Fecundity-at-Age-Model"


#' @rdname Fecundity-Models
#' @export
FecundityAtLength <- function(Length, L50, L50_95, MaxFec) {
  logistic_50_95(Length, x50 = L50, x50_95 = L50_95, asymp=MaxFec)
}
class(FecundityAtLength) <- "Fecundity-at-Length-Model"


#' @rdname Fecundity-Models
#' @export
FecundityAtWeight <- function(Weight, W50, W50_95, MaxFec) {
  logistic_50_95(Weight, x50 = W50, x50_95 = W50_95, asymp=MaxFec)
}
class(FecundityAtWeight) <- "Fecundity-at-Weight-Model"




#' @rdname Fecundity-Models
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @return Prints to console and invisible data.frame or model names
#' @export
FecundityModels <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c(
      "Fecundity-at-Age-Model",
      "Fecundity-at-Length-Model",
      "Fecundity-at-Weight-Model"
    ),
    full, print
  )
}

#' @rdname Fecundity-Models
#' @export
FecundityModelsLength <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c("Fecundity-at-Length-Model"),
    full, print
  )
}

#' @rdname Fecundity-Models
#' @export
FecundityModelsAge <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c("Fecundity-at-Age-Model"),
    full, print
  )
}

#' @rdname Fecundity-Models
#' @export
FecundityModelsWeight <- function(full = TRUE, print = TRUE) {
  ReturnModels(
    ModelClass = c("Fecundity-at-Weight-Model"),
    full, print
  )
}

