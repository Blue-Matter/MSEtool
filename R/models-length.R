#' Length-at-Age Growth Models
#'
#' Length-at-age (LAA) growth models
#'
#' @param Ages Numeric vector of ages (always in years).
#' @param Linf asymptotic length (used by von Bertalanffy, Gompertz, Brody)
#' @param K growth coefficient (used by von Bertalanffy, Brody)
#' @param t0 theoretical age at zero length (von Bertalanffy)
#' @param L0 length at age 0 (Brody)
#' @param g growth rate parameter (Gompertz)
#' @param a age at inflection (Gompertz)
#' @param d Richards shape parameter. `d = 1` recovers von Bertalanffy; `d < 1`
#'   gives faster early growth; `d > 1` gives slower early growth.
#'
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#'
#' @details
#' 
#' `LengthModels()` prints the list of models and parameters to the console
#'
#' - **von Bertalanffy**: \deqn{L(a) = L_\infty (1 - e^{-K (a - t_0)})}
#' - **Brody**: \deqn{L(a) = L_\infty - (L_\infty - L_0) e^{-K a}}
#' - **Gompertz**: \deqn{L(a) = L_\infty \exp(-\exp(-g (a - a)))}
#' - **Richards**: \deqn{L(a) = L_\infty \left(1 - e^{-K(a - t_0)}\right)^{1/d}}
#'
#' @return
#' * `LengthModels()` invisibly returns a data frame
#'  containing the models
#' *  Growth models: Numeric vector of expected lengths at age.
#'
#' @seealso [Ages()], [Length()], [Stock()]
#' @example man-examples/models-length.R
#'
#' @name Length-at-Age-Models
#' @rdname Length-at-Age-Models
NULL



#' @name Length-at-Age-Models
#' @export
vonBert <- function(Ages, Linf, K, t0 = 0) {
  LAA <- Linf * (1 - exp(-K * (Ages - t0)))
  LAA[LAA < 0] <- 0
  LAA
}
class(vonBert) <- "Length-at-Age-Model"


#' @name Length-at-Age-Models
#' @export
Brody <- function(Ages, L0, Linf, K) {
  LAA <- Linf - (Linf - L0) * exp(-K * Ages)
  LAA[LAA < 0] <- 0
  LAA
}
class(Brody) <- "Length-at-Age-Model"


#' @name Length-at-Age-Models
#' @export
Gompertz <- function(Ages, Linf, g, a) {
  LAA <- Linf * exp(-exp(-g * (Ages - a)))
  LAA[LAA < 0] <- 0
  LAA
}
class(Gompertz) <- "Length-at-Age-Model"


#' @name Length-at-Age-Models
#' @export
Richards <- function(Ages, Linf, K, t0 = 0, d = 1) {
  LAA <- Linf * (1 - exp(-K * (Ages - t0)))^(1 / d)
  LAA[LAA < 0] <- 0
  LAA
}
class(Richards) <- "Length-at-Age-Model"



#' @name Length-at-Age-Models
#' @export
LengthModels <- function(full = TRUE, print = TRUE) {
  .ReturnModels(ModelClass = "Length-at-Age-Model", full, print)
}
