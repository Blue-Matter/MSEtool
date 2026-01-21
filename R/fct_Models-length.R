#' Length-at-Age Growth Models
#'
#' The length-at-age models included in the package.
#' 
#'  Currently only the von Bertalanffy is included.
#' 
#' `LengthModels()` prints the list of models and parameters to the console
#' 
#' @section Models:
#'
#' **von Bertalanffy**
#' \deqn{L(a) = L_\infty (1 - e^{-K (a - t_0)})}
#'
#' @param Ages Numeric vector of ages (always in years). See [Ages()].#'
#' @param Linf Asymptotic length.
#' @param K Growth rate parameter.
#' @param t0 Theoretical age at zero length. Positive values mean negative 
#' length at some positive age; an unlikely reality for most species!! 
#' Negative lengths will be set to 0. 
#' Values << 0 can imply a relatively large size at age-0; unlikely for many species
#'  (life-bearers like sharks may be an exception).
#'
#' @param full Logical. Provide a complete table (TRUE) or just the model names (FALSE)?
#' @param print Logical. Print out the results (TRUE) or just return the data.frame (FALSE)?
#' 
#' @return 
#' * `LengthModels()` invisibly returns a data frame containing the models
#' *  Growth models: Numeric vector of expected lengths at age.
#'
#' @seealso [Ages()], [Length()] 
#' @examples
#' Ages <- seq(0, 10, by = 0.5)
#' vonBert(Ages, Linf=80, K=0.25,t0 =-0.5)
#' 
#' @name Length-at-Age-Models
#' @rdname Length-at-Age-Models
NULL


#' @name Length-at-Age-Models
#' @export
LengthModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass='Length-at-Age-Model', full, print)
}

#' @name Length-at-Age-Models
#' @export
vonBert <- function(Ages, Linf, K, t0 = 0) {
  LAA <- Linf * (1-exp(-K*(Ages-t0)))
  LAA[LAA<0] <- 0
  LAA

}
class(vonBert) <- 'Length-at-Age-Model'
