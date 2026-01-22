#' Natural Mortality Models
#'
#' Natural mortality models for age, length, and weight.
#'
#' @param Ages numeric vector of age classes (for age-based models)
#' @param Length numeric vector of length classes (for length-based models)
#' @param Weight numeric vector of weight classes (for weight-based models)
#' @param M constant natural mortality rate (for constant M-at-age)
#' @param Mref reference mortality (for allometric or Lorenzen models)
#' @param Lref reference length for allometric or Lorenzen M-at-length
#' @param Wref reference weight for Lorenzen M-at-weight
#' @param c exponent for allometric M-at-length or Lorenzen models (default -0.288)
#' @param full logical; provide a complete table of models (TRUE) or just model names (FALSE)
#' @param print logical; print results (TRUE) or return data frame invisibly (FALSE)
#'
#' @section Model Equations:
#' - **Constant M-at-age**: \deqn{M(a) = M}  
#' - **Allometric M-at-length**: \deqn{M(L) = M_{ref} \cdot \left(\frac{L}{L_{ref}}\right)^c}  
#' - **Lorenzen M-at-length**: \deqn{M(L) = M_{ref} \cdot \left(\frac{L}{L_{ref}}\right)^{-0.288}}  
#' - **Lorenzen M-at-weight**: \deqn{M(W) = M_{ref} \cdot \left(\frac{W}{W_{ref}}\right)^{-0.288}}
#'
#'
#' `NaturalMortalityModels()` prints a list of available models.
#' 
#' These functions return numeric vectors of natural mortality for the given ages, lengths, or weights.  
#'
#' @return
#' Numeric vector of natural mortality at each age, length, or weight.  
#' `NaturalMortalityModels()` invisibly returns a data frame describing available models.
#'
#' @seealso [Ages()], [Length()], [Weight()], [Stock()]
#' @example man-examples/models-mortality.R
#'
#' @name NaturalMortality-Models
#' @rdname NaturalMortality-Models
NULL


#' @rdname NaturalMortality-Models
#' @export
NaturalMortalityModels <- function(full=TRUE, print=TRUE) {
  ReturnModels(ModelClass=c('NaturalMortality-at-Age-Model',
                            'NaturalMortality-at-Length-Model',
                            'NaturalMortality-at-Weight-Model'),
               full, print)
}


# Constant natural mortality
#' @rdname NaturalMortality-Models
#' @export
MortalityAtAge <- function(Ages, M) {
  rep(M, length(Ages))
}
class(MortalityAtAge) <- 'NaturalMortality-at-Age-Model'


# Allometric M-at-length
#' @rdname NaturalMortality-Models
#' @export
MortalityAtLength <- function(Length, Mref, Lref, c) {
  Mref * (Length / Lref)^c
}
class(MortalityAtLength) <- 'NaturalMortality-at-Length-Model'


# Lorenzen M-at-length
#' @rdname NaturalMortality-Models
#' @export
LorenzenMortalityLength <- function(Length, Mref, Lref, c=-0.288) {
  Mref * (Length / Lref)^c
}
class(LorenzenMortalityLength) <- 'NaturalMortality-at-Length-Model'


# Lorenzen M-at-weight
#' @rdname NaturalMortality-Models
#' @export
LorenzenMortalityWeight <- function(Weight, Mref, Wref, c=-0.288) {
  Mref * (Weight / Wref)^c
}
class(LorenzenMortalityWeight) <- 'NaturalMortality-at-Weight-Model'

