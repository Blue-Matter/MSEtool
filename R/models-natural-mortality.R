#' Natural Mortality Models
#'
#' Natural mortality models for age, length, and weight.
#'
#' @param Ages numeric vector of age classes
#' @param Length numeric vector of length classes
#' @param Weight numeric vector of weight classes
#' @param M  natural mortality rate. Recycled if not `length(Ages)`
#' @param Mref reference mortality
#' @param Lref reference length for Lorenzen M-at-length
#' @param Wref reference weight for Lorenzen M-at-weight
#' @param c exponent for Lorenzen models (default -0.288)
#' @param full logical; provide a complete table of models (TRUE) or just model names (FALSE)
#' @param print logical; print results (TRUE) or return data frame invisibly (FALSE)
#' 
#' @details
#' at-length and at-weight schedules are converted internally to at-age using the age-length 
#' age-weight key respectively.
#' 
#' - **M-at-age**: \deqn{M(a) = M}  
#' - **Lorenzen M-at-length**: \deqn{M(L) = M_{ref} \cdot \left(\frac{L}{L_{ref}}\right)^{-0.288}}  
#' - **Lorenzen M-at-weight**: \deqn{M(W) = M_{ref} \cdot \left(\frac{W}{W_{ref}}\right)^{-0.288}}
#'
#' `NaturalMortalityModels()` prints a list of available models.
#' 
#'
#' @return
#' Numeric vector of natural mortality at each age, length, or weight.  
#' `NaturalMortalityModels()` invisibly returns a data frame describing available models.
#'
#' @seealso [Ages()], [Length()], [Weight()], [Stock()]
#' @example man-examples/models-natural-mortality.R
#'
#' @name NaturalMortality-Models
#' @rdname NaturalMortality-Models
NULL


# Constant natural mortality
#' @rdname NaturalMortality-Models
#' @export
MortalityAtAge <- function(Ages, M) {
  rep(M, length(Ages))[1:length(Ages)]
}
class(MortalityAtAge) <- 'NaturalMortality-at-Age-Model'


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



#' @rdname NaturalMortality-Models
#' @export
NaturalMortalityModels <- function(full=TRUE, print=TRUE) {
  .ReturnModels(ModelClass=c('NaturalMortality-at-Age-Model',
                            'NaturalMortality-at-Length-Model',
                            'NaturalMortality-at-Weight-Model'),
               full, print)
}
