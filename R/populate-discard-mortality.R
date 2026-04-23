#' Populate a DiscardMortality Object
#'
#' Populates a `DiscardMortality` object with mean-at-age and mean-at-length 
#' values.
#'
#' @param DiscardMortality A [DiscardMortality()] object to populate.
#' @param Ages An [Ages()] object defining age classes.
#' @param Length A [Length()] object. Required if the model depends on 
#'   length-at-age.
#' @param nSim Integer. Number of simulation replicates.
#' @param Years Numeric vector of model years.
#' @param nArea Integer. Number of spatial areas.
#' @param CalcAtLength Logical; if `TRUE`, calculates mean-at-length from 
#'   age-based arrays.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical; if `TRUE`, suppresses informational messages.
#' @param force Logical; if `TRUE`, forces re-population even if digest 
#'   indicates object is current.
#' @param replace Used internally.
#' @param ASKOverride Used internally.
#' @details
#' `PopulateDiscardMortality()` handles population of discard mortality at age 
#' and optionally at length. Steps include:
#'
#' * Converting mean-at-length to mean-at-age if necessary
#' * Calculating mean-at-length from age-based arrays if `CalcAtLength = TRUE`
#' * Adding an 'Area' dimension and setting dimension names
#' * Handling empty objects by setting discard mortality to zero
#'
#' @return
#' A populated [DiscardMortality()] object.
#'
#' @examples
#' \dontrun{
#' DM <- DiscardMortality()
#' DM_pop <- PopulateDiscardMortality(
#'   DM, Ages = Ages, Length = Length, Years = 2000:2025, nSim = 5
#' )
#' }
#'
#' @export
PopulateDiscardMortality <- function(DiscardMortality,
                                     Ages = NULL,
                                     Length = NULL,
                                     nSim = 5,
                                     Years = NULL,
                                     nArea = 1,
                                     CalcAtLength = TRUE,
                                     seed = NULL,
                                     silent = FALSE,
                                     force = FALSE,
                                     replace = FALSE,
                                     ASKOverride = NULL) {
  
  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed)
  
  if (CheckDigest(DiscardMortality, argList) & !force) {
    return(DiscardMortality)
  }
  
  # Default: no discard mortality if object is empty
  if (EmptyObject(DiscardMortality)) {
    DiscardMortality@MeanAtAge <- array(0, dim = c(1, length(Ages@Classes), 1, 1)) 
    dimnames(DiscardMortality@MeanAtAge) <- list(
      Sim = 1,
      Age = Ages@Classes,
      Year = Years[1],
      Area = 1
    )
    
    DiscardMortality@Classes <- Length@Classes
    DiscardMortality@MeanAtLength <- array(0, dim = c(1, length(DiscardMortality@Classes), 1, 1))
    dimnames(DiscardMortality@MeanAtLength) <- list(
      Sim = 1,
      Class = DiscardMortality@Classes,
      Year = Years[1],
      Area = 1
    )
    return(SetDigest(DiscardMortality, argList))
  }
  
  SetSeed(seed)
  
  AgeClasses <- Ages@Classes
  nAge <- length(AgeClasses)
  
  if (!is.null(DiscardMortality@MeanAtAge)) {
    if (!is.array(DiscardMortality@MeanAtAge)) {
      if (!length(DiscardMortality@MeanAtAge)==1 &&
          !length(DiscardMortality@MeanAtAge)==nAge)
        cli::cli_abort("If `MeanAtAge(DiscardMortality)` is numeric vector, it must be length 1 or length `nAge`")
      
      DiscardMortality@MeanAtAge <- array(DiscardMortality@MeanAtAge, dim=c(1, nAge, 1),
            dimnames=list(Sim = 1,
                          Age=AgeClasses,
                          Year=Years[1]))
    }
    
  }
 
  
  DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, Length)
  
  DiscardMortality@MeanAtAge <- AddAtAgeDimnames(DiscardMortality@MeanAtAge, 
                                          Ages, Years,
                                          name='DiscardMortality')
  
  if (CalcAtLength) {
    DiscardMortality <- MeanAtAge2MeanAtLength(DiscardMortality, Length,
                                               replace = replace, 
                                               Years=Years,
                                               ASK = ASKOverride)
  }
  
  # Add Area dimension
  DiscardMortality@MeanAtLength <- AddDimension(DiscardMortality@MeanAtLength, "Area")
  DiscardMortality@MeanAtAge <- AddDimension(DiscardMortality@MeanAtAge, "Area")
  
  # Add dimension names if missing
  if (is.null(dimnames(DiscardMortality@MeanAtLength))) {
    dd <- dim(DiscardMortality@MeanAtLength)
    if (!is.null(dd)) {
      dimnames(DiscardMortality@MeanAtLength) <- list(
        Sim = 1:dd[1],
        Class = DiscardMortality@Classes,
        Year = Years[1:dd[3]],
        Area = 1:dd[4]
      )
    }
  }
  
  if (is.null(dimnames(DiscardMortality@MeanAtAge))) {
    dd <- dim(DiscardMortality@MeanAtAge)
    if (!is.null(dd)) {
      dimnames(DiscardMortality@MeanAtAge) <- list(
        Sim = 1:dd[1],
        Age = Ages@Classes[1:dd[2]],
        Year = Years[1:dd[3]],
        Area = 1:dd[4]
      )
    }
  }
  
  SetDigest(DiscardMortality, argList)
}
