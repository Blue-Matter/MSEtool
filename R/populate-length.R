#' Populate a Length Object
#'
#' Populate a `Length` object by generating stochastic length-at-age values,
#' expanding parameters across simulation replicates and years, and optionally
#' constructing age–length keys.
#'
#'
#' @param Length A [Length()] object to populate.
#' @param Ages An [Ages()] object defining age classes. If `NULL`, defaults
#'   ages 0:10 are used.
#' @param Years Numeric vector of model years. If `NULL`, defaults years
#' `seq(1950, CurrentYear + 5)` are used.
#' @param nSim Integer. Number of simulation replicates.
#' @param ALK Logical. If `TRUE`, populate age–length keys.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if the object
#'   digest is unchanged.
#'
#' @details
#' `PopulateLength()` expands length-at-age parameters and generates stochastic
#' realizations across simulations and years. Population involves:
#'
#' * Structuring model parameters across `nSim` and `Years`
#' * Resolving the length-at-age model
#' * Generating mean and random length-at-age values
#' * Expanding coefficients of variation
#' * Optionally constructing age–length keys
#'
#' To avoid unnecessary recomputation, a digest of the length object and key
#' arguments is checked before population. If the digest is unchanged and
#' `force = FALSE`, the input object is returned unchanged.
#'
#' @return
#' A populated [Length()] object.
#'
#' @seealso
#' [Populate()], [PopulateStock()]
#'
#' @examples
#' \dontrun{
#' Length <- PopulateLength(
#'   Length,
#'   Ages = Ages,
#'   Years = Years,
#'   nSim = 100,
#'   ALK = TRUE,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateLength <- function(Length,
                           Ages = NULL,
                           Years = NULL,
                           nSim = 5,
                           ALK = TRUE,
                           seed = NULL,
                           silent = FALSE,
                           force = FALSE) {
  Ages <- DefaultAges(Ages)
  Years <- DefaultYears(Years)
  
  argList <- list(Ages, nSim, Years, ALK, seed)
  
  if (EmptyObject(Length)) 
    return(Length)
  
  # cli::cli_abort('{.val Length} is required but is currently empty')
  
  if (CheckDigest(Length, argList) & !force) 
    return(Length)
  
  
  SetSeed(seed)
  
  Length@Pars <- StructurePars(Pars = Length@Pars, nSim, Years)
  Length@Model <- FindModel(Length)
  Length <- PopulateMeanAtAge(Length, Ages, Years)
  Length <- PopulateRandom(Length)
  Length@CVatAge <- StructureCV(Length@CVatAge, nSim)
  dd <- dim(Length@CVatAge)
  
  if (is.null(dimnames(Length@CVatAge))) {
    dimnames(Length@CVatAge) <- list(
      Sim = (1:nSim)[1:dd[1]],
      Age = Ages@Classes[1:dd[2]],
      Year = Years[1:dd[3]]
    )
  }
  
  if (is.null(Length@CVatAge)) 
    ALK <- FALSE
  
  if (!is.null(Length@CVatAge)) 
    Length <- PopulateClasses(Length)
  
  if (ALK && !is.null(Length@Classes)) 
    Length <- PopulateASK(object = Length, 
                          Ages = Ages, 
                          silent = silent)
  
  SetDigest(Length, argList)
}
