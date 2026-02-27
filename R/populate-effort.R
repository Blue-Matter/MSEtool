#' Populate an Effort Object
#'
#' Populate an `Effort` object by generating historical effort, 
#' and populating distribution and targeting across areas and simulation replicates.
#'
#' @param Effort An [Effort()] object to populate.
#' @param HistYears Numeric vector of historical years to populate effort for.
#' @param nArea Integer. Number of spatial areas.
#' @param nSim Integer. Number of simulation replicates.
#' @param seed Integer. Random number seed used for stochastic generation.
#'
#' @details
#' `PopulateEffort()` performs the following steps:
#'
#' * Generates historical effort if the `Effort` slot is a data frame.
#' * Ensures the effort matrix has correct dimensions and names.
#' * Validates fleet effort distribution and targeting across areas.
#'
#' @return
#' A populated [Effort()] object.
#'
#' @examples
#' \dontrun{
#' Eff <- Effort()
#' Eff_pop <- PopulateEffort(
#'   Effort = Eff,
#'   HistYears = 2000:2025,
#'   nArea = 3,
#'   nSim = 50,
#'   seed = 123
#' )
#' }
#'
#' @export
PopulateEffort <- function(Effort,
                           HistYears,
                           nArea = 1,
                           nSim = 5,
                           seed = NULL) {
  
  SetSeed(seed)
  
  if (is.null(Effort@Effort)) {
    return(Effort)
  }
  
  if (inherits(Effort@Effort, "data.frame")) {
    Effort@Effort <- GenHistEffort(
      Effort = Effort@Effort,
      nSim = nSim,
      Years = HistYears
    )
    Effort@Units <- "unitless"
  }
  
  dd <- dim(Effort@Effort)
  if (dd[2] != length(HistYears)) {
    cli::cli_abort("`ncol(Effort@Effort)` is not equal to `length(HistYears)`")
  }
  
  dimnames(Effort@Effort) <- list(
    Sim = 1:nrow(Effort@Effort),
    Year = HistYears
  )
  
  Effort@Distribution <- PopulateDistribution(
    Distribution = Effort@Distribution,
    nSim = nSim,
    HistYears = HistYears,
    nArea = nArea
  )
  
  Effort@Targeting <- PopulateTargeting(
    Targeting = Effort@Targeting,
    nSim = nSim,
    Years = HistYears
  )
  
  Effort
}



PopulateTargeting <- function(Targeting, nSim, Years) {
  if (is.null(Targeting)) {
    return(
      array(0.8, c(1,1), 
            dimnames = list(Sim=1, Year=Years[1]))  
    )
  }
  
  if (is.array(Targeting)) {
    dd <- dim(Targeting)
    dnames <- dimnames(Targeting)
    if (length(dd)>2) {
      cli::cli_abort("`Targeting` must be numeric or an array with dimensions: Sim x Year")
    }
    
    if ((dd[2]!=1 | dd[2]!=length(Years)) & is.null(dnames[['Year']])) {
      cli::cli_abort("Year dimension of `Targeting`array must be length 1, length `Years(OM)` or have named dimensions.")
    }
    
    if (is.null(dnames)) {
      dimnames(Targeting) <- list(
        Sim=1:dd[1],
        Years=Years[1:dd[2]]
      )
    }
    
  }
  Targeting
}

PopulateDistribution <- function(Distribution,
                                 nSim = 5,
                                 HistYears = NULL,
                                 nArea = NULL) {
  if (is.null(Distribution)) {
    Distribution <- array(NA,
                          dim = c(1, 1, nArea),
                          dimnames = list(
                            Sim = 1,
                            Year = HistYears[1],
                            Area = 1:nArea
                          )
    )
  } else {
    CheckClass(Distribution, c("array", "matrix"))
    dd <- dim(Distribution)
    # TODO check dimensions
    # TODO add dimnames if needed
  }
  
  Distribution
}

