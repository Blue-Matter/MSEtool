# TODO - use IsIdenticalSim and .IdenticalYears to speed up if identical over sims and/or years

#' Calculate Cumulative Survival-at-Age
#'
#' Computes cumulative survival from recruitment to each age class, accounting
#' for natural mortality, optional fishing mortality, spawn timing within a
#' time step, and optional semelparous mortality. The calculation follows:
#'
#' - **Age 1**: `S[1] = exp(-Z[1] * SpawnTimeFrac)`
#' - **Age a**: `S[a] = S[a-1] * exp(-(Z[a-1] * (1 - f) + Z[a] * f)) * (1 - Semelparous[a-1])`
#'
#' where `Z = M + F` and `f = SpawnTimeFrac`.
#'
#' If `PlusGroup = TRUE`, the plus-group age class is adjusted to account for
#' ongoing mortality: `S[nAge] = S[nAge] / (1 - exp(-Z[nAge]))`.
#'
#' @param NaturalMortality Numeric array of natural mortality-at-age. Either
#'   a 2D array (`Age × Year`) or 3D array (`Sim × Age × Year`), with named
#'   dimensions.
#' @param FishingMortality Optional numeric array of fishing mortality-at-age,
#'   matching the dimensions of `NaturalMortality`. If `NULL` (default),
#'   only natural mortality is used.
#' @param PlusGroup Logical. If `TRUE` (default), the plus-group survival is
#'   adjusted for ongoing mortality beyond the maximum age class.
#' @param SpawnTimeFrac Numeric. Fractional position of spawning within a
#'   time step, between 0 (start; default) and 1 (end). Either length 1
#'   (applied to all simulations) or length `nSim`.
#' @param Semelparous Either `FALSE` (default) to ignore semelparous
#'   mortality, or a numeric array of semelparous mortality-at-age matching
#'   the dimensions of `NaturalMortality`.
#'
#' @return A numeric array of cumulative survival-at-age with the same
#'   dimensions and dimnames as `NaturalMortality`. Each value represents
#'   the probability of surviving from recruitment to that age class in that
#'   year.
#'   
#' @export
CalcSurvival <- function(NaturalMortality, 
                         FishingMortality = NULL, 
                         PlusGroup = TRUE, 
                         SpawnTimeFrac = 0, 
                         Semelparous = FALSE) {

  d <- dim(NaturalMortality)
  if (!is.array(NaturalMortality) || !(length(d) %in% c(2L, 3L)))
    cli::cli_abort("`NaturalMortality` must be a 2D or 3D array with named dimensions.")
  
  Years      <- as.numeric(dimnames(NaturalMortality)[['Year']])
  AgeClasses <- as.numeric(dimnames(NaturalMortality)[['Age']])
  
  bySim <- length(d) == 3L
  if (!bySim) {
    NaturalMortality <- AddDimension(NaturalMortality, 'Sim') |>
      .Aperm(c('Sim', 'Age', 'Year'))
    FishingMortality <- AddDimension(FishingMortality, 'Sim') |>
      .Aperm(c('Sim', 'Age', 'Year'))
  }
  
  d     <- dim(NaturalMortality)
  nSim  <- d[1]
  nAge  <- d[2]
  nYear <- d[3]
  
  Semelparous <- .ProcessSemelparous(Semelparous, nSim, AgeClasses, Years) |>
    .SubsetYear(Years)
  if (!bySim)
    Semelparous <- AddDimension(Semelparous, 'Sim') |>
    .Aperm(c('Sim', 'Age', 'Year'))
  
  if (length(SpawnTimeFrac) != nSim)
    SpawnTimeFrac <- rep(SpawnTimeFrac, nSim)[seq_len(nSim)]
  
  if (!is.null(FishingMortality)) {
    ArrayList        <- ArrayExtend(NaturalMortality, FishingMortality)
    NaturalMortality <- ArrayList[[1]]
    FishingMortality <- ArrayList[[2]]
  }
  
  Z        <- ArraySum(NaturalMortality, FishingMortality)
  Survival <- array(0, dim=dim(NaturalMortality), dimnames=dimnames(NaturalMortality))
  
  Survival[, 1, ] <- exp(-Z[, 1, ] * SpawnTimeFrac)
  
  for (a in 2:nAge) {
    Survival[, a, ] <- Survival[, a - 1, ] *
      exp(-(Z[, a - 1, ] * (1 - SpawnTimeFrac) + Z[, a, ] * SpawnTimeFrac)) *
      (1 - Semelparous[, a - 1, ])
  }
  
  if (PlusGroup)
    Survival[, nAge, ] <- Survival[, nAge, ] / (1 - exp(-Z[, nAge, ]))
  
  if (!bySim)
    Survival <- DropDimension(Survival, 'Sim')
  
  Survival
}

#' Initialise a Semelparous Mortality Array
#'
#' Converts the `Semelparous` argument of [CalcSurvival()] into a
#' consistently structured array. If `Semelparous` is `FALSE`, returns a
#' zero array of dimensions `Sim × Age × Year`. Otherwise extends the
#' supplied array to match `nSim`, `AgeClasses`, and `Years` via [Extend()].
#'
#' @param Semelparous Either `FALSE` or a numeric array of semelparous
#'   mortality-at-age.
#' @param nSim Integer. Number of simulations.
#' @param AgeClasses Numeric vector of age class labels.
#' @param Years Numeric vector of year labels.
#'
#' @return A numeric array with dimensions `Sim × Age × Year` and named
#'   dimnames.
#' @keywords internal
.ProcessSemelparous <- function(Semelparous, nSim=NULL, AgeClasses=NULL, Years=NULL) {
  if (inherits(Semelparous, 'logical')) {
    Semelparous <- array(
      0,
      dim      = c(nSim, length(AgeClasses), length(Years)),
      dimnames = list(Sim=seq_len(nSim), Age=AgeClasses, Year=Years)
    )
  }
  Semelparous |> Extend(nSim = nSim, AgeClasses = AgeClasses, Years = Years)
}
