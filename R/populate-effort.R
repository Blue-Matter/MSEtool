#' Populate an Effort Object
#'
#' Populate an `Effort` object by generating historical effort,
#' and populating distribution and targeting across areas and simulation replicates.
#'
#' @param Effort An [effort-class] object to populate.
#' @param HistYears Numeric vector of historical years to populate effort for.
#' @param ProjYears Numeric vector of projected years. Used to extend
#'   `TripsScalar`/`AnglerPerTrip`/`Theta` (if supplied) to cover the full
#'   model time series; not used for `Effort@Effort` itself.
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
#' * If supplied, extends `TripsScalar`/`AnglerPerTrip`/`Theta` to cover
#'   historical and projected years (a bare scalar, length-2 uniform
#'   bounds, or length-`nSim` vector for `Theta` is first resolved to a
#'   single-year `Sim x Year` array, then replicated across all years).
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
                           ProjYears = NULL,
                           nArea = 1,
                           nSim = 5,
                           seed = NULL) {

  .SetSeed(seed)

  nSim  <- .GetNSim(Effort, nSim)

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

  Effort@Distribution <- .PopulateDistribution(
    Distribution = Effort@Distribution,
    nSim = nSim,
    HistYears = HistYears,
    nArea = nArea
  )

  Effort@Targeting <- .PopulateTargeting(
    Targeting = Effort@Targeting,
    nSim = nSim,
    Years = HistYears
  )


  if (is.null(Effort@Mode))
    Effort@Mode <- 'Density'

  if (!Effort@Mode %in% c('Biomass', 'Density'))
    cli::cli_abort("`Mode` must be either `Density` or `Biomass`")

  Years <- c(HistYears, ProjYears)
  Effort@TripsScalar   <- .PopulateEffortTrips(Effort@TripsScalar, nSim, Years, HistYears, "TripsScalar")
  Effort@AnglerPerTrip <- .PopulateEffortTrips(Effort@AnglerPerTrip, nSim, Years, HistYears, "AnglerPerTrip")
  Effort@Theta         <- .PopulateTheta(Effort@Theta, nSim, HistYears)

  Effort
}

# Normalises a Sim x Year effort-behaviour array (TripsScalar/AnglerPerTrip):
# wraps a bare scalar into a 1x1 array, validates the Sim dimension is 1 or
# nSim, and extends the Year dimension to cover HistYears + ProjYears.
.PopulateEffortTrips <- function(x, nSim, Years, HistYears, label) {
  if (is.null(x)) return(x)

  dd <- dim(x)
  if (is.null(dd)) {
    x <- array(x, dim = c(1, 1), dimnames = list(Sim = 1, Year = HistYears[1]))
    dd <- dim(x)
  }

  if (dd[1] != nSim && dd[1] != 1)
    cli::cli_abort(c(
      "x" = "Incorrect number of rows in `Fleet |> Effort() |> {label}()`",
      "i" = "Must have either 1 row or `nSim` ({.val {nSim}}) rows."
    ))

  if (is.null(dimnames(x)))
    dimnames(x) <- list(Sim = 1:nrow(x), Year = Years[1:ncol(x)])

  ExtendYears(x, Years = Years)
}

# Resolves Theta to a Sim x Year array, the same way every other
# time-varying model parameter (Maturity@Pars, Weight@Pars, etc.) is
# structured - see .StructurePar(). A bare scalar, length-2 uniform
# bounds, or length-nSim vector all collapse to a single Year column named
# with the first historical year; a Sim x Year array supplied directly is
# respected as-is (dimnames assigned if missing). No explicit year
# extension here - like every other structured OM parameter, the Year
# dimension is expanded to the full historical + projection time series
# later, by .ExtendHist()/.ExtendOM(), which recurses over every array in
# `OM@Fleet` (including `Effort@Theta`) at the start of Project().
.PopulateTheta <- function(x, nSim, HistYears) {
  if (is.null(x)) return(x)
  .StructurePar(x, nSim = nSim, Years = HistYears[1])
}



.PopulateTargeting <- function(Targeting, nSim, Years) {
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
    
    if ((dd[2] != 1 && dd[2] != length(Years)) & is.null(dnames[['Year']])) {
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

.PopulateDistribution <- function(Distribution,
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
    .CheckClass(Distribution, c("array", "matrix"))
    dd <- dim(Distribution)
    # TODO check dimensions
    # TODO add dimnames if needed
  }
  
  Distribution
}
