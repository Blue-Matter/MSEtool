#' Spatial
#'
#' Construct a [Spatial()] object defining spatial structure and movement
#' associated with a [Stock()].
#'
#' @param UnfishedDist Numeric or numeric array giving the relative unfished
#'   biomass distribution over areas.
#' @param ProbStaying Numeric or numeric array giving the probability of
#'   remaining in an area.
#' @param RelativeSize Numeric or numeric array giving relative area sizes.
#' @param Movement Numeric array giving movement probabilities among areas.
#' @param FracOther Numeric array defining relative movement among areas when
#'   more than two areas are present.
#' @param Arrangement Numeric matrix defining spatial layout (plotting only).
#' @param CVDist Logit-scale CV penalty applied to `UnfishedDist`.
#' @param CVStay Logit-scale CV penalty applied to `ProbStaying`.
#' @param Misc Miscellaneous list.
#'
#' @details
#' The `Spatial` constructor creates an object describing the spatial
#' distribution and movement dynamics of a stock.
#' 
#' It is only required for operating models that have explicit spatial structure.
#'
#' ## Two Area
#' For operating models with 2 spatial areas, `UnfishedDist` and `ProbStaying`
#' represent the the fraction of the unfished biomass and the probability of remaining
#' in Area 1 respectively.
#'
#' The fraction of unfished biomass in Area 2 is `1-UnfishedDist`. The probability
#' of remaining in Area 2 is calculated by an optimization routine
#' that solves for the specified fraction in Area 1 and the probability of remaining
#' in Area 1.
#'
#' `UnfishedDist` and `ProbStaying` can be one of the following:
#'
#' 1. **Numeric length 1**: Constant across all simulations and age classes;
#' 2. **Numeric length 2**: Lower and upper bounds of a uniform distribution;
#' 3. **Numeric 3D array**: Dimensions `c(nSim, nArea, nAge)`. Movement by age.
#' Must sum to 1 across areas for each simulation and age class.
#' 4. **Numeric 4D array**: Dimensions `c(nSim, nArea, nAge, nTS)`. Movement by age and time step.
#' Must sum to 1 across areas for each simulation, age class, and time step.
#'
#'
#' ## More than Two Areas
#'
#' For operating models with more than 2 spatial areas, `UnfishedDist` must be a
#' numeric array with at least 2 dimensions: `nsim` and `nArea`. It must sum to 1
#' across areas.
#'
#' Add additional dimensions for movement by age (`c(nSim, nArea, nAge)`) and
#' by age and time step (`c(nSim, nArea, nAge, nTS)`).
#'
#' With the exception of `nArea`, each dimension can either be length 1 or length corresponding
#' to the number of simulations, number of age class, and total number of time steps
#' respectively for `nSim`, `nAge`, and `nTS`.
#'
#' `ProbStaying` is the desired probability of staying within each area. The optimization
#' routine solves `ProbStaying` to find the movement matrix that is closted to the
#' specified unfished distribution (`UnfishedDist`; see `?FitMovement` for details).
#'
#' `FracOther` is required for models with more than 2 areas. It must be a numeric
#' array with at least dimensions `c(nSim, nArea, nArea)`, with option for `nAge`
#' and `nTS` dimensions as described above.
#'
#' The diagonal values of `FracOther` (within each simulation, age class, and time step)
#' represent the probability of remaining in an area and therefore should be set to `NA`,
#' as `ProbStaying` is solved as described above.
#'
#' The off-diagonal values represent, for each row, the relative probability of
#' moving from Area in row and column `i` to Area in row `i` and column `j`.
#'
#' ## RelativeSize
#'
#' `RelativeSize` is used for calculating the density of biomass in each area.
#'
#' For 2 area models, `RelativeSize` is the relative size of Area 1. IT should
#' either be a single numeric value, a numeric vector length 2 reperesenting the
#' lower and upper bounds of a uniform distribution, or a numeric vector of length `nSim`.
#'
#' For more than 2 areas, `RelativeSize` should be a numeric vector length `nArea`,
#' or a numeric matrix with dimensions `nSim` by `nArea`.
#'
#' If `RelativeSize` is not specified, all areas are assumed to be the same size.
#'
#' ## Movement
#' `Movement` is calculated internally from `UnfishedDist` and `ProbStaying`. If it is
#' provided, the asymptotic unfished distribution will be calculated and assigned
#' to `UnfishedDist` (over-writing any values that are in `UnfishedDist`).
#' 
#' `Movement` is a numeric array  with dimensions `nSim`, `nArea`, `nArea`, `nAge`, and
#' `nTS`, with each element containing the probability of moving from the first area
#' dimension (row in an `nArea` by `nArea` matrix) to the second area (column in an `nArea` by `nArea` matrix).
#' The last two dimensions `nAge` and `nTS` are optional.
#' 
#'
#' A `Spatial` object can be attached to a [Stock()] using [SetSpatial()] and
#' retrieved using [GetSpatial()].
#'
#' Individual components may be accessed or modified using accessor and
#' replacement functions such as [UnfishedDist()] and [ProbStaying()].
#'
#' @return A [Spatial()] object.
#'
#' @seealso
#' [GetSpatial()], [SetSpatial()],
#' [UnfishedDist()], [ProbStaying()], [Movement()]
#'
#'
#' @export
Spatial <- function(UnfishedDist,
                    ProbStaying = NULL,
                    RelativeSize = NULL,
                    Movement = NULL,
                    FracOther = NULL,
                    Arrangement = NULL,
                    CVDist = 0.1,
                    CVStay = 1,
                    Misc = list()) {
  
  if (missing(UnfishedDist)) {
    object <- methods::new("spatial")
    methods::validObject(object)
    return(object)
  }
  
  object <- methods::new(
    "spatial",
    UnfishedDist = UnfishedDist,
    ProbStaying  = ProbStaying,
    RelativeSize = RelativeSize,
    Movement     = Movement,
    FracOther    = FracOther,
    Arrangement = Arrangement,
    CVDist       = CVDist,
    CVStay       = CVStay,
    Misc         = Misc
  )
  
  methods::validObject(object)
  object
}


#' Spatial accessors and assignment functions
#'
#' Functions for accessing and modifying a [Spatial()] object, and for
#' attaching or retrieving a `Spatial` object from a [Stock()].
#'
#' @param Stock A [Stock()] object.
#' @param x A [Spatial()] object.
#' @param value Replacement value.
#'
#' @details
#' - `GetSpatial()` and `SetSpatial()` retrieve or assign the `Spatial`
#'   component of a [Stock()] object.
#' - Accessors such as `UnfishedDist()` and `ProbStaying()` retrieve
#'   individual components of a [Spatial()] object.
#' - Replacement functions (e.g. `UnfishedDist<-`) update the corresponding
#'   component and validate the object.
#'
#' Conceptual details and valid inputs are documented in [Spatial()].
#'
#' @name Spatial-accessors
NULL


# ---- Stock attachment ----

#' @rdname Spatial-accessors
#' @export
GetSpatial <- function(Stock) {
  CheckClass(Stock, "stock", "Stock")
  Stock@Spatial
}

#' @rdname Spatial-accessors
#' @export
SetSpatial <- function(Stock, Spatial) {
  CheckClass(Stock, "stock", "Stock")
  CheckClass(Spatial, "spatial", "Spatial")
  Stock@Spatial <- Spatial
  methods::validObject(Stock)
  Stock
}


# ---- Slot accessors ----

#' @rdname Spatial-accessors
#' @export
UnfishedDist <- function(x) {
  CheckClass(x, "spatial", "x")
  x@UnfishedDist
}

#' @rdname Spatial-accessors
#' @export
`UnfishedDist<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@UnfishedDist <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial-accessors
#' @export
ProbStaying <- function(x) {
  CheckClass(x, "spatial", "x")
  x@ProbStaying
}

#' @rdname Spatial-accessors
#' @export
`ProbStaying<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@ProbStaying <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial-accessors
#' @export
RelativeSize <- function(x) {
  CheckClass(x, "spatial", "x")
  x@RelativeSize
}

#' @rdname Spatial-accessors
#' @export
`RelativeSize<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@RelativeSize <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial-accessors
#' @export
Movement <- function(x) {
  CheckClass(x, "spatial", "x")
  x@Movement
}

#' @rdname Spatial-accessors
#' @export
`Movement<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@Movement <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial-accessors
#' @export
FracOther <- function(x) {
  CheckClass(x, "spatial", "x")
  x@FracOther
}

#' @rdname Spatial-accessors
#' @export
`FracOther<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@FracOther <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial-accessors
#' @export
Arrangement <- function(x) {
  CheckClass(x, "spatial", "x")
  x@Arrangement
}

#' @rdname Spatial-accessors
#' @export
`Arrangement<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@Arrangement <- value
  methods::validObject(x)
  x
}
