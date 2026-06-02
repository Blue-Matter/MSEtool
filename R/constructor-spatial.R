#' Spatial Constructor and Accessors
#'
#' Construct a [spatial-class] object defining the spatial structure and
#' movement dynamics for a [stock-class], or access and replace the `Spatial`
#' slot of a [stock-class] and its individual slots. `Spatial` is optional —
#' leave it empty for non-spatial (single-area) models.
#'
#' @param UnfishedDist `numeric`, `array`, or `NULL`. Relative unfished biomass
#'   distribution across areas. For two-area models, a single value giving the
#'   fraction in Area 1 (Area 2 is derived as `1 - UnfishedDist`). For
#'   multi-area models, must sum to 1 across areas. Accepted formats:
#'   - Scalar: constant across all simulations, ages, and years.
#'   - Length-2 bounds vector: sampled from `Uniform(lower, upper)` once per
#'     simulation (two-area models only).
#'   - Named array `Sim × Area × Age` or `Sim × Area × Age × Year`: full
#'     age- and time-varying specification.
#'   When `UnfishedDist` is a [stock-class] object, `Spatial()` acts as a
#'   pass-through accessor and returns `x@Spatial`. Default `NULL`.
#' @param ProbStaying `numeric` or `array`. Probability of remaining in an
#'   area between time steps. For two-area models, specifies Area 1 only; the
#'   Area 2 value is solved internally. Accepts the same formats as
#'   `UnfishedDist`. Default `NULL`.
#' @param RelativeSize `numeric`, `array`, `character(1)`, or `NULL`. Relative
#'   size of each area, used for biomass density calculations. For two-area
#'   models, a single value gives the fraction of Area 1 (Area 2 = `1 -
#'   RelativeSize`). For multi-area models, a vector of length `nArea` summing
#'   to 1, or a `Sim × Area` array. The special value `"EqualDensity"` sets
#'   `RelativeSize` equal to the mean `UnfishedDist` across ages and years,
#'   producing constant density across areas regardless of their relative size.
#'   When `NULL` (default), all areas are assumed equal in size.
#' @param Movement `array` or `NULL`. Movement probability matrix with
#'   dimensions `Sim × FromArea × ToArea`, optionally extended to `Sim ×
#'   FromArea × ToArea × Age × Year`. Row `[i, ]` gives the probabilities of
#'   moving from area `i` to each area; rows must sum to 1. When supplied, the
#'   asymptotic unfished distribution is derived from `Movement` and assigned
#'   to `UnfishedDist`, overwriting any existing values. When `NULL` (default),
#'   the movement matrix is fitted during [Populate()] from `UnfishedDist`,
#'   `ProbStaying`, and `FracOther`.
#' @param FracOther `array` or `NULL`. Required for models with more than two
#'   areas. Array with dimensions `Sim × Area × Area`, optionally extended to
#'   include `Age` and `Year` dimensions. Diagonal elements must be `NA`
#'   (staying probabilities are taken from `ProbStaying`). Off-diagonal element
#'   `[i, j]` gives the relative probability of moving from area `i` to area
#'   `j`; values need not sum to 1 as they are normalised internally. Default
#'   `NULL`.
#' @param Arrangement `array` or `NULL`. Spatial layout of areas for plotting
#'   purposes. Reserved for future use; currently stored but not applied.
#'   Default `NULL`.
#' @param CVDist `numeric(1)`. Standard deviation of the log-scale penalty on
#'   the difference between the optimised asymptotic distribution and the
#'   target `UnfishedDist` during multi-area movement optimisation. Larger
#'   values allow the fitted movement matrix to deviate further from the target
#'   distribution. Has no effect for two-area models. Default `0.1`.
#' @param CVStay `numeric(1)`. Standard deviation of the logit-scale penalty
#'   on the difference between the optimised diagonal (staying probability) and
#'   the target `ProbStaying` during multi-area movement optimisation. Larger
#'   values allow more deviation from the target staying probability. Has no
#'   effect for two-area models. Default `1`.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x A [spatial-class] object for slot accessors, or a [stock-class]
#'   object for `Spatial<-`.
#' @param value For `Spatial<-`: a [spatial-class] object. For slot
#'   replacement functions: the replacement value for the corresponding slot.
#'
#' @details
#' ## Default Behaviour for Non-Spatial Models
#'
#' `Spatial` is optional. When the `Spatial` slot of a [stock-class] is empty,
#' [Populate()] creates a default single-area structure automatically — no
#' `Spatial` object needs to be specified for non-spatial models.
#'
#' ## Two-Area Models
#'
#' Specify `UnfishedDist` as the unfished fraction in Area 1 and `ProbStaying`
#' as the probability of remaining in Area 1. [Populate()] solves for the Area
#' 2 movement probabilities numerically so that the asymptotic distribution
#' matches `UnfishedDist` and the Area 1 staying probability matches
#' `ProbStaying`:
#'
#' ```r
#' # Fixed parameters across all simulations
#' sp <- Spatial(UnfishedDist = 0.3,   # 30% of unfished biomass in Area 1
#'               ProbStaying  = 0.6,   # 60% probability of staying in Area 1
#'               RelativeSize = 0.4)   # Area 1 is 40% of total area
#'
#' # Stochastic across simulations
#' sp <- Spatial(UnfishedDist = c(0.2, 0.4),
#'               ProbStaying  = c(0.5, 0.8),
#'               RelativeSize = c(0.3, 0.5))
#' ```
#'
#' ## Age-Varying Movement (Two-Area)
#'
#' Supply `UnfishedDist` as a named `Sim × Area × Age` array. Age-specific
#' movement is fitted independently for each age class:
#'
#' ```r
#' nSim <- 1
#' ages <- Ages(MaxAge = 5)
#' nage <- nAge(ages)
#'
#' # Fish progressively move to Area 2 with age
#' frac2 <- seq(0.1, 0.9, length.out = nage)
#' ud <- array(
#'   c(1 - frac2, frac2),
#'   dim      = c(nSim, 2, nage),
#'   dimnames = list(Sim = 1, Area = 1:2, Age = ages@Classes)
#' )
#' sp <- Spatial(UnfishedDist = ud, ProbStaying = 0.95)
#' ```
#'
#' ## Multi-Area Models (Three or More Areas)
#'
#' `UnfishedDist` must be a `Sim × Area` (or higher-dimensional) array summing
#' to 1 across areas. `FracOther` is required and specifies the relative
#' probability of movement between each pair of areas. `CVDist` and `CVStay`
#' control the penalty weights in the movement optimisation:
#'
#' ```r
#' nArea <- 3
#'
#' ud <- matrix(c(0.5, 0.2, 0.3), nrow = 1, ncol = nArea)
#'
#' # Off-diagonal [i, j] = relative probability of moving from i to j
#' # Diagonal must be NA (staying probability comes from ProbStaying)
#' fo <- array(NA, dim = c(1, nArea, nArea))
#' fo[1, 1, ] <- c(NA, 1,   0.1)
#' fo[1, 2, ] <- c(1,  NA,  1  )
#' fo[1, 3, ] <- c(0.1, 1,  NA )
#'
#' sp <- Spatial(UnfishedDist = ud,
#'               ProbStaying  = c(0.9, 0.2, 0.9),
#'               FracOther    = fo,
#'               RelativeSize = c(0.1, 0.4, 0.5))
#' ```
#'
#' ## Supplying a Movement Matrix Directly
#'
#' When `Movement` is supplied, the asymptotic unfished distribution is derived
#' from it and assigned to `UnfishedDist`, overwriting any existing values.
#' The matrix must have dimensions `Sim × FromArea × ToArea`, with rows
#' summing to 1:
#'
#' ```r
#' mov <- array(
#'   c(0.7, 0.3,
#'     0.2, 0.8),
#'   dim      = c(1, 2, 2),
#'   dimnames = list(Sim = 1, FromArea = 1:2, ToArea = 1:2)
#' )
#' sp <- Spatial(Movement = mov)
#' ```
#'
#' ## Equal Density Across Areas
#'
#' Setting `RelativeSize = "EqualDensity"` assigns `RelativeSize` equal to the
#' mean `UnfishedDist` across ages and years, so that biomass density is
#' constant across areas regardless of their relative size. Useful when areas
#' are defined by biomass share rather than physical extent:
#'
#' ```r
#' sp <- Spatial(UnfishedDist = 0.3,
#'               ProbStaying  = 0.7,
#'               RelativeSize = "EqualDensity")
#' ```
#'
#' ## Pass-Through Access from a Stock
#'
#' When `UnfishedDist` is a [stock-class] object, `Spatial()` returns the
#' `Spatial` slot directly:
#'
#' ```r
#' Spatial(my_stock)             # returns my_stock@Spatial
#' Spatial(my_stock) <- my_sp   # replaces my_stock@Spatial
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' UnfishedDist(sp)  <- 0.4
#' ProbStaying(sp)   <- 0.7
#' RelativeSize(sp)  <- 0.5
#' Movement(sp)      <- my_mov
#' FracOther(sp)     <- my_fo
#' CVDist(sp)        <- 0.05
#' CVStay(sp)        <- 0.5
#' ```
#'
#' @return
#' - `Spatial()` returns a [spatial-class] object. If `UnfishedDist` is a
#'   [stock-class], returns `x@Spatial`.
#' - `Spatial<-` returns the [stock-class] `x` with the `Spatial` slot
#'   replaced and the object re-validated.
#' - `UnfishedDist()`, `ProbStaying()`, `RelativeSize()`, `Movement()`,
#'   `FracOther()`, `Arrangement()`, `CVDist()`, `CVStay()` return the value
#'   of the corresponding slot from `x`.
#' - All replacement variants return `x` with the named slot updated and the
#'   object re-validated.
#'
#' @seealso
#' - [spatial-class] for the class definition and slot-level documentation.
#' - [FitMovement()] for fitting movement matrix.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#'
#' @family spatial
#'
#' @example man-examples/class-Spatial.R
#'
#' @export
Spatial <- function(UnfishedDist = NULL,
                    ProbStaying = NULL,
                    RelativeSize = NULL,
                    Movement = NULL,
                    FracOther = NULL,
                    Arrangement = NULL,
                    CVDist = 0.1,
                    CVStay = 1,
                    Misc = list()) {
  
  
  if (inherits(UnfishedDist, 'stock'))
    return(UnfishedDist@Spatial)
  
  object <- methods::new(
    "spatial",
    UnfishedDist = UnfishedDist,
    ProbStaying  = ProbStaying,
    RelativeSize = RelativeSize,
    Movement     = Movement,
    FracOther    = FracOther,
    Arrangement  = Arrangement,
    CVDist       = CVDist,
    CVStay       = CVStay,
    Misc         = Misc
  )
  
  methods::validObject(object)
  object
}



#' @rdname Spatial
#' @export
UnfishedDist <- function(x) {
  CheckClass(x, "spatial", "x")
  x@UnfishedDist
}

#' @rdname Spatial
#' @export
`UnfishedDist<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@UnfishedDist <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial
#' @export
ProbStaying <- function(x) {
  CheckClass(x, "spatial", "x")
  x@ProbStaying
}

#' @rdname Spatial
#' @export
`ProbStaying<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@ProbStaying <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial
#' @export
RelativeSize <- function(x) {
  CheckClass(x, "spatial", "x")
  x@RelativeSize
}

#' @rdname Spatial
#' @export
`RelativeSize<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@RelativeSize <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial
#' @export
Movement <- function(x) {
  CheckClass(x, "spatial", "x")
  x@Movement
}

#' @rdname Spatial
#' @export
`Movement<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@Movement <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial
#' @export
FracOther <- function(x) {
  CheckClass(x, "spatial", "x")
  x@FracOther
}

#' @rdname Spatial
#' @export
`FracOther<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@FracOther <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial
#' @export
Arrangement <- function(x) {
  CheckClass(x, "spatial", "x")
  x@Arrangement
}

#' @rdname Spatial
#' @export
`Arrangement<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@Arrangement <- value
  methods::validObject(x)
  x
}


#' @rdname Spatial
#' @export
CVDist <- function(x) {
  CheckClass(x, "spatial", "x")
  x@CVDist
}

#' @rdname Spatial
#' @export
`CVDist<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@CVDist <- value
  methods::validObject(x)
  x
}

#' @rdname Spatial
#' @export
CVStay <- function(x) {
  CheckClass(x, "spatial", "x")
  x@CVStay
}

#' @rdname Spatial
#' @export
`CVStay<-` <- function(x, value) {
  CheckClass(x, "spatial", "x")
  x@CVStay <- value
  methods::validObject(x)
  x
}



#' @rdname Spatial
#' @export
`Spatial<-` <- function(x, value) {
  CheckClass(x, "stock", "x")
  x@Spatial <- value
  methods::validObject(x)
  x
}




