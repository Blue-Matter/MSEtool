#' Spatial
#'
#' Construct and manipulate a [spatial-class] object defining the spatial
#' structure and movement dynamics associated with a [Stock()] object. A
#' `Spatial` object is only required for operating models with explicit spatial
#' structure (`nArea > 1`).
#'
#' @param UnfishedDist Numeric scalar, vector, or array. Relative unfished
#'   biomass distribution over areas. If `UnfishedDist` is a [stock-class]
#'   object, the `Spatial` slot of that stock is returned. See `Details` for
#'   required structure by number of areas.
#' @param ProbStaying Numeric scalar, vector, or array. Probability of
#'   remaining in an area between time steps. See `Details` for required
#'   structure by number of areas.
#' @param RelativeSize Numeric scalar, vector, or array. Relative size of each
#'   area, used for calculating biomass density. If not specified, all areas
#'   are assumed equal in size. See `Details` for required structure by number of
#'   areas.
#' @param Movement Numeric array. Movement probability matrix with dimensions
#'   `nSim x nArea x nArea`, with optional additional `nAge` and `nTS`
#'   dimensions. Each element gives the probability of moving from the area in
#'   the first area dimension (row) to the area in the second area dimension
#'   (column). If provided, `UnfishedDist` is overwritten with the asymptotic
#'   unfished distribution derived from `Movement`. Default `NULL`.
#' @param FracOther Numeric array. Required for models with more than 2 areas.
#'   Defines the relative probability of movement among areas, with dimensions
#'   `nSim x nArea x nArea` plus optional `nAge` and `nTS` dimensions.
#'   Diagonal elements (probability of staying) must be set to `NA` as they
#'   are solved from `ProbStaying`. Off-diagonal element `[i, j]` gives the
#'   relative probability of moving from area `i` to area `j`. Default `NULL`.
#' @param Arrangement Numeric matrix. Spatial layout of areas for plotting
#'   purposes only. Not currently used. Default `NULL`.
#' @param CVDist Numeric. Logit-scale coefficient of variation applied to
#'   `UnfishedDist` to generate simulation variability. Default `0.1`.
#' @param CVStay Numeric. Logit-scale coefficient of variation applied to
#'   `ProbStaying` to generate simulation variability. Default `1`.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' @param x A [spatial-class] object for slot accessors, or a [stock-class]
#'   object for `Spatial<-`.
#' @param value For `Spatial<-`: a [spatial-class] object. For slot
#'   replacement functions: the new value for the corresponding slot.
#'
#' @details
#' A `Spatial` object is only required for operating models with more than one
#' area (`nArea > 1`). For single-area models it does not need to be specified.
#'
#' ## Two-Area Models
#'
#' For two-area models, `UnfishedDist` and `ProbStaying` represent the
#' unfished biomass fraction and probability of remaining in Area 1
#' respectively. The Area 2 values are derived automatically: the unfished
#' fraction in Area 2 is `1 - UnfishedDist`, and the probability of remaining
#' in Area 2 is solved via an optimisation routine that matches the specified
#' Area 1 distribution and retention probability.
#'
#' `UnfishedDist` and `ProbStaying` may be specified as:
#' 1. **Numeric length 1**: constant across all simulations and age classes.
#' 2. **Numeric length 2**: lower and upper bounds of a uniform distribution
#'    sampled across simulations.
#' 3. **3D array** `c(nSim, nArea, nAge)`: movement varying by age. Must sum
#'    to 1 across areas within each simulation and age class.
#' 4. **4D array** `c(nSim, nArea, nAge, nTS)`: movement varying by age and
#'    time step. Must sum to 1 across areas within each simulation, age class,
#'    and time step.
#'
#' For arrays, each dimension except `nArea` may be length 1 (replicated
#' internally) or match `nSim`, `nAge`, or `nTS` respectively.
#'
#' ## More Than Two Areas
#'
#' For models with more than two areas, `UnfishedDist` must be a numeric array
#' with at least dimensions `c(nSim, nArea)`, summing to 1 across areas.
#' Additional `nAge` and `nTS` dimensions may be included as described above.
#'
#' `FracOther` is required for models with more than two areas. It must have
#' at least dimensions `c(nSim, nArea, nArea)`. Diagonal elements must be set
#' to `NA` as the probability of staying is solved from `ProbStaying`.
#' Off-diagonal element `[i, j]` gives the relative probability of moving from
#' area `i` to area `j`.
#'
#' ## RelativeSize
#'
#' For two-area models, `RelativeSize` is the relative size of Area 1 and may
#' be a single numeric value, a length-2 vector of uniform distribution bounds,
#' or a numeric vector of length `nSim`. For models with more than two areas,
#' `RelativeSize` should be a numeric vector of length `nArea` or a matrix
#' with dimensions `nSim x nArea`. If not specified, all areas are assumed
#' equal in size.
#'
#' ## Movement Matrix
#'
#' `Movement` is normally calculated internally from `UnfishedDist` and
#' `ProbStaying`. If supplied directly, the asymptotic unfished distribution
#' is derived from `Movement` and assigned to `UnfishedDist`, overwriting any
#' existing values. `Movement` must have dimensions `nSim x nArea x nArea`,
#' with optional additional `nAge` and `nTS` dimensions.
#'
#' ## Attaching to a Stock
#'
#' A `Spatial` object can be attached to a [Stock()] with
#' `Spatial(Stock) <- MySpatial` and retrieved with `Spatial(Stock)`.
#'
#' Individual slots may be accessed or modified using [UnfishedDist()],
#' [ProbStaying()], [RelativeSize()], [Movement()], [FracOther()],
#' [Arrangement()], [CVDist()], and [CVStay()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Spatial()` returns a [spatial-class] object. If `UnfishedDist` is a
#'   [stock-class] object, the `Spatial` slot of that stock is returned.
#' - `Spatial<-` returns `x` with the `Spatial` slot replaced.
#' - Slot accessors return the value of the corresponding slot from `x`.
#' - Slot replacement functions return `x` with the corresponding slot
#'   updated.
#'
#' @seealso [spatial-class], [Stock()]
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




