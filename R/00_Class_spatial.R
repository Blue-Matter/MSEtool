

## Spatial ----

#' Spatial Object
#'
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' The `Spatial` function is used to create S4 class `spatial` objects or to
#' access or assign `spatial` objects to [Stock()] class objects
#'
#' @details
#' ## About the `spatial` Class
#' Objects of class `spatial` contain information relating to spatial distribution
#' and movement of the stock.
#'
#' `Spatial` is only required for spatial operating models. Leave it empty for non-spatial models.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('spatial')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('spatial')`
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
#' @slot UnfishedDist Numeric or numeric array. The relative distribution of the stock over areas.
#' See `Details`.
#' @slot ProbStaying Numeric or numeric array. The probability of remaining in an area in a given time step. See `Details`.#'
#' @slot RelativeSize Numeric or numeric array. The relative size of each area. See `RelativeSize` in `Details`.
#' @slot Movement A numeric matrix or array. See `Movement` in `Details`.
#' @slot FracOther See `More than Two Areas` in `Details`.
#' @slot Arrangement A numeric matrix with the layout ofthe areas. Used for plotting only.
#' @slot CVDist The logit CV associated with `UnfishedDist` (used as a penalty when optimizing for `UnfishedDist`). See `?FitMovement` for details.
#' @slot CVStay The logit CV associated with `ProbStaying` (used as a penalty when optimizing for diagonal (`ProbStaying`)). See `?FitMovement` for details.
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('spatial', c('CalcMovement')`
#' @name Spatial
#' @rdname Spatial
#' @docType class
#' @example man-examples/Spatial-class.R
#' @export
setClass('spatial',
         slots=c(UnfishedDist='num.array',
                 ProbStaying='num.array',
                 RelativeSize='num.array.char.null',
                 Movement='array.list.null',
                 FracOther='array.list.null',
                 Arrangement='array.list.null',
                 CVDist='numeric',
                 CVStay='numeric',
                 Misc='list'
         )
)


setValidity('spatial', isValidObject)

setMethod("initialize", "spatial", function(.Object,
                                            UnfishedDist=NULL,
                                            ProbStaying=NULL,
                                            RelativeSize=NULL,
                                            Movement=NULL,
                                            FracOther=NULL,
                                            CVDist=0.1,
                                            CVStay=1,
                                            Misc=list()) {
  .Object@RelativeSize <- RelativeSize
  .Object@ProbStaying <- ProbStaying
  .Object@UnfishedDist <- UnfishedDist
  .Object@Movement <- Movement
  .Object@FracOther <- FracOther
  .Object@CVDist <- CVDist
  .Object@CVStay <- CVStay
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})


#' @describeIn Spatial Create a new `spatial` class object
#' @param UnfishedDist Numeric or numeric array. The relative distribution of the stock over areas.
#' See `Details`.
#' @param ProbStaying Numeric or numeric array. The probability of remaining in an area in a given time step. See `Details`.#'
#' @param RelativeSize Numeric or numeric array. The relative size of each area. See `RelativeSize` in `Details`.
#' @param Movement A numeric matrix or array. See `Movement` in `Details`.
#' @param FracOther See `More than Two Areas` in `Details`.
#' @param Arrangement A numeric matrix with the layout ofthe areas. Used for plotting only.
#' @param CVDist The logit CV associated with `UnfishedDist` (used as a penalty when optimizing for `UnfishedDist`). See `?FitMovement` for details.
#' @param CVStay The logit CV associated with `ProbStaying` (used as a penalty when optimizing for diagonal (`ProbStaying`)). See `?FitMovement` for details.
#' @param Misc `r Misc_param()`
#' @export
Spatial <- function(UnfishedDist=NULL,
                    ProbStaying=NULL,
                    RelativeSize=NULL,
                    Movement=NULL,
                    FracOther=NULL,
                    CVDist=0.1,
                    CVStay=1,
                    Misc=list()) {
  if (methods::is(UnfishedDist, 'stock'))
    return(UnfishedDist@Spatial)
  
  methods::new('spatial',
               UnfishedDist=UnfishedDist,
               ProbStaying=ProbStaying,
               RelativeSize=RelativeSize,
               Movement=Movement,
               FracOther=FracOther,
               CVDist=CVDist,
               CVStay=CVStay,
               Misc=Misc)
}


#' @describeIn Spatial Assign a `spatial` class object to a [Stock()] object
#' @param x A [Stock()] object
#' @param value A `spatial` object to assign to `x`
#' @export
`Spatial<-` <- function(x, value) {
  assignSlot(x, value, 'Spatial')
}

