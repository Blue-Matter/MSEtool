
## Depletion ----

#' Depletion Object
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#'
#' The `Depletion` function is used to create S4 class `depletion` objects or to access or
#' assign `depletion` objects to [Stock()] class objects
#'
#' @details
#' ## About the `depletion` Class
#' `depletion` is an S4 class used in [Stock()] class objects. It describes the
#' `Initial` depletion (at beginning of historical period) and the `Final` depletion
#' (at the end of the historical period).
#'
#' Depletion is defined as stock biomass divided by the unfished biomass; i.e.,
#' the lower the depletion value the lower the stock's biomass is related to it's
#' unfished level.
#'
#' ### `Initial`
#'
#' The stock at the beginning of the historical period is assumed to be in an
#' unfished state unless `Initial` is populated.
#'
#' When `Initial` is populated, an optimization routine adjusts the mean
#' recruitment deviations for the initial age classes such that the biomass relative
#' to `Reference` in the first time step is equal to `Initial`.
#'
#' ### `Final`
#'
#' `Final` is used to specify the depletion level in the last historical time step.
#' It is only required if the `Catchability` slot (`q`) in [FishingMortality()] is
#' not populated. In this case, an optimization routine will calculate the `q`
#' value for each simulation that results in the biomass relative to `Reference`
#' in the last historical time step to be equal to `Final`.
#'
#' ### `Reference`
#'
#' `Reference` describes the reference point used to calculate depletion.
#'
#' It can be a character string of either `B0` (default) or `BMSY`,
#' where `B0` is the equilibrium unfished biomass and `BMSY` the equilibrium
#' biomass corresponding the maximum sustainable yield.
#'
#' How are `B0` and `BMSY` calculated? Good question. That hasn't been documented
#' yet. Bug us to update the documentation!
#'
#' It can also be a numeric matrix with dimensions `(nSim, 2)` where the first
#' column is the absolute value for `Initial` and the second column the absolute
#' value for `Final`. Like all objects, the number of rows can be `nSim` if `Initial`
#' or `Final` vary over simulations, or otherwise a value of `1`.
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('depletion')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('depletion')`
#'
#' @slot Initial A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the first historical time step. See `Details`.
#' @slot Final A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the last historical time step.  See `Details`.
#' @slot Reference The reference point used to calculate Depletion.  See `Details`.
#' @slot Misc `r Misc_param()`
#'
#' @seealso `r See_Also('depletion')`
#'
#' @name Depletion
#' @rdname Depletion
#' @docType class
#' @example man-examples/Depletion-class.R
#'
#' @export
setClass('depletion',
         slots=c(Initial='num.array',
                 Final='num.array',
                 Reference='array.char.null'),
         contains = c('MiscClass')
)

setValidity('depletion', isValidObject)

setMethod("initialize", "depletion", function(.Object,
                                              Initial=numeric(),
                                              Final=numeric(),
                                              Reference='B0') {
  .Object@Initial <- Initial
  .Object@Final <- Final
  .Object@Reference <- Reference
  #   .Object@Created <- Sys.time()
  .Object
})


#' @describeIn Depletion Create a new `Depletion` object
#' @param Initial A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the first historical time step. See `Details`.
#' @param Final A numeric of length `nSim` or length 1 specifying the biomass
#'  relative to `Reference` in the last historical time step.  See `Details`.
#' @param Reference The reference point used to calculate Depletion.  See `Details`.
#' @export
Depletion <- function(Initial=numeric(),
                      Final=numeric(),
                      Reference='B0') {
  
  if (methods::is(Initial, 'stock'))
    return(Initial@Depletion)
  
  .Object <- methods::new('depletion',
                          Initial=Initial,
                          Final=Final,
                          Reference=Reference)
  
  validObject(.Object)
  .Object
}

#' @describeIn Depletion Assign an `Depletion` object to a [Stock()] object
#' @param x A [Stock()] class object
#' @param value An `depletion` class object to assign to `x`
#' @export
`Depletion<-` <- function(x, value) {
  assignSlot(x, value, 'Depletion')
}



