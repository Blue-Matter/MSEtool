#' Effort Class and Constructor
#'
#' The [Effort()] class stores historical fishing effort and
#' associated spatial structure for [Fleet()] object. Effort may be supplied
#' directly as an array or generated stochastically from a data frame.
#'
#' @section Class slots:
#'
#' \describe{
#'   \item{\code{Effort}}{
#'     Numeric array of fishing effort with dimensions \code{nSim x nYear}, or a
#'     \code{data.frame} describing a stochastic historical effort process (see `Details`).
#'   }
#'   \item{\code{Units}}{
#'     Optional character string describing the units of effort
#'     (e.g., "days", "trips", "kW-days"). Used only for reporting.
#'   }
#'   \item{\code{Distribution}}{
#'     Optional numeric array with dimensions \code{nSim x nYear x nArea}
#'     giving the fraction of total effort allocated to each spatial area. Only
#'     applicable for spatial operating models. 
#'     If not supplied, `Distribution` will be derived internally from
#'     \code{Targeting} and spatial utility calculations.
#'   }
#'   \item{\code{Targeting}}{
#'     Numeric array or scalar controlling spatial targeting behavior.
#'     Higher values imply stronger concentration of effort in high-utility areas.
#'     Values are commonly around 0.8–1.0. If left empty, a default value of 0.8 
#'     is assumed. 
#'     If `Targeting` is a numeric vector length 2, `nSim` values will be sampled
#'     from a uniform distribution with bounds `range(Targeting)`.
#'   }
#'   \item{\code{Maximum}}{
#'     Numeric array giving the maximum possible fishing effort by simulation
#'     and year. If supplied, effort below this level represents latent capacity.
#'     Currently not enforced in the operating model but reserved for
#'     bioeconomic extensions.
#'   }
#'   \item{\code{Misc}}{
#'     A named list of additional objects carried with the effort class.
#'   }
#' }
#' 
#' @param Effort Numeric array or \code{data.frame} describing fishing effort (see `Details`).
#' @param Units Character string giving effort units.
#' @param Distribution Optional spatial effort distribution array.
#' @param Targeting Spatial targeting parameter or array.
#' @param Maximum Optional maximum effort array.
#' @param Misc List of miscellaneous objects.
#'
#' @param x A [Fleet()] object.
#' @param value An [Effort()] object to assign.
#' 
#' @details
#' Effort represents total fishing activity prior to spatial allocation.
#' 
#' When \code{Distribution} is supplied, effort is partitioned explicitly
#' across areas. Otherwise, spatial allocation is derived internally
#' using spatial utility, targeting strength, and fleet behavior.
#' 
#' If `Effort` is a correctly structured `data.frame`, it will be used by 
#' [GenerateHistoricalEffort()] to produce stochastic historical fishing effort.
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' 
#' @seealso [GenerateHistoricalEffort()]
#'
#' @name Effort
#' @export
setClass("effort",
         slots=c(
           Effort='num.array.df',
           Units='char.null',
           Distribution='num.array',
           Targeting='num.array', # currently fixed to 1
           Maximum='num.array' # TODO - not currently used. Maximum fishing effort - Effort < Maximum is latent effort. Increasing Maximum requires Investment (see Bioeconomic)
         ),
         contains='MiscClass'
)

setValidity('effort', isValidObject)


setMethod("initialize", "effort", function(.Object,
                                           Effort=NULL,
                                           Units='',
                                           Distribution=NULL,
                                           Targeting=NULL,
                                           Maximum=NULL,
                                           Misc=list()) {
  
  .Object@Effort <- Effort
  .Object@Units <- Units
  .Object@Distribution <- Distribution
  .Object@Targeting <- Targeting
  .Object@Maximum <- Maximum
  .Object@Misc <- Misc
  .Object
})



#' @rdname Effort
#' @export
Effort <- function(Effort=NULL,
                   Units='',
                   Distribution=NULL,
                   Targeting=NULL,
                   Maximum=NULL,
                   Misc=list()) {
  
  if (inherits(Effort,'fleet')) {
    return(Effort@Effort)
  }
    
  methods::new('effort',
               Effort=Effort,
               Units=Units,
               Distribution=Distribution,
               Targeting=Targeting,
               Maximum=Maximum,
               Misc=Misc)
}


#' @rdname Effort
#' @export
`Effort<-` <- function(x, value) {
  assignSlot(x, value, 'Effort')
}
