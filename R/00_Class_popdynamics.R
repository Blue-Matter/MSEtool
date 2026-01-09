

#' Population Dynamics Object
#' 
#' An S4 Object for storing population time-series information
#' 
#' @slot Number Number-at-Age: list of length [nStock()] with each element containing an
#' array with dimensions Sim, Age, and Year
#' @slot Biomass Total Biomass: array with dimensions Sim, Stock, Year
#' @slot SBiomass Spawning Biomass: array with dimensions Sim, Stock, Year
#' @slot SProduction Spawning Production: array with dimensions Sim, Stock, Year
#' @slot Misc `r Misc_param()`  
#' @name PopDynamics  
#' @include 00_Class_unions.R
#' @export
setClass("popdynamics",
         slots=c(Number='array.list.null',
                 Biomass='array.list.null',
                 SBiomass='array.list.null',
                 SProduction='array.list.null',
                 Misc='list'
         )
)