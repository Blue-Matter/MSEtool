#' `popdynamics` Class
#'
#' Stores population time-series for a single stock across simulations,
#' ages, years, and areas.
#'
#' @slot Number  Number-at-age. List of length `nStock`, each element an array
#'   with dimensions `Sim`, `Age`, `Year`, `Area`.
#' @slot Biomass Total biomass. Array with dimensions `Sim`, `Stock`, `Year`.
#' @slot SBiomass Spawning biomass. Array with dimensions `Sim`, `Stock`, `Year`.
#' @slot SProduction Spawning production. Array with dimensions `Sim`, `Stock`, `Year`.
#' @slot Misc Miscellaneous list for additional outputs.
#'
#' @include class-unions.R
#' @export
#' @name popdynamics-class
#' @aliases popdynamics
setClass("popdynamics",
         slots = c(
           Number      = "array.list.null",
           Biomass     = "array.null",
           SBiomass    = "array.null",
           SProduction = "array.null",
           Misc        = "list"
         )
)