
#' Population Dynamics Object
#' 
#' An S4 Object for storing population time-series information
#' 
#' @slot Number Number-at-Age: list of length`nStock` with each element containing an
#' array with dimensions `Sim`, `Age`, `Year`, and `Area`
#' @slot Biomass Total Biomass: array with dimensions `Sim`, `Stock`, `Year`
#' @slot SBiomass Spawning Biomass: array with dimensions `Sim`, `Stock`, `Year`
#' @slot SProduction Spawning Production: array with dimensions `Sim` `Stock`, `Year`
#' @slot Misc Miscellanous list
#' @name PopDynamics  
#' @include class-unions.R
#' @export
setClass("popdynamics",
         slots=c(Number='array.list.null',
                 Biomass='array.null',
                 SBiomass='array.null',
                 SProduction='array.null',
                 Misc='list'
         )
)






#' `unfished` Object
#'
#' Stores unfished equilibrium and dynamic population states.
#'
#' @slot Equilibrium Unfished equilibrium population dynamics.
#' @slot Dynamic Unfished dynamic population dynamics.
#' @slot Misc Miscellaneous list.
#'
#' @export
setClass(
  "unfished",
  slots = c(
    Equilibrium = "popdynamics",
    Dynamic     = "popdynamics",
    Misc        = "list"
  )
)

#' `refpointsMSY` Object
#'
#' Stores MSY-based biological reference points.
#'
#' @slot FMSY Fishing mortality at MSY.
#' @slot BMSY Biomass at MSY.
#' @slot SBMSY Spawning biomass at MSY.
#' @slot SPMSY Spawning production at MSY.
#' @slot SPRMSY Spawning potential ratio at MSY.
#' @slot MSY Maximum sustainable yield.
#' @slot MSYLandings Landings at MSY.
#' @slot Misc Miscellaneous list.
#'
#' @export
setClass(
  "refpointsMSY",
  slots = c(
    FMSY         = "num.array.null",
    BMSY         = "num.array.null",
    SBMSY        = "num.array.null",
    SPMSY        = "num.array.null",
    SPRMSY       = "num.array.null",
    MSY          = "num.array.null",
    MSYLandings  = "num.array.null",
    Misc         = "list"
  )
)