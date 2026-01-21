

#' Population Dynamics Object
#' 
#' An S4 Object for storing population time-series information
#' 
#' @slot Number Number-at-Age: list of length [nStock()] with each element containing an
#' array with dimensions Sim, Age, and Year
#' @slot Biomass Total Biomass: array with dimensions Sim, Stock, Year
#' @slot SBiomass Spawning Biomass: array with dimensions Sim, Stock, Year
#' @slot SProduction Spawning Production: array with dimensions Sim, Stock, Year
#' @slot Misc Miscellanous list
#' @name PopDynamics  
#' @include class-unions.R
#' @export
setClass("popdynamics",
         slots=c(Number='array.list.null',
                 Biomass='array.list.null',
                 SBiomass='array.list.null',
                 SProduction='array.list.null',
                 Misc='list'
         )
)


#' `timeseries` Object
#'
#' Stores detailed simulated time-series outputs including population,
#' catch, effort, and fishing mortality components.
#'
#' @slot Number List of number-at-age arrays (Sim × Age × Year × Area), one per stock.
#' @slot Biomass Total biomass array (Sim × Stock × Year).
#' @slot SBiomass Spawning biomass array (Sim × Stock × Year).
#' @slot SProduction Spawning production array (Sim × Stock × Year).
#'
#' @slot LandingsAtAge List of landings-at-age arrays (Sim × Age × Year × Fleet × Area).
#' @slot DiscardsAtAge List of discards-at-age arrays (Sim × Age × Year × Fleet × Area).
#' @slot LandingsAtSize List of landings-at-size arrays (Sim × Class × Year × Fleet × Area).
#' @slot DiscardsAtSize List of discards-at-size arrays (Sim × Class × Year × Fleet × Area).
#'
#' @slot Effort Effort array (Sim × Year × Fleet).
#' @slot Distribution Fleet effort distribution by area (Sim × Year × Fleet × Area).
#'
#' @slot FDead Fishing mortality causing death (list of arrays by stock).
#' @slot FRetain Fishing mortality retained (list of arrays by stock).
#' @slot FDeadArea Area-specific fishing mortality causing death (list by stock).
#' @slot FRetainArea Area-specific retained fishing mortality (list by stock).
#'
#' @slot Misc Miscellaneous list.
#'
#' @export
setClass(
  "timeseries",
  slots = c(
    Number          = "list",
    Biomass         = "array",
    SBiomass        = "array",
    SProduction     = "array",
    
    LandingsAtAge   = "list",
    DiscardsAtAge   = "list",
    LandingsAtSize  = "list",
    DiscardsAtSize  = "list",
    
    Effort          = "array",
    Distribution    = "array",
    
    FDead           = "array.list.null",
    FRetain         = "array.list.null",
    FDeadArea       = "array.list.null",
    FRetainArea     = "array.list.null",
    
    Misc            = "list"
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

' `refpointsMSY` Object
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