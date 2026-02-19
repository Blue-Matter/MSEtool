#' `Timeseries` Class
#'
#' Stores detailed simulated time-series outputs including population,
#' catch, effort, and fishing mortality components.
#'
#' @slot Number List of number-at-age arrays Stock: Sim × Age × Year × Area (x MP for `MSE` objects).
#' @slot Biomass Total biomass array(Sim × Stock × Year (x MP for `MSE` objects).
#' @slot SBiomass Spawning biomass array Sim × Stock × Year (x MP for `MSE` objects).
#' @slot SProduction Spawning production array Sim × Stock × Year (x MP for `MSE` objects).
#' 
#' @slot Interactions Total interaction by Stock and Fleet (biomass). Array: Sim x Stock x Year x Fleet (x MP for `MSE` objects).
#' @slot Landings Total landings by Stock and Fleet (biomass). Array: Sim x Stock x Year x Fleet (x MP for `MSE` objects).
#' @slot Discards Total discards by Stock and Fleet (biomass). Array: Sim x Stock x Year x Fleet (x MP for `MSE` objects).
#' 
#' @slot InteractAtAge List of interactions-at-age (numbers) arrays. Stock: Sim × Age × Year × Fleet × Area  (x MP for `MSE` objects).
#' @slot LandingsAtAge List of landings-at-age (numbers) arrays. Stock: Sim × Age × Year × Fleet × Area  (x MP for `MSE` objects).
#' @slot DiscardsAtAge List of discards-at-age (numbers) arrays. Stock: Sim × Age × Year × Fleet × Area (x MP for `MSE` objects).
#' 
#' @slot LandingsAtSize Nested list of landings-at-size (numbers) arrays Stock, Fleet: Sim × Class × Year × Area (x MP for `MSE` objects). 
#' @slot DiscardsAtSize Nested list of discards-at-size (numbers) arrays Stock, Fleet: Sim × Class × Year × Area (x MP for `MSE` objects).
#'
#' @slot Effort Effort array Sim × Year × Fleet (x MP for `MSE` objects).
#' @slot Distribution Fleet effort distribution by area: Sim × Year × Fleet × Area (x MP for `MSE` objects).
#'
#' @slot FInteract Apical fishing mortality for fish that interact with the fishing gear.  Array: Sim x Stock x Year x Fleet (x MP for `MSE` objects).'
#' @slot FDead Apical fishing mortality for fish that are killed by fishing gear.  Array: Sim x Stock x Year x Fleet (x MP for `MSE` objects). 
#' @slot FRetain Apical fishing mortality for fish that are retained by fishers. Array: Sim x Stock x Year x Fleet (x MP for `MSE` objects).
#' 
#' @slot FInteractArea Area-specific fishing mortality-at-age for fish that interact with the fishing gear (list by stock). Stock: Sim x Age x Year x Fleet x Area (x MP for `MSE` objects).
#' @slot FDeadArea Area-specific fishing mortality-at-age of fish killed by fishing gear (list by stock). Stock: Sim x Age x Year x Fleet x Area (x MP for `MSE` objects).
#' @slot FRetainArea Area-specific fishing mortality-at-age of retained fish (list by stock). Stock: Sim x Age x Year x Fleet x Area (x MP for `MSE` objects).
#'
#' @slot Misc Miscellaneous list.
#' @name timeseries-class
#' @include class-unions.R
#' @export
setClass(
  "timeseries",
  slots = c(
    Number          = "list",
    Biomass         = "array",
    SBiomass        = "array",
    SProduction     = "array",
    
    Interactions    = "array",
    Landings        = "array",
    Discards        = "array",
    
    InteractAtAge   = "list",
    LandingsAtAge   = "list",
    DiscardsAtAge   = "list",
    LandingsAtSize  = "list",
    DiscardsAtSize  = "list",
    
    Effort          = "array",
    Distribution    = "array",
    
    FInteract       = "array.null",
    FDead           = "array.null",
    FRetain         = "array.null",
    
    FInteractArea       = "array.list.null",
    FDeadArea       = "array.list.null",
    FRetainArea     = "array.list.null",
    
    Misc            = "list"
  )
)