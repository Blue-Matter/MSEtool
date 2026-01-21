
#' Per-Recruit Object
#'
#' An S4 class for storing per-recruit quantities evaluated across fishing
#' mortality levels. The `perrecruit` object contains numbers-per-recruit,
#' spawning-per-recruit, biomass-per-recruit, and yield-related quantities,
#' optionally stratified by simulation, stock, year, and fishing mortality.
#'
#' Objects of this class are typically produced internally during reference
#' point or equilibrium calculations and are not intended to be modified
#' directly by users.
#'
#' @slot apicalF Apical fishing mortality value used for per-recruit evaluation.
#'
#' @slot NPR0 Numbers-per-recruit at unfished conditions.
#' A list by stock or an array with dimensions `Sim`, `Age`, `Year`.
#'
#' @slot NPR0_SP Spawning numbers-per-recruit at unfished conditions.
#' A list by stock or an array with dimensions `Sim`, `Age`, `Year`.
#'
#' @slot NPRF Numbers-per-recruit at fishing mortality.
#' A list by stock or an array with dimensions `Sim`, `Age`, `Year`, `F`.
#'
#' @slot NPRF_SP Spawning numbers-per-recruit at fishing mortality.
#' A list by stock or an array with dimensions `Sim`, `Age`, `Year`, `F`.
#'
#' @slot SPR0 Spawning-per-recruit at unfished conditions.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot SPRF Spawning-per-recruit at fishing mortality.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot SPR Spawning-per-recruit relative to unfished conditions.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot Biomass Biomass-per-recruit.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot SBiomass Spawning biomass-per-recruit.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot SProduction Spawning production-per-recruit.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot Removals Total removals-per-recruit.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot Landings Landings-per-recruit.
#' Array with dimensions `Sim`, `Stock`, `Year`, `F`.
#'
#' @slot Misc Additional miscellaneous information.
#' @include class-unions.R
#' @export
setClass("perrecruit",
         slots=c(apicalF='numeric', 
                 NPR0='array.list.null', # Sim, Age, Year or list by Stock
                 NPR0_SP='array.list.null', # Sim, Age, Year or list by Stock
                 NPRF='array.list.null', # Sim, Age, Year, F or list by Stock
                 NPRF_SP='array.list.null', # Sim, Age, Year, F or list by Stock
                 SPR0='num.array.null', # Sim, Stock, Year, F
                 SPRF='num.array.null', # Sim, Stock,  Year, F
                 SPR='num.array.null', # Sim, Stock, Year, F
                 Biomass='num.array.null', # Sim,Stock,  Year, F
                 SBiomass='num.array.null', # Sim, Stock, Year, F
                 SProduction='num.array.null', # Sim, Stock, Year, F
                 Removals='num.array.null', # Sim, Stock, Year, F
                 Landings='num.array.null', # Sim, Stock, Year, F
                 Misc='list'
         )
)

