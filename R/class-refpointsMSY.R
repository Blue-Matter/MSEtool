#' `refpointsMSY` Class
#'
#' Stores MSY-based biological reference points across simulations.
#' All slots are arrays or numerics with a `Sim` dimension unless otherwise
#' noted.
#'
#' @slot FMSY        Fishing mortality rate at MSY.
#' @slot BMSY        Total biomass at MSY.
#' @slot SBMSY       Spawning biomass at MSY.
#' @slot SPMSY       Spawning production at MSY.
#' @slot SPRMSY      Spawning potential ratio at MSY.
#' @slot MSY         Maximum sustainable yield (removals).
#' @slot MSYLandings Landed catch at MSY.
#' @slot Misc        Miscellaneous list for additional outputs.
#' @include class-unions.R
#' @export
#' @name refpointsMSY-class
#' @aliases refpointsMSY
setClass("refpointsMSY",
         slots = c(
           FMSY        = "num.array.null",
           BMSY        = "num.array.null",
           SBMSY       = "num.array.null",
           SPMSY       = "num.array.null",
           SPRMSY      = "num.array.null",
           MSY         = "num.array.null",
           MSYLandings = "num.array.null",
           Misc        = "list"
         )
)