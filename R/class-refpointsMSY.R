#' The `refpointsMSY` S4 Class
#'
#' An S4 class storing MSY-based biological reference points. Produced
#' internally during reference point calculations and not intended to be
#' modified directly by users.
#'
#' @slot FMSY        Fishing mortality rate at MSY (`Sim × Stock × Year`).
#' @slot BMSY        Total biomass at MSY (`Sim × Stock × Year`).
#' @slot SBMSY       Spawning biomass at MSY (`Sim × Stock × Year`).
#' @slot SPMSY       Spawning production at MSY (`Sim × Stock × Year`).
#' @slot SPRMSY      Spawning potential ratio at MSY (`Sim × Stock × Year`).
#' @slot MSYLandings Landed catch at MSY (`Sim × Stock × Year`).
#' @slot MSYDiscards Dead discards at MSY (`Sim × Stock × Year`).
#' @slot Misc        List reserved for internal use.
#'
#' @details
#' ## Reference point calculations
#' MSY reference points are derived from the per-recruit quantities in a
#' [perrecruit-class] object by identifying the apical fishing mortality
#' that maximises total removals, accounting for the stock-recruitment
#' relationship. All slots share the same `Sim × Stock × Year` dimensions,
#' where the `Year` dimension reflects the year(s) for which biological and
#' fishery parameters were evaluated.
#'
#' @seealso [perrecruit-class]
#' @include class-unions.R
#' @export
#' @name refpointsMSY-class
setClass("refpointsMSY",
         slots = c(
           FMSY        = "num.array.null",
           BMSY        = "num.array.null",
           SBMSY       = "num.array.null",
           SPMSY       = "num.array.null",
           SPRMSY      = "num.array.null",
           MSYLandings = "num.array.null",
           MSYDiscards = "num.array.null",
           Misc        = "list"
         )
)