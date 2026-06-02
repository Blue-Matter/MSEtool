#' The `perrecruit` S4 Class
#'
#' An S4 class storing per-recruit quantities evaluated across a vector of
#' apical fishing mortality values. Produced internally by [CalcPerRecruit()]
#' during reference point and equilibrium calculations.
#'
#' @slot apicalF        Numeric vector of apical fishing mortality values at
#'   which per-recruit quantities were evaluated. Length `nF`.
#' @slot NPR0           Numbers-per-recruit at unfished conditions.
#'   Array (`Sim × Age × Year`) or a list by stock.
#' @slot NPR0_SP        Spawning numbers-per-recruit at unfished conditions,
#'   accounting for `SpawnTimeFrac`. Array (`Sim × Age × Year`) or a list by
#'   stock. Equal to `NPR0` when `SpawnTimeFrac` is zero for all stocks.
#' @slot NPRF           Numbers-per-recruit at fishing mortality.
#'   Array (`Sim × Age × Year × F`) or a list by stock,
#'   where the `F` dimension has length `nF`.
#' @slot NPRF_SP        Spawning numbers-per-recruit at fishing mortality,
#'   accounting for `SpawnTimeFrac`. Array (`Sim × Age × Year × F`) or a list
#'   by stock. Populated only when `SpawnTimeFrac > 0` for any stock;
#'   otherwise `NULL`.
#' @slot SPR0           Spawning-per-recruit at unfished conditions.
#'   Array (`Sim × Stock × Year`).
#' @slot SPRF           Spawning-per-recruit at fishing mortality.
#'   Array (`Sim × Stock × Year × F`).
#' @slot SPR            Spawning-per-recruit relative to unfished, i.e.
#'   `SPRF / SPR0`. Array (`Sim × Stock × Year × F`).
#' @slot Biomass        Total biomass-per-recruit.
#'   Array (`Sim × Stock × Year × F`).
#' @slot SBiomass       Spawning biomass-per-recruit.
#'   Array (`Sim × Stock × Year × F`).
#' @slot SProduction    Spawning production-per-recruit, computed from
#'   fecundity-at-age rather than weight × maturity.
#'   Array (`Sim × Stock × Year × F`).
#' @slot Removals       Total removals-per-recruit (landings + dead discards),
#'   summed over fleets. Array (`Sim × Stock × Year × F`).
#' @slot Landings       Landings-per-recruit, summed over fleets.
#'   Array (`Sim × Stock × Year × F`).
#' @slot Misc           List reserved for internal use.
#'
#' @details
#' ## Spawning numbers-per-recruit
#' When `SpawnTimeFrac > 0` for any stock, `NPRF_SP` differs from `NPRF`:
#' survival to the spawn date is computed as
#' \deqn{N \cdot \exp(-Z \cdot \texttt{SpawnTimeFrac})}
#' so that `SPR`, `SBiomass`, and `SProduction` reflect abundance at the time
#' of spawning rather than at the start of the year. When `SpawnTimeFrac = 0`
#' for all stocks, `NPRF_SP` is not populated.
#'
#' ## SPFrom
#' When stocks share a spawning component (e.g. a complex), the `SPFrom` slot
#' of each stock's [SRR()] determines which stock's `SPRF` is used when
#' computing relative `SPR`. This is handled upstream in [CalcPerRecruit()]
#' before the `SPRF` and `SPR0` slots are populated.
#'
#' ## Relationships between slots
#' \deqn{\texttt{SPR} = \texttt{SPRF} / \texttt{SPR0}}
#' \deqn{\texttt{SBiomass} = \sum_a \texttt{NPRF\_SP}_a \cdot W_a \cdot m_a}
#' \deqn{\texttt{SProduction} = \sum_a \texttt{NPRF\_SP}_a \cdot f_a}
#' where \eqn{W_a}, \eqn{m_a}, and \eqn{f_a} are weight, maturity, and
#' fecundity at age respectively.
#'
#' @seealso [CalcPerRecruit()]
#' @include class-unions.R
#' @export
#' @name perrecruit-class
setClass('perrecruit',
         slots = c(
           apicalF      = 'numeric',
           NPR0         = 'array.list.null',
           NPR0_SP      = 'array.list.null',
           NPRF         = 'array.list.null',
           NPRF_SP      = 'array.list.null',
           SPR0         = 'num.array.null',
           SPRF         = 'num.array.null',
           SPR          = 'num.array.null',
           Biomass      = 'num.array.null',
           SBiomass     = 'num.array.null',
           SProduction  = 'num.array.null',
           Removals     = 'num.array.null',
           Landings     = 'num.array.null',
           Misc         = 'list'
         )
)

setValidity('perrecruit', function(object) {
  # TODO
  TRUE
})
