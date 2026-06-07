#' The `equilibrium` S4 Class
#'
#' An S4 class storing absolute-scale equilibrium quantities evaluated across a
#' vector of apical fishing mortality values, accounting for the
#' stock-recruitment relationship. Produced by [CalcEquilibrium()].
#'
#' @slot apicalF      Numeric vector of apical fishing mortality values at
#'   which equilibrium quantities were evaluated. Length `nF`.
#' @slot SPR0         Spawning-per-recruit at unfished conditions.
#'   Array (`Sim × Stock × Year`).
#' @slot SPR          Spawning-per-recruit relative to unfished, i.e.
#'   `SPRF / SPR0`. Array (`Sim × Stock × Year × F`).
#' @slot RelRecruits  Relative recruitment from the stock-recruitment
#'   relationship, evaluated at each `SPR` value.
#'   Array (`Sim × Stock × Year × F`).
#' @slot Recruits     Absolute equilibrium recruitment (`R0 × RelRecruits`).
#'   Array (`Sim × Stock × Year × F`).
#' @slot Number       Absolute equilibrium numbers-at-age
#'   (`NPRF × Recruits`), summed over the age dimension.
#'   Array (`Sim × Stock × Year × F`).
#' @slot Biomass      Absolute equilibrium total biomass.
#'   Array (`Sim × Stock × Year × F`).
#' @slot SBiomass     Absolute equilibrium spawning biomass.
#'   Array (`Sim × Stock × Year × F`).
#' @slot SProduction  Absolute equilibrium spawning production, computed from
#'   fecundity-at-age rather than weight × maturity.
#'   Array (`Sim × Stock × Year × F`).
#' @slot Removals     Absolute equilibrium total removals (landings + dead
#'   discards), summed over fleets. Array (`Sim × Stock × Year × F`).
#' @slot Landings     Absolute equilibrium landings, summed over fleets.
#'   Array (`Sim × Stock × Year × F`).
#' @slot Misc         List reserved for internal use.
#'
#' @details
#' ## Scaling from per-recruit to absolute
#' All F-dependent slots are obtained by multiplying the corresponding
#' per-recruit quantity from a [perrecruit-class] object by equilibrium
#' recruitment:
#' \deqn{\texttt{Recruits}_{s,F} = R_{0,s} \cdot \phi(\texttt{SPR}_{s,F})}
#' where \eqn{\phi} is the relative recruitment function from the stock's
#' [SRR()] and \eqn{R_{0,s}} is unfished recruitment. Values of
#' `RelRecruits` below zero are floored at zero before scaling.
#'
#' ## Complexes
#' When stocks belong to the same complex they share a single apical F, and
#' `SPR` for each stock is computed relative to that complex-level F. The
#' `RelRecruits` scaling is still applied per stock.
#'
#'
#' @seealso [CalcEquilibrium()], [perrecruit-class]
#' @include class-unions.R
#' @export
#' @name equilibrium-class
setClass('equilibrium',
         slots = c(
           apicalF     = 'numeric',
           SPR0        = 'num.array.null',
           SPR         = 'num.array.null',
           RelRecruits = 'num.array.null',
           Recruits    = 'num.array.null',
           Number      = 'num.array.null',
           Biomass     = 'num.array.null',
           SBiomass    = 'num.array.null',
           SProduction = 'num.array.null',
           Removals    = 'num.array.null',
           Landings    = 'num.array.null',
           Misc        = 'list'
         )
)
setValidity('equilibrium', function(object) {
  # TODO
  TRUE
})