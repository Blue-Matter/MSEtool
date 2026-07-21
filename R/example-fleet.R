#' Asymptotic Selectivity Example Fleet
#'
#' An example [fleet-class] object demonstrating a fleet with effort that
#' stabilises early in the historical period and asymptotic length-based
#' selectivity. Intended for use with [AlbacoreExStock] or [ButterfishExStock]
#' to illustrate fleet parameterization in `MSEtool`. See also [DomeExFleet]
#' for a contrasting dome-shaped selectivity example.
#'
#' @format A [fleet-class] object with the following slots populated:
#'
#' - `Name`: `"AsympExFleet"`. Unique fleet identifier.
#' - `Effort`: An [Effort()] object with a piecewise-linear historical effort
#'   trajectory defined at four relative time points (0, 0.3, 0.6, 1.0),
#'   with lower and upper bounds of (0, 0.4, 1, 1) and (0, 0.6, 1, 1)
#'   respectively, and `CV = 0.1` controlling stochastic variation around
#'   the trend.
#' - `Selectivity`: A [Selectivity()] object with asymptotic length-based
#'   selectivity parameters — `L5` (0.4–0.5), `LFS` (0.7–0.8), and
#'   `Vmaxlen = 1` (fixed) — with `isRel = TRUE` indicating that `L5` and
#'   `LFS` are expressed relative to maturity `L50`.
#' - `Catchability`: An empty [Catchability()] object; efficiency defaults to
#'   1 across all years during population (see Details).
#' - `Retention`: An empty [Retention()] object; full retention is assumed
#'   during population (see Details).
#' - `DiscardMortality`: An empty [DiscardMortality()] object.
#' - `Closure`: `NULL`. No spatial or temporal closures defined.
#' - `WeightFleetRetained`, `WeightFleetSelected`: `NA`. Computed from
#'   selectivity, retention, and the stock's weight-at-length during
#'   population (see Details).
#' - `Bioeconomic`: An empty [Bioeconomic()] object.
#' - `nYear`, `pYear`, `nSim`, `CurrentYear`, `Years`, `Seasons`: Inherited
#'   from the paired [Stock()] object by [PopulateFleet()].
#' - `Misc`: Empty list for user-defined information.
#'
#' @details
#' [PopulateFleet()] expands the input parameters into `[nSim x nAge x nYear]`
#' arrays using the age, length, and spatial structure of the paired populated
#' stock. The sections below describe what each component-level `Populate*()`
#' function produces.
#'
#' ## Effort
#' [PopulateEffort()] interpolates the piecewise-linear effort trajectory
#' across all historical years for each simulation, then applies log-normal
#' stochastic deviates scaled by `CV = 0.1`. The trajectory rises from 0 to
#' 40–60% of maximum effort by 30% of the historical period, then reaches
#' full effort by 60% of the period and remains stable thereafter. This
#' contrasts with [DomeExFleet], where effort continues to rise through to
#' the final historical year.
#'
#' ## Catchability
#' Because the `Catchability` slot is empty, [PopulateCatchability()] sets
#' catchability efficiency to 1 across all simulations and years. No trend
#' (`qInc`) or inter-annual variability (`qCV`) is applied.
#'
#' ## Selectivity
#' [PopulateSelectivity()] samples `L5` and `LFS` for each simulation from
#' uniform distributions defined by the prior bounds. Because `isRel = TRUE`,
#' both are scaled by the maturity `L50` of the paired stock before computing
#' the selectivity curve. `Vmaxlen = 1` is fixed, so selectivity reaches 1.0
#' at maximum length in all simulations, producing a strictly asymptotic
#' curve. The resulting selectivity-at-length is converted to
#' selectivity-at-age using the stock age–length key, and an area dimension
#' is added.
#'
#' ## Retention
#' Because the `Retention` slot is empty, [PopulateRetention()] sets full
#' retention (1.0) across all ages, lengths, simulations, and years.
#'
#' ## Discard Mortality
#' Because the `DiscardMortality` slot is empty, [PopulateDiscardMortality()]
#' applies default discard mortality values. All discarded fish are assumed
#' to survive (discard mortality = 0) unless overridden.
#'
#' ## Spatial Closures
#' Because `Closure` is `NULL`, [PopulateClosure()] applies no spatial or
#' temporal closures; all areas are open to fishing in all years.
#'
#' ## Fleet Weight-at-Age
#' Because `WeightFleetRetained`/`WeightFleetSelected` are `NA`,
#' [PopulateFleet()] computes them from selectivity, retention, and the
#' stock's weight-at-length, so fleet-level landings/discards biomass
#' calculations reflect the fleet's own selectivity and retention pattern
#' rather than the stock's unselected mean weight-at-age.
#'
#' @seealso [DomeExFleet], [AlbacoreExStock], [ButterfishExStock], [Fleet()],
#'   [PopulateFleet()], [Populate()], [fleet-class]
#'
#' @examples
#' AsympExFleet
#'
#' \dontrun{
#' PopulatedStock <- PopulateStock(AlbacoreExStock, nYear = 50, pYear = 30, nSim = 48)
#' PopulatedFleet <- PopulateFleet(AsympExFleet, Stock = PopulatedStock, seed = 42)
#' }
#'
"AsympExFleet"

#' Dome-Shaped Selectivity Example Fleet
#'
#' An example [fleet-class] object demonstrating a fleet with increasing
#' historical effort and dome-shaped length-based selectivity. Intended for
#' use with [AlbacoreExStock] or [ButterfishExStock] to illustrate fleet
#' parameterization.  
#'
#' @format A [fleet-class] object with the following slots populated:
#'
#' - `Name`: `"DomeExFleet"`. Unique fleet identifier.
#' - `Effort`: An [Effort()] object with a piecewise-linear historical effort
#'   trajectory defined at four relative time points (0, 0.3, 0.6, 1.0),
#'   with lower and upper bounds of (0, 0.4, 0.4, 1) and (0, 0.6, 0.6, 1)
#'   respectively, and `CV = 0.1` controlling stochastic variation around the
#'   trend.
#' - `Selectivity`: A [Selectivity()] object with dome-shaped length-based
#'   selectivity parameters — `L5` (0.2–0.4), `LFS` (0.75–1.1), and
#'   `Vmaxlen` (0.5–1.0) — with `isRel = TRUE` indicating that `L5` and
#'   `LFS` are expressed relative to maturity `L50`.
#' - `Catchability`: An empty [Catchability()] object; efficiency defaults to
#'   1 across all years during population (see Details).
#' - `Retention`: An empty [Retention()] object; full retention is assumed
#'   during population (see Details).
#' - `DiscardMortality`: An empty [DiscardMortality()] object. Discard mortality
#'   is assumed to be 0.
#' - `Closure`: `NULL`. No spatial or temporal closures defined.
#' - `WeightFleetRetained`, `WeightFleetSelected`: `NA`. Computed from
#'   selectivity, retention, and the stock's weight-at-length during
#'   population (see Details).
#' - `Bioeconomic`: An empty [Bioeconomic()] object.
#' - `nYear`, `pYear`, `nSim`, `CurrentYear`, `Years`, `Seasons`: Inherited
#'   from the paired [Stock()] object by [PopulateFleet()].
#' - `Misc`: Empty list for user-defined information.
#'
#' @details
#' [PopulateFleet()] expands the input parameters into `[nSim x nAge x nYear]`
#' arrays using the age, length, and spatial structure of the paired populated
#' [stock-class] object. The sections below describe what each component-level 
#' `Populate*()` function produces.
#'
#' ## Effort
#' [PopulateEffort()] interpolates the piecewise-linear effort trajectory
#' across all historical years for each simulation, then applies log-normal
#' stochastic deviates scaled by `CV = 0.1`. The trajectory rises from 0 to
#' a plateau at 40–60% of maximum effort around 30–60% of the historical
#' period, then increases to full effort by the final historical year.
#'
#' ## Catchability
#' Because the `Catchability` slot is empty, [PopulateCatchability()] sets
#' catchability efficiency to 1 across all simulations and years. No trend
#' (`qInc`) or inter-annual variability (`qCV`) is applied.
#'
#' ## Selectivity
#' [PopulateSelectivity()] samples `L5`, `LFS`, and `Vmaxlen` for each
#' simulation from uniform distributions defined by the prior bounds.
#' Because `isRel = TRUE`, `L5` and `LFS` are scaled by the maturity `L50`
#' of the paired stock before computing the selectivity curve. The resulting
#' dome-shaped selectivity-at-length is converted to selectivity-at-age
#' using the stock age–length key, and an area dimension is added. The
#' `Vmaxlen` range (0.5–1.0) allows selectivity at maximum length to vary
#' from strongly dome-shaped to asymptotic across simulations.
#'
#' ## Retention
#' Because the `Retention` slot is empty, [PopulateRetention()] sets full
#' retention (1.0) across all ages, lengths, simulations, and years.
#'
#' ## Discard Mortality
#' Because the `DiscardMortality` slot is empty, [PopulateDiscardMortality()]
#' applies default discard mortality values. All discarded fish are assumed
#' to survive (discard mortality = 0) unless overridden.
#'
#' ## Spatial Closures
#' Because `Closure` is `NULL`, [PopulateClosure()] applies no spatial or
#' temporal closures; all areas are open to fishing in all years.
#'
#' ## Fleet Weight-at-Age
#' Because `WeightFleetRetained`/`WeightFleetSelected` are `NA`,
#' [PopulateFleet()] computes them from selectivity, retention, and the
#' stock's weight-at-length, so fleet-level landings/discards biomass
#' calculations reflect the fleet's own selectivity and retention pattern
#' rather than the stock's unselected mean weight-at-age.
#'
#' @seealso [AlbacoreExStock], [ButterfishExStock], [Fleet()],
#'   [PopulateFleet()], [Populate()], [fleet-class]
#'
#' @examples
#' DomeExFleet
#'
#' \dontrun{
#' PopulatedStock <- PopulateStock(AlbacoreExStock, nYear = 50, pYear = 30, nSim = 48)
#' PopulatedFleet <- PopulateFleet(DomeExFleet, Stock = PopulatedStock, seed = 42)
#' }
#'
"DomeExFleet"