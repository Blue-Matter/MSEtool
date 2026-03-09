
#' `hist` Class
#'
#' The `hist` class stores the complete historical time-series generated from
#' an [om-class] object. It extends the [timeseries-class] with the originating
#' operating model, unfished reference states, biological reference points, and
#' associated observational data. Objects of this class are typically created
#' by running [Simulate()], and
#' serve as the starting point for forward projections in the MSE.
#'
#' @slot OM The [om-class] object used to generate the historical time-series.
#'
#' @slot Unfished An [unfished-class] object containing two
#'   [popdynamics-class] sub-objects:
#'
#'   - `Equilibrium`: Unfished equilibrium population dynamics, used as the
#'     baseline for computing biological reference points such as B0 and SSB0.
#'   - `Dynamic`: Unfished dynamic population trajectories, representing the
#'     counterfactual population in the absence of fishing over the historical
#'     period.
#'
#' @slot Reference A [reference-class] object containing biological and
#'   management reference points.
#'   
#' @slot Data A nested named list of [data-class] objects containing historical
#'   observations generated during the spool-up phase. The list has two levels:
#'
#'   - **Level 1** (`nSim` elements, or length 1 if all simulations are
#'     identical): one entry per simulation.
#'   - **Level 2** (one entry per stock or stock complex defined in the [om-class]):
#'     a [data-class] object holding the observations for that stock/complex.
#'
#' @slot Log Internal named list storing diagnostics, warnings, and
#'   bookkeeping information generated during the historical simulation.
#'   Not intended for direct user access.
#'
#' @slot Misc Named list for carrying arbitrary additional objects alongside
#'   the historical results. Useful for attaching model-specific diagnostics
#'   or intermediate outputs.
#'
#' @slot Number List of numbers-at-age arrays, one element per stock.
#'   Each array has dimensions `Sim × Age × Year × Area`.
#'
#' @slot Biomass Total biomass array with dimensions `Sim × Stock × Year`.
#'
#' @slot SBiomass Spawning biomass array with dimensions `Sim × Stock × Year`.
#'
#' @slot SProduction Spawning production array with dimensions
#'   `Sim × Stock × Year`.
#'
#' @slot Interactions Total biomass interacting with fishing gear, by stock and
#'   fleet. Array with dimensions `Sim × Stock × Year × Fleet`.
#'
#' @slot Landings Total landed biomass by stock and fleet. Array with
#'   dimensions `Sim × Stock × Year × Fleet`.
#'
#' @slot Discards Total discarded biomass by stock and fleet. Array with
#'   dimensions `Sim × Stock × Year × Fleet`.
#'
#' @slot InteractAtAge List of interactions-at-age arrays (numbers), one
#'   element per stock. Each array has dimensions
#'   `Sim × Age × Year × Fleet × Area`.
#'
#' @slot LandingsAtAge List of landings-at-age arrays (numbers), one element
#'   per stock. Each array has dimensions `Sim × Age × Year × Fleet × Area`.
#'
#' @slot DiscardsAtAge List of discards-at-age arrays (numbers), one element
#'   per stock. Each array has dimensions `Sim × Age × Year × Fleet × Area`.
#'
#' @slot LandingsAtSize Nested list of landings-at-size arrays (numbers),
#'   indexed by stock then fleet. Each array has dimensions
#'   `Sim × Class × Year × Area`.
#'
#' @slot DiscardsAtSize Nested list of discards-at-size arrays (numbers),
#'   indexed by stock then fleet. Each array has dimensions
#'   `Sim × Class × Year × Area`.
#'
#' @slot Effort Fishing effort array with dimensions `Sim × Year × Fleet`.
#'
#' @slot Distribution Fleet effort distribution across areas. Array with
#'   dimensions `Sim × Year × Fleet × Area`.
#'
#' @slot FInteract Apical fishing mortality for fish that interact with the
#'   fishing gear. Array with dimensions `Sim × Stock × Year × Fleet`
#'   (`array.null`).
#'
#' @slot FDead Apical fishing mortality for fish killed by the fishing gear.
#'   Array with dimensions `Sim × Stock × Year × Fleet` (`array.null`).
#'
#' @slot FRetain Apical fishing mortality for fish retained by fishers. Array
#'   with dimensions `Sim × Stock × Year × Fleet` (`array.null`).
#'
#' @slot FInteractArea Area- and age-specific fishing mortality for fish
#'   interacting with the gear, as a list by stock. Each element has dimensions
#'   `Sim × Age × Year × Fleet × Area` (`array.list.null`).
#'
#' @slot FDeadArea Area- and age-specific fishing mortality for fish killed by
#'   the gear, as a list by stock. Each element has dimensions
#'   `Sim × Age × Year × Fleet × Area` (`array.list.null`).
#'
#' @slot FRetainArea Area- and age-specific fishing mortality for retained
#'   fish, as a list by stock. Each element has dimensions
#'   `Sim × Age × Year × Fleet × Area` (`array.list.null`).
#'
#' @seealso [om-class], [unfished-class], [reference-class], [timeseries-class],
#'   [data-class], [popdynamics-class]
#'
#' @include class-unions.R
#' @include class-data.R
#' @include class-om.R
#' @include class-unfished.R
#' @include class-timeseries.R
#' @include class-reference.R
#' @name hist
#' @export
setClass(
  "hist",
  slots = c(
    OM        = "om",
    Unfished  = "unfished",
    Reference = "reference",
    Data      = "list",
    Log       = "list",
    Misc      = "list"
  ),
  contains = "timeseries"
)


setValidity("hist", function(object) {
  # TODO: structural checks on dimensions / consistency
  TRUE
})

