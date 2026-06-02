#' The `depletion` S4 Class
#'
#' Defines the initial and final depletion assumptions for a [stock-class]
#' object, expressed relative to a reference biomass. Objects are typically
#' created via [Depletion()], which documents all parameters and validates
#' inputs. This object is optional — see *Default Behaviour* in [Depletion()]
#' for what happens when it is omitted.
#'
#' @slot Initial `array` or `NULL`. Depletion relative to `Reference` in the
#'   first historical time step, as a `nSim`-length array after [Populate()].
#'   Before population, may be `NULL` (stock assumed dynamic unfished at the start of
#'   the historical period), a scalar, a length-2 bounds vector, or a
#'   length-`nSim` vector. See [Depletion()] for full details.
#' @slot Final `array` or `NULL`. Target depletion relative to `Reference` in
#'   the terminal historical time step, as a `nSim`-length array after
#'   [Populate()]. When populated, the `Efficiency` parameter in
#'   [Catchability()] is optimised to achieve this depletion level, overwriting
#'   any existing `Efficiency` values. When `NULL`, no optimisation occurs and
#'   [Catchability()] must be populated directly. See [Depletion()] for full
#'   details.
#' @slot Reference `character(1)`. Reference biomass used to scale depletion
#'   values. Currently implemented options are `"B0"` (total unfished biomass,
#'   default) and `"SB0"` (spawning biomass at unfished equilibrium). The
#'   options `"BMSY"`, `"SBMSY"`, `"SP0"`, and `"SPMSY"` are accepted by the
#'   validator but are reserved for future use.
#' @slot Misc `list`. Used internally.
#'
#' @details
#' Direct construction via [methods::new()] is not recommended; use
#' [Depletion()] instead, which validates inputs.
#'
#' Both `Initial` and `Final` may remain `NULL` after [Populate()] — `NULL`
#' indicates the slot is not in use rather than an uninitialised state. When
#' both are `NULL`, the `depletion` object has no effect on model behaviour.
#'
#' @seealso
#' - [Depletion()] for the constructor and accessor functions.
#' - [Catchability()] for the fleet catchability object whose `Efficiency`
#'   parameter is optimised when `Final` is populated.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#'
#' @family depletion
#'
#' @export
#' @include class-unions.R
#' @name depletion-class
setClass('depletion',
         slots = c(
           Initial   = "num.array.null",
           Final     = "num.array.null",
           Reference = "array.char.null",
           Misc      = "list"
         )
)

setValidity('depletion', function(object) {
  # TODO 
  TRUE
})




