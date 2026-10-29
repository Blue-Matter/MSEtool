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
#'   [Catchability()] must be populated directly. For stocks linked by
#'   `SRR@SPFrom`, the target of the source stock applies to the linked group.
#'   See [Depletion()] for full details.
#' @slot Reference `character(1)`. Reference biomass used to scale depletion
#'   values: `"B0"` (total unfished biomass, default) or `"SB0"` (spawning
#'   biomass at unfished equilibrium).
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
  .CheckDepletionReference(object@Reference)
})

.ValidDepletionReference <- c('B0', 'SB0')

.CheckDepletionReference <- function(Reference) {
  if (!length(Reference)) return(TRUE)
  if (length(Reference) == 1 && Reference %in% .ValidDepletionReference) return(TRUE)
  paste0("`Reference` must be one of ",
         paste0("'", .ValidDepletionReference, "'", collapse = ' or '),
         ", not ", paste0("'", Reference, "'", collapse = ', '))
}



