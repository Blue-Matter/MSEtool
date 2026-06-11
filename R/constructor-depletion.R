#' Depletion Constructor and Accessors
#'
#' Construct a [depletion-class] object defining the initial and/or final
#' depletion assumptions for a [stock-class], or access and replace the
#' `Depletion` slot of a [stock-class] and its individual slots. `Depletion`
#' is optional — see *Default Behaviour* below.
#'
#' @param Initial `numeric` or `NULL`. Depletion relative to `Reference` at
#'   the start of the historical period. Accepted formats:
#'   - `NULL` (default): stock is assumed unfished at the start of the
#'     historical period. No adjustment to recruitment deviations.
#'   - Scalar: same depletion for all simulations.
#'   - Length-2 bounds vector: depletion sampled from `Uniform(lower, upper)`
#'     independently for each simulation.
#'   - Length-`nSim` vector: one value per simulation.
#'   When provided, early historical recruitment deviations are adjusted so
#'   that biomass relative to `Reference` in the first time step matches the
#'   specified value. When `Initial` is a [stock-class] object, `Depletion()`
#'   acts as a pass-through accessor and returns `x@Depletion`.
#' @param Final `numeric` or `NULL`. Target depletion relative to `Reference`
#'   in the terminal historical time step. Accepts the same formats as
#'   `Initial`. When populated, the `Efficiency` parameter in [Catchability()]
#'   is optimised to achieve this depletion level, overwriting any existing
#'   `Efficiency` values. When `NULL` (default), no optimisation occurs and
#'   [Catchability()] must be populated directly with valid values.
#' @param Reference `character(1)`. Reference biomass used to scale depletion.
#'   Currently implemented options:
#'   - `"B0"` (default): total unfished equilibrium biomass.
#'   - `"SB0"`: spawning biomass at unfished equilibrium.
#'   The options `"BMSY"`, `"SBMSY"`, `"SP0"`, and `"SPMSY"` are reserved for
#'   future use and currently pass validation but are not implemented.
#' @param x A [depletion-class] object for slot accessors, or a [stock-class]
#'   object for `Depletion<-`.
#' @param value For `Depletion<-`: a [depletion-class] object. For `Initial<-`
#'   and `Final<-`: a numeric scalar, length-2 bounds vector, or
#'   length-`nSim` vector.
#'
#' @details
#' ## Default Behaviour When Omitted
#'
#' `Depletion` is optional. When both `Initial` and `Final` are `NULL` (the
#' default), the object has no effect on model behaviour:
#'
#' - The stock is assumed unfished at the start of the historical period.
#' - Depletion in the terminal year is determined by the interaction between
#'   [Stock()] and [Fleet()] parameters; [Catchability()] must be populated
#'   directly in this case.
#'
#' ## Initial Depletion
#'
#' When `Initial` is supplied, the operating model adjusts early recruitment
#' deviations so that biomass relative to `Reference` in the first time step
#' matches the target. Values must be in (0, 1]:
#'
#' ```r
#' # Fixed initial depletion across all simulations
#' dep <- Depletion(Initial = 0.8)
#'
#' # Stochastic initial depletion — sampled from Uniform(0.6, 0.9)
#' dep <- Depletion(Initial = c(0.6, 0.9))
#'
#' # One value per simulation
#' dep <- Depletion(Initial = runif(48, 0.6, 0.9))
#' ```
#'
#' ## Final Depletion
#'
#' When `Final` is supplied, the operating model optimises `Efficiency` in
#' [Catchability()] so that depletion in the terminal historical year matches
#' the target. Any existing `Efficiency` values are overwritten. When `Final`
#' is `NULL`, `Efficiency` is used as supplied:
#'
#' ```r
#' # Force terminal depletion to 0.4 relative to B0
#' dep <- Depletion(Final = 0.4)
#'
#' # Stochastic terminal depletion
#' dep <- Depletion(Final = c(0.3, 0.5))
#'
#' # Both initial and final depletion specified
#' dep <- Depletion(Initial = 0.9, Final = 0.4)
#'
#' # Relative to spawning biomass at unfished equilibrium
#' dep <- Depletion(Final = 0.4, Reference = "SB0")
#' ```
#'
#' ## Pass-Through Access from a Stock
#'
#' When `Initial` is a [stock-class] object, `Depletion()` returns the
#' `Depletion` slot directly:
#'
#' ```r
#' Depletion(my_stock)             # returns my_stock@Depletion
#' Depletion(my_stock) <- my_dep   # replaces my_stock@Depletion
#' ```
#'
#' ## Slot Accessors
#'
#' Individual slots can be read or replaced using generic functions matching
#' their names. All replacement functions re-validate the object:
#'
#' ```r
#' Initial(dep)   <- 0.8
#' Final(dep)     <- 0.4
#' ```
#'
#' @return
#' - `Depletion()` returns a [depletion-class] object. If `Initial` is a
#'   [stock-class], returns `x@Depletion`.
#' - `Depletion<-` returns the [stock-class] `x` with the `Depletion` slot
#'   replaced and the object re-validated.
#' - `Initial()` and `Final()` return the value of the corresponding slot
#'   from `x` (a `nSim`-length array after [Populate()], or `NULL`).
#' - `Initial<-` and `Final<-` return `x` with the named slot updated and the
#'   object re-validated.
#'
#' @seealso
#' - [depletion-class] for the class definition and slot-level documentation.
#' - [Catchability()] for the fleet catchability object whose `Efficiency`
#'   parameter is optimised when `Final` is populated.
#' - [Populate()] for array population.
#' - [Stock()] for the enclosing stock constructor.
#'
#' @family depletion
#'
#' @example man-examples/class-Depletion.R
#'
#' @export
Depletion <- function(Initial = NULL,
                      Final = NULL,
                      Reference = "B0") {
  
  if (isStockOrList(Initial)) 
    return(ExtractStockSlot(Initial, "Depletion"))
  
  methods::new("depletion",
               Initial   = Initial,
               Final     = Final,
               Reference = Reference)
}



#' @rdname Depletion
#' @export
Initial <- function(x) {
  CheckClass(x, "depletion", "Depletion")
  x@Initial
}


#' @rdname Depletion
#' @export
`Initial<-` <- function(x, value) {
  CheckClass(x, "depletion", "Depletion")
  x@Initial <- value
  methods::validObject(x)
  x
}


#' @rdname Depletion
#' @export
Final <- function(x) {
  CheckClass(x, "depletion", "Depletion")
  x@Final
}


#' @rdname Depletion
#' @export
`Final<-` <- function(x, value) {
  CheckClass(x, "depletion", "Depletion")
  x@Final <- value
  methods::validObject(x)
  x
}

#' @rdname Depletion
#' @export
`Depletion<-`<- function(x, value) {
  AssignSlotRecursive(x, value, 'Depletion')
}
