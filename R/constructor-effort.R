#' Effort
#'
#' Construct and manipulate an [effort-class] object defining historical
#' fishing effort and spatial structure for a [Fleet()] object.
#'
#' @param Effort Numeric array or data frame. Historical fishing effort. If a
#'   correctly structured data frame, [GenHistEffort()] is called to generate
#'   stochastic effort across simulations. If a [fleet-class], [effort-class],
#'   [hist-class], [obs-class], or [mse-class] object is passed, the `Effort`
#'   slot of that object is returned. 
#'   
#'   If an array, must have dimensions `Sim x Year` with named dimnames. The
#'   `Year` dimension must match the historical years of the OM.
#'
#'   If a data frame, must be in the format required by [GenHistEffort()],
#'   which will generate a `Sim x Year` array stochastically.
#'   
#' @param Units Character. Units of effort (e.g., `"hours"`, `"trips"`).
#'   Default `NULL`. Note that effort is converted to fishing mortality via
#'   gear efficiency (`q`) defined in the [catchability-class] object.
#'   
#' @param Distribution Numeric array. Fraction of total effort allocated to
#'   each area. Only used for spatial models (i.e., `nArea > 1`). Must have
#'   dimensions `Sim x Year x Area` with named dimnames, and values must sum
#'   to 1 over the `Area` dimension within each simulation and year. The `Sim`
#'   and `Year` dimensions may each be length 1 (replicated internally) or
#'   match `nSim` and the historical years respectively. If `NULL` (default),
#'   spatial allocation is calculated internally from spatial utility
#'   calculations and fleet behaviour.
#'   
#' @param Targeting Numeric array. Fleet targeting parameter with dimensions
#'   `Sim x Year`. The `Sim` dimension may be length 1 or match `nSim`. The
#'   `Year` dimension must be either length 1 or match the historical years,
#'   and must have named `Year` dimnames if not length 1 or full length.
#'   Default `NULL`, in which case a value of `0.8` is used for all
#'   simulations and years.
#'   
#' @param Maximum Numeric. Maximum allowable effort. Default `NULL`. Not
#' currently used.
#' @param Misc List. Miscellaneous additional inputs. Default `list()`.
#' 
#' @param df Logical. Only used when `Effort` is a [hist-class] or an 
#' [mse-class] object. If `FALSE` (default) the raw `Effort` array is returned. 
#' If `TRUE` a tidy `data.frame` is returned via [extract_effort()].
#'   
#' @param x An [effort-class] object, or a compatible object for `Effort<-`.
#' @param value For `Effort<-`: an [effort-class] object. For slot replacement
#'   functions: the new value for the corresponding slot.
#'
#' @details
#' Effort represents total fishing activity prior to spatial allocation. It is
#' converted to fishing mortality via gear efficiency (`q`) defined in the
#' associated [catchability-class] object.
#'
#' ## Effort Array Structure
#'
#' `Effort@Effort` must be a numeric array with dimensions `Sim x Year`:
#' - `Sim`: number of simulation replicates, or length 1 (replicated
#'    internally).
#' - `Year`: must match the historical years of the OM with named `Year`
#'    dimnames.
#'
#' Alternatively, a data frame in the format required by [GenHistEffort()]
#' may be supplied, and a stochastic `Sim x Year` array will be generated
#' automatically.
#'
#' ## Spatial Distribution
#'
#' `Distribution` represents the fraction of effort in each area and is only
#' used for spatial models (`nArea > 1`). It must have dimensions
#' `Sim x Year x Area` and sum to 1 over areas within each simulation and
#' year. The `Sim` and `Year` dimensions may each be length 1 (replicated
#' internally). If not specified (default), spatial allocation is derived
#' internally from spatial utility calculations and fleet behaviour.
#'
#' ## Targeting
#'
#' `Targeting` is a `Sim x Year` array. The `Year` dimension must be length 1
#' or match the historical years; if neither, named `Year` dimnames are
#' required. If not specified, a default value of `0.8` is applied to all
#' simulations and years.
#'
#' ## Attaching to a Fleet
#'
#' An `Effort` object can be attached to a [Fleet()] with
#' `Effort(Fleet) <- MyEffort` and retrieved with `Effort(Fleet)`.
#'
#' Individual slots may be accessed or modified using [Units()],
#' [Distribution()], [Targeting()], and [Maximum()].
#'
#' `r TechManLink()`
#'
#' @return
#' - `Effort()` returns an [effort-class] object. If `Effort` is a
#'   [fleet-class], [effort-class], [hist-class], [obs-class], or [mse-class]
#'   object, the `Effort` slot of that object is returned.
#' - `Effort<-` returns `x` with the `Effort` slot replaced.
#' - `Distribution()`, `Targeting()`, `Maximum()` return the corresponding
#'   slot from `x`.
#' - Their replacement forms return `x` with the corresponding slot updated.
#'
#' @seealso [effort-class], [Fleet()], [GenHistEffort()], [Catchability()]
#'
#' @examples
#' e <- Effort()
#' Units(e)
#' Distribution(e)
#'
#' @include class-unions.R
#' @name Effort
#' @export
Effort <- function(Effort       = NULL,
                   Units        = NULL,
                   Distribution = NULL,
                   Targeting    = NULL,
                   Maximum      = NULL,
                   Misc         = list(),
                   df           = FALSE) {
  
  if (inherits(Effort, c('fleet', 'effort', 'hist', 'obs', 'mse')))
    return(extract_effort(Effort, df))
  
  methods::new(
    "effort",
    Effort       = Effort,
    Units        = Units,
    Distribution = Distribution,
    Targeting    = Targeting,
    Maximum      = Maximum,
    Misc         = Misc
  )
}

#' @rdname Effort
#' @export
`Effort<-` <- function(x, value) {
  AssignSlot(x,value,'Effort')
}


#' @rdname Effort
#' @export
Distribution <- function(x) {
  CheckClass(x, "effort", "x")
  x@Distribution
}

#' @rdname Effort
#' @export
`Distribution<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Distribution <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Targeting <- function(x) {
  CheckClass(x, "effort", "x")
  x@Targeting
}

#' @rdname Effort
#' @export
`Targeting<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Targeting <- value
  methods::validObject(x)
  x
}

#' @rdname Effort
#' @export
Maximum <- function(x) {
  CheckClass(x, "effort", "x")
  x@Maximum
}

#' @rdname Effort
#' @export
`Maximum<-` <- function(x, value) {
  CheckClass(x, "effort", "x")
  x@Maximum <- value
  methods::validObject(x)
  x
}


