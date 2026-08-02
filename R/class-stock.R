#' The `stock` S4 Class
#'
#' Defines the biological and population-dynamics properties of a stock for use
#' in an operating model. Objects are typically created via [Stock()], which
#' documents all parameters in detail and initialises sub-objects automatically.
#'
#' @slot Name `character`. Unique stock identifier.
#' @slot CommonName `character`. Common name of the species.
#' @slot Species `character`. Scientific (Latin) name of the species.
#' @slot Ages An [ages-class] object defining the age structure. See [Ages()].
#' @slot Length A [length-class] object defining the length-at-age schedule.
#'   See [Length()].
#' @slot Weight A [weight-class] object defining the weight-at-age and
#'   weight-at-length schedules. See [Weight()].
#' @slot NaturalMortality A [naturalmortality-class] object defining the
#'   natural mortality schedule. See [NaturalMortality()].
#' @slot Maturity A [maturity-class] object defining the maturity schedule.
#'   See [Maturity()].
#' @slot Fecundity A [fecundity-class] object defining egg production as a
#'   function of age or length. Optional — when empty, spawning production
#'   (`SProduction`) equals spawning biomass (`SBiomass`). See [Fecundity()].
#' @slot SRR A [srr-class] object defining the stock-recruitment relationship
#'   and recruitment variability. See [SRR()] and [SRRModels()].
#' @slot Spatial A [spatial-class] object defining spatial structure and
#'   movement dynamics. Optional — when empty, a single well-mixed area is
#'   assumed. See [Spatial()].
#' @slot Depletion A [depletion-class] object defining depletion assumptions
#'   at the start (`Initial`) and/or end (`Final`) of the historical period.
#'   Optional — when empty, the stock starts unfished and terminal depletion
#'   is determined by the [Fleet()] and [Catchability()] parameters.
#'   See [Depletion()].
#' @slot nYear `numeric(1)`. Number of historical years. Initialised to `20`
#'   by [Stock()] and overridden when attached to an [OM()].
#' @slot pYear `numeric(1)`. Number of projection years. Initialised to `30`
#'   by [Stock()] and overridden when attached to an [OM()].
#' @slot nSim `numeric(1)`. Number of stochastic simulations. Initialised to
#'   `48` by [Stock()] and overridden when attached to an [OM()].
#' @slot CurrentYear `numeric(1)`. Final calendar year of the historical
#'   period. Initialised to the current system year by [Stock()] and
#'   overridden when attached to an [OM()].
#' @slot Years `numeric`. Vector of all model years (historical + projection),
#'   derived from `nYear`, `pYear`, `CurrentYear`, and `Seasons`. Overridden
#'   when attached to an [OM()].
#' @slot Seasons `numeric(1)`. Number of seasons per calendar year.
#' @slot Misc `list`. Used internally.
#' @slot Log `list`. Internal named list storing diagnostics, warnings, and
#'   assumptions recorded during processing. See [Log()]. Not intended for
#'   direct user access.
#'
#' @details
#' This class is the central biological object in the operating model framework.
#' The bookkeeping slots `nYear`, `pYear`, `nSim`, `CurrentYear`, and `Years`
#' are populated with working defaults by [Stock()] and overridden automatically
#' when the stock is attached to an [OM()]; they need not be set manually.
#'
#' Direct construction via [methods::new()] is not recommended; use [Stock()]
#' instead, which validates inputs and initialises all sub-objects.
#'
#' @seealso
#' - [Stock()] for the constructor and accessor functions.
#' - [Ages()], [Length()], [Weight()], [NaturalMortality()], [Maturity()],
#'   [Fecundity()], [SRR()], [Spatial()], [Depletion()] for the sub-object
#'   constructors.
#' - [OM()] for the operating model constructor.
#' - [Specifying Biological and Fleet Schedules](https://docs.openmse.com/concept-schedules.html) for how
#'   `Pars`, `Model`, and `MeanAt*` arrays are specified across sub-objects.
#'
#' @family stock
#'
#' @include class-unions.R
#' @include class-ages.R
#' @include class-length.R
#' @include class-weight.R
#' @include class-naturalmortality.R
#' @include class-maturity.R
#' @include class-fecundity.R
#' @include class-srr.R
#' @include class-spatial.R
#' @include class-depletion.R
#' @name stock-class
#' @rdname stock-class
setClass(
  "stock",
  slots = c(
    Name             = "char.null",
    CommonName       = "char.null",
    Species          = "char.null",
    Ages             = "ages",
    Length           = "length",
    Weight           = "weight",
    NaturalMortality = "naturalmortality",
    Maturity         = "maturity",
    Fecundity        = "fecundity",
    SRR              = "srr",
    Spatial          = "spatial",
    Depletion        = "depletion",
    nYear            = "num.null",
    pYear            = "num.null",
    nSim             = "num.null",
    CurrentYear      = "num.null",
    Years            = "num.null",
    Seasons          = "num.null",
    Misc             = "list",
    Log              = "list"
  )
)

setValidity("stock", function(object) {
  # TODO: structural consistency checks
  TRUE
})
