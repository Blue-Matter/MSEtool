#' The `catchdata` S4 Class
#'
#' Stores time series observations of landed or discarded catch and associated
#' uncertainty. Used in the `Landings` and `Discards` slots of a [data-class]
#' object. Objects are typically created via [CatchData()], which documents all
#' parameters in detail.
#'
#' @slot Name `character` or `NULL`. Fleet names, length `nFleet`.
#' @slot Value `array` or `NULL`. Observed catch values with dimensions
#'   `[nYear x nFleet]`. See [CatchData()].
#' @slot CV `array` or `NULL`. Coefficients of variation matching the
#'   dimensions of `Value`. See [CatchData()].
#' @slot Units `character` or `NULL`. Units of catch measurement per fleet
#'   (e.g., `"t"` for tonnes, `"numbers"`). See [CatchData()].
#' @slot Ref `array` or `NULL`. Reference catch values (e.g., a historical
#'   baseline), matching the dimensions of `Value`. See [CatchData()].
#' @slot RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. See [CatchData()].
#'
#' @seealso [CatchData()] for the constructor and full parameter documentation.
#'   [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name catchdata-class
setClass(
  "catchdata",
  slots = c(
    Name  = "char.null",
    Value = "array.null",
    CV    = "array.null",
    Units = "char.null",
    Ref   = "array.null",
    RefCV = "array.null"
  )
)


#' The `effortdata` S4 Class
#'
#' Stores time series observations of fishing effort and associated uncertainty.
#' Used in the `Effort` slot of a [data-class] object. Objects are typically
#' created via [EffortData()], which documents all parameters in detail.
#'
#' @slot Name `character` or `NULL`. Names of the fleets
#' @slot Value `array` or `NULL`. Observed effort values with dimensions
#'   `[nYear x nFleet]`. See [EffortData()].
#' @slot CV `array` or `NULL`. Coefficients of variation matching the
#'   dimensions of `Value`. See [EffortData()].
#' @slot Units `character` or `NULL`. Units of effort measurement per fleet
#'   (e.g., `"days"`, `"trips"`, `"unitless"`). See [EffortData()].
#'
#' @seealso [EffortData()] for the constructor and full parameter documentation.
#'   [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name effortdata-class
NULL

setClass(
  "effortdata",
  slots = c(
    Name  = "char.null",
    Value = "array.null",
    CV    = "array.null",
    Units = "char.null"
  )
)


#' The `indicesdata` S4 Class
#'
#' Stores abundance or biomass index observations from fishery-dependent
#' (CPUE) or fishery-independent (Survey) sources, including observation
#' uncertainty, reference values, timing, and selectivity mapping. Used in
#' the `CPUE` and `Survey` slots of a [data-class] object. Objects are
#' typically created via [IndicesData()], which documents all parameters in
#' detail.
#'
#' @slot Name `character` or `NULL`. Name of the index. See [IndicesData()].
#' @slot Value `array` or `NULL`. Observed index values with dimensions
#'   `[nYear x nIndex]`. See [IndicesData()].
#' @slot CV `array` or `NULL`. Coefficients of variation for the index
#'   observations, matching the dimensions of `Value`. See [IndicesData()].
#' @slot Units `character` or `NULL`. Units of the index (e.g., `"kg/trip"`,
#'   `"numbers/tow"`). See [IndicesData()].
#' @slot Ref `numeric` or `NULL`. Reference value for each index (e.g., a
#'   historical mean or target level). See [IndicesData()].
#' @slot RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. See [IndicesData()].
#' @slot Timing `numeric`. Timing of each observation as a fraction of the
#'   time step (0-1). Simulated
#'   indices are decayed by the mortality accrued up to that point;
#'   `0` (default) observes the population at the start of the time step.
#'   See [IndicesData()].
#' @slot Selectivity An array or character specification mapping each index
#'   to a fleet selectivity or defining an independent selectivity curve.
#'   See [IndicesData()].
#' @slot Misc A named list for any additional index-level metadata.
#'
#' @seealso [IndicesData()] for the constructor and full parameter
#'   documentation. [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name indicesdata-class
setClass(
  "indicesdata",
  slots = c(
    Name        = "char.null",
    Value       = "array.null",
    CV          = "array.null",
    Units       = "char.null",
    Ref         = "num.null",
    RefCV       = "array.null",
    Timing      = "numeric",
    Selectivity = "array.char.num",
    Misc        = "list"
  )
)


#' The `compdata` S4 Class
#'
#' Stores age or size composition observations of catch samples.
#' Used in the `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, and
#' `DiscardsAtSize` slots of a [data-class] object. Objects are typically
#' created via [CompData()], which documents all parameters in detail.
#'
#' @slot Name `character` or `NULL`. Fleet names, length `nFleet`.
#' @slot Value `array` or `NULL`. Composition counts with dimensions
#'   `[nYear x nFleet x nClass]`. For age compositions the third dimension
#'   indexes a class grid shared by every fleet (see `Classes`). For size
#'   compositions the third dimension is a *fleet-relative position*, not a
#'   shared bin: fleets are not required to use the same size-class grid, so
#'   `nClass` is the largest class count among the object's fleets, and a
#'   fleet with fewer classes has unused (`NA`) cells at the end of its
#'   slice. See [CompData()].
#' @slot Classes `numeric`, `list`, or `NULL`. For age compositions, a
#'   numeric vector of class values (ages in years) shared by every fleet,
#'   length matching `dim(Value)[3]`. For size compositions, a `list` of
#'   numeric vectors, one per fleet (named to match `Name`), each giving
#'   that fleet's own lower bounds of size bins in the appropriate unit; bin
#'   `k` of fleet `fl` spans `[Classes[[fl]][k], Classes[[fl]][k+1])`, and
#'   `length(Classes[[fl]])` may be less than `dim(Value)[3]` (the unused
#'   tail of that fleet's slice in `Value`). See [CompData()].
#' @slot Units `character` or `NULL`. Units of the class variable
#'   (e.g., `"years"`, `"cm"`, `"mm"`), shared by every fleet. See
#'   [CompData()].
#' @slot Log `list`. Internal named list storing diagnostics, warnings, and
#'   assumptions recorded during processing. See [Log()]. Not intended for
#'   direct user access.
#' @slot Misc `list`. Named list for additional composition-level metadata.
#'
#' @seealso
#' - [CompData()] for the constructor and full parameter documentation.
#' - [data-class] for the enclosing data object.
#'
#' @family data
#'
#' @include class-unions.R
#' @name compdata-class
NULL

setClass(
  "compdata",
  slots = c(
    Name    = "char.null",
    Value   = "array.null",
    Classes = "num.list.null",
    Units   = "char.null",
    Log     = "list",
    Misc    = "list"
  )
)

methods::setValidity("compdata", function(object) {
  classes <- object@Classes
  value   <- object@Value

  if (is.null(classes) || is.null(value))
    return(TRUE)

  nClass <- dim(value)[3]

  if (is.list(classes)) {
    nFleet <- dim(value)[2]
    if (length(classes) != nFleet)
      return(sprintf(
        "`Classes` has %d fleet%s but `Value` has %d fleet%s.",
        length(classes), if (length(classes) == 1) '' else 's',
        nFleet, if (nFleet == 1) '' else 's'
      ))
    tooLong <- lengths(classes) > nClass
    if (any(tooLong))
      return(sprintf(
        "`Classes[[%s]]` has more classes than `Value`'s Class dimension (%d).",
        paste(which(tooLong), collapse = ', '), nClass
      ))
    return(TRUE)
  }

  if (length(classes) != nClass)
    return(sprintf(
      "`Classes` has length %d but `Value`'s Class dimension has length %d.",
      length(classes), nClass
    ))

  TRUE
})


#' `lifehistorydata` Class
#'
#' Groups the biological life-history components required to describe
#' population dynamics. Each slot holds a dedicated sub-object defining
#' the model and parameters for that process. Used in the `LifeHistory` slot
#' of a [data-class] object.
#'
#' @slot Ages An object of class [ages-class] defining the age structure of the
#'   population (minimum age, maximum age, plus-group).
#' @slot Length An object of class [length-class] defining the growth model
#'   (e.g., von Bertalanffy) and length-at-age parameters.
#' @slot Weight An object of class [weight-class] defining the weight-at-age or
#'   length–weight relationship.
#' @slot NaturalMortality An object of class [naturalmortality-class] defining
#'   natural mortality rates, which may be age-, size-, or time-varying.
#' @slot Maturity An object of class [maturity-class] defining maturity-at-age or
#'   maturity-at-length schedules.
#' @slot Fecundity An object of class [fecundity-class] defining fecundity-at-age
#'   or fecundity-at-length relationships.
#' @slot SRR An object of class [srr-class] defining the stock–recruitment
#'   relationship (e.g., Beverton–Holt, Ricker) and associated parameters.
#' @slot Spatial An object of class [spatial-class] defining the spatial structure
#'   of the population, including movement and area allocation.
#' @slot Depletion An object of class [depletion-class] specifying the initial
#'   depletion level relative to unfished biomass.
#' @slot Misc A named list for any additional life-history metadata.
#'
#' `LifeHistoryData()` creates a new `lifehistorydata` object.
#'
#' @return `LifeHistoryData()` returns a `lifehistorydata` object.
#' @name lifehistorydata-class
#' @seealso [data-class], [Data()]
#' @include class-unions.R
#' @include class-stock.R
#'
#' @export
setClass(
  "lifehistorydata",
  slots = c(
    Ages              = "ages",
    Length            = "length",
    Weight            = "weight",
    NaturalMortality  = "naturalmortality",
    Maturity          = "maturity",
    Fecundity         = "fecundity",
    SRR               = "srr",
    Spatial           = "spatial",
    Depletion         = "depletion",
    Misc              = "list"
  )
)

#' @rdname lifehistorydata-class
#' @export
LifeHistoryData <- function() {
  new('lifehistorydata')
}


#' `exploitationdata` Class
#'
#' Groups the fleet-specific exploitation process components required to
#' describe how fishing mortality is distributed across the population.
#' Used in the `Exploitation` slot of a [data-class] object.
#'
#' @slot Selectivity An object of class [selectivity-class] defining the
#'   age- or length-based selectivity pattern for each fleet.
#' @slot Retention An object of class [retention-class] defining the probability
#'   of retaining a fish of a given age or length once caught.
#' @slot DiscardMortality An object of class [discardmortality-class] defining
#'   the mortality rate of discarded fish by age or length.
#' @slot Misc A named list for any additional exploitation-level metadata.
#'
#' `ExploitationData()` creates a new `exploitationdata` object.
#'
#' @return `ExploitationData()` returns a `exploitationdata` object.
#' @name exploitationdata-class
#' @seealso [data-class], [Data()]
#' @include class-unions.R
#' @include class-selectivity.R
#' @include class-retention.R
#' @include class-discardmortality.R
#'
#' @export
setClass(
  "exploitationdata",
  slots = c(
    Selectivity      = "selectivity",
    Retention        = "retention",
    DiscardMortality = "discardmortality",
    Misc             = "list"
  )
)

#' @rdname exploitationdata-class
#' @export
ExploitationData <- function() {
  new('exploitationdata')
}


#' `referencedata` Class
#'
#' Stores biological and management reference points by extending
#' [refpointsMSY-class]. Used in the `Reference` slot of a [data-class] object.
#'
#' This class inherits all slots from [refpointsMSY-class] (e.g., MSY, FMSY,
#' BMSY) and appends a `Misc` slot for any additional reference point data not
#' covered by the parent class.
#'
#' @slot Misc A named list for any additional reference point metadata not
#'   covered by the inherited [refpointsMSY-class] slots.
#'
#' `ReferenceData()` creates a new `referencedata` object.
#'
#' @return `ReferenceData()` returns a `referencedata` object.
#'
#' @seealso [data-class], [Data()], [refpointsMSY-class]
#' @include class-refpointsMSY.R
#' @name referencedata-class
#' @export
setClass(
  "referencedata",
  slots = c(
    Misc = "list"
  ),
  contains = "refpointsMSY"
)

#' @rdname referencedata-class
#' @export
ReferenceData <- function() {
  new('referencedata')
}


#' The `advicedata` S4 Class
#'
#' Stores management advice outputs generated during a model run or provided
#' as input. Used in the `Advice` slot of a [data-class] object and accessed
#' by [LastTAC()] when retrieving the most recent catch limit.
#'
#' @slot TAC A numeric vector, array, or list of Total Allowable Catch (TAC)
#'   values. When a vector, elements correspond to successive advice years.
#' @slot Effort A numeric vector, array, or list of advised fishing effort
#'   values. Structured analogously to `TAC`.
#' @slot Misc A named list for any additional advice-level metadata (e.g.,
#'   harvest control rule outputs, reference point comparisons).
#'
#' `AdviceData()` creates a new `advicedata` object.
#'
#' @return `AdviceData()` returns a `advicedata` object.
#'
#' @seealso [data-class], [Data()], [LastTAC()]
#' @include class-unions.R
#' @name advicedata-class
#' @aliases advicedata
#' @export
setClass(
  "advicedata",
  slots = c(
    TAC    = "num.array.list",
    Effort = "num.array.list",
    Misc   = "list"
  )
)

#' @rdname advicedata-class
#' @export
AdviceData <- function() {
  new('advicedata')
}


#' The `data` S4 Class
#'
#' The `data` class stores observed or simulated fishery data used by an
#' [OM()] object. Data may include life-history information, exploitation
#' patterns, catches, indices, age and size compositions, and management advice.
#'
#' Objects should be created with the [Data()] constructor, which initialises
#' all sub-object slots to empty objects of the appropriate class when not
#' supplied.
#'
#' @slot Name Optional character string. Name of the data object.
#' @slot CommonName Optional character string. Common name of the stock.
#' @slot Species Optional character string. Scientific name of the species.
#' @slot Agency Optional character string. Name of the managing agency.
#' @slot Author Optional character string. Name(s) of the data author(s).
#' @slot Email Optional character string. Contact email(s) for the author(s).
#' @slot Region Optional character string. Geographic region of the stock.
#' @slot Latitude Optional numeric. Latitude of the stock in decimal degrees.
#' @slot Longitude Optional numeric. Longitude of the stock in decimal degrees.
#'
#' @slot Years Numeric vector of calendar years covered by the data.
#' @slot YearLH Numeric. The last historical calendar year (always a whole
#'   integer, even when `Seasons > 1` and `Years` holds sub-annual decimal
#'   steps), separating the historical period from the projection period.
#'   Defaults to `floor(max(Years))` when not supplied to [Data()].
#' @slot Seasons Positive integer. Number of seasons per year. Defaults to `1`.
#' @slot nArea Positive integer. Number of spatial areas. Defaults to `1`.
#'
#' @slot LifeHistory An object of class [lifehistorydata-class] containing
#'   biological parameters such as growth, maturity, and natural mortality.
#' @slot Exploitation An object of class [exploitationdata-class] containing
#'   selectivity, retention, and discard mortality parameters.
#' @slot Reference An object of class [referencedata-class] containing biological
#'   reference points such as unfished biomass and MSY-based quantities.
#'
#' @slot Effort An object of class [effortdata-class] containing fishing effort
#'   time series.
#'
#' @slot Landings An object of class [catchdata-class] containing landed catch
#'   time series.
#' @slot Discards An object of class [catchdata-class] containing discarded catch
#'   time series.
#'
#' @slot CPUE An object of class [indicesdata-class] containing catch-per-unit-effort
#'   indices.
#' @slot Survey An object of class [indicesdata-class] containing fishery-independent
#'   survey indices.
#'
#' @slot LandingsAtAge An object of class [compdata-class] containing age composition
#'   of landed catch.
#' @slot DiscardsAtAge An object of class [compdata-class] containing age composition
#'   of discarded catch.
#' @slot LandingsAtSize An object of class [compdata-class] containing size
#'   composition of landed catch.
#' @slot DiscardsAtSize An object of class [compdata-class] containing size
#'   composition of discarded catch.
#'
#' @slot Advice An object of class [advicedata-class] containing TAC recommendations
#'   and related management advice.
#'
#' @slot Misc A named list for any additional user-defined data. Defaults to
#'   `list()`.
#' @slot Log `list`. Internal named list storing diagnostics, warnings, and
#'   assumptions recorded during processing. See [Log()]. Not intended for
#'   direct user access.
#'
#' @seealso [Data()]
#' @name data-class
#' @export
setClass(
  "data",
  slots = c(
    Name = "char.null",
    CommonName = "char.null",
    Species = "char.null",
    Agency = "char.null",
    Author = "char.null",
    Email = "char.null",
    Region = "char.null",
    Latitude = "num.null",
    Longitude = "num.null",

    Years = "num.null",
    YearLH = "num.null",
    Seasons = "num.null",
    nArea = "num.null",

    LifeHistory = "lifehistorydata",
    Exploitation = "exploitationdata",
    Reference = "referencedata",

    Effort = "effortdata",

    Landings = "catchdata",
    Discards = "catchdata",

    CPUE = "indicesdata",
    Survey = "indicesdata",

    LandingsAtAge = "compdata",
    DiscardsAtAge = "compdata",

    LandingsAtSize = "compdata",
    DiscardsAtSize = 'compdata',

    Advice = "advicedata",
    Log = "list",
    Misc = "list"
  )
)
