#' CatchData Constructor
#'
#' Construct a [catchdata-class] object storing observed landed or discarded
#' catch and associated uncertainty.
#'
#' @param Name `character` or `NULL`. Fleet names, length `nFleet`.
#'   Default `NULL`.
#' @param Value `array` or `NULL`. Observed catch values with dimensions
#'   `[nYear x nFleet]`. Default `NULL`.
#' @param CV `array` or `NULL`. Coefficients of variation for the catch
#'   observations, matching the dimensions of `Value`. Default `NULL`.
#' @param Units `character` or `NULL`. Units of catch measurement, one element
#'   per fleet: `"Biomass"` or `"Number"`. Default `NULL`. See [CheckCatch()]
#'   for the consequences of `Landings` and `Discards` having mismatched
#'   units.
#' @param Ref `array` or `NULL`. Reference catch values (e.g., a historical
#'   baseline), matching the dimensions of `Value`. Default `NULL`.
#' @param RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. Default `NULL`.
#'
#' @details
#' ## Usage
#'
#' All arguments are optional; an empty object is valid and slots are
#' populated later (e.g., by `.GenHistDataCatch()`).
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' Landings(data) <- CatchData(Value = myArray, CV = myCVArray)
#' Discards(data) <- CatchData(Value = myArray, CV = myCVArray)
#' ```
#'
#' @return A [catchdata-class] object.
#'
#' @seealso
#'
#' - [catchdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [CatchObs()] for the observation error structure that generates
#'   [catchdata-class] objects during simulation.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' cd <- CatchData()
#'
#' # With values
#' yrs <- 2000:2020
#' fleets <- c("Trawl", "Longline")
#' val <- array(runif(length(yrs) * length(fleets)),
#'              dim = c(length(yrs), length(fleets)),
#'              dimnames = list(Year = yrs, Fleet = fleets))
#' cd <- CatchData(Name  = fleets,
#'                 Value = val,
#'                 CV    = array(0.1, dim = dim(val), dimnames = dimnames(val)),
#'                 Units = c("Biomass", "Biomass"))
#'
#' @export
CatchData <- function(Name  = NULL,
                      Value = NULL,
                      CV    = NULL,
                      Units = NULL,
                      Ref   = NULL,
                      RefCV = NULL) {
  .Object <- methods::new("catchdata")
  if (!is.null(Name))  .Object@Name  <- Name
  if (!is.null(Value)) .Object@Value <- Value
  if (!is.null(CV))    .Object@CV    <- CV
  if (!is.null(Units)) .Object@Units <- Units
  if (!is.null(Ref))   .Object@Ref   <- Ref
  if (!is.null(RefCV)) .Object@RefCV <- RefCV
  methods::validObject(.Object)
  .Object
}


#' CompData Constructor
#'
#' Construct a [compdata-class] object storing age or size composition
#' observations of catch samples.
#'
#' @param Name `character` or `NULL`. Fleet names, length `nFleet`.
#'   Default `NULL`.
#' @param Value `array` or `NULL`. Composition counts with dimensions
#'   `[nYear x nFleet x nClass]`. Values represent counts scaled to sum to
#'   `SampleSize` within each year and fleet — see [CompObs()] for the
#'   simulation model. Default `NULL`.
#' @param Classes `numeric`, `list`, or `NULL`. For age compositions, a
#'   numeric vector of class values shared by every fleet, length matching
#'   `dim(Value)[3]`. For size compositions, a `list` of numeric vectors,
#'   one per fleet (named to match `Name`), since fleets are not required to
#'   share a size-class grid -- see [compdata-class]. Default `NULL`.
#' @param Units `character` or `NULL`. Units of the class variable
#'   (e.g., `"years"`, `"cm"`, `"mm"`). Default `NULL`.
#' @param Log `list`. Named list for diagnostic and audit logging.
#'   Default `list()`.
#' @param Misc `list`. Named list for additional metadata. Default `list()`.
#'
#' @include generate-data-hist-sizecomp.R
#'
#' @details
#' ## Usage
#'
#' All arguments are optional; an empty object is valid and slots are
#' populated later (e.g., by `.GenHistDataAgeComp()` or
#' `.GenHistDataSizeComp()`.
#'
#' ## Value Dimensions and Units
#'
#' `Value` stores counts, compositions are normalised
#' to sum to `SampleSize` within each year-fleet combination. The
#' simulation model that generates `Value` is defined in [CompObs()].
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' LandingsAtAge(data)  <- CompData(Value = myArray, Classes = ages)
#' DiscardsAtAge(data)  <- CompData(Value = myArray, Classes = ages)
#'
#' # Size compositions: Classes is a list, one vector per fleet, since
#' # fleets need not share a size-class grid.
#' LandingsAtSize(data) <- CompData(Value = myPaddedArray,
#'                                  Classes = list(Trawl = bins1, Longline = bins2))
#' DiscardsAtSize(data) <- CompData(Value = myPaddedArray,
#'                                  Classes = list(Trawl = bins1, Longline = bins2))
#' ```
#'
#' @return A [compdata-class] object.
#'
#' @seealso
#'
#' - [compdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [CompObs()] for the observation error structure that generates
#'   [compdata-class] objects during simulation.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' cd <- CompData()
#'
#' # With values
#' yrs    <- 2000:2020
#' ages   <- 1:10
#' fleets <- c("Trawl", "Longline")
#' val <- array(0L,
#'              dim = c(length(yrs), length(fleets), length(ages)),
#'              dimnames = list(Year = yrs, Fleet = fleets, Age = ages))
#' cd <- CompData(Name    = fleets,
#'                Value   = val,
#'                Classes = ages,
#'                Units   = "years")
#' @name CompData
#' @export
CompData <- function(Name    = NULL,
                     Value   = NULL,
                     Classes = NULL,
                     Units   = NULL,
                     Log     = list(),
                     Misc    = list()) {
  .Object <- methods::new("compdata")
  if (!is.null(Name))    .Object@Name    <- Name
  if (!is.null(Value))   .Object@Value   <- Value
  if (!is.null(Classes)) .Object@Classes <- Classes
  if (!is.null(Units))   .Object@Units   <- Units
  .Object@Log  <- Log
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}


#' EffortData Constructor and Accessors
#'
#' Construct an [effortdata-class] object storing observed fishing effort and
#' uncertainty, or access and replace individual slots.
#'
#' @param Name `character` or `NULL`. Name of the dataset. Default `NULL`.
#' @param Value `array` or `NULL`. Observed effort values with dimensions
#'   `[nYear x nFleet]`. Default `NULL`.
#' @param CV `array` or `NULL`. Coefficients of variation for the effort
#'   observations, matching the dimensions of `Value`. Default `NULL`.
#' @param Units `character` or `NULL`. Units of effort measurement, one
#'   element per fleet (e.g., `"days"`, `"trips"`, `"unitless"`).
#'   Default `NULL`.
#'
#' @details
#' ## Usage
#'
#' `EffortData()` is the standard constructor. All arguments are optional;
#' an empty object is valid and slots are populated later (e.g., by
#' `.GenHistDataEffort()`).
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' Effort(data) <- EffortData(Value = myArray, CV = myCVArray)
#' ```
#'
#' @return
#'
#' - `EffortData()` returns an [effortdata-class] object.
#' - `Effort()` returns the `Effort` slot from a [data-class] object `x`.
#' - `Effort<-` returns `x` with the `Effort` slot replaced.
#'
#' @seealso
#'
#' - [effortdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [EffortObs()] for the observation error structure that generates
#'   [effortdata-class] objects during simulation.
#' - `.GenHistDataEffort()` for how `Value`, `CV`, and `Units` are populated
#'   from the operating model history.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' ed <- EffortData()
#'
#' # With values
#' yrs <- 2000:2020
#' nFleet <- 2
#' val <- array(runif(length(yrs) * nFleet),
#'              dim = c(length(yrs), nFleet),
#'              dimnames = list(Year = yrs, Fleet = c("Trawl", "Longline")))
#' ed <- EffortData(Value = val,
#'                  CV    = array(0.2, dim = dim(val), dimnames = dimnames(val)),
#'                  Units = c("hours", "hooks"))
#'
#' @name EffortData
#' @export
EffortData <- function(Name  = NULL,
                       Value = NULL,
                       CV    = NULL,
                       Units = NULL) {
  .Object <- methods::new("effortdata")
  if (!is.null(Name))  .Object@Name  <- Name
  if (!is.null(Value)) .Object@Value <- Value
  if (!is.null(CV))    .Object@CV    <- CV
  if (!is.null(Units)) .Object@Units <- Units
  methods::validObject(.Object)
  .Object
}


#' IndicesData Constructor
#'
#' Construct an [indicesdata-class] object storing an abundance or biomass
#' index and associated uncertainty.
#'
#' @param Name `character` or `NULL`. Name of the index. Default `NULL`.
#' @param Value `array` or `NULL`. Observed index values with dimensions
#'   `[nYear x nIndex]`. Default `NULL`.
#' @param CV `array` or `NULL`. Coefficients of variation for the index
#'   observations, matching the dimensions of `Value`. Default `NULL`.
#' @param Units `character` or `NULL`. Units of the index: `"Biomass"`,
#'   `"Number"`, or `"Recruitment"`. Default `NULL`.
#' @param Ref `numeric` or `NULL`. Reference value for each index (e.g., a
#'   historical mean or target level), length `nIndex`. Default `NULL`.
#' @param RefCV `array` or `NULL`. Coefficients of variation for the reference
#'   values, matching the dimensions of `Ref`. Default `NULL`.
#' @param Timing `numeric`. Timing of each observation as a fraction of the
#'   time step (0-1), length `nIndex`. Simulated indices are decayed by the mortality
#'   accrued up to that point. Default `0`, the start of the time step.
#' @param Selectivity An array or character specification mapping each index
#'   to a fleet selectivity or defining an independent selectivity curve, or
#'   `NULL`. Default `NULL`.
#' @param Misc `list`. Named list for additional metadata. Default `list()`.
#'
#' @details
#' ## Usage
#'
#' All arguments are optional; an empty object is valid and slots are
#' populated later (e.g., by `.GenHistDataIndices()`).
#'
#' ## Attaching to a Data Object
#'
#' ```r
#' CPUE(data)   <- IndicesData(Value = myArray, CV = myCVArray)
#' Survey(data) <- IndicesData(Value = myArray, CV = myCVArray)
#' ```
#'
#' @return An [indicesdata-class] object.
#'
#' @seealso
#'
#' - [indicesdata-class] for the class definition.
#' - [data-class] and [Data()] for the enclosing data object.
#' - [IndicesObs()] for the observation error structure that generates
#'   [indicesdata-class] objects during simulation.
#'
#' @family data
#'
#' @examples
#' # Empty object
#' id <- IndicesData()
#'
#' # With values
#' yrs <- 2000:2020
#' val <- array(runif(length(yrs)),
#'              dim = c(length(yrs), 1),
#'              dimnames = list(Year = yrs, Index = "Survey1"))
#' id <- IndicesData(Name  = "Survey1",
#'                   Value = val,
#'                   CV    = array(0.2, dim = dim(val), dimnames = dimnames(val)),
#'                   Units = "Biomass")
#'
#' @export
IndicesData <- function(Name        = NULL,
                        Value       = NULL,
                        CV          = NULL,
                        Units       = NULL,
                        Ref         = NULL,
                        RefCV       = NULL,
                        Timing      = 0,
                        Selectivity = NULL,
                        Misc        = list()) {
  .Object <- methods::new("indicesdata")
  if (!is.null(Name))        .Object@Name        <- Name
  if (!is.null(Value))       .Object@Value       <- Value
  if (!is.null(CV))          .Object@CV          <- CV
  if (!is.null(Units))       .Object@Units       <- Units
  if (!is.null(Ref))         .Object@Ref         <- Ref
  if (!is.null(RefCV))       .Object@RefCV       <- RefCV
  if (!is.null(Timing))      .Object@Timing      <- Timing
  if (!is.null(Selectivity)) .Object@Selectivity <- Selectivity
  .Object@Misc <- Misc
  methods::validObject(.Object)
  .Object
}


#' Constructor and Accessor for `Data`
#'
#' Creates a new [data-class] object, or extracts the `Data` or `PPD` slot from
#' an existing [om-class], [hist-class], or [mse-class] class object.
#'
#' When `Name` is an [om-class] or [hist-class] object, the function returns the
#' corresponding `@@Data` slot. When `Name` is an [mse-class] object, the `@@PPD`
#' slot is returned instead. Otherwise, a new `data` object is constructed from
#' the supplied arguments.
#'
#' All slot arguments default to `NULL`, in which case an empty sub-object of
#' the appropriate class is initialised automatically:
#'
#' - `LifeHistory` → [lifehistorydata-class]
#' - `Exploitation` → [exploitationdata-class]
#' - `Reference` → [referencedata-class]
#' - `Effort` → [effortdata-class]
#' - `Landings`, `Discards` → [catchdata-class]
#' - `CPUE`, `Survey` → [indicesdata-class]
#' - `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, `DiscardsAtSize` → [compdata-class]
#' - `Advice` → [advicedata-class]
#'
#' @param Name Either a character string naming the new [data-class] object, or an
#'   existing [om-class], [hist-class], or [mse-class] object from which to extract data.
#'   Defaults to `"New Data Object"`.
#' @param CommonName Optional character string. Common name of the stock.
#' @param Species Optional character string. Scientific name of the species.
#' @param Agency Optional character string. Name of the managing agency.
#' @param Author Optional character string. Name of the data author.
#' @param Email Optional character string. Contact email for the author.
#' @param Region Optional character string. Geographic region of the stock.
#' @param Latitude Optional numeric. Latitude of the stock.
#' @param Longitude Optional numeric. Longitude of the stock.
#' @param Years Vector of calendar years covered by the data. **Required**
#' @param YearLH The last historical calendar year; always a whole integer,
#'   even when `Seasons > 1` and `Years` holds sub-annual decimal steps.
#'   Separates the historical period from the projection period. Defaults to
#'   `floor(max(Years))`.
#' @param Seasons A positive integer giving the number of seasons per year.
#'   Defaults to `1`.
#' @param nArea A positive integer giving the number of spatial areas.
#'   Defaults to `1`.
#' @param LifeHistory Optional. An object of class [lifehistorydata-class] .
#' @param Exploitation Optional. An object of class [exploitationdata-class].
#' @param Reference Optional. An object of class [referencedata-class].
#' @param Effort Optional. An object of class [effortdata-class].
#' @param Landings Optional. An object of class [catchdata-class] for landed catch.
#' @param Discards Optional. An object of class [catchdata-class] for discarded
#'   catch.
#' @param CPUE Optional. An object of class [indicesdata-class] for catch-per-unit-
#'   effort indices.
#' @param Survey Optional. An object of class [indicesdata-class] for fishery-
#'   independent survey indices.
#' @param LandingsAtAge Optional. An object of class [compdata-class] for age
#'   composition of landings.
#' @param DiscardsAtAge Optional. An object of class [compdata-class] for age
#'   composition of discards.
#' @param LandingsAtSize Optional. An object of class [compdata-class] for size
#'   composition of landings.
#' @param DiscardsAtSize Optional. An object of class [compdata-class] for size
#'   composition of discards.
#' @param Advice Optional. An object of class [advicedata-class] containing TAC and
#'   related advice.
#' @param Misc A named list for any additional user-defined data. Defaults to
#'   `list()`.
#' @param x An [om-class] object, for use with `Data<-`.
#' @param value A [data-class] object, or a list of [data-class] objects for
#'   multi-stock operating models, to assign to the `Data` slot.
#'
#' @return A [data-class] object, or when `Name` is an [mse-class] object, a list
#'   of [data-class] objects from the `@@PPD` slot. `Data<-` returns `x` with
#'   the `Data` slot replaced by `value`.
#'
#' @seealso [data-class], [LastTAC()], [LastHistYearInd()], [ProjectionYear()]
#' @name Data
#' @export
Data <- function(Name = 'New Data Object',
                 CommonName = NULL,
                 Species = NULL,
                 Agency = NULL,
                 Author = NULL,
                 Email = NULL,
                 Region = NULL,
                 Latitude = NULL,
                 Longitude = NULL,
                 Years = NULL,
                 YearLH  = NULL,
                 Seasons = 1,
                 nArea = 1,
                 LifeHistory = NULL,
                 Exploitation = NULL,
                 Reference = NULL,
                 Effort = NULL,
                 Landings = NULL,
                 Discards = NULL,
                 CPUE = NULL,
                 Survey = NULL,
                 LandingsAtAge = NULL,
                 DiscardsAtAge = NULL,
                 LandingsAtSize = NULL,
                 DiscardsAtSize = NULL,
                 Advice = NULL,
                 Misc = list()
) {

  classes <- c('om', 'hist')

  if (inherits(Name, classes))
    return(Name@Data)

  if (inherits(Name, 'mse'))
    return(Name@PPD)

  if (is.null(LifeHistory))
    LifeHistory <- new('lifehistorydata')

  if (is.null(Exploitation))
    Exploitation <- new('exploitationdata')

  if (is.null(Reference))
    Reference <- new('referencedata')

  if (is.null(Effort))
    Effort <- new('effortdata')

  if (is.null(Landings))
    Landings <- new('catchdata')

  if (is.null(Discards))
    Discards <- new('catchdata')

  if (is.null(CPUE))
    CPUE <- new('indicesdata')

  if (is.null(Survey))
    Survey <- new('indicesdata')

  if (is.null(LandingsAtAge))
    LandingsAtAge <- new('compdata')

  if (is.null(DiscardsAtAge))
    DiscardsAtAge <- new('compdata')

  if (is.null(LandingsAtSize))
    LandingsAtSize <- new('compdata')

  if (is.null(DiscardsAtSize))
    DiscardsAtSize <- new('compdata')

  if (is.null(Advice))
    Advice <- new('advicedata')

  if (!is.null(Years) && is.null(YearLH))
    YearLH <- floor(max(Years))

  object <- methods::new(
    "data",
    Name = Name,
    Agency = Agency,
    Author = Author,
    Email = Email,
    Region = Region,
    Latitude = Latitude,
    Longitude = Longitude,
    Years = Years,
    YearLH  = YearLH ,
    Seasons = Seasons,
    nArea = nArea,

    LifeHistory = LifeHistory,
    Exploitation = Exploitation,
    Reference = Reference,
    Effort = Effort,

    Landings = Landings,
    Discards = Discards,

    CPUE = CPUE,
    Survey = Survey,

    LandingsAtAge = LandingsAtAge,
    DiscardsAtAge = DiscardsAtAge,

    LandingsAtSize = LandingsAtSize,
    DiscardsAtSize = DiscardsAtSize,

    Advice = Advice,

    Misc = Misc,
    Log = list()
  )

  methods::validObject(object)
  object
}

#' @rdname Data
#' @export
`Data<-` <- function(x, value) {
  .AssignSlotRecursive(x, value, 'Data')
}
