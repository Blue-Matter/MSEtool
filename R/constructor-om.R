#' Create an Operating Model
#'
#' Construct and manipulate an [om-class] object defining the complete
#' specification of an Operating Model (OM) for use in Management Strategy
#' Evaluation (MSE).
#'
#' @param Name Character. Name of the operating model. If an S4 object with
#'   an `OM` slot is passed, that slot is returned instead. Default
#'   `"A new OM object"`.
#' @param Agency Character. Name of the agency responsible for management.
#'   Supports Markdown. Default `""`.
#' @param Author Character vector. Name(s) of the author(s) of the operating
#'   model. Default `""`.
#' @param Email Character vector. Email address(es) corresponding to `Author`.
#'   Supports Markdown. Default `""`.
#' @param Region Character. Name of the geographic region of the fishery.
#'   Default `""`.
#' @param Latitude Numeric. Latitude (decimal degrees) of the center of
#'   `Region`. Default `NULL`.
#' @param Longitude Numeric. Longitude (decimal degrees) of the center of
#'   `Region`. Default `NULL`.
#' @param Sponsor Character. Organization sponsoring development of the
#'   operating model. Supports Markdown. Default `""`.
#' @param nSim Positive integer. Number of stochastic simulations. Default
#'   `48`.
#' @param nYear Numeric. Number of historical years. Default `20`.
#' @param pYear Numeric. Number of projection years. Default `30`.
#' @param CurrentYear Integer. Final historical calendar year of the operating
#'   model. Default is the current system year.
#' @param Seasons Integer. Number of seasons per year. Default `1`.
#' @param Stock A [stock-class] object or named list of [stock-class] objects.
#'   Default `NULL`.
#' @param Fleet A hierarchical named list of [fleet-class] objects indexed by
#'   stock then fleet. Each stock must have the same number of fleets. Default
#'   `NULL`.
#' @param Obs A hierarchical named list of [obs-class] objects indexed by
#'   stock and fleet. Default `NULL`.
#' @param Imp A hierarchical named list of [imp-class] objects indexed by
#'   stock and fleet. Default `NULL`.
#' @param Data A [data-class] object or list of [data-class] objects
#'   associated with the operating model. Default `NULL`.
#' @param DataLag Integer. Number of time steps that data are lagged relative
#'   to management implementation. Default `0`.
#' @param CatchFrac List. Controls catch fraction allocation among fleets or
#'   stocks. Default `NULL`.
#' @param Allocation List. Controls effort or catch allocation among fleets or
#'   stocks. Default `NULL`.
#' @param EFactor List. Effort or exploitation modifiers applied during
#'   projection. Default `NULL`.
#' @param Complexes List. Defines stock complexes for data aggregation and
#'   management. Default `NULL`.
#' @param Herm List. Defines hermaphroditism or movement between stocks.
#'   Default `NULL`.
#' @param SharePar Logical. Whether key parameters are shared among stocks.
#'   Default `NULL`.
#' @param Relations List. Biological or ecological relationships among stocks
#'   (e.g., predator-prey). Default `NULL`.
#' @param Interval Numeric scalar or named numeric vector. Management update
#'   interval in years. Default `1`.
#' @param nReps Positive integer. Number of stochastic replicates used for
#'   generating management advice. Default `1`.
#' @param pStar Numeric. Percentile (0–1) applied to stochastic management
#'   advice. Default `0.5`.
#' @param maxF Numeric. Maximum allowable instantaneous fishing mortality.
#'   Default `3`.
#' @param Seed Integer. Random number generator seed for reproducibility.
#'   Default `101`.
#' @param Control Named list of operating model control settings. If `NULL`
#'   (default), `ControlDefault` is used.
#' @param Misc List. Miscellaneous objects or developer-use components.
#'   Default `list()`.
#' @param Source Character. References to data sources or documentation.
#'   Supports Markdown. Default `NULL`.
#'
#' @details
#' The [om-class] object defines a complete operating model specification for
#' use in Management Strategy Evaluation (MSE). See the
#' [openMSE Technical Manual](https://docs.openmse.com/) for full details on
#' model structure and parameterisation.
#'
#' ## Pass-Through Access
#'
#' If `Name` is an S4 object with an `OM` slot (e.g., a `hist-class` or
#' `mse-class` object), the `OM` slot of that object is returned directly
#' rather than constructing a new OM.
#'
#' ## Accessing and Assigning Slots
#'
#' All slots in [om-class] objects can be accessed or assigned using
#' functions matching the slot names. See [OM-accessors] for the full list.
#' For example:
#' ```r
#' Agency(om)
#' Agency(om) <- "DFO"
#' ```
#'
#' @return An [om-class] object, or the `OM` slot of `Name` if `Name` is an
#'   S4 object with an `OM` slot.
#'
#' @seealso [om-class], [Stock()], [Fleet()], [Obs()], [Imp()], [Data()],
#'   [OM-accessors], [PopulateOM()], [RunMSE()]
#'
#' @examples
#' om <- OM()
#' Agency(om)
#' Agency(om) <- "DFO"
#' Agency(om)
#'
#' @export
OM <- function(Name='A new `OM` object',
               Agency='',
               Author='',
               Email='',
               Region='',
               Latitude=NULL,
               Longitude=NULL,
               Sponsor='',
               
               nSim=48,
               nYear=20,
               pYear=30,
               
               CurrentYear=as.numeric(format(Sys.Date(), '%Y')),
               Seasons=1,
               
               Stock=NULL,
               Fleet=NULL,
               Obs=NULL,
               Imp=NULL,
               
               Data=NULL,
               DataLag=0,
               
               CatchFrac=NULL,
               Allocation=NULL,
               EFactor=NULL,
               
               Complexes=NULL,
               Herm=NULL,
               SharePar=NULL,
               Relations=NULL,
               
               Interval=1,
               nReps=1,
               pStar=0.5,
               maxF=3,
               Seed=101,
               
               Control=NULL,
               
               Misc=list(),
               Source=NULL) {
  
  if (!inherits(Name, 'character')) {
    if (!'OM' %in% slotNames(Name))
      cli::cli_abort(c('x'='No slot {.val OM} found in object class {.val {class(Name)}}'))
    return(Name@OM)
  }
  
  .Object <- new('om')
  .Object@Name <- Name
  .Object@Agency <- Agency
  .Object@Author <- Author
  .Object@Email <- Email
  .Object@Region <- Region
  .Object@Latitude <- Latitude
  .Object@Longitude <- Longitude
  .Object@Sponsor <- Sponsor
  
  .Object@nSim <- nSim
  .Object@nYear <- nYear
  .Object@pYear <- pYear
  
  .Object@CurrentYear <- CurrentYear
  .Object@Seasons <- Seasons
  .Object@Years <- CalcYears(nYear, pYear, CurrentYear, Seasons)
  
  .Object@Stock <- Stock
  .Object@Fleet <- Fleet
  .Object@Obs <- Obs
  .Object@Imp <- Imp
  
  .Object@Data <- Data
  .Object@DataLag <- DataLag
  
  .Object@CatchFrac <- CatchFrac
  .Object@Allocation <- Allocation
  .Object@EFactor <- EFactor
  
  .Object@Complexes <- Complexes
  .Object@Herm <- Herm
  .Object@SharePar <- SharePar
  .Object@Relations <- Relations
  
  .Object@Interval <- Interval
  .Object@nReps <- nReps
  .Object@pStar <- pStar
  .Object@maxF <- maxF
  .Object@Seed <- Seed 
  if (!is.null(Control)) {
    .Object@Control <- Control
  } else {
    .Object@Control <- MSEtool::ControlDefault
  }
  
  .Object@Misc <- Misc
  .Object@Source <- Source
  
  methods::validObject(.Object)
  .Object
}



#' Access and Modify OM Slots
#'
#' Accessor and assignment functions for slots in [om-class] objects.
#' Each function retrieves or replaces the value of the corresponding slot.
#'
#' @param x An [om-class] object or a [hist-class] object
#' @param value The value to assign to the corresponding slot.
#'
#' @return
#' - Accessor functions return the value of the named slot.
#' - Replacement functions return `x` with the named slot updated.
#'
#' @examples
#' om <- OM()
#' Agency(om)
#' Agency(om) <- "DFO"
#'
#' nSim(om)
#' nSim(om) <- 100
#'
#' @seealso [OM()], [om-class]
#' @name OM-accessors
NULL

ishist <- function(x, slot_name) {
  if (inherits(x,'hist'))
    x <- x@OM 
  AccessSlot(x, slot_name)
}

#' @rdname OM-accessors
#' @export
Agency <- function(x) {
  ishist(x, 'Agency')
}

#' @rdname OM-accessors
#' @export
`Agency<-` <- function(x, value) {
  AssignSlot(x, value, 'Agency')
}

#' @rdname OM-accessors
#' @export
Allocation <- function(x) {
  ishist(x, 'Allocation')
}

#' @rdname OM-accessors
#' @export
`Allocation<-` <- function(x, value) {
  AssignSlot(x, value, 'Allocation')
}

#' @rdname OM-accessors
#' @export
Author <- function(x) {
  ishist(x, 'Author')
}

#' @rdname OM-accessors
#' @export
`Author<-` <- function(x, value) {
  AssignSlot(x, value, 'Author')
}

#' @rdname OM-accessors
#' @export
CatchFrac <- function(x) {
  ishist(x, 'CatchFrac')
}

#' @rdname OM-accessors
#' @export
`CatchFrac<-` <- function(x, value) {
  AssignSlot(x, value, 'CatchFrac')
}

#' @rdname OM-accessors
#' @export
Complexes <- function(x) {
  ishist(x, 'Complexes')
}

#' @rdname OM-accessors
#' @export
`Complexes<-` <- function(x, value) {
  AssignSlot(x, value, 'Complexes')
}

#' @rdname OM-accessors
#' @export
Control <- function(x) {
  ishist(x, 'Control')
}

#' @rdname OM-accessors
#' @export
`Control<-` <- function(x, value) {
  AssignSlot(x, value, 'Control')
}

#' @rdname OM-accessors
#' @export
CurrentYear <- function(x) {
  ishist(x, 'CurrentYear')
}

#' @rdname OM-accessors
#' @export
`CurrentYear<-` <- function(x, value) {
  AssignSlot(x, value, 'CurrentYear')
}

#' @rdname OM-accessors
#' @export
DataLag <- function(x) {
  ishist(x, 'DataLag')
}

#' @rdname OM-accessors
#' @export
`DataLag<-` <- function(x, value) {
  AssignSlot(x, value, 'DataLag')
}

#' @rdname OM-accessors
#' @export
EFactor <- function(x) {
  ishist(x, 'EFactor')
}

#' @rdname OM-accessors
#' @export
`EFactor<-` <- function(x, value) {
  AssignSlot(x, value, 'EFactor')
}

#' @rdname OM-accessors
#' @export
Email <- function(x) {
  ishist(x, 'Email')
}

#' @rdname OM-accessors
#' @export
`Email<-` <- function(x, value) {
  AssignSlot(x, value, 'Email')
}

#' @rdname OM-accessors
#' @export
Herm <- function(x) {
  ishist(x, 'Herm')
}

#' @rdname OM-accessors
#' @export
`Herm<-` <- function(x, value) {
  AssignSlot(x, value, 'Herm')
}

#' @rdname OM-accessors
#' @export
Interval <- function(x) {
  ishist(x, 'Interval')
}

#' @rdname OM-accessors
#' @export
`Interval<-` <- function(x, value) {
  AssignSlot(x, value, 'Interval')
}

#' @rdname OM-accessors
#' @export
Latitude <- function(x) {
  ishist(x, 'Latitude')
}

#' @rdname OM-accessors
#' @export
`Latitude<-` <- function(x, value) {
  AssignSlot(x, value, 'Latitude')
}

#' @rdname OM-accessors
#' @export
Longitude <- function(x) {
  ishist(x, 'Longitude')
}

#' @rdname OM-accessors
#' @export
`Longitude<-` <- function(x, value) {
  AssignSlot(x, value, 'Longitude')
}

#' @rdname OM-accessors
#' @export
maxF <- function(x) {
  ishist(x, 'maxF')
}

#' @rdname OM-accessors
#' @export
`maxF<-` <- function(x, value) {
  AssignSlot(x, value, 'maxF')
}

#' @rdname OM-accessors
#' @export
nReps <- function(x) {
  ishist(x, 'nReps')
}

#' @rdname OM-accessors
#' @export
`nReps<-` <- function(x, value) {
  AssignSlot(x, value, 'nReps')
}

#' @rdname OM-accessors
#' @export
nYear <- function(x) {
  ishist(x, 'nYear')
}

#' @rdname OM-accessors
#' @export
`nYear<-` <- function(x, value) {
  AssignSlot(x, value, 'nYear')
}

#' @rdname OM-accessors
#' @export
pStar <- function(x) {
  ishist(x, 'pStar')
}

#' @rdname OM-accessors
#' @export
`pStar<-` <- function(x, value) {
  AssignSlot(x, value, 'pStar')
}

#' @rdname OM-accessors
#' @export
pYear <- function(x) {
  ishist(x, 'pYear')
}

#' @rdname OM-accessors
#' @export
`pYear<-` <- function(x, value) {
  AssignSlot(x, value, 'pYear')
}

#' @rdname OM-accessors
#' @export
Region <- function(x) {
  ishist(x, 'Region')
}

#' @rdname OM-accessors
#' @export
`Region<-` <- function(x, value) {
  AssignSlot(x, value, 'Region')
}

#' @rdname OM-accessors
#' @export
Relations <- function(x) {
  ishist(x, 'Relations')
}

#' @rdname OM-accessors
#' @export
`Relations<-` <- function(x, value) {
  AssignSlot(x, value, 'Relations')
}

#' @rdname OM-accessors
#' @export
Seasons <- function(x) {
  ishist(x, 'Seasons')
}

#' @rdname OM-accessors
#' @export
`Seasons<-` <- function(x, value) {
  AssignSlot(x, value, 'Seasons')
}

#' @rdname OM-accessors
#' @export
SharePar <- function(x) {
  ishist(x, 'SharePar')
}

#' @rdname OM-accessors
#' @export
`SharePar<-` <- function(x, value) {
  AssignSlot(x, value, 'SharePar')
}

#' @rdname OM-accessors
#' @export
Source <- function(x) {
  ishist(x, 'Source')
}

#' @rdname OM-accessors
#' @export
`Source<-` <- function(x, value) {
  AssignSlot(x, value, 'Source')
}

#' @rdname OM-accessors
#' @export
Sponsor <- function(x) {
  ishist(x, 'Sponsor')
}

#' @rdname OM-accessors
#' @export
`Sponsor<-` <- function(x, value) {
  AssignSlot(x, value, 'Sponsor')
}