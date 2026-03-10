#' Create an Operating Model
#'
#' Construct and manipulate an [om-class] object defining the specifications
#' of an Operating Model (OM)
#'
#' @param Name Name of the operating model. Character string.
#' @param Agency Optional. Name of the agency responsible for management.
#' Character string. Supports Markdown.
#' @param Author Optional. Name(s) of author(s) of the operating model.
#' Character vector.
#' @param Email Optional. Email address(es) corresponding to `Author`.
#' Character vector. Supports Markdown.
#' @param Region Optional. Name of the geographic region of the fishery.
#' Character string.
#' @param Latitude Optional. Latitude (decimal degrees) representing the
#' center of `Region`. Numeric scalar.
#' @param Longitude Optional. Longitude (decimal degrees) representing the
#' center of `Region`. Numeric scalar.
#' @param Sponsor Optional. Organization sponsoring development of the
#' operating model. Character string. Supports Markdown.
#'
#' @param nSim Number of stochastic simulations. Positive integer.
#' @param nYear Number of historical years. Numeric scalar.
#' @param pYear Number of projection years. Numeric scalar.
#' @param CurrentYear Final historical year of the operating model.
#' Integer scalar.
#' @param Seasons Number of seasons per year. Numeric scalar.
#'
#' @param Stock A [Stock()] object or list of [Stock()] objects.
#' @param Fleet A hierarchical list of [Fleet()] objects by stock and fleet. 
#' Each stock must have the same number of fleets.
#' 
#' @param Obs A hierarchical list of [Obs()] objects by stock/complex and fleet.
#' @param Imp A hierarchical list of [Imp()] objects by stock/complex and fleet.
#'
#' @param Data A [Data()] object or list of [Data()] objects associated with
#' the operating model.
#' @param DataLag Integer specifying the number of time steps that data are
#' lagged relative to management implementation.
#'
#' @param CatchFrac Optional list controlling catch fraction allocation.
#' @param Allocation Optional list controlling fleet or stock allocation.
#' @param EFactor Optional list of effort or exploitation modifiers.
#'
#' @param Complexes Optional list defining stock complexes for data aggregation
#' and management.
#' @param Herm Optional list defining hermaphroditism or movement between stocks.
#' @param SharePar Logical indicating whether key parameters are shared among
#' stocks.
#' @param Relations Optional list defining biological or ecological relationships
#' among stocks.
#'
#' @param Interval Management update interval. Numeric scalar or named numeric
#' vector.
#' @param nReps Number of stochastic replicates for management advice.
#' @param pStar Percentile applied to stochastic management advice.
#' @param maxF Maximum allowable fishing mortality.
#' @param Seed Optional random number generator seed.
#'
#' @param Control Named list of operating model control settings.
#' @param Misc List for miscellaneous objects or developer-use components.
#' @param Source Optional character string referencing data sources or
#' documentation. Supports Markdown.
#'
#' @details
#'  
#' ## About the `OM` Object
#' 
#' The `OM` object defines a complete operating model specification used in
#' Management Strategy Evaluation (MSE). See the [openMSE Technical Manual](https://docs.openmse.com/) 
#' for more details.
#' 
#' ## Accessing and Assigning Slots
#'
#' Slots in [om-class] objects can be accessed or assigned using
#' functions matching the slot names (e.g., `Agency(om)` or
#' `Agency(om) <- "DFO"`). 
#' 
#'
#' @return An [om-class] object.
#'
#' @seealso
#'
#' * [Stock()]
#' * [Fleet()]
#' * [Obs()]
#' * [Imp()]
#' * [Data()]
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
    .Object@Control <- ControlDefault
  }
  
  .Object@Misc <- Misc
  .Object@Source <- Source
  
  methods::validObject(.Object)
  .Object
}



#' Access and Modify OM Slots
#'
#' Accessor and assignment functions for slots in [om-class] objects.
#'
#' Each function retrieves or replaces the value of the corresponding slot.
#' Assignment methods validate input using internal consistency checks.
#'
#' For example:
#'
#' * `Agency(om)` returns the agency name
#' * `Agency(om) <- "DFO"` updates the agency
#'
#' @name OM-accessors
NULL

#' @rdname OM-accessors
#' @export
Agency <- function(OM) {
  AccessSlot(OM, 'Agency')
}

#' @rdname OM-accessors
#' @export
`Agency<-` <- function(x, value) {
  AssignSlot(x, value, 'Agency')
}

#' @rdname OM-accessors
#' @export
Allocation <- function(OM) {
  AccessSlot(OM, 'Allocation')
}

#' @rdname OM-accessors
#' @export
`Allocation<-` <- function(x, value) {
  AssignSlot(x, value, 'Allocation')
}

#' @rdname OM-accessors
#' @export
Author <- function(OM) {
  AccessSlot(OM, 'Author')
}

#' @rdname OM-accessors
#' @export
`Author<-` <- function(x, value) {
  AssignSlot(x, value, 'Author')
}

#' @rdname OM-accessors
#' @export
CatchFrac <- function(OM) {
  AccessSlot(OM, 'CatchFrac')
}

#' @rdname OM-accessors
#' @export
`CatchFrac<-` <- function(x, value) {
  AssignSlot(x, value, 'CatchFrac')
}

#' @rdname OM-accessors
#' @export
Complexes <- function(OM) {
  AccessSlot(OM, 'Complexes')
}

#' @rdname OM-accessors
#' @export
`Complexes<-` <- function(x, value) {
  AssignSlot(x, value, 'Complexes')
}

#' @rdname OM-accessors
#' @export
Control <- function(OM) {
  AccessSlot(OM, 'Control')
}

#' @rdname OM-accessors
#' @export
`Control<-` <- function(x, value) {
  AssignSlot(x, value, 'Control')
}

#' @rdname OM-accessors
#' @export
CurrentYear <- function(OM) {
  AccessSlot(OM, 'CurrentYear')
}

#' @rdname OM-accessors
#' @export
`CurrentYear<-` <- function(x, value) {
  AssignSlot(x, value, 'CurrentYear')
}


#' @rdname OM-accessors
#' @export
DataLag <- function(OM) {
  AccessSlot(OM, 'DataLag')
}

#' @rdname OM-accessors
#' @export
`DataLag<-` <- function(x, value) {
  AssignSlot(x, value, 'DataLag')
}

#' @export
Dynamics <- function(OM) {
  AccessSlot(OM, 'Dynamics')
}

#' @rdname OM-accessors
#' @export
`Dynamics<-` <- function(x, value) {
  AssignSlot(x, value, 'Dynamics')
}

#' @rdname OM-accessors
#' @export
EFactor <- function(OM) {
  AccessSlot(OM, 'EFactor')
}

#' @rdname OM-accessors
`EFactor<-` <- function(x, value) {
  AssignSlot(x, value, 'EFactor')
}

#' @rdname OM-accessors
#' @export
Email <- function(OM) {
  AccessSlot(OM, 'Email')
}

#' @rdname OM-accessors
#' @export
`Email<-` <- function(x, value) {
  AssignSlot(x, value, 'Email')
}


#' @rdname OM-accessors
#' @export
Herm <- function(OM) {
  AccessSlot(OM, 'Herm')
}

#' @rdname OM-accessors
#' @export
`Herm<-` <- function(x, value) {
  AssignSlot(x, value, 'Herm')
}

#' @rdname OM-accessors
#' @export
Interval <- function(OM) {
  AccessSlot(OM, 'Interval')
}

#' @rdname OM-accessors
#' @export
`Interval<-` <- function(x, value) {
  AssignSlot(x, value, 'Interval')
}


#' @rdname OM-accessors
#' @export
Latitude <- function(OM) {
  AccessSlot(OM, 'Latitude')
}

#' @rdname OM-accessors
#' @export
`Latitude<-` <- function(x, value) {
  AssignSlot(x, value, 'Latitude')
}

#' @rdname OM-accessors
#' @export
Longitude <- function(OM) {
  AccessSlot(OM, 'Longitude')
}

#' @rdname OM-accessors
#' @export
`Longitude<-` <- function(x, value) {
  AssignSlot(x, value, 'Longitude')
}

#' @rdname OM-accessors
#' @export
maxF <- function(OM) {
  AccessSlot(OM, 'maxF')
}

#' @rdname OM-accessors
#' @export
`maxF<-` <- function(x, value) {
  AssignSlot(x, value, 'maxF')
}

#' @rdname OM-accessors
#' @export
nReps <- function(OM) {
  AccessSlot(OM, 'nReps')
}

#' @rdname OM-accessors
#' @export
`nReps<-` <- function(x, value) {
  AssignSlot(x, value, 'nReps')
}

#' @rdname OM-accessors
#' @export
nYear <- function(OM) {
  AccessSlot(OM, 'nYear')
}

#' @rdname OM-accessors
#' @export
`nYear<-` <- function(x, value) {
  AssignSlot(x, value, 'nYear')
}

#' @rdname OM-accessors
#' @export
pStar <- function(OM) {
  AccessSlot(OM, 'pStar')
}

#' @rdname OM-accessors
#' @export
`pStar<-` <- function(x, value) {
  AssignSlot(x, value, 'pStar')
}


#' @rdname OM-accessors
#' @export
pYear <- function(OM) {
  AccessSlot(OM, 'pYear')
}

#' @rdname OM-accessors
#' @export
`pYear<-` <- function(x, value) {
  AssignSlot(x, value, 'pYear')
}

#' @rdname OM-accessors
#' @export
Region <- function(OM) {
  AccessSlot(OM, 'Region')
}

#' @rdname OM-accessors
#' @export
`Region<-` <- function(x, value) {
  AssignSlot(x, value, 'Region')
}


#' @rdname OM-accessors
#' @export
Relations <- function(OM) {
  AccessSlot(OM, 'Relations')
}

#' @rdname OM-accessors
#' @export
`Relations<-` <- function(x, value) {
  AssignSlot(x, value, 'Relations')
}

#' @rdname OM-accessors
#' @export
Seasons <- function(OM) {
  if (inherits(OM,'hist'))
      return(OM@OM@Seasons)
  OM@Seasons
}

#' @rdname OM-accessors
#' @export
`Seasons<-` <- function(x, value) {
  AssignSlot(x, value, 'Seasons')
}


#' @rdname OM-accessors
#' @export
SharePar <- function(OM) {
  AccessSlot(OM, 'SharePar')
}

#' @rdname OM-accessors
#' @export
`SharePar<-` <- function(x, value) {
  AssignSlot(x, value, 'SharePar')
}



#' @rdname OM-accessors
#' @export
Source <- function(OM) {
  AccessSlot(OM, 'Source')
}

#' @rdname OM-accessors
#' @export
`Source<-` <- function(x, value) {
  AssignSlot(x, value, 'Source')
}

#' @rdname OM-accessors
#' @export
Sponsor <- function(OM) {
  AccessSlot(OM, 'Sponsor')
}

#' @rdname OM-accessors
#' @export
`Sponsor<-` <- function(x, value) {
  AssignSlot(x, value, 'Sponsor')
}

