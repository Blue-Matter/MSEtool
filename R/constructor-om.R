#' Create an Operating Model
#'
#' Constructs a new [OM()] object describing a fisheries operating model.
#'
#' @param Name Name of the operating model.
#' @param Agency Management agency.
#' @param Author Author(s) of the model.
#' @param Email Email address(es) of the author(s).
#' @param Region Geographic region.
#' @param Latitude Latitude of the region center.
#' @param Longitude Longitude of the region center.
#' @param Sponsor Sponsoring organization.
#'
#' @param nSim Number of stochastic simulations.
#' @param nYear Number of historical years.
#' @param pYear Number of projection years.
#' @param CurrentYear Final historical year.
#' @param Seasons Number of seasons per year.
#'
#' @param Stock [Stock()] object(s).
#' @param Fleet [Fleet()] object(s).
#' @param Obs [Obs()] object(s).
#' @param Imp [Imp()] object(s).
#'
#' @param Data Observed fishery data.
#' @param DataLag Data lag for management.
#'
#' @param Complexes Stock complexes.
#' @param Herm Hermaphroditism structure.
#' @param SharePar Parameter sharing flag.
#' @param Relations Ecological relationships.
#'
#' @param Interval Management interval.
#' @param nReps Number of management replicates.
#' @param pStar Management percentile.
#' @param Seed Random seed.
#'
#' @param Control Control settings.
#' @param Misc Miscellaneous objects.
#' @param Source Reference information.
#'
#' @rdname OM
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


#' @rdname OM
#' @export
Complexes <- function(OM) {
  CheckClass(OM)
  OM@Complexes
}



