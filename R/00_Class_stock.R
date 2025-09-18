
# Stock Class ----

#' Stock Object
#' 
#' @include 00_Class_unions.R
#' @include 00_Class_child.R
#' @include 00_Class_ages.R
#' @include 00_Class_length.R
#' @include 00_Class_weight.R
#' @include 00_Class_naturalmortality.R
#' @include 00_Class_maturity.R
#' @include 00_Class_fecundity.R 
#' @include 00_Class_srr.R
#' @include 00_Class_spatial.R
#' @include 00_Class_depletion.R
#'
#'
#' @details
#' ## About the `stock` Class
#' ...
#'
#' ## Creating New Objects
#' `r Creating_New_Objects('stock')`
#'
#' ## Accessing and Assigning Slots
#' `r Accessing_Assigning_Slots('stock')`
#'
#' @slot Name Character string. Unique name for this stock
#' @slot CommonName Common name of the stock
#' @slot Species Scientific name (genus and species)
#' @slot Ages A [Ages()] object. *Required*.
#' @slot Length A [Length()] object.
#' @slot Weight A [Weight()] object.
#' @slot NaturalMortality A [NaturalMortality()] object.
#' @slot Maturity A [Maturity()] object.
#' @slot Fecundity A [Fecundity()] object. Optional.
#' @slot SRR A [SRR()] object.
#' @slot Spatial A [Spatial()] object. Optional.
#' @slot Depletion A [Depletion()] object. Optional.
#' @slot nSim The number of simulations. Numeric. Positive integer `nSim=1` will
#' produce a deterministic operating model. Can be left empty and will be populated internally.
#' @slot CurrentTime Numeric value specifying the last historical time step. Must be in `TimeSteps`.
#' @slot TimeSteps Numeric vector specifying the time steps
#'
#' @slot Misc `r Misc_param()`
#' @slot Log A list. Used internally for logging and debugging.
#'
#' @seealso `r See_Also('stock')`
#'
#' @name StockClass
#'
#' @example man-examples/Stock-class.R
#' @export
setClass('stock',
         slots=c(Name='char.null',
                 CommonName='char.null',
                 Species='char.null',
                 Ages='ages',
                 Length='length',
                 Weight='weight',
                 NaturalMortality='naturalmortality',
                 Maturity='maturity',
                 Fecundity='fecundity',
                 SRR='srr',
                 Spatial='spatial',
                 Depletion='depletion',
                 nYear='num.null',
                 pYear='num.null',
                 nSim='num.null',
                 CurrentYear='num.null',
                 TimeUnits='char.null',
                 TimeSteps='num.null',
                 TimeStepsPerYear='num.null',
                 Misc='list',
                 Log='list')
)

setMethod("initialize", "stock", function(.Object,
                                          Name=NULL,
                                          CommonName=NULL,
                                          Species=NULL,
                                          Ages=new('ages'),
                                          Length=new('length'),
                                          Weight=new('weight'),
                                          NaturalMortality=new('naturalmortality'),
                                          Maturity=new('maturity'),
                                          Fecundity=new('fecundity'),
                                          SRR=new('srr'),
                                          Spatial=new('spatial'),
                                          Depletion=new('depletion'),
                                          nYear=20,
                                          pYear=30,
                                          nSim=48,
                                          CurrentYear=as.numeric(format(Sys.Date(), '%Y')),
                                          TimeUnits='year',
                                          Misc=list()) {
  .Object@Name <- Name
  .Object@CommonName <- CommonName
  .Object@Species <- Species
  .Object@Ages <- Ages
  .Object@Length <- Length
  .Object@Weight <- Weight
  .Object@NaturalMortality <- NaturalMortality
  .Object@Maturity <- Maturity
  .Object@Fecundity <- Fecundity
  .Object@SRR <- SRR
  .Object@Spatial <- Spatial
  .Object@Depletion <- Depletion
  .Object@nYear <- nYear
  .Object@pYear <- pYear
  .Object@nSim <- nSim
  .Object@CurrentYear <- CurrentYear
  
  .Object@TimeStepsPerYear <- TSperYear(TimeUnits)
  .Object@TimeSteps <- CalcTimeSteps(nYear, pYear, CurrentYear, TimeUnits)
  .Object@TimeUnits <- TimeUnits
  .Object@Misc <- Misc
  #   .Object@Created <- Sys.time()
  .Object
})

setValidity('stock', isValidObject)


#' @describeIn StockClass Create a new `stock` class object
#' @param Name Character string. Unique name for this stock
#' @param CommonName Common name of the stock
#' @param Species Scientific name (genus and species)
#' @param Ages A [Ages()] object. *Required*.
#' @param Length A [Length()] object.
#' @param Weight A [Weight()] object.
#' @param NaturalMortality A [NaturalMortality()] object.
#' @param Maturity A [Maturity()] object.
#' @param Fecundity A [Fecundity()] object. Optional.
#' @param SRR A [SRR()] object.
#' @param Spatial A [Spatial()] object. Optional.
#' @param Depletion A [Depletion()] object. Optional.
#' @param nSim The number of simulations. Numeric. Positive integer `nSim=1` will
#' produce a deterministic operating model. Can be left empty and will be populated internally.
#' @param CurrentYear The last historical year of the operating model. Defaults
#' to the year the Operating Model object is built. Must include `CurrentYear` but can be
#' in units other than `year`. See `ValidUnits('Ages')`
#' @param TimeSteps Numeric vector of the time steps.
#'
#' @param Misc `r Misc_param()`
#' @export
Stock <- function(Name=NULL,
                  CommonName=NULL,
                  Species=NULL,
                  Ages=new('ages'),
                  Length=new('length'),
                  Weight=new('weight'),
                  NaturalMortality=new('naturalmortality'),
                  Maturity=new('maturity'),
                  Fecundity=new('fecundity'),
                  SRR=new('srr'),
                  Spatial=new('spatial'),
                  Depletion=new('depletion'),
                  Misc=list(),
                  ...) {
  
  if (methods::is(Name, 'om')) {
    if (methods::is(Name@Stock, 'list')) {
      if (methods::is(CommonName, 'numeric')) {
        if (CommonName > nStock(Name)) {
          if (nStock(Name)==1) {
            cli::cli_abort('OM has only {.val {nStock(Name)}} stock')
          } else {
            cli::cli_abort('OM has only {.val {nStock(Name)}} stocks')
          }
        } else {
          return(Name@Stock[[CommonName]])
        }
      } else {
        cli::cli_inform('`Stock` is a list. Returning stock list. \n Use `Stock(OM, x)` to access stock `x`')
        return(Name@Stock)
      }
    }
    return(Name@Stock)
  }
  
  dots <- list(...)
  nYear <- 20
  pYear <- 30
  nSim <- 48
  CurrentYear <- as.numeric(format(Sys.Date(), '%Y'))
  TimeUnits <- 'year'
  for (nm in names(dots)) 
    assign(nm, dots[[nm]])
  
  methods::new('stock',
               Name=Name,
               CommonName=CommonName,
               Species=Species,
               Ages=Ages,
               Length=Length,
               Weight=Weight,
               NaturalMortality=NaturalMortality,
               Maturity=Maturity,
               Fecundity=Fecundity,
               SRR=SRR,
               Spatial=Spatial,
               Depletion=Depletion,
               nYear=nYear,
               pYear=pYear,
               nSim=nSim,
               CurrentYear=CurrentYear,
               TimeUnits=TimeUnits,
               Misc=Misc)
}


#' @describeIn StockClass Assign an `stock` class object to a [OM()] object
#' @param x A [OM()] class object
#' @param value An `stock` class object to assign to `x`
#' @export
`Stock<-` <- function(x, value) {
  assignSlot(x, value, 'Stock')
}