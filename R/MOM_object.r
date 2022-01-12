# MOM object

# ---- MOM Class ----
#' Class \code{'MOM'}
#'
#' An object containing all the parameters needed to control a multi-stock, multi-fleet MSE which can
#' be build from component Stock, Fleet, Obs, and Imp objects.
#'
#' Almost all of these inputs are a vector of length 2 which describes the upper and lower
#' bounds of a uniform distribution from which to sample the parameter.
#'
#'
#' @name MOM-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('MOM', Stock_list, Fleet_list, Obs_list, Imp_list)}.
#' @slot Name Name of the operating model
#' @slot Agency Name of the agency responsible for the management of the fishery. Character string
#' @slot Region Name of the general geographic region of the fishery. Character string
#' @slot Sponsor Name of the organization who sponsored the OM. Character string
#' @slot Latitude Latitude (decimal degrees). Negative values represent the South of the Equator. Numeric. Single value
#' @slot Longitude Longitude (decimal degrees). Negative values represent the West of the Prime Meridian. Numeric. Single value
#' @slot nsim The number of simulations
#' @slot proyears The number of projected years
#' @slot interval The assessment interval - how often would you like to update the management system?
#' @slot pstar The percentile of the sample of the management recommendation for each method
#' @slot maxF Maximum instantaneous fishing mortality rate that may be simulated for any given age class
#' @slot reps Number of samples of the management recommendation for each method. Note that when this is set to 1, the mean value of
#' the data inputs is used.
#' @slot cpars A hierarchical list nstock then nfleet long of custom parameters. Time series are a matrix nsim rows by nyears columns. Single parameters are a vector nsim long
#' @slot seed A random seed to ensure users can reproduce results exactly
#' @slot Source A reference to a website or article from which parameters were taken to define the operating model
#' @slot Stocks List of stock objects
#' @slot Fleets List of Fleet objects
#' @slot Obs    Hierarchical List of Observation model objects Level 1 is stock, level 2 is fleet
#' @slot Imps   Hierarchical List of Implementation model objects Level 1 is stock, level 2 is fleet
#' @slot CatchFrac A list nstock long, of matrices nsim x nfleet representing the fraction of current catches of the various fleets to each stock (each matrix is nsim by nfleet long and rows sum to 1 for each stock)
#' @slot Allocation  A list nstock long, of matrices nsim x nfleet representing the fraction of future TACs of the various fleets to each stock (each matrix is nsim by nfleet long and rows sum to 1 for each stock).
#' @slot Efactor A list nstock long, of current effort factors by fleet (default is 1 - same as current effort)
#' @slot Complexes A list of stock complexes. Each position is a vector of stock numbers (as they appear in StockPars) for which data should be aggregated and TAC recommendations split among stocks according to vulnerable biomass
#' @slot SexPars A list of slots that control sex-specific dynamics
#' @slot Rel A list of biological / ecological relationships among stocks
#' over-ridden if an MP of class 'MP_F" is supplied that is a multi-fleet MP.
#' @author T. Carruthers and A. Hordyk
#' @export
#' @keywords classes
setClass("MOM", representation(Name = "character", Agency="character",
                               Region="character", Sponsor="character",
                               Latitude="numeric", Longitude="numeric",
                               nsim="numeric", proyears="numeric",

                               interval='numeric', pstar='numeric',
                               maxF='numeric', reps='numeric',
                               cpars="list", seed="numeric",
                               Source="character",Stocks='list',
                               Fleets='list',Obs='list',Imps='list',
                               CatchFrac='list',Allocation='list',
                               Efactor='list',Complexes='list',
                               SexPars='list',Rel='list'))


# initialize MOM
setMethod("initialize", "MOM", function(.Object, Stocks=NULL, Fleets=NULL,
                                        Obs=NULL, Imps=NULL, CatchFrac=NULL,
                                        cpars=NULL, interval=4, pstar=0.5,
                                        maxF=0.8, reps=1, nsim=48, proyears=50,
                                        Source=NULL,
                                        Allocation=NULL,Efactor=NULL,
                                        Complexes=NULL, SexPars=NULL, Rel=NULL) {
  # .Object}) ; .Object<-new('MOM')

  if (is.null(Stocks)|is.null(Fleets)|is.null(Obs)|is.null(Imps)) {
    message("A specified list of objects is required for each of the following arguments: Stocks, Fleets, Obs, Imps, CatchFrac. Returning a blank MOM object")
    .Object@seed <- 1
    return(.Object)
  }


  # Needed:
  # helper function to expand Fleets by stock
  # helper function to expand CatchFrac by simulation given a vector

  # Known issues:
  # Check for same nareas in cpars$mov

  for(i in 1:length(Stocks))if (class(Stocks[[i]]) != "Stock")  stop(paste("Could not build operating model:", deparse(substitute(Stocks[[i]])), "not of class Stock"))
  for(i in 1:length(Fleets))for(j in 1:length(Fleets[[i]]))if (class(Fleets[[i]][[j]]) != "Fleet")  stop(paste("Could not build operating model:", deparse(substitute(Fleets[[i]][[j]])), "not of class Fleet"))
  for(i in 1:length(Obs))for(j in 1:length(Obs[[i]]))  if (class(Obs[[i]][[j]]) != "Obs") stop(paste("Could not build operating model:", deparse(substitute(Obs[[i]][[j]])), "not of class Obs"))
  for(i in 1:length(Imps))for(j in 1:length(Imps[[i]])) if (class(Imps[[i]][[j]]) != "Imp") stop(paste("Could not build operating model:", deparse(substitute(Imp)), "not of class Imp"))
  .Object@Name <- paste(c("MultiOM:",SIL(Stocks,"Name"),SIL(Fleets,"Name")),collapse=" - ")

  if( length(unique(SIL(Fleets,"nyears")))>1)stop("Fleet objects must have the same historical duration (nyears)")


  # Now copy the values for stock, fleet and observation slots to same
  .Object@Stocks<-Stocks
  .Object@Fleets<-Fleets

  np<-length(Stocks)
  nf<-length(Fleets[[1]])

  if(is.null(cpars)){
    .Object@cpars<-list()
    for(p in 1:np){
      .Object@cpars[[p]]<-list()
      for(f in 1:nf){
        .Object@cpars[[p]][[f]]<-list()
      }
    }
  } else {
    .Object@cpars<-cpars
  }

  .Object@Obs<-Obs
  .Object@Imps<-Imps

  if(is.null(Source)){
    .Object@Source <- Stocks[[1]]@Source
  }else{
    .Object@Source <- Source
  }

  .Object@nsim <- nsim
  .Object@proyears <- proyears
  .Object@interval <- interval
  .Object@pstar <- pstar
  .Object@maxF <- maxF
  .Object@reps <- reps
  if(is.null(Allocation))Allocation<-new('list')
  if(is.null(Efactor))Efactor<-new('list')
  if(is.null(SexPars))SexPars<-new('list')
  if(is.null(Complexes))Complexes<-new('list')
  .Object@Allocation <- Allocation
  .Object@Efactor<-Efactor
  .Object@SexPars<-SexPars
  .Object@Complexes

  if(is.null(CatchFrac)){
    CatchFrac<-list()
    for(i in 1:length(Stocks)) CatchFrac[[i]]<-matrix(1,nrow=nsim,ncol=1)
  }

  .Object@CatchFrac<-CatchFrac

  if(is.null(Rel))  Rel<-list()
  .Object@seed=1
  .Object@Rel=Rel
  .Object

})


# setGeneric("tinyErr", function(x, obs = TRUE, imp = TRUE, proc = TRUE, grad = TRUE, silent = FALSE)
#   standardGeneric("tinyErr") )


# #' @name tinyErr
# #' @aliases tinyErr,MOM-method
# #' @title Remove observation error, process error, implementation error or future gradients in (time varying) parameters
# #'
# #' @description This function allows like-by-like comparison of results
# #' among DLMtool `runMSE()` and MSEtool `multiMSE()`.
# #'
# #' @param x Object of class MOM. A Multi-OM object
# #' @param obs Logical. Whether observation error should be removed
# #' @param imp Logical. Whether implementation error should be removed
# #' @param proc Logical. Whether process error should be removed (e.g. interannual variability in natural mortality rate)
# #' @param grad Logical. Whether gradients (consistent temporal changes in parameters) should be removed
# #' @param silent Logical. Should changes be printed to the console?
# #' @author T.Carruthers and A. Hordyk
# #' @keywords internal
# #' @exportMethod tinyErr
# setMethod("tinyErr", signature(x = "MOM"),
#           function(x, obs = TRUE, imp = TRUE, proc = TRUE, grad = TRUE, silent = FALSE) {
#             MOM <- x
#
#             MOMout <- MOM
#
#             np<-length(MOM@Stocks)
#             nf<-length(MOM@Fleets[[1]])
#
#             if (obs) {
#               if (!silent)
#                 message("Removing all Observation Error")
#               for(p in 1:np){for(f in 1:nf){
#                 MOMout@Obs[[p]][[f]]<-DLMtool::Perfect_Info
#               }}
#
#             }
#             if (imp) {
#               if (!silent)
#                 message("Removing all Implementation Error")
#               for(p in 1:np){for(f in 1:nf){
#                 MOMout@Imps[[p]][[f]]<-DLMtool::Perfect_Imp
#               }}
#             }
#             if (proc) {
#               if (!silent)
#                 message("Removing all Process Error")
#
#               vars <- c("cv", "sd", "Perr")
#
#               # Stock P error
#               nms <- c(slotNames("Stock"))
#               ind <- unique(grep(paste(vars, collapse = "|"), nms,
#                                  value = FALSE))
#               for(p in 1:np){
#
#                 for (X in seq_along(ind)) {
#                   n <- length(slot(MOMout@Stocks[[p]], nms[ind[X]]))
#                   if (n == 0)
#                     n <- 2
#                   slot(MOMout@Stocks[[p]], nms[ind[X]]) <- rep(0, n)
#                 }
#
#               }
#
#               # Fleet P error
#
#               nms <- c(slotNames("Fleet"))
#               ind <- unique(grep(paste(vars, collapse = "|"), nms,
#                                  value = FALSE))
#               for(p in 1:np){
#                 for(f in 1:nf){
#                   for (X in seq_along(ind)) {
#                     n <- length(slot(MOMout@Fleets[[p]][[f]], nms[ind[X]]))
#                     if (n == 0)
#                       n <- 2
#                     slot(MOMout@Fleets[[p]][[f]], nms[ind[X]]) <- rep(0, n)
#                   }
#
#                 }
#               }
#
#             }
#
#             if (grad) {
#               if (!silent)
#                 message("Removing all Gradients")
#               vars <- c("grad", "inc")
#
#               # Stock grad
#               nms <- c(slotNames("Stock"))
#               ind <- unique(grep(paste(vars, collapse = "|"), nms,
#                                  value = FALSE))
#               for(p in 1:np){
#
#                 for (X in seq_along(ind)) {
#                   n <- length(slot(MOMout@Stocks[[p]], nms[ind[X]]))
#                   if (n == 0)
#                     n <- 2
#                   slot(MOMout@Stocks[[p]], nms[ind[X]]) <- rep(0, n)
#                 }
#
#               }
#
#               nms <- c(slotNames("Fleet"))
#               ind <- unique(grep(paste(vars, collapse = "|"), nms,
#                                  value = FALSE))
#               for(p in 1:np){
#                 for(f in 1:nf){
#                   for (X in seq_along(ind)) {
#                     n <- length(slot(MOMout@Fleets[[p]][[f]], nms[ind[X]]))
#                     if (n == 0)
#                       n <- 2
#                     slot(MOMout@Fleets[[p]][[f]], nms[ind[X]]) <- rep(0, n)
#                   }
#
#                 }
#               }
#
#
#             }
#             MOMout
#             })
#
