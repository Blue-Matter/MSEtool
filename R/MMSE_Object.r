
# ---- MMSE Class ----
#' Class \code{'MMSE'}
#'
#' A Multi Management Strategy Evaluation object that contains information about
#' simulation conditions and performance of MPs for a multi-stock, multi-fleet operating model.
#'
#'
#' @name MMSE-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new('MMSE', Name, nyears, proyears, nMPs, MPs, nsim, OMtable, Obs,
#' B_BMSYa, F_FMSYa, Ba, FMa, Ca, OFLa, Effort, PAA, CAA, CAL, CALbins)}
#'
#' @slot Name Name of the MMSE object. Single value. Character string
#' @slot nyears The number of years for the historical simulation. Single value. Positive integer
#' @slot proyears The number of years for the projections - closed loop simulations. Single value. Positive integer
#' @slot nMPs Number of management procedures simulation tested. Single value. Positive integer.
#' @slot MPs The names of the MPs that were tested. Vector of length nMPs. Character strings.
#' @slot MPcond The MP condition. Character ('bystock': an MP per stock, 'byfleet' and MP per stock and fleet, 'MMP' an MP for all stocks and fleets)
#' @slot MPrefs The names of the MPs applied for each stock (row) and fleet (column). An array.
#' @slot nsim Number of simulations. Single value. Positive integer
#' @slot nstocks Number of stocks. Single value. Positive integer
#' @slot nfleets Number of fleets. Single value. Positive integer
#' @slot Snames Names of the stocks
#' @slot Fnames Names of the fleets (matrix nstocks x nfleets)
#' @slot Stocks The stock operating model objects. List of Stocks
#' @slot Fleets The fleet operating model objects. Hierarchical list, fleets nested in stocks.
#' @slot Obss The fleet specific observation error operating model objects. Hierarchical list, fleets nested in stocks.
#' @slot Imps The fleet soecific implementation error operating model objects. Hierarchical list, fleets nested in stocks.
#'
#' @slot OM A table of sampled parameters of the operating model. Data frame of nsim rows.
#' @slot Obs A table of sampled parameters of the observation model. Data frame of nsim rows.
#'
#' @slot B_BMSY Simulated biomass relative to BMSY over the projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot F_FMSY Simulated fishing mortality rate relative to FMSY over the projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot B Simulated stock biomass over the projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot SSB Simulated spawning stock biomass over the projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot VB Simulated vulnerable biomass over the projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot FM Simulated fishing mortality rate over the projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot C Simulated catches (taken) over the projection. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot TAC Simulated Total Allowable Catch (prescribed) over the projection (this is NA for input controls). An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot SSB_hist Simulated historical spawning stock biomass. An array with dimensions: nsim, nages, nyears, nareas. Non-negative real numbers
#' @slot CB_hist Simulated historical catches in weight. An array with dimensions: nsim, nages, nyears, nareas. Non-negative real numbers
#' @slot FM_hist Simulated historical fishing mortality rate. An array with dimensions: nsim, nages, nyears, nareas. Non-negative real numbers
#' @slot Effort Simulated relative fishing effort in the projection years. An array with dimensions: nsim, nMPs, proyears. Non-negative real numbers
#' @slot PAA Population at age in last projection year. An array with dimensions: nsim, nMPs, nages. Non-negative real numbers
#' @slot CAA Catch at age in last projection year. An array with dimensions: nsim, nMPs, nages. Non-negative real numbers
#' @slot CAL Catch at length in last projection year. An array with dimensions: nsim, nMPs, nCALbins. Non-negative real numbers
#' @slot CALbins Mid-points of the catch-at-length bins. Vector of length nCALbins. Positive real numbers.
#' @slot MSY_P Array of projected MSY by year with dimensions: nsim, nstock, nMP, proyears.
#' @slot FMSY_P Array of projected FMSY by year with dimensions: nsim, nstock, nMP, proyears.
#' @slot SSBMSY_P Array of projected Spawning Stock Biomass at MSY by year with dimensions: nsim, nstock, nMP, proyears.
#' @slot Misc Miscellanenous output such as posterior predictive data
#' @author T. Carruthers
#' @keywords classes
#' @export
setClass("MMSE", representation(Name = "character", nyears = "numeric",
                                proyears = "numeric", nMPs = "numeric", MPs = "list", MPcond="character",MPrefs="array",
                                nsim = "numeric",nstocks="numeric",nfleets="numeric", Snames="character",Fnames='array',
                                Stocks="list",Fleets="list",Obss="list",Imps="list",
                                OM = "list", Obs = "list", B_BMSY = "array", F_FMSY = "array",
                                B = "array", SSB="array", VB="array", FM = "array", C = "array",
                                TAC = "array", SSB_hist = "array",
                                CB_hist = "array", FM_hist = "array", Effort = "array", PAA= "array", CAA= "array",
                                CAL= "list", CALbins="list", MSY_P="array", FMSY_P="array", SSBMSY_P="array", Misc="list"))


setMethod("initialize", "MMSE", function(.Object, Name, nyears, proyears,
                                         nMPs, MPs, MPcond, MPrefs,
                                         nsim, nstocks, nfleets, Snames, Fnames, Stocks, Fleets, Obss, Imps,
                                         OM, Obs, B_BMSY, F_FMSY, B, SSB, VB, FM, C, TAC,
                                         SSB_hist, CB_hist, FM_hist, Effort = array(), PAA,  CAA, CAL, CALbins,
                                         MSY_P, FMSY_P, SSBMSY_P, Misc) {
  .Object@Name <- Name
  .Object@nyears <- nyears
  .Object@proyears <- proyears
  .Object@nMPs <- nMPs
  .Object@MPs <- MPs
  .Object@MPcond<-MPcond
  .Object@MPrefs<-MPrefs
  .Object@nsim <- nsim
  .Object@nstocks<-nstocks
  .Object@nfleets<-nfleets
  .Object@Snames<-Snames
  .Object@Fnames<-Fnames
  .Object@Stocks<-Stocks
  .Object@Fleets<-Fleets
  .Object@Obss<-Obs
  .Object@Imps<-Imps
  #.Object@MOM<-MOM
  .Object@OM <- OM
  .Object@Obs <- Obs
  .Object@B_BMSY <- B_BMSY
  .Object@F_FMSY <- F_FMSY
  .Object@B <- B
  .Object@SSB <- SSB
  .Object@VB <- VB
  .Object@FM <- FM
  .Object@C <- C
  .Object@TAC <- TAC
  .Object@SSB_hist <- SSB_hist
  .Object@CB_hist <- CB_hist
  .Object@FM_hist <- FM_hist
  .Object@Effort <- Effort
  .Object@PAA <- PAA
  .Object@CAA <- CAA
  .Object@CAL <- CAL
  .Object@CALbins <- CALbins
  .Object@Misc <- Misc

  .Object
})
