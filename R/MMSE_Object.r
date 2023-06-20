# ---- multiHist class ----


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
#' @slot Imps The fleet specific implementation error operating model objects. Hierarchical list, fleets nested in stocks.
#'
#' @slot OM A table of sampled parameters of the operating model. Data frame of nsim rows.
#' @slot Obs A table of sampled parameters of the observation model. Data frame of nsim rows.
#' 
#' @slot SB_SBMSY Simulated spawning biomass relative to SBMSY over the projection. An array with dimensions: nsim, nStocks, nMPs, proyears. Non-negative real numbers
#' @slot F_FMSY Simulated fishing mortality rate relative to FMSY over the projection. An array with dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative real numbers
#' @slot N Simulated stock numbers over the projection. An array with dimensions: nsim, nStocks, `maxage`+1, nMPs, proyears, nareas. Non-negative real numbers
#' @slot B Simulated stock biomass over the projection. An array with dimensions: nsim, nStocks, nMPs, proyears. Non-negative real numbers
#' @slot SSB Simulated spawning stock biomass over the projection. An array with dimensions: nsim, nStocks, nMPs, proyears. Non-negative real numbers
#' @slot VB Simulated vulnerable biomass over the projection. An array with dimensions: nsim, nStocks, nMPs, proyears. Non-negative real numbers
#' @slot FM Simulated fishing mortality rate over the projection. An array with dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative real numbers
#' @slot SPR A list of SPR values. Currently not used.
#' @slot Catch Simulated catches (landings) over the projection. An array with dimensions: nsim,  nStocks, nFleets, nMPs, proyears. Non-negative real numbers
#' @slot Removals Simulated removals (landings+discards) over the projection. An array with dimensions: nsim,  nStocks, nFleets, nMPs, proyears. Non-negative real numbers
#' @slot Effort Simulated relative fishing effort in the projection years. An array with dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative real numbers
#' @slot TAC Simulated Total Allowable Catch (prescribed) over the projection (this is NA for input controls). An array with dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative real numbers
#' @slot TAE Simulated Total Allowable Effort (prescribed) over the projection (this is NA for output controls). An array with dimensions: nsim, nStocks, nFleets, nMPs, proyears. Non-negative real numbers
#' @slot BioEco A named list of bio-economic output. Not currently used.
#' @slot RefPoint Named list of annual MSY reference points MSY, FMSY, and SBMSY.
#' Array with dimensions: nsim, nstocks, nMPs, nyears+proyears. Will be the same as `multiHist@Ref$ByYear` unless selectivity is changed by MP
#' @slot multiHist The object of class `multiHist` containing information from the spool-up period.
#' @slot PPD Posterior predictive data. List of `Data` objects at the end of
#'  the projection period (length `nMPs`)
#' @slot Misc Miscellaneous output such as posterior predictive data
#' @author T. Carruthers
#' @keywords classes
#' @export
setClass("MMSE", representation(Name = "character", nyears = "numeric",
                                proyears = "numeric", nMPs = "numeric", MPs = "list", MPcond="character",MPrefs="array",
                                nsim = "numeric",nstocks="numeric",nfleets="numeric", Snames="character",Fnames='array',
                                Stocks="list",Fleets="list",Obss="list",Imps="list",
                                OM = "list", Obs = "list", SB_SBMSY = "array", F_FMSY = "array",
                                N='array', B = "array", SSB="array", VB="array", FM = "array",
                                SPR='list', Catch = "array", Removals='array', Effort = "array",
                                TAC = "array", TAE='array',
                                BioEco='list', RefPoint='list', multiHist='list',
                                PPD='list', Misc="list"))


setMethod("initialize", "MMSE", function(.Object, Name, nyears, proyears,
                                         nMPs, MPs, MPcond, MPrefs,
                                         nsim, nstocks, nfleets, Snames, Fnames, Stocks, Fleets, Obss, Imps,
                                         OM, Obs, SB_SBMSY, F_FMSY, N, B, SSB, VB, FM, SPR,
                                         Catch, Removals, Effort, TAC,TAE,
                                         BioEco, RefPoint, multiHist, PPD, Misc) {
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
  .Object@SB_SBMSY <- SB_SBMSY
  .Object@F_FMSY <- F_FMSY
  .Object@N <- N
  .Object@B <- B
  .Object@SSB <- SSB
  .Object@VB <- VB
  .Object@FM <- FM
  .Object@SPR <- SPR
  .Object@Catch <- Catch
  .Object@Removals <- Removals
  .Object@Effort <- Effort
  .Object@TAC <- TAC
  .Object@TAE <- TAE
  .Object@BioEco <- BioEco
  .Object@RefPoint <- RefPoint
  .Object@multiHist <- multiHist
  .Object@PPD <- PPD
  .Object@Misc <- Misc

  .Object
})



# ---- Summary of MMSE object ----

#' Summary of MMSE object
#'
#' @param object object of class `MMSE`
#' @param ... a list of names of PM methods
#' @param silent Should summary be printed to console? Logical.
#' @param Refs An optional named list (matching the PM names) with numeric values to override the default `Ref` values. See examples.
#' @rdname summary-MMSE
#' @export
setMethod('summary', signature="MMSE", function(object, ..., silent=FALSE, Refs=NULL) {
  PMlist <- unlist(list(...))
  
  if(length(PMlist) == 0) PMlist <- c("PNOF", "P50", "AAVY", "LTY")
  if (!methods::is(PMlist,'character')) stop("Must provide names of PM methods")
  # check
  for (X in seq_along(PMlist))
    if (!methods::is(get(PMlist[X]), "PM")) stop(PMlist[X], " is not a valid PM method")
  
  if (!silent) message("Calculating Performance Metrics")
  storeMean <- vector('list', length(PMlist))
  storeName <- vector('list', length(PMlist))
  storeCap <- vector('list', length(PMlist))
  storeHeading <- vector('list', length(PMlist))
  storeMP <- vector('list', length(PMlist))
  for (X in 1:length(PMlist)) {
    ref <- Refs[[PMlist[X]]]
    if (is.null(ref)) {
      runPM <- eval(call(PMlist[X], object))
    } else {
      runPM <- eval(call(PMlist[X], object, Ref=ref))
    }
    storeMean[[X]] <- runPM@Mean
    storeName[[X]] <- runPM@Name
    storeCap[[X]] <- runPM@Caption
    storeMP[[X]] <- runPM@MPs
  }
  
  df <- data.frame('MP'=storeMP[[1]], signif(do.call('cbind', storeMean),2), stringsAsFactors = FALSE)
  # heading <- do.call('rbind', storeHeading)
  colnames(df)[2:(length(PMlist)+1)] <- PMlist #caps # gsub(" ", "", caps)
  if (!silent) {
    dfprint <- data.frame('Performance Metrics' = do.call('rbind', storeName), gap="", do.call('rbind', storeCap))
    names(dfprint)[2:3] <- ''
    print(dfprint)
    cat("\n")
    cat("\nPerformance Statistics:\n")
    print(df)
  }
  
  invisible(df)
  
})

# ---- show method ----
#' @rdname show-MSEtool
setMethod("show", "MMSE", function(object) show_int(object))
