
#' Run a Management Strategy Evaluation
#'
#' Functions to run the Management Strategy Evaluation (closed-loop
#' simulation) for a specified operating model
#'
#' @param OM An operating model object (class [MSEtool::OM-class] or class `Hist`). Also works for
#' `MOM` objects, as a wrapper for `ProjectMOM`
#' @param MPs A vector of methods (character string) of class MP
#' @param Hist Should model stop after historical simulations? Returns an object of
#' class 'Hist' containing all historical data
#' @param silent Should messages be printed out to the console?
#' @param nsim Optional. numeric value to override `OM@nsim`.
#' @param parallel Logical or a named list. Should MPs be run using parallel processing? 
#' For \code{runMSE}, can also be \code{"sac"} to run the entire MSE in parallel
#' using the split-apply-combine technique. See Details for more information. 
#' @param extended Logical. Return extended projection results?
#' if TRUE, `MSE@Misc$extended` is a named list with extended data
#' (including historical and projection by area), extended version of `MSE@Hist`
#' is returned, and returns `MSE@PPD` with StockPars, FleetPars, and ReferencePoints in `MSE@PPD`
#' @param checkMPs Logical. Check if the specified MPs exist and can be run on `SimulatedData`?
#'
#' @describeIn runMSE Run the Historical Simulations and Forward Projections
#'  from an object of class `OM
#' @details 
#' ## Running MPs in parallel
#' 
#' For most MPs, running in parallel can actually lead to an increase in computation time, due to the overhead in sending the 
#' information over to the cores. Consequently, by default the MPs will not be run in parallel if `parallel=TRUE` 
#' (although other internal code will be run in parallel mode).
#' 
#' To run MPs in parallel, specify a named list with the name of the MP(s) assigned as TRUE. For example,`parallel=list(AvC=TRUE`)
#' will run the `AvC` MP in parallel mode.
#' 
#' ## Split-apply-combine MSE in parallel
#' 
#' Additional savings in computation time can be achieved by running the entire simulation in batches. Individual simulations of the operating model 
#' are divided into separate cores using \link{SubCpars}, `Simulate` and `Project` are applied independently for each core via `snowfall::sfClusterApplyLB`, and the 
#' output (a list of MSE objects) is stitched back together into a single MSE object using \link{joinMSE}. 
#' 
#' The ideal number of cores will be determined based on the number of simulations and available cores.
#'  
#' There are several issues to look out for when using this split-apply-combine technique:
#' 
#' \itemize{
#' \item Numerical optimization for depletion may fail in individual cores when \code{OM@cpars$qs} is not specified.
#' \item Length bins should be specified in the operating model in \code{OM@cpars$CAL_bins}. Otherwise, length bins can vary by core and
#' create problems when combining into a single object.
#' \item Compared to non-parallel runs, sampled parameters in the operating model will vary despite the same value in \code{OM@seed}.
#' \item If there is an error in individual cores or while combining the parallel output into a single Hist or MSE object, the list of output (from the cores) will be returned.
#' }
#'
#' @return Functions return objects of class \linkS4class{Hist} or \linkS4class{MSE}
#' \itemize{
#'   \item Simulate - An object of class \linkS4class{Hist}
#'   \item Project - An object of class \linkS4class{MSE}
#'   \item runMSE - An object of class \linkS4class{MSE} if \code{Hist = TRUE} otherwise a class \linkS4class{Hist} object
#' }
#' @export
runMSE <- function(OM=MSEtool::testOM, MPs = NA, Hist=FALSE, silent=FALSE,
                   parallel=FALSE, extended=FALSE, checkMPs=FALSE) {

  # ---- Initial Checks and Setup ----
  if (methods::is(OM,'OM')) {
    if (OM@nsim <=1) stop("OM@nsim must be > 1", call.=FALSE)

  } else if (methods::is(OM,'Hist')) {
    if (!silent) message("Using `Hist` object to reproduce historical dynamics")

    # # --- Extract cpars from Hist object ----
    # cpars <- list()
    # cpars <- c(OM@SampPars$Stock, OM@SampPars$Fleet, OM@SampPars$Obs,
    #            OM@SampPars$Imp, OM@OMPars, OM@OM@cpars)
    # 
    # # --- Populate a new OM object ----
    # newOM <- OM@OM
    # newOM@cpars <- cpars
    # OM <- newOM
  } else {
    stop("You must specify an operating model")
  }

  # check MPs
  if (checkMPs & !Hist)
    MPs <- CheckMPs(MPs=MPs, silent=silent)
  
  if (is.character(parallel) && parallel == "sac") {
    MSEout <- runMSE_sac(OM, MPs, Hist = Hist, silent = silent, extended = extended)
    return(MSEout)
  }

  if (methods::is(OM,'OM')) {
    HistSims <- Simulate(OM, parallel, silent)  
  } else {
    HistSims <- OM
  }
  
  if (Hist) {
    if(!silent) message("Returning historical simulations")
    return(HistSims)
  }

  if(!silent) message("Running forward projections")
  MSEout <- try(Project(Hist=HistSims, MPs, parallel, silent, extended = extended, checkMPs=FALSE), silent=TRUE)
  if (methods::is(MSEout, 'try-error')) {
    message('The following error occured when running the forward projections: ',
            crayon::red(attributes(MSEout)$condition))
    message('Returning the historical simulations (class `Hist`). To avoid re-running spool up, ',
            'the forward projections can be run with ',
            '`runMSE(Hist, MPs, ...)`')
    return(HistSims)
  }

  MSEout

}



runMSE_sac <- function(OM, MPs, Hist = FALSE, silent = FALSE, extended = FALSE) {
  
  if (OM@nsim <= 48) stop("OM@nsim should be greater than 48 to effectively use split-apply-combine.")
  if (is.null(OM@cpars$CAL_bins)) warning("OM@cpars$CAL_bins was not provided which can create issues with parallel = \"sac\"")
  ncpus <- set_parallel(TRUE)
  
  nsim <- OM@nsim
  nits <- ceiling(nsim/48)
  itsim <- rep(48,nits)
  if (nits < ncpus) {
    if (nits < 4) {
      nits <- 4
      itsim <- rep(ceiling(nsim/4), 4)
    } else{
      nits <- ncpus
      itsim <- rep(ceiling(nsim/ncpus), ncpus)
    }
  }
  cnt <- 1
  while (sum(itsim) != nsim | any(itsim<2)) {
    diff <-  nsim - sum(itsim)
    if (diff >0) {
      itsim[cnt] <- itsim[cnt]+1
    }
    if(diff < 0) {
      itsim[cnt] <- itsim[cnt]-1
    }
    cnt <- cnt+1
    if (cnt > length(itsim)) cnt <- 1
  }
  
  #### Split
  if (!silent) {
    message("Running MSE using split-apply-combine on ", length(itsim), " cores with these number of simulations:\n",
            paste0(itsim, collapse = ", "))
  }
  sims <- lapply(1:length(itsim), function(i) {
    if (i > 1) {
      (sum(itsim[1:(i-1)]) + 1): sum(itsim[1:i])
    } else {
      1:itsim[i]
    }
  })
  
  #### Apply Simulate() in parallel
  if (!silent) message("Running Simulate() in parallel..")
  HistList <- snowfall::sfClusterApplyLB(1:nits, function(i, OM, iter) {
    OM@seed <- OM@seed + i
    tryCatch(Simulate(SubCpars(OM, sims = iter[[i]]), silent = TRUE), error = function(e) as.character(e))
  }, OM = OM, iter = sims)
  
  # Error check Hist objects
  HistErr <- sapply(HistList, function(x) !inherits(x, "Hist"))
  if (any(HistErr)) {
    warning("Returning list of Hist objects. There was an error when running Simulate() for core(s): ", paste0(c(1:length(HistErr))[HistErr], collapse = ", "))
    return(HistList)
  }
  
  # Combine Hist objects and exit
  if (Hist) {
    Histout <- try(joinHist(HistList), silent = TRUE)
    if (methods::is(Histout, "try-error")) {
      warning("Error in joinHist() for combining Hist objects. Returning list of Hist objects.")
      Histout <- HistList
    }
    return(Histout)
  }
  
  #### Apply Project() in parallel
  Export_customMPs(MPs)
  if (!silent) message("Running Project() in parallel with ", length(MPs), " MPs..")
  MSElist <- snowfall::sfClusterApplyLB(HistList, function(i, MPs, extended) {
    tryCatch(Project(i, MPs = MPs, parallel = FALSE, silent = TRUE, extended = extended, checkMPs = FALSE),
             error = function(e) as.character(e))
  }, MPs = MPs, extended = extended)
  
  #### Combine MSE objects
  MSEout <- try(joinMSE(MSElist), silent = TRUE)
  if (methods::is(MSEout, "try-error")) {
    warning("Error in joinMSE() for combining MSE objects. Returning list of MSE objects.")
    MSEout <- MSElist
  }
  return(MSEout)
}
