#' What objects of this class are available
#' 
#' Generic class finder
#' 
#' Finds objects of the specified class in the global environment or the
#' DLMtool package.
#' 
#' @param classy A class of object (character string, e.g. 'Fleet')
#' @param builtin Logical. Only return Objects of class 'classy' from DLMtool & DLMextra packages?
#' @examples
#' avail("OM")
#' @author T. Carruthers
#' @seealso \link{Can} \link{Cant} \link{avail}
#' @examples 
#' Stocks <- avail("Stock")
#' Fleets <- avail("Fleet")
#' MPs <- avail("MP")
#' @export 
avail <- function(classy, builtin=FALSE) {
  temp <- try(class(classy), silent=TRUE)
  if (class(temp) == "try-error") classy <- deparse(substitute(classy))
  if (temp == "function") classy <- deparse(substitute(classy))
  
  if (classy %in% c('Output', 'Input', "Mixed", "Reference")) {
    MPs <- avail('MP')
    gettype <- MPtype(MPs)
    temp <- gettype[gettype[,2] %in% classy,1]
    if (length(temp) < 1) stop("No MPs of type '", classy, "' found", call. = FALSE)
    return(temp)
    
  } else {
    if (builtin) {
      temp <- c(ls("package:OMtool")[vapply(ls("package:OMtool"), getclass, logical(1), classy = classy)])
    } else {
      temp <- c(ls("package:OMtool")[vapply(ls("package:OMtool"), getclass, logical(1), classy = classy)], 
                ls(envir = .GlobalEnv)[vapply(ls(envir = .GlobalEnv), getclass, logical(1), classy = classy)])
    }
    
    pkgs <- search()
    if ("package:DLMextra" %in% pkgs) {
      temp_extra <- ls("package:DLMextra")[vapply(ls("package:DLMextra"), getclass, logical(1), classy = classy)]
      temp <- c(temp, temp_extra)
    }
    
    if (classy == "Observation") message("Class 'Observation' has been re-named 'Obs'")	
    if (length(temp) < 1) stop("No objects of class '", classy, "' found", call. = FALSE)
    return(unique(temp))
  }
}




#' Is a value NA or zero.
#' 
#' As title
#' 
#' 
#' @param x A numeric value.
#' @return TRUE or FALSE 
#' @author T. Carruthers
#' @keywords internal
#' @export
NAor0 <- function(x) {
  if (length(x) == 0) 
    return(TRUE)
  if (length(x) > 0) 
    return(is.na(x[1]))
}


#' Calculate CV from vector of values 
#' 
#' 
#' @param x vector of numeric values 
#' @author T. Carruthers
#' @return numeric
#' @keywords internal
#' @export
cv <- function(x) sd(x)/mean(x)


#' Get parameters of lognormal distribution from mean and standard deviation in normal
#' space
#' 
#' @param m mean in normal space 
#' @param sd standard deviation in normal space
#' @author T. Carruthers
#' @return numeric
#' @describeIn sdconv Returns sigma of lognormal distribution
#' @keywords internal
#' @export
sdconv <- function(m, sd) (log(1 + ((sd^2)/(m^2))))^0.5


#' @describeIn sdconv Returns mu of lognormal distribution
#' @export
mconv <- function(m, sd) log(m) - 0.5 * log(1 + ((sd^2)/(m^2)))

#' Calculate parameters for beta distribution from mean and standard deviation in
#' normal space
#' 
#' @param m mean 
#' @param sd standard deviation
#' @author T. Carruthers
#' @return numeric
#' @describeIn alphaconv Returns alpha of beta distribution
#' @keywords internal
#' @export
alphaconv <- function(m, sd) m * (((m * (1 - m))/(sd^2)) - 1)


#' @describeIn alphaconv Returns beta of beta distribution
#' @export 
betaconv <- function(m, sd) (1 - m) * (((m * (1 - m))/(sd^2)) - 1)

#' Lognormal distribution for DLMtool 
#' 
#' Variant of rlnorm which returns the mean when reps = 1.
#' 
#' @param reps number of random numbers 
#' @param mu mean 
#' @param cv coefficient of variation
#' @param x vector 
#' @author T. Carruthers
#' @return numeric
#' @describeIn trlnorm Generate log-normally distributed random numbers
#' @keywords internal 
#' @export 
trlnorm <- function(reps, mu, cv) {
  if (all(is.na(mu))) return(rep(NA, reps))
  if (all(is.na(cv))) return(rep(NA, reps))
  if (reps == 1)  return(mu)
  return(rlnorm(reps, mconv(mu, mu * cv), sdconv(mu, mu * cv)))
}



#' @describeIn trlnorm Calculate density of log-normally distributed random numbers 
#' @export 
tdlnorm <- function(x, mu, cv) dlnorm(x, mconv(mu, mu * cv), sdconv(mu, mu * cv))


#' Setup parallel processing
#'
#' Sets up parallel processing using the snowfall package
#'
#' @param cpus the number of CPUs to use for parallel processing. If left empty
#' all physical cores will be used, unless `logical=TRUE`, in which case both 
#' physical and logical (virtual) cores will be used.
#' @param logical Use the logical cores as well? Using the virtual cores may
#' not lead to any significant increase in speed. 
#' You can test the optimal number of cores using `optCPU()`
#' @param ... other arguments passed to 'snowfall::sfInit'
#' @examples
#' \dontrun{
#' setup() # set-up the physical processors
#' setup(6) # set-up 6 processors
#' setup(logical=TRUE) # set-up physical and logical cores
#' }
#' @export 
setup <- function(cpus=NULL, logical=FALSE, ...) {
  if (is.null(cpus)) 
    cpus <- parallel::detectCores(logical=logical)
  if(snowfall::sfIsRunning()) 
    snowfall::sfStop()
  snowfall::sfInit(parallel=TRUE,cpus=cpus, ...)
  # sfLibrary("DLMtool", character.only = TRUE, verbose=FALSE)
  # pkgs <- search()
  # if ("package:MSEtool" %in% pkgs) 
  #   sfLibrary("MSEtool", character.only = TRUE, verbose=FALSE)
}


#' Determine optimal number of cpus
#'
#' @param nsim Numeric. Number of simulations.
#' @param thresh Recommended n cpus is what percent of the fastest time?
#' @param plot Logical. Show the plot?
#' @param msg Logical. Should messages be printed to console?
#' @param maxn Optional. Maximum number of cpus. Used for demo purposes
#'
#' @templateVar url parallel-processing 
#' @templateVar ref determining-optimal-number-of-processors
# #' @template userguide_link
#' 
#' @export
#' @seealso \link{setup}
#' @examples
#' \dontrun{
#' optCPU()
#' }
#' @author A. Hordyk
optCPU <- function(nsim=96, thresh=5, plot=TRUE, msg=TRUE, maxn=NULL) {
  cpus <- 1:parallel::detectCores()
  if (!is.null(maxn)) cpus <- 1:maxn
  
  time <- NA
  OM <- DLMtool::testOM
  OM@nsim <- nsim
  for (n in cpus) {
    if (msg) message('Running MSE with ', nsim, ' simulations and ', n, ' of ', max(cpus), ' cpus')
    if (n == 1) {
      snowfall::sfStop()
      st <- Sys.time()
      tt <- runMSE(OM, silent = TRUE)
      time[n] <- difftime(Sys.time(), st, units='secs')
    } else{
      if (msg) {
        setup(cpus=n)
      } else {
        sink('temp')
        suppressMessages(setup(cpus=n))
        sink()
      }
      st <- Sys.time()
      tt <- runMSE(OM, silent=TRUE, parallel=TRUE)  
      
      time[n] <- difftime(Sys.time(), st, units='secs')
      
    }
  } 
  df <- data.frame(ncpu=cpus, time=time)
  df$time <- round(df$time,2)
  rec <- min(which(time < min(time) * (1 + thresh/100)))
  if (plot) {
    plot(df, type='b', ylab="time (seconds)", xlab= "# cpus", bty="l", lwd=2)
    points(rec, df[rec,2], cex=2, pch=16, col="blue")
  }
  return(df)
}



#' @rdname tinyErr
#' @export
setGeneric("tinyErr", function(x, ...) standardGeneric("tinyErr")) 

#' @name tinyErr
#' @aliases tinyErr,OM-method
#' @title Remove observation, implementation, and process error
#' 
#' @description Takes an existing OM object and converts it to one without any observation
#' error, implementation error, very little process error, and/or gradients in
#' life history parameters and catchability.  
#' 
#' @details Useful for debugging and testing that MPs perform as expected under perfect conditions.
#'
#' @param x An object of class `OM` 
#' @param ... Arguments to generic function
#' @param obs Logical. Remove observation error? `Obs` is replaced with `Perfect_Info`
#' @param imp Logical. Remove implementation error? `Imp` is replaced with `Perfect_Imp`
#' @param proc Logical. Remove process error? All `sd` and `cv` slots in `Stock` 
#' and `Fleet` object are set to 0.
#' @param grad Logical. Remove gradients? All `grad` slots in `Stock` and 
#' `qinc` in `Fleet` are set to 0.
#' @param silent Logical. Display messages?
#' 
#' @templateVar url modifying-the-om
#' @templateVar ref the-tinyerr-function
# #' @template userguide_link
#' 
#' @return An updated object of class `OM`
#' @export
#'
#' @examples
#' OM_noErr <- tinyErr(OMtool::testOM)
setMethod("tinyErr", signature(x = "OM"),
          function(x, obs=TRUE, imp=TRUE, proc=TRUE, grad=TRUE, silent=FALSE) {
            OM <- x
            if (length(OM@cpars)>0) 
              warning("Note that this function doesn't apply to parameters in cpars.\n Must be removed manually e.g `OM@cpars$Perr_y <- NULL`")
            
            if (!inherits(OM, 'OM')) stop("Object must be class `OM`", call.=FALSE)
            OMperf <- new("OM", OMtool::Albacore, OMtool::Generic_Fleet,
                          OMtool::Perfect_Info, OMtool::Perfect_Imp)
            OMout <- OM 
            
            if (obs) {
              if (!silent) message("Removing all Observation Error")
              OMout <- Replace(OMout, OMperf, "Obs", silent = TRUE)
            }
            if (imp) {
              if (!silent) message("Removing all Implementation Error")
              OMout <- Replace(OMout, OMperf, "Imp", silent = TRUE)
            }
            if (proc) {
              if (!silent) message("Removing all Process Error")
              vars <- c("cv", "sd", "Perr", "AC")
              nms <- c(slotNames('Stock'), slotNames('Fleet'))
              ind <- unique(grep(paste(vars, collapse = "|"), nms, value = FALSE))
              for (X in seq_along(ind)) {
                n <- length(slot(OMout, nms[ind[X]]))
                if (n == 0) n <- 2
                slot(OMout, nms[ind[X]]) <- rep(0, n)
              }
            }
            if (grad) {
              if (!silent)  message("Removing all Gradients")
              vars <- c("grad", "inc")
              nms <- c(slotNames('Stock'), slotNames('Fleet'))
              ind <- unique(grep(paste(vars, collapse = "|"), nms, value = FALSE))
              for (X in seq_along(ind)) {
                n <- length(slot(OMout, nms[ind[X]]))
                if (n == 0) n <- 2
                slot(OMout, nms[ind[X]]) <- rep(0, n)
              }
            }
            OMout
          })

