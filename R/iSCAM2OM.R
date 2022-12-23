# === OM specification using iSCAM stock assessment ====================

#' @name iSCAM2OM
#' @title Reads MPD or MCMC estimates and data from iSCAM file structure into an operating model
#' @aliases iSCAM2Data
#' @description Functions for importing an iSCAM assessment. From a fitted model, \code{iSCAM2OM}
#' populates the various slots of an operating model and \code{iSCAM2Data} generates a Data object.
#' These functions rely on several functions written by Chris Grandin (DFO PBS).
#' @param iSCAMdir A folder with iSCAM input and output files in it. Alternatively, a list returned by
#' \link{load.iscam.files}.
#' @param nsim The number of simulations to take for parameters with
#' uncertainty (for OM@cpars custom parameters)
#' @param proyears The number of MSE projection years
#' @param mcmc Logical, whether to use mcmc samples to create custom parameters cpars. Alternatively, a list
#' returned by \link{read.mcmc}. Set the seed for the function to sub-sample the mcmc samples.
#' @param Name The name of the operating model
#' @param Source Reference to assessment documentation e.g. a url
#' @param length_timestep How long is a model time step in years
#' (e.g. a quarterly model is 0.25, a monthly model 1/12) (currently only uses annual time step)
#' @param nyr_par_mu integer, the number of recent years to estimate vulnerability over for future projections
#' @param Author Who did the assessment
#' @param report logical should a numbers at age reconstruction plot be produced?
#' @param silent logical should progress reporting be printed to the console?
#' @section Biological parameters:
#' The function calls \code{model <- load.iscam.files(iSCAMdir)} and grabs the following matrices:
#' 
#' \itemize{
#' \item \code{model$mpd$d3_weight_mat} - fecundity (product of weight and maturity at age)
#' \item \code{model$mpd$ma} - maturity at age
#' }
#' 
#' @section MPD historical reconstruction:
#' The function calls \code{model <- load.iscam.files(iSCAMdir)} and then grabs the following matrices:
#' 
#' \itemize{
#' \item \code{model$mpd$N} - abundance at age
#' \item \code{model$mpd$F} - fishing mortality at age
#' \item \code{model$mpd$M} - natural mortality at age
#' }
#' 
#' If a delay-difference model is recognized, then the following is used instead:
#' \itemize{
#' \item \code{model$mpd$F_dd} - fishing mortality at age
#' \item \code{model$mpd$M_dd} - natural mortality at age
#' }
#' 
#' Abundance at age is reconstructed using \code{model$mpd$rt} (recruitment) and projected with \code{F_dd} and
#' \code{M_dd} to match \code{model$mpd$numbers}.
#'   
#' @section MCMC historical reconstruction:
#' If \code{mcmc = TRUE} the function calls \code{mcmc_model <- read.mcmc(iSCAMdir)}, and grabs \code{nsim} sub-samples of the MCMC output 
#' through the following arrays:
#' 
#' \itemize{
#' \item \code{mcmc_model$params} and \code{mcmc_model$ft} - fishing mortality at age from the fleet selectivity parameters and apical F's
#' \item \code{mcmc_model$m} - year-specific natural mortality at age
#' \item \code{mcmc_model$params$rinit} and \code{mcmc_model$rt} - recruitment and abundance
#' }
#' 
#' @section Start age:
#' While the iSCAM start age can be greater than zero, abundance at age is back-calculated to age zero using the M
#' at the start age.
#' 
#' These arrays are then passed to \code{VPA2OM} to generate the operating model.
#' 
#' @section Reference points:
#' iSCAM calculates the stock-recruit relationship and subsequently a single set of MSY and unfished reference 
#' points using R0, steepness, and unfished spawners per recruit from the mean M, fecundity, and growth (mean with 
#' respect to time).
#' 
#' R0 and h are recalculated for the operating model by obtaining the stock-recruit alpha and beta from the 
#' iSCAM parameters and the mean unfished spawners per recruit in the first \code{ageM} (age of 50% maturity) years.
#' R0 is also back calculated to age zero. 
#' 
#' @author T. Carruthers, Q. Huynh
#' @export
iSCAM2OM<-function(iSCAMdir, nsim=48, proyears=50, mcmc=FALSE, Name="iSCAM model",
                   Source="No source provided", length_timestep=1,
                   nyr_par_mu=2, Author="No author provided", report=FALSE, silent=FALSE) {
  
  nseas<-1/length_timestep # defaults to an annual model
  
  message("-- Using function of Chris Grandin (DFO PBS) to extract data from iSCAM file structure --")
  if(is.character(iSCAMdir)) {
    replist <- load.iscam.files(iSCAMdir)
  } else {
    replist <- iSCAMdir
  }
  if(is.logical(mcmc) && mcmc) {
    message("-- Reading MCMC files --")
    mcmc_model <- read.mcmc(iSCAMdir)
  } else if(is.list(mcmc)) {
    mcmc_model <- mcmc
  }
  message("-- End of iSCAM extraction operations --")
  
  do_mcmc <- exists("mcmc_model", inherits = FALSE)
  if(do_mcmc) {
    n_mcmc <- nrow(mcmc_model$params)
    if(nsim < n_mcmc) {
      mcmc_samp <- sample(1:n_mcmc, nsim)
    } else {
      message("You requested a greater number of simulations than the number of mcmc samples that are available - sampling with replacement")
      mcmc_samp <- sample(1:n_mcmc, nsim, replace = TRUE)
    }
  }
  
  delay_diff <- !is.null(replist$mpd$F_dd)
  
  # get dimensions
  nyears<-(replist$dat$end.yr-replist$dat$start.yr+1)
  maxage<-replist$dat$end.age
  sage<-replist$dat$start.age
  n_age <- maxage + 1
  
  # filling dimensions
  aind <- sage:maxage # for filling all quantities (that do not include age zero)
  nafill <- length(aind)
  
  # growth parameters
  Linf <- replist$dat$linf[1]
  K <- replist$dat$k[1]
  t0 <- replist$dat$to[1]
  
  ageArray <- array(rep(0:maxage,each=nsim),c(nsim,n_age,nyears))
  
  # make matrices
  Mataa<-laa<-array(NA,c(nsim,n_age,nyears)) # Maturity, length-at-age
  faa<-Maa<-waa<-array(0,c(nsim,n_age,nyears)) # N, F, M, weight-at-age
  naa <- array(NA_real_, c(nsim, n_age, nyears + 1))
  
  # F and selectivity
  if(delay_diff) {
    if(do_mcmc) {
      faat <- lapply(mcmc_model$ft[[1]], function(x) as.matrix(x)[mcmc_samp, ]) %>% 
        simplify2array() %>% apply(1:2, sum)
      faa[, aind+1, ] <- array(faat, c(nsim, nyears, nafill)) %>% aperm(c(1, 3, 2))
    } else {
      faat <- matrix(replist$mpd$F_dd, length(aind), nyears, byrow = TRUE)
      faa[, aind+1, ]<-array(rep(faat,each=nsim),c(nsim,nafill,nyears))
    }
  } else {
    if(do_mcmc) {
      faat <- local({
        iseltype <- replist$ctl$sel["iseltype", ] # includes surveys
        nfleet <- length(iseltype)
        apicalF <- lapply(mcmc_model$ft[[1]], function(x) x[mcmc_samp, ])
        sel <- lapply(1:nfleet, function(x) {
          a <- matrix(0:maxage, nsim, n_age, byrow = TRUE)
          if(iseltype[x] == 1 || iseltype[x] == 6) {
            sel_g <- mcmc_model$params[[paste0("sel_g", x)]][mcmc_samp]
            sel_sd <- mcmc_model$params[[paste0("sel_sd", x)]][mcmc_samp]
            return(1/(1 + exp(-(a - sel_g)/sel_sd)))
          } else {
            stop("selectivity type not expected.")
          }
        })
        F_at_age <- lapply(1:nfleet, function(x) {
          lapply(1:nsim, function(xx) outer(as.numeric(apicalF[[x]][xx, ]), sel[[x]][xx, ])) %>% simplify2array()
        }) %>% simplify2array() # array of year x n_age x nsim x nfleet
        apply(F_at_age, 1:3, sum)
      })
      faa[, aind+1, ] <- faat[, aind+1, ] %>% aperm(3:1)
    } else {
      faat <- t(replist$mpd$F) # age x nyears
      faa[,aind+1,]<-array(rep(faat,each=nsim),c(nsim,nafill,nyears))
    }
    
    # Fishing mortality rate for years with F = 0
    fout <- lapply(1:nsim, function(x) {
      fout <- faa[x, , ]
      tofill <- apply(fout, 2, function(x) all(x == 0))
      if(any(tofill)) {
        Vtemp <- apply(fout, 1, mean)
        fout[, tofill] <- 1e-8 * Vtemp/max(Vtemp)
      }
      return(fout)
    })
    faa <- simplify2array(fout) %>% aperm(c(3, 1, 2))
  }
  
  # M
  if(delay_diff) {
    if(do_mcmc) {
      Maa[, aind + 1, ] <- array(mcmc_model$params$m_gs1[mcmc_samp], c(nsim, nafill, nyears))
    } else {
      Maa[, aind + 1, ] <- array(replist$mpd$M_dd, c(nyears, length(aind), nsim)) %>% aperm(3:1)
    }
  } else {
    if(do_mcmc) {
      if(is.null(mcmc_model$m)) { # Assume constant M over time
        Maa[, aind + 1, ] <- mcmc_model$params$m_gs1[mcmc_samp]
      } else {
        Maa[, aind + 1, ] <- mcmc_model$m[mcmc_samp, ] %>% as.matrix() %>% array(c(nsim, nyears, nafill)) %>% aperm(c(1, 3, 2))
      }
    } else {
      Maa[, aind + 1, ] <- rep(t(replist$mpd$M),each=nsim)
    }
  }
  
  # Weight at age
  waat<-t(replist$mpd$d3_wt_mat)[,1:nyears]/replist$mpd$ma # age x nyears
  waa[,aind+1,]<-array(rep(waat,each=nsim),c(nsim,nafill,nyears)) # !
  
  # Maturity
  if(sage > 0) {
    Mataat <- c(rep(0, sage), replist$mpd$ma)
  } else {
    Mataat <- replist$mpd$ma
  }
  
  Mataa[]<-rep(Mataat,each=nsim)
  laa<-Linf*(1-exp(-K*(ageArray-t0)))
  laa[1:sage] <- 0
  
  # Abundance
  if(delay_diff) {
    if(do_mcmc) {
      naa[, aind + 1, 1] <- vapply(mcmc_samp, get_Ninit_DD, numeric(length(aind)), 
                                   n_age = length(aind), mcmc_model = mcmc_model, replist = replist,
                                   ctl = replist$ctl$misc[, 1]["unfishedfirstyear"] < 2) %>% t()
      naa[, sage + 1, 2:sage] <- naa[, sage + 1, 1]
      naa[, sage + 1, (sage + 1):nyears] <- mcmc_model$rt[[1]][mcmc_samp, ] %>% as.matrix()
      
      for(y in 1:nyears) {
        naa[, (sage+2):n_age, y+1] <- naa[, (sage+1):(n_age-1), y] * 
          exp(-faa[, (sage+1):(n_age-1), y] - Maa[, (sage+1):(n_age-1), y])
        naa[, n_age, y+1] <- naa[, n_age, y+1] + naa[, n_age, y] * exp(-faa[, n_age, y] - Maa[, n_age, y])
      }
      naa[, sage + 1, nyears + 1] <- mcmc_model$params$rbar[mcmc_samp]
    } else {
      
      naat <- matrix(NA_real_, n_age, nyears + 1)
      NPR <- NPR_DD(replist$mpd$M_dd[1], 
                    FF = ifelse(replist$ctl$misc[, 1]["unfishedfirstyear"] < 2, 0, replist$mpd$F_dd[1]),
                    n_age = length(aind))
      
      Rinit <- replist$mpd$numbers[1]/sum(NPR)
      naat[sage:maxage + 1, 1] <- Rinit * NPR
      naat[sage + 1, 2:sage] <- replist$mpd$rbar * exp(replist$par$log_rec_devs[2:sage])
      naat[sage + 1, (sage + 1):nyears] <- replist$mpd$rt
      for(y in 1:nyears) {
        naat[(sage+2):n_age, y+1] <- naat[(sage+1):(n_age-1), y] * exp(-replist$mpd$F_dd[y] - replist$mpd$M_dd[y])
        naat[n_age, y+1] <- naat[n_age, y+1] + naat[n_age, y] * exp(-replist$mpd$F_dd[y] - replist$mpd$M_dd[y])
      }
      naat[sage + 1, nyears + 1] <- replist$mpd$rbar
      naa <- array(rep(naat, each = nsim), c(nsim, n_age, nyears + 1))
    }
  } else {
    if(do_mcmc) {
      rmcmc_yr <- as.numeric(colnames(mcmc_model$rt[[1]]))
      rmcmc_ind <- match(rmcmc_yr, replist$dat$start.yr:replist$dat$end.yr)
      naa[, sage + 1, rmcmc_ind] <- mcmc_model$rt[[1]][mcmc_samp, ] %>% as.matrix()
      
      NPR <- vapply(1:nsim, function(x) NPR_DD(Maa[x, sage + 1, 1], n_age = length(aind)), numeric(length(aind))) %>% t()
      
      naa[, sage:maxage + 1, 1] <- mcmc_model$params$rinit[mcmc_samp] * NPR
      if(min(rmcmc_ind) > 2) naa[, sage+1, 2:(min(rmcmc_ind) - 1)] <- naa[, sage+1, 1]
      for(y in 2:nyears) {
        for(a in (sage + 2):n_age) naa[, a, y] <- naa[, a-1, y-1] * exp(-faa[, a-1, y-1] - Maa[, a-1, y-1]) 
        naa[, n_age, y] <- naa[, n_age, y] + naa[, n_age, y-1] * exp(-faa[, n_age, y-1] - Maa[, n_age, y-1]) 
      }
    } else {
      naat <- matrix(NA_real_, n_age, nyears + 1)
      naat[(sage+1):n_age, ] <- t(replist$mpd$N) # age x nyears
      naa <- array(rep(naat, each = nsim), c(nsim, n_age, nyears + 1))
    }
  }
  
  if(sage > 0) { # Missing cohorts to be filled in by VPA2OM
    aind_missing <- sage:1
    for(i in 1:length(aind_missing)) {
      Maa[, aind_missing[i], ] <- tiny + .Machine$double.eps
      naa[, aind_missing[i], 2:nyears - 1] <- naa[, aind_missing[i] + 1, 2:nyears] * exp(Maa[, aind_missing[i], 2:nyears - 1])
    }
  }
  
  
  # steepness and sigmaR
  if(do_mcmc) {
    h <- mcmc_model$params$h[mcmc_samp]
    Perr <- sqrt((1 - mcmc_model$params$rho)/mcmc_model$params$vartheta)[mcmc_samp]
    R0 <- mcmc_model$params$ro[mcmc_samp]
  } else {
    h <- rep(replist$mpd$steepness, nsim)
    Perr <- rep(sqrt((1 - replist$par$theta6)/replist$par$theta7), nsim)
    R0 <- rep(replist$mpd$ro, nsim)
  }
  
  phi0 <- local({ # Get unfished spawners per recruit from mean M 
    wt <- apply(waat, 1, mean)
    fec <- apply(replist$mpd$d3_wt_mat, 2, mean)
    if(do_mcmc) {
      Mbar <- apply(Maa, 1:2, mean)
      vapply(1:nsim, function(x) {
        MSYCalcs(logF = log(1e-8), M_at_Age = Mbar[x, ], 
                 Wt_at_Age = c(rep(0, sage), wt), Mat_at_Age = c(rep(0, sage), replist$mpd$ma), 
                 Fec_at_Age = c(rep(0, sage), fec), V_at_Age = rep(0, n_age), 
                 maxage = n_age - 1, SRrelx = 4, opt = 0, plusgroup = 1)["SB"] %>% as.numeric()
      }, numeric(1))
    } else {
      Mbar <- apply(Maa[1, , ], 1, mean)
      MSYCalcs(logF = log(1e-8), M_at_Age = Mbar,
               Wt_at_Age = c(rep(0, sage), wt), Mat_at_Age = c(rep(0, sage), replist$mpd$ma), 
               Fec_at_Age = c(rep(0, sage), fec), V_at_Age = rep(0, n_age),
               maxage = n_age - 1, SRrelx = 4, opt = 0, plusgroup = 1)["SB"] %>% as.numeric()
    }
  })
  
  # make the OM
  OM <- Assess2OM(Name=Name,
                  proyears=proyears, interval=2, CurrentYr=replist$dat$end.yr, h=h,
                  Obs = MSEtool::Imprecise_Unbiased, Imp=MSEtool::Perfect_Imp,
                  naa[, , 1:nyears], faa, waa, Mataa, Maa, laa,
                  nyr_par_mu = nyr_par_mu, LowerTri=sage,
                  recind=0, plusgroup=TRUE, altinit=0, fixq1=TRUE,
                  report=report, silent=FALSE, R0 = R0, phi0 = phi0, Perr = Perr)
  
  # growth parameters
  OM@cpars$Linf <- rep(Linf, nsim)
  OM@cpars$K <- rep(K, nsim)
  OM@cpars$t0 <- rep(t0, nsim)
  
  OM@CurrentYr <- max(replist$mpd$yr)
  
  # Observation model parameters ==============================================================================
  OM@cpars$Data <- iSCAM2Data(replist, Name = Name, Source = Source, length_timestep = length_timestep,
                              Author = Author)
  if(!all(is.na(length(OM@cpars$Data@AddInd)))) {
    OM@cpars$AddIbeta <- matrix(1, nsim, dim(OM@cpars$Data@AddInd)[2])
  }
  
  return(OM)
}


NPR_DD <- function(M, FF = 0, n_age) {
  NPR <- exp(-(FF + M) * c(1:n_age - 1))
  NPR[length(NPR)] <- NPR[length(NPR)]/(1 - exp(-FF - M))
  return(NPR)
}

get_Ninit_DD <- function(i, n_age, mcmc_model, replist, ctl) {
  FF <- ifelse(ctl, 0, mcmc_model$ft[[1]][[1]][i, 1])
  NPR <- NPR_DD(mcmc_model$params$m_gs1[i], FF, n_age)
  SSBPR <- sum(NPR * replist$mpd$d3_wt_mat[1, ])
  Rinit <- mcmc_model$sbt[[1]][i, 1]/SSBPR
  Rinit * NPR
}

#' @name iSCAM
#' @title Reads iSCAM files into a hierarchical R list object
#' @seealso \link{iSCAM2OM}
#' @description Internal functions for reading iSCAM input and output files
#' into R
#' @author Chris Grandin (DFO PBS)
NULL

#' @describeIn iSCAM Wrapper function to generate R list
#' @param model.dir An iSCAM directory
#' @param burnin The initial mcmc samples to be discarded
#' @param thin The degree of chain thinning 1 in every thin
#' iterations is kept
#' @param verbose Should detailed outputs be provided.
#' @export
load.iscam.files <- function(model.dir,
                             burnin = 1000,
                             thin = 1,
                             verbose = FALSE){
  ## Load all the iscam files for output and input, and return the model object.
  ## If MCMC directory is present, load that and perform calculations for mcmc
  ##  parameters.

  starter.file.name <- "iscam.dat"
  par.file <- "iscam.par"
  rep.file <- "iscam.rep"
  mcmc.file <- "iscam_mcmc.csv"
  mcmc.biomass.file <- "iscam_sbt_mcmc.csv"
  mcmc.recr.file <- "iscam_rt_mcmc.csv"
  mcmc.recr.devs.file <- "iscam_rdev_mcmc.csv"
  mcmc.fishing.mort.file <- "iscam_ft_mcmc.csv"
  mcmc.fishing.mort.u.file <- "iscam_ut_mcmc.csv"
  mcmc.vuln.biomass.file <- "iscam_vbt_mcmc.csv"
  mcmc.proj.file <- "iscammcmc_proj_Gear1.csv"
  mpd.proj.file <- "iscammpd_proj_Gear1.csv"

  model <- list()
  model$path <- model.dir
  ## Get the names of the input files
  inp.files <- fetch.file.names(model.dir, starter.file.name)
  model$dat.file <- inp.files[[1]]
  model$ctl.file <- inp.files[[2]]
  #model$proj.file <- inp.files[[3]]

  ## Load the input files
  model$dat <- read.data.file(model$dat.file)
  model$ctl <- read.control.file(model$ctl.file,
                                 model$dat$num.gears,
                                 model$dat$num.age.gears)
  #model$proj <- read.projection.file(model$proj.file)

  model$par <- read.par.file(file=file.path(model.dir, par.file))
  ## Load MPD results
  model$mpd <- read.report.file(file.path(model.dir, rep.file))
  model.dir.listing <- dir(model.dir)
  ## Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  ## Set the mcmc path. This doesn't mean it exists.
  model$mcmcpath <- file.path(model.dir, "mcmc")

  ## If it has an 'mcmc' sub-directory, load it
  if(dir.exists(model$mcmcpath)){

   # model$mcmc <- read.mcmc(model$mcmcpath)
    model$mcmc <- read.mcmc(model.dir)
    ## Do the mcmc quantile calculations
    model$mcmccalcs <- calc.mcmc(model,
                                 burnin,
                                 thin,
                                 lower = 0.025,
                                 upper = 0.975)
  }
  model
}


#' @describeIn iSCAM A function for returning the three types of iSCAM input and output files
#' @param path File path
#' @param filename The filename
#' @export
fetch.file.names <- function(path, ## Full path to the file
                             filename){
  ## Read the starter file and return a list with 3 elements:
  ## 1. Data file name
  ## 2. Control file name
  ## 3. Projection file name

  ## Get the path the file is in
  d <- readLines(file.path(path, filename), warn = FALSE)
  ## Remove comments
  d <- gsub("#.*", "", d)
  ## Remove trailing whitespace
  d <- gsub(" +$", "", d)
  list(file.path(path, d[1]),
       file.path(path, d[2]),
       file.path(path, d[3]))
}



#' @describeIn iSCAM A function for returning the results of the .rep iscam file
#' @param fn File location
#' @export
read.report.file <- function(fn){
  # Read in the data from the REP file given as 'fn'.
  # File structure:
  # It is assumed that each text label will be on its own line,
  # followed by one or more lines of data.
  # If the label is followed by a single value or line of data,
  #  a vector will be created to hold the data.
  # If the label is followed by multiple lines of data,
  #  a matrix will be created to hold the data. The matrix might be
  #  ragged so a check is done ahead of time to ensure correct
  #  matrix dimensions.
  #
  # If a label has another label following it but no data,
  #  that label is thrown away and not included in the returned list.
  #
  # A label must start with an alphabetic character followed by
  # any number of alphanumeric characters (includes underscore and .)

  dat <- readLines(fn, warn = FALSE)
  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Find the line indices of the labels
  # Labels start with an alphabetic character followed by
  # zero or more alphanumeric characters
  idx  <- grep("^[[:alpha:]]+[[:alnum:]]*", dat)
  objs <- dat[idx]     # A vector of the object names
  nobj <- length(objs) # Number of objects
  ret  <- list()
  indname <- 0

  for(obj in 1:nobj){
    indname <- match(objs[obj], dat)
    if(obj != nobj){ # If this is the last object
      inddata <- match(objs[obj + 1], dat)
    }else{
      inddata <- length(dat) + 1 # Next row
    }
    # 'inddiff' is the difference between the end of data
    # and the start of data for this object. If it is zero,
    # throw away the label as there is no data associated with it.
    inddiff <- inddata - indname
    tmp <- NA
    if(inddiff > 1){
      if(inddiff == 2){
        # Create and populate a vector
        vecdat <- dat[(indname + 1) : (inddata - 1)]
        vecdat <- strsplit(vecdat,"[[:blank:]]+")[[1]]
        vecnum <- as.numeric(vecdat)
        ret[[objs[obj]]] <- vecnum
      }else if(inddiff > 2){
        # Create and populate a (possible ragged) matrix
        matdat <- dat[(indname + 1) : (inddata - 1)]
        matdat <- strsplit(c(matdat), "[[:blank:]]+")
        # Now we have a vector of strings, each representing a row
        # of the matrix, and not all may be the same length
        rowlengths <- unlist(lapply(matdat, "length"))
        nrow <- max(rowlengths)
        ncol <- length(rowlengths)
        # Create a new list with elements padded out by NAs
        matdat <- lapply(matdat, function(.ele){c(.ele, rep(NA, nrow))[1:nrow]})
        matnum <- do.call(rbind, matdat)
        mode(matnum) <- "numeric"
        ret[[objs[obj]]] <- matnum
      }
    }else{
      # Throw away this label since it has no associated data.
    }
  }
  return(ret)
}



#' @describeIn iSCAM A function for returning the results of the .dat iscam file
#' @param file File location
#' @param verbose should detailed results be printed to console
#' @export
read.data.file <- function(file = NULL,
                           verbose = FALSE){
  ## Read in the iscam datafile given by 'file'
  ## Parses the file into its constituent parts
  ## And returns a list of the contents

  data <- readLines(file, warn=FALSE)
  tmp <- list()
  ind <- 0

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  # Get the element number for the "Gears" names if present
  dat <- grep("^#.*Gears:.+", data)
  tmp$has.gear.names <- FALSE
  if(length(dat > 0)){
    gear.names.str <- gsub("^#.*Gears:(.+)", "\\1", data[dat])
    gear.names <- strsplit(gear.names.str, ",")[[1]]
    tmp$gear.names <- gsub("^[[:blank:]]+", "", gear.names)
    tmp$has.gear.names <- TRUE
  }


  ## Get the element number for the "CatchUnits" if present
  dat <- grep("^#.*CatchUnits:.+", data)
  if(length(dat > 0)){
    catch.units.str <- gsub("^#.*CatchUnits:(.+)", "\\1", data[dat])
    tmp$catch.units <- gsub("^[[:blank:]]+", "", catch.units.str)
  }

  ## Get the element number for the "IndexUnits" if present
  dat <- grep("^#.*IndexUnits:.+", data)
  if(length(dat > 0)){
    index.units.str <- gsub("^#.*IndexUnits:(.+)", "\\1", data[dat])
    tmp$index.units <- gsub("^[[:blank:]]+", "", index.units.str)
  }

  ## Save the number of specimens per year (comment at end of each age comp
  ##  line), eg. #135 means 135 specimens contributed to the age proportions for
  ##  that year
  age.n <- vector()
  ## Match age comp lines which have N's as comments
  tmp$has.age.comp.n <- FALSE
  pattern <- "^[[:digit:]]{4}[[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]].*#([[:digit:]]+).*"
  dat <- data[grep(pattern, data)]
  if(length(dat) > 0){
    for(n in 1:length(dat)){
      age.n[n] <- sub(pattern, "\\1", dat[n])
    }
  }
  ## age.n is now a vector of values of N for the age comp data.
  ## The individual gears have not yet been parsed out, this will
  ##  happen later when the age comps are read in.

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the DAT file changes format
  tmp$num.areas  <- as.numeric(dat[ind <- ind + 1])
  tmp$num.groups <- as.numeric(dat[ind <- ind + 1])
  tmp$num.sex    <- as.numeric(dat[ind <- ind + 1])
  tmp$start.yr   <- as.numeric(dat[ind <- ind + 1])
  tmp$end.yr     <- as.numeric(dat[ind <- ind + 1])
  tmp$start.age  <- as.numeric(dat[ind <- ind + 1])
  tmp$end.age    <- as.numeric(dat[ind <- ind + 1])
  tmp$num.gears  <- as.numeric(dat[ind <- ind + 1])

  ## Gear allocation
  tmp$gear.alloc  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  if(!tmp$has.gear.names){
    tmp$gear.names <- 1:length(tmp$gear.alloc)
  }

  ## Age-schedule and population parameters
  tmp$linf      <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$k         <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$to        <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lw.alpha  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$lw.beta   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.at.50.mat <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$sd.at.50.mat  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$use.mat   <- as.numeric(dat[ind <- ind + 1])
  tmp$mat.vec   <- as.numeric(strsplit(dat[ind <- ind + 1],", ")[[1]])

  ## Delay-difference options
  tmp$dd.k.age   <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.alpha.g <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.rho.g   <- as.numeric(dat[ind <- ind + 1])
  tmp$dd.wk      <- as.numeric(dat[ind <- ind + 1])

  ## Catch data
  tmp$num.catch.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$catch         <- matrix(NA, nrow = tmp$num.catch.obs, ncol = 7)

  for(row in 1:tmp$num.catch.obs){
    tmp$catch[row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  }
  colnames(tmp$catch) <- c("year", "gear", "area", "group", "sex", "type", "value")
  ## Abundance indices are a ragged object and are stored as a list of matrices
  tmp$num.indices     <- as.numeric(dat[ind <- ind + 1])
  tmp$num.index.obs   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$survey.type <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  ##nrows <- sum(tmp$nitnobs)
  tmp$indices <- list()
  for(index in 1:tmp$num.indices){
    nrows <- tmp$num.index.obs[index]
    ncols <- 8
    tmp$indices[[index]] <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$indices[[index]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$indices[[index]]) <- c("iyr","it","gear","area","group","sex","wt","timing")
  }
  ## Age composition data are a ragged object and are stored as a list of matrices
  tmp$num.age.gears <- as.numeric(dat[ind <- ind + 1])
  ##if(!tmp$hasAgeGearNames){
  ##  tmp$ageGearNames <- 1:length(tmp$nagears)
  ##}

  tmp$num.age.gears.vec       <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.start.age <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.end.age   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$eff                     <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.comp.flag           <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.comps <- NULL
  ## One list element for each gear (tmp$nagears)
  ## Check to see if there are age comp data
  if(tmp$num.age.gears.vec[1] > 0){
    tmp$age.comps <- list()
    for(gear in 1:tmp$num.age.gears){
      nrows <- tmp$num.age.gears.vec[gear]
      ## 5 of the 6 here is for the header columns
      ncols <- tmp$num.age.gears.end.age[gear] - tmp$num.age.gears.start.age[gear] + 6
      tmp$age.comps[[gear]] <- matrix(NA, nrow = nrows, ncol = ncols)
      for(row in 1:nrows){
        tmp$age.comps[[gear]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
      }
      colnames(tmp$age.comps[[gear]]) <- c("year",
                                           "gear",
                                           "area",
                                           "group",
                                           "sex",
                                           tmp$num.age.gears.start.age[gear]:tmp$num.age.gears.end.age[gear])
    }
  }
  ## Build a list of age comp gear N's
  tmp$age.gears.n <- list()
  start <- 1
  for(ng in 1:length(tmp$num.age.gears.vec)){
    end <- start + tmp$num.age.gears.vec[ng] - 1
    tmp$age.gears.n[[ng]] <- age.n[start:end]
    start <- end + 1
  }
  ## Empirical weight-at-age data
  tmp$num.weight.tab <- as.numeric(dat[ind <- ind + 1])
  tmp$num.weight.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$waa <- NULL

  if(tmp$num.weight.obs > 0){
    ## Parse the weight-at-age data
    nrows       <- tmp$num.weight.obs
    ncols       <- tmp$end.age - tmp$start.age + 6
    tmp$weight.at.age <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$weight.at.age[row,] <-
        as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
    }
    colnames(tmp$weight.at.age) <- c("year",
                                     "gear",
                                     "area",
                                     "group",
                                     "sex",
                                     tmp$start.age:tmp$end.age)
  }

  ## Annual Mean Weight data
  ## Catch data
  tmp$num.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.obs <- as.numeric(dat[ind <- ind + 1])
  if(tmp$num.mean.weight.obs >0){
    tmp$mean.weight.data  <- matrix(NA, nrow = sum(tmp$num.mean.weight.obs), ncol = 7)
    for(row in 1:sum(tmp$num.mean.weight.obs)){
      tmp$mean.weight.data[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$mean.weight.data) <- c("year",
                                        "meanwt",
                                        "gear",
                                        "area",
                                        "group",
                                        "sex",
                                        "timing")
  }
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}


#' @describeIn iSCAM A function for returning the results of the iscam control file
#' @param file File location
#' @param num.gears The number of gears
#' @param num.age.gears The number age-gears
#' @param verbose should detailed results be printed to console
#' @export
read.control.file <- function(file = NULL,
                              num.gears = NULL,
                              num.age.gears = NULL,
                              verbose = FALSE){
  ## Read in the iscam control file given by 'file'
  ## Parses the file into its constituent parts and returns a list of the
  ##  contents.
  ## num.gears is the total number of gears in the datafile
  ## num.age.gears in the number of gears with age composition information in the
  ##  datafile

  if(is.null(num.gears)){
    cat("You must supply the total number of gears (num.gears). ",
         "Returning NULL.")
    return(NULL)
  }
  if(is.null(num.age.gears)){
    cat("You must supply the number of gears with age composition ",
         "(num.age.gears). Returning NULL.")
    return(NULL)
  }

  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## Remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Save the parameter names, since they are comments and will be deleted in
  ##  subsequent steps.
  ## To get the npar, remove any comments and preceeding and trailing
  ##  whitespace for it.
  dat1 <- gsub("#.*", "", dat[1])
  dat1 <- gsub("^[[:blank:]]+", "", dat1)
  dat1 <- gsub("[[:blank:]]+$", "", dat1)
  n.par <- as.numeric(dat1)
  param.names <- vector()
  ## Lazy matching with # so that the first instance matches, not any other
  pattern <- "^.*?#([[:alnum:]]+_*[[:alnum:]]*).*"
  for(param.name in 1:n.par){
    ## Each parameter line in dat which starts at index 2,
    ##  retrieve the parameter name for that line
    param.names[param.name] <- sub(pattern, "\\1", dat[param.name + 1])
  }
  ## Now that parameter names are stored, parse the file.
  ##  remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the CTL file and needs to
  ## be updated whenever the CTL file changes format.
  tmp <- list()
  ind <- 0
  tmp$num.params <- as.numeric(dat[ind <- ind + 1])
  tmp$params <- matrix(NA, nrow = tmp$num.params, ncol = 7)
  for(param in 1:tmp$num.params){
    tmp$params[param,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  colnames(tmp$params) <- c("ival","lb","ub","phz","prior","p1","p2")
  ## param.names is retreived at the beginning of this function
  rownames(tmp$params) <- param.names

  ## Age and size composition control parameters and likelihood types
  nrows <- 8
  ncols <- num.age.gears
  tmp$age.size <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$age.size[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$age.size) <- c("gearind",
                              "likelihoodtype",
                              "minprop",
                              "comprenorm",
                              "logagetau2phase",
                              "phi1phase",
                              "phi2phase",
                              "degfreephase")
  ## Ignore the int check value
  ind <- ind + 1

  ## Selectivity parameters for all gears
  nrows <- 10
  ncols <- num.gears
  tmp$sel <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$sel[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$sel) <- c("iseltype",
                         "agelen50log",
                         "std50log",
                         "nagenodes",
                         "nyearnodes",
                         "estphase",
                         "penwt2nddiff",
                         "penwtdome",
                         "penwttvs",
                         "nselblocks")

  ## Start year for time blocks, one for each gear
  max.block <- max(tmp$sel[10,])
  tmp$start.yr.time.block <- matrix(nrow = num.gears, ncol = max.block)
  for(ng in 1:num.gears){
    ## Pad the vector with NA's to make it the right size if it isn't
    ##  maxblocks size.
    tmp.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    if(length(tmp.vec) < max.block){
      for(i in (length(tmp.vec) + 1):max.block){
        tmp.vec[i] <- NA
      }
    }
    tmp$start.yr.time.block[ng,] <- tmp.vec
  }

  ## Priors for survey Q, one column for each survey
  tmp$num.indices <- as.numeric(dat[ind <- ind + 1])
  nrows <- 3
  ncols <- tmp$num.indices
  tmp$surv.q <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$surv.q[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$surv.q) <- c("priortype",
                            "priormeanlog",
                            "priorsd")

  ## Controls for fitting to mean weight data
  tmp$fit.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.cv <- as.numeric(dat[ind <- ind + 1])
  n.vals <- tmp$num.mean.weight.cv
  tmp$weight.sig <-  vector(length = n.vals)
  for(val in 1:n.vals){
    tmp$weight.sig[val] <- as.numeric(dat[ind <- ind + 1])
  }

  ## Miscellaneous controls
  n.rows <- 16
  tmp$misc <- matrix(NA, nrow = n.rows, ncol = 1)
  for(row in 1:n.rows){
    tmp$misc[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$misc) <- c("verbose",
                          "rectype",
                          "sdobscatchfirstphase",
                          "sdobscatchlastphase",
                          "unfishedfirstyear",
                          "maternaleffects",
                          "meanF",
                          "sdmeanFfirstphase",
                          "sdmeanFlastphase",
                          "mdevphase",
                          "sdmdev",
                          "mnumestnodes",
                          "fracZpriorspawn",
                          "agecompliketype",
                          "IFDdist",
                          "fitToMeanWeight")
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}



#' @describeIn iSCAM A function for returning the results of the iscam projection file
#' @param file File location
#' @param verbose should detailed results be printed to console
#' @export
read.projection.file <- function(file = NULL,
                                 verbose = FALSE){
  ## Read in the projection file given by 'file'
  ## Parses the file into its constituent parts
  ##  and returns a list of the contents

  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## remove the lines that start with #.
  dat <- data[-dat]

  ## remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the proj file changes format.
  tmp <- list()
  ind <- 0

  ## Get the TAC values
  tmp$num.tac  <- as.numeric(dat[ind <- ind + 1])
  for(tac in 1:tmp$num.tac){
    ## Read in the tacs, one, per line
    tmp$tac.vec[tac] <- as.numeric(dat[ind <- ind + 1])
  }

  ## If the tac vector is on one line
  ##tmp$tac.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  ## Get the control options vector
  tmp$num.ctl.options <- as.numeric(dat[ind <- ind + 1])
  n.rows <- tmp$num.ctl.options
  n.cols <- 1
  tmp$ctl.options  <- matrix (NA, nrow = n.rows, ncol = n.cols)
  for(row in 1:n.rows){
    tmp$ctl.options[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  or it here.
  option.names <- c("syrmeanm",
                    "nyrmeanm",
                    "syrmeanfecwtageproj",
                    "nyrmeanfecwtageproj",
                    "syrmeanrecproj",
                    "nyrmeanrecproj",
                    "shortcntrlpts",
                    "longcntrlpts",
                    "bmin")
  rownames(tmp$ctl.options) <- option.names[1:tmp$num.ctl.options]
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}


#' @describeIn iSCAM A function for returning the results of the iscam .par file
#' @param file File location
#' @param verbose should detailed results be printed to console
#' @export
read.par.file <- function(file = NULL,
                          verbose = FALSE){
  ## Read in the parameter estimates file given by 'file'
  ## Parses the file into its constituent parts
  ## And returns a list of the contents

  data <- readLines(file, warn = FALSE)
  tmp <- list()
  ind <- 0

  ## Remove preceeding #
  conv.check <- gsub("^#[[:blank:]]*", "", data[1])
  ## Remove all letters, except 'e'
  ##convCheck <- gsub("[[:alpha:]]+","",convCheck)
  convCheck <- gsub("[abcdfghijklmnopqrstuvwxyz]",
                    "",
                    conv.check,
                    ignore.case = TRUE)
  ## Remove the equals signs
  conv.check <- gsub("=", "", conv.check)
  ## Remove all preceeding and trailing whitespace
  conv.check <- gsub("^[[:blank:]]+", "", conv.check)
  conv.check <- gsub("[[:blank:]]+$", "", conv.check)
  ## Remove the non-numeric parts
  conv.check <- strsplit(conv.check, " +")[[1]]
  conv.check <- conv.check[grep("^[[:digit:]]", conv.check)]
  ## The following values are saved for appending to the tmp list later

  num.params   <- conv.check[1]
  obj.fun.val <-  format(conv.check[2], digits = 6, scientific = FALSE)
  max.gradient <-  format(conv.check[3], digits = 8, scientific = FALSE)

  ##Remove the first line from the par data since we already parsed it and saved the values
  data <- data[-1]

  ## At this point, every odd line is a comment and every even line is the value.
  ## Parse the names from the odd lines (oddData) and parse the
  ## values from the even lines (evenData)
  odd.elem <- seq(1, length(data), 2)
  even.elem <- seq(2, length(data), 2)
  odd.data <- data[odd.elem]
  even.data <- data[even.elem]

  ## Remove preceeding and trailing whitespace if it exists from both
  ##  names and values.
  names <- gsub("^[[:blank:]]+", "", odd.data)
  names <- gsub("[[:blank:]]+$", "", names)
  values <- gsub("^[[:blank:]]+", "", even.data)
  values <- gsub("[[:blank:]]+$", "", values)

  ## Remove the preceeding # and whitespace and the trailing : from the names
  pattern <- "^#[[:blank:]]*(.*)[[:blank:]]*:"
  names <- sub(pattern, "\\1", names)

  ## Remove any square brackets from the names
  names <- gsub("\\[|\\]", "", names)

  data.length <- length(names)
  for(item in 1:(data.length)){
    tmp[[item]] <-
      as.numeric(strsplit(values[ind <- ind + 1], "[[:blank:]]+")[[1]])
  }

  names(tmp) <- names
  tmp$num.params <- num.params
  tmp$obj.fun.val <- as.numeric(obj.fun.val)
  #tmp$max.gradient <- as.numeric(max.gradient)
  tmp
}


#' @describeIn iSCAM A function for returning the results of the iscam mcmc files
#' @param model.dir Folder name
#' @param verbose should detailed results be printed to console
#' @export
read.mcmc <- function(model.dir = NULL,
                      verbose = TRUE){
  ## Read in the MCMC results from an iscam model run found in the directory
  ##  model.dir.
  ## Returns a list of the mcmc outputs, or NULL if there was a problem or
  ##  there are no MCMC outputs.
  ## Adding M file 10.14.2021

  mcmc.file <- "iscam_mcmc.csv"
  mcmc.biomass.file <- "iscam_sbt_mcmc.csv"
  mcmc.recr.file <- "iscam_rt_mcmc.csv"
  mcmc.recr.devs.file <- "iscam_rdev_mcmc.csv"
  mcmc.fishing.mort.file <- "iscam_ft_mcmc.csv"
  mcmc.natural.mort.file <- "iscam_m_mcmc.csv"
  mcmc.fishing.mort.u.file <- "iscam_ut_mcmc.csv"
  mcmc.vuln.biomass.file <- "iscam_vbt_mcmc.csv"
  mcmc.proj.file <- "iscammcmc_proj_Gear1.csv"
  mpd.proj.file <- "iscammpd_proj_Gear1.csv"


  if(is.null(model.dir)){
    cat("You must supply a directory name (model.dir). Returning NULL.")
    return(NULL)
  }
  mcmcfn     <- file.path(model.dir, mcmc.file)
  mcmcsbtfn  <- file.path(model.dir, mcmc.biomass.file)
  mcmcrtfn   <- file.path(model.dir, mcmc.recr.file)
  mcmcrdevfn <- file.path(model.dir, mcmc.recr.devs.file)
  mcmcftfn   <- file.path(model.dir, mcmc.fishing.mort.file)
  mcmcmfn    <- file.path(model.dir, mcmc.natural.mort.file)
  mcmcutfn   <- file.path(model.dir, mcmc.fishing.mort.u.file)
  mcmcvbtfn  <- file.path(model.dir, mcmc.vuln.biomass.file)
  mcmcprojfn <- file.path(model.dir, mcmc.proj.file)

  tmp        <- list()
  if(file.exists(mcmcfn)){
    tmp$params <- read.csv(mcmcfn)
  }
  if(file.exists(mcmcsbtfn)){
    sbt        <- read.csv(mcmcsbtfn)
    tmp$sbt    <- extract.group.matrices(sbt, prefix = "sbt")
  }
  if(file.exists(mcmcrtfn)){
    rt         <- read.csv(mcmcrtfn)
    tmp$rt     <- extract.group.matrices(rt, prefix = "rt")
  }
  if(file.exists(mcmcftfn)){
    ft         <- read.csv(mcmcftfn)
    tmp$ft     <- extract.area.sex.matrices(ft, prefix = "ft")
  }
  if(file.exists(mcmcmfn)) {
    tmp$m <- read.csv(mcmcmfn)
  }
  if(file.exists(mcmcutfn)){
    ut         <- read.csv(mcmcutfn)
    tmp$ut     <- extract.area.sex.matrices(ut, prefix = "ut")
  }
  if(file.exists(mcmcrdevfn)){
    rdev       <- read.csv(mcmcrdevfn)
    tmp$rdev   <- extract.group.matrices(rdev, prefix = "rdev")
  }
  if(file.exists(mcmcvbtfn)){
    vbt        <- read.csv(mcmcvbtfn)
    tmp$vbt    <- extract.area.sex.matrices(vbt, prefix = "vbt")
  }
  tmp$proj <- NULL
  if(file.exists(mcmcprojfn)){
    tmp$proj   <- read.csv(mcmcprojfn)
  }
  tmp
}

extract.group.matrices <- function(data = NULL,
                                   prefix = NULL){
  ## Extract the data frame given (data) by unflattening into a list of matrices
  ## by group. The group number is located in the names of the columns of the
  ## data frame in this format: "prefix[groupnum]_year" where [groupnum] is one
  ## or more digits representing the group number and prefix is the string
  ## given as an argument to the function.
  ## Returns a list of matrices, one element per group.

  if(is.null(data) || is.null(prefix)){
    cat("You must give two arguments (data & prefix). Returning NULL.")
    return(NULL)
  }
  tmp <- list()

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique.groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique.groups))
  ## This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique.groups)){
    ## Get all the column names (group.names) for this group by making a specific
    ##  pattern for it
    group.pattern <- paste0(prefix, group, "_[[:digit:]]+")
    group.names   <- names[grep(group.pattern, names)]
    ## Remove the group number in the name, as it is not needed anymore
    pattern      <- paste0(prefix, "[[:digit:]]+_([[:digit:]]+)")
    group.names   <- sub(pattern, "\\1", group.names)

    # Now, the data must be extracted
    # Get the column numbers that this group are included in
    dat <- data[,grep(group.pattern, names)]
    colnames(dat) <- group.names
    tmp[[group]]  <- dat
  }
  tmp
}

extract.area.sex.matrices <- function(data = NULL,
                                      prefix = NULL){
  ## Extract the data frame given (data) by unflattening into a list of matrices
  ##  by area-sex and gear. The area-sex number is located in the names of the
  ##  columns of the data frame in this format:
  ##  "prefix[areasexnum]_gear[gearnum]_year" where [areasexnum] and [gearnum]
  ##  are one or more digits and prefix is the string given as an argument
  ##  to the function.
  ## Returns a list (area-sex) of lists (gears) of matrices, one element
  ##  per group.

  if(is.null(data) || is.null(prefix)){
    cat("You must give two arguments (data & prefix). Returning NULL.")
    return(NULL)
  }

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_gear[[:digit:]]+_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique.groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique.groups))
  ## This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique.groups)){
    ## Get all the column names (group.names) for this group by making a
    ##  specific pattern for it
    group.pattern <- paste0(prefix, group, "_gear[[:digit:]]+_[[:digit:]]+")
    group.names <- names[grep(group.pattern, names)]
    ## Remove the group number in the name, as it is not needed anymore
    pattern <- paste0(prefix, "[[:digit:]]+_gear([[:digit:]]+_[[:digit:]]+)")
    group.names <- sub(pattern, "\\1", group.names)
    ## At this point, group.names' elements look like this: 1_1963
    ## The first value is the gear, and the second, the year.
    ## Get the unique gears for this area-sex group
    pattern <- "([[:digit:]]+)_[[:digit:]]+"
    gears <- sub(pattern, "\\1", group.names)
    unique.gears <- unique(as.numeric(gears))
    tmp2 <- vector("list", length = length(unique.gears))
    for(gear in 1:length(unique.gears)){
      gear.pattern <- paste0(prefix, group,"_gear", gear, "_[[:digit:]]+")
      ## Now, the data must be extracted
      ## Get the column numbers that this group are included in
      dat <- data[,grep(gear.pattern, names)]
      ##colnames(dat) <- groupNames
      tmp2[[gear]] <- dat
    }
    tmp[[group]] <- tmp2
  }
  tmp
}

calc.mcmc <- function(model,
                      burnin = 1000,
                      thin = 1,
                      lower = 0.025,
                      upper = 0.975){
  ## Do the mcmc calculations, e.g. quantiles for sbt, recr, recdevs, F, U, vbt
  ## Returns a list of them all
  ##
  ## mcmc - output of the read.mcmc function
  ## burnin - the number of posteriors to remove from the data
  ## thin - the thinning to apply to the posterior samples
  ## lower - lower quantile for confidence interval calcs
  ## upper - upper quantile for confidence interval calcs

  mcmc.thin <- function(mcmc.dat){
    ## apply burnin and thinning to the data

    nm <- names(mcmc.dat)
    mcmc.obj <- mcmc.dat#apply(mcmc.dat, 2, mcmc)
    mcmc.window <- NULL
    for(col in 1:ncol(mcmc.obj)){
      tmp <- window(mcmc.obj[,col],
                    start = burnin + 1,
                    thin = thin)
      mcmc.window <- cbind(mcmc.window, tmp)
    }
    mcmc.window <- as.data.frame(mcmc.window)
    names(mcmc.window) <- nm
    mcmc.window
  }

  probs <- c(lower, 0.5, upper)

  ## Parameters
  mc <- model$mcmc
  params.dat <- mc$params
  params.dat <- strip.areas.groups(params.dat)
  params.dat <- strip.static.params(model, params.dat)
  nm <- names(params.dat)

  p.dat <- params.dat[ , -which(nm %in% c("msy",
                                          "fmsy",
                                          "bmsy",
                                          "umsy",
                                          "ssb",
                                          "bo"))]
  p.names <- names(p.dat)
  p.dat <- mcmc.thin(p.dat)
  p.quants <- apply(p.dat, 2, quantile, prob = probs)

  ## Reference points
  r.dat <- params.dat[ , which(nm %in% c("msy",
                                         "fmsy",
                                         "bmsy",
                                         "umsy",
                                         "bo"))]
  r.names <- names(r.dat)
  r.dat <- mcmc.thin(r.dat)
  r.quants <- apply(r.dat, 2, quantile, prob = probs)

  ## Spawning biomass
  sbt.dat <- mcmc.thin(mc$sbt[[1]])
  sbt.quants <- apply(sbt.dat,
                      2,
                      quantile,
                      prob = probs)
  ## Depletion
  depl.dat <- apply(sbt.dat,
                    2,
                    function(x){x / r.dat$bo})
  depl.quants <- apply(sbt.dat / r.dat$bo,
                       2,
                       quantile,
                       prob = probs)
  ## Recruitment
  recr.dat <- mcmc.thin(mc$rt[[1]])
  recr.mean <- apply(recr.dat,
                     2,
                     mean)
  recr.quants <- apply(recr.dat,
                       2,
                       quantile,
                       prob = probs)
  ## Recruitment deviations
  recr.devs.dat <- mcmc.thin(mc$rdev[[1]])
  recr.devs.quants <- apply(recr.devs.dat,
                            2,
                            quantile,
                            prob = probs)
  ## Vulnerable biomass by gear (list of data frames)
  vuln.dat <- lapply(mc$vbt[[1]], mcmc.thin)
  vuln.quants <- lapply(vuln.dat,
                        function(x){
                          apply(x,
                                2,
                                quantile,
                                prob = lower,
                                na.rm = TRUE)})
  ## Fishing mortalities by gear (list of data frames)
  f.mort.dat <- lapply(mc$ft[[1]], mcmc.thin)
  f.mort.quants <- lapply(f.mort.dat,
                          function(x){
                            apply(x,
                                  2,
                                  quantile,
                                  prob = lower,
                                  na.rm = TRUE)})

  u.mort.dat <- lapply(mc$ut[[1]], mcmc.thin)
  u.mort.quants <- lapply(u.mort.dat,
                          function(x){
                            apply(x,
                                  2,
                                  quantile,
                                  prob = lower,
                                  na.rm = TRUE)})

  sapply(c("p.dat",
           "p.quants",
           "r.dat",
           "r.quants",
           "sbt.dat",
           "sbt.quants",
           "depl.dat",
           "depl.quants",
           "recr.dat",
           "recr.quants",
           "recr.devs.dat",
           "recr.devs.quants",
           "vuln.dat",
           "vuln.quants",
           "f.mort.dat",
           "f.mort.quants",
           "u.mort.dat",
           "u.mort.quants"),
         function(x){get(x)})
}

strip.areas.groups <- function(dat){
  ## This is a hack function to remove the area and group prefixes for the
  ##  mcmc data 'dat'. The reason is that for now we are just working with a
  ##  single group and area, and the extra text in the parameter names are
  ##  confusing, eg. 'ro_gr1' will become just 'ro'. If you make a model with
  ##  more than one group or area this will need to be revisited. Also, this
  ##  removes 'f' which is assumed to be the objective function value. Note
  ##  that q1, q2, q3... will stay the same and m1 and m2 will remain if the
  ##  model was two-sex.

  pnames <- names(dat)
  ## M will only ever be 1 or 2, for each sex
  pnames <- gsub("m_gs1", "m1", pnames)
  pnames <- gsub("m_gs2", "m2", pnames)

  pnames <- gsub("msy1", "msy", pnames)
  pnames <- gsub("fmsy1", "fmsy", pnames)
  pnames <- gsub("SSB1", "ssb", pnames)
  pnames <- gsub("sel_sd([0-9]+)", "selsd\\1", pnames)
  pnames <- gsub("sel_g([0-9]+)", "sel\\1", pnames)
  ## Remove underscores
  names(dat) <- gsub("_+.*", "", pnames)
  ## Remove objective function value
  dat[,names(dat) != "f"]
}

strip.static.params <- function(model, dat){
  ## Strip out the static (non-estimated) parameters from the mcmc output data
  ##  for the given scenario. We only need to see estimated parameters on the
  ##  diagnostic plots. If there are no static parameters, NULL will be returned

  # Check the control file to see which parameters were static
  inp <- as.data.frame(model$ctl$param)
  static <- inp[inp$phz <= 0,]
  snames <- rownames(static)

  ## Now remove those from the mcmc data
  pnames <- names(dat)
  ## remove the log_ stuff from the input parameter names
  snames <- gsub("log_", "", snames)
  ## There will be either one "m" or "m1" and "m2" in pnames.
  ## If "m" is in snames, remove the "m1", and "m2" from pnames as well if they
  ##  exist
  if("m" %in% snames){
    ms <- c("m1", "m2")
    snames <- c(snames, "m1", "m2")
  }
  ## The following also removes "m" in a combined sex model
  dat <- dat[,!(pnames %in% snames)]

  ## Remove static selectivity params
  sel.params <- as.data.frame(model$ctl$sel)
  est.phase <- sel.params["estphase",]
  static.sel <- est.phase < 1
  sel.post.names <- names(dat)[grep("sel[0-9]+",
                                    names(dat))]
  sel.post.names <- sel.post.names[static.sel]
  sel.sd.post.names <- names(dat)[grep("selsd[0-9]+",
                                       names(dat))]
  sel.sd.post.names <- sel.sd.post.names[static.sel]
  dat.names <- names(dat)
  static.sel.inds <- NULL
  if(length(sel.post.names) > 0){
    ## If there are static parameters, remove them
    for(static.sel in 1:length(sel.post.names)){
      static.sel.inds <- c(static.sel.inds,
                           grep(sel.post.names[static.sel],
                                dat.names))
      static.sel.inds <- c(static.sel.inds,
                           grep(sel.sd.post.names[static.sel],
                                dat.names))
    }
    dat <- dat[,-static.sel.inds]
  }
  dat
}


#' Combines indices into a single index using linear modelling (** Deprecated **)
#'
#' @description iSCAM assessments often make use of multiple indices of abundance.
#' The data object and MPs currently only make use of a single index.
#' combiSCAMinds is a function that creates a single index from many using
#' linear modelling. It is a simple way of providing initial calculations of
#' management recommendations and it should be noted that this process
#' is important and in a real application would require due diligence (ie
#' peer reviewed data workshop).
#' @param idata List: the indices recorded in a read from an iSCAM data folder, e.g. replist$data$indices
#' @param Year Integer vector: the years of the data object ie Data@Year
#' @param fleeteffect Logical: should a fleet effect be added to the linear model?
#' @author T. Carruthers
#' @export iSCAMinds
iSCAMinds<-function(idata,Year,fleeteffect=T){
  
  .Deprecated(msg = "This function is no longer needed as the Data object now supports multiple indices.")
  
  ind<-NULL
  for(i in 1:length(idata)){

    edat<-as.data.frame(idata[[i]])
    index<-edat$it/mean(edat$it)
    ind<-rbind(ind,cbind(edat$iyr,rep(i,nrow(edat)),index))

  }

  ind<-as.data.frame(ind)
  names(ind)<-c("Y","FF","I")
  ind$Y<-as.factor(ind$Y)
  ind$FF<-as.factor(ind$FF)

  if(fleeteffect)lm<-lm(log(I)~Y+FF,dat=ind)
  if(!fleeteffect)lm<-lm(log(I)~Y,dat=ind)
  Years<-Year[Year%in%ind$Y]
  newdat<-as.data.frame(cbind(Years,rep(1,length(Years))))
  names(newdat)<-c("Y","FF")
  newdat$Y<-as.factor(newdat$Y)
  newdat$FF<-as.factor(newdat$FF)
  pred<-predict(lm,newdat)
  ind<-rep(NA,length(Year))
  ind[Year%in%Years]<-exp(pred)/(mean(exp(pred)))
  as.data.frame(cbind(Year,ind))

}

#' Combines all iSCAM age composition data across fleets
#'
#' @description iSCAM assessments are often fitted to numerous fleets that have differing
#' age selectivities. iSCAMcomps is a simple way of providing the aggregate catch at age
#' data. It should be noted that this process is important and in a real application would
#' require due diligence (ie peer reviewed data workshop).
#' @param replist S3 class object: the output from a read from an iSCAM data folder
#' @param Year Integer vector: the years of the data object ie Data@Year
#' @author T. Carruthers
#' @export iSCAMcomps
iSCAMcomps<-function(replist,Year){

  ny <- length(Year)
  na <- replist$dat$end.age
  
  compN <- replist$dat$age.gears.n
  
  CAA_out <- lapply(replist$dat$age.comps, function(x) {
    CAA <- matrix(0, ny, na + 1)
    yind <- match(x[, "year"], Year)
    aind <- match(replist$dat$start.age:na, dimnames(x)[[2]])
    
    CAA[yind, replist$dat$start.age:na + 1] <- x[, aind]
    return(CAA)
  }) %>% simplify2array() %>% apply(1:2, sum, na.rm = TRUE)
  
  array(CAA_out, c(1, ny, na + 1))
}


#' @rdname iSCAM2OM 
#' @export
iSCAM2Data<-function(iSCAMdir,Name="iSCAM assessment",Source="No source provided",
                     length_timestep=1,Author="No author provided"){
  
  if (is.character(iSCAMdir)) {
    message("-- Using function of Chris Grandin (DFO PBS) to extract data from iSCAM file structure --")
    replist <- load.iscam.files(iSCAMdir)
    message("-- End of iSCAM extraction operations --")
  } else {
    replist <- iSCAMdir
  }

  Data <- new("Data")
  Data@Name <- Name
  Data@Year <- replist$mpd$yr
  
  ny <- length(Data@Year)
  final3y <- -2:0 + ny
  
  Catch <- split(as.data.frame(replist$dat$catch), replist$dat$catch[, "gear"]) %>% 
    vapply(function(x) {
      out <- numeric(ny); out[match(x$year, Data@Year)] <- x$value
      return(out)
    }, numeric(ny)) %>% rowSums()
  Data@Cat <- ifelse(Catch < 1e-8, 1e-8, Catch) %>% matrix(1)
  Data@CV_Cat <- matrix(replist$ctl$misc["sdobscatchlastphase", 1], 1, ny)
  
  # AddInd, CV_AddInd, AddIndV, AddIndType, AddIunits
  n_index <- replist$dat$num.indices
  Data@AddInd <- vapply(replist$dat$indices, function(x) {
    out <- rep(NA_real_, ny)
    out[match(x[, "iyr"], Data@Year)] <- x[, "it"]
    return(out)
  }, numeric(ny)) %>% array(c(1, ny, n_index)) %>% aperm(c(1, 3, 2))
  
  Data@CV_AddInd <- vapply(replist$dat$indices, function(x) {
    out <- rep(NA_real_, ny)
    out[match(x[, "iyr"], Data@Year)] <- x[, "wt"]
    return(replist$ctl$weight.sig/out)
  }, numeric(ny)) %>% array(c(1, ny, n_index)) %>% aperm(c(1, 3, 2))
  
  Data@AddIunits <- ifelse(replist$dat$survey.type == 1, 0, 1)
  Data@AddIndType <- ifelse(replist$dat$survey.type == 3, 2, 3)
  
  #Data@AddIndV <- vapply(replist$dat$indices, function(x) {
  #  out <- rep(NA_real_, ny)
  #  out[match(x[, "iyr"], Data@Year)] <- x[, "wt"]
  #  return(replist$ctl$weight.sig/out)
  #}, numeric(ny)) %>% array(c(1, n_index, ny))
  
  Data@t <- length(Data@Year)
  Data@AvC <- mean(Data@Cat)
  
  Data@Dt <- replist$mpd$sbt[ny]/replist$mpd$sbt[1]
  if(!is.null(replist$mpd$M)) {
    Data@Mort <- apply(replist$mpd$M[final3y, ], 1, mean) %>% mean()
  } else {
    Data@Mort <- replist$mpd$M_dd[ny]
  }
  
  UMSY <- replist$mpd$msy/(replist$mpd$msy+replist$mpd$bmsy)
  FMSY <- replist$mpd$fmsy
  BMSY <- replist$mpd$bmsy
  MSY <- replist$mpd$msy
  
  Data@FMSY_M <- FMSY/Data@Mort
  Data@BMSY_B0 <- BMSY/replist$mpd$bo
  Data@Cref <- MSY
  Data@Bref <- BMSY
  
  Data@vbLinf <- replist$dat$linf[1]
  Data@vbK <- replist$dat$k[1]
  Data@vbt0 <- replist$dat$to[1]
  
  if(all(!diff(replist$mpd$ma))) {
    A50 <- replist$dat$age.at.50.mat
    A95 <- -(replist$dat$sd.at.50.mat*log(1/0.95-1)-replist$dat$age.at.50.mat)
  } else {
    A50 <- LinInterp(replist$mpd$ma, replist$dat$start.age:replist$dat$end.age, 0.5)
    A95 <- LinInterp(replist$mpd$ma, replist$dat$start.age:replist$dat$end.age, 0.95)
  }
  Data@L50 <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), A50)
  Data@L95 <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), A95)

  Data@MaxAge <- replist$dat$end.age
  
  if(!is.null(replist$mpd$F)) {
    FF <- replist$mpd$F
    sel <- FF/apply(FF, 1, max)
    selfinal <- apply(sel[final3y, ], 2, mean, na.rm = TRUE)
    if(all(is.na(selfinal))) {
      selfinal <- apply(sel, 2, mean, na.rm = TRUE)
    }
    
    AFC <- LinInterp(selfinal, replist$dat$start.age:replist$dat$end.age, 0.05, ascending = TRUE)
    AFS <- LinInterp(selfinal, replist$dat$start.age:replist$dat$end.age, 0.95, ascending = TRUE)
  } else { # Delay difference
    AFC <- replist$dat$start.age
    AFS <- AFC + 0.1
  }
  Data@LFC <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), AFC)
  Data@LFS <- vB(c(Data@vbLinf, Data@vbK, Data@vbt0), AFS)
  
  if(!is.null(replist$dat$age.comps)) Data@CAA <- iSCAMcomps(replist, Data@Year)
  
  SSB <- replist$mpd$sbt
  #B <- replist$mpd$bt
  SSB0 <- replist$mpd$sbo
  
  depletion <- SSB/SSB0
  
  Data@Dep <- depletion[ny]
  
  Data@SpAbun <- SSB[ny]
  if(!is.null(replist$mpd$F_dd)) {
    Data@Abun <- Data@SpAbun
  }
  
  Data@wla <- replist$dat$lw.alpha
  Data@wlb <- replist$dat$lw.beta
  
  Data@steep <- replist$mpd$steepness
  Data@Ref <- Data@Cat[1, ny]
  Data@Ref_type <- paste(max(Data@Year), "Catch")
  Data@MPrec <- Data@Cat[1, ny]
  Data@MPeff <- 1
  Data@LHYear <- max(Data@Year)
  
  Data

}



