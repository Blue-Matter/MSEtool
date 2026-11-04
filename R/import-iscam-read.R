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
  p.quants <- apply(p.dat, 2, quantile, probs = probs)

  ## Reference points
  r.dat <- params.dat[ , which(nm %in% c("msy",
                                         "fmsy",
                                         "bmsy",
                                         "umsy",
                                         "bo"))]
  r.names <- names(r.dat)
  r.dat <- mcmc.thin(r.dat)
  r.quants <- apply(r.dat, 2, quantile, probs = probs)

  ## Spawning biomass
  sbt.dat <- mcmc.thin(mc$sbt[[1]])
  sbt.quants <- apply(sbt.dat,
                      2,
                      quantile,
                      probs = probs)
  ## Depletion
  depl.dat <- apply(sbt.dat,
                    2,
                    function(x){x / r.dat$bo})
  depl.quants <- apply(sbt.dat / r.dat$bo,
                       2,
                       quantile,
                       probs = probs)
  ## Recruitment
  recr.dat <- mcmc.thin(mc$rt[[1]])
  recr.mean <- apply(recr.dat,
                     2,
                     mean)
  recr.quants <- apply(recr.dat,
                       2,
                       quantile,
                       probs = probs)
  ## Recruitment deviations
  recr.devs.dat <- mcmc.thin(mc$rdev[[1]])
  recr.devs.quants <- apply(recr.devs.dat,
                            2,
                            quantile,
                            probs = probs)
  ## Vulnerable biomass by gear (list of data frames)
  vuln.dat <- lapply(mc$vbt[[1]], mcmc.thin)
  vuln.quants <- lapply(vuln.dat,
                        function(x){
                          apply(x,
                                2,
                                quantile,
                                probs = lower,
                                na.rm = TRUE)})
  ## Fishing mortalities by gear (list of data frames)
  f.mort.dat <- lapply(mc$ft[[1]], mcmc.thin)
  f.mort.quants <- lapply(f.mort.dat,
                          function(x){
                            apply(x,
                                  2,
                                  quantile,
                                  probs = lower,
                                  na.rm = TRUE)})

  u.mort.dat <- lapply(mc$ut[[1]], mcmc.thin)
  u.mort.quants <- lapply(u.mort.dat,
                          function(x){
                            apply(x,
                                  2,
                                  quantile,
                                  probs = lower,
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
