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
#' @param spawn_time_frac Numeric between 0-1 indicating when spawning occurs within the time step
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
#' While the iSCAM start age can be greater than zero, abundance at age is back-calculated to age zero with M, maturity, growth = 0. In this
#' way, the stock-recruit dynamics from iSCAM are preserved.
#' 
#' These arrays are then passed to \link{Assess2OM} to generate the operating model.
#' 
#' @section Reference points:
#' iSCAM calculates the stock-recruit relationship and subsequently a single set of MSY and unfished reference 
#' points using R0, steepness, and unfished spawners per recruit from the mean M, fecundity, and growth (mean with 
#' respect to time).
#' 
#' R0 and h are recalculated for the operating model by obtaining the stock-recruit alpha and beta from the 
#' iSCAM parameters and the mean unfished spawners per recruit in the first \code{ageM} (age of 50% maturity) years.
#' 
#' @author T. Carruthers, Q. Huynh
#' @export
#' @section Superseded:
#' `r lifecycle::badge("superseded")` These functions are superseded as of MSEtool 4.0.0 and will be removed in a future release:
#' - `iSCAM2OM()`
#' - `iSCAM2Data()` (use [ImportiSCAMData()] instead)
iSCAM2OM<-function(iSCAMdir, nsim=48, proyears=50, mcmc=FALSE, spawn_time_frac = 0, Name="iSCAM model",
                   Source="No source provided", length_timestep=1,
                   nyr_par_mu=2, Author="No author provided", report=FALSE, silent=FALSE) {
  .SupersedeLegacy("iSCAM2OM()")
  
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
    SB0 <- mcmc_model$params$sbo[mcmc_samp]
  } else {
    h <- rep(replist$mpd$steepness, nsim)
    Perr <- rep(sqrt((1 - replist$par$theta6)/replist$par$theta7), nsim)
    R0 <- rep(replist$mpd$ro, nsim)
    SB0 <- rep(replist$mpd$sbo, nsim)
  }
  
  phi0 <- SB0/R0
  
  # make the OM
  OM <- Assess2OM(Name=Name,
                  proyears=proyears, interval=2, CurrentYr=replist$dat$end.yr, h=h,
                  Obs = MSEtool::Imprecise_Unbiased, Imp=MSEtool::Perfect_Imp,
                  naa[, , 1:nyears], faa, waa, Mataa, Maa, laa,
                  nyr_par_mu = nyr_par_mu, LowerTri=sage,
                  recind=0, plusgroup=TRUE, altinit=0, fixq1=TRUE,
                  report=report, silent=FALSE, R0 = R0, phi0 = phi0, Perr = Perr, 
                  spawn_time_frac = spawn_time_frac)
  
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
#' @section Superseded:
#' `r lifecycle::badge("superseded")` `iSCAMinds()` is superseded as of MSEtool 4.0.0 and will be removed in a future release.
iSCAMinds<-function(idata,Year,fleeteffect=T){
  .SupersedeLegacy("iSCAMinds()")
  
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
#' @section Superseded:
#' `r lifecycle::badge("superseded")` `iSCAMcomps()` is superseded as of MSEtool 4.0.0 and will be removed in a future release.
iSCAMcomps<-function(replist,Year){
  .SupersedeLegacy("iSCAMcomps()")

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
  .SupersedeLegacy("iSCAM2Data()", "ImportiSCAMData()")
  
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



