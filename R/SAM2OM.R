#' Takes a fitted SAM model and samples historical population and fishing dynamics from the MLE fit and variance-covariance matrix. 
#'
#'
#' @description Takes a fitted SAM model and samples historical population and fishing dynamics from the MLE fit and variance-covariance matrix. 
#' Maturity-at-age-year, Mortality-at-age-year and weight-at-age-year are identical among simulations and are a direct copy of the matrices in the WHAM fitting object. 
#' @param fit an object of class 'sam' created by sam.fit
#' @param nsim Positive integer. The number of simulations. 
#' @param proyears Positive integer. The number of projection years for MSE.
#' @param interval Positive integer. The interval at which management procedures will update the management advice in \link[MSEtool]{runMSE}, e.g., 1 = annual updates.
#' @param Name Character string. The name of the operating model.
#' @param WLa positive real number or array `[sim, ages, year]`. The default weight-length parameter a (W=aL^b)
#' @param WLb positive real number or array `[sim, ages, year]`. The default weight-length parameter b (W=aL^b)
#' @param h positive real number, the steepness of the stock-recruitment relationship
#' @param Obs The observation model (class Obs). This function only updates the catch and index observation error.
#' @param Imp The implementation model (class Imp). This function does not update implementation parameters.
#' @param nyr_par_mu Positive integer. The number of recent years that natural mortality, age vulnerability, weight, length and maturity parameters are averaged over for defining future projection conditions.
#' @param LowerTri Integer. The number of recent years for which model estimates of recruitment are ignored (not reliably estimated by the assessment)
#' @param plusgroup Logical. Does the assessment assume that the oldest age class is a plusgroup?
#' @param altinit Integer. Various assumptions for how to set up the initial numbers. 0: standard, 1: no plus group, 2: temporary fix for MSEtool plus group initialization
#' @param fixq1 Logical. Should q be fixed (ie assume the F-at-age array faa is accurate?
#' @param report Logical, if TRUE, a diagnostic will be reported showing the matching of the OM reconstructed numbers at age vs the assessment.
#' @param stoch Logical, should stochasticity in historical stock and fleet dynamics be simulated?
#' @param silent Whether to silence messages to the console.
#' @param ... Additional arguments
#' @details Use a seed for the random number generator to sample future recruitment.
#' @return An object of class \linkS4class{OM}.
#' @author T. Carruthers
#' @export
#' @seealso \link{Assess2OM}
SAM2OM<-function(fit, nsim=32, proyears=30, interval=1, Name = "SAM Model", WLa=1, WLb=3, h = 0.8, 
                  Obs = MSEtool::Imprecise_Unbiased, Imp=MSEtool::Perfect_Imp,
                  nyr_par_mu = 3, LowerTri=2, plusgroup=T, altinit=0, 
                  fixq1 = T, report = FALSE, stoch = FALSE, silent = FALSE, ...){
  
  # nsim=3; proyears=30; interval=2; Name = "A SAM model"; WLa=1; WLb=3; h=0.8; WAAind = 1;  Obs = MSEtool::Imprecise_Unbiased; Imp=MSEtool::Perfect_Imp; nyr_par_mu = 3; LowerTri=2; recind=1; plusgroup=T; altinit=0; fixq1 = T; report = TRUE; stoch = FALSE; silent = FALSE
 
  if(!requireNamespace("TMB", quietly = TRUE) || !requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("Install the TMB and mvtnorm packages to use SAM2OM")
  }

  if(!requireNamespace("stockassessment", quietly = TRUE)) {
    stop("Install the `stockassessment` package to use SAM2OM. \n`pak::pkg_install('fishfollower/SAM/stockassessment')`")
  }
  
  vanilla_SAM = is.null(fit$input) # is this SAM or some earlier version / modified version (e.g. Chub Mackerel)
  
  if(vanilla_SAM){
    
    dims = dim(faytable(fit))
    ny = dims[1]
    na = dims[2]
    yrs = row.names(faytable(fit))
    CurrentYr = max(as.numeric(yrs))
    waa = array(rep(t(fit$data$stockMeanWeight),each=nsim),c(nsim,na,ny)) # fit$input$dat$waa #weight at age
    Mataa = array(rep(t(fit$data$propMat),each=nsim),c(nsim,na,ny)) # fit$input$dat$maa #maturity at age
    Maa = array(rep(t(fit$data$natMor),each=nsim),c(nsim,na,ny)) # fit$input$dat$M #M at age
    laa = (waa/WLa)^(1/WLb) # length at age isn't used unless catch at length and mean length are simulated
    caa = array(rep(t(caytable(fit)),each=nsim),c(nsim,na,ny)) # fit$caa #estimated catch at age
    
    
    if(!stoch){ # deterministic historical reconstruction
      
      faa = array(rep(t(faytable(fit)),each=nsim),c(nsim,na,ny)) # f at age
      naa = array(rep(t(ntable(fit)),each=nsim),c(nsim,na,ny)) # number at age (million)
        
    }else{ # rerun SAM 
      
      stop("Stochastic samples of numbers and fishing mortality rate at age are coming soon!")
      
      # Code in testing
      param <- as.list(fit$sdrep, what = "Estimate")
      param_sd <- as.list(fit$sdrep, what = "Std. Error")
      mean_logN <- param$logN
      n_age <- nrow(mean_logN)
      
      # diagonal covariance matrix
      cov_logN <- diag(as.numeric(param_sd$logN)^2)
      
      # alternative with a full covariance matrix
      SD <- TMB::sdreport(fit$obj, getJointPrecision = TRUE)
      ind_logN <- SD$jointPrecision@Dimnames[[1]] == "logN"
      cov_logN <- SD$jointPrecision[ind_logN, ind_logN] %>% as.matrix()
     
      log_NAA <- mvtnorm::rmvnorm(nsim, as.numeric(mean_logN), cov_logN) %>%
        array(c(nsim, n_age, nscodData$noYears))
      
      MAA <- nscodData$natMor
      FAA <- array(NA, dim(log_NAA))
      
      for (y in 2:nscodData$noYears - 1) {
        SAA <- log_NAA[, 2:n_age, y+1]-log_NAA[, 2:n_age - 1, y] # survival
        ZAA <- -log(SAA)                                           # Z
        FAA[, 2:n_age - 1, y] <- ZAA - matrix(MAA[y, 2:n_age - 1], nsim, n_age - 1, byrow = TRUE)
      }
      
      naa = exp(log_NAA)
      faa = FAA
      
    }
    
  }else{ # modified  / old SAM
    
    dims<-dim(fit$naa)
    ny<-dims[2]
    na<-dims[1]
    yrs = names(fit$input$dat$waa)
    CurrentYr = max(as.numeric(yrs))
    
    if(!stoch){ # deterministic historical reconstruction
      
      naa = array(rep(fit$naa,each=nsim),c(nsim,na,ny)) # number at age (million)
      faa = array(rep(fit$faa,each=nsim),c(nsim,na,ny)) # fit$faa #F at age
      caa = array(rep(fit$caa,each=nsim),c(nsim,na,ny)) # fit$caa #estimated catch at age
      waa = array(rep(unlist(fit$input$dat$waa),each=nsim),c(nsim,na,ny)) # fit$input$dat$waa #weight at age
      Mataa = array(rep(unlist(fit$input$dat$maa),each=nsim),c(nsim,na,ny)) # fit$input$dat$maa #maturity at age
      Maa = array(rep(unlist(fit$input$dat$M),each=nsim),c(nsim,na,ny)) # fit$input$dat$M #M at age
      laa = (waa/WLa)^(1/WLb) # length at age isn't used unless catch at length and mean length are simulated
    
    }else{ # rerun SAM with ignore.parm.uncertainty = FALSE
      
      stop("Stochastic samples of numbers and fishing mortality rate at age are coming soon!")
      
    } 
    
  }
  
  R0 = apply(naa[,1,]*exp(Maa[,1,]),1,mean)
  
  VPA2OM(Name, proyears, interval, CurrentYr, h=h, Obs, Imp, 
           naa, faa, waa, Mataa, Maa, laa,
           nyr_par_mu, LowerTri,
           recind=1, plusgroup, altinit=0, fixq1=fixq1, 
           report=report, silent=silent, R0 = R0) 
  
}