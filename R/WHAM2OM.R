#' Takes a fitted SAM model and samples historical population and fishing dynamics from the MLE fit and variance-covariance matrix. 
#'
#'
#' @description Takes a fitted SAM model and samples historical population and fishing dynamics from the MLE fit and variance-covariance matrix. 
#' Maturity-at-age-year, Mortality-at-age-year and weight-at-age-year are identical among simulations and are a direct copy of the matrices in the WHAM fitting object. 
#' @param obj a SAM output object
#' @param nsim Positive integer. The number of simulations. 
#' @param proyears Positive integer. The number of projection years for MSE.
#' @param interval Positive integer. The interval at which management procedures will update the management advice in \link[MSEtool]{runMSE}, e.g., 1 = annual updates.
#' @param Name Character string. The name of the operating model.
#' @param WLa positive real number or array `[sim, ages, year]`. The default weight-length parameter a (W=aL^b)
#' @param WLb positive real number or array `[sim, ages, year]`. The default weight-length parameter b (W=aL^b)
#' @param WAAind positive integer. The index of the WHAM weight-at-age array input$data$waa to be assumed as the weight-at-age for the operating model
#' @param Obs The observation model (class Obs). This function only updates the catch and index observation error.
#' @param Imp The implementation model (class Imp). This function does not update implementation parameters.
#' @param nyr_par_mu Positive integer. The number of recent years that natural mortality, age vulnerability, weight, length and maturity parameters are averaged over for defining future projection conditions.
#' @param LowerTri Integer. The number of recent years for which model estimates of recruitment are ignored (not reliably estimated by the assessment)
#' @param plusgroup Logical. Does the assessment assume that the oldest age class is a plusgroup?
#' @param altinit Integer. Various assumptions for how to set up the initial numbers. 0: standard, 1: no plus group, 2: temporary fix for MSEtool plus group initialization
#' @param fixq1 Logical. Should q be fixed (ie assume the F-at-age array faa is accurate?
#' @param report Logical, if TRUE, a diagnostic will be reported showing the matching of the OM reconstructed numbers at age vs the assessment.
#' @param silent Whether to silence messages to the console.
#' @param ... Additional arguments, including R0 (unfished recruitment), phi0 (unfished spawners per recruit associated with R0 and h for calculating stock recruit parameters),
#' @details Use a seed for the random number generator to sample future recruitment.
#' @return An object of class \linkS4class{OM}.
#' @author T. Carruthers
#' @export
#' @seealso \link{Assess2OM}
WHAM2OM<-function(obj, nsim=3, proyears=30, interval=2, Name = NULL, WLa=1, WLb=3,
                  WAAind = 1,
                  Obs = MSEtool::Imprecise_Unbiased, Imp=MSEtool::Perfect_Imp,
                  nyr_par_mu = 3, LowerTri=2, plusgroup=T, altinit=0, 
                  fixq1 = T, report = FALSE, silent = FALSE, ...){
  
  # nsim=3; proyears=30; interval=2; Name = NULL; WLa=1; WLb=3; WAAind = 1;  Obs = MSEtool::Imprecise_Unbiased; Imp=MSEtool::Perfect_Imp; nyr_par_mu = 3; LowerTri=2; recind=1; plusgroup=T; altinit=0; fixq1 = T; report = TRUE; silent = FALSE
  if(!requireNamespace("TMB", quietly = TRUE) || !requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("Install the TMB and mvtnorm packages to use SAM2OM")
  }

  # Do a TMB sd report and get means / invert Hessian
  SD <- TMB::sdreport(obj, getJointPrecision = TRUE)
  mu <- c(SD$par.fixed, SD$par.random)
  covm <- solve(SD$jointPrecision)
  
  # clumsy and ugly reordering of mu to match covnams (sorry, more coffee needed!)
  numu<-rep(NA,length(mu))
  munams<-names(mu)
  nams<-unique(munams)
  ni<-length(nams)
  covnams<-rownames(covm)
  ind<-1:length(mu)
  
  # reorder rows
  for(i in 1:ni){
    indto<-ind[covnams == nams[i]]
    indfrom<-ind[munams == nams[i]]
    numu[indto]<-mu[indfrom]
  }
  names(numu)<-covnams
  
  # multivariate normal sampling of pars
  samps <- mvtnorm::rmvnorm(nsim, numu, covm)
  
  report_internal_fn <- function(x, samps, obj) {
    obj$report(samps[x, ])
  }
  
  output <- lapply(1:nsim, report_internal_fn, samps = samps, obj = obj) # List of model output for each simulation

  yind<-obj$years_full %in% obj$years  
  
  dims<-dim(output[[1]]$NAA[yind,])
  ny<-dims[1]
  na<-dims[2]
  
  naa<-aperm(array(unlist(lapply(output,FUN=function(x)x$NAA[yind,])),c(ny,na,nsim)),c(3,2,1))   
  faa<-aperm(array(unlist(lapply(output,FUN=function(x)x$FAA[yind,1,])),c(ny,na,nsim)),c(3,2,1))  
  Maa<-aperm(array(unlist(lapply(output,FUN=function(x)x$MAA[yind,])),c(ny,na,nsim)),c(3,2,1)) 
  
  
  Mataa<-aperm(array(obj$input$data$mature[yind,],c(ny,na,nsim)),c(3,2,1))
  waa<-aperm(array(obj$input$data$waa[WAAind,yind,],c(ny,na,nsim)),c(3,2,1))
  laa<-(waa/WLa)^(1/WLb)
  
  if(obj$input$data$recruit_model%in%c(1,2)){
    h=0.999
  }else{
    stop("OM NOT MADE: Currently coded for recruit_model types 1 and 2 that either model recruitment as a fixed effect with no SR relationship or model deviations from mean recruitment")
    
    # if(obj$input$data$use_steepness){
    #  h=obj$input$data$recruit_pars[1]
      #R0=obj$input$data$recruit_pars[2]
      
    #}else{
      
     # phi0<-apply(exp(t(-apply(Maa[,,ny],1,cumsum)))*Mataa[,,ny]*waa[,,ny],1,sum) # unfished spawning biomass per recruit in final year
   
      #if(obj$input$data$recruit_model == 3){ # Beverton-Holt
        
        
      #}else{ # Ricker
        
      #}
    #}
    
  }
  
  CurrentYr<-obj$years[length(obj$years)]   
  if(is.null(Name)) Name=obj$model_name
  
  OM<-VPA2OM(Name, proyears, interval, CurrentYr, h=h, Obs, Imp, 
           naa, faa, waa, Mataa, Maa, laa,
           nyr_par_mu, LowerTri,
           recind=1, plusgroup, altinit=0, fixq1=fixq1,# recind = 1 because WHAM models report at age arrays starting second year of life ie not age zero
           report=report, silent=silent, R0 = apply(naa[,1,]*exp(Maa[,1,]),1,mean)) 
  
  
  # Sample some selectivities potentially for use later and put these in a WHAM Misc slot
  SAM = list()
  nsel<-length(output[[1]]$selAA)
  selfunc<-function(x)    sapply(x$selAA, function(y)c(0,y[nrow(y),])/max(y[nrow(y),])) # takes most recent selectivity at age for each block
  SAM$AddIndV<- aperm(array(unlist(lapply(output,FUN=selfunc)),c(na+1,nsel,nsim)),c(3,2,1))
  OM@Misc$WHAM <- SAM
  
  OM
}