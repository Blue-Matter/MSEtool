#' Takes a fitted WHAM (Wood's Hole Assessment Model) and samples historical population and fishing dynamics from the MLE fit and variance-covariance matrix. 
#'
#'
#' @description Takes a fitted WHAM (Wood's Hole Assessment Model) and samples historical population and fishing dynamics from the MLE fit and variance-covariance matrix. 
#' Maturity-at-age-year, Mortality-at-age-year and weight-at-age-year are identical among simulations and are a direct copy of the matrices in the WHAM fitting object. 
#' @param obj a list object created by fit_wham(). 
#' @param nsim Positive integer. The number of simulations. 
#' @param proyears Positive integer. The number of projection years for MSE.
#' @param interval Positive integer. The interval at which management procedures will update the management advice in \link[MSEtool]{runMSE}, e.g., 1 = annual updates.
#' @param Name Character string. The name of the operating model.
#' @param h The steepness of the stock-recruitment curve (greater than 0.2 and less than 1, assumed to be close to 1 to match VPA assumption).
#' @param WLa positive real number or array `[sim, ages, year]`. The default weight-length parameter a (W=aL^b)
#' @param WLb positive real number or array `[sim, ages, year]`. The default weight-length parameter b (W=aL^b)
#' @param WAAind positive integer. The index of the WHAM weight-at-age array input$data$waa to be assumed as the weight-at-age for the operating model
#' @param Obs The observation model (class Obs). This function only updates the catch and index observation error.
#' @param Imp The implementation model (class Imp). This function does not update implementation parameters.
#' @param nyr_par_mu Positive integer. The number of recent years that natural mortality, age vulnerability, weight, length and maturity parameters are averaged over for defining future projection conditions.
#' @param LowerTri Integer. The number of recent years for which model estimates of recruitment are ignored (not reliably estimated by the assessment)
#' @param recind Positive integer. The first age class that fish 'recruit to the fishery'. The default is 0 - ie the first position in the age dimension of naa is age zero
#' @param plusgroup Logical. Does the assessment assume that the oldest age class is a plusgroup?
#' @param altinit Integer. Various assumptions for how to set up the initial numbers. 0: standard, 1: no plus group, 2: temporary fix for MSEtool plus group initialization
#' @param fixq1 Logical. Should q be fixed (ie assume the F-at-age array faa is accurate?
#' @param report Logical, if TRUE, a diagnostic will be reported showing the matching of the OM reconstructed numbers at age vs the assessment.
#' @param silent Whether to silence messages to the console.
#' @param ... Additional arguments, including R0 (unfished recruitment), phi0 (unfished spawners per recruit associated with R0 and h for calculating stock recruit parameters),
#' Perr (recruitment standard deviation for sampling future recruitment), and AC (autocorrelation in future recruitment deviates). For all, either a numeric or a length nsim vector.
#' @details Use a seed for the random number generator to sample future recruitment.
#' @return An object of class \linkS4class{OM}.
#' @author T. Carruthers
#' @export
#' @seealso \link{Assess2OM}

WHAM2OM<-function(obj, nsim=3, proyears=30, interval=2, Name = NULL, WLa=1, WLb=3,
                  WAAind = 1,
                  Obs = MSEtool::Imprecise_Unbiased, Imp=MSEtool::Perfect_Imp,
                  nyr_par_mu = 3, LowerTri=2, recind=0, plusgroup=T, altinit=0, 
                  fixq1 = T, report = FALSE, silent = FALSE, ...){

  SD <- TMB::sdreport(obj, getJointPrecision = TRUE)
  mu <- c(SD$par.fixed, SD$par.random)
  covm <- solve(SD$jointPrecision)
  
  samps <- mvtnorm::rmvnorm(nsim, mu, covm)
  
  report_internal_fn <- function(x, samps, obj) {
    obj$report(samps[x, ])
  }
  
  output <- lapply(1:nsim, report_internal_fn, samps = samps, obj = obj) # List of model output for each simulation

  dims<-dim(output[[1]]$NAA)
  ny<-dims[1]
  na<-dims[2]
  
  naa<-aperm(array(unlist(lapply(output,FUN=function(x)x$NAA)),c(ny,na,nsim)),c(3,2,1))   
  faa<-aperm(array(unlist(lapply(output,FUN=function(x)x$FAA)),c(ny,na,nsim)),c(3,2,1))  
  Maa<-aperm(array(unlist(lapply(output,FUN=function(x)x$MAA)),c(ny,na,nsim)),c(3,2,1)) 
  Mataa<-aperm(array(obj$input$data$mature,c(ny,na,nsim)),c(3,2,1))
  waa<-aperm(array(obj$input$data$waa[WAAindCa,,],c(ny,na,nsim)),c(3,2,1))
  laa<-(waa/WLa)^(1/WLb)
  
  if(obj$input$data$recruit_model%in%c(1,2)){
    h=0.999
  }else{
    stop("OM NOT MADE: Currently coded for recruit_model types 1 and 2 that either model recruitment as a fixed effect with no SR relationship or model deviations from mean recruitment")
   # if(obj$input$data$use_steepness){
      #h=obj$input$data$recruit_pars[1]
      #R0=obj$input$data$recruit_pars[2]
    #}else{
    #  h=NULL # reconfigure
    #  R0=NULL # reconfigure
    #}
    
    #if(obj$input$data$recruit_model == 3){ # Beverton-Holt
      
    #}else{ # Ricker
      
    #} 
    
  }
           
  CurrentYr<-obj$years[length(obj$years)]   
  if(is.null(Name))Name=obj$model_name
  
  VPA2OM(Name, proyears, interval, CurrentYr, h=h, Obs, Imp, 
           naa, faa, waa, Mataa, Maa, laa,
           nyr_par_mu, LowerTri,
           recind=0, plusgroup, altinit=0, fixq1=TRUE,
           report=report, silent=silent, ...) 
  
}