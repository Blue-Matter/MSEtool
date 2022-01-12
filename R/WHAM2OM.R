

WHAM2OM<-function(obj,ns=3,WLa=1,WLb=3){

  SD <- TMB::sdreport(obj, getJointPrecision = TRUE)
  mu <- c(SD$par.fixed, SD$par.random)
  covm <- solve(SD$jointPrecision)
  
  samps <- mvtnorm::rmvnorm(nsim, mu, covm)
  
  report_internal_fn <- function(x, samps, obj) {
    obj$report(samps[x, ])
  }
  
  output <- lapply(1:ns, report_internal_fn, samps = samps, obj = obj) # List of model output for each simulation

  dims<-dim(output[[1]]$NAA)
  ny<-dims[1]
  na<-dims[2]
  
  naa<-array(unlist(lapply(output,FUN=function(x)x$NAA)),c(ny,na,ns))   
  faa<-array(unlist(lapply(output,FUN=function(x)x$FAA)),c(ny,na,ns))  
  Maa<-array(unlist(lapply(output,FUN=function(x)x$MAA)),c(ny,na,ns)) 
  Mataa<-array(obj$input$data$mature,c(ny,na,ns))
  waa<-array(obj$input$data$waa,c(ny,na,ns))
  laa<-(waa/WLa)^(1/WLb)
  
           
  CurrentYr<-obj$years[length(obj$years)]         
  
}