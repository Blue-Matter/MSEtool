
# #' Predict recruitment and return fit to S-R observations
# #'
# #' @description Internal function to \link{optSR}
# #' @param pars an initial guess at model parameters steepness and R0
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param mode should fit (= 1) or recruitment deviations (not 1) be returned
# #' @param plot should a plot of the model fit be produced?#'
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @author T. Carruthers
# #' @export
getSR <- function(pars, SSB, rec, SSBpR, mode = 1, plot = FALSE, type = c("BH", "Ricker")){
  R0 <- exp(pars[2])
  if(type == "BH") {
    h <- 0.2 + 0.8 * ilogit(pars[1])
    recpred<-((0.8*R0*h*SSB)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB))
  }
  if(type == "Ricker") {
    h <- 0.2 + exp(pars[1])
    recpred <- SSB * (1/SSBpR) * (5*h)^(1.25*(1 - SSB/(R0*SSBpR)))
  }

  if(plot){
    ord <- order(SSB)
    plot(SSB[ord], rec[ord], ylim=c(0, max(rec, R0)), xlim=c(0, max(SSB, R0*SSBpR)), xlab="", ylab="")
    SSB2 <- seq(0, R0*SSBpR, length.out=500)
    if(type == "BH") recpred2 <- ((0.8*R0*h*SSB2)/(0.2*SSBpR*R0*(1-h)+(h-0.2)*SSB2))
    if(type == "Ricker") recpred2 <- SSB2 * (1/SSBpR) * (5*h)^(1.25*(1 - SSB2/(R0*SSBpR)))
    lines(SSB2, recpred2, col='blue')
    abline(v=c(0.2*R0*SSBpR, R0*SSBpR), lty=2, col='red')
    abline(h=c(R0, R0*h), lty=2, col='red')
    legend('topright', legend=c(paste0("h = ", round(h,3)), paste0("ln(R0) = ", round(log(R0),3))), bty='n')
  }

  if(mode==1){
    #return(sum(((recpred-rec)/10000)^2))
    sigmaR <- sqrt(sum((log(rec/recpred))^2)/length(recpred))
    return(-sum(dnorm(log(rec)-log(recpred),0,sigmaR,log=T)))
    #-dnorm(pars[1],0,6,log=T)) # add a vague prior on h = 0.6
    #return(-sum(dnorm(recpred,rec,rec*0.5,log=T)))
  }else{
    return(rec-recpred)
  }
}

# #' Wrapper for estimating stock recruitment parameters from resampled stock-recruitment data
# #'
# #' @param x position to accommodate lapply-type functions
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param pars an initial guess at model parameters steepness and R0
# #' @param frac the fraction of observations for resampling
# #' @param plot should a plot of model fit be produced?
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @return Estimated value of steepness.
# #' @author T. Carruthers
# #' @export
optSR<-function(x, SSB, rec, SSBpR, pars, frac = 0.5, plot = FALSE, type = c("BH", "Ricker")) {
  type <- match.arg(type)
  samp <- sample(1:length(SSB), size = ceiling(length(SSB) * frac), replace = FALSE)
  opt <- optim(pars, getSR, method = "BFGS", #lower = c(-6, pars[2]/50), upper = c(6, pars[2] * 50),
               SSB = SSB[samp], rec = rec[samp], SSBpR = SSBpR, mode = 1, plot = FALSE, type = type)
  if(plot) getSR(opt$par, SSB, rec, SSBpR, mode = 2, plot = plot, type = type)
  if(type == "BH") h <- 0.2 + 0.8 * ilogit(opt$par[1])
  if(type == "Ricker") h <- 0.2 + exp(opt$par[1])
  return(h)
}

# #' Function that returns a stochastic estimate of steepness given observed stock recruitment data
# #'
# #' @param nsim number of samples of steepness to generate
# #' @param SSB 'observations' of spawning biomass
# #' @param rec 'observations' (model predictions) of recruitment
# #' @param SSBpR spawning stock biomass per recruit at unfished conditions
# #' @param plot should plots of model fit be produced?
# #' @param type what type of stock recruitment curve is being fitted ("BH" = Beverton-Holt or "Ricker")
# #' @return Vector of length nsim with steepness values.
# #' @author T. Carruthers
# #' @export
SRopt <- function(nsim, SSB, rec, SSBpR, plot = FALSE, type = c("BH", "Ricker")) {
  type <- match.arg(type)
  R0temp <- rec[1] # have a guess at R0 for initializing nlm
  pars <- c(0, log(R0temp))
  #SSBpR=SSB[1]/rec[1]
  vapply(1:nsim, optSR, numeric(1), SSB = SSB, rec = rec, SSBpR = SSBpR, pars = pars, frac = 0.8,
         plot = plot, type = type)
}


