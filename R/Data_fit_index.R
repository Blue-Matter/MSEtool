

#' Fit an observed index to simulated biomass and generate residuals
#'
#' @param sim.index A numeric vector with simulated biomass by year (all historical years) 
#' @param obs.ind A numeric vector with the observed index by year (all historical years, NAs for missing values)
#' @param beta Optional beta value for hyper-stability/depletion. Otherwise it is estimated
#' @param proyears Number of projection years
#'
#' @return
#' @export
#'
#' @examples
Index_Fit <- function(sim.index, obs.ind, beta=NA, proyears=50) {
  
  if (any(obs.ind<0, na.rm=TRUE)) 
    stop('Observed index cannot have negative values', call.=FALSE)
  
  # standardize index and biomass to mean 1 
  s.sim.index <- sim.index/mean(sim.index, na.rm=TRUE)
  s.obs.ind <- obs.ind/mean(obs.ind, na.rm=TRUE)
  
  # convert to log space
  l.sim.index <- log(s.sim.index)
  l.obs.index <- log(s.obs.ind)
  
  # mean 0 
  l.sim.index <- l.sim.index-mean(l.sim.index, na.rm = TRUE)
  l.obs.index <- l.obs.index-mean(l.obs.index, na.rm = TRUE)
  
  
  # estimate beta (if not provided)
  if (is.na(beta)) {
    opt<-optimize(getbeta,x=exp(l.sim.index),y=exp(l.obs.index),
                  interval=c(0.1,10))
    beta <- opt$minimum
  } 
  
  # adjust true biomass for beta 
  l.sim.index <- log((exp(l.sim.index)^beta))
  
  # calculate residuals (log-space) 
  res <- l.sim.index - l.obs.index
  
  non.nas <- which(!is.na(res))
  res.groups <- split(non.nas, cumsum(c(1, diff(non.nas) != 1)))
  
  # calculate auto-correlation for each group of contiguous residuals
  group.length <- res.groups %>% lapply(length) %>% unlist() %>% as.numeric()
  group.ind <- which(group.length>1) 
  ac.group <- vector('numeric', length=length(group.ind))
  ac.group.n <- ac.group
  
  cnt <- 0
  for (n in group.ind) {
    cnt <- cnt+1
    ac.group[cnt] <- acf(res[res.groups[[n]]], plot=F)$acf[2,1,1]
    ac.group.n[cnt] <- group.length[n]
  }
  
  ac <- weighted.mean(ac.group, ac.group.n)
  ac[ac<0] <- 0 # https://github.com/Blue-Matter/MSEtool/issues/65
  
  # standard deviation & correlation
  sd <- sd(res, na.rm=TRUE)
  
  # generate residuals for future projections space 
  mu <- -0.5 * (sd)^2 * (1 - ac)/sqrt(1 - ac^2)
  Res <- rnorm(proyears, mu, sd)
  
  # apply a pseudo AR1 autocorrelation
  non.na.res <- res[!is.na(res)]
  lst.err <- non.na.res[length(non.na.res)] # most recent obs error
  Res <- applyAC(res=Res, ac=ac, max.years=proyears, 
                 lst.err=lst.err) # log-space
  
  out <- list()
  out$Res.Hist <- exp(res) # residuals from historical years
  out$Res.Project <- exp(Res) # residuals for projection years
  out$ac <- ac # log residuals
  out$sd <- sd # log residuals
  out$beta <- beta
  out$obs.index <- obs.ind
  out$sim.index <- sim.index
  out
}
