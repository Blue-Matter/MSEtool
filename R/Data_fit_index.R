
Fit_Index <- function(ind_slot='Ind', indcv_slot="CV_Ind", Data_out,
                      RealData, StockPars, ObsPars, SampCpars, nsim, nyears, proyears, msg=TRUE) {
  
  ind_type <- switch(ind_slot,
                     Ind = 'Total Index',
                     SpInd = 'Spawning Index',
                     VInd = 'Vulnerable Index')
  text <- paste0("`OM@cpars$Data@", ind_slot, '`')
  
  slot(Data_out, ind_slot) <- matrix(slot(RealData, ind_slot)[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)
  slot(Data_out, indcv_slot) <- matrix(slot(RealData, indcv_slot)[1,1:nyears], nrow=nsim, ncol=nyears, byrow=TRUE)
  
  if (msg)
    message('Updating Simulated' ,ind_type, 'from', text)
  
  obs_err_var <- switch(ind_slot,
                        Ind = 'Ierr_y',
                        SpInd = 'SpIerr_y',
                        VInd = 'VIerr_y')
  
  if (!is.null(SampCpars[[obs_err_var]])) {
    if (msg) message(paste0(ind_type, ' Observation Error found (cpars$', obs_err_var, ').'), 
                     'Not updating observation error.')
    return(list(Data_out=Data_out, ObsPars=ObsPars))
  } 
  # calculate observation error 
  fitbeta <- TRUE
  
  beta_var <- switch(ind_slot,
                     Ind = 'I_beta',
                     SpInd = 'SpI_beta',
                     VInd = 'VI_beta')
  
  if (!is.null(SampCpars[[beta_var]])) {
    if (msg) message(paste0(ind_type, ' beta found (cpars$', beta_var, ').'),
                     'Not updating observation beta parameter')
    ObsPars[[beta_var]] <- SampCpars[[beta_var]]
    fitbeta <- FALSE
  } else {
    ObsPars[[beta_var]] <- rep(NA, nsim)
  }
  
  biomass_var <- switch(ind_slot,
                        Ind = 'Biomass',
                        SpInd = 'SSB',
                        VInd = 'VBiomass')
  
  # Calculate Error
  SimBiomass <- apply(StockPars[[biomass_var]], c(1, 3), sum)
  
  
  # Fit to observed index and generate residuals for projections
  # Calculate residuals (with or without estimated beta)
  Res_List <- lapply(1:nsim, function(x) Calc_Residuals(sim.index=SimBiomass[x,], 
                                                        obs.ind=slot(Data_out, ind_slot)[x,],
                                                        beta=ObsPars[[beta_var]][x]))
  
  lResids_Hist <- do.call('rbind', lapply(Res_List, '[[', 1))
  if (fitbeta)
    ObsPars[[beta_var]] <- as.vector(do.call('cbind', lapply(Res_List, '[[', 2)))
  
  if (msg & fitbeta)
    message(paste0('Updating Obs@', beta_var), 'from real index. Range:',
            paste0(range(round(ObsPars[[beta_var]],2)), collapse = "-"),
            paste0("Use `cpars$", beta_var, "` to override"))
  
  # Calculate statistics
  Stats_List <- lapply(1:nsim, function(x) Calc_Stats(lResids_Hist[x,]))
  Stats <- do.call('rbind', Stats_List)
  
  # Generate residuals for projections
  Resid_Hist <- exp(lResids_Hist) # historical residuals in normal space
  Resid_Proj <- Gen_Residuals(Stats, nsim, proyears)
  
  ObsPars[[obs_err_var]][, 1:nyears] <- Resid_Hist # update Obs Error
  ObsPars[[obs_err_var]][, (nyears+1):(nyears+proyears)] <- Resid_Proj
  
  stat_var <- switch(ind_slot,
                     Ind = 'Ind_Stat',
                     SpInd = 'SpInd_Stat',
                     VInd = 'VInd_Stat')
  
  
  ObsPars[[stat_var]] <- Stats[,1:2]
  list(Data_out=Data_out, ObsPars=ObsPars)
  
  
}


Calc_Residuals <- function(sim.index, obs.ind, beta=NA) {
  
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
  res <- l.obs.index - l.sim.index
  
  out <- list()
  out$res <- res 
  out$beta <- beta
  out
}

Calc_Stats <- function(res) {
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
  
  # return statistics 
  non.na.res <- res[!is.na(res)]
  lst.err <- non.na.res[length(non.na.res)] # most recent obs error 
  
  data.frame(AC=ac, SD=sd, lst.err=lst.err) # log-space residuals
}

Gen_Residuals <- function(df, nsim, proyears) {
  sd <- df$SD
  ac <- df$AC
  ac[!is.finite(ac)] <- 0
  lst.err <- df$lst.err
  if (all(is.na(sd))) return(rep(NA, nsim))
  mu <- -0.5 * (sd)^2 * (1 - ac)/sqrt(1 - ac^2)
  Res <- matrix(rnorm(proyears*nsim, mu, sd), nrow=proyears, ncol=nsim, byrow=TRUE)
  # apply a pseudo AR1 autocorrelation
  Res <- sapply(1:nsim, applyAC, res=Res, ac=ac, max.years=proyears, lst.err=lst.err) # log-space
  exp(t(Res))
}

applyAC <- function(x, res, ac, max.years, lst.err) {
  for (y in 1:max.years) {
    if (y == 1) {
      res[y,x] <- ac[x] * lst.err[x] + res[y,x] * (1-ac[x] * ac[x])^0.5
    } else {
      res[y,x] <- ac[x] * res[y-1,x] + res[y,x] * (1-ac[x] * ac[x])^0.5
    }
  }
  res[,x]
}

