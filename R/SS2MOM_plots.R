compareNmulti <- function(replist, multiHist) {
  year_lab <- "Year"
  mainyrs <- replist$startyr:replist$endyr
  
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  if(n.yrs == length(mainyrs)) {
    OM_years <- replist$startyr:replist$endyr
  } else {
    nseas <- 1/replist$seasdurations
    year_frac <- data.frame(mainyrs = mainyrs, seas = rep(1:nseas, length(mainyrs)/nseas), 
                            true_year = rep(1:(length(mainyrs)/nseas), each = nseas))
    age_frac <- data.frame(age = 0:OM@maxage) %>% mutate(true_age = floor(age/nseas))
    OM_years <- dplyr::filter(year_frac, seas == 1) %>% getElement("mainyrs")
  }
  N_SS <- replist$natage  %>% filter(Yr %in% OM_years, `Beg/Mid`=="B")
  cols <- which(colnames(N_SS)=='0'):ncol(replist$natage)
  N_SS <- N_SS %>% 
    tidyr::pivot_longer(cols=all_of(cols)) %>%
    group_by(Yr, Sex) %>%
    summarize(N=sum(value), .groups = 'keep')
  
  np <- length(multiHist)
  N_OMlist <- list()
  for (p in 1:np) {
    N_OMlist[[p]] <- data.frame(Sim=1:nsim, 
                                Yr=rep(mainyrs, each=nsim), 
                                Sex=rep(p, nsim*n.yrs),
                                N=c(as.vector(apply(multiHist[[p]][[1]]@TSdata$Number, 1:2, sum))))
  }
  N_OM <- do.call('rbind', N_OMlist)
  N_OM$Model <- 'MOM'
  N_SS$Model <- 'SS'
  N_dat <- bind_rows(N_OM, N_SS)
  
  ggplot(N_dat, aes(x=Yr, y=N, color=Model, linetype=Model)) +
    facet_wrap(~Sex) +
    geom_line() +
    theme_bw() + 
    labs(x="Year", y="Total number")
  
  
}