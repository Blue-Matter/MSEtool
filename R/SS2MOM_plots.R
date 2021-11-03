compareNmulti <- function(replist, multiHist) {
  year_lab <- "Year"
  mainyrs <- replist$startyr:replist$endyr
  maxage <- multiHist[[1]][[1]]@SampPars$Stock$maxage
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  if(n.yrs == length(mainyrs)) {
    OM_years <- replist$startyr:replist$endyr
  } else {
    nseas <- 1/replist$seasdurations
    year_frac <- data.frame(mainyrs = mainyrs, seas = rep(1:nseas, length(mainyrs)/nseas), 
                            true_year = rep(1:(length(mainyrs)/nseas), each = nseas))
    age_frac <- data.frame(age = 0:maxage) %>% mutate(true_age = floor(age/nseas))
    OM_years <- dplyr::filter(year_frac, seas == 1) %>% getElement("mainyrs")
  }
  N_SS <- replist$natage  %>% filter(Yr %in% OM_years, Seas == 1, `Beg/Mid`=="B")
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
  N_dat <- dplyr::bind_rows(N_OM, N_SS)
  
  ggplot(N_dat, aes(x=Yr, y=N, color=Model, linetype=Model)) +
    facet_wrap(~Sex) +
    geom_line() +
    theme_bw() + 
    labs(x="Year", y="Total number")
}

compareSBmulti <- function(replist, multiHist) {
  mainyrs <- replist$startyr:replist$endyr
  SB_SS <- replist$timeseries %>% dplyr::filter(Yr %in% mainyrs, Seas == 1) %>% dplyr::select(Year=Yr, SB=SpawnBio)
  
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  SB_OM <- data.frame(Sim=1:nsim, 
                      Year=rep(mainyrs, each=nsim), 
                      SB=c(as.vector(apply(multiHist[[1]][[1]]@TSdata$SBiomass, 1:2, sum))))
  
  SB_OM$Model <- 'MOM'
  SB_SS$Model <- 'SS'
  SB_dat <- bind_rows(SB_OM, SB_SS)
  
  ggplot(SB_dat, aes(x=Year, y=SB, color=Model, linetype=Model)) +
    geom_line() +
    theme_bw() + 
    labs(x="Year", y="Spawning Biomass")
}

compareSB_depmulti <- function(replist, multiHist) {
  mainyrs <- replist$startyr:replist$endyr
  SB_SS <- replist$timeseries %>% dplyr::filter(Yr %in% mainyrs, Seas == 1) %>% dplyr::select(Year=Yr, SB=SpawnBio)
  SB_SS$SB <- SB_SS$SB/replist$SBzero # See also replist$derived_quants$Value[rownames(replist$derived_quants) == "SSB_Virgin"]
  
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  SB_OM <- data.frame(Sim=1:nsim, 
                      Year=rep(mainyrs, each=nsim), 
                      SB=c(as.vector(apply(multiHist[[1]][[1]]@TSdata$SBiomass, 1:2, sum))))
  
  # SB0
  sb0 <- data.frame(Sim=1:nsim, SB0=multiHist[[1]][[1]]@Ref$ReferencePoints$SSB0)
  SB_OM <- dplyr::left_join(SB_OM, sb0, by="Sim")
  SB_OM <- SB_OM %>% dplyr::mutate(SB=SB/SB0)
  
  SB_OM$Model <- 'MOM'
  SB_SS$Model <- 'SS'
  SB_dat <- bind_rows(SB_OM, SB_SS)
  
  ggplot(SB_dat, aes(x=Year, y=SB, color=Model, linetype=Model)) +
    geom_line() +
    theme_bw() + 
    ylim(0,1) +
    labs(x="Year", y="Spawning Depletion")
}

compareBmulti <- function(replist, multiHist) {
  mainyrs <- replist$startyr:replist$endyr
  B_SS <- replist$timeseries %>% dplyr::filter(Yr %in% mainyrs, Seas == 1) %>% dplyr::select(Year=Yr, B=Bio_all)
  
  n.p <- length(multiHist)
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  
  B_OMlist <- list()
  for (p in 1:n.p) {
    B_OMlist[[p]] <- data.frame(Sim=1:nsim, 
                                Year=rep(mainyrs, each=nsim), 
                                Sex=p,
                                B=c(as.vector(apply(multiHist[[p]][[1]]@TSdata$Biomass, 1:2, sum))))
  }
  B_OM <- do.call('rbind', B_OMlist)
  B_OM <- B_OM %>% group_by(Sim, Year) %>% summarise(B=sum(B), .groups = 'keep')
  
  B_OM$Model <- 'MOM'
  B_SS$Model <- 'SS'
  B_dat <- bind_rows(B_OM, B_SS)
  
  ggplot(B_dat, aes(x=Year, y=B, color=Model, linetype=Model)) +
    geom_line() +
    theme_bw() + 
    labs(x="Year", y="Total Biomass")
}

compareCmulti <- function(replist, multiHist) {
  # removals 
  mainyrs <- replist$startyr:replist$endyr
  C_SS <- replist$catch %>% dplyr::filter(Yr %in% mainyrs) %>%
    dplyr::select(Year=Yr, Fleet=Fleet, C=Exp, Seas = Seas) %>%
    dplyr::group_by(Year, Fleet) %>% 
    dplyr::summarise(C = sum(C))
  
  n.p <- length(multiHist)
  n.f <- length(multiHist[[1]])
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  
  C_OMlist <- list()
  cnt <- 0
  for (p in 1:n.p) {
    for (fl in 1:n.f) {
      cnt <- cnt+1
      C_OMlist[[cnt]] <- data.frame(Sim=1:nsim, 
                                    Year=rep(mainyrs, each=nsim), 
                                    Sex=p,
                                    Fleet=fl,
                                    C=c(as.vector(apply(multiHist[[p]][[fl]]@TSdata$Removals, 1:2, sum))),
                                    R=c(as.vector(apply(multiHist[[p]][[fl]]@TSdata$Landings, 1:2, sum))))
      
    }
  }
  C_OM <- do.call('rbind', C_OMlist)
  C_OM <- C_OM %>% group_by(Year, Fleet, Sim) %>% summarise(C=sum(C), R=sum(R), .groups = 'drop')
  
  C_OM$Model <- 'MOM'
  C_SS$Model <- 'SS'
  C_dat <- bind_rows(C_OM, C_SS)
  
  ggplot(C_dat, aes(x=Year, y=C, color=Model, linetype=Model)) +
    geom_line() +
    facet_wrap(~Fleet, scales="free") +
    theme_bw() + 
    labs(x="Year", y="Catch (removals) by Fleet")
  
  # break into groups of 8 fleets
  fleets <- C_dat$Fleet %>% unique()
  fleet_groups <- split(fleets, ceiling(seq_along(fleets)/8))
  pout <- list()
  for (i in seq_along(fleet_groups)) {
    C_dat2 <- C_dat %>% dplyr::filter(Fleet %in% fleet_groups[[i]]) %>% 
      mutate(Fleet2 = factor(replist$FleetNames[Fleet], levels = replist$FleetNames[fleet_groups[[i]]]))
    pout[[i]] <- ggplot(C_dat2, aes(x=Year, y=C, color=Model, linetype=Model)) +
      geom_line() +
      facet_wrap(~Fleet2, scales="free") +
      theme_bw() + 
      labs(x="Year", y="Catch (removals) by Fleet")
  }
  
  for (i in seq_along(fleet_groups)) {
    print(pout[[i]]) 
  }
  
}

compareC_overallmulti <- function(replist, multiHist) {
  # removals 
  mainyrs <- replist$startyr:replist$endyr
  C_SS <- replist$catch %>% dplyr::filter(Yr %in% mainyrs) %>%
    dplyr::select(Year=Yr, Fleet=Fleet, C=Exp)
  
  n.p <- length(multiHist)
  n.f <- length(multiHist[[1]])
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  
  C_OMlist <- list()
  cnt <- 0
  for (p in 1:n.p) {
    for (fl in 1:n.f) {
      cnt <- cnt+1
      C_OMlist[[cnt]] <- data.frame(Sim=1:nsim, 
                                    Year=rep(mainyrs, each=nsim), 
                                    Sex=p,
                                    Fleet=fl,
                                    C=c(as.vector(apply(multiHist[[p]][[fl]]@TSdata$Removals, 1:2, sum))),
                                    R=c(as.vector(apply(multiHist[[p]][[fl]]@TSdata$Landings, 1:2, sum))))
      
    }
  }
  C_OM <- do.call('rbind', C_OMlist)
  C_OM <- C_OM %>% group_by(Year, Fleet, Sim) %>% summarise(C=sum(C), R=sum(R), .groups = 'drop')
  
  C_OM$Model <- 'MOM'
  C_SS$Model <- 'SS'
  C_dat <- bind_rows(C_OM, C_SS)
  # Overall
  C_dat_total <- C_dat %>% group_by(Year, Model, Sim) %>% summarise(Catch=sum(C), .groups = 'keep')
  
  ggplot(C_dat_total, aes(x=Year, y=Catch, color=Model, linetype=Model)) +
    geom_line() +
    theme_bw() + 
    labs(x="Year", y="Total Catch (removals)")
  
}

compareRecmulti <- function(replist, multiHist) {
  year_lab <- "Year"
  mainyrs <- replist$startyr:replist$endyr
  maxage <- multiHist[[1]][[1]]@SampPars$Stock$maxage
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  if(n.yrs == length(mainyrs)) {
    OM_years <- replist$startyr:replist$endyr
  } else {
    nseas <- 1/replist$seasdurations
    year_frac <- data.frame(mainyrs = mainyrs, seas = rep(1:nseas, length(mainyrs)/nseas), 
                            true_year = rep(1:(length(mainyrs)/nseas), each = nseas))
    age_frac <- data.frame(age = 0:maxage) %>% mutate(true_age = floor(age/nseas))
    OM_years <- dplyr::filter(year_frac, seas == 1) %>% getElement("mainyrs")
  }
  N_SS <- replist$natage  %>% filter(Yr %in% OM_years, `Beg/Mid`=="B")
  cols <- which(colnames(N_SS)=='0')
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
                                N=c(as.vector(apply(multiHist[[p]][[1]]@AtAge$Number[,1,,], 1:2, sum))))
  }
  N_OM <- do.call('rbind', N_OMlist)
  N_OM$Model <- 'MOM'
  N_SS$Model <- 'SS'
  N_dat <- dplyr::bind_rows(N_OM, N_SS)
  
  ggplot(N_dat, aes(x=Yr, y=N, color=Model, linetype=Model)) +
    facet_wrap(~Sex) +
    geom_line() +
    theme_bw() + 
    labs(x="Year", y="Recruitment (age 0)")
}

compareAmulti <- function(replist, multiHist) {
  year_lab <- "Year"
  mainyrs <- replist$startyr:replist$endyr
  maxage <- multiHist[[1]][[1]]@SampPars$Stock$maxage
  n.yrs <- dim(multiHist[[1]][[1]]@TSdata$Number)[2]
  nsim <- dim(multiHist[[1]][[1]]@TSdata$Number)[1]
  if(n.yrs == length(mainyrs)) {
    OM_years <- replist$startyr:replist$endyr
  } else {
    nseas <- 1/replist$seasdurations
    year_frac <- data.frame(mainyrs = mainyrs, seas = rep(1:nseas, length(mainyrs)/nseas), 
                            true_year = rep(1:(length(mainyrs)/nseas), each = nseas))
    age_frac <- data.frame(age = 0:maxage) %>% mutate(true_age = floor(age/nseas))
    OM_years <- dplyr::filter(year_frac, seas == 1) %>% getElement("mainyrs")
  }
  N_SS <- replist$natage  %>% filter(Yr %in% OM_years, Seas == 1, `Beg/Mid`=="B") # More recruits show up in later seasons within year
  cols <- which(colnames(N_SS)=='0'):ncol(replist$natage)
  N_SS <- N_SS %>% 
    tidyr::pivot_longer(cols=all_of(cols)) %>%
    mutate(Age=as.numeric(name), N=value) %>% 
    group_by(Age, Yr, Sex) %>% 
    summarise(N = sum(N))

  np <- length(multiHist)
  N_OMlist <- list()
  Ages <- N_SS$Age %>% unique()
  for (p in 1:np) {
    N_OMlist[[p]]  <- data.frame(Age=Ages,
                                  Yr=rep(mainyrs, each=length(Ages)), 
                                  Sex=rep(p, n.yrs*length(Ages)),
                                  N=c(as.vector(apply(multiHist[[p]][[1]]@AtAge$Number[1,,,],1:2, sum))))
  }
  N_OM <- do.call('rbind', N_OMlist)
  N_OM$Model <- 'MOM'
  N_SS$Model <- 'SS'
  N_dat <- dplyr::bind_rows(N_OM, N_SS)
  
  # break into groups of 16 years
  yrs <- N_dat$Yr %>% unique()
  yr_groups <- split(yrs, ceiling(seq_along(yrs)/16))
  pout <- list()
  for (i in seq_along(yr_groups)) {
    N_dat2 <- N_dat %>% dplyr::filter(Yr %in% yr_groups[[i]])
    N_dat2$Sex <- as.factor(N_dat2$Sex)
    pout[[i]] <- ggplot(N_dat2, aes(x=Age, y=N, color=Sex, linetype=Model)) +
      facet_wrap(~Yr, ncol=4) +
      geom_line() +
      theme_bw() + 
      labs(x="Age", y="Numbers-at-Age")
  }
  
  for (i in seq_along(yr_groups)) {
   print(pout[[i]]) 
  }
}



