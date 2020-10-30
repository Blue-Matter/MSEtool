#' Creates a time series per simulation that has a random normal walk with sigma
#' 
#' @param targ mean
#' @param targsd standard deviation
#' @param nyears number of years to simulate
#' @param nsim number of simulations
#' @keywords internal
#'  
GenerateRandomWalk <- function(targ, targsd, nyears, nsim, rands=NULL) {
  mutemp <- -0.5 * targsd^2
  
  if (!is.null(rands)) {
    temp <- rands
  } else {
    temp <- array(exp(rnorm(nsim*nyears, mutemp, targsd)),dim = c(nsim, nyears))
  }
  if (nsim >1) {
    return(targ * temp/apply(temp, 1, mean))
  } else {
    return(targ * temp/mean(temp))
  }
  
}


#' Optimization function to find a movement model that matches user specified
#' movement characteristics modified for Rcpp.
#' 
#' The user specifies the probability of staying in the same area and spatial
#' heterogeneity (both in the unfished state).
#' 
#' This is paired with movfit to find the correct movement model.
#' 
#' @param x A position in vectors Prob_staying and Frac_area_1
#' @param Prob_staying User specified probability that individuals in area 1
#' remain in that area (unfished conditions)
#' @param Frac_area_1 User specified fraction of individuals found in area 1
#' (unfished conditions)
#' @return A markov movement matrix
#' @author T. Carruthers
#' @export
#' @examples
#' 
#' Prob_staying<-0.8 # probability  that individuals remain in area 1 between time-steps
#' Frac_area_1<-0.35 # the fraction of the stock found in area 1 under equilibrium conditions
#' markovmat<-getmov2(1,Prob_staying, Frac_area_1)
#' vec<-c(0.5,0.5) # initial guess at equilibrium distribution (2 areas)
#' for(i in 1:300)vec<-apply(vec*markovmat,2,sum) # numerical approximation to stable distribution
#' c(markovmat[1,1],vec[1]) # pretty close right?
#' 
#' 
getmov2 <- function(x, Prob_staying, Frac_area_1) {
  test <- optim(par = c(0, 0, 0), movfit_Rcpp, method = "L-BFGS-B", 
                lower = rep(-6, 3), upper = rep(6, 3), prb = Prob_staying[x], 
                frac = Frac_area_1[x])	
  mov <- array(c(test$par[1], test$par[2], 0, test$par[3]), dim = c(2, 2))
  mov <- exp(mov)
  mov/array(apply(mov, 1, sum), dim = c(2, 2))
}

#' Calculate historical fishing mortality
#' 
#' 
#' @param Esd vector of standard deviation 
#' @param nyears number of years 
#' @param EffYears index of years
#' @param EffLower vector of lower bound
#' @param EffUpper vector of upper bound
#' @export getEffhist
#' @keywords internal
#'  
getEffhist <- function(Esd, nyears, EffYears, EffLower, EffUpper) {
  if (length(EffLower) == length(EffUpper) & length(EffUpper) == length(EffYears)) {
    nsim <- length(Esd)  # get nsim 
    if (EffYears[1] == 1 & EffYears[length(EffYears)] == nyears & length(EffYears) == nyears) {
      refYear <- EffYears
    } else{
      refYear <- ceiling(range01(EffYears + 0.5) * nyears) # standardize years 
      refYear[1] <- 1 # first year is year 1 
      refYear[length(refYear)] <- nyears  # first year is year 1 
    }
    
    if (any(EffLower > EffUpper)) {
      ind <- which(EffLower > EffUpper)
      message("Some values in 'EffLower' are higher than 'EffUpper': Years ", paste(ind, ""),
              "\nSetting 'EffLower' to the lower of the two values.")
      tt <- cbind(EffLower, EffUpper)
      EffLower <- apply(tt, 1, min)
      EffUpper <- apply(tt, 1, max)
    }
    
    # sample Effort
    # fmat <- rbind(EffLower, EffUpper)
    
    # nyrs <- length(EffLower)
    # Effs <- matrix(0, nsim, nyrs)
    
    # ind <- which(diff(fmat) > 0)[1]
    # for (X in 1:ind) {
    # Effs[,X] <- runif(nsim, min(fmat[,X]), max(fmat[,X]))  
    # }
    
    # val <- (Effs[,ind] - min(fmat[,ind]))/ diff(fmat[,ind])
    # for (X in 2:nyrs) Effs[,X] <- min(fmat[,X]) + diff(fmat[,X])*val
    
    Effs <- mapply(runif, n = nsim, min = EffLower, max = EffUpper)  # sample Effort
    if (nsim > 1) {
      if (ncol(Effs) == 1) {
        effort <- matrix(Effs, nrow=nsim, ncol=nyears)
      } else {
        effort <- t(sapply(1:nsim, function(x) approx(x = refYear, 
                                                      y = Effs[x, ], method = "linear", n = nyears)$y))  # linear interpolation
      }
      
    } 
    if (nsim == 1) {
      if (length(Effs) == 1) {
        effort <- matrix(Effs, nrow=nsim, ncol=nyears)
      } else {
        effort <- matrix(approx(x = refYear, y = Effs, method = "linear", n = nyears)$y, nrow = 1)
      }
    }
    
    if (!all(effort == mean(effort))) effort <- range01(effort)  
    
    effort[effort == 0] <- 0.01
    
    Emu <- -0.5 * Esd^2
    Eerr <- array(exp(rnorm(nyears * nsim, rep(Emu, nyears), rep(Esd, nyears))), c(nsim, nyears))  # calc error
    out <- NULL
    eff <- effort * Eerr  # add error 
    out[[1]] <- eff
    out[[2]] <- (effort[, nyears] - effort[, nyears - 4])/5
    return(out)
  } else {
    message("Input vectors of effort years and bounds not of same length")
    return(NULL)
  }
}

#' Standardize values
#' 
#' Function to standardize to value relative to minimum and maximum values
#' @param x vector of values 
#' @param Max Maximum value
#' @param Min Minimum value 
#' @keywords internal
Range <- function(x, Max, Min) {
  (x - Min)/(Max - Min)  
}

range01 <- function(x) {
  (x - min(x))/(max(x) - min(x)) 
}

#' Calculate selectivity curve
#'
#' @param x Simulation number
#' @param lens Matrix of lengths (nsim by nlengths)
#' @param lfs Vector of length at full selection (nsim long)
#' @param sls Vector of sigmas of ascending limb (nsim long)
#' @param srs Vector of sigmas of descending limb (nsim long)
#'
#' @export
getsel <- function(x, lens, lfs, sls, srs) {
  if (is.null(ncol(lens))) return(dnormal(lens, lfs[x], sls[x], srs[x]))
  dnormal(lens[x,], lfs[x], sls[x], srs[x])
}


#' Double-normal selectivity curve
#'
#' @param lens Vector of lengths 
#' @param lfs Length at full selection
#' @param sl Sigma of ascending limb
#' @param sr Sigma of descending limb
#'
#'
dnormal<-function(lens,lfs,sl,sr){
  cond<-lens<=lfs
  sel<-rep(NA,length(lens))
  sel[cond]<-2.0^-((lens[cond]-lfs)/sl*(lens[cond]-lfs)/sl)
  sel[!cond]<-2.0^-((lens[!cond]-lfs)/sr*(lens[!cond]-lfs)/sr)
  sel
}

calcV <- function(x, Len_age, LenCV, SLarray, n_age, nyears, proyears, CAL_binsmid) {
  len_at_age <- Len_age[x,,]
  len_aa_sd <- LenCV[x] * len_at_age
  sel_at_length <- SLarray[x,,]
  v <- matrix(tiny, n_age, nyears+proyears)
  for (yr in 1:(nyears+proyears)) {
    ALK <- mapply(dnorm, mean=len_at_age[,yr], sd=len_aa_sd[,yr], MoreArgs=list(x=CAL_binsmid))
    ALK[ALK<=0] <- tiny
   
    if (all(ALK[,1]==tiny)) {
      ALK[,1] <- 0
      ALK[1,1] <- 1
    }
    ALK_t <- matrix(colSums(ALK), nrow=nrow(ALK), ncol=ncol(ALK), byrow = TRUE)
    ALK <- t(ALK/ALK_t)
    sela <- ALK %*% sel_at_length[,yr] 
    v[,yr] <- sela[,1]
  }
  v
}

# calculate average unfished ref points over first A50 years
CalcUnfishedRefs <- function(x, ageM, N0_a, SSN0_a, SSB0_a, B0_a, VB0_a, SSBpRa, SSB0a_a) {
  avg.ind <- 1:(ceiling(ageM[x,1])+1) # unfished eq ref points averaged over these years 
  nyears <- dim(N0_a)[2]
  if (length(avg.ind) > nyears) avg.ind <- 1:nyears
  
  N0 <- mean(N0_a[x, avg.ind])
  SSN0  <- mean(SSN0_a[x, avg.ind])
  SSB0 <- mean(SSB0_a[x, avg.ind])
  B0 <- mean(B0_a[x, avg.ind])
  VB0 <- mean(VB0_a[x, avg.ind])
  SSBpR <- mean(SSBpRa[x, avg.ind])
  if (length(avg.ind)>1) {
    SSB0a <- apply(SSB0a_a[x,avg.ind,], 2, mean)  
  } else {
    SSB0a <- SSB0a_a[x,avg.ind,]
  }
  
  list(N0=N0, SSN0=SSN0, SSB0=SSB0, B0=B0, VB0=VB0, SSBpR=SSBpR, SSB0a=SSB0a)
}


CalcMSYRefs <- function(x, MSY_y, FMSY_y, SSBMSY_y, BMSY_y, VBMSY_y, ageM, nyears) {
  n.yrs <- ceiling(ageM[x,nyears]) # MSY ref points averaged over these years
  nyears1 <- dim(ageM)[2]
  minY <- floor(n.yrs/2) 
  maxY <- n.yrs - minY - 1 
  avg.ind <- (nyears - minY):(nyears + maxY)
  avg.ind <- avg.ind[avg.ind>0]
  if (max(avg.ind) > nyears1) avg.ind <- avg.ind[avg.ind < nyears1]
  
  MSY <- mean(MSY_y[x, avg.ind])
  FMSY <- mean(FMSY_y[x, avg.ind])
  SSBMSY <- mean(SSBMSY_y[x, avg.ind])
  BMSY <- mean(BMSY_y[x, avg.ind])
  VBMSY <- mean(VBMSY_y[x, avg.ind])
  data.frame(MSY=MSY, FMSY=FMSY, SSBMSY=SSBMSY, BMSY=BMSY, VBMSY=VBMSY)
}

CalcDynamicSSB0 <- function(StockPars, nsim, nareas, nyears, proyears, maxF, Mhist, Nhist) {
  n_age <- StockPars$maxage + 1
  UnfishedHist <- sapply(1:nsim, function(x) 
    popdynCPP(nareas, StockPars$maxage, 
              Ncurr=Nhist[x,,1,], 
              nyears,  
              M_age=StockPars$M_ageArray[x,,], 
              Asize_c=StockPars$Asize[x,], 
              MatAge=StockPars$Mat_age[x,,], 
              WtAge=StockPars$Wt_age[x,,],
              Vuln=matrix(1, n_age, nyears + proyears),
              Retc=matrix(1, n_age, nyears + proyears), 
              Prec=StockPars$Perr_y[x,], 
              movc=split.along.dim(StockPars$mov[x,,,,],4), 
              SRrelc=StockPars$SRrel[x], 
              Effind=rep(0,nyears),  
              Spat_targc=1, 
              hc=StockPars$hs[x], 
              R0c=StockPars$R0a[x,], 
              SSBpRc=StockPars$SSBpR[x,], 
              aRc=StockPars$aR[x,], 
              bRc=StockPars$bR[x,], 
              Qc=1, 
              Fapic=0, 
              MPA=matrix(1, nyears + proyears, nareas), 
              maxF=maxF, 
              control=1, 
              SSB0c=StockPars$SSB0[x], 
              plusgroup=StockPars$plusgroup))
  
  Dynamic_SSB0 <- aperm(array(as.numeric(unlist(UnfishedHist[4,], use.names=FALSE)), 
                              dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3)) %>% apply(c(1,3), sum)
  
  # Projection period 
  # (but called in Simulate.R, always check if closed-loop in Project.R has been updated; last checked 10-14-2020)
  N_P <- SSN_P <- SSB_P <- array(NA, dim = c(nsim, n_age, proyears, nareas))
  
  # Movement and mortality in first year
  SAYRt <- as.matrix(expand.grid(1:nsim, 1:n_age, 1 + nyears, 1:nareas)) 
  SAYR <- as.matrix(expand.grid(1:nsim, 1:n_age, 1, 1:nareas))
  SAY1 <- SAYRt[, 1:3]
  
  N <- aperm(array(as.numeric(unlist(UnfishedHist[1,], use.names=FALSE)), 
                   dim=c(n_age, nyears, nareas, nsim)), c(4,1,2,3))
  NextYrN <- lapply(1:nsim, function(x)
    popdynOneTScpp(nareas, StockPars$maxage, Ncurr=N[x,,nyears,],
                   Zcurr=Mhist[x,,nyears,],
                   mov=StockPars$mov[x,,,,nyears+1],
                   plusgroup = StockPars$plusgroup))
  
  # The stock at the beginning of projection period
  N_P[,,1,] <- aperm(array(unlist(NextYrN), dim=c(n_age, nareas, nsim, 1)), c(3,1,4,2))
  SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAY1]  # Calculate spawning stock numbers
  SSB_P[SAYR] <- SSN_P[SAYR] * StockPars$Wt_age[SAY1]
  
  # recruitment in first projection year
  SSBcurr <- apply(SSB_P[,,1,],c(1,3), sum)
  recdev <- StockPars$Perr_y[, nyears+n_age-1]
  rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars$SRrel, SSBcurr=SSBcurr,
                     recdev=recdev, hs=StockPars$hs,
                     aR= StockPars$aR, bR=StockPars$bR, R0a=StockPars$R0a,
                     SSBpR=StockPars$SSBpR)
  N_P[,1,1,] <- t(rec_area)
  
  # Loop over proyears
  M_P <- StockPars$M_ageArray[, , (nyears+1):(nyears+proyears)] %>% 
    array(c(nsim, n_age, proyears, nareas)) # Taken from CalcMPDynamics
  for(y in 2:proyears) {
    SAYRt <- as.matrix(expand.grid(1:nsim, 1:n_age, y + nyears, 1:nareas))  # Trajectory year
    SAYt <- SAYRt[, 1:3]
    SAYR <- as.matrix(expand.grid(1:nsim, 1:n_age, y, 1:nareas))
    
    NextYrN <- lapply(1:nsim, function(x)
      popdynOneTScpp(nareas, StockPars$maxage,
                     Ncurr=N_P[x,,y-1,],
                     Zcurr=M_P[x,,y-1,],
                     mov=StockPars$mov[x,,,, nyears+y],
                     plusgroup=StockPars$plusgroup))
    
    N_P[,,y,] <- aperm(array(unlist(NextYrN), dim=c(n_age, nareas, nsim, 1)), c(3,1,4,2))
    SSN_P[SAYR] <- N_P[SAYR] * StockPars$Mat_age[SAYt]  # Calculate spawning stock numbers
    SSB_P[SAYR] <- SSN_P[SAYR] * StockPars$Wt_age[SAYt]  # Calculate spawning stock biomass
    
    # recruitment in this year
    SSBcurr <- apply(SSB_P[,,y,],c(1,3), sum)
    recdev <- StockPars$Perr_y[, y+nyears+n_age-1]
    rec_area <- sapply(1:nsim, calcRecruitment, SRrel=StockPars$SRrel,
                       SSBcurr=SSBcurr,
                       recdev=recdev, hs=StockPars$hs, aR=StockPars$aR,
                       bR=StockPars$bR, R0a=StockPars$R0a, SSBpR=StockPars$SSBpR)
    
    N_P[,1,y,] <- t(rec_area)
  }
  
  Dynamic_SSB0_P <- apply(SSB_P, c(1, 3), sum)
  
  return(abind::abind(Dynamic_SSB0, Dynamic_SSB0_P, along = 2))
}

#' Linear interpolation of a y value at level xlev based on a vector x and y
#'
#' @param x A vector of x values
#' @param y A vector of y values (identical length to x)
#' @param xlev A the target level of x from which to guess y
#' @param ascending Are the the x values supposed to be ordered before interpolation
#' @param zeroint is there a zero-zero x-y intercept?
#' @author T. Carruthers
#' @keywords internal
LinInterp<-function(x,y,xlev,ascending=F,zeroint=F){
  
  if(zeroint){
    x<-c(0,x)
    y<-c(0,y)
  } 
  
  if(ascending){
    cond<-(1:length(x))<which.max(x)
  }else{
    cond<-rep(TRUE,length(x))
  }
  
  close<-which.min((x[cond]-xlev)^2)
  ind<-c(close,close+(x[close]<xlev)*2-1)
  ind <- ind[ind <= length(x)]
  if (length(ind)==1) ind <- c(ind, ind-1)
  ind<-ind[order(ind)]
  pos<-(xlev-x[ind[1]])/(x[ind[2]]-x[ind[1]])
  y[ind[1]]+pos*(y[ind[2]]-y[ind[1]])
  
}

calcRecruitment <- function(x, SRrel, SSBcurr, recdev, hs, aR, bR, R0a, SSBpR) {
  if (SRrel[x] == 1) { # BH rec
    rec_A <- recdev[x] * (4*R0a[x,] * hs[x] * SSBcurr[x,])/(SSBpR[x,] * 
                                                              R0a[x,] * (1-hs[x]) + (5*hs[x]-1) * SSBcurr[x,])
  } else { # Ricker rec
    rec_A <- recdev[x] * aR[x,] * SSBcurr[x,] * exp(-bR[x,]*SSBcurr[x,])
  }
  rec_A
}

lcs<-function(x){
  if ("matrix" %in% class(x)) {
    nsim <- nrow(x)
    nyr <- ncol(x)
    x1 <- x/matrix(apply(x, 1, mean, na.rm=TRUE), nrow=nsim, ncol=nyr) # rescale to mean 1
    x2<- log(x1) # log it
    x3 <- x2 -matrix(apply(x2, 1, mean, na.rm=TRUE), nrow=nsim, ncol=nyr) # mean 0
    x3
  } else {
    x1<-x/mean(x, na.rm=TRUE) # rescale to mean 1
    x2<-log(x1)     # log it
    x3<-x2-mean(x2, na.rm=TRUE) # mean 0
    x3
  }
  
}


#' get object class
#' 
#' Internal function for determining if object is of classy
#' 
#' 
#' @param x Character string object name
#' @param classy A class of object (character string, e.g. 'Fleet')
#' @author T. Carruthers with nasty hacks from A. Hordyk
#' @return TRUE or FALSE
getclass <- function(x, classy) {
  return(any(class(get(x)) == classy)) # inherits(get(x), classy) - this gives a problem since we now inherit Stock etc in OM
} 

indfit <- function(sim.index,obs.ind, Year, plot=FALSE, lcex=0.8){
  
  sim.index <- lcs(sim.index[!is.na(obs.ind)]) # log space conversion of standardized simulated index
  obs.ind <- lcs(obs.ind[!is.na(obs.ind)]) # log space conversion of standardized observed ind
  
  if(plot){
    par(mfrow=c(1,2),mai=c(0.7,0.5,0.05,0.01),omi=c(0.01,0.2,0.01,0.01))
    plot(exp(sim.index),exp(obs.ind),xlab="",ylab="",pch=19,col=rgb(0,0,0,0.5))
    mtext("Model estimate",1,line=2.2)
    mtext("Index",2,outer=T,line=0)
  }
  
  opt<-optimize(getbeta,x=exp(sim.index),y=exp(obs.ind),interval=c(0.1,10))
  res<-exp(obs.ind)-(exp(sim.index)^opt$minimum)
  ac<-acf(res,plot=F)$acf[2,1,1] # lag-1 autocorrelation
  
  res2<-obs.ind-sim.index                  # linear, without hyperdepletion / hyperstability
  ac2<-acf(res2,plot=F)$acf[2,1,1] # linear AC
  
  if(plot){
    SSBseq<-seq(min(exp(sim.index)),max(exp(sim.index)),length.out=1000)
    lines(SSBseq,SSBseq^opt$minimum,col='#0000ff90',pch=19)
    legend('bottomright',legend=round(c(sum((obs.ind-sim.index)^2),opt$objective),3),text.col=c("black","blue"),bty='n',title="SSQ",cex=lcex)
    legend('topleft',legend=round(opt$minimum,3),text.col="blue",bty='n',title='Hyper-stability, beta',cex=lcex)
    legend('left',legend=round(stats::cor(sim.index,obs.ind),3),bty='n',title='Correlation',cex=lcex)
    
    plot(Year,sim.index,ylab="",xlab="",ylim=range(c(obs.ind,sim.index)),type="l")
    mtext("Year",1,line=2.2)
    points(Year,obs.ind,col='#ff000090',pch=19)
    legend('topleft',legend=round(ac,3),text.col="red",bty='n',title="Lag 1 autocorrelation",cex=lcex)
    legend('bottomleft',legend=round(sd(res),3),text.col="red",bty='n',title="Residual StDev",cex=lcex)
    legend('topright',legend=c("Model estimate","Index"),text.col=c("black","red"),bty='n',cex=lcex)
  }
  
  data.frame(beta=opt$minimum,AC=ac,sd=sd(exp(obs.ind)/(exp(sim.index)^opt$minimum)),
             cor=stats::cor(sim.index,obs.ind),AC2=ac2,sd2=sd(obs.ind-sim.index))
  
  # list(stats=data.frame(beta=opt$minimum,AC=ac,sd=sd(exp(obs.ind)/(exp(sim.index)^opt$minimum)),
  #                       cor=cor(sim.index,obs.ind),AC2=ac2,sd2=sd(obs.ind-sim.index)),
  #      mult=exp(obs.ind)/(exp(sim.index)^opt$minimum))
  
}

getbeta<-function(beta,x,y)sum((y-x^beta)^2)

generateRes <- function(df, nsim, proyears, lst.err) {
  sd <- df$sd 
  ac <- df$AC
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
      res[y,x] <- ac[x] * lst.err[x] + lst.err[x] * (1-ac[x] * ac[x])^0.5 
    } else {
      res[y,x] <- ac[x] * res[y-1,x] + res[y,x] * (1-ac[x] * ac[x])^0.5  
    }
  }
  res[,x]
}


#' Check for duplicated MPs names
#'
#' Custom MPs cannot have the same names of MPs in MSEtool and related packages  
#' @param MPs Character vector of MP names
#'
#' @return An error if duplicated MP names, otherwise nothing
CheckDuplicate <- function(MPs) {
  # check if custom MP names already exist in DLMtool
  tt <- suppressWarnings(try(lsf.str(envir=globalenv()), silent=TRUE))
  if (class(tt)!="try-error") {
    gl.funs <- as.vector(tt)
    pkg.funs <- as.vector(ls.str('package:MSEtool'))
    if ('package:DLMtool' %in% search()) 
      pkg.funs <- c(pkg.funs, as.vector(ls.str('package:DLMtool')))
    if ('package:SAMtool' %in% search()) 
      pkg.funs <- c(pkg.funs, as.vector(ls.str('package:SAMtool')))
    
    if (length(gl.funs)>0) {
      gl.clss <- unlist(lapply(lapply(gl.funs, get), class))
      gl.MP <- gl.funs[gl.clss %in% 'MP']
      if (length(gl.MP)>0) {
        inc.gl <- gl.MP[gl.MP %in% MPs]
        if (length(inc.gl)>0) {
          dup.MPs <- inc.gl[inc.gl %in% pkg.funs]
          if (length(dup.MPs)>0) {
            stop("Custom MP names already exist in MSEtool or other packages.\nRename Custom MPs: ", 
                 paste0(dup.MPs, collapse=", "))
          }
        }
      } 
    }
  } 
}


#' Check that specified MPs are valid and will run on MSEtool::SimulatedData
#'
#' @param MPs Character vector of MP names 
#' @param silent Logical. Report messages?
#'
#' @return MP names
CheckMPs <- function(MPs=NA, silent=FALSE) {
  if (all(is.na(MPs))) {
    if (!silent) message('Argument `MPs=NA`, using all example MPs in `MSEtool`')
    MPs <- avail("MP", 'MSEtool')
  } 
  
  # Check for custom MPs with same name as built-it MPs
  CheckDuplicate(MPs)
  
  if (!silent) message('Checking MPs')
  
  # Check MP names are valid functions of class MP
  chkMP <- list()
  for (mm in MPs) 
    chkMP[[mm]] <- try(get(mm), silent=TRUE)
  
  clss <- unlist(lapply(chkMP, class))
  invalid <- clss[clss!='MP']
  if (length(invalid)>0) {
    stop("Some MPs are not a functions of class `MP`: ", 
         paste0(names(invalid), collapse=", "), call.=FALSE) 
  }
  
  # Check on simulated data
  pass <- rep(TRUE, length(MPs))
  for (i in seq_along(MPs)) {
    mp <- get(MPs[i])
    tryMP <- try(sapply(1:3, mp, MSEtool::SimulatedData), silent=TRUE)
    err <- unique(unlist(lapply(tryMP, class) ))
    if (err =='character') pass[i] <- FALSE
  }
  if (any(!pass))
    warning('Some MPs fail with MSEtool::SimulatedData: ', 
            paste0(MPs[!pass], collapse=", "), call.=FALSE) 
  
  MPs
}


# Calculate consecutive runs 
# https://stackoverflow.com/a/16912472/2885462
findIntRuns <- function(run){
  rundiff <- c(1, diff(run))
  difflist <- split(run, cumsum(rundiff!=1))
  unlist(lapply(difflist, function(x){
    if(length(x) %in% 1:2) as.character(x) else paste0(x[1], "-", x[length(x)])
  }), use.names=FALSE)
}


