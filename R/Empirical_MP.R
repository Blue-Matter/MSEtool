
#' General purpose polynomial smoother
#'
#' @description Polynomial smoother (no gradient prediction) applied to a vector that can include NA values. Intended to be rapid for use in management procedures
#' @param xx Vector of real numbers, data to be smoothed. 
#' @param plot Logical, should the 'fit' of the smoother be plotted?  
#' @param enp_mult Fraction, effective number of parameters multiplier. The smoother parameter number is length(xx) x enp_mult. So higher values of enp_mult means less smoothing (more parameters).
#' @param plotname Character, in case you want to put a label on the plot (plot = T).  
#' @param xlab Character, in case you want an xaxis label on the plot (plot = T)
#' @param ylab Character, in case you want a yaxis label on the plot (plot = T)
#' @param x Numeric vector same length as xx, in case you want to have a custom xaxis (e.g. years)
#' @author T. Carruthers
#' @export
smoothy<-function(xx,plot=F,enp_mult,plotname="",xlab="x",ylab="y",x = NA){
  tofill<-!is.na(xx)
  xx[xx==0]<-1E3
  predout<-rep(NA,length(xx))
  if(is.na(x[1]))x = 1:length(xx)
  dat<-data.frame(x=x,y=log(xx))
  enp.target<-sum(tofill)*enp_mult
  out<-loess(y~x,data=dat,enp.target=enp.target)
  predout[tofill]<-exp(predict(out))
  if(plot){
    plot(x,xx,type="p",xlab=xlab,ylab=ylab,main=plotname); grid()
    lines(x,predout,col="#ff000090",lwd=2)
    legend('topright',c("Observed","Smoothed"),text.col=c("black","red"),bty='n')
  }
  predout
}


#' Create indices that are sampled at various frequencies
#'
#' @description Given an index (historical period and projected period) this function creates sparsity in the projected index to simulate varying frequency (intensity) of data collection. 
#' @param I_hist Vector of real numbers, concatinated observed (historical) and simulated (projected) indices.
#' @param I_freq Positive integer. The frequency of index sampling (e.g. 1 is every year, 2 is every 2 years - a gap every 2 years in the projected, simulated data). 
#' @param LHYr Positive integer, a year (e.g. 2023), the last historical year, demarks the historical period where observations have been collected from the projected period where sparsity is to be simulated. 
#' @param CurYr Positive integer, a year (e.g. 2043), the most recent year of the simulation.  
#' @param Year Vector of positive integers (as long as I_hist), the years corresponding with I_hist. 
#' @return A thinned ector I_hist long of index observations. 
#' @author T. Carruthers
#' @export
doIfreq=function(I_hist, I_freq, LHYr, CurYr, Year){
  I_hist[I_hist < 0] <- NA
  nI = nrow(I_hist)
  ny <- ncol(I_hist)
  nkeep = sum(I_freq!=0)
  I_keep = array(NA,c(nkeep,ny))
  Is = (1:nI)[I_freq>0]
  
  j = 0
  for(i in Is){
    j = j+1
    Ivec = I_hist[i,]
    pind = match((LHYr+1):CurYr,Year)
    if(!(is.na(pind[1]))){
      np = length(pind)
      makeNA = rep(c(rep(TRUE,I_freq[i]-1),FALSE),100)[1:np]
      Ivec[pind[makeNA]] = NA
      I_keep[j,] = Ivec
    }else{
      I_keep[j,] = Ivec
    }
  } 
  I_keep
}


#' Calculate a management recommendation given constraints 
#'
#' @description Creates a TAC management recommendation given constraints on how much that can change from previous TAC and contraints on minimum and maximum TAC
#' @param MPrec Positive real number, the previous management recommendation (e.g. 100 tonnes). 
#' @param mod Imperfect fraction, the proposed modification (change to MPrec) (e.g. 1.2 is a 20% increase)
#' @param delta_down A vector 2 positions long, the minimum and maximum levels of downward change (e.g. when mod < 1) in the recommendation. 
#' @param delta_up A vector 2 positions long, the minimum and maximum levels of upward change (e.g. when mod > 1) in the recommendation. 
#' @param TACrng A vector 2 positions long, the minimum and maximum TAC (same units as MPrec).  
#' @return n object of class \linkS4class{Rec}.
#' @author T. Carruthers
#' @export
doRec = function(MPrec, mod, delta_down, delta_up, TACrng){ 

  # TAC change
  minchng_down = delta_down[1]
  maxchng_down = delta_down[2]
  minchng_up = delta_up[1]
  maxchng_up = delta_up[2]
  
  if(mod > (1+maxchng_up)) mod = 1+maxchng_up    # above max TAC increase
  if(mod < (1-maxchng_down)) mod = 1-maxchng_down    # below max TAC reduction
  if(mod < (1+minchng_up) & mod > (1-minchng_down)) mod = 1  # within min TAC change
  
  # TAC constraint
  TrialTAC = MPrec * mod
  Rec = new('Rec')
  if(TrialTAC > TACrng[2]){
    Rec@TAC = TACrng[2] # max TAC
  }else if(TrialTAC < TACrng[1]){
    Rec@TAC = TACrng[1] # min TAC
  }else{
    Rec@TAC = TrialTAC
  }
  
  Rec
}

#' Hockey Stick Harvest control rule that modifies TAC. 
#'
#' @description A hockey stick (2 inflection points) HCR that accepts estimated level relative to reference level and modifies a proposed TAC based on control points for the x axis (est/ref) and y axis (fraction of TAC)
#' @param trial_TAC Postitive real number, the proposed total allowable catch before HCR modification. 
#' @param est Positive real number on same scale as ref, the estimated stock level (e.g. mean current index level)
#' @param ref Positive real number on same scale as est, a reference level of stock level (e.g. index level at BMSY)
#' @param CP Vector of real numbers, 2 positions long (c(Lx, Ux)), the lower and upper control points of a hockey stick HCR on the xaxis (est/ref). Below Lx (est/ref < Lx) the TAC is trial_TAC x Ly. Above Ux (est/ref > Ux) the TAC is trial_TAC x Uy. Between the TAC is linearly ramped between these levels.  
#' @param CPy Vector of real numbers, 2 positions long (c(Ly, Uy)), the lower and upper control points of a hockey stick HCR on the yaxis (fraction of trial_TAC).   
#' @return A real number (TAC advice but theoretically could be used for effort, size limits etc).
#' @author T. Carruthers
#' @export
doHCR = function(trial_TAC, est, ref, CP = c(0,1), CPy = c(0,1)){
  lev = est / ref
  if(lev <= CP[1]) TAC = trial_TAC*CPy[1]
  if(lev >= CP[2]) TAC = trial_TAC*CPy[2]
  if(lev > CP[1] & lev < CP[2]) TAC = (trial_TAC*CPy[1])+trial_TAC * (CPy[2]-CPy[1])*(lev - CP[1]) / (CP[2] - CP[1])
  TAC
}


#' A flexible empirical management procedure. 
#'
#'
#' @description An all-purpose empirical MP that runs of Indices of relative abundance
#' @param x Positive integer, the simulation number (a position in data object Data)
#' @param Data An object of class 'Data' containing all fishery data (simulated or real - real has only one 'simulation')
#' @param reps Positive integer, the number of stochastic samples of management advice (not applicable here)
#' @param Inds Vector of positive integers. The indices (dimension 2) of the Additional Indices Data@AddInd to be used in calculation. When this is NA, the single index Data@Ind is used
#' @param I_freq Vector of positive integers. Same length as Inds - how frequently will each index be available. 1 is every year, 2 is every 2 years, etc. 
#' @param I_wt Vector of positive real numbers. Same lengtt as Inds - the weighting of each index in the calculation of mean index level. 
#' @param calib_yrs Positive integer. The number of recent historical years used to calculate the 'current' Catch per Index value (more or less a nuisance parameter)
#' @param enp_mult Fraction. The degree of smoothing for the polynomial function of indices. Larger numbers mean more smoothing. This is effective number of parameters. 0.3 means that the number of parameters in the polynomial smoother is 30% the length of the index.  
#' @param Ind_fac Positive real number. The factor (multiplier) of current catch(calib_yrs) / index(calib_yrs) to fish at in the future. A value of 2 means that per index the catches will be twice as high as today. If NA, the fraction of  defaults to perfectly known mean((0.75 * FMSY)/last_historical_F) - mean over simulations. 
#' @param TACrng Vector 2 positions long, the minimum and maximum allowable catches. If NA this defaults to c(0, max_historical_catch*100) - essentially no TAC limit. 
#' @param delta_down Vector 2 positions long, the minimum and maximum allowable fractional downward change in TAC among management cycles. 
#' @param delta_up Vector 2 positions long, the minimum and maximum allowable fractional upward change in TAC among management cycles. 
#' @param resp Positive real number, the responsiveness of the TAC change algorithm. TAC_change = exp(log(new_TAC/old_TAC)*resp). Lower values linearly reduce the logspace TAC response and make smaller adjustements as proposed TAC changes are larger). 
#' @param curI_2_target Positive real number, the current (most recent historical year) index relative that at the target biomass level. If NA this defaults to perfectly known mean(last_historical_biomass / (1.25 * BMSY)), mean over all simulations.  
#' @param HCR_CP_B Vector of positive real numbers. Biomass control points of an HCR. These are the x-axis locations of the hockey stick inflection points. c(0,1) means a linear ramp from I/I_target. c(0.5,1) means no fishing til half I_target then a linear ramp in fishing to I_target. c(0,0) means no HCR. 
#' @param HCR_CP_TAC Vector of positive real numbers. Response control points of an HCR. These are the y-levels corresponding with the hockey stick. These are the minimum and maximum modifiers applied to the TAC recommendation. 
#' @param Mode Integer. What type of index-based MP is used? 1 = Index rate, aims to fish at a rate of index (ie TAC = f(I, current_C / current_I, Ind_fac, HCR_CP_B, HCR_CP_TAC)), 2 = Index target, makes incremental TAC adjustments based on I/I_target (i.e. TAC = f(I, curI_2_target, ))
#' @return An object of class `MP`.
#' @author T. Carruthers
#' @export
Emp <- function (x, Data, reps = 1, 
                      Inds = NA,               # use these indices  (6) Survey_spring, (5) Survey_Aut, (3) UKR_fleet,  (2) ROM_Trawl, (1) TUR_East_Trawl
                      I_freq=NA,               # how often are the data collected n the future (0 = never, 1 = every year, 2 = every 2 years, etc)
                      I_wt = NA,               # index weighting in calculation
                      calib_yrs = 2,           # calibration years (which historical years are used as the reference for catch / index)
                      enp_mult = 0.3,          # effective number of parameters multiplier for polynomial smoother - this keeps smoothing consistent with length of time series
                      Ind_fac = NA,            # TAC is this constant factor of the index
                      TACrng = NA,             # essentially no max TAC
                      delta_down = c(0.01,0.5),# between 0 and 50% downward TAC change 
                      delta_up = c(0.01,0.5),  # between 0 and 50% upward TAC change 
                      resp = 1,                # responsiveness linear to index change
                      curI_2_target = NA,      # what is current stock depletion relative to target?
                      HCR_CP_B = c(0,0),       # increase from zero FMSY to MSY frac from position 1 to position 2. c(0,0) is a constant F policy at MSY frac
                      HCR_CP_TAC=c(0,1),       # increase from zero FMSY to MSY frac from position 1 to position 2. c(0,0) is a constant F policy at MSY frac
                      Mode = 1                 # 1 is rate, 2 is target level
                 ){     
  # x=readRDS("C:/temp/x.rds"); Data = readRDS("C:/temp/Data.rds"); reps = 1; Inds = NA; I_freq=NA; I_wt = NA; calib_yrs = 2; enp_mult = 0.3; Ind_fac = NA; TACrng = NA; delta_down = c(0.01,0.5); delta_up = c(0.01,0.5); resp = 1; curI_2_target = NA;  HCR_CP_B = c(0,0); HCR_CP_TAC=c(0,1); Mode = 1
  #saveRDS(Data, "C:/temp/Data.rds")# ;stop()
  #saveRDS(x, "C:/temp/x.rds")
  
  dependencies = "Data@Cat, Data@AddInd, Data@Ind, Data@Misc"
  MPrec  = Data@MPrec[x] # last management recommendation
  ny = length(Data@Cat[x, ])
  ystart <- which(!is.na(Data@Cat[x, ]))[1]
  yind <- ystart:ny
  LHYr = Data@LHYear
  LHYrInd = match(LHYr, Data@Year)
  CurYr = max(Data@Year)
  Year <- Data@Year[yind]
  C_hist <- Data@Cat[x, yind]


  if(is.na(Inds[1])){ # user does not specify AddInd indices
    I_hist <-array(Data@Ind[x,yind],c(1,ny))
  }else{
    nIndices <- dim(Data@AddInd)[2]
    Inds <- Inds[Inds %in% 1:nIndices]
    I_hist <- array(Data@AddInd[x,Inds, yind],c(length(Inds),ny))
  } 
  
  nI = nrow(I_hist)
  if(is.na(I_freq[1])) I_freq = rep(1, nI) # defaults to sampling every year
  if(is.na(I_wt[1])) I_wt = rep(1, nI)     # defaults to even weighting of indices

  I_wt <- I_wt[1:nI] # in case provided I_wt is incorrect length
  
  if(is.na(Ind_fac)){ # user does not specify a rate of C/I relative to today
    FMSY = Data@Misc$ReferencePoints$ByYear$FMSY[,LHYrInd]
    FM = Data@Misc$FleetPars$Find[, LHYrInd] * Data@Misc$FleetPars$qs
    Ind_fac = mean(FMSY/FM) * 0.75 # to get to 75% FMSY fishing (PGY)
  }
  
  if(is.na(curI_2_target)){ # user does not specify a target level of Index 
    Dep = Data@Misc$StockPars$Depletion
    Brel = Data@Misc$StockPars$BMSY_B0
    curI_2_target = mean(Dep)/mean(Brel * 1.25)  # to get to 125% BMSY 
  }
  
  if(is.na(TACrng))TACrng = c(0, 100*max(Data@Cat)) #essentially no limit on catches
  
  I_keep = doIfreq(I_hist, I_freq, LHYr, CurYr, Year) # index sample frequency
  caliby = LHYrInd-(calib_yrs-1):0
  calibmuI = apply(I_keep[,caliby,drop=F],1,mean,na.rm=T)
  ref = mean(calibmuI/curI_2_target, na.rm=T)
  calibmuC = mean(C_hist[caliby],na.rm=T)
  C_I = calibmuC / calibmuI # historical catch per index (average over calib_yrs)
  
  nkeep = sum(I_freq!=0)
  I_smth = array(NA,c(nkeep,ny)) # smoothed index
  for(j in 1: nkeep)  I_smth[j,] = smoothy(I_keep[j,], enp_mult = enp_mult) #, plot=x==1)
  est = weighted.mean(I_smth[,ny], I_wt, na.rm=T)
  
  if(Mode==1){ # Index rate MP
    if (all(is.na(I_smth[,ny]))) 
      cli::cli_abort(c('All index values are NA in the terminal year'))
      
    TACbyI = I_smth[,ny] * C_I
    TACtemp = mean(TACbyI,na.rm=T) * Ind_fac
    if(is.na(TACtemp)|is.null(TACtemp))TACtemp = Data@MPrec[x]
    trial_TAC = TACfilter(TACtemp)
    HCRadj_TAC = doHCR(trial_TAC, est = est, ref = ref, CP = HCR_CP_B, CPy = HCR_CP_TAC)
    mod = exp(log(HCRadj_TAC/MPrec)*resp)        # implied TAC change
  } else {  # Index target MP
    mod = exp(log(est / ref)*resp) 
  }

  
  Rec = doRec(MPrec, mod, delta_down, delta_up, TACrng)
  Rec 
  
}

class(Emp)="MP"

