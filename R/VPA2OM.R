# =======================================================================================================================================
# === VPA2OM ============================================================================================================================
# =======================================================================================================================================

# T. Carruthers
# A function for converting stochastic (bootstrap VPA) outputs to a DLMtool / MSEtool operating model

#' Reads bootstrap estimates from a VPA stock assessment into an operating model.
#'
#'
#' @description A function that uses a set of VPA bootstrap estimates of numbers-at-age, fishing mortality rate-at-age, M-at-age,
#' weight-at-age, length-at-age and Maturity-at-age to define a fully described DLMtool / MSEtool operating model. The user still
#' needs to parameterize most of the observation and implementation portions of the operating model.
#' @param Name Character string. The name of the operating model.
#' @param proyears Positive integer. The number of projection years for MSE.
#' @param interval Positive integer. The interval at which management procedures will update the management advice in \link[DLMtool]{runMSE}, e.g., 1 = annual updates.
#' @param CurrentYr Positive integer. The current year (final year of VPA fitting to data)
#' @param h Numeric value greater than 0.2 and less than 1. The steepness of the stock-recruitment curve (assumed to be close to 1 to match VPA assumption).
#' @param Obs The observation model (class Obs). This function only updates the catch and index observation error.
#' @param Imp The implementation model (class Imp). This function does not update implementation parameters.
#' @param naa Numeric array [sim, ages, year]. Numbers-at-age [first age is age zero].
#' @param faa Numeric array [sim, ages, year]. Fishing mortality rate-at-age [first age is age zero].
#' @param waa Numeric array [sim, ages, year]. Weight-at-age [first age is age zero].
#' @param Mataa Numeric array [sim, ages, year]. Maturity (spawning fraction)-at-age [first age is age zero].
#' @param Maa Numeric array [sim, ages, year]. Natural mortality rate-at-age [first age is age zero].
#' @param laa Numeric array [sim, ages, year]. Length-at-age [first age is age zero].
#' @param nyr_par_mu Positive integer. The number of recent years that natural mortality, age vulnerability, weight, length and maturity parameters are averaged over for defining future projection conditions.
#' @param LowerTri Integer. The number of recent years for which model estimates of recruitment are ignored (not reliably estimated by the VPA)
#' @param recind Positive integer. The first age class that fish 'recruit to the fishery'. The default is 2 - ie the first position in the age dimension of naa is age zero
#' @param plusgroup Logical. Does the VPA assume that the oldes age class is a plusgroup?
#' @param altinit Integer. Various assumptions for how VPAs set up the initial numbers. 0: standard, 1: no plus group, 2: temporary fix for DLMtool plus group initialization
#' @param fixq1 Logical. Should q be fixed (ie assume the F-at-age array faa is accurate?
#' @param report Logical, if TRUE, a diagnostic will be reported showing the matching of the OM reconstructed numbers at age vs the VPA assessment.
#' @param silent Whether to silence messages to the console.
#' @return An object of class \linkS4class{OM}.
#' @author T. Carruthers
#' @export
#' @seealso \link{SS2OM}

VPA2OM<-function(Name="A fishery made by VPA2OM",
                 proyears=50, interval=2, CurrentYr=2019,
                 h=0.999,
                 Obs = DLMtool::Imprecise_Unbiased, Imp=DLMtool::Perfect_Imp,
                 naa, faa, waa, Mataa, Maa, laa,
                 nyr_par_mu = 3, LowerTri=1,
                 recind=2, plusgroup=TRUE, altinit=0, fixq1=TRUE,
                 report=FALSE, silent=FALSE

){

  # Name=""; proyears=50; interval=2; CurrentYr=2019; h=0.999; Obs = Imprecise_Unbiased; Imp=Perfect_Imp; naa=pack$naa; faa=pack$faa; waa=pack$waa;
  # Mataa=pack$Mataa; Maa=pack$Maa; laa=pack$laa; nyr_par_mu = 3; LowerTri=1;  recind=2; plusgroup=T; altinit=1; fixq1=T; report=T; silent=T
  simup<-function(param,OM){   # Generic function for converting VPA outputs to an OM
    paramnam<-deparse(substitute(param))
    if(length(param)==1){
      OM@cpars[[paramnam]]<-rep(param,nsim)
    }else if(length(param)<nsim){
      stop(paste('parameter vector', paramnam, 'was length',length(param),'which is neither length 1 or nsim long'))
    }else{
      OM@cpars[[paramnam]]<-param
    }
    slot(OM,paramnam)<-rep(param[1],2)
    OM
  }

  cond<-dim(naa)!=dim(faa) | dim(naa)!=dim(waa) | dim(naa)!=dim(Mataa) | dim(naa)!=dim(Maa) | dim(naa)!=dim(laa)
  if(is.na(cond[1])|length(cond)==0|any(cond)) stop('One or more of the following arrays do not have the same shape: naa, faa, waa, Mataa, Maa, Laa')

  if(recind!=1){
    ageind<-recind:dim(naa)[2]
    naa<-naa[,ageind,]
    faa<-faa[,ageind,]
    waa<-waa[,ageind,]
    Mataa<-Mataa[,ageind,]
    Maa<-Maa[,ageind,]
    laa<-laa[,ageind,]
  }

  # Dimensions
  nsim<-dim(naa)[1]
  maxage<-dim(naa)[2]
  nyears<-dim(naa)[3]

  # Mine values to spec-out the OM
  R0 <- naa[,1,1] # R0 is mean numbers at age 0

  # You were here - M age 1 is zero, so surv should be your current c(1,surv[1:4],plusgroup[5])
  surv<-aperm(exp(-apply(Maa,c(1,3),cumsum)),c(2,1,3))

  OM<-new('OM')
  OM@cpars<-list()
  if(plusgroup){
    surv[,maxage,]<-surv[,maxage,]+surv[,maxage,]*exp(-Maa[,maxage,])/(1-exp(-Maa[,maxage,])) # indefinite integral
    OM@cpars$plusgroup <- rep(1, nsim)
  }

  SSB<-naa*Mataa*waa
  SSBd<-apply(SSB,c(1,3),sum) # matplot(t(SSBd),type='l',yaxs='n')
  maxind<-min(nyears,max(2,min((1:maxage)[Mataa[1,,1]>0.5])))
  avg.ind<-1:maxind
  SSB0<-apply(SSB[,,avg.ind],1,sum)/length(avg.ind) # as calculated in DLMtool
  SSBpR<-SSB0/R0

  D<-apply(SSB[,,nyears],1,sum)/SSB0

  OM@Name <- Name
  OM@M<-OM@L50<-OM@L50_95<-OM@a<-OM@b<-OM@L5<-OM@LFS<-OM@Vmaxlen <-c(1,1)

  # Dimensions
  OM@nsim<-nsim
  OM@nyears<-nyears
  OM@maxage<-maxage
  OM@proyears<-proyears
  OM@interval<-interval

  # Sampled parameters

  OM<-simup(h,OM);
  Size_area_1<-Frac_area_1<-Prob_staying<-0.5
  OM<-simup(Size_area_1,OM); OM<-simup(Frac_area_1,OM); OM<-simup(Prob_staying,OM)
  OM<-simup(D,OM); OM<-simup(R0,OM)

  # 'Contant terms'
  LenCV=0.1
  OM<-simup(LenCV,OM)
  OM@SRrel <- 1 # 1 = BevHolt, 2 = Ricker
  OM@isRel<-F  # absolute selectivity relative to maturity - no used here
  OM@CurrentYr<-CurrentYr

  # Time varying terms
  OM@Msd<-OM@Ksd<-OM@Linfsd<-OM@qcv<-OM@Esd<-OM@AC<-rep(0,2)

  # Closed loop MP terms
  OM@pstar<-0.5
  OM@reps<-1

  # dummy - filled to get past checks but not used due to cpars matrices in VPA
  OM@EffYears<-c(1,5,OM@nyears)
  OM@EffLower<-rep(1,3)
  OM@EffUpper<-rep(1.1,3)

  # Niche terms (assign defaults)
  OM@Spat_targ<-c(1,1)
  OM@qinc<-rep(0,2)

  # Invent an OM with full observation error model for replacing
  temp<-new('OM', DLMtool::Albacore, DLMtool::Generic_Fleet, Obs, Imp)
  OM<-Replace(OM,temp,Sub="Obs")
  OM<-Replace(OM,temp,Sub="Imp")

  # Custom parameter arrays
  Wt_age <- M_ageArray <- Len_age <- Mat_age <- V <- array(NA,c(nsim,maxage,nyears+proyears))

  # Historical filling
  Find<-apply(faa,c(1,3),max)
  Wt_age[,,1:nyears]<-waa
  M_ageArray[,,1:nyears]<-Maa
  Len_age[,,1:nyears]<-laa
  Mat_age[,,1:nyears]<-Mataa
  Fmax<-aperm(array(rep(Find,maxage),c(nsim,nyears,maxage)),c(1,3,2))
  V[,,1:nyears]<-faa/Fmax

  # Future filling
  parmu<-function(arr,nyears,proyears,nyr_par_mu){ # function for calculation mean values over last nyr_par_mu years
    arr[,,nyears+(1:proyears)]<-array(rep(apply(arr[,,nyears-(0:(nyr_par_mu-1))],1:2,mean),proyears),c(dim(arr)[1:2],proyears))
    arr
  }

  # Projected arrays based on mean of last nyr_par_mu years
  Wt_age<-parmu(Wt_age,nyears,proyears,nyr_par_mu)
  M_ageArray<-parmu(M_ageArray,nyears,proyears,nyr_par_mu)
  Len_age<-parmu(Len_age,nyears,proyears,nyr_par_mu)
  Mat_age<-parmu(Mat_age,nyears,proyears,nyr_par_mu)
  V<-parmu(V,nyears,proyears,nyr_par_mu)

  OM@cpars$Wt_age <- Wt_age
  OM@cpars$M_ageArray <- M_ageArray
  OM@cpars$Len_age <- Len_age
  OM@cpars$Mat_age <- Mat_age
  OM@cpars$V <- V
  OM@cpars$Find<-Find
  OM@cpars$R0<-R0

  # Deterministic Recruitment
  recd<-(0.8*R0*h*SSBd) / (0.2*SSBpR*R0*(1-h) + (h-0.2)*SSBd)

  recdevs<-log(naa[,1,]/recd[,]) # age zero
  procsd<-apply(recdevs,1,sd)
  procmu <- -0.5 * (procsd)^2  # adjusted log normal mean
  AC<-apply(recdevs,1,function(x)stats::acf(x,plot=F)$acf[2,1,1])

  Perr<-array(NA,c(nsim,nyears+proyears+maxage-1))
  Perr<-matrix(rnorm(nsim*(maxage+nyears+proyears-1),rep(procmu,maxage+nyears+proyears-1),rep(procsd,maxage+nyears+proyears-1)),nrow=nsim) # fill grid with uncorrelated devs
  Perr[,maxage+(0:(nyears-1))]<-recdevs

  # Initial naa initialization options (VPAs apparently do this differently...)
  if(altinit==0){       # normal assumption
    Perr[,(maxage-1):1] <- log(naa[,2:maxage,1]/(R0*surv[,2:maxage,1]))
  }else if(altinit==1){ # initial conditions calculated without plus group
    Perr[,(maxage-1):1] <- log(naa[,2:maxage,1]/(R0*surv[,1:(maxage-1),1]))
  }else if(altinit==2){ # temporary fix for DLMtool initialization of plusgroup

    Perr[,(maxage-1):2] <- log(naa[,2:(maxage-1),1]/(R0*surv[,1:(maxage-2),1]))
    survDLMtool<-aperm(exp(-apply(Maa[,,1],1,cumsum)),c(2,1))
    fac<-surv[,maxage,1]/survDLMtool[,maxage]
    Perr[,1] <- log(naa[,maxage,1]/(R0*surv[,maxage-1,1]*fac))

  }

  Perr[,maxage+(0:(nyears-LowerTri-1))]<- recdevs[,1:(nyears-LowerTri)]
  for (y in maxage+((nyears-LowerTri):(nyears + proyears))-1) Perr[, y] <- AC * Perr[, y - 1] +   Perr[, y] * (1 - AC * AC)^0.5  # apply process error
  Perr<-exp(Perr)

  OM@cpars$Perr<-Perr
  OM@Perr<-rep(mean(procsd),2)

  if(fixq1) OM@cpars$qs<-rep(1,nsim) # Overrides q estimation to fix q at 1 for VPA for which F history is

  OM@maxF<-10

  if(report){ # Produce a quick diagnostic plot of OM vs VPA numbers at age

    if(!silent) message("\nRunning historical simulations to compare VPA output and OM conditioning...\n")

    Hist <- runMSE(OM, Hist = TRUE)

    nc<-ceiling(maxage/3)
    nr<-ceiling(maxage/nc)
    par(mfrow=c(nr,nc),mai=c(0.4,0.4,0.3,0.05),omi=c(0.25,0.25,0.01,0.01))

    yrs<-CurrentYr-((nyears-1):0)

    cols<-rep('black',nyears)
    pch<-rep(1,nyears)
    cols[nyears-(0:LowerTri)]<-"blue";pch[nyears-(0:LowerTri)]<-4

    for(a in 1:maxage){
      ylim=c(0,max(naa[1,a,],Hist@AtAge$Nage[1,a,])*1.05)
      if(a==1)plot(yrs,naa[1,a,],xlab="",ylab="",col=cols,pch=pch,ylim=ylim,yaxs='i')
      if(a>1)plot(yrs,naa[1,a,],xlab="",ylab="",ylim=ylim,yaxs='i')
      lines(yrs,Hist@AtAge$Nage[1,a,],col='green')
      mtext(paste("Age ",a),3,line=0.5,cex=0.9)
      if(a==1)legend('top',legend=c("Assessment","OM","Recr. ignored (LowerTri)"),text.col=c('black','green','blue'),bty='n')
      res<-Hist@AtAge$Nage[1,a,]-naa[1,a,]
      plotres<-abs(res)>(mean(naa[1,a,]*0.025))
      if(any(plotres))for(y in 1:nyears)if(plotres[y])lines(rep(yrs[y],2),c(naa[1,a,y],Hist@AtAge$Nage[1,a,y]),col='red')
    } #plot(Hist)

    mtext("Year",1,line=0.3,outer=T)
    mtext("Numbers",2,line=0.4,outer=T)

  }

  return(OM)

}
