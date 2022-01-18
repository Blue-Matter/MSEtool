#' Reads bootstrap estimates from a stock assessment model (including VPA) into an operating model. \code{Assess2OM}
#' is identical to \code{VPA2OM}.
#'
#'
#' @description A function that uses a set of bootstrap estimates of numbers-at-age, fishing mortality rate-at-age, M-at-age,
#' weight-at-age, length-at-age and Maturity-at-age to define a fully described MSEtool operating model. The user still
#' needs to parameterize most of the observation and implementation portions of the operating model.
#' @param Name Character string. The name of the operating model.
#' @param proyears Positive integer. The number of projection years for MSE.
#' @param interval Positive integer. The interval at which management procedures will update the management advice in \link[MSEtool]{runMSE}, e.g., 1 = annual updates.
#' @param CurrentYr Positive integer. The current year (final year of fitting to data)
#' @param h The steepness of the stock-recruitment curve (greater than 0.2 and less than 1, assumed to be close to 1 to match VPA assumption).
#' Either a single numeric or a length nsim vector.
#' @param Obs The observation model (class Obs). This function only updates the catch and index observation error.
#' @param Imp The implementation model (class Imp). This function does not update implementation parameters.
#' @param naa Numeric array `[sim, ages, year]`. Numbers-at-age `[first age is age zero]`.
#' @param faa Numeric array `[sim, ages, year]`. Fishing mortality rate-at-age `[first age is age zero]`.
#' @param waa Numeric array `[sim, ages, year]`. Weight-at-age `[first age is age zero]`.
#' @param Mataa Numeric array `[sim, ages, year]`. Maturity (spawning fraction)-at-age `[first age is age zero]`.
#' @param Maa Numeric array `[sim, ages, year]`. Natural mortality rate-at-age `[first age is age zero]`.
#' @param laa Numeric array `[sim, ages, year]`. Length-at-age `[first age is age zero]`.
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
#' @aliases VPA2OM
#' @export
#' @seealso \link{SS2OM}
Assess2OM <- function(Name="A fishery made by VPA2OM",
                      proyears=50, interval=2, CurrentYr=as.numeric(format(Sys.Date(), "%Y")),
                      h=0.999,
                      Obs = MSEtool::Imprecise_Unbiased, Imp=MSEtool::Perfect_Imp,
                      naa, faa, waa, Mataa, Maa, laa,
                      nyr_par_mu = 3, LowerTri=1,
                      recind=0, plusgroup=TRUE, altinit=0, fixq1=TRUE,
                      report=FALSE, silent=FALSE, ...) {

  simup<-function(param, OM, do_slot = TRUE){   # Generic function for converting VPA outputs to an OM
    paramnam<-deparse(substitute(param))
    if(length(param)==1){
      OM@cpars[[paramnam]]<-rep(param,nsim)
    }else if(length(param)<nsim){
      stop(paste('parameter vector', paramnam, 'was length',length(param),'which is neither length 1 or nsim long'))
    }else{
      OM@cpars[[paramnam]]<-param
    }
    if(do_slot) slot(OM,paramnam)<-rep(param[1],2)
    OM
  }
  
  dots <- list(...) #R0, phi0

  cond <- vapply(list(faa, waa, Mataa, Maa, laa), function(x) all(dim(naa) == dim(x)), logical(1))
  if(!length(cond) || any(is.na(cond)) || any(!cond)) {
    stop('One or more of the following arrays do not have the same shape: naa, faa, waa, Mataa, Maa, Laa')
  }

  if(recind==1) {  # create a dummy age 0 dimension to the various arrays
    
    ageind<-1:dim(naa)[2]
    dims<-c(dim(naa)[1],1,dim(naa)[3])
    zeros<-array(0,dims)
    
    # N0 back inputed
    N0<-array(naa[,1,]*exp(Maa[,1,]),dims)
    naa<-abind(N0,naa,along=2) 
    
    # F, weight, length and maturity assumed to be zero
    faa<-abind(zeros,faa,along=2) 
    waa<-abind(zeros,waa,along=2)
    Mataa<-abind(zeros,Mataa,along=2)
    laa<-abind(zeros,laa,along=2)
    
    # M copied from first year to age zero
    Maa<-abind(Maa[,1,],Maa,along=2)
    
    message("Age zero positions for arrays were created with the following assumptions: N(0) = N(1) * exp(M(1)), F(0) = weight(0) = maturity(0) = length(0) = 0, M(0) = M(1)")
    
  }

  # Dimensions
  nsim<-dim(naa)[1]
  n_age <- dim(naa)[2]
  maxage<-dim(naa)[2] - 1
  nyears<-dim(naa)[3]

  # Mine values to spec-out the OM
  if(!is.null(dots$R0)) {
    if(length(dots$R0) == 1) {
      R0 <- rep(dots$R0, nsim)
    } else {
      R0 <- dots$R0
    }
  } else {
    R0 <- naa[,1,1] # R0 is mean numbers at age 0
  }
  if(length(h) == 1) h <- rep(h, nsim)

  OM<-new('OM')
  
  surv <- array(1, dim(Maa))
  surv[, -1, ] <- exp(-apply(Maa[, -n_age, ], c(1,3), cumsum)) %>% aperm(c(2,1,3))
  if(plusgroup) {
    surv[, n_age, ] <- surv[, n_age, ]/(1 - exp(-Maa[, n_age, ]))
  }
  SSBpR <- apply(surv * Mataa * waa, c(1, 3), sum)
  
  SSBpR_out <- vapply(1:nsim, function(x) {
    ageM <- min(LinInterp(Mataa[x,, 1], y = 1:n_age, 0.5), maxage - 1)
    SSBpR[x, 1:(ceiling(ageM) + 1)] %>% mean()
  }, numeric(1))
  
  # Recalculate R0 and h based on phi0 to match openMSE
  if(!is.null(dots$phi0)) {
    new_SR <- local({ # BH only
      if(length(dots$phi0) == 1) {
        phi0 <- rep(dots$phi0, nsim)
      } else {
        phi0 <- dots$phi0
      }
      Arec <- 4*h/(1-h)/phi0
      Brec <- (5*h-1)/(1-h)/R0/phi0
      
      K <- Arec * SSBpR_out
      h_out <- K/(4 + K)
      R0_out <- (5*h_out-1)/(1-h_out)/Brec/SSBpR_out
      list(h = h_out, R0 = R0_out)
    })
    R0 <- new_SR$R0
    h <- new_SR$h
  }
  SSB0 <- R0 * SSBpR_out
  
  SSB <- apply(naa*Mataa*waa, c(1, 3), sum, na.rm = TRUE)
  D <- SSB[,nyears]/SSB0

  OM@Name <- Name
  OM@M<-OM@L50<-OM@L50_95<-OM@a<-OM@b<-OM@L5<-OM@LFS<-OM@Vmaxlen <-c(1,1)

  # Dimensions
  OM@nsim<-nsim
  OM@nyears<-nyears
  OM@maxage<-maxage
  OM@proyears<-proyears
  OM@interval<-interval
  
  OM <- simup(D, OM)
  hs <- h
  OM <- simup(hs, OM, do_slot = FALSE)
  OM@h <- rep(hs[1], 2)
  OM <- simup(R0, OM)
  OM@Size_area_1 <- OM@Frac_area_1 <- OM@Prob_staying <- rep(0.5, 2)
  
  # 'Contant terms'
  LenCV=0.1
  OM<-simup(LenCV,OM)
  OM@SRrel <- 1 # 1 = BevHolt, 2 = Ricker
  OM@isRel<-FALSE  # absolute selectivity relative to maturity - no used here
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
  OM<-suppressMessages(Replace(OM, Obs, Sub = "Obs"))
  OM<-suppressMessages(Replace(OM, Imp, Sub = "Imp"))

  # Custom parameter arrays
  Wt_age <- M_ageArray <- Len_age <- Mat_age <- V <- array(NA, c(nsim, n_age, nyears + proyears))

  # Historical filling
  Find<-apply(faa,c(1,3),max)
  Wt_age[,,1:nyears]<-waa
  M_ageArray[,,1:nyears]<-Maa
  Len_age[,,1:nyears]<-laa
  Mat_age[,,1:nyears]<-Mataa
  Fmax<-aperm(array(rep(Find,n_age),c(nsim,nyears,n_age)),c(1,3,2))
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
  OM@cpars$Find <- Find

  # Deterministic Recruitment
  recd <- vapply(1:nsim, function(x) (0.8*R0[x]*h[x]*SSB[x, ])/(0.2*SSB0[x]*(1-h[x]) + (h[x]-0.2)*SSB[x, ]), 
                 numeric(nyears)) %>% t()

  recdevs <- log(naa[, 1, ]/recd) # age zero
  recdevs[is.na(recdevs)] <- 0
  
  if(!is.null(dots$Perr)) {
    if(length(dots$Perr) == 1) {
      procsd <- rep(dots$Perr, nsim)
    } else {
      procsd <- dots$Perr
    }
  } else {
    procsd <- apply(recdevs, 1, sd)
  }
  
  if(!is.null(dots$AC)) {
    if(length(dots$AC) == 1) {
      AC <- rep(dots$AC, nsim)
    } else {
      AC <- dots$AC
    }
  } else {
    AC <- apply(recdevs, 1, function(x) stats::acf(x, plot = FALSE)$acf[2,1,1])
  }

  # Initial naa initialization options (VPAs apparently do this differently...)
  Perr <- array(NA_real_, c(nsim, maxage + nyears - LowerTri))
  if(altinit < 2) {       # normal assumption with or without plusgroup
    Perr[, n_age:1] <- log(naa[, , 1]/(R0 * surv[, , 1]))
  } else if(altinit==2) { # temporary fix for DLMtool initialization of plusgroup
    Perr[, n_age:2] <- log(naa[, 1:(n_age-1), 1]/(R0 * surv[, 1:(n_age-1), 1]))
    survDLMtool<-aperm(exp(-apply(Maa[,,1],1,cumsum)),c(2,1))
    fac<-surv[, n_age, 1]/survDLMtool[, n_age]
    Perr[, 1] <- log(naa[, n_age, 1]/(R0 * surv[, n_age, 1] * fac))
  }
  Perr[, maxage + 2:(nyears - LowerTri)] <- recdevs[, 2:(nyears - LowerTri)]
  Perr_pro <- sample_recruitment(Perr_hist = Perr, proyears = proyears + LowerTri, procsd = procsd, AC = AC)
  
  OM@cpars$Perr_y <- exp(cbind(Perr, Perr_pro))
  OM@Perr <- rep(mean(procsd),2)
  OM@AC <- rep(mean(AC), 2)

  if(fixq1) OM@cpars$qs <- rep(1, nsim) # Overrides q estimation to fix q at 1 for VPA for which F history is

  OM@maxF <- ceiling(max(Find))

  OM@Linf <- rep(1,2)
  OM@K <- rep(0.3,2)
  OM@t0 <- rep(0,2)
  OM@DR <- rep(0,2)
  OM@MPA<-FALSE
  
  if(!plusgroup) OM@cpars$plusgroup <- 0L

  if(report){ # Produce a quick diagnostic plot of OM vs VPA numbers at age

    if(!silent) message("\nRunning historical simulations to compare VPA output and OM conditioning...\n")

    Hist <- runMSE(OM, Hist = TRUE, silent = TRUE)
    
    nc<-ceiling(maxage/3)
    nr<-ceiling(maxage/nc)
    
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(mfrow=c(nr,nc),mai=c(0.4,0.4,0.3,0.05),omi=c(0.25,0.25,0.01,0.01))

    yrs<-CurrentYr-((nyears-1):0)

    cols<-rep('black',nyears)
    pch<-rep(1,nyears)
    cols[nyears-(0:(LowerTri-1))]<-"blue"
    pch[nyears-(0:(LowerTri-1))]<-4

    for(a in 0:maxage){
      N_OM<-apply(Hist@AtAge$Number[1,a+1,,], 1, sum)
      ylim=c(0,max(naa[1,a+1,],N_OM, na.rm = TRUE) * 1.05)
      if(a==0) plot(yrs,naa[1,a+1,],xlab="",ylab="",col=cols,pch=pch,ylim=ylim,yaxs='i')
      if(a>0) plot(yrs,naa[1,a+1,],xlab="",ylab="",ylim=ylim,yaxs='i')
      lines(yrs,N_OM,col='green')
      mtext(paste("Age ",a),3,line=0.5,cex=0.9)
      if(a==0)legend('top',legend=c("Assessment","OM","Recr. ignored (LowerTri)"),text.col=c('black','green','blue'),bty='n')
      res<-N_OM-naa[1,a+1,]
      plotres<- abs(res) > mean(naa[1,a+1,]*0.025, na.rm = TRUE)
      if(any(plotres, na.rm = TRUE)) {
        for(y in 1:nyears) {
          if(!is.na(plotres[y]) && plotres[y]) lines(rep(yrs[y], 2), c(naa[1,a+1,y],N_OM[y]),col='red')
        }
      }
    }

    mtext("Year",1,line=0.3,outer=T)
    mtext("Numbers",2,line=0.4,outer=T)

  }

  return(OM)

}

#' @rdname Assess2OM
#' @export
VPA2OM <- Assess2OM

