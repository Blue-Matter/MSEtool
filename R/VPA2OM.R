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
#' @param ... Additional arguments (for all, either a numeric or a length nsim vector):
#' \itemize{
#' \item \code{fecaa} Fecundity at age. Default fecundity is the product of maturity and weight at age.
#' \item \code{SRrel} Stock-recruit relationship. (\code{1} for Beverton-Holt (default), \code{2} for Ricker)
#' \item \code{R0} unfished recruitment
#' \item \code{phi0} unfished spawners per recruit associated with R0 and h. With time-varying parameters, openMSE uses the mean phi0
#' in the first \code{ageM} (age of 50 percent maturity) years for the stock-recruit relationship. \code{Assess2OM} will re-calculate R0 and h
#' in the operating model such that the stock-recruit \code{alpha} and \code{beta} parameters match values implied in the input.
#' \item \code{Perr} recruitment standard deviation (lognormal distribution) for sampling future recruitment
#' \item \code{AC} autocorrelation in future recruitment deviates.
#' \item \code{spawn_time_frac} The fraction of a year when spawning takes place (e.g., 0.5 is the midpoint of the year)
#' }
#' @details Use a seed for the random number generator to sample future recruitment.
#' @return An object of class \linkS4class{OM}.
#' @author T. Carruthers
#' @aliases VPA2OM
#' @export
#' @seealso \link{SS2OM} \link{iSCAM2OM} \link{WHAM2OM} \link{ASAP2OM}
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

  if(!is.null(dots$fecaa)) {
    fecaa <- dots$fecaa
    if(!all(dim(naa) == dim(fecaa))) stop("Dimension of fecaa not equal to that for naa")
  } else {
    fecaa <- waa * Mataa
  }

  nyears<-dim(naa)[3]
  nsim<-dim(naa)[1]

  if(recind==1) {  # create a dummy age 0 dimension to the various arrays

    ageind<-1:dim(naa)[2]
    dims<-c(nsim,1,nyears)
    zeros<-array(0,dims)

    # N0 back imputed
    rec<-naa[,1,2:nyears]#*exp(Maa[,1,2:nyears])
    muR0<-apply(rec,1,mean) # mean R0 is assumed for most recent N0 - will be filled anyway using LowerTri argument
    N0<-array(cbind(rec,muR0),dims)
    naa<-abind(N0,naa,along=2)

    # F, weight, length and maturity assumed to be zero
    faa<-abind(zeros,faa,along=2)
    waa<-abind(zeros,waa,along=2)
    Mataa<-abind(zeros,Mataa,along=2)
    laa<-abind(zeros,laa,along=2)
    fecaa<-abind(zeros,fecaa,along=2)

    # M copied from first year to age zero
    Maa<-abind(array(tiny + .Machine$double.eps,dim(Maa[,1,])),Maa,along=2)

    message("Age zero positions for arrays were created with the following assumptions: N(0) = N(1) * exp(M(1)), N0 in most recent year is mean(R0), F(0) = weight(0) = maturity(0) = length(0) = 0, M(0) = M(1)")

  }

  # Dimensions
  n_age <- dim(naa)[2]
  maxage<-dim(naa)[2] - 1

  # Stock-recruit relationship
  if(!is.null(dots$SRrel)) {
    SRrel <- dots$SRrel
  } else {
    SRrel <- 1
  }

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
  
  if(!is.null(dots$spawn_time_frac)) {
    if(lengths(dots$spawn_time_frac) == 1) {
      spawn_time_frac <- rep(dots$spawn_time_frac, nsim)
    } else {
      spawn_time_frac <- dots$spawn_time_frac
    }
  } else {
    spawn_time_frac <- rep(0, nsim)
  }
  
  SBsurv <- lapply(1:nsim, calc_survival,
                   StockPars = list(M_ageArray = Maa, n_age = n_age, spawn_time_frac = spawn_time_frac),
                   plusgroup = plusgroup, inc_spawn_time = TRUE) %>% 
    abind(along = 3) %>% 
    aperm(c(3,1,2))

  SSBpR <- apply(SBsurv * fecaa, c(1, 3), sum)

  SSBpR_out <- vapply(1:nsim, function(x) {
    ageM <- min(LinInterp(Mataa[x,, 1], y = 1:n_age, 0.5), maxage - 1)
    SSBpR[x, 1:(ceiling(ageM) + 1)] %>% mean()
  }, numeric(1))

  # Recalculate R0 and h that uses the replacement line corresponding to 1/SSBpR_out (used by openMSE)
  if(!is.null(dots$phi0)) {
    new_SR <- local({
      if(length(dots$phi0) == 1) {
        phi0 <- rep(dots$phi0, nsim)
      } else {
        phi0 <- dots$phi0
      }

      if(SRrel == 1) {
        Arec <- 4*h/(1-h)/phi0
        Brec <- (5*h-1)/(1-h)/R0/phi0

        K <- Arec * SSBpR_out
        h_out <- K/(4 + K)
        R0_out <- (5*h_out-1)/(1-h_out)/Brec/SSBpR_out
      } else {
        Arec <- (5*h)^1.25/phi0
        Brec <- 1.25 * log(5*h)/R0/phi0

        h_out <- 0.2 * (Arec * SSBpR_out)^0.8
        R0_out <- 1.25 * log(5 * h_out)/Brec/SSBpR_out
      }

      list(h = h_out, R0 = R0_out)
    })
    R0 <- new_SR$R0
    h <- new_SR$h
  }
  SSB0 <- R0 * SSBpR_out

  SSB <- apply(naa * exp(-spawn_time_frac * (Maa + faa)) * fecaa, c(1, 3), sum, na.rm = TRUE)
  D <- SSB[,nyears]/SSB0

  OM@Name <- Name
  OM@M<-OM@L50<-OM@L50_95<-OM@L5<-OM@LFS<-OM@Vmaxlen <-c(1,1)
  OM@a<-OM@b<-1

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
  OM@SRrel <- SRrel # 1 = BevHolt, 2 = Ricker
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
  Wt_age <- M_ageArray <- Len_age <- Mat_age <- V <- Fec_age <- array(NA, c(nsim, n_age, nyears + proyears))

  # Historical filling
  Find<-apply(faa,c(1,3),max)
  Wt_age[,,1:nyears]<-waa
  M_ageArray[,,1:nyears]<-Maa
  Len_age[,,1:nyears]<-laa
  Mat_age[,,1:nyears]<-Mataa
  Fmax <-aperm(array(rep(Find,n_age),c(nsim,nyears,n_age)),c(1,3,2))
  V[,,1:nyears]<-faa/Fmax
  
  adjustV <- function(Vi) {
    nan.ind <- apply(Vi, 2, max) %>% is.nan() %>%  which()
    if (length(nan.ind)>0) {
      non.nan.ind <- max(nan.ind)+1
      Vi[, nan.ind] <- Vi[, non.nan.ind]  
    }
    Vi
  }
  V <- sapply(1:nsim, function(i) adjustV(V[i,,]), simplify = 'array') %>% 
    aperm(c(3,1,2))
  
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

  if(!is.null(dots$fecaa)) {
    Fec_age[,,1:nyears] <- fecaa
    OM@cpars$Fec_age <- parmu(Fec_age,nyears,proyears,nyr_par_mu)
  }

  # Deterministic Recruitment
  if(SRrel == 1) {
    recd <- vapply(1:nsim, function(x) (0.8*R0[x]*h[x]*SSB[x, ])/(0.2*SSB0[x]*(1-h[x]) + (h[x]-0.2)*SSB[x, ]),
                   numeric(nyears)) %>% t()
  } else {
    recd <- local({
      a <- (5 * h)^1.25/SSBpR_out
      b <- 1.25 * log(5 * h)/SSBpR_out/R0
      vapply(1:nsim, function(x) a[x] * SSB[x, ] * exp(-b[x] * SSB[x, ]), numeric(nyears)) %>% t()
    })
  }

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
  surv <- lapply(1:nsim, calc_survival,
                 StockPars = list(M_ageArray = Maa, n_age = n_age),
                 plusgroup = plusgroup, inc_spawn_time = FALSE) %>% 
    abind(along = 3) %>% 
    aperm(c(3,1,2))
  
  Perr <- array(NA_real_, c(nsim, maxage + nyears - LowerTri))
  if(altinit < 2) {       # normal assumption with or without plusgroup
    
    
    Perr[, n_age:1] <- log(naa[, , 1]/(R0 * surv[, , 1]))
    
  } else if(altinit==2) { # temporary fix for DLMtool initialization of plusgroup
    
    Perr[, n_age:2] <- log(naa[, 1:(n_age-1), 1]/(R0 * SBsurv[, 1:(n_age-1), 1]))
    survDLMtool<-aperm(exp(-apply(Maa[,,1],1,cumsum)),c(2,1))
    fac<-surv[, n_age, 1]/survDLMtool[, n_age]
    Perr[, 1] <- log(naa[, n_age, 1]/(R0 * surv[, n_age, 1] * fac))
    
  }
  Perr[, maxage + 2:(nyears - LowerTri)] <- recdevs[, 2:(nyears - LowerTri)]
  Perr_pro <- sample_recruitment(Perr_hist = Perr, proyears = proyears + LowerTri, procsd = procsd, AC = AC)

  OM@cpars$Perr_y <- exp(cbind(Perr, Perr_pro))
  OM@Perr <- rep(mean(procsd),2)
  OM@AC <- rep(mean(AC), 2)
  OM@cpars$AC <- AC
  OM@cpars$Perr <- procsd

  if(fixq1) OM@cpars$qs <- rep(1, nsim) # Overrides q estimation to fix q at 1 for VPA for which F history is

  OM@maxF <- ceiling(max(Find))

  OM@Linf <- rep(1,2)
  OM@K <- rep(0.3,2)
  OM@t0 <- rep(0,2)
  OM@DR <- rep(0,2)
  OM@MPA<-FALSE
  if (any(spawn_time_frac > 0)) OM@cpars$spawn_time_frac <- spawn_time_frac

  if(!plusgroup) OM@cpars$plusgroup <- 0L

  if(report){ # Produce a quick diagnostic plot of OM vs VPA numbers at age

    if(!silent) message("\nRunning historical simulations to compare VPA output and OM conditioning...\n")

    Hist <- runMSE(OM, Hist = TRUE, silent = TRUE)

    nc<-ceiling((maxage+1)/3)
    nr<-ceiling((maxage+1)/nc)

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






#' Reads bootstrap estimates from a stock assessment model into a multi-fleet operating model.
#'
#' @description A function that develops a multiple fleet operating model (\linkS4class{MOM}) and either models a unisex or 2-sex
#' stock from arrays of abundance, fishing mortality, and biological parameters. The user still
#' needs to parameterize most of the observation and implementation portions of the operating model.
#' @param Name Character string. The name of the multi-OM.
#' @param proyears Positive integer. The number of projection years for MSE.
#' @param interval Positive integer. The interval at which management procedures will update the management advice in \link[MSEtool]{multiMSE}, e.g., 1 = annual updates.
#' @param CurrentYr Positive integer. The current year (e.g., final year of fitting to data)
#' @param h The steepness of the stock-recruitment curve. Either a single numeric or a length nsim vector.
#' @param Obs Either a single observation model to be used for all sexes and populations (class \linkS4class{Obs}), or a list where
#' \code{Obs[[f]]} is the \linkS4class{Obs} object for fleet `f` (identical between sexes).
#' @param Imp Either a single implementation model to be used for all sexes and populations (class \linkS4class{Imp}), or a list where
#' \code{Imp[[f]]} is the \linkS4class{Obs} object for fleet `f` (identical between sexes).
#' @param naa Numbers-at-age by sex `[first age is age zero]`. Four-dimensional numeric array `[sim, ages, year, p]`. `[p]` indexes the population, where
#' `[p = 1]` for females and `[p = 2]` for males.
#' @param faa Fishing mortality rate-at-age by sex and fleet `[first age is age zero]`. Five-dimensional numeric array `[sim, ages, year, p, f]` where `[f]` indexes fishery fleet.
#' @param waa Weight-at-age `[first age is age zero]`. Four-dimensional numeric array `[sim, ages, year, p]`.
#' @param Mataa  Maturity (spawning fraction)-at-age `[first age is age zero]`. Four-dimensional numeric array  `[sim, ages, year, p]`.
#' @param Maa Natural mortality rate-at-age `[first age is age zero]`. Four-dimensional numeric array `[sim, ages, year, p]`.
#' @param laa Length-at-age `[first age is age zero]`. Four-dimensional numeric array `[sim, ages, year, p]`.
#' @param fecaa Fecundity at age `[first age is age zero]`. If missing, default fecundity is the product of maturity and weight at age.
#' @param nyr_par_mu Positive integer. The number of recent years that natural mortality, age vulnerability, weight, length and maturity parameters are averaged over for defining future projection conditions.
#' @param LowerTri Integer. The number of recent years for which model estimates of recruitment are ignored (not reliably estimated by the assessment)
#' @param recind Positive integer. The first age class that fish 'recruit to the fishery'. The default is 0 - ie the first position in the age dimension of naa is age zero
#' @param plusgroup Logical. Does the assessment assume that the oldest age class is a plusgroup?
#' @param altinit Integer. Various assumptions for how to set up the initial numbers. 0: standard, 1: no plus group, 2: temporary fix for MSEtool plus group initialization
#' @param fixq1 Logical. Should q be fixed (ie assume the F-at-age array faa is accurate?
#' @param report Logical, if TRUE, a diagnostic will be reported showing the matching of the OM reconstructed numbers at age vs the assessment.
#' @param silent Whether to silence messages to the console.
#' @param ... Additional arguments (for all, either a numeric or a length nsim vector):
#' \itemize{
#' \item \code{SRrel} Stock-recruit relationship. (\code{1} for Beverton-Holt (default), \code{2} for Ricker)
#' \item \code{R0} unfished recruitment
#' \item \code{phi0} unfished spawners per recruit associated with R0 and h. With time-varying parameters, openMSE uses the mean phi0
#' in the first \code{ageM} (age of 50 percent maturity) years for the stock-recruit relationship. \code{Assess2OM} will re-calculate R0 and h
#' in the operating model such that the stock-recruit \code{alpha} and \code{beta} parameters match values implied in the input.
#' \item \code{Perr} recruitment standard deviation (lognormal distribution) for sampling future recruitment
#' \item \code{AC} autocorrelation in future recruitment deviates.
#' }
#' @details Use a seed for the random number generator to sample future recruitment.
#' @return An object of class \linkS4class{MOM}.
#' @author Q. Huynh
#' @export
#' @seealso \link{SS2MOM} \link{multiMSE} \link{Assess2OM}
Assess2MOM <- function(Name = "MOM created by Assess2MOM",
                       proyears = 50, interval = 2, CurrentYr = as.numeric(format(Sys.Date(), "%Y")),
                       h = 0.999,
                       Obs = MSEtool::Imprecise_Unbiased, Imp = MSEtool::Perfect_Imp,
                       naa, faa, waa, Mataa, Maa, laa, fecaa,
                       nyr_par_mu = 3, LowerTri = 1,
                       recind = 0, plusgroup = TRUE, altinit = 0, fixq1 = TRUE,
                       report = FALSE, silent = FALSE, ...) {
  np <- dim(naa)[4]
  ny <- dim(naa)[3]
  nf <- dim(faa)[5]
  if (!silent) message("A ", np, "-sex MOM will be created with ", nf, " fishing fleets and ", ny, " historical years.")

  if (inherits(Obs, "Obs")) Obs <- replicate(nf, Obs)
  if (inherits(Imp, "Imp")) Imp <- replicate(nf, Imp)

  if (missing(fecaa)) fecaa <- Mataa * waa

  OMs <- lapply(1:np, function(p) {
    lapply(1:nf, function(f) {
      Assess2OM(Name="A fishery made by VPA2OM",
                proyears = proyears, interval = interval, CurrentYr = CurrentYr,
                h = h, Obs = Obs[[f]], Imp = Imp[[f]],
                naa = naa[, , , p],
                faa = faa[, , , p, f],
                waa = waa[, , , p],
                Mataa = Mataa[, , , p],
                Maa = Maa[, , , p],
                laa = laa[, , , p],
                nyr_par_mu = nyr_par_mu,
                LowerTri = LowerTri,
                recind = recind,
                plusgroup = plusgroup,
                altinit = altinit,
                fixq1 = fixq1,
                report = FALSE, # Plots will be inaccurate anyway
                silent = TRUE,
                fecaa = fecaa[, , , p],
                ...)
    })
  })
  
  
  MOM <- suppressMessages(new("MOM"))

  slot_intersect <- intersect(slotNames("MOM"), slotNames("OM"))
  for(i in slot_intersect) slot(MOM, i) <- slot(OMs[[1]][[1]], i)
  MOM@Name <- Name
  MOM@cpars <- list()
  MOM@cpars <- lapply(1:np, function(p) {
    lapply(1:nf, function(f) {
      cp <- OMs[[p]][[f]]@cpars
      cp$Perr_y <- OMs[[1]][[1]]@cpars$Perr_y
      cp$D <- rep(1, length(cp$D))
      return(cp)
    })
  })

  MOM@Stocks <- lapply(1:np, function(p) {
    ss <- SubOM(OMs[[p]][[1]], "Stock")
    if (np == 1) {
      ss@Name <- "Unisex"
    } else if (p == 1) {
      ss@Name <- "Female"
    } else {
      ss@Name <- "Male"
    }
    return(ss)
  })

  MOM@Fleets <- lapply(1:np, function(p) {
    lapply(1:nf, function(f) {
      ff <- SubOM(OMs[[p]][[f]], "Fleet")
      ff@Name <- paste("Fleet", f)
      return(ff)
    })
  })

  MOM@Obs <- lapply(1:np, function(p) Obs)
  MOM@Imps <- lapply(1:np, function(p) Imp)

  MOM@CatchFrac <- lapply(1:np, function(p) {
    Z <- apply(faa[, , ny, p, ], 1:2, sum) + Maa[, , ny, p] # nsim x n_age
    CAA <- sapply(1:nf, function(f) naa[, , ny, p] * faa[, , ny, p, f] * (1 - exp(-Z))/Z,
                  simplify = "array") # nsim x n_age x nf
    CB <- apply(CAA * replicate(nf, waa[, , ny, p]), c(1, 3), sum)
    t(apply(CB, 1, function(x) x/sum(x)))
  })

  if (np == 2) {
    if (!silent) message("Stock recruit parameters based on female biology and recruitment predicted by female SSB.")
    MOM@SexPars <- list(SSBfrom = matrix(c(1, 0), 2, 2, byrow = TRUE))
    MOM@Complexes <- list(c(1, 2))
  }
  return(MOM)
}

