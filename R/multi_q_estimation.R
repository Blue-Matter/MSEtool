
#' Optimize for catchability (q) and fishing dist for a MICE model
#'
#' Function optimizes catchability (q, where F=qE) required to get to user-specified stock
#' depletion across stocks and fleets if there are relationships among stocks
#'
#' @param x Integer, the simulation number
#' @param StockPars A list of sampled stock parameters, one list element per stock
#' @param FleetPars A hierarchical list of sampled fleet parameters,
#' first list level is stock, second is fleet
#' @param np The number of stocks
#' @param nf The number of fleets
#' @param nareas The number of areas
#' @param maxage The maximum number of modeled ages
#' @param nyears The number of historical 'spool-up' years (from unfished to now)
#' @param N An array of stock numbers [nsim,np,maxage,nyears,nareas] -
#' only the values from the first year are used
#' @param VF An array of vulnerability [nsim,np,nf,maxage,nyears+proyears]
#' @param FretA An array of retention [nsim,np,nf,maxage,nyears+proyears]
#' @param maxF A numeric value specifying the maximum fishing mortality for any
#' single age class
#' @param MPA An of spatial closures by year [np,nf,nyears+proyears,nareas]
#' @param CatchFrac A list of stock-specific fleet fractions of current catch
#' list[[stock]][nsim, nf]
#' @param bounds Bounds for total q estimation
#' @param tol A numeric value that is the fraction of machine tolerance
#' (once reduction in objective function steps below this, optimization ends)
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific dynamics SSBfrom stock_age
#' @author T.Carruthers
#' @keywords internal
getq_multi_MICE <- function(x, StockPars, FleetPars, np,nf, nareas, maxage,
                            nyears, N, VF, FretA, maxF=0.9, MPA,CatchFrac,
                            bounds= c(1e-05, 15),tol=1E-6,Rel,SexPars, plusgroup,
                            optVB=FALSE) {

  n_age <- maxage +1 # include age-0
  Nx <- array(N[x,,,,],dim(N)[2:5])
  VFx <- array(VF[x,,,,],dim(VF)[2:5])
  FretAx <- array(FretA[x,,,,],dim(VF)[2:5])

  Kx<-matrix(unlist(lapply(StockPars,function(dat)dat['K'])),ncol=np)[x,]
  Linfx<-matrix(unlist(lapply(StockPars,function(dat)dat['Linf'])),ncol=np)[x,]
  t0x<-matrix(unlist(lapply(StockPars,function(dat)dat['t0'])),ncol=np)[x,]
  Mx<-matrix(unlist(lapply(StockPars,function(dat)dat['M'])),ncol=np)[x,]
  R0x<-matrix(unlist(lapply(StockPars,function(dat)dat['R0'])),ncol=np)[x,]
  SSB0x<-matrix(unlist(lapply(StockPars,function(dat)dat['SSB0'])),ncol=np)[x,]
  VB0x<-matrix(unlist(lapply(StockPars,function(dat)dat['VB0'])),ncol=np)[x,]

  hsx<-matrix(unlist(lapply(StockPars,function(dat)dat['hs'])),ncol=np)[x,]
  ax<-matrix(unlist(lapply(StockPars,function(dat)dat['a'])),ncol=np)[1,]
  bx<-matrix(unlist(lapply(StockPars,function(dat)dat['b'])),ncol=np)[1,]
  SRrelx<-matrix(unlist(lapply(StockPars,function(dat)dat['SRrel'])),ncol=np)[x,]

  distx<-Asizex<-SSBpRx<-R0ax<-aRx<-bRx<-array(NA,c(np,nareas))
  Perrx<-array(NA,c(np,nyears+n_age))
  movx<-array(NA,c(np,n_age,nareas,nareas,nyears))

  for(p in 1:np){
    distx[p,]<-StockPars[[p]]$R0a[x,]/sum(StockPars[[p]]$R0a[x,])
    Perrx[p,]<-StockPars[[p]]$Perr_y[x,1:(nyears+n_age)]
    movx[p,,,,]<-StockPars[[p]]$mov[x,,,,1:nyears]
    SSBpRx[p,]<-StockPars[[p]]$SSBpR[x,]
    R0ax[p,]<-StockPars[[p]]$R0a[x,]
    aRx[p,]<-StockPars[[p]]$aR[x,]
    bRx[p,]<-StockPars[[p]]$bR[x,]
    Asizex[p,]<-StockPars[[p]]$Asize[x,]
  }

  M_ageArrayx<-Mat_agex<-array(NA,c(np,n_age,nyears))
  Effind<-array(NA,c(np,nf,nyears))
  Spat_targ<-array(NA,c(np,nf))

  for(p in 1:np){
    Mat_agex[p,,]<-StockPars[[p]]$Mat_age[x,,1:nyears]
    M_ageArrayx[p,,]<-StockPars[[p]]$M_ageArray[x,,1:nyears]
    Effind[p,,]<-t(matrix(unlist(lapply(FleetPars[[p]], function(dat,x)
      dat['Find'][[1]][x,],x=x)),ncol=nf))
    Spat_targ[p,]<-unlist(lapply(FleetPars[[p]], function(dat,x)
      dat['Spat_targ'][[1]][x],x=x))
  }

  CF<-t(matrix(unlist(lapply(CatchFrac,function(dat)dat[x,])),nrow=nf))
  Fdist<-CF/Effind[,,nyears] # Catch divided by effort (q proxy)
  Fdist<-Fdist/apply(Fdist[,,drop=F],1,sum)    # q ratio proxy (real space)

  if(nf==1){
    par<-rep(-5,np)
  }else{
    # low initial F followed by logit guess at fraction based on Fdist
    # according to catch fraction in recent year
    par<-c(rep(-5,np),log(Fdist[,2:nf]/(1-Fdist[,2:nf])))
  }

  depc <- matrix(unlist(lapply(StockPars,function(dat)dat['D'])),ncol=np)[x,]
  CFc<-array(NA,c(np,nf))
  for(p in 1:np)CFc[p,]=CatchFrac[[p]][x,]
  factr<-tol/.Machine$double.eps

  opt<-optim(par,qestMICE,
             method="L-BFGS-B",
             lower=c(rep(log(bounds[1]),np),rep(-5,np*(nf-1))),
             upper=c(rep(log(bounds[2]),np),rep(5,np*(nf-1))),
             depc=depc, CFc=CFc, mode='opt', np=np, nf=nf, nyears=nyears,
             nareas=nareas, maxage=maxage, Nx=Nx, VFx=VFx, FretAx=FretAx,
             Effind=Effind, distx=distx, movx=movx, Spat_targ=Spat_targ,
             M_ageArrayx=M_ageArrayx, Mat_agex=Mat_agex, Asizex=Asizex,Kx=Kx,
             Linfx=Linfx, t0x=t0x, Mx=Mx, R0x=R0x, R0ax=R0ax, SSBpRx=SSBpRx,
             SSB0x=SSB0x, hsx=hsx, ax=ax, bx=bx, aRx=aRx, bRx=bRx, Perrx=Perrx,
             SRrelx=SRrelx, Rel=Rel, SexPars=SexPars, x=x, plusgroup=plusgroup,
             optVB=optVB, VB0x=VB0x,
             control=list(trace=1,factr=factr))

  out<-qestMICE(par=opt$par, depc=depc,CFc=CFc,mode='calc', np=np, nf=nf,
                nyears=nyears, nareas=nareas, maxage=maxage, Nx=Nx, VFx=VFx,
                FretAx=FretAx, Effind=Effind, distx=distx, movx=movx,
                Spat_targ=Spat_targ, M_ageArrayx=M_ageArrayx, Mat_agex=Mat_agex,
                Asizex=Asizex, Kx=Kx, Linfx=Linfx, t0x=t0x, Mx=Mx, R0x=R0x,
                R0ax=R0ax, SSBpRx=SSBpRx, SSB0x=SSB0x, hsx=hsx, aRx=aRx, bRx=bRx,
                ax=ax, bx=bx, Perrx=Perrx, SRrelx=SRrelx,
                Rel=Rel,SexPars=SexPars,x=x, plusgroup=plusgroup,
                optVB=optVB, VB0x=VB0x)

  out
}


#' Internal function for optimizing catchability (q) for a MICE model
#'
#' Function returns objective function that fits both stock depletion and catch fraction among fleets
#'
#' @param par Integer, the simulation number
#' @param depc Numeric vector, nstock long of specified stock depletion (SSB now / SSB0)
#' @param CFc Matrix [nstock, nfleet], a catch fraction among fleets (sums to 1 for each stock (row))
#' @param mod Character if 'opt' qestMICE returns the objective function otherwise the fitted values in a list
#' @param nf Integer, number of stocks
#' @param nf Integer, number of fleets
#' @param nyears Integer, number of historical years (unfished til today)
#' @param nareas Integer, number of areas (default is 2)
#' @param maxage Integer, maximum number of age classes for calculation
#' @param Nx Array [stock, age, year, area] of stock numbers
#' @param VFx Array [fleet, age, year, area] of the vulnerability curve
#' @param FretAx Array [fleet, age, year, area] of the retention curve
#' @param Effind Array [fleet, year] of effort
#' @param movx Array [stock,age,area,area] of movement transitions
#' @param Spat_targ Matrix [stock, fleet] of spatial targetting parameter (0 evenly spatial distributed, 1 proportional to vulnerable biomass)
#' @param M_ageArrayx Array [stock, age,year] of Natural mortality rate at age
#' @param Mat_agex Array [stock, age, year] of maturity (spawning fraction) age age
#' @param Asizex Matrix [stock, area] Area size
#' @param Kx Vector [stock] of von B growth parameter K
#' @param Linf Vector [stock] of von B asymptotic length parameter Linf
#' @param t0 Vector [stock] of von B theoretical age at zero length (t0)
#' @param Mx Vector [stock] mature natural mortality rate
#' @param R0x Vector [stock] unfished recruitment
#' @param R0ax Matrix [stock, area] unfished recruitment by area
#' @param SSBpRx Matrix [stock, area] spawning biomass per recruit by area
#' @param SSB0x Vector [stock] Unfished spawning stock biomass
#' @param hsx Vector [stock] steepness of the stock recruitment curve
#' @param aRx Vector [stock] stock recruitment parameter alpha (for Ricker curve)
#' @param bRx Vector [stock] stock recruitment parameter beta (for Ricker curve)
#' @param ax Vector [stock] weight-length parameter a W=aL^b
#' @param bx Vector [stock] weight-length parameter b W=aL^b
#' @param Perrx Matrix [stock, year] process error - the lognormal factor for recruitment strength
#' @param SRrelx Integer vector [stock] the form of the stock recruitment relationship (1 = Beverton-Holt, 2= Ricker)
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific dynamics (SSBfrom, stcck_age)
#' @param x Integer. The simulation number
#' @author T.Carruthers
#' @keywords internal
qestMICE<-function(par,depc,CFc,mode='opt',np,nf,nyears,nareas,maxage,Nx,VFx,
                   FretAx,Effind,distx,movx,Spat_targ,M_ageArrayx,Mat_agex,
                   Asizex,Kx,Linfx,t0x,Mx,R0x,R0ax,SSBpRx,SSB0x,hsx,aRx, bRx,
                   ax,bx,Perrx,SRrelx,Rel,SexPars,x, plusgroup, optVB, VB0x){

  n_age <- maxage + 1 # include age-0
  qsx<-exp(par[1:np])
  if(nf==1){
    qfracx<-matrix(1,nrow=np)
  }else{
    qlogit<-array(0,c(np,nf))
    qlogit[,2:nf]<-par[(np+1):length(par)]
    qfracx<-exp(qlogit)/apply(exp(qlogit),1,sum)
  }

  HistVars<-popdynMICE(qsx,qfracx,np,nf,nyears,nareas,maxage,Nx,VFx,FretAx,
                       Effind,movx,Spat_targ,M_ageArrayx, Mat_agex,Asizex,Kx,
                       Linfx,t0x,Mx,R0x,R0ax,SSBpRx,hsx,aRx, bRx,ax,bx,Perrx,
                       SRrelx,Rel,SexPars,x, plusgroup)


  if (optVB) {
    VBest<-apply(HistVars$VBx,c(1,3),sum)
    deppred<-VBest[,nyears]/VB0x
  } else {
    SSBest<-apply(HistVars$SSBx,c(1,3),sum)
    deppred<-SSBest[,nyears]/SSB0x  
  }
  

  if(length(SexPars)>0){ # you need to make depletion just one variable for all components of a sex-specific model

    sexmatches<-sapply(1:nrow(SexPars$SSBfrom),function(x,mat)
      paste(mat[x,],collapse="_"), mat=SexPars$SSBfrom)
    parcopy<-match(sexmatches,sexmatches)
    deppred<-deppred[parcopy]

    qsx <- qsx[parcopy] # copy female q to males

  }

  Cpred0<-array(NA,c(np,nf,n_age,nareas))
  Cind<-TEG(dim(Cpred0))
  Find<-cbind(Cind[,1:3],nyears,Cind[,4]) # p f age y area
  Bind<-Find[,c(1,3:5)]
  Cpred0[Cind]<-HistVars$Bx[Bind]*(1-exp(-HistVars$FMy[Find]))
  Ctot<-apply(Cpred0,1:2,sum)
  Cpred<-Ctot/apply(Ctot,1,sum)

  depOBJ<-sum((log(depc) - log(deppred))^2)
  cOBJ<-sum(log(CFc/Cpred)^2) # Lazy - should be: sum(log(CFc[,2:nf]/Cpred[,2:nf])^2) but this doesn't work for single fleets and it makes no difference anyway

  if(mode=='opt'){
    return(depOBJ+cOBJ)
  }else{
    return(list(qtot=qsx,
                qfrac=qfracx,
                CFc=CFc,
                Cpred=Cpred,
                depc=depc,
                deppred=deppred))#,Vulnf=Vulnf,Retf=Retf,MPAf=MPAf))
  }

}



