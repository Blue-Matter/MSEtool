#' Population dynamics for a MICE model (multiyear)
#'
#' Calls popdynOneMICE iteratively to reconstruct a time series given MICE model inputs
#'
#' @param qsx Total catchability
#' @param qfracx Vector [fleet], the fraction of total qs by fleet
#' @param np Integer, the number of stocks
#' @param nf Integer, number of fleets
#' @param nyears Integer, number of historical years (unfished til today)
#' @param nareas Integer, the number of spatial areas
#' @param maxage Integer, maximum modeled age
#' @param Nx Array [stock, age, year, area] of stock numbers
#' @param VFx Array [fleet, age, year, area] of the vulnerability curve
#' @param FretAx Array [fleet, age, year, area] of the retention curve
#' @param Effind Array [fleet, year] of effort
#' @param movx Array [stock,age,area,area] of movement transitions
#' @param Spat_targ Matrix [stock, fleet] of spatial targeting parameter
#' (0 evenly spatial distributed, 1 proportional to vulnerable biomass)
#' @param M_ageArrayx Array [stock, age,year] of Natural mortality rate at age
#' @param Mat_agex Array [stock, age, year] of maturity (spawning fraction) age
#' @param Asizex  Array [stock, area] Area size
#' @param Kx Vector [stock] of von B growth parameter K
#' @param Linf Vector [stock] of von B asymptotic length parameter Linf
#' @param t0 Vector [stock] of von B theoretical age at zero length (t0)
#' @param Mx Vector [stock] mature natural mortality rate
#' @param R0x Vector [stock] unfished recruitment
#' @param R0ax Matrix [stock, area] unfished recruitment by area
#' @param SSBpRx Matrix [stock, area] spawning biomass per recruit by area
#' @param hsx Vector [stock] steepness of the stock recruitment curve
#' @param aRx Vector [stock] stock recruitment parameter alpha (for Ricker curve)
#' @param bRx Vector [stock] stock recruitment parameter beta (for Ricker curve)
#' @param ax Vector [stock] weight-length parameter a W=aL^b
#' @param bx Vector [stock] weight-length parameter b W=aL^b
#' @param Perrx Matrix [stock, year] process error - the lognormal factor for recruitment strength
#' @param SRrelx Integer vector [stock] the form of the stock recruitment
#' relationship (1 = Beverton-Holt, 2= Ricker)
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific relationships (SSBfrom, stock_age)
#' @param x Integer the simulation number
#' @author T.Carruthers
#' @keywords internal
popdynMICE<-function(qsx,qfracx,np,nf,nyears,nareas,maxage,Nx,VFx,FretAx,Effind,
                     movx,Spat_targ, M_ageArrayx,Mat_agex,Asizex,Kx,Linfx,
                     t0x,Mx,R0x,R0ax,SSBpRx,hsx,aRx,
                     bRx,ax,bx,Perrx,SRrelx,Rel,SexPars,x, plusgroup, maxF) {

  n_age <- maxage + 1 # include age-0
  Bx<-SSNx<-SSBx<-VBx<-Zx<-array(NA,dim(Nx))
  VBfx<-array(NA,c(np,nf,n_age,nyears,nareas)) # initial year calculation
  Fy<-array(NA,c(np,nf,nyears))
  Fty<-array(NA,c(np,n_age,nyears,nareas))
  FMy<-FMrety<-VBfx<-array(NA,c(np,nf,n_age,nyears,nareas))
  Wt_agey<-array(NA,c(np,n_age,nyears))
  Ky<-Linfy<-t0y<-My<-hsy<-ay <-by<-array(NA,c(np,nyears))
  Ky[,1]<-Kx; Linfy[,1]<-Linfx; t0y[,1]<-t0x; My[,1]<-Mx; hsy[,1]<-hsx; ay[,1]<-ax; by[,1]<-bx

  Len_age<-matrix(Linfx*(1-exp(-(rep(0:maxage,each=np)-t0x)*(Kx))),nrow=np)
  Wt_agey[,,1]<-ax*Len_age^bx

  VBfind<-as.matrix(expand.grid(1:np,1:nf,1:n_age,1,1:nareas))
  Nind<-as.matrix(expand.grid(1:np,1:n_age,1,1:nareas))
  FMx<-FMretx<-Fdist<-array(NA,c(np,nf,n_age,nareas))
  Find<-TEG(dim(Fdist))
  VBcur<-array(NA,dim(VBfx)[c(1,2,3,5)])
  Ecur<-array(NA,c(np,nf))
  Vcur<-Retcur<-array(NA,c(np,nf,n_age))

  for(y in 2:(nyears+1)){

    Nind[,3]<-y-1
    Bx[Nind]<-Nx[Nind]*Wt_agey[Nind[,1:3]]
    #SSBx[Nind]<-Bx[Nind]*Mat_agex[Nind[,1:3]]
    VBfind[,4]<-y-1
    #      p f a r               p a y r             p f a y
    VBfx[VBfind]<-Bx[VBfind[,c(1,3,4,5)]]*VFx[VBfind[,1:4]]
    VBcur[]<-VBfx[,,,y-1,]#array(VBfx[,,,y-1,],dim(VBfx)[c(1,2,3,5)])
    Fdist[Find]<-VBcur[Find]^Spat_targ[Find[,1:2]]
    #VBagg<-apply(Fdist,1:3,sum)
    Fdist[Find]<-Fdist[Find]/apply(Fdist,1:3,sum)[Find[,1:3]]
    Fdist[is.na(Fdist)]<-0 # This is an NA catch for hermaphroditism
    Ecur[]<-Effind[,,y-1] #matrix(Effind[,,y-1],nrow=np,ncol=nf)
    Vcur[]<-VFx[,,,y-1]#array(VFx[,,,y-1],c(np,nf,maxage))
    FMx[Find]<-qsx[Find[,1]]*qfracx[Find[,1:2]]*Ecur[Find[,1:2]]*Fdist[Find]*
      Vcur[Find[,1:3]]/Asizex[Find[,c(1,4)]]
    
    # apply maxF 
    FMx[FMx>maxF] <- maxF
    Retcur[]<-FretAx[,,,y-1]#array(FretAx[,,,1],c(np,nf,maxage))
    FMretx[Find]<-qsx[Find[,1]]*qfracx[Find[,1:2]]*Ecur[Find[,1:2]]*Fdist[Find]*
      Retcur[Find[,1:3]]/Asizex[Find[,c(1,4)]]
    FMretx[FMretx>maxF] <- maxF
    #Ft<-array(apply(FMx,c(1,3,4),sum),c(np,maxage,nareas))#FMx[VBind]+M_agecur[VBind[,c(1,3)]]
    # y<-y+1
    # Fy[,,y-1]<-Effind[,,y-1]*qsx*qfracx  # this is basically apical F - yet to be subject to Fdist and Asize (inside popdynOneMICE)
    # y<-2; M_agecur=M_ageArrayx[,,y-1];Mat_agecur=Mat_agex[,,y-1];    PerrYrp=Perrx[,y+maxage-2]
    #Retcur=array(FretAx[,,,y-1],dim(FretAx)[1:3])
    #Fcur=array(Fy[,,y-1],dim(Fy)[1:2])
    #Ncur=array(Nx[,,y-1,],dim(Nx)[c(1:2,4)])
    #M_agecur=array(M_ageArrayx[,,y-1],dim(M_ageArrayx)[1:2])
    #Mat_agecur<-array(Mat_agex[,,y-1],dim(Mat_agex)[1:2])

    out<-popdynOneMICE(np,nf,nareas, maxage,
                       Ncur=array(Nx[,,y-1,],dim(Nx)[c(1:2,4)]),
                       Vcur=Vcur,
                       FMretx=FMretx,
                       FMx=FMx,
                       PerrYrp=Perrx[,y+n_age-1],
                       hsx=hsy[,y-1], aRx=aRx, bRx=bRx,
                       movy=array(movx[,,,,y-1],c(np,n_age,nareas,nareas)),
                       Spat_targ=Spat_targ, SRrelx=SRrelx,
                       M_agecur=array(M_ageArrayx[,,y-1],dim(M_ageArrayx)[1:2]),
                       Mat_agecur=array(Mat_agex[,,y-1],dim(Mat_agex)[1:2]),
                       Asizex=Asizex,
                       Kx=Ky[,y-1], Linfx=Linfy[,y-1], t0x=t0y[,y-1],
                       Mx=My[,y-1], R0x=R0x,R0ax=R0ax,SSBpRx=SSBpRx,ax=ay[,y-1],
                       bx=by[,y-1],Rel=Rel, SexPars=SexPars, x=x,
                       plusgroup=plusgroup)


    if(y<=nyears){
      Nx[,,y,]<-out$Nnext
      Wt_agey[,,y]<-out$Wt_age
      M_ageArrayx[,,y]<-out$M_agecurx
      Ky[,y]<-out$Kx; Linfy[,y]<-out$Linfx; t0y[,y]<-out$t0x; My[,y]<-out$Mx;
      hsy[,y]<-out$hsx; ay[,y]<-out$ax; by[,y]<-out$bx
      VBx[,,y,]<-out$VBt
      #VBfx[,,,y,]<-out$VBft
    }

    Zx[,,y-1,]<-out$Zt
    Fty[,,y-1,]<-out$Ft
    FMy[,,,y-1,]<-out$FMx
    FMrety[,,,y-1,]<-out$FMretx

  }

  Nind<-TEG(dim(Nx))
  Bx[Nind]<-Nx[Nind]*Wt_agey[Nind[,1:3]]
  SSNx[Nind]<-Nx[Nind]*Mat_agex[Nind[,1:3]]
  SSBx[Nind]<-Bx[Nind]*Mat_agex[Nind[,1:3]]

  list(Nx=Nx,Bx=Bx,SSNx=SSNx,SSBx=SSBx,VBx=VBx,FMy=FMy,FMrety=FMrety,Ky=Ky,
       Linfy=Linfy,t0y=t0y,My=My,hsy=hsy,ay=ay,by=by,VBfx=VBfx,Zx=Zx,Fty=Fty)
}



#' Population dynamics for a MICE model (single year)
#'
#' Completes a single iteration of recruitment, mortality, fishing and
#' movement given MICE model inputs
#'
#' @param np Integer, the number of stocks
#' @param nf Integer, number of fleets
#' @param nyears Integer, number of historical years (unfished til today)
#' @param maxage Integer, maximum modelled age
#' @param Ncur Array [stock, age, area] of stock numbers
#' @param Vcur Array [fleet, age, area] of the vulnerability curve
#' @param FMretx Array [stock, fleet, age, area] of the retention curve
#' @param FMx Array [stock, fleet, age, area] fishing mortality rate
#' @param PerrYrp Vector [stock] process error - the lognormal factor for
#' recruitment strength
#' @param hsx Vector [stock] steepness of the stock recruitment curve
#' @param aRx Vector [stock] stock recruitment parameter alpha (for Ricker curve)
#' @param bRx Vector [stock] stock recruitment parameter beta (for Ricker curve)
#' @param movy Array [stock,age,area,area] of movement transitions
#' @param Spat_targ Matrix [stock, fleet] of spatial targetting parameter
#' (0 evenly spatial distributed, 1 proportional to vulnerable biomass)
#' @param SRrelx Integer vector [stock] the form of the stock recruitment
#'  relationship (1 = Beverton-Holt, 2= Ricker)
#' @param M_agecur Matrix [stock, age] of Natural mortality rate at age
#' @param Mat_agecur Matrix [stock, age] of maturity (spawning fraction) age age
#' @param Asizex Matrix [stock, area] of relative area sizes
#' @param Kx Vector [stock] of von B growth parameter K
#' @param Linf Vector [stock] of von B asymptotic length parameter Linf
#' @param t0 Vector [stock] of von B theoretical age at zero length (t0)
#' @param Mx Vector [stock] mature natural mortality rate
#' @param R0x Vector [stock] unfished recruitment
#' @param R0ax Matrix [stock, area] unfished recruitment by area
#' @param SSBpRx Matrix [stock, area] spawning biomass per recruit by area
#' @param ax Vector [stock] weight-length parameter a W=aL^b
#' @param bx Vector [stock] weight-length parameter b W=aL^b
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific relationships (SSBfrom, stock_age)
#' @param x Integer. The simulation number
#' @author T.Carruthers
#' @keywords internal
popdynOneMICE<-function(np,nf,nareas, maxage, Ncur, Vcur, FMretx, FMx, PerrYrp,
                        hsx, aRx, bRx, movy,Spat_targ,
                        SRrelx,M_agecur,Mat_agecur,Asizex,
                        Kx,Linfx,t0x,Mx,R0x,R0ax,SSBpRx,ax,bx,Rel,SexPars,x,
                        plusgroup){

  n_age <- maxage+1
  # ----Initial Bcur calc (before any weight at age recalculation change) ----
  # Bcalc
  Bcur<-SSBcur<-SSNcur<-array(NA,dim(Ncur))
  Nind<-TEG(dim(Ncur)) # p, age, area
  Len_age<-matrix(Linfx*(1-exp(-(rep(0:maxage,each=np)-t0x)*(Kx))),nrow=np)
  Wt_age<-ax*Len_age^bx
  Bcur[Nind]<-Ncur[Nind]*Wt_age[Nind[,1:2]]
  SSBcur[Nind]<-Bcur[Nind]*Mat_agecur[Nind[,1:2]]
  SSNcur[Nind]<-Ncur[Nind]*Mat_agecur[Nind[,1:2]]

  # old surv
  surv <- array(c(rep(1,np),
                  t(exp(-apply(M_agecur, 1, cumsum)))[, 1:(n_age-1)]),
                c(np,n_age))  # Survival array
  oldM<-apply(M_agecur*Mat_agecur,1,sum)/apply(Mat_agecur,1,sum)
  M_agecurx<-M_agecur*Mx/oldM

  if(np>1 & length(Rel)>0){ # If there are MICE relationships

    Responses<-ResFromRel(Rel,Bcur,SSBcur,Ncur,seed=1)
    for(rr in 1:nrow(Responses))
      eval(parse(text=paste0(Responses[rr,4],"[",Responses[rr,3],"]<-",
                             as.numeric(Responses[rr,1]))))

    Len_age<-matrix(Linfx*(1-exp(-(rep(0:maxage,each=np)-t0x)*(Kx))),nrow=np)
    Wt_age<-ax*Len_age^bx

    # Parameters that could have changed: M, K, Linf, t0, a, b, hs
    # Recalc B SSB, SSN, M_age ------------------
    Bcur[Nind]<-Ncur[Nind]*Wt_age[Nind[,1:2]]
    SSBcur[Nind]<-Bcur[Nind]*Mat_agecur[Nind[,1:2]]
    SSNcur[Nind]<-Ncur[Nind]*Mat_agecur[Nind[,1:2]]
    M_agecurx<-M_agecur*Mx/oldM  # updated M

    # --- This is redundant code for updating parameters when R0 changes -----
    #surv <- cbind(rep(1,np),t(exp(-apply(M_agecurx, 1, cumsum)))[, 1:(maxage-1)])  # Survival array
    #SSB0x<-apply(R0x*surv*Mat_agecur*Wt_age,1,sum)
    #SSBpRx<-SSB0x/R0x
    #SSBpRax<-SSBpRx*distx
    #SSB0ax<-distx*SSB0x
    #R0ax<-distx*R0x
    #R0recalc thus aR bR recalc ---------------
    #bRx <- matrix(log(5 * hsx)/(0.8 * SSB0ax), nrow=np)  # Ricker SR params
    #aRx <- matrix(exp(bRx * SSB0ax)/SSBpRx, nrow=np)  # Ricker SR params

  } # end of MICE

  if(length(SexPars$SSBfrom)>0){
    SSBs<-SSBcur
    for(p in 1:np){ # use SSB from another stock to predict recruitment
      SSBcur[p,,]<-apply(SexPars$SSBfrom[p,]*SSBs,2:3,sum)
    }
  }

  # Vulnerable biomass calculation --------------------------------------------------
  VBft<-Fdist<-array(NA,c(np,nf,n_age,nareas))
  VBind<-TEG(dim(VBft))
  VBft[VBind]<-Vcur[VBind[,1:3]]*Bcur[VBind[,c(1,3:4)]]
  Ft<-array(apply(FMx,c(1,3,4),sum),c(np,n_age,nareas))#FMx[VBind]+M_agecur[VBind[,c(1,3)]]
  Zcur<-Ft+array(rep(M_agecur[VBind[,c(1,3)]],nareas),c(np,n_age,nareas))

  Nnext<-array(NA,c(np,n_age,nareas))

  # just for vulnerable biomass calculation
  SumF<-apply(FMx,c(1,3:4),sum,na.rm=T) # sum over fleets stock,age,area
  MaxF<-apply(SumF,c(1,3),max)     # get max F
  Selx<-array(NA,dim(SumF))
  Selx[Nind]<-SumF[Nind]/MaxF[Nind[,c(1,3)]]
  VBt<-Bcur*Selx
  
  for(p in 1:np){
    NextYrN <- popdynOneTScpp(nareas, maxage, Ncurr=Ncur[p,,],
                            Zcurr=Zcur[p,,], mov=movy[p,,,],
                            plusgroup = plusgroup[p])
    Nnext[p,,]<-NextYrN
    Nnext[p,1,] <- 0
  }

  if(length(SexPars$Herm)>0){ # Hermaphroditic mode
    Nnext[is.na(Nnext)]<-0 # catch for NAs
    for(i in 1:length(SexPars$Herm)){
      ps<-as.numeric(strsplit(names(SexPars$Herm)[i],"_")[[1]][2:3])
      pfrom<-ps[2]
      pto<-ps[1]
      frac<-rep(1,maxage)
      frac[1:length(SexPars$Herm[[i]][x,])]<-SexPars$Herm[[i]][x,]
      h_rate<-hrate(frac)
      Nnext[pto,,]<- Nnext[pto,,]*(frac>0) # remove any recruitment
      Nmov<-Nnext[pfrom,,]*h_rate
      Nnext[pto,,]<- Nnext[pto,,]+Nmov
      Nnext[pfrom,,]<-Nnext[pfrom,,]-Nmov  # subtract fish
    }
  }

  SSBcur[Nind]<-Nnext[Nind]*Wt_age[Nind[,1:2]]*Mat_agecur[Nind[,1:2]]
  SSBcurr <- apply(SSBcur, c(1,3), sum)
  
  # this year's recruitment
  for (p in 1:np) {
    if (SRrelx[p]== 1) { # BH rec
      rec_A <- PerrYrp[p] * (4*R0ax[p,] * hsx[p] * SSBcurr[p,])/
        (SSBpRx[p,] * R0ax[p,] * (1-hsx[p]) + (5*hsx[p]-1) *SSBcurr[p,])
    } else {
      rec_A <- PerrYrp[p] * aRx[p,] * SSBcurr[p,] * exp(-bRx[p,]*SSBcurr[p,])
    }
    Nnext[p,1,] <- rec_A
  }
  
  Bcur[Nind]<-Nnext[Nind]*Wt_age[Nind[,1:2]]
  SSBcur[Nind]<-Bcur[Nind]*Mat_agecur[Nind[,1:2]]
  SSNcur[Nind]<-Nnext[Nind]*Mat_agecur[Nind[,1:2]]

  # returns new N and any updated parameters:
  list(Nnext=Nnext,M_agecurx=M_agecurx,R0x=R0x,R0ax=R0ax,hsx=hsx, #5
       aRx=aRx,bRx=bRx,Linfx=Linfx,Kx=Kx,t0x=t0x,Mx=Mx,ax=ax,bx=bx, #13
       Len_age=Len_age,Wt_age=Wt_age,surv=surv,FMx=FMx,FMretx=FMretx, #18
       VBt=VBt,VBft=VBft,Zt=Zcur,Ft=Ft,Bt=Bcur, SSNt=SSNcur, SSBt=SSBcur) #25

}


#' Returns Results of a set of MICE relationships
#'
#' Predicts stock-specific parameters from another stocks biomass, spawning biomass or numbers
#'
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param Bcur An array of current stock biomass [stock, age, area]
#' @param SSBcur An array of current spawning stock biomass [stock, age, area]
#' @param Ncur An array of current stock numbers [stock, age, area]
#' @author T.Carruthers
#' @keywords internal
ResFromRel<-function(Rel,Bcur,SSBcur,Ncur,seed){

  IVnams<-c("B","SSB","N")
  IVcode<-c("Bcur","SSBcur","Ncur")

  DVnam<-c("M","a", "b", "R0", "hs", "K", "Linf", "t0")
  modnam<-c("Mx","ax","bx","R0x","hsx","Kx","Linfx","t0x")

  nRel<-length(Rel)
  out<-array(NA,c(nRel,4))

  for(r in 1:nRel){

    fnams<-names(attr(Rel[[r]]$terms,"dataClasses"))
    DV<-fnams[1]
    Dp<-unlist(strsplit(DV,"_"))[2]
    Dnam<-unlist(strsplit(DV,"_"))[1]
    IV<-fnams[2:length(fnams)]
    nIV<-length(IV)
    IVs<-matrix(unlist(strsplit(IV,"_")),ncol=nIV)
    newdat<-NULL
    for(iv in 1:nIV){
      p<-as.numeric(IVs[2,iv])
      if(IVs[1,iv]=="B")newdat<-rbind(newdat,sum(Bcur[p,,]))
      if(IVs[1,iv]=="SSB")newdat<-rbind(newdat,sum(SSBcur[p,,]))
      if(IVs[1,iv]=="N")newdat<-rbind(newdat,sum(Ncur[p,,]))
    }
    newdat<-as.data.frame(newdat)
    names(newdat)<-IV
    ys<-predict(Rel[[r]],newdat=newdat)
    templm<-Rel[[r]]
    templm$fitted.values <- ys
    out[r,1]<-unlist(simulate(templm, nsim=1, seed=seed))
    out[r,2]<-DV
    out[r,3]<-Dp
    out[r,4]<-modnam[match(Dnam,DVnam)]

  }

  out

}


#' Derives the rate of exchange from one sex to another based on asymptotic fraction
#'
#' @param frac A vector of asymptotic sex fraction (must start with zero and end with 1)
#' @author T.Carruthers
#' @keywords internal
hrate<-function(frac){

  m1frac<-1-frac
  ind1<-(1:(length(frac)-1))
  ind2<-ind1+1
  hrate<-rep(0,length(frac))
  hrate[ind2]<-1-(m1frac[ind2]/m1frac[ind1])
  hrate[is.na(hrate)]<-1
  hrate[hrate<0]<-0
  #cbind(frac,m1frac,hrate)
  hrate

}


