
#' Slot in list: get the slot values from a list of objects
#'
#' Create of vector of values that correspond with a slot in a list of objects
#'
#' @param listy A list of objects
#' @param sloty A character vector representing the slot name
#' @author T. Carruthers
SIL<-function(listy,sloty) {
  if(class(listy[[1]])=="list"){
    out<-NULL
    for(i in 1:length(listy))out<-c(out,unlist(lapply(listy[[i]],function(x)slot(x,sloty))))
  }else{
    out<-unlist(lapply(listy,function(x)slot(x,sloty)))
  }
  out
}

#' Item in list: get the list values from a list of lists
#'
#' Create of vector of values that correspond with a slot in a list of objects
#'
#' @param listy A list of objects
#' @param namey A character vector representing the list item's name
#' @param lev1 Logical, should NIL default to the first level of the list?
#' @author T. Carruthers
NIL<-function(listy,namey,lev1=T){
  if(class(listy[[1]])=="list"&!lev1){
    out<-NULL
    for(i in 1:length(listy))out<-c(out,unlist(lapply(listy[[i]],function(x)x[namey])))
  }else{
    out<-unlist(lapply(listy,function(x)x[namey]))
  }
  out
}

#' Expand the Herm list in SexPars to a matrix of fractions at age
#'
#' @param Herm A list of Hermaphroditic fractions at age (starting age class 1)
#' @param maxage The maximum age of stocks being simulated
#' @param np The total number of stocks being simulated
#' @param nsim The number of simulations
#' @author T. Carruthers
expandHerm<-function(Herm,maxage,np,nsim){
  HermFrac<-array(1,c(nsim,np,maxage))
  if(length(Herm)>0){
    ps<-matrix(as.numeric(sapply(names(Herm),function(x)strsplit(x,"_")[[1]][2:3])),nrow=length(Herm),byrow=T)
    for(i in 1:length(Herm)){
      HermFrac[,ps[i,1],1:ncol(Herm[[1]])]<-Herm[[1]]
      HermFrac[,ps[i,2],]<-0
      HermFrac[,ps[i,2],1:ncol(Herm[[1]])]<-1-Herm[[1]]
    }
  }
  HermFrac
}

#' Tom's expand grid
#'
#' Create an indexing grid from just a vector of maximum dimension sizes
#'
#' @param vec A vector of maximum array sizes
#' @author T. Carruthers
TEG<-function(vec){ # make index for list calculation
  dim<-new('list')
  ndims<-length(vec)
  for(i in 1:ndims)dim[[i]]<-1:vec[i]
  as.matrix(expand.grid(dim))
}

#' Reconstruct historical dynamics
#'
#' Function that reconstructs historical stock trends from fitted qs and all other parameters including MICE components
#'
#' @param x Integer, the simulation number
#' @param StockPars A list of sampled stock parameters, one list element per stock
#' @param FleetPars A hierarcical list of sampled fleet parameters, first list level is stock, second is fleet
#' @param np The number of stocks
#' @param nf The number of fleets
#' @param nareas The number of areas
#' @param maxage The maximum number of modelled ages
#' @param nyears The number of historical 'spool-up' years (from unfished to now)
#' @param N An array of stock numbers [nsim,np,maxage,nyears,nareas] - only the values from the first year are used
#' @param VF An array of vulnerability [nsim,np,nf,maxage,nyears+proyears]
#' @param FretA An array of retention [nsim,np,nf,maxage,nyears+proyears]
#' @param maxF A numeric value specifying the maximum fishing mortality for any single age class
#' @param MPA An of spatial closures by year [np,nf,nyears+proyears,nareas]
#' @param CatchFrac A list of stock-specific fleet fractions of current catch list[[stock]][nsim, nf]
#' @param bounds Bounds for total q estimation
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific relationships (SSBfrom, stock_age)
#' @author T.Carruthers
#' @keywords internal
HistMICE<-function(x,StockPars, FleetPars, np,nf, nareas, maxage, nyears, N, VF, 
                   FretA, maxF=0.9, MPA,Rel,SexPars,qs,qfrac,
                   plusgroup){

  n_age <- maxage+1 # including age-0
  Nx<-array(N[x,,,,],dim(N)[2:5])
  VFx<-array(VF[x,,,,],dim(VF)[2:5])
  FretAx<-array(FretA[x,,,,],dim(VF)[2:5])
  #NIL(StockPars,"K")

  Kx<-matrix(unlist(lapply(StockPars,function(dat)dat['K'])),ncol=np)[x,]
  Linfx<-matrix(unlist(lapply(StockPars,function(dat)dat['Linf'])),ncol=np)[x,]
  t0x<-matrix(unlist(lapply(StockPars,function(dat)dat['t0'])),ncol=np)[x,]
  Mx<-matrix(unlist(lapply(StockPars,function(dat)dat['M'])),ncol=np)[x,]
  R0x<-matrix(unlist(lapply(StockPars,function(dat)dat['R0'])),ncol=np)[x,]

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
    Asizex[p,]<-StockPars[[p]]$Asize[x,]
    movx[p,,,,]<-StockPars[[p]]$mov[x,,,,1:nyears]
    SSBpRx[p,]<-StockPars[[p]]$SSBpR[x,]
    R0ax[p,]<-StockPars[[p]]$R0a[x,]
    aRx[p,]<-StockPars[[p]]$aR[x,]
    bRx[p,]<-StockPars[[p]]$bR[x,]
  }

  M_ageArrayx<-Mat_agex<-array(NA,c(np,n_age,nyears))
  Effind<-array(NA,c(np,nf,nyears))
  Spat_targ<-array(NA,c(np,nf))

  for(p in 1:np){
    Mat_agex[p,,]<-StockPars[[p]]$Mat_age[x,,1:nyears]
    M_ageArrayx[p,,]<-StockPars[[p]]$M_ageArray[x,,1:nyears]
    Effind[p,,]<-t(matrix(unlist(lapply(FleetPars[[p]],function(dat,x)dat['Find'][[1]][x,],x=x)),ncol=nf))
    Spat_targ[p,]<-unlist(lapply(FleetPars[[p]],function(dat,x)dat['Spat_targ'][[1]][x],x=x))
  }

  qsx<-qs[x,]
  qfracx<-matrix(qfrac[x,,],c(np,nf))

  popdynMICE(qsx=qsx,qfracx=qfracx,np,nf,nyears,nareas,maxage,Nx,VFx,FretAx,Effind,
             movx,Spat_targ,M_ageArrayx,Mat_agex,Asizex,Kx,Linfx,t0x,Mx,R0x,R0ax,
             SSBpRx,hsx,aRx, bRx,ax,bx,Perrx,SRrelx,Rel,SexPars,x,
             plusgroup, maxF)

}


#' Normalize
#'
#' Normalize an array over certain dimensions
#'
#' @param listy A list of objects
#' @param namey A character vector representing the list item's name
#' @author T. Carruthers
nlz<-function(arr,dims=NULL,func="max"){
  arrdim<-dim(arr)
  ndim<-length(arrdim)
  if(is.null(dims))dims=1:(ndim-1)
  agg<-apply(arr,dims,func)
  out<-arr
  ind<-TEG(arrdim)
  out[ind]<-out[ind]/agg[ind[,dims]]
  out
}


#' Dimensions of a hierarchical list object
#'
#' @param x A list
#' @author T. Carruthers
ldim<-function(x){
  if(class(x)!='list'){
    #message(paste(deparse(substitute(x))," is not a list - ldim operates on hieracical list objects"))
    return(0)
  }else{
    dim<-NULL
    cond=TRUE
    while(cond){
      dim=c(dim,length(x))
      cond=(class(x[[1]])=='list')
      if(cond)x=x[[1]]
    }
    return(dim)
  }
}


#' Combine data among stocks
#'
#' Catches, CAA, CAL are summed. LFC and LFS are weighted averages. ML, Lc and Lbar are recalculated from summed CAL. All other observations are for fleet 1 (indicative)
#'
#' @param MSElist A hierarcical list of data objects stock then fleet then MP
#' @param StockPars A list of stock parameters
#' @param np The number of stocks
#' @param mm Integer the MP number
#' @param nf The number of fleets
#' @param realVB A matrix of real vulnerable biomass [nsim,np, year]
#' @author T. Carruthers
multiDataS<-function(MSElist,StockPars,np,mm,nf,realVB){

  nsim<-dim(MSElist[[1]][[1]][[mm]]@Cat)[1]
  nyears<-dim(MSElist[[1]][[1]][[mm]]@Cat)[2]
  na<-dim(MSElist[[1]][[1]][[mm]]@CAA)[3]
  nl<-dim(MSElist[[1]][[1]][[mm]]@CAL)[3]
  ni<-np*nf


  if(realVB[1,1,1]==0)realVB[,,1]<-realVB[,,2] # impute vulnerable biomass for year 1 if missing (a negligible issue to be fixed in popdyn_MICE)

  DBF<-list()
  realVBi<-array(NA,c(nsim,nyears,nf*np))

  for(f in 1:nf){
    for(p in 1:np){
      i<-p+(np*(f-1))

      DBF[[i]]<-MSElist[[p]][[f]][[mm]]
      realVBi[,,i]<-realVB[,p,]
    }
  }


  Dataout<-DBF[[1]]

  Cat<-array(SIL(DBF,"Cat"),c(nsim,nyears,ni))
  CAA<-array(SIL(DBF,"CAA"),c(nsim,nyears,na,ni))
  CAL<-array(SIL(DBF,"CAL"),c(nsim,nyears,nl,ni))
  LFS<-array(SIL(DBF,"LFS"),c(nsim,ni))
  LFC<-array(SIL(DBF,"LFC"),c(nsim,ni))

  ML<-array(SIL(DBF,"ML"),c(nsim,nyears,ni)) # for checking purposes
  Lc<-array(SIL(DBF,"Lc"),c(nsim,nyears,ni))
  Lbar<-array(SIL(DBF,"Lbar"),c(nsim,nyears,ni))

  # Additions
  Dataout@Cat<-apply(Cat,1:2,sum)
  Dataout@CAA<-apply(CAA,1:3,sum)
  Dataout@CAL<-apply(CAL,1:3,sum)

  # Weighted means
  Dataout@LFS<-apply(LFS*Cat[,nyears,],1,sum)/apply(Cat[,nyears,],1,sum)
  Dataout@LFC<-apply(LFC*Cat[,nyears,],1,sum)/apply(Cat[,nyears,],1,sum)

  # Data among stocks have varying length bins so we resort to weighted averages here
  Dataout@ML<-apply(ML*Cat,1:2,sum)/apply(Cat,1:2,sum)
  Dataout@Lc<-apply(Lc*Cat,1:2,sum)/apply(Cat,1:2,sum)
  Dataout@Lbar<-apply(Lbar*Cat,1:2,sum)/apply(Cat,1:2,sum)

  # You were here!!!
  Ind<-array(SIL(DBF,"Ind"),c(nsim,nyears,ni))
  Rec<-array(SIL(DBF,"Ind"),c(nsim,nyears,ni))
  Dataout@Ind<-apply(Ind*realVBi,1:2,sum)/apply(realVBi,1:2,sum)
  Dataout@Rec<-apply(Rec*realVBi,1:2,sum)/apply(realVBi,1:2,sum)

  AvC<-array(SIL(DBF,"AvC"),c(nsim,ni))
  Dataout@AvC<-apply(AvC,1,sum)

  popsimslot<-function(Dataout,sloty,realVBi,nsim,ni){
    temp<-array(SIL(DBF,sloty),c(nsim,ni))
    slot(Dataout,sloty)<-apply(temp*realVBi[,nyears,],1,sum)/apply(realVBi[,nyears,],1,sum)
    Dataout
  }

  Dataout<-popsimslot(Dataout,'Dt',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'Mort',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'FMSY_M',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'BMSY_B0',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'L50',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'L95',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'LFC',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'LFS',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'Dep',realVBi,nsim,ni)

  Abun<-array(SIL(DBF,'Abun'),c(nsim,ni))
  Dataout@Abun<-apply(Abun,1,sum)

  SpAbun<-array(SIL(DBF,'SpAbun'),c(nsim,ni))
  Dataout@SpAbun<-apply(SpAbun,1,sum)

  Dataout<-popsimslot(Dataout,'vbK',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'vbLinf',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'vbt0',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'LenCV',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'wla',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'wlb',realVBi,nsim,ni)

  Dataout<-popsimslot(Dataout,'steep',realVBi,nsim,ni)
  Dataout<-popsimslot(Dataout,'sigmaR',realVBi,nsim,ni)

  Cref<-array(SIL(DBF,'Cref'),c(nsim,ni))
  Dataout@Cref<-apply(Cref,1,sum)

  Dataout<-popsimslot(Dataout,'Iref',realVBi,nsim,ni)

  Bref<-array(SIL(DBF,'Bref'),c(nsim,ni))
  Dataout@Bref<-apply(Bref,1,sum)

  Dataout
}


#' Fill any NAs arising from MPCalcs (hermaphroditism mode)
#'
#' @param MPCalcs A list of arrays arising fromt the DLMtool function CalcMPDynamics()
#' @author T. Carruthers
MPCalcsNAs<-function(MPCalcs){
  for(i in 1:length(MPCalcs)){
    if (names(MPCalcs)[i] != "TAE") MPCalcs[[i]][is.na(MPCalcs[[i]])]<-0  # TAE can be NA
  }
  MPCalcs

}


#' Combine data among fleets
#'
#' Catches, CAA, CAL are summed. LFC and LFS are weighted averages. ML, Lc and Lbar are recalculated from summed CAL. All other observations are for fleet 1 (indicative)
#'
#' @param MSElist A hierarcical list of data objects stock then fleet then MP
#' @param StockPars A list of stock parameters
#' @param p Integer the Stock number
#' @param mm Integer the MP number
#' @param nf The number of fleets
#' @author T. Carruthers
multiData<-function(MSElist,StockPars,p,mm,nf){

  DBF<-list()
  for(f in 1:nf)DBF[[f]]<-MSElist[[p]][[f]][[mm]]
  Dataout<-DBF[[1]]
  nsim<-dim(Dataout@Cat)[1]
  nyears<-dim(Dataout@Cat)[2]
  na<-dim(Dataout@CAA)[3]
  nl<-dim(Dataout@CAL)[3]

  Cat<-array(SIL(DBF,"Cat"),c(nsim,nyears,nf))
  CAA<-array(SIL(DBF,"CAA"),c(nsim,nyears,na,nf))
  CAL<-array(SIL(DBF,"CAL"),c(nsim,nyears,nl,nf))
  LFS<-array(SIL(DBF,"LFS"),c(nsim,nf))
  LFC<-array(SIL(DBF,"LFC"),c(nsim,nf))

  #ML<-array(SIL(DBF,"ML"),c(nsim,nyears,nf)) # for checking purposes
  #Lc<-array(SIL(DBF,"Lc"),c(nsim,nyears,nf))
  #Lbar<-array(SIL(DBF,"Lbar"),c(nsim,nyears,nf))

  # Additions
  Dataout@Cat<-apply(Cat,1:2,sum)
  Dataout@CAA<-apply(CAA,1:3,sum)
  Dataout@CAL<-apply(CAL,1:3,sum)

  # Weighted means
  Dataout@LFS<-apply(LFS*Cat[,nyears,],1,sum)/apply(Cat[,nyears,],1,sum)
  Dataout@LFC<-apply(LFC*Cat[,nyears,],1,sum)/apply(Cat[,nyears,],1,sum)

  # Recalculations
  MLbin <- (StockPars[[p]]$CAL_bins[1:(length(StockPars[[p]]$CAL_bins) - 1)] + StockPars[[p]]$CAL_bins[2:length(StockPars[[p]]$CAL_bins)])/2
  temp <- Dataout@CAL * rep(MLbin, each = nsim * nyears)
  Dataout@ML <- apply(temp, 1:2, sum)/apply(Dataout@CAL, 1:2, sum)
  Dataout@Lc <- array(MLbin[apply(Dataout@CAL, 1:2, which.max)], dim = c(nsim, nyears))
  nuCAL <- Dataout@CAL
  for (i in 1:nsim) for (j in 1:nyears) nuCAL[i, j, 1:match(max(1, Dataout@Lc[i, j]), MLbin, nomatch=1)] <- NA
  temp <- nuCAL * rep(MLbin, each = nsim * nyears)
  Dataout@Lbar <- apply(temp, 1:2, sum, na.rm=TRUE)/apply(nuCAL, 1:2, sum, na.rm=TRUE)
  #      cbind(ML[1,,],Dataout@ML[1,],rep(0,nyears),Lc[1,,],Dataout@Lc[1,],rep(0,nyears),Lbar[1,,],Dataout@Lbar[1,]) # check

  Dataout
}
