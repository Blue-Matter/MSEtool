
#' Slot in list: get the slot values from a list of objects
#'
#' Create of vector of values that correspond with a slot in a list of objects
#'
#' @param listy A list of objects
#' @param sloty A character vector representing the slot name
#' @author T. Carruthers
SIL <- function(listy, sloty) {
  if (methods::is(listy[[1]], "list")) {
    out <- list()
    for(i in 1:length(listy)) out[[i]] <- sapply(listy[[i]], slot, sloty)
    out <- do.call(c, out)
  } else {
    out <- sapply(listy, slot, sloty)
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
  if(!methods::is(listy[[1]], "list") && !lev1) {
    out <- list()
    for(i in 1:length(listy)) out[[i]] <- sapply(listy[[i]], getElement, namey)
    out <- do.call(c, out)
  }else{
    out <- sapply(listy, getElement, namey)
  }
  out
}

#' @name Herm-int 
#' @aliases expandHerm
#' @title Internal Herm functions
#' 
#' @description - `expandHerm` expands the Herm list in SexPars to a matrix of fractions at age
#'
#' @param Herm A list of Hermaphroditic fractions at age
#' @param maxage The maximum age of stocks being simulated
#' @param np The total number of stocks being simulated
#' @param nsim The number of simulations
#' @author T. Carruthers
expandHerm <- function(Herm, maxage, np, nsim) {
  n_age <- maxage + 1
  HermFrac <- array(1, c(nsim, np, n_age))
  if (length(Herm) > 0){
    ps <- matrix(as.numeric(sapply(names(Herm), function(x) strsplit(x,"_")[[1]][2:3])),
                 nrow = length(Herm), byrow = TRUE)
    for(i in 1:length(Herm)) {
      HermFrac[, ps[i,1], 1:ncol(Herm[[1]])] <- Herm[[1]][, , 1]
      HermFrac[, ps[i,2], ] <- 0
      HermFrac[, ps[i,2], 1:ncol(Herm[[1]])] <- 1 - Herm[[1]][, , 1]
    }
  }
  HermFrac
}


#' @rdname Herm-int 
#' @aliases checkHerm
#' 
#' @description - `checkHerm` checks that each array in the list has dimension nsim x maxage+1 x nyears + proyears.
#' For backwards compatibility, also converts matrices to arrays by adding the year dimension.
#' 
#' @param nyears The number of historical years
#' @param proyears The number of projection years
#' @author Q. Huynh
checkHerm <- function(Herm, maxage, nsim, nyears, proyears) {
  n_age <- maxage + 1
  
  if (length(Herm)) {
    for(i in 1:length(Herm)) {
      
      if (is.null(dim(Herm[[i]]))) {
        stop("Hermaphroditic inputs must be a matrix or array.")
      }
      
      if (is.matrix(Herm[[i]])) {
        if (!all(dim(Herm[[i]]) == c(nsim, n_age))) {
          stop("Herm[[", i, "]] must be a matrix with dimensions: nsim, maxage+1 but has dimensions: ",
               paste(dim(Herm[[i]]), collapse = ","))
        }
        Herm[[i]] <- replicate(nyears + proyears, Herm[[i]])
      }
      
      if (is.array(Herm[[i]])) {
        if (!all(dim(Herm[[i]]) == c(nsim, n_age, proyears + nyears))) {
          stop("Herm[[", i, "]] must be array with dimensions: nsim, maxage+1, nyears + proyears but has dimensions: ",
               paste(dim(Herm[[i]]), collapse = ","))
        }
      }
      
    }
  }
  
  return(Herm)
}

#' @rdname Herm-int 
#' @aliases subsetHerm
#' 
#' @description - `subsetHerm` returns year-specific Herm values.
#' 
#' @param y The year to subset
#' @author Q. Huynh
subsetHerm <- function(Herm, y) {
  if (length(Herm)) {
    Herm <- lapply(Herm, function(x) x[, , y])
  }
  return(Herm)
}

#' Tom's expand grid
#'
#' Create an indexing grid from just a vector of maximum dimension sizes
#'
#' @param vec A vector of maximum array sizes
#' @author T. Carruthers
TEG <- function(vec) { # make index for list calculation
  dims <- lapply(vec, seq_len)
  as.matrix(expand.grid(dims))
}

getLHpars <- function(x, name, StockPars, nyears) {
  sapply(StockPars, function(y) getElement(y, name)[x, 1:nyears], simplify = "array") %>% t()
}

#' Reconstruct historical dynamics
#'
#' Function that reconstructs historical stock trends from fitted qs and all other parameters including MICE components
#'
#' @param x Integer, the simulation number
#' @param StockPars A list of sampled stock parameters, one list element per stock
#' @param FleetPars A hierarchical list of sampled fleet parameters, first list level is stock, second is fleet
#' @param np The number of stocks
#' @param nf The number of fleets
#' @param nareas The number of areas
#' @param maxage The maximum number of modeled ages
#' @param nyears The number of historical 'spool-up' years (from unfished to now)
#' @param N An array of stock numbers `[nsim,np,maxage,nyears,nareas]` - only the values from the first year are used
#' @param VF An array of vulnerability `[nsim,np,nf,maxage,nyears+proyears]`
#' @param FretA An array of retention `[nsim,np,nf,maxage,nyears+proyears]`
#' @param maxF A numeric value specifying the maximum fishing mortality for any single age class
#' @param MPA An of spatial closures by year `[np,nf,nyears+proyears,nareas]`
#' @param Rel A list of inter-stock relationships see slot Rel of MOM object class
#' @param SexPars A list of sex-specific relationships (SSBfrom, stock_age)
#' @param qs Vector of q values
#' @param qfrac Array of qfrac
#' @param plusgroup Plusgroup - np-length vector
#' @author T.Carruthers
#' @keywords internal
HistMICE <- function(x, StockPars, FleetPars, np,nf, nareas, maxage, nyears, N, VF,
                     FretA, maxF = 0.9, MPA, Rel, SexPars, qs, qfrac,
                     plusgroup) {
  
  # Ensure this code matches getq_multi_MICE()
  n_age <- maxage + 1 # include age-0
  Nx <- array(N[x,,,,], c(np, n_age, nyears, nareas))
  VFx <- array(VF[x,,,,], c(np, nf, n_age, nyears))
  FretAx <- array(FretA[x,,,,] , c(np, nf, n_age, nyears))
  
  # Vectors of length np
  Kx <- sapply(StockPars, getElement, "K")[x, ]
  Linfx <- sapply(StockPars, getElement, "Linf")[x, ]
  t0x <- sapply(StockPars, getElement, "t0")[x, ]
  Mx <- sapply(StockPars, getElement, "M")[x, ]
  R0x <- sapply(StockPars, getElement, "R0")[x, ]
  SSB0x <- sapply(StockPars, getElement, "SSB0")[x, ]
  B0x <- sapply(StockPars, getElement, "B0")[x, ]
  
  hsx <- sapply(StockPars, getElement, "hs")[x, ]
  ax <- sapply(StockPars, getElement, "a")
  bx <- sapply(StockPars, getElement, "b")
  SRrelx <- sapply(StockPars, getElement, "SRrel")[x, ]
  
  # Matrix np x nyears + nage
  Perrx <- sapply(1:np, function(p) StockPars[[p]]$Perr_y[x, 1:(nyears + n_age)]) %>% t()
  
  # Matrix np x nage x areas x areas x nyears+1
  movx <- sapply(1:np, function(p) StockPars[[p]]$mov[x, , , , 0:nyears + 1], simplify = "array") %>% aperm(c(5, 1:4))
  
  # Matrix np x nareas
  distx <- sapply(1:np, function(p) StockPars[[p]]$R0a[x, ]/sum(StockPars[[p]]$R0a[x, ])) %>% t()
  SSBpRx <- sapply(1:np, function(p) StockPars[[p]]$SSBpR[x, ]) %>% t()
  R0ax <- sapply(1:np, function(p) StockPars[[p]]$R0a[x, ]) %>% t()
  aRx <- sapply(1:np, function(p) StockPars[[p]]$aR[x, ]) %>% t()
  bRx <- sapply(1:np, function(p) StockPars[[p]]$bR[x, ]) %>% t()
  Asizex <- sapply(1:np, function(p) StockPars[[p]]$Asize[x, ]) %>% t()
  
  # Arrays np x nage x nyears + 1
  Mat_agex <- sapply(1:np, function(p) StockPars[[p]]$Mat_age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  Fec_agex <- sapply(1:np, function(p) StockPars[[p]]$Fec_Age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  M_ageArrayx <- sapply(1:np, function(p) StockPars[[p]]$M_ageArray[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  WatAgex <- sapply(1:np, function(p) StockPars[[p]]$Wt_age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  Len_agex <- sapply(1:np, function(p) StockPars[[p]]$Len_age[x, , 0:nyears + 1], simplify = "array") %>%
    aperm(c(3, 1:2))
  
  # Array np x nf x nyears
  Effind <- sapply(1:np, function(p) {
    sapply(1:nf, function(f) FleetPars[[p]][[f]][["Find"]][x, ]) %>% matrix(ncol = nf)
  }, simplify = "array") %>% aperm(3:1)
  
  # Matrix np x nf
  Spat_targ <- sapply(1:np, function(p) {
    sapply(1:nf, function(f) FleetPars[[p]][[f]][["Spat_targ"]][x])
  }) %>% matrix(nf, np) %>% t()
  
  # Matrix np x nyears + 1 
  Karrayx <- getLHpars(x, 'Karray', StockPars, nyears + 1)
  Linfarrayx <- getLHpars(x, 'Linfarray', StockPars, nyears + 1) 
  t0arrayx <- getLHpars(x, 't0array', StockPars, nyears + 1) 
  Marrayx <- getLHpars(x, 'Marray', StockPars, nyears + 1) 

  qsx <- qs[x, ]
  qfracx <- array(qfrac[x,,], c(np, nf))
  
  spawn_time_frac <- rep(0, np)
  for (p in 1:np) {
    spawn_time_frac[p] <- StockPars[[p]]$spawn_time_frac[x]
  }
  
  SRRfun_p <- lapply(1:np, function(p) StockPars[[p]]$SRRfun)
  SRRpars_p <- lapply(1:np, function(p) StockPars[[p]]$SRRpars[[x]])
  
  popdynMICE(qsx = qsx, qfracx = qfracx, np = np, nf = nf, nyears = nyears, nareas = nareas, maxage = maxage, 
             Nx = Nx, VFx = VFx, FretAx = FretAx, Effind = Effind,
             movx = movx, Spat_targ = Spat_targ, M_ageArrayx = M_ageArrayx, Mat_agex = Mat_agex, Fec_agex = Fec_agex,
             Asizex = Asizex, WatAgex = WatAgex, Len_agex = Len_agex, Karrayx = Karrayx,
             Linfarrayx = Linfarrayx, t0arrayx = t0arrayx, Marrayx = Marrayx,
             R0x = R0x, R0ax = R0ax, SSBpRx = SSBpRx, hsx = hsx, aRx = aRx, bRx = bRx,
             ax = ax, bx = bx, Perrx = Perrx, SRrelx = SRrelx, Rel = Rel, SexPars = SexPars, x = x,
             plusgroup = plusgroup, maxF = maxF, SSB0x = SSB0x, B0x = B0x, MPA,
             SRRfun=SRRfun_p, SRRpars=SRRpars_p,
             spawn_time_frac=spawn_time_frac)

}

# Normalize an array over certain dimensions
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
  if(!methods::is(x,'list')){
    #message(paste(deparse(substitute(x))," is not a list - ldim operates on hieracical list objects"))
    return(0)
  }else{
    dim<-NULL
    cond=TRUE
    while(cond){
      dim=c(dim,length(x))
      cond=methods::is(x[[1]], 'list')
      if(cond)x=x[[1]]
    }
    return(dim)
  }
}


#' Combine data among stocks
#'
#' Catches, CAA, CAL are summed. Indices, LFC and LFS are weighted averages. ML, Lc and Lbar are recalculated from summed CAL.
#' All other observations are for fleet 1 and weighted average across stocks
#'
#' @param MSElist A hierarchical list of data objects stock then fleet then MP
#' @param Real.Data.Map Matrix describing which data are mapped across stocks
#' @param np The number of stocks
#' @param mm Integer the MP number
#' @param nf The number of fleets
#' @param realVB A matrix of real vulnerable biomass `[nsim,np, year]`
#' @author T. Carruthers
#' @export
multiDataS<-function(MSElist,Real.Data.Map,np,mm,nf,realVB){
  
  # check Data mirroring
  unique.data <- unique(Real.Data.Map[1,]) # currently only works for single fleet 
  np <- length(unique.data)
  
  # check dimensions of CAL - need to be the same for all stocks/fleets
  nL <- matrix(NA, np,nf)
  # nA <- matrix(NA, np,nf)
  for (p in unique.data) {
    for(f in 1:nf) {
      nL[p,f]<-dim(MSElist[[p]][[f]][[mm]]@CAL)[3]
      # nA[p,f]<-dim(MSElist[[p]][[f]][[mm]]@CAA)[3]
    }
  }
  
  if (length(unique(as.numeric(nL)))>1) {
    stop('All stocks/fleets must have the same length bins if using Complex mode. Use `cpars$CAL_bins` or `cpars$CAL_binsmid`')
  }

  nsim<-dim(MSElist[[1]][[1]][[mm]]@Cat)[1]
  nyears<-dim(MSElist[[1]][[1]][[mm]]@Cat)[2]
  na<-dim(MSElist[[1]][[1]][[mm]]@CAA)[3]
  nl<-dim(MSElist[[1]][[1]][[mm]]@CAL)[3]
  ni<-np*nf

  if(realVB[1,1,1]==0)realVB[,,1]<-realVB[,,2] # impute vulnerable biomass for year 1 if missing (a negligible issue to be fixed in popdyn_MICE)

  DBF<-list()
  realVBi<-array(NA,c(nsim,nyears,nf*np))

  for(f in 1:nf){
    for(p in unique.data){
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
  Dataout@LFS<-apply(LFS*Cat[,nyears,],1,sum)/apply(Cat[,nyears,, drop=FALSE],1,sum)
  Dataout@LFC<-apply(LFC*Cat[,nyears,],1,sum)/apply(Cat[,nyears,, drop=FALSE],1,sum)

  # Data among stocks have varying length bins so we resort to weighted averages here
  Dataout@ML<-apply(ML*Cat,1:2,sum)/apply(Cat,1:2,sum)
  Dataout@Lc<-apply(Lc*Cat,1:2,sum)/apply(Cat,1:2,sum)
  Dataout@Lbar<-apply(Lbar*Cat,1:2,sum)/apply(Cat,1:2,sum)

  Ind<-array(SIL(DBF,"Ind"),c(nsim,nyears,ni))
  Rec<-array(SIL(DBF,"Ind"),c(nsim,nyears,ni))
  Dataout@Ind<-apply(Ind*realVBi,1:2,sum)/apply(realVBi,1:2,sum)
  Dataout@Rec<-apply(Rec*realVBi,1:2,sum)/apply(realVBi,1:2,sum)

  AvC<-array(SIL(DBF,"AvC"),c(nsim,ni))
  Dataout@AvC<-apply(AvC,1,sum)

  popsimslot<-function(Dataout,sloty,realVBi,nsim,ni){
    temp<-array(SIL(DBF,sloty),c(nsim,ni))
    slot(Dataout,sloty)<-apply(temp*realVBi[,nyears,],1,sum)/apply(realVBi[,nyears,, drop=FALSE],1,sum)
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

  # last historical F
  FinF <- rep(0,nsim)
  for (fl in 1:nf) {
    FinF <- FinF + DBF[[fl]]@OM$FinF
  }
  Dataout@OM$FinF <- FinF


  # add Misc slot from first stock and fleet
  Dataout@Misc <- MSElist[[1]][[1]][[mm]]@Misc
  # for(p in 1:np){
  #   Dataout@Misc[[p]] <- list()
  #   for(f in 1:nf){
  #     Dataout@Misc[[p]][[f]] <- MSElist[[p]][[f]][[mm]]@Misc
  #   }
  # }
  
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
#' @param MSElist A hierarchical list of data objects stock then fleet then MP
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

