library(MSEtool)

devtools::load_all()

SSdir <- 'G:/My Drive/1_PROJECTS/North_Atlantic_Swordfish/OMs/grid_2022/000_base_case'

MOM <- SS2MOM(SSdir=SSdir,nsim=5, Name='North Atlantic Swordfish') 

OM <- OM2om(MOM)
OM@nYears
OM@pYears

object.size(MOM) |> format(units='Mb')
object.size(OM)  |> format(units='Mb')

om2OM <- function(OM) {
  
  
  
  MOM <- new('MOM', Name=OM@Name)
  

    
}

getUniformBounds <- function(object,name='NaturalMortality', Par="M") {
  sl <- slot(object, name)
  val <- sl@Pars[[Par]]
  if (methods::is(val, 'array')) {
    stop('not done')
  }
  if (methods::is(val, 'numeric')) {
    if (length(val)==1)
      return(rep(val,2))
    if (length(val)==2)
      return(val)
  }
  if (is.null(val))
    return(numeric())
  val
}

setCparValues <- function(val, dim=1, nsim, nTS=NULL) {
  dd <- dim(val)
  
  if (length(dd)==2) {
    if (dd[1]==nsim)
      return(val)
    
    val <- abind::adrop(replicate(nsim, val), 1) |> t()
    return(val)
  }
  
  
  if (length(dd)==3) {
    if (all(dd==1))
      return(rep(val[1,1,1], nsim))

    
    if (dd[1]==1) {
      val <- abind::adrop(replicate(nsim, val), drop=1) |> aperm(c(3,1,2))
    }
    if (dd[3]==1) {
      val <- abind::adrop(replicate(nTS, val), drop=3) 
    }
    return(val)
    
  }
  if (is.null(dd) & dim==1)
    return(rep(val, nsim)[1:nsim])
  
  
}

getRecDevs <- function(Stock, nsim) {
  RecDevInit <- Stock@SRR@RecDevInit
  if (is.null(dim(RecDevInit)))
    RecDevInit <-  matrix(RecDevInit, nrow=nsim, ncol=length(RecDevInit), byrow=TRUE)
  
  RecDevHist <- Stock@SRR@RecDevHist
  if (is.null(dim(RecDevHist)))
    RecDevHist <- matrix(Stock@SRR@RecDevHist, nrow=nsim, ncol=length(Stock@SRR@RecDevHist), byrow=TRUE)
  
  RecDevProj <- Stock@SRR@RecDevProj
  if (is.null(dim(RecDevProj)))
    RecDevProj <- matrix(Stock@SRR@RecDevProj, nrow=nsim, ncol=length(Stock@SRR@RecDevProj), byrow=TRUE)
  
  
  cbind(RecDevInit, RecDevHist, RecDevProj)
} 

om2MOM <- function(OM) {
  OM <- Populate(OM, silent=TRUE)
  nsim <- nSim(OM)
  nTS <- length(TimeSteps(OM))
  
  # everything is done in cpars
  Stocks <- vector('list', nStock(OM))
  Fleets <- vector('list', nStock(OM))
  cpars <- vector('list', nStock(OM))
  for (st in 1:nStock(OM)) {
    Fleets[[st]] <- vector('list', nFleet(OM))
    cpars[[st]] <- vector('list', nFleet(OM))
    
    stock <- new('Stock')
    stock@Name <- OM@Stock[[st]]@Name
    stock@Common_Name <- OM@Stock[[st]]@CommonName
    stock@Species <- OM@Stock[[st]]@Species
    stock@maxage <- OM@Stock[[st]]@Ages@MaxAge
    stock@R0 <- OM@Stock[[st]]@SRR@Pars$R0
    
    stock@h <- getUniformBounds(OM@Stock[[st]], 'SRR', 'h')
    sltnames <- slotNames('Stock')
    sltnames <- sltnames[!sltnames %in% c('Name', 'Common_Name',
                                          'Species', 'maxage', 'R0',
                                          'Source', 'SRrel', 'h')]
    
    stock@SRrel <- 1 # hard-coded to BH right now
    
    
    for (sl in sltnames) {
      slot(stock, sl) <- rep(0,2)
    }
    
    Stocks[[st]] <- stock
    
    for (fl in 1:nFleet(OM)) {
      fleet <- new('Fleet')
      fleet@Name <- OM@Fleet[[st]][[fl]]@Name
      sltnames <- slotNames('Fleet')
      sltnames <- sltnames[!sltnames %in% c('Name', 'Misc',
                                            'isRel',
                                            'MPA',
                                            'CurrentYr')]
      fleet@isRel <- FALSE
      
      for (sl in sltnames) {
        slot(fleet, sl) <- rep(0,2)
      }
      fleet@nyears <- nYears(OM)
      fleet@CurrentYr <- OM@CurrentYear
      Fleets[[st]][[fl]] <- fleet
      
      Cpars <- list()
      Cpars$Data <- OM@Data[[st]][[fl]]
      if (fl ==1) {
        Cpars$R0 <- setCparValues(OM@Stock[[st]]@SRR@Pars$R0, 1, nsim)
        Cpars$M <- setCparValues(OM@Stock[[st]]@NaturalMortality@Pars$M, 1, nsim)
        Cpars$Msd <- setCparValues(OM@Stock[[st]]@NaturalMortality@Pars$MSD, 1, nsim)
        Cpars$h <- setCparValues(OM@Stock[[st]]@SRR@Pars$h, 1, nsim)
        Cpars$Perr <-  setCparValues(OM@Stock[[st]]@SRR@SD, 1, nsim)
        Cpars$AC <-  setCparValues(OM@Stock[[st]]@SRR@AC, 1, nsim)
        
        Cpars$Linf <-  setCparValues(OM@Stock[[st]]@Length@Pars$Linf, 1, nsim)
        Cpars$Linfsd <-  setCparValues(OM@Stock[[st]]@Length@Pars$LinfSD, 1, nsim)
        Cpars$K <-  setCparValues(OM@Stock[[st]]@Length@Pars$K, 1, nsim)
        Cpars$Ksd <-  setCparValues(OM@Stock[[st]]@Length@Pars$KSD, 1, nsim)
        
        Cpars$t0 <-  setCparValues(OM@Stock[[st]]@Length@Pars$t0, 1, nsim)
        Cpars$LenCV <-  setCparValues(OM@Stock[[st]]@Length@CVatAge, 1, nsim)
        
        Cpars$L50 <- setCparValues(OM@Stock[[st]]@Maturity@Pars$L50, 1, nsim)
        Cpars$L50_95 <- setCparValues(OM@Stock[[st]]@Maturity@Pars$L50_95, 1, nsim)
        
        Cpars$D <- setCparValues(OM@Stock[[st]]@Depletion@Final, 1, nsim)
        
        # TODO
        Cpars$Size_area_1 <- rep(0.5, nsim) # setCparValues(OM@Stock[[st]]@Spatial@RelativeSize, 1, nsim)
        Cpars$Frac_area_1 <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@UnfishedDist, 1, nsim)
        Cpars$Prob_staying <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@ProbStaying, 1, nsim)
        
        Cpars$hs <- setCparValues(OM@Stock[[st]]@SRR@Pars$h, 1, nsim)
        
 
        Cpars$Perr_y <- getRecDevs(OM@Stock[[st]], nsim)
        
        Cpars$Len_age <- setCparValues(val=OM@Stock[[st]]@Length@MeanAtAge, 3, nsim, nTS)
        
        Cpars$LatASD <- setCparValues(val=OM@Stock[[st]]@Length@CVatAge, 3, nsim, nTS) *
          setCparValues(val=OM@Stock[[st]]@Length@MeanAtAge, 3, nsim, nTS)
        
        Cpars$Wt_age  <- setCparValues(val=OM@Stock[[st]]@Weight@MeanAtAge, 3, nsim, nTS)
        Cpars$Mat_age <- setCparValues(val=OM@Stock[[st]]@Maturity@MeanAtAge, 3, nsim, nTS)
        
        Cpars$M_ageArray <- setCparValues(val=OM@Stock[[st]]@NaturalMortality@MeanAtAge, 3, nsim, nTS)
        Cpars$Fec_age <- setCparValues(val=OM@Stock[[st]]@Fecundity@MeanAtAge, 3, nsim, nTS)
        Cpars$Wa 
        Cpars$Wb  
        
         Cpars$CAL_bins
        Cpars$CAL_binsmid
        Cpars$binWidth
        
   
        
        Cpars$ageMarray
        Cpars$age95array
        
        Cpars$Marray
        
        Cpars$mov
        
        Cpars$L95
        Cpars$Asize
        Cpars$ageM
        Cpars$age95
        Cpars$initD 
       
        
      
        
        
      } 
      
      Cpars$Fdisc <- setCparValues(OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge[,1,1], 1, nsim)
      Cpars$qinc <- setCparValues(OM@Fleet[[st]][[fl]]@Effort@qInc, 1, nsim)
      Cpars$qcv <- setCparValues(OM@Fleet[[st]][[fl]]@Effort@qCV, 1, nsim)
          
      Cpars$L5 <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$L5, 1, nsim)
      Cpars$LFS <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LFS, 1, nsim)
      Cpars$Vmaxlen <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$Vmaxlen, 1, nsim)
      
      Cpars$LR5 <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LR5, 1, nsim)
      Cpars$LFR <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LFR, 1, nsim)
      Cpars$Rmaxlen <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$Rmaxlen, 1, nsim)
      
      Cpars$Find <- setCparValues(val=OM@Fleet[[st]][[fl]]@Effort@Effort, 2, nsim)
      Cpars$qs <- setCparValues(val=OM@Fleet[[st]][[fl]]@Effort@Catchability, 1, nsim)
      
      Cpars$Wt_age_C <- setCparValues(val=OM@Fleet[[st]][[fl]]@Weight, 3, nsim, nTS)
      
      Cpars$V <- setCparValues(val=OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge, 3, nsim, nTS)
      Cpars$SLarray <- setCparValues(val=OM@Fleet[[st]][[fl]]@Selectivity@MeanAtLength, 3, nsim, nTS)
      
      Cpars$retA <- setCparValues(val=OM@Fleet[[st]][[fl]]@Retention@MeanAtAge, 3, nsim, nTS)
      Cpars$retL <- setCparValues(val=OM@Fleet[[st]][[fl]]@Retention@MeanAtLength, 3, nsim, nTS)
      Cpars$DR <- rep(0, nsim)
      Cpars$dFfinal
      
      Cpars$qvar
     
      
    
      Cpars$V_real
      Cpars$retA_real
      Cpars$retL_real
      Cpars$SLarray_real
      Cpars$Fdisc_array1
      Cpars$Fdisc_array2
      
      # Obs and Imp to do ...
      
    
      cpars[[st]][[fl]] <- Cpars
    }
  }
  
  new('MOM',
      nsim=nsim,
      proyears=pYears(OM),
      Stocks=Stocks,
      Fleets=Fleets,
      Obs=OM@Obs, # currently unchanged
      Imps=OM@Imp, # currently unchanged
      CatchFrac=OM@CatchFrac,
      cpars=cpars,
      interval=OM@Interval,
      pstar=OM@pStar,
      maxF=OM@maxF,
      reps=OM@nReps,
      Allocation=OM@Allocation,
      Efactor=OM@Efactor,
      Complexes=OM@Complexes,
      SexPars=OM@SexPars,
      Rel=OM@Relations)
  
}

  
newMOM <- om2MOM(OM)


Hist <- Simulate(newMOM)


# TODO - add Data


OM <- Populate(OM)


object.size(MOM) |> format(units='Mb')
object.size(OM)  |> format(units='Mb')



devtools::load_all()





