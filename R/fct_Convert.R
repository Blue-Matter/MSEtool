#' Convert old style and new style object classes
#' @include MOM_object.r
#'
Convert <- function(x, ...) {
  
  if (inherits(x, 'OM'))
    return(ConvertOM(x, ...))
  
} 


OM2om <- function(OM, Author='', CurrentYear=NULL, Populate=TRUE) {

  
  if (methods::is(OM, 'OM')) {

 

  }

  if (methods::is(OM, 'MOM')) {
    om@Stock <- ConvertToList(MOM2stock(OM, TimeSteps))
    names(om@Stock) <- names(OM@Stocks)
    class(om@Stock) <- 'StockList'
    om@Fleet <- list()
    class(om@Fleet) <- 'StockFleetList'
    for (st in 1:length(om@Stock)) {
      om@Fleet[[st]] <- MOM2fleet(OM, st)
      names(om@Fleet[[st]]) <- names(OM@Fleets[[st]])
      class(om@Fleet[[st]]) <- 'FleetList'
    }
    om@Obs <- OM@Obs
    om@Imp <- OM@Imps
    om@CatchFrac <- OM@CatchFrac
    om@Allocation <- OM@Allocation
    
    # 
    if (!is.null(OM@SexPars$SSBfrom)) {
      for (st in 1:length(om@Stock)) {
        ind <- which(OM@SexPars$SSBfrom[st,]==1)
        if (ind != st)
          om@Stock[[st]]@SRR@SPFrom <- ind
      }
    }
   
    # TODO might remove SexPars slot later
    om@SexPars <- ImportSexPars(OM)
  
    om@Efactor <- OM@Efactor
    om@Complexes<- OM@Complexes
    om@Relations <- OM@Rel
    om@Data <- vector('list', nStock(om))
    om@Misc$cpars <- vector('list', nStock(om))
    for (st in 1:nStock(om)) {
      om@Data[[st]] <- vector('list', nFleet(om))
      om@Misc$cpars[[st]] <- vector('list', nFleet(om))
      for (fl in 1:nFleet(om)) {
        l <- OM@cpars[[st]][[fl]]
        om@Data[[st]][[fl]] <- l$Data
        l$Data <- NULL
        l <- l[!names(l)%in%cparsdrop$Var]
        om@Misc$cpars[[st]][[fl]] <- l
      }
    }
    # reduce time-series dimension
    om <- ArrayReduceDims(om)
  }
  if (Populate)
    om <- Populate(om)
  om
}

ImportSexPars <- function(OM) {
  sexpars <- new('sexpars')
  # TODO remove SPFrom functions and slot 
  # if (!is.null(OM@SexPars$SSBfrom)) {
  #   SPFrom(sexpars) <- OM@SexPars$SSBfrom
  #   rownames(SPFrom(sexpars)) <- names(OM@Stocks)
  #   colnames(SPFrom(sexpars)) <- names(OM@Stocks)
  # }
    
  
  if (!is.null(OM@SexPars$Herm))
    Herm(sexpars) <- OM@SexPars$Herm
  
  if (!is.null(OM@SexPars$share_par))
    SharePar(sexpars) <- OM@SexPars$share_par
  sexpars
}


AdjustEffort <- function(FishingMortality, DiscardMortality) {
  d2 <- dim(DiscardMortality@MeanAtAge)

  if (max(FishingMortality@nVessels)>0 & !is.null(d2)) {
    nyear <- FishingMortality@nYear
    if (d2[3]>1) {
      nyear <- min(d2[3], nyear)
      meanFdisc <- apply(DiscardMortality@MeanAtAge[,,1:nyear, drop=FALSE],c(1,3) , mean)
      FishingMortality@nVessels <- FishingMortality@nVessels/meanFdisc
    } else {
      meanFdisc <- apply(DiscardMortality@MeanAtAge[,,1, drop=FALSE],1 , mean)
      FishingMortality@nVessels <- FishingMortality@nVessels/meanFdisc
    }
  }
  FishingMortality
}






















MOM2fleet <- function(MOM, st) {

  nfleets <- length(MOM@Fleets[[1]])
  FleetNames <- names(MOM@Fleets[[1]])
  if (is.null(FleetNames))
    FleetNames <- paste('Fleet', 1:nfleets)

  FleetList <- list()

  for (fl in 1:nfleets) {
    Fleet <- MOM@Fleets[[st]][[fl]]
    cpars <- MOM@cpars[[st]][[fl]]
    FleetList[[fl]] <- OM2fleet(Fleet, cpars)
  }
  names(FleetList) <- FleetNames
  if (nfleets==1) return(FleetList[[1]])
  FleetList
}

MOM2stock <- function(MOM, TimeSteps=NULL) {
  StockList <- list()
  stocks <- MOM@Stocks
  nstocks <- length(stocks)

  for (st in 1:nstocks) {
    Stock <- stocks[[st]]
    cpars <- MOM@cpars[[st]][[1]]
    StockList[[st]] <- OM2stock(Stock, cpars, TimeSteps)
  }
  if (nstocks==1) return(StockList[[1]])
  StockList
}


# IdenticalSim <- function(value) {
#   dd <- dim(value)
#   if (is.null(dd))
#     return(mean(value) == value[1])
# 
#   temp <- apply(value, 1, mean)
#   mean(temp) == temp[1]
# 
# }

# IdenticalTime <- function(value) {
#   dd <- dim(value)
#   if (length(dd)==2)
#     return(all(value[1,] == mean(value[1,])))
# 
#   if (length(dd)==3)
#     return(all(value[1,1,] == mean(value[1,1,])))
# }

get_cpars <- function(OM, st=1, fl=1) {
  cpars <- OM@cpars
  if (!length(cpars)) return(NULL)

  if (inherits(OM, 'OM')) {
    return(OM@cpars)
  }

  if (inherits(OM, 'MOM')) {
    cpars <- try(MOM@cpars[[st]][[fl]], silent = TRUE)
    if (inherits(cpars, 'list')) {
      return(cpars)
    } else {
      return(NULL)
    }
  }
}

#' @export
ArrayReduceDims <- function(array, IncSim=TRUE, IncAge=FALSE, IncTimeStep=TRUE, debug=FALSE, silent=FALSE, id=NULL) {
  
  if (debug)
    print(class(array))
  
  if (!IncSim & !IncTimeStep & !IncAge) 
    return(array)
  
  if (!length(array))
    return(array)
  
  if (!is.array(array)) {
    if (isS4(array)) {
      if (!silent) {
        if (is.null(id)) {
          id <- cli::cli_progress_bar('Reducing dimensions to minimum size', type='tasks')
        } else {
          cli::cli_progress_update(id=id) 
        }
      }
      
      if (inherits(array, 'data'))
        return(array)
      slots <- slotNames(array)
      for (sl in slots) {
        slot(array, sl) <- Recall(slot(array, sl), IncSim, IncAge, IncTimeStep, debug, silent, id)
      }
      return(array)
    }
    if (is.list(array)) {
      if (length(array)) {
        for (i in 1:length(array)) {
          temp <- Recall(array[[i]], IncSim, IncAge, IncTimeStep, debug, silent, id)
          if (!is.null(temp))
          array[[i]] <- temp 
        }
        return(array)
      }
    }
  }
  
  dnames <- array |> dimnames() |> names()
  
  indSim <- which(dnames=='Sim')
  indAge <- which(dnames=='Age')
  indTimeStep <- which(dnames=='TimeStep')
  
  incSim <- length(indSim)
  incAge <- length(indAge)
  incTimeStep <- length(indTimeStep)
  
  if (!IncSim)
    incSim <- 0
  
  if (!IncAge)
    incAge <- 0
  
  if (!IncTimeStep)
    incTimeStep <- 0
  
  if (incSim & incTimeStep & incAge) {
    idenSim <- IdenticalSims(array) 
    idenTime <- IdenticalTimeSteps(array)
    idenAge <- IdenticalAge(array)
  
    if (idenSim & idenTime & idenAge) 
      return(abind::asub(array, list(1,1,1), c(indSim, indAge, indTimeStep), drop=FALSE))
      
    if (!idenSim & idenTime & idenAge) 
      return(abind::asub(array, list(1,1), c(indAge, indTimeStep), drop=FALSE))
      
    if (idenSim & !idenTime & idenAge) {
      return(abind::asub(array, list(1,1,UniqueTimeSteps(array)), c(indSim, indAge, indTimeStep), drop=FALSE))
    }
      
    if (!idenSim & !idenTime & idenAge) 
      return(abind::asub(array, list(1,UniqueTimeSteps(array)), c(indAge, idenTime), drop=FALSE))
      
    if (idenSim & idenTime & !idenAge)
      return(abind::asub(array, list(1,1), c(indSim, indTimeStep), drop=FALSE))
    
    if (!idenSim & idenTime & !idenAge) 
      return(abind::asub(array, list(1), c(indTimeStep), drop=FALSE))
      
    if (idenSim & !idenTime & !idenAge) 
      return(abind::asub(array, list(1, UniqueTimeSteps(array)), c(indSim, indTimeStep), drop=FALSE))
      
    if (!idenSim & !idenTime & !idenAge) {
      return(abind::asub(array, list(UniqueTimeSteps(array)), c(indTimeStep), drop=FALSE))
    }
      
  }
  
  if (!incSim & incTimeStep & incAge) {
    idenTime <- IdenticalTimeSteps(array)
    idenAge <- IdenticalAge(array)
    
    if (idenTime & idenAge) 
      return(abind::asub(array, list(1,1), c(indAge, indTimeStep), drop=FALSE))
    if (!idenTime & idenAge) 
      return(abind::asub(array, list(1, UniqueTimeSteps(array)), c(indAge, indTimeStep), drop=FALSE))
    
    if (idenTime & !idenAge)
      return(abind::asub(array, list(1), c(indTimeStep), drop=FALSE))
    
    if (!idenTime & !idenAge) 
      return(abind::asub(array, list(UniqueTimeSteps(array)), c(indTimeStep), drop=FALSE))
  }
  
  if (incSim & !incTimeStep & incAge) {
    idenSim <- IdenticalSims(array) 
    idenAge <- IdenticalAge(array)
    
    if (idenSim  & idenAge) 
      return(abind::asub(array, list(1,1), c(indSim, indAge), drop=FALSE))
    
    if (!idenSim & idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (idenSim & idenAge) 
      return(abind::asub(array, list(1,1), c(indSim, indAge), drop=FALSE))
    
    if (!idenSim & idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (idenSim & !idenAge)
      return(abind::asub(array, list(1,1), c(indSim), drop=FALSE))
    
    if (!idenSim & !idenAge) 
      return(array)
    
  }
  
  if (!incSim & !incTimeStep & incAge) {
    idenAge <- IdenticalAge(array)
    
    if (idenAge) 
      return(abind::asub(array, list(1), c(indAge), drop=FALSE))
    
    if (!idenAge) 
      return(array)
  }
  
  if (incSim & incTimeStep & !incAge) {
    idenSim <- IdenticalSims(array) 
    idenTime <- IdenticalTimeSteps(array)
    
    if (idenSim & idenTime) 
      return(abind::asub(array, list(1,1), c(indSim, indTimeStep), drop=FALSE))
    
    if (!idenSim & idenTime) 
      return(abind::asub(array, list(1,1), c(indTimeStep), drop=FALSE))
    
    if (idenSim & !idenTime) 
      return(abind::asub(array, list(1, UniqueTimeSteps(array)), c(indSim, indTimeStep), drop=FALSE))
    
    if (!idenSim & !idenTime) 
      return(abind::asub(array, list(UniqueTimeSteps(array)), c(indTimeStep), drop=FALSE))

  }
  
  if (!incSim & incTimeStep & !incAge) {
    idenTime <- IdenticalTimeSteps(array)
    if (idenTime) 
      return(abind::asub(array, list(1), c(indTimeStep), drop=FALSE))
    
    if (!idenTime) 
      return(abind::asub(array, UniqueTimeSteps(array), indTimeStep, drop=FALSE))
  }
  
  if (incSim & !incTimeStep & !incAge) {
    idenSim <- IdenticalSims(array) 
    if (idenSim) 
      return(abind::asub(array, list(1), c(indSim), drop=FALSE))
    if (!idenSim) 
      return(array)
  }
  

  
  array
}
  


process_cpars <- function(value) {
  
  if (is.null(value)) return()
  dd <- dim(value)
  if (is.null(dd)) {
    if (IdenticalSim(value)) {
      return(value[1])
    } else {
      return(value)
    }
  }

  if (length(dd)==2) {
    if (IdenticalSim(value) & IdenticalTime(value)) {
      return(value[1,1, drop=FALSE])
    }
    if (IdenticalSim(value) & !IdenticalTime(value)) {
      return(value[1,, drop=FALSE])
    }
    if (!IdenticalSim(value) & IdenticalTime(value)) {
      return(value[,1, drop=FALSE])
    }
    if (!IdenticalSim(value) & !IdenticalTime(value)) {
      return(value)
    }

  }

  if (length(dd)==3) {
    if (IdenticalSim(value) & IdenticalTime(value)) {
      return(value[1,,1, drop=FALSE])
    }
    if (IdenticalSim(value) & !IdenticalTime(value)) {
      return(value[1,,, drop=FALSE])
    }
    if (!IdenticalSim(value) & IdenticalTime(value)) {
      return(value[,,1, drop=FALSE])
    }
    if (!IdenticalSim(value) & !IdenticalTime(value)) {
      return(value)
    }
  }

  if (length(dd)==4) {
    if (IdenticalSim(value))
      return(value[1,,,,drop=FALSE])
  }
  if (length(dd)==5) {
    if (IdenticalSim(value))
      return(value[1,,,,,drop=FALSE])
  }

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
  
  if (length(dd)==2 & dim==1)
    return(rep(val[1,], nsim)[1:nsim])
  
  
  if (length(dd)==2 & dim==2) {
    if (dd[1]==nsim & dd[2]==nTS)
      return(val)
    if (dd[1]==1 & dd[2]==nTS) {
      val <- abind::adrop(replicate(nsim, val), 1) |> t()
    }
    if (dd[1]==1 & dd[2]==1) {
      val <- matrix(val, nsim, nTS)
    }
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


om2Stock <- function(OM, st=NULL) {
  
  stock <- new('Stock')
  if (methods::is(OM@Stock, 'list')) {
    stock@Name <- OM@Stock[[st]]@Name
    stock@Common_Name <- OM@Stock[[st]]@CommonName
    stock@Species <- OM@Stock[[st]]@Species
    stock@maxage <- OM@Stock[[st]]@Ages@MaxAge
    stock@R0 <- OM@Stock[[st]]@SRR@Pars$R0
    stock@h <- getUniformBounds(OM@Stock[[st]], 'SRR', 'h')
  } else {
    stock@Name <- OM@Stock@Name
    stock@Common_Name <- OM@Stock@CommonName
    stock@Species <- OM@Stock@Species
    stock@maxage <- OM@Stock@Ages@MaxAge
    stock@R0 <- OM@Stock@SRR@Pars$R0
    stock@h <- getUniformBounds(OM@Stock, 'SRR', 'h')
  }
  
  sltnames <- slotNames('Stock')
  sltnames <- sltnames[!sltnames %in% c('Name', 'Common_Name',
                                        'Species', 'maxage', 'R0',
                                        'Source', 'SRrel', 'h')]
  
  stock@SRrel <- 1 # hard-coded to BH right now
  for (sl in sltnames) {
    slot(stock, sl) <- rep(0,2)
  }
  stock
}

om2Fleet <- function(OM, st=NULL, fl=NULL) {
  
  
  fleet <- new('Fleet')
  sltnames <- slotNames('Fleet')
  sltnames <- sltnames[!sltnames %in% c('Name', 'Misc',
                                        'isRel',
                                        'MPA',
                                        'CurrentYr')]
  for (sl in sltnames) {
    slot(fleet, sl) <- rep(0,2)
  }
  
  if (methods::is(OM@Fleet, 'list')) {
    fleet@Name <- OM@Fleet[[st]][[fl]]@Name
  } else {
    fleet@Name <- OM@Fleet@Name
  }
  
  
  fleet@isRel <- FALSE
  fleet@nYear <- nYear(OM)
  fleet@CurrentYr <- OM@CurrentYear
  fleet
}

om2mov <- function(Movement, nsim, maxage, narea, nTS) {
  if (is.null(Movement))
    return(NULL)
  
  movement <- Movement |> aperm(c(1,4,2,3,5)) # different order 
  
  dd <- dim(movement)
  if (dd[2] ==1) {
    movement <- abind::adrop(replicate(maxage+1, movement),2) |>
      aperm(c(1,5,2,3,4))
  }
  
  if (dd[5]==1) {
    movement <- abind::adrop(movement[,,,,1, drop=FALSE], 5)
  } 
  movement
}

om2Cpars <- function(OM, st=NULL, fl=NULL) {
  Cpars <- list()
  nsim <- OM@nSim
  nTS <- length(OM@TimeSteps)
  
  if (isS4(OM@Data)) {
    Cpars$Data <- OM@Data
  } else {
    Cpars$Data <- OM@Data[[st]][[fl]]
  }
  
  if (isS4(OM@Stock)) {
    Cpars$R0 <- setCparValues(OM@Stock@SRR@Pars$R0, 1, nsim)
    Cpars$M <- setCparValues(OM@Stock@NaturalMortality@Pars$M, 1, nsim)
    Cpars$Msd <- setCparValues(OM@Stock@NaturalMortality@Pars$MSD, 1, nsim)
    Cpars$h <- setCparValues(OM@Stock@SRR@Pars$h, 1, nsim)
    Cpars$Perr <-  setCparValues(OM@Stock@SRR@SD, 1, nsim)
    Cpars$AC <-  setCparValues(OM@Stock@SRR@AC, 1, nsim)
    
    Cpars$Linf <-  setCparValues(OM@Stock@Length@Pars$Linf, 1, nsim)
    Cpars$Linfsd <-  setCparValues(OM@Stock@Length@Pars$LinfSD, 1, nsim)
    Cpars$K <-  setCparValues(OM@Stock@Length@Pars$K, 1, nsim)
    Cpars$Ksd <-  setCparValues(OM@Stock@Length@Pars$KSD, 1, nsim)
    
    Cpars$t0 <-  setCparValues(OM@Stock@Length@Pars$t0, 1, nsim)
    dd <- dim(OM@Stock@Length@CVatAge)
    if (all(dd==1)) {
      Cpars$LenCV <-  setCparValues(OM@Stock@Length@CVatAge, 1, nsim, nTS)
    }
    Cpars$L50 <- setCparValues(OM@Stock@Maturity@Pars$L50, 1, nsim)
    Cpars$L50_95 <- setCparValues(OM@Stock@Maturity@Pars$L50_95, 1, nsim)
    Cpars$D <- setCparValues(OM@Stock@Depletion@Final, 1, nsim)
    
    # TODO
    Cpars$Size_area_1 <- rep(0.5, nsim) # setCparValues(OM@Stock[[st]]@Spatial@RelativeSize, 1, nsim)
    Cpars$Frac_area_1 <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@UnfishedDist, 1, nsim)
    Cpars$Prob_staying <- rep(0.5, nsim) #setCparValues(OM@Stock[[st]]@Spatial@ProbStaying, 1, nsim)
    
    Cpars$hs <- setCparValues(OM@Stock@SRR@Pars$h, 1, nsim)
    
    
    Cpars$Perr_y <- getRecDevs(OM@Stock, nsim)
    
    Cpars$Len_age <- setCparValues(val=OM@Stock@Length@MeanAtAge, 3, nsim, nTS)
    
    Cpars$LatASD <- setCparValues(val=OM@Stock@Length@CVatAge, 3, nsim, nTS) *
      setCparValues(val=OM@Stock@Length@MeanAtAge, 3, nsim, nTS)
    
    Cpars$Wt_age  <- setCparValues(val=OM@Stock@Weight@MeanAtAge, 3, nsim, nTS)
    Cpars$Mat_age <- setCparValues(val=OM@Stock@Maturity@MeanAtAge, 3, nsim, nTS)
    
    Cpars$M_ageArray <- setCparValues(val=OM@Stock@NaturalMortality@MeanAtAge, 3, nsim, nTS)
    Cpars$Fec_age <- setCparValues(val=OM@Stock@Fecundity@MeanAtAge, 3, nsim, nTS)
    
    Cpars$CAL_binsmid <- OM@Stock@Length@Classes
    by <- Cpars$CAL_binsmid[2] - Cpars$CAL_binsmid[1]
    Cpars$CAL_bins <- seq(Cpars$CAL_binsmid[1]-0.5*by, by=by, length.out=length(Cpars$CAL_binsmid)+1)
    Cpars$binWidth <- by
    
    Cpars$mov <- om2mov(OM@Stock@Spatial@Movement, nsim, OM@Stock@Ages@MaxAge, nArea(OM), length(TimeSteps(OM)))
    Cpars$initD <- setCparValues(val=OM@Stock@Depletion@Initial, 1, nsim, nTS)
    
  } else {
    if (fl==1) {
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
      dd <- dim(OM@Stock[[st]]@Length@CVatAge)
      if (all(dd==1)) {
        Cpars$LenCV <-  setCparValues(OM@Stock[[st]]@Length@CVatAge, 1, nsim, nTS)
      }
      
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
      
      Cpars$CAL_binsmid <- OM@Stock[[st]]@Length@Classes
      by <- Cpars$CAL_binsmid[2] - Cpars$CAL_binsmid[1]
      Cpars$CAL_bins <- seq(Cpars$CAL_binsmid[1]-0.5*by, by=by, length.out=length(Cpars$CAL_binsmid)+1)
      Cpars$binWidth <- by
      
      Cpars$mov <- om2mov(OM@Stock[[st]]@Spatial@Movement, nsim, OM@Stock@Ages@MaxAge, nArea(OM), length(TimeSteps(OM)))
      Cpars$initD <- setCparValues(val=OM@Stock[[st]]@Depletion@Initial, 1, nsim, nTS)
    }
  }
  
  if (isS4(OM@Fleet)) {
    Cpars$Fdisc <- setCparValues(OM@Fleet@DiscardMortality@MeanAtAge[,1,1], 1, nsim)
    Cpars$qinc <- setCparValues(OM@Fleet@Effort@qInc, 1, nsim)
    Cpars$qcv <- setCparValues(OM@Fleet@Effort@qCV, 1, nsim)
    
    Cpars$L5 <- setCparValues(OM@Fleet@Selectivity@Pars$L5, 1, nsim)
    Cpars$LFS <- setCparValues(OM@Fleet@Selectivity@Pars$LFS, 1, nsim)
    Cpars$Vmaxlen <- setCparValues(OM@Fleet@Selectivity@Pars$Vmaxlen, 1, nsim)
    
    Cpars$LR5 <- setCparValues(OM@Fleet@Selectivity@Pars$LR5, 1, nsim)
    Cpars$LFR <- setCparValues(OM@Fleet@Selectivity@Pars$LFR, 1, nsim)
    Cpars$Rmaxlen <- setCparValues(OM@Fleet@Selectivity@Pars$Rmaxlen, 1, nsim)
    
    Cpars$Find <- setCparValues(val=OM@Fleet@Effort@Effort, 2, nsim, 
                                nTS=length(TimeSteps(OM, 'Historical')))
    Cpars$qs <- setCparValues(val=OM@Fleet@Effort@Catchability, 1, nsim)
    
    Cpars$Wt_age_C <- setCparValues(val=OM@Fleet@WeightFleet, 3, nsim, nTS)
    
    Cpars$V <- setCparValues(val=OM@Fleet@Selectivity@MeanAtAge, 3, nsim, nTS)
    Cpars$SLarray <- setCparValues(val=OM@Fleet@Selectivity@MeanAtLength, 3, nsim, nTS)
    
    Cpars$retA <- setCparValues(val=OM@Fleet@Retention@MeanAtAge, 3, nsim, nTS)
    Cpars$retL <- setCparValues(val=OM@Fleet@Retention@MeanAtLength, 3, nsim, nTS)
    Cpars$DR <- rep(0, nsim)
  } else {
    Cpars$Fdisc <- setCparValues(OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge[,1,1], 1, nsim)
    Cpars$qinc <- setCparValues(OM@Fleet[[st]][[fl]]@Effort@qInc, 1, nsim)
    Cpars$qcv <- setCparValues(OM@Fleet[[st]][[fl]]@Effort@qCV, 1, nsim)
    
    Cpars$L5 <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$L5, 1, nsim)
    Cpars$LFS <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LFS, 1, nsim)
    Cpars$Vmaxlen <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$Vmaxlen, 1, nsim)
    
    Cpars$LR5 <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LR5, 1, nsim)
    Cpars$LFR <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$LFR, 1, nsim)
    Cpars$Rmaxlen <- setCparValues(OM@Fleet[[st]][[fl]]@Selectivity@Pars$Rmaxlen, 1, nsim)
    
    Cpars$Find <- setCparValues(val=OM@Fleet[[st]][[fl]]@Effort@Effort, 2, nsim, 
                                nTS=length(TimeSteps(OM, 'Historical')))
    Cpars$qs <- setCparValues(val=OM@Fleet[[st]][[fl]]@Effort@Catchability, 1, nsim)
    
    Cpars$Wt_age_C <- setCparValues(val=OM@Fleet[[st]][[fl]]@Weight, 3, nsim, nTS)
    
    Cpars$V <- setCparValues(val=OM@Fleet[[st]][[fl]]@Selectivity@MeanAtAge, 3, nsim, nTS)
    Cpars$SLarray <- setCparValues(val=OM@Fleet[[st]][[fl]]@Selectivity@MeanAtLength, 3, nsim, nTS)
    
    Cpars$retA <- setCparValues(val=OM@Fleet[[st]][[fl]]@Retention@MeanAtAge, 3, nsim, nTS)
    Cpars$retL <- setCparValues(val=OM@Fleet[[st]][[fl]]@Retention@MeanAtLength, 3, nsim, nTS)
    Cpars$DR <- rep(0, nsim)
    
    # Cpars$qvar
    # Cpars$V_real
    # Cpars$retA_real
    # Cpars$retL_real
    # Cpars$SLarray_real
    
    Cpars$Fdisc_array1 <- setCparValues(val=OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtAge, 3, nsim, nTS)
    Cpars$Fdisc_array2 <- setCparValues(val=OM@Fleet[[st]][[fl]]@DiscardMortality@MeanAtLength, 3, nsim, nTS)
    
    
  }
  
  if (methods::is(OM@Misc$cpars, 'list')) {
    Cpars <- c(Cpars,OM@Misc$cpars[[st]][[fl]]) # any Obs and Imp stuff  
  } else {
    Cpars <- c(Cpars,OM@Misc$cpars) # any Obs and Imp stuff  
  }
  Cpars
}

om2OM <- function(OM) {
  OM <- Populate(OM, messages=FALSE)
  nsim <- nSim(OM)
  nTS <- length(TimeSteps(OM))
  
  OMout <- new('OM', Stock=om2Stock(OM), 
               Fleet=om2Fleet(OM)
               )
  OMout@cpars <- om2Cpars(OM)
  OMout@Name <- OM@Name
  OMout@Agency <- OM@Agency
  OMout@Region <- OM@Region
  OMout@Sponsor <- OM@Sponsor
  OMout@Latitude <- OM@Latitude
  OMout@Longitude <- OM@Longitude
  OMout@nsim <- OM@nSim
  OMout@proyears <- OM@pYear
  OMout@interval <- OM@Interval
  OMout@pstar <- OM@pStar
  OMout@maxF <- OM@maxF
  OMout@reps <- OM@nReps
  OMout@seed <- OM@Seed
  OMout@Source <- OM@Source
  OMout <- Replace(OMout, OM@Obs, silent=TRUE)
  OMout <- Replace(OMout, OM@Imp, silent=TRUE)
 
  OMout
}

om2MOM <- function(OM) {
  OM <- Populate(OM, messages=FALSE)
  nsim <- nSim(OM)
  nTS <- length(TimeSteps(OM))
  
  # everything is done in cpars
  Stocks <- vector('list', nStock(OM))
  names(Stocks) <- names(OM@Stock)
  Fleets <- vector('list', nStock(OM))
  names(Fleets) <- names(OM@Stock)
  cpars <- vector('list', nStock(OM))
  names(cpars) <- names(OM@Stock)
  for (st in 1:nStock(OM)) {
    Fleets[[st]] <- vector('list', nFleet(OM))
    names(Fleets[[st]]) <- names(OM@Fleet[[st]])
    cpars[[st]] <- vector('list', nFleet(OM))
    names(cpars[[st]]) <- names(OM@Fleet[[st]])
    
    Stocks[[st]] <- om2Stock(OM, st)
    for (fl in 1:nFleet(OM)) {
      Fleets[[st]][[fl]] <- om2Fleet(OM, st, fl)
      cpars[[st]][[fl]] <- om2Cpars(OM, st, fl)
    }
  }
  
  MOM <- new('MOM',
             nsim=nsim,
             proyears=pYear(OM),
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
  
  MOM@Name <- OM@Name
  MOM@Agency <- OM@Agency
  MOM@Region <- OM@Region
  MOM@Sponsor <- OM@Sponsor
  MOM@Latitude <- OM@Latitude
  MOM@Longitude <- OM@Longitude
  MOM@nsim <- OM@nSim
  MOM@proyears <- OM@pYear
  MOM@interval <- OM@Interval
  MOM@pstar <- OM@pStar
  MOM@maxF <- OM@maxF
  MOM@reps <- OM@nReps
  MOM@seed <- OM@Seed
  MOM@Source <- OM@Source
  MOM
  
}



# ----- Data -----

ConvertData <- function(Data) {
  if (inherits(Data,'data')) {
    return(data2Data(Data))
    
  } else if (inherits(Data,'Data')) {
    
  } else {
    cli::cli_abort('`Data` must be class `data` or `Data`')
  }
  
}

#' @export
data2Data <- function(data) {
  # TODO - AddInd, LifeHistory, CAL, etc etc 
  
  DataOut <- new('Data')
  DataOut@Year <- data@TimeSteps
  DataOut@LHYear <- data@TimeStepLH
  DataOut@Ind <- matrix(data@CPUE@Value[,1], nrow=1)
  DataOut@CV_Ind <- matrix(data@CPUE@CV[,1], nrow=1)
  
  DataOut@Cat <- matrix(rowSums(data@Landings@Value, na.rm=TRUE) + rowSums(data@Discards@Value, na.rm=TRUE), nrow=1)
  DataOut@CV_Cat <- matrix(data@Landings@CV[,1], nrow=1)
  DataOut@CV_Cat[is.na(DataOut@CV_Cat)] <- 0.2 # hack for some DLMtool MPs that require CV
  
  DataOut
}


