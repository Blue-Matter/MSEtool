

#' @rdname Convert
#' @export
ConvertOM <- function(OM, Author='', CurrentYear=NULL, Seasons=1, Populate=TRUE, silent=FALSE) {
  CheckClass(OM, c('OM'), 'OM')
  
  if (!silent)
    cli::cli_alert('Converting object of class {.cls OM} to class {.cls om}')

  om <- OM()
  om@Name <- OM@Name
  om@Agency <-  OM@Agency
  om@Region <-  OM@Region
  om@Author <- Author
  om@Longitude <- OM@Longitude
  om@Latitude <- OM@Latitude
  om@Sponsor <- OM@Sponsor
  om@nSim <- OM@nsim
  om@nYear <- OM@nyears
  om@pYear <- OM@proyears
  om@Interval <- OM@interval
  om@Seed <- OM@seed
  om@pStar <- OM@pstar
  om@maxF <- OM@maxF
  om@nReps <- OM@reps
  om@Source <- OM@Source
  om@CurrentYear <- ifelse(is.null(CurrentYear),
                           as.numeric(format(Sys.Date(), '%Y')),
                           CurrentYear
                           )
  om@Seasons <- Seasons
  om@Years <- CalcYears(nYear=om@nYear,
                        pYear=om@pYear,
                        CurrentYear=om@CurrentYear,
                        Seasons)
  
  YearsList <- list(HistTS=Years(om, 'Historical'),
                        ProjTS=Years(om, 'Projection'),
                        TimeUnits=TimeUnits,
                        Seasons=Seasons
  )
  StockName <- SubOM(OM, 'Stock')@Name
  om@Stock <- MakeNamedList(StockName,
                            OM2stock(OM, cpars=OM@cpars, YearsList, OM@nsim, OM@seed)
  )
  FleetName <- SubOM(OM, 'Fleet')@Name
  om@Fleet <- MakeNamedList(StockName,
                            MakeNamedList(FleetName,
                                          OM2fleet(OM, OM@cpars, OM@Fdisc)
                                          )
  )
  om <- UpdateSelRet(OM, om)
 
  om@Obs <- MakeNamedList(StockName,
                          MakeNamedList(FleetName,
                                        OM2obs(OM, OM@cpars)
                          )
  )
  
  om@Imp <- MakeNamedList(StockName,
                          MakeNamedList(FleetName,
                                        OM2imp(OM, OM@cpars)
                          )
  )
  
  # update because Vmaxlen and Rmaxlen now correspond with maximum length class
  om <- om |> 
    SolveForVmaxlen('Selectivity') |>
    SolveForVmaxlen('Retention') |>
    ProcessEFactor()
  
  if (Populate)
    om <- PopulateOM(om, silent=FALSE)
  
  om
}


UpdateSelRet <- function(OM, om) {
  if (!as.logical(OM@isRel))
    return(om)
  
  nStock <- nStock(om)
  nFleet <- nFleet(om)
  
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      L50 <- GetLengthClass(om@Stock[[st]]@Maturity, 0.5)
      om@Fleet[[st]][[fl]]@Selectivity@Pars <- StructurePars(Pars=om@Fleet[[st]][[fl]]@Selectivity@Pars,
                                                 nsim=om@nSim, 
                                                 Years=om@Years)
      om@Fleet[[st]][[fl]]@Selectivity@Pars$L5 <- ArrayMultiply(L50, 
                                                                om@Fleet[[st]][[fl]]@Selectivity@Pars$L5)
      om@Fleet[[st]][[fl]]@Selectivity@Pars$LFS <- ArrayMultiply(L50, 
                                                                 om@Fleet[[st]][[fl]]@Selectivity@Pars$LFS)
      om@Fleet[[st]][[fl]]@Retention@Pars <- StructurePars(Pars=om@Fleet[[st]][[fl]]@Retention@Pars,
                                                           nsim=om@nSim, 
                                                           Years=om@Years)
      om@Fleet[[st]][[fl]]@Retention@Pars$LR5 <- ArrayMultiply(L50,
                                                               om@Fleet[[st]][[fl]]@Retention@Pars$LR5)
      om@Fleet[[st]][[fl]]@Retention@Pars$LFR <- ArrayMultiply(L50, 
                                                               om@Fleet[[st]][[fl]]@Retention@Pars$LFR)
    }
  }

  om
}

GetLengthClass <- function(object, RefValue=0.5) {
  array <- object@MeanAtLength
  dd <- dim(array)
  
  out <- array(0, dim=dd[c(1,3)], dimnames=dimnames(array)[c(1,3)])
  
  for (s in 1:dd[1]) {
    for (ts in 1:dd[3]) {
      ind <- which.min(abs(array[s,,ts]-RefValue))
      out[s,ts] <- object@Classes[ind]
    }
  }
  out
}


SolveForVmaxlen <- function(om, type=c('Selectivity', 'Retention')) {
  type <- match.arg(type, c('Selectivity', 'Retention'))
  # calculates new value for Vmaxlen/Rmaxlen to correspond with maximum
  # length bin rather than Linf, as previously defined
  
  nStock <- nStock(om)
  nFleet <- nFleet(om)
  StockNames <- StockNames(om)
  FleetNames <- FleetNames(om)
  
  Var_Vmax <- switch(type, 
                     'Selectivity'='Vmaxlen',
                     'Retention'='Rmaxlen')
  
  Var_L5 <- switch(type, 
                   'Selectivity'='L5',
                   'Retention'='LR5')
  
  Var_LFR <- switch(type, 
                    'Selectivity'='LFS',
                    'Retention'='LFR')
  
  for (st in 1:nStock) {
    Linf <- om@Stock[[st]]@Length@Pars$Linf 
    dd <- prod(dim(Linf)) * nFleet
    
    for (fl in 1:nFleet) {
      cli::cli_progress_bar('Calculating {.var {Var_Vmax}} for Stock: {.val {StockNames[st]}} Fleet:  {.val {FleetNames[st]}}', total=dd)
      
      L5 <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_L5]]
      LFS <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_LFR]]
      Vmaxlen <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_Vmax]]
      
      df <- rbind(dim(Linf),
                  dim(L5),
                  dim(LFS),
                  dim(Vmaxlen))
      nsim <- max(df[,1])
      
      YearsList <- list(dimnames(Linf)$Year,
                        dimnames(L5)$Year,
                        dimnames(LFS)$Year,
                        dimnames(Vmaxlen)$Year
      )
      Years <- YearsList[[which.max(df[,2])]]                      
      
      Linf <- Linf |> ArrayExpand(nsim, Years=Years)
      L5 <- L5 |> ArrayExpand(nsim, Years=Years)
      LFS <- LFS |> ArrayExpand(nsim, Years=Years)
      Vmaxlen <- Vmaxlen |> ArrayExpand(nsim, Years=Years)
      
      VmaxlenOut <- array(0, dim=dim(Linf))
      dimnames(VmaxlenOut) <- dimnames(Linf)
      
      for (s in 1:nsim) {
        for (ts in seq_along(Years)) {
          VmaxlenOut[s,ts] <- VmaxLenOpt(L5[s,ts], 
                                         LFS[s,ts],
                                         Vmaxlen[s, ts],
                                         Linf[s,ts])
          cli::cli_progress_update()
        }
      }
      slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_Vmax]] <- VmaxlenOut
      cli::cli_progress_done()
    }
  }
  om
}

VmaxLenOpt <- function(l5, lfs, vmaxlen, linf) {
  if (vmaxlen > 0.99)
    return(vmaxlen)
  opt <- optimize(optForVmaxLen,
                  interval=logit(c(0.001, 0.999)),
                  l5=l5,
                  lfs=lfs,
                  linf=linf,
                  vmaxlen=vmaxlen)
  return(ilogit(opt$minimum))
}


optForVmaxLen <- function(logitTrial, l5, lfs, linf, vmaxlen) {
  trial <- ilogit(logitTrial)
  lens <- seq(0, linf,length.out=100)
  sel <- DoubleNormal(lens,l5, lfs, trial)
  (sel[length(sel)] - vmaxlen)^2
}

