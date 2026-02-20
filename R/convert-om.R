#' Convert legacy OM object to new OM class
#'
#' This function converts an existing legacy [OM-class] object into the
#' current `om` S4 class, copying attributes, updating year vectors,
#' and optionally populating the object.
#'
#' @param OM An [OM-class] object to convert
#' @param Author Character string. Author of the OM object. Optional.
#' @param CurrentYear Numeric. Last historical year of OM. Defaults to
#'   the current year if missing from `OM`.
#' @param Seasons Numeric length 1. Number of seasons per year.
#' @param Populate Logical. If `TRUE`, calls [PopulateOM()] on the converted object.
#' @param silent Logical. Suppress messages if `TRUE`.
#'
#' @return An  `om` class object.
#'
#' @details
#' 
#' **Note**: Maximum length slots (`Vmaxlen` and `Rmaxlen`) are updated to reflect the 
#' maximum length class rather than `Linf` as was done previously. This will change
#' the selectivity/retention curves compared to legacy object if `Vmaxlen` or 
#' `Rmaxlen` are < 1. Alternatively, users can set the `MeanAtLength` or 
#' `MeanAtAge` arrays directly in the new [Selectivity()] and [Retention()] objects.
#'
#' @examples
#' \dontrun{
#' OMlegacy <- LoadOM("MyLegacyOM.rds")
#' om_new <- ConvertOM(OMlegacy)
#' }
#'
#' @export
ConvertOM <- function(OM,
                      Author = '',
                      CurrentYear = NULL,
                      Seasons = 1,
                      Populate = TRUE,
                      silent = FALSE) {
  
 
  CheckClass(OM, c('OM'), 'OM')
  
  if (!silent) {
    cli::cli_alert('Converting object of class {.cls OM} to class {.cls om}')
  }
  
  # Initialize new OM object
  om <- OM()
  om@Name        <- OM@Name
  om@Agency      <- OM@Agency
  om@Region      <- OM@Region
  om@Author      <- Author
  om@Longitude   <- OM@Longitude
  om@Latitude    <- OM@Latitude
  om@Sponsor     <- OM@Sponsor
  om@nSim        <- OM@nsim
  om@Seasons     <- Seasons
  om@nYear       <- OM@nyears/Seasons
  om@pYear       <- OM@proyears/Seasons
  om@Interval    <- OM@interval
  om@Seed        <- OM@seed
  om@pStar       <- OM@pstar
  om@maxF        <- OM@maxF
  om@nReps       <- OM@reps
  om@Source      <- OM@Source
  om@CurrentYear <- OM@CurrentYr
  
  if (om@CurrentYear < 1000) {
    om@CurrentYear <- ifelse(
      is.null(CurrentYear),
      as.numeric(format(Sys.Date(), '%Y')),
      CurrentYear
    )
  }

  om@Years   <- CalcYears(
    nYear = om@nYear,
    pYear = om@pYear,
    CurrentYear = om@CurrentYear,
    Seasons = Seasons
  )
  
  # Prepare years list for stock/fleet conversion
  
  YearsList <- list(
    HistTS    = Years(om, 'Historical'),
    ProjTS    = Years(om, 'Projection'),
    TimeUnits = CalcTSUnits(Seasons),
    nYear = om@nYear,
    pYear = om@pYear,
    CurrentYear = om@CurrentYear,
    Seasons   = Seasons
  )
  
  # Stocks
  StockName <- SubOM(OM, 'Stock')@Name
  om@Stock <- MakeNamedList(
    StockName,
    OM2stock(
      OM,
      cpars = OM@cpars,
      YearsList,
      nSim = OM@nsim,
      seed = OM@seed
    )
  )
  
  # Fleets
  FleetName <- SubOM(OM, 'Fleet')@Name
  om@Fleet <- MakeNamedList(
    StockName,
    MakeNamedList(
      FleetName,
      OM2fleet(OM, YearsList=YearsList, cpars=OM@cpars)
    )
  )
  
  # Update selectivity/retention slots
  om <- UpdateSelRet(OM, om)
  
  # Observations
  om@Obs <- MakeNamedList(
    StockName,
    MakeNamedList(
      FleetName,
      ConvertObs(OM, silent = TRUE)
    )
  )
  
  # Implementation
  om@Imp <- MakeNamedList(
    StockName,
    MakeNamedList(
      FleetName,
      ConvertImp(OM, silent = TRUE)
    )
  )
  
  # Update max length and E-factor
  om <- om |>
    SolveForVmaxlen('Selectivity') |>
    SolveForVmaxlen('Retention') |>
    ProcessEFactor()
  
  # Populate object if requested
  if (Populate) {
    om <- PopulateOM(om, silent = FALSE)
  }
  
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
                                                 nSim=om@nSim, 
                                                 Years=om@Years)
      om@Fleet[[st]][[fl]]@Selectivity@Pars$L5 <- ArrayMultiply(L50, 
                                                                om@Fleet[[st]][[fl]]@Selectivity@Pars$L5)
      om@Fleet[[st]][[fl]]@Selectivity@Pars$LFS <- ArrayMultiply(L50, 
                                                                 om@Fleet[[st]][[fl]]@Selectivity@Pars$LFS)
      om@Fleet[[st]][[fl]]@Retention@Pars <- StructurePars(Pars=om@Fleet[[st]][[fl]]@Retention@Pars,
                                                           nSim=om@nSim, 
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
    om@Stock[[st]] <- PopulateStock(Stock=om@Stock[[st]],
                                    nYear=om@nYear,
                                    pYear=om@pYear,
                                    CurrentYear = om@CurrentYear,
                                    nSim = om@nSim,
                                    Seasons = om@Seasons
    )
    
    Linf <- om@Stock[[st]]@Length@Pars$Linf 
    if (is.null(Linf))
      next()
    dd <- prod(dim(Linf)) * nFleet
    
    for (fl in 1:nFleet) {
      cli::cli_progress_bar('Calculating {.var {Var_Vmax}} for Stock: {.val {StockNames[st]}} Fleet:  {.val {FleetNames[st]}}', total=dd)
      
      if (type=='Selectivity') {
        om@Fleet[[st]][[fl]]@Selectivity <- PopulateSelectivity(Selectivity=om@Fleet[[st]][[fl]]@Selectivity,
                                                                Ages=om@Stock[[st]]@Ages,
                                                                Length=om@Stock[[st]]@Length,
                                                                Weight=om@Stock[[st]]@Weight,
                                                                Maturity=om@Stock[[st]]@Maturity,
                                                                nSim = om@nSim,
                                                                Years=Years(om),
                                                                nArea = nArea(om)
        )
      } else {
        om@Fleet[[st]][[fl]]@Retention <- PopulateRetention(Retention=om@Fleet[[st]][[fl]]@Retention,
                                                                Ages=om@Stock[[st]]@Ages,
                                                                Length=om@Stock[[st]]@Length,
                                                                Weight=om@Stock[[st]]@Weight,
                                                                Maturity=om@Stock[[st]]@Maturity,
                                                                nSim = om@nSim,
                                                                Years=Years(om),
                                                                nArea = nArea(om)
        )
      }
      
      
      L5 <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_L5]]
      if (is.null(L5))
        next()
      
      
      LFS <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_LFR]]
      Vmaxlen <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_Vmax]]
      
      
      if (all(L5==0) && all(LFS==0))
        next()
        
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
      
      Linf <- Linf |> Extend(nsim, Years=Years)
      L5 <- L5 |> Extend(nsim, Years=Years)
      LFS <- LFS |> Extend(nsim, Years=Years)
      Vmaxlen <- Vmaxlen |> Extend(nsim, Years=Years)
      
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

