#' Convert Legacy OM Object to New om Class
#'
#' Converts a legacy [OM-legacy-class] object to the current [om-class],
#' copying attributes, updating year vectors, and optionally populating the
#' object via [PopulateOM()].
#'
#' @param OM An [OM-legacy-class] object to convert.
#' @param Author Character. Author of the OM. Default `""`.
#' @param CurrentYear Numeric. Last historical calendar year. If `NULL`
#'   (default), taken from `OM@CurrentYr`; if that is less than 1000, defaults
#'   to the current system year.
#' @param Seasons Integer. Number of seasons per year. Default `1`.
#' @param Populate Logical. If `TRUE` (default), calls [PopulateOM()] on the
#'   converted object.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' Maximum length slots (`Vmaxlen` and `Rmaxlen`) are updated to reflect the
#' maximum length class rather than `Linf` as in legacy objects. This changes
#' the selectivity and retention curves if `Vmaxlen` or `Rmaxlen` are less
#' than 1. As an alternative, users can set the `MeanAtLength` or `MeanAtAge`
#' arrays directly in the [Selectivity()] and [Retention()] objects.
#'
#' @return An [om-class] object.
#'
#' @seealso [Convert()], [ConvertMOM()], [ConvertStock()], [ConvertFleet()],
#'   [ConvertObs()], [ConvertImp()], [PopulateOM()]
#'
#' @examples
#' \dontrun{
#' OMlegacy <- readRDS("MyLegacyOM.rds")
#' om_new <- ConvertOM(OMlegacy)
#' }
#'
#' @export
ConvertOM <- function(OM,
                      Author      = '',
                      CurrentYear = NULL,
                      Seasons     = 1,
                      Populate    = TRUE,
                      silent      = FALSE) {
  
  CheckClass(OM, c('OM'), 'OM')
  
  if (!silent) 
    cli::cli_alert('Converting object of class {.cls OM} to class {.cls om}')
  
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





#' Update Selectivity and Retention for Relative Length Parameters
#'
#' Scales `L5`, `LFS`, `LR5`, and `LFR` parameters by the length-at-50%-maturity
#' when the legacy OM used relative length parameters (`OM@isRel == TRUE`).
#' Called internally by [ConvertOM()].
#'
#' @param OM An [OM-legacy-class] object.
#' @param om An [om-class] object being constructed by [ConvertOM()].
#'
#' @return The [om-class] object `om` with updated selectivity and retention
#'   parameters.
#'
#' @seealso [ConvertOM()], [Selectivity()], [Retention()]
#' @keywords internal
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

#' Get Length Class Corresponding to a Reference Value
#'
#' Finds the length class in a `MeanAtLength` array that most closely
#' corresponds to a given reference value (e.g., 0.5 for length-at-50%-maturity).
#' Called internally by [UpdateSelRet()].
#'
#' @param object An S4 object with a `MeanAtLength` array (Sim × Length × Year)
#'   and a `Classes` slot of length classes.
#' @param RefValue Numeric. The reference value to match in `MeanAtLength`.
#'   Default `0.5`.
#'
#' @return A numeric array of dimensions (Sim × Year) giving the length class
#'   closest to `RefValue` for each simulation and time step.
#'
#' @seealso [UpdateSelRet()], [ConvertOM()]
#' @keywords internal
GetLengthClass <- function(object, RefValue=0.5) {
  array <- object@MeanAtLength
  dd <- dim(array)
  
  out <- array(0, dim=dd[c(1,3)], dimnames=dimnames(array)[c(1,3)])
  
  for (s in seq_len(dd[1])) {
    for (ts in seq_len(dd[3])) {
      ind <- which.min(abs(array[s,,ts]-RefValue))
      out[s,ts] <- object@Classes[ind]
    }
  }
  out
}


#' Solve for Vmaxlen or Rmaxlen at the Maximum Length Class
#'
#' Recalculates `Vmaxlen` (selectivity) or `Rmaxlen` (retention) to correspond
#' to the maximum length bin rather than `Linf`, as was done in legacy objects.
#' Called internally by [ConvertOM()].
#'
#' @param om An [om-class] object.
#' @param type Character. One of `"Selectivity"` or `"Retention"`.
#'
#' @return The [om-class] object with updated `Vmaxlen` or `Rmaxlen` parameters
#'   for all stock-fleet combinations.
#'
#' @seealso [ConvertOM()], [VmaxLenOpt()], [Selectivity()], [Retention()]
#' @keywords internal
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
  
  for (st in seq_len(nStock)) {
    om@Stock[[st]] <- PopulateStock(Stock=om@Stock[[st]],
                                    nYear=om@nYear,
                                    pYear=om@pYear,
                                    CurrentYear = om@CurrentYear,
                                    nSim = om@nSim,
                                    Seasons = om@Seasons
    )
    
    Linf <- om@Stock[[st]]@Length@Pars$Linf 
    if (is.null(Linf))  next
    dd <- prod(dim(Linf)) * nFleet
    
    for (fl in seq_len(nFleet)) {
      cli::cli_progress_bar('Calculating {.var {Var_Vmax}} for Stock: {.val {StockNames[st]}} Fleet:  {.val {FleetNames[fl]}}', total=dd)
      
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
      
      if (is.null(L5)) next
      
      LFS <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_LFR]]
      Vmaxlen <- slot(om@Fleet[[st]][[fl]], type)@Pars[[Var_Vmax]]
      
      if (all(L5==0) && all(LFS==0)) next
        
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
      Years <- as.numeric(YearsList[[which.max(df[,2])]])
      
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

#' Optimise Vmaxlen at the Maximum Length Bin
#'
#' Finds the `Vmaxlen` value (on the logit scale) that reproduces the original
#' selectivity at the maximum length bin (`Linf`) under the double-normal curve.
#' Called internally by [SolveForVmaxlen()].
#'
#' @param l5 Numeric. Length at 5% selectivity.
#' @param lfs Numeric. Length at full selectivity.
#' @param vmaxlen Numeric. Original selectivity at `Linf` (between 0 and 1).
#' @param linf Numeric. Asymptotic length (`Linf`).
#'
#' @return Numeric. The optimised `Vmaxlen` value on the probability scale
#'   (0–1). Returns `vmaxlen` unchanged if it exceeds 0.99.
#'
#' @seealso [SolveForVmaxlen()], [optForVmaxLen()]
#' @keywords internal
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

#' Objective Function for Vmaxlen Optimisation
#'
#' Computes the squared difference between the double-normal selectivity at
#' `Linf` and the target `vmaxlen`, used by [VmaxLenOpt()] via [stats::optimize()].
#'
#' @param logitTrial Numeric. Trial value of `Vmaxlen` on the logit scale.
#' @param l5 Numeric. Length at 5% selectivity.
#' @param lfs Numeric. Length at full selectivity.
#' @param linf Numeric. Asymptotic length (`Linf`).
#' @param vmaxlen Numeric. Target selectivity at `Linf`.
#'
#' @return Numeric. Squared difference between predicted and target selectivity
#'   at `Linf`.
#'
#' @seealso [VmaxLenOpt()], [SolveForVmaxlen()]
#' @keywords internal
optForVmaxLen <- function(logitTrial, l5, lfs, linf, vmaxlen) {
  trial <- ilogit(logitTrial)
  lens <- seq(0, linf,length.out=100)
  sel <- DoubleNormal(lens,l5, lfs, trial)
  (sel[length(sel)] - vmaxlen)^2
}


SetSlotDimNames <- function(object, slot_name, value, Sims, Ages, Years, 
                            reduce = TRUE, inc_year = TRUE) {
  
  dd <- dim(value)
  
  if (length(dd) != 3L)
    cli::cli_abort(
      c('x' = 'Expected a 3D array for slot {.field {slot_name}}, got {length(dd)}D'),
      .internal = TRUE
    )
  
  dimnames(value) <- list(
    Sim  = Sims[seq_len(dd[1])],
    Age  = Ages[seq_len(dd[2])],
    Year = Years[seq_len(dd[3])]
  )
  
  if (reduce)
    value <- ReduceDims(value, IncYear = inc_year)
  
  slot(object, slot_name) <- value
  object
}
