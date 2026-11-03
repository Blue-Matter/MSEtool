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
#' The converted `om` reproduces the legacy schedules:
#'
#' - Maturity uses the [MaturityAtAge()] model. `A50` and `A50_95` are the
#'   ages at which mean length-at-age equals `L50` and `L95`, for each
#'   simulation and year. When `cpars$Mat_age` is supplied it is used directly.
#' - Effort control points in `EffYears` are placed in the same historical
#'   years as in the legacy `OM`.
#' - Relative selectivity and retention parameters (`OM@isRel = TRUE`) are
#'   multiplied by the sampled `L50` of each simulation.
#' - `Vmaxlen` and `Rmaxlen` are rescaled from selectivity at `Linf` to
#'   selectivity at the maximum length class, so the descending limb of
#'   dome-shaped curves is unchanged.
#'
#' Effort is allocated across areas by the spatial targeting model described
#' in [Effort()]; `OM@Spat_targ` is not converted.
#'
#' @return An [om-class] object.
#'
#' @seealso [Convert()], [ConvertMOM()], [ConvertStock()], [ConvertFleet()],
#'   [ConvertObs()], [ConvertImp()], [PopulateOM()]
#'
#' @examples
#' om_new <- ConvertOM(testOM)
#'
#' @export
ConvertOM <- function(OM,
                      Author      = '',
                      CurrentYear = NULL,
                      Seasons     = 1,
                      Populate    = TRUE,
                      silent      = FALSE) {
  
  .CheckClass(OM, c('OM'), 'OM')
  
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
    .OM2stock(
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
      .OM2fleet(OM, YearsList=YearsList, cpars=OM@cpars)
    )
  )
  
  # Update selectivity/retention slots
  om <- .UpdateSelRet(OM, om)
  
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
  
  # Update max length
  om <- om |>
    .SolveForVmaxlen('Selectivity') |>
    .SolveForVmaxlen('Retention')
  
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
.UpdateSelRet <- function(OM, om) {
  if (!as.logical(OM@isRel))
    return(om)
  
  nStock <- nStock(om)
  nFleet <- nFleet(om)
  
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      L50 <- .LegacyL50(om@Stock[[st]], om@nSim)
      om@Fleet[[st]][[fl]]@Selectivity@Pars <- .StructurePars(Pars=om@Fleet[[st]][[fl]]@Selectivity@Pars,
                                                 nSim=om@nSim, 
                                                 Years=om@Years)
      om@Fleet[[st]][[fl]]@Selectivity@Pars$L5 <- ArrayMultiply(L50, 
                                                                om@Fleet[[st]][[fl]]@Selectivity@Pars$L5)
      om@Fleet[[st]][[fl]]@Selectivity@Pars$LFS <- ArrayMultiply(L50, 
                                                                 om@Fleet[[st]][[fl]]@Selectivity@Pars$LFS)
      om@Fleet[[st]][[fl]]@Retention@Pars <- .StructurePars(Pars=om@Fleet[[st]][[fl]]@Retention@Pars,
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

#' Length at 50% Maturity Used by Legacy Relative Selectivity
#'
#' Returns the per-simulation length at 50% maturity that legacy `OM` objects
#' multiply relative selectivity and retention parameters by. Called internally
#' by `.UpdateSelRet()`.
#'
#' @param Stock A populated [stock-class] object.
#' @param nSim Integer. Number of simulations.
#'
#' @return A `Sim x Year` array, constant over years. Uses the legacy L50
#'   stored by the maturity conversion, otherwise `Maturity@Pars$L50`,
#'   otherwise the length at which maturity-at-age reaches 0.5 in the last
#'   historical year.
#'
#' @seealso `.UpdateSelRet()`, [ConvertOM()]
#' @keywords internal
.LegacyL50 <- function(Stock, nSim) {
  Maturity <- Stock@Maturity
  Years <- Stock@Years
  L50 <- Maturity@Misc$LegacyL50 %||% Maturity@Pars$L50
  if (!is.null(L50)) {
    L50 <- Extend(L50, nSim)
    L50 <- if (is.array(L50)) L50[, 1] else rep(L50, length.out = nSim)
  } else {
    MatAge <- Extend(Maturity@MeanAtAge, nSim, Years = Years)
    LenAge <- Extend(Stock@Length@MeanAtAge, nSim, Years = Years)
    yr <- Stock@nYear
    L50 <- vapply(seq_len(nSim), \(s) .LinInterp(MatAge[s, , yr], LenAge[s, , yr], 0.5), numeric(1))
  }
  array(rep(L50, length(Years)), c(nSim, length(Years)),
        dimnames = list(Sim = seq_len(nSim), Year = Years))
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
#' @seealso [ConvertOM()], `.VmaxLenOpt()`, [Selectivity()], [Retention()]
#' @keywords internal
.SolveForVmaxlen <- function(om, type=c('Selectivity', 'Retention')) {
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
      Original <- slot(om@Fleet[[st]][[fl]], type)
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
                                                                Selectivity=om@Fleet[[st]][[fl]]@Selectivity,
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
      
      MaxLen <- max(.ClassMidpoints(slot(om@Fleet[[st]][[fl]], type)@Classes))
      VmaxlenOut <- array(0, dim=dim(Linf))
      dimnames(VmaxlenOut) <- dimnames(Linf)
      
      for (s in 1:nsim) {
        for (ts in seq_along(Years)) {
          VmaxlenOut[s,ts] <- .VmaxLenOpt(LFS[s,ts],
                                         Vmaxlen[s, ts],
                                         Linf[s,ts],
                                         MaxLen)
          cli::cli_progress_update()
        }
      }
      Updated <- slot(om@Fleet[[st]][[fl]], type)
      Updated@Pars[[Var_Vmax]] <- VmaxlenOut
      # drop at-size schedules generated from the old Vmaxlen
      if (is.null(Original@MeanAtAge)) Updated@MeanAtAge <- NULL
      if (is.null(Original@MeanAtLength)) Updated@MeanAtLength <- NULL
      slot(om@Fleet[[st]][[fl]], type) <- Updated
      cli::cli_progress_done()
    }
  }
  om
}

#' Rescale Vmaxlen from Linf to the Maximum Length Class
#'
#' Returns the double-normal `Vmaxlen` at `maxlen` that gives the same
#' descending limb as selectivity `vmaxlen` at `linf`. Called internally by
#' `.SolveForVmaxlen()`.
#'
#' @param lfs Numeric. Length at full selectivity.
#' @param vmaxlen Numeric. Selectivity at `linf` (between 0 and 1).
#' @param linf Numeric. Asymptotic length (`Linf`).
#' @param maxlen Numeric. Largest length-class midpoint.
#'
#' @return Numeric. `Vmaxlen` at `maxlen`. Returns `vmaxlen` unchanged if
#'   `lfs >= linf`.
#'
#' @seealso `.SolveForVmaxlen()`
#' @keywords internal
.VmaxLenOpt <- function(lfs, vmaxlen, linf, maxlen) {
  if (!is.finite(lfs) || lfs >= linf)
    return(vmaxlen)
  max(max(vmaxlen, 1e-15)^(((maxlen - lfs) / (linf - lfs))^2), 1e-15)
}


.SetSlotDimNames <- function(object, slot_name, value, Sims, Ages, Years, 
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
