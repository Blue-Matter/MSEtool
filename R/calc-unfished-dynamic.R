#' Calculate Dynamic Unfished Population Dynamics
#'
#' Calculate dynamic unfished population quantities for all [Stock()]
#' objects in an operating model: number-at-age, total biomass, spawning
#' biomass, and spawning production over the historical and projection years.
#'
#' The population dynamics model is run from the unfished initial state with
#' no fishing mortality, using the historical and projection recruitment
#' deviations (`RecDevHist` and `RecDevProj` in [SRR()]) and any time-varying
#' life-history parameters. The projection-year values depend only on the
#' operating model, so they are the same for all management procedures.
#'
#' @param Hist A [hist-class] object, or an [om-class] object from which a
#'   `hist` object is built.
#' @param IdenticalHist Logical; if `TRUE`, assumes all simulations are
#'   identical apart from the projection recruitment deviations, and calculates
#'   the historical years for a single simulation. If `NULL` (default),
#'   determined automatically by checking whether all simulations are identical.
#' @param silent Logical; if `TRUE`, suppress messages during calculation.
#'
#' @return A [popdynamics-class] object. `Number` is a list of length `nStock`,
#'   each element an array with dimensions `Sim × Age × Year × Area`. `Biomass`,
#'   `SBiomass`, and `SProduction` are arrays with dimensions
#'   `Sim × Stock × Year`. The `Year` dimension covers the historical and
#'   projection years, and is collapsed to a single year only when the values
#'   are constant over time. The `Sim` dimension is collapsed by [ReduceDims()]
#'   when the values are identical across simulations.
#'
#' @seealso [CalcUnfished_Equilibrium()]
#'
#' @export
CalcUnfished_Dynamic <- function(Hist, IdenticalHist=NULL, silent=FALSE) {

  .MsgStep("Calculating dynamic unfished conditions",
           "Calculated dynamic unfished conditions", silent)

  if (inherits(Hist,'om'))
    Hist <- .OM2Hist(Hist, silent=TRUE)
  
  if (EmptyObject(Hist@Unfished@Equilibrium )) 
    Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Hist@OM, silent)
  
  # the engine skips time steps whose numbers are already populated
  Hist@Number <- lapply(Hist@Number, \(x) { x[] <- 0; x })
  Hist <- .CalcDynamicInitial(Hist, Unfished = TRUE)
  
  if (is.null(Hist@Misc$SAVE)) 
    Hist <- .PrepHistMisc(Hist) 
  
  nStock <- nStock(Hist)
  nFleet <- nFleet(Hist)
  nArea <- nArea(Hist)
  nSim <- nSim(Hist)

  HistYears <- Years(Hist, 'H')
  ProjYears <- Years(Hist, 'P')
  AllYears <- Years(Hist)

  Hist_Copy <- .ExtendUnfishedHist(Hist, AllYears)
  Hist_Copy@Misc$Catchability[] <- 1E-15

  if (is.null(IdenticalHist))
    IdenticalHist <- .IdenticalSims(Hist@OM, ignore=c('RecDevProj', 'Depletion'))

  RunDynamics <- function(HistIn, Years, Sims) {
    CalcFisheryDynamics_(HistIn=HistIn,
                         Years=Years,
                         AllYears=AllYears,
                         Sims=Sims,
                         nSim=nSim,
                         nStock,
                         nFleet,
                         nArea,
                         DoCalcCatch=0,
                         clone=1)
  }

  out <- new("popdynamics")
  PopSlots <- setdiff(slotNames('popdynamics'), 'Misc')

  if (!IdenticalHist) {
    unfished <- RunDynamics(Hist_Copy, AllYears, seq_len(nSim))
  } else {
    IdenticalProj <- all(purrr::map_lgl(Hist@OM@Stock, \(stock) .IdenticalSims(stock@SRR@RecDevProj)))
    unfished <- RunDynamics(Hist_Copy, if (IdenticalProj) AllYears else HistYears, 1)
    for (sl in PopSlots)
      slot(unfished, sl) <- .CopyFirstSim(x=slot(unfished, sl))

    if (!IdenticalProj && length(ProjYears)) {
      # projection rec devs differ by sim; rerun from the steps whose recruits enter after the historical period
      nLag <- max(Hist@Misc$RecLag)
      unfished <- RunDynamics(unfished, c(utils::tail(HistYears, nLag), ProjYears), seq_len(nSim))
    }
  }

  out <- .CopySlots(S4in    = unfished, 
                   S4out   = out, 
                   slots   = slotNames(out), 
                   reduce  = TRUE, 
                   IncYear = FALSE)
  for (sl in PopSlots)
    slot(out, sl) <- .CollapseConstantYears(slot(out, sl))
  out@Misc <- list()

  out
}

# Hist arrays used by the engine extended over all years; effort carried forward from the last historical year
.ExtendUnfishedHist <- function(Hist, Years) {
  nSim <- Hist@OM@nSim
  for (sl in slotNames('timeseries')) {
    default <- switch(sl, Misc = NULL, Effort = NULL, Distribution = NA, 0)
    slot(Hist, sl) <- Extend(array = slot(Hist, sl), nSim = nSim, Years = Years, default = default)
  }
  Hist
}

# Year dimension collapsed to the first time step only when all time steps are identical
.CollapseConstantYears <- function(x) {
  if (is.list(x))
    return(lapply(x, .CollapseConstantYears))
  yd <- match('Year', names(dimnames(x)))
  if (!is.array(x) || is.na(yd) || dim(x)[yd] < 2)
    return(x)
  m <- matrix(aperm(x, c(seq_along(dim(x))[-yd], yd)), ncol = dim(x)[yd])
  if (!isTRUE(all(round(m, 4) == round(m[, 1], 4))))
    return(x)
  abind::asub(x, 1, yd, drop = FALSE)
}
