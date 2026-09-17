
.SimulateOM <- function(OM           = NULL,
                        parallel     = FALSE,
                        silent       = FALSE,
                        nSim         = NULL,
                        control      = SimControl(),
                        Reduce       = TRUE,
                        refpointsMSY = NULL,
                        ...) {

  # ---- setup & validation ----
  StartTime <- Sys.time()
  .OnExit()
  .CheckClass(OM)
  OM <- UpdateObject(OM)

  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_info(' Starting `Simulate` for OM {.val {OM@Name}}')
  }

  OM <- .StartUp(OM, nSim, silent=silent)

  if (is.null(OM@Name) || nchar(OM@Name) < 2)
    OM@Name <- 'Unnamed OM'

  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")

  # sims identical (aside from proj rec devs) can skip redundant per-sim work below
  IdenticalHist <- .IdenticalSims(OM, ignore='RecDevProj')

  # ---- build hist object ----
  Hist <- .OM2Hist(OM, silent)

  # calculate unfished - equilibrium 
  Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(OM, silent)

  # calculate initial time step
  Hist <- .CalcDynamicInitial(Hist)

  # prepare hist@Misc list - used for access in C++  - removed later
  Hist <- .PrepHistMisc(Hist)

  # calculate unfished - dynamics 
  if (control$DynamicUnfished)
    Hist@Unfished@Dynamic <- CalcUnfished_Dynamic(Hist = Hist,
                                                  IdenticalHist = IdenticalHist,
                                                  silent = silent)

  # ---- optimize q for specified depletion (if applicable)
  Hist <- .OptFinalDepletion(Hist, parallel = parallel, silent = silent)

  # ---- calculate unfished spawning per recruit ----
  Hist@Reference@SPR0 <- CalcSPR0(Hist)

  # ---- MSY reference points ----
  MSYYears <- OM@Control$RefYears %||% utils::tail(Years(Hist@OM, 'Historical'), 1)
  if (Hist@OM@Seasons > 1L) MSYYears <- unique(floor(MSYYears)) # collapse sub-annual time steps to whole years

  if (!is.null(refpointsMSY)) {
    # user-supplied refpoints: validate and reuse instead of recalculating MSY
    .ValidateRefpointsMSY(Hist, refpointsMSY, MSYYears)
    Hist@Reference@MSY <- refpointsMSY
    if (!silent)
      cli::cli_alert_info('Using user-supplied {.cls refpointsMSY} object -- skipped MSY reference point calculation')
  } else if (control$MSYRefs) {
    MSYFun <- if (control$MSYRefsCpp) CalcMSYCpp else CalcMSY
    Hist@Reference@MSY <- MSYFun(Hist,
                                 Years    = MSYYears,
                                 type     = OM@Control$MSYType %||% 'Removals',
                                 parallel = parallel,
                                 silent   = silent)
  }

  if (control$MGT)
    Hist@Reference@MGT <- CalcMGT(Hist, silent = silent)

  # ---- historical fishery dynamics & catch-at-size ----
  Hist <- local({
    if (!silent)
      cli::cli_progress_message("Calculating Historical Fishery Dynamics")
    .CalcFisheryDynamics(Hist, IdenticalSim=IdenticalHist, clone = 1,
                         DoBackCalcEffort = .BackCalcEffortFlag(Hist))
  })
  if (!silent)
    cli::cli_alert_success("Calculated Historical Fishery Dynamics")

  CatchAtSizeNeeded <- .NeedsCatchAtSize(OM, control)
  CatchAtSizeCpp    <- control$CalcCatchAtSizeCpp %||% TRUE
  Hist@OM@Control$CalcCatchAtSizeNeeded <- CatchAtSizeNeeded
  Hist@OM@Control$CalcCatchAtSizeCpp    <- CatchAtSizeCpp

  if (is.na(control$CalcCatchAtSize %||% NA) && !all(CatchAtSizeNeeded))
    Hist <- .CaptureLog(Hist,
      string = cli::format_inline(
        "Skipping catch-at-size for {sum(!CatchAtSizeNeeded)} of {length(CatchAtSizeNeeded)} stock{?s}. No size-composition observation model or real data configured. Set {.code SimControl(CalcCatchAtSize = TRUE)} to force it."
      ),
      name = 'CatchAtSize',
      type = 'assumption'
    )

  AnyCatchAtSizeNeeded <- any(CatchAtSizeNeeded)

  Hist <- local({
    if (!silent && AnyCatchAtSizeNeeded)
      cli::cli_progress_message("Calculating Historical Catch-at-Size")
    .CalcCatchAtSize(Hist, Years = HistYears, needed = CatchAtSizeNeeded,
                     useCpp = CatchAtSizeCpp)
  })

  if (!silent && AnyCatchAtSizeNeeded)
    cli::cli_alert_success("Calculated Historical Catch-at-Size")

  # ---- reference points & ref yield ----
  if (!is.null(Hist@Reference@MSY))
    Hist@Reference@MSY@Misc$FCurrent <- SumOverFleet(Hist@FInteract) |>
    .AggregateFToComplex(Hist@OM) |>
    .AlignDenomYears(as.character(MSYYears))

  if (control$RefPoints) {
    Hist <- local({
      if (!silent)
        cli::cli_progress_message("Calculating Reference Points")
      CalcRefPoints(Hist,
                    Years  = OM@Control$RefYears,
                    type   = OM@Control$MSYType %||% 'Removals',
                    silent = TRUE)
    })
    if (!silent)
      cli::cli_alert_success("Calculated Reference Points")
  }
  
  ref_types <- c(
    if (control$RefLandings) 'Landings',
    if (control$RefRemovals) 'Removals'
  )

  if (length(ref_types) > 0)
    Hist <- .CalcRefYield(Hist, type=ref_types, Units='Biomass', parallel=parallel, silent=silent)

  Hist <- .RestoreHistMisc(Hist) # undo .PrepHistMisc above

  # ---- observation conditioning & data generation ----
  if (control$ConditionObs)
    Hist <- .ConditionObs(Hist, silent, EstimateBeta = control$EstimateBeta %||% TRUE)

  if (control$GenerateData)
    Hist <- .GenerateHistoricalData(Hist, parallel=parallel, silent=silent)

  if (control$BLow)
    Hist <- CalcBLow(Hist, silent = silent)

  # ---- finalize ----
  Hist <- .AddSimNumber(Hist)
  Hist <- .CheckAllocation(Hist)
  Hist <- .CheckSeasonalAllocation(Hist)
  Hist <- .LogDepletionAchievement(Hist)
  Hist <- .ReduceHist(Hist, Reduce)

  elapsed <- round(difftime(Sys.time(), StartTime, units='auto'), 2) |> format()
  if (!silent)
    cli::cli_alert_success('Completed `Simulate` for OM {.val {OM@Name}} ({elapsed})')

  Hist@Log <- .JoinLog(OM@Log, Hist@Log)

  if (!silent)
    .CheckLog(Hist, 'Hist')

  .SetDigest(Hist) 
}

