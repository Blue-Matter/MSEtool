
.SimulateOM <- function(OM           = NULL,
                        parallel     = FALSE,
                        silent       = FALSE,
                        nSim         = NULL,
                        control      = SimControl(),
                        Reduce       = TRUE,
                        refpointsMSY = NULL,
                        ...) {

  StartTime <- Sys.time()
  .OnExit()
  .CheckClass(OM)
  OM <- UpdateObject(OM)

  if (!silent) {
    cli::cli_text('')
    cli::cli_alert_info(' Starting  `Simulate` for OM {.val {OM@Name}}')
  }

  OM <- .StartUp(OM, nSim, silent=silent)

  if (is.null(OM@Name) || nchar(OM@Name) < 2)
    OM@Name <- 'Unnamed OM'

  HistYears <- Years(OM, "Historical")
  ProjYears <- Years(OM, "Projection")
  IdenticalHist <- .IdenticalSims(OM, ignore='RecDevProj')

  Hist <- .OM2Hist(OM=OM, silent=silent)

  Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(OM, silent)

  Hist <- .CalcDynamicInitial(Hist)

  # use for access in C++  - removed later
  Hist <- .PrepHistMisc(Hist)

  if (control$DynamicUnfished)
    Hist@Unfished@Dynamic <- CalcUnfished_Dynamic(Hist = Hist,
                                                  IdenticalHist = IdenticalHist,
                                                  silent = silent)

  Hist <- .OptFinalDepletion(Hist, parallel = parallel, silent = silent)

  Hist@Reference@SPR0 <- CalcSPR0(Hist)

  MSYYears <- OM@Control$RefYears %||% utils::tail(Years(Hist@OM, 'Historical'), 1)
  if (Hist@OM@Seasons > 1L) MSYYears <- unique(floor(MSYYears))

  if (!is.null(refpointsMSY)) {
    .ValidateRefpointsMSY(Hist, refpointsMSY, MSYYears)
    Hist@Reference@MSY <- refpointsMSY
    if (!silent)
      cli::cli_alert_info('Using user-supplied {.cls refpointsMSY} object -- skipped MSY reference point calculation')
  } else if (control$MSYRefs) {
    Hist@Reference@MSY <- CalcMSY(Hist,
                                  Years    = MSYYears,
                                  type     = OM@Control$MSYType %||% 'Removals',
                                  parallel = parallel,
                                  silent   = silent)
  }

  if (control$MGT)
    Hist@Reference@MGT <- CalcMGT(Hist, silent = silent)

  Hist <- .CalcFisheryDynamics(Hist, IdenticalSim=IdenticalHist, clone = 1,
                               DoBackCalcEffort = .BackCalcEffortFlag(Hist))
  Hist <- .CalcCatchAtSize(Hist, Years = HistYears)

  if (!is.null(Hist@Reference@MSY))
    Hist@Reference@MSY@Misc$FCurrent <- SumOverFleet(Hist@FInteract) |> 
    .AggregateFToComplex(Hist@OM) |> 
    .AlignDenomYears(as.character(MSYYears))

  if (control$RefPoints)
    Hist <- CalcRefPoints(Hist,
                          Years  = OM@Control$RefYears,
                          type   = OM@Control$MSYType %||% 'Removals',
                          silent = silent)

  if (!silent)
    cli::cli_alert_success("Simulated Historical Fishery")

  ref_types <- c(
    if (control$RefLandings) 'Landings',
    if (control$RefRemovals) 'Removals'
  )

  if (length(ref_types) > 0)
    Hist <- .CalcRefYield(Hist, type=ref_types, Units='Biomass', parallel=parallel, silent=silent)

  Hist <- .RestoreHistMisc(Hist)

  if (control$ConditionObs)
    Hist <- .ConditionObs(Hist, silent, EstimateBeta = control$EstimateBeta %||% TRUE)

  if (control$GenerateData)
    Hist <- .GenerateHistoricalData(Hist, parallel=parallel, silent=silent)

  if (control$BLow)
    Hist <- CalcBLow(Hist, silent = silent)

  Hist <- .AddSimNumber(Hist)
  Hist <- .CheckAllocation(Hist)
  Hist <- .LogDepletionAchievement(Hist)
  Hist <- .ReduceHist(Hist, Reduce)

  EndTime <- Sys.time()

  elapsed <- round(difftime(Sys.time(), StartTime, units='auto'), 2) |> format()
  if (!silent)
    cli::cli_alert_success('Completed `Simulate` for OM {.val {OM@Name}} ({elapsed})')

  Hist@Log <- .JoinLog(OM@Log, Hist@Log)

  if (!silent)
    .CheckLog(Hist, 'Hist')

  .SetDigest(Hist)
}

