
# ---- one component ---------------------------------------------------------

# Names an observation spec can refer to, and those an unnamed value applies to
.ObsNames <- function(Truth, comp) {
  Par <- .ObsParNames(Truth)
  Ex  <- .ObsExtraNames(Truth, comp)
  if (length(Par)) return(list(Names = c(Par, Ex), Implicit = Par))
  if (.HasSlot(Truth, "MeanAtAge")) return(list(Names = c(".All", Ex), Implicit = ".All"))
  list(Names = Ex, Implicit = Ex)
}

.WarnUnknownSpec <- function(Spec, Names, Label, comp, Implicit = Names) {
  Given <- c(names(.SpecAsList(Spec[["Bias"]])), names(.SpecAsList(Spec[["CV"]])))
  Given <- unique(Given[nzchar(Given)])
  Given <- Given[!grepl("^[0-9]+$", Given)]
  Unknown <- setdiff(Given, Names)
  if (!length(Unknown)) return(invisible())
  cli::cli_warn(c(
    "!" = "{Label}: {.field {comp}} observation error for {.val {Unknown}} is ignored.",
    "i" = if (identical(Implicit, ".All"))
      "{.field {comp}} is a schedule: use an unnamed {.arg Bias} or {.arg CV}."
    else "The parameters of {.field {comp}} are {.val {Names}}."
  ), call = NULL)
}

# Observe one component; `Build(Mult)` returns the observed object for the multipliers.
# Sims whose observed schedule is invalid are redrawn and then left unperturbed.
.ObserveComponent <- function(comp, Truth, Spec, Stock, nSim, KeyBase, Build, Label) {
  Nms   <- .ObsNames(Truth, comp)
  Names <- Nms$Names
  .WarnUnknownSpec(Spec, Names, Label, comp, Nms$Implicit)

  Draw <- function(attempt)
    .DrawObsMult(Spec, Stock, Names, nSim,
                 if (attempt > 1) paste0(KeyBase, "_try", attempt) else KeyBase,
                 Implicit = Nms$Implicit)

  First <- Draw(1)
  Mult  <- First$Mult
  bad   <- rep(FALSE, nSim)

  for (attempt in seq_len(.MaxObsTry)) {
    Obs <- tryCatch(Build(Mult), error = function(e) e)
    bad <- if (inherits(Obs, "error")) rep(TRUE, nSim) else !.ValidObserved(Obs, comp, nSim)
    if (!any(bad)) return(list(Obj = Obs, CV = First$CV))
    if (attempt == .MaxObsTry) break
    New <- Draw(attempt + 1)$Mult
    for (nm in Names) Mult[[nm]][bad] <- New[[nm]][bad]
  }

  for (nm in Names) Mult[[nm]][bad] <- 1
  Obs <- Build(Mult)
  cli::cli_warn(
    "{Label}: {sum(bad)} of {nSim} simulation{?s} could not draw a valid observed {.field {comp}}; the true values are used.",
    call = NULL)
  list(Obj = Obs, CV = First$CV)
}

# keep the observable slots of a populated component
.StripObserved <- function(Obj, comp) {
  Out <- methods::new(.ObsClass[[comp]])
  for (s in intersect(.ObsDataSlots[[comp]], methods::slotNames(Obj))) {
    v <- methods::slot(Obj, s)
    if (!is.null(v)) methods::slot(Out, s, check = FALSE) <- v
  }
  Out
}

# Populate* call for a component
.PopulateObserved <- function(comp, Obj, Ctx, Ages, Year, nSim, seed) {
  CalcAtLength <- !is.null(Ctx$Length) && !EmptyObject(Ctx$Length)
  switch(comp,
    Length = PopulateLength(Obj, Ages = Ages, Years = Year, nSim = nSim, ALK = TRUE,
                            seed = seed, silent = TRUE, force = TRUE),
    Weight = PopulateWeight(Obj, Ages = Ages, Length = Ctx$Length, Years = Year, nSim = nSim,
                            AWK = TRUE, CalcAtLength = CalcAtLength, seed = seed, silent = TRUE,
                            force = TRUE),
    NaturalMortality = PopulateNaturalMortality(Obj, Ages = Ages, Length = Ctx$Length, Years = Year,
                            nSim = nSim, CalcAtLength = CalcAtLength, seed = seed, silent = TRUE,
                            force = TRUE),
    Maturity = PopulateMaturity(Obj, Ages = Ages, Length = Ctx$Length, Weight = Ctx$Weight,
                            Years = Year, nSim = nSim, CalcAtLength = CalcAtLength, seed = seed,
                            silent = TRUE, force = TRUE),
    Fecundity = PopulateFecundity(Obj, Ages = Ages, Length = Ctx$Length, Weight = Ctx$Weight,
                            Maturity = Ctx$Maturity, Years = Year, nSim = nSim,
                            CalcAtLength = CalcAtLength, seed = seed, silent = TRUE, force = TRUE),
    Selectivity = PopulateSelectivity(Obj, Ages = Ages, Length = Ctx$Length, Weight = Ctx$Weight,
                            Maturity = Ctx$Maturity, nSim = nSim, Years = Year, nArea = 1,
                            CalcAtLength = CalcAtLength, seed = seed, silent = TRUE),
    Retention = PopulateRetention(Obj, Ages = Ages, Length = Ctx$Length, Weight = Ctx$Weight,
                            Maturity = Ctx$Maturity, Selectivity = Ctx$Selectivity, nSim = nSim,
                            Years = Year, nArea = 1, CalcAtLength = CalcAtLength, seed = seed,
                            silent = TRUE, force = TRUE),
    Obj
  )
}

# Observed `comp` for all sims from its snapshot `Truth`
.ObserveOne <- function(comp, Truth, Spec, Stock, Ctx, Ages, Year, nSim, KeyBase, Label) {
  ParBased <- length(.ObsParNames(Truth)) > 0

  Build <- if (ParBased) {
    function(Mult) {
      Obj <- .ApplyObsMult(Truth, comp, Mult) |> .ClearDerived()
      .PopulateObserved(comp, Obj, Ctx, Ages, Year, nSim, paste(KeyBase, "populate"))
    }
  } else {
    function(Mult) {
      Obj <- .ApplyObsMult(Truth, comp, Mult)
      if (".All" %in% names(Mult)) Obj <- .ApplyScheduleMult(Obj, comp, Mult[[".All"]])
      if (comp == "Length" && ".All" %in% names(Mult)) {
        Obj@ALK <- NULL
        attr(Obj, "digest") <- NULL
        Obj <- .PopulateObserved(comp, Obj, Ctx, Ages, Year, nSim, paste(KeyBase, "populate"))
      }
      Obj
    }
  }

  .ObserveComponent(comp, Truth, Spec, Stock, nSim, KeyBase, Build, Label)
}


# ---- stocks and fleets -----------------------------------------------------

.SnapshotComponents <- function(object, comps, Year)
  stats::setNames(lapply(comps, \(comp) .SnapshotYear(methods::slot(object, comp), Year)), comps)

# `lifehistoryobs` of a complex: the first fleet's non-empty one
.ComplexLHSpec <- function(ObsList, FleetNms) {
  for (obs in ObsList[intersect(FleetNms, names(ObsList))])
    if (!EmptyObject(obs@LifeHistory)) return(obs@LifeHistory)
  NULL
}

# Components the real data already provide for a stock
.RealPopulated <- function(Real, comp) {
  !is.null(Real) && .HasSlot(Real, comp) && !EmptyObject(methods::slot(Real, comp))
}

# Observed life history of a stock (all sims). `Obj` is NULL when nothing is observed.
.ObserveStockBiology <- function(Stock, LHObs, Real, StockName, Year, nSim, Seed) {
  Truth <- .SnapshotComponents(Stock, .LHComponents, Year)
  Ctx   <- Truth
  Obj   <- methods::new("lifehistorydata")
  ParCV <- list()
  Label <- cli::format_inline("Observed life history of {.val {StockName}}")

  for (comp in .LHComponents) {
    Spec <- methods::slot(LHObs, comp)
    if (!length(Spec) || .RealPopulated(Real, comp)) next
    if (EmptyObject(Truth[[comp]])) {
      cli::cli_warn("{Label}: {.field {comp}} is not defined, so it is not observed.", call = NULL)
      next
    }
    Res <- .ObserveOne(comp, Truth[[comp]], Spec, StockName, Ctx, Stock@Ages, Year, nSim,
                       paste(Seed, "LHObs", StockName, comp, sep = "_"), Label)
    Ctx[[comp]] <- Res$Obj
    methods::slot(Obj, comp) <- .StripObserved(Res$Obj, comp)
    ParCV[[comp]] <- Res$CV
  }

  if (!length(ParCV)) return(list(Obj = NULL, Ctx = Ctx))
  Obj@Ages <- Stock@Ages
  list(Obj = Obj, ParCV = ParCV, Ctx = Ctx)
}

# Observed exploitation of a fleet for a stock (all sims). NULL when nothing is observed.
.ObserveFleetExploitation <- function(Fleet, ExObs, Real, Ctx, Ages, StockName, FleetName, Year, nSim, Seed) {
  Truth <- .SnapshotComponents(Fleet, .ExComponents, Year)
  Obj   <- methods::new("exploitationdata")
  ParCV <- list()
  Label <- cli::format_inline("Observed exploitation of {.val {FleetName}} for {.val {StockName}}")

  for (comp in .ExComponents) {
    Spec <- methods::slot(ExObs, comp)
    if (!length(Spec) || .RealPopulated(Real, comp)) next
    if (EmptyObject(Truth[[comp]])) {
      cli::cli_warn("{Label}: {.field {comp}} is not defined, so it is not observed.", call = NULL)
      next
    }
    Res <- .ObserveOne(comp, Truth[[comp]], Spec, StockName, Ctx, Ages, Year, nSim,
                       paste(Seed, "ExObs", StockName, FleetName, comp, sep = "_"), Label)
    Ctx[[comp]] <- Res$Obj
    methods::slot(Obj, comp) <- .StripObserved(Res$Obj, comp)
    ParCV[[comp]] <- Res$CV
  }

  if (!length(ParCV)) return(NULL)
  list(Obj = Obj, ParCV = ParCV)
}

# Observed life history and exploitation for every complex, stock and fleet.
# Returns list(LH = [[complex]][[stock]], Ex = [[complex]][[stock]][[fleet]]); both NULL when nothing is observed.
.GenerateObservedBiology <- function(Hist, HistYears) {
  OM <- Hist@OM
  if (is.null(OM@Obs)) return(list(LH = NULL, Ex = NULL))

  Complexes <- OM@Complexes
  StockNms  <- StockNames(OM)
  FleetNms  <- FleetNames(OM)
  nSim      <- OM@nSim
  Seed      <- OM@Seed %||% 101
  Year      <- max(HistYears)

  LH <- Ex <- stats::setNames(vector("list", length(Complexes)), names(Complexes))
  Any <- FALSE

  for (i in seq_along(Complexes)) {
    ObsList  <- OM@Obs[[i]]
    LHSpec   <- .ComplexLHSpec(ObsList, FleetNms)
    ExFleets <- FleetNms[vapply(FleetNms, \(fl) fl %in% names(ObsList) &&
                                  !EmptyObject(ObsList[[fl]]@Exploitation), logical(1))]
    if (is.null(LHSpec) && !length(ExFleets)) next

    Real <- if (length(OM@Data) >= i) OM@Data[[i]]

    for (s in Complexes[[i]]) {
      nm     <- StockNms[s]
      Stock  <- OM@Stock[[s]]
      RealLH <- if (!is.null(Real)) .StockLifeHistory(Real, nm)

      Ctx <- NULL
      if (!is.null(LHSpec)) {
        Res <- .ObserveStockBiology(Stock, LHSpec, RealLH, nm, Year, nSim, Seed)
        Ctx <- Res$Ctx
        if (!is.null(Res$Obj)) {
          LH[[i]][[nm]] <- Res[c("Obj", "ParCV")]
          Any <- TRUE
        }
      }

      for (fl in ExFleets) {
        if (is.null(Ctx))
          Ctx <- .SnapshotComponents(Stock, .LHComponents, Year)
        RealEx <- if (!is.null(Real)) .StockFleetExploitation(Real, nm, fl)
        Res <- .ObserveFleetExploitation(OM@Fleet[[s]][[fl]], ObsList[[fl]]@Exploitation, RealEx,
                                         Ctx, Stock@Ages, nm, fl, Year, nSim, Seed)
        if (!is.null(Res)) {
          Ex[[i]][[nm]][[fl]] <- Res
          Any <- TRUE
        }
      }
    }
  }

  if (!Any) return(list(LH = NULL, Ex = NULL))
  list(LH = LH, Ex = Ex)
}


# ---- per sim ---------------------------------------------------------------

# Pars as numbers, with the CV of each observed parameter as `<name>_CV`
.ScalarizePars <- function(Obj, CV, sim) {
  if (!.HasSlot(Obj, "Pars") || !length(Obj@Pars)) return(Obj)
  Pars <- lapply(Obj@Pars, \(p) as.numeric(p)[1])
  for (nm in intersect(names(CV), names(Pars))) {
    cv <- CV[[nm]][sim]
    if (length(cv) && is.finite(cv) && cv > 0) Pars[[paste0(nm, "_CV")]] <- cv
  }
  Obj@Pars <- Pars
  Obj
}

# observed object of one sim
.SliceObserved <- function(Gen, sim, nSim) {
  Obj <- if (nSim > 1) .SubsetSim(Gen$Obj, sim, keep_sim_name = FALSE, nSim = nSim) else Gen$Obj
  for (comp in intersect(names(Gen$ParCV), methods::slotNames(Obj)))
    methods::slot(Obj, comp) <- .ScalarizePars(methods::slot(Obj, comp), Gen$ParCV[[comp]], sim)
  Obj
}

# components of `Real` that are populated win; the others come from `Gen`
.MergeObserved <- function(Real, Gen) {
  for (s in setdiff(methods::slotNames(Real), "Misc"))
    if (EmptyObject(methods::slot(Real, s)))
      methods::slot(Real, s, check = FALSE) <- methods::slot(Gen, s)
  Real
}

.GenHistDataLifeHistory <- function(sim, Data, PreGen, StockNms, nSim) {
  Out <- list()
  for (nm in StockNms) {
    Real <- .StockLifeHistory(Data, nm)
    Gen  <- PreGen[[nm]]
    LH <- if (is.null(Gen)) Real else {
      Obj <- .SliceObserved(Gen, sim, nSim)
      if (is.null(Real)) Obj else .MergeObserved(Real, Obj)
    }
    if (!is.null(LH)) Out[[nm]] <- LH
  }
  if (length(Out)) Out else Data@LifeHistory
}

.GenHistDataExploitation <- function(sim, Data, PreGen, StockNms, FleetNms, nSim) {
  Out <- list()
  for (nm in StockNms) {
    for (fl in FleetNms) {
      Real <- .StockFleetExploitation(Data, nm, fl)
      Gen  <- PreGen[[nm]][[fl]]
      Ex <- if (is.null(Gen)) Real else {
        Obj <- .SliceObserved(Gen, sim, nSim)
        if (is.null(Real)) Obj else .MergeObserved(Real, Obj)
      }
      if (!is.null(Ex)) Out[[nm]] <- c(Out[[nm]], stats::setNames(list(Ex), fl))
    }
  }
  if (length(Out)) Out else Data@Exploitation
}
