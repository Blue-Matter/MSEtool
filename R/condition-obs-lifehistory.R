
# Conditioning of the life history and exploitation observation models on real data.
#
# For each parameter in both the real data and the operating model, the bias of
# a sim is the ratio of the real value to the sim's value in the last historical
# time step, and the CV is the `<name>_CV` reported with the real data. Values
# are stored by stock in the `Stock` element of the component, and the user's own
# `Bias` and `CV` are kept.

# real Pars as values and CVs; `<name>_CV` and `CV_<name>` entries are CVs
.SplitParsCV <- function(Pars) {
  Nms    <- names(Pars)
  IsCV   <- .IsCVPar(Nms)
  Base   <- sub("^CV_", "", sub("_CV$", "", Nms))
  Values <- lapply(Pars[!IsCV], \(p) suppressWarnings(as.numeric(p)[1]))
  CV     <- lapply(Pars[IsCV], \(p) suppressWarnings(as.numeric(p)[1]))
  names(CV) <- Base[IsCV]
  list(Values = Values, CV = CV)
}

# per-sim mean of an array or vector with a Sim dimension
.SimMeans <- function(x, nSim) {
  if (is.null(x) || !length(x)) return(rep(NA_real_, nSim))
  if (is.array(x) && "Sim" %in% names(dimnames(x))) {
    x <- ExtendSims(x, nSim)
    return(unname(apply(x, 1, mean, na.rm = TRUE)))
  }
  rep_len(as.numeric(x), nSim)
}

# [nSim x nAge] schedule of a snapshot
.SimSchedule <- function(x, nSim) {
  x <- ExtendSims(x, nSim)
  matrix(x, nrow = nSim)
}

# the final-year schedule of real data as a vector
.RealSchedule <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  if (!is.array(x)) return(as.numeric(x))
  x   <- .SnapshotYear(x, Inf)
  dn  <- names(dimnames(x))
  Dim <- if (!is.null(dn)) match(TRUE, dn %in% c("Age", "Class")) else NA_integer_
  if (is.na(Dim)) Dim <- which.max(dim(x))
  idx <- lapply(dim(x), \(d) 1L)
  idx[[Dim]] <- seq_len(dim(x)[Dim])
  as.numeric(do.call(`[`, c(list(x), idx, list(drop = TRUE))))
}

# Bias and CV of one component: list(Bias, CV), or NULL when nothing can be conditioned
.ConditionComponent <- function(comp, RealComp, Truth, nSim) {
  Bias <- CV <- list()
  Ratio <- function(real, true) {
    r <- real / true
    r[!is.finite(r) | r <= 0] <- 1
    .NameSims(r, nSim)
  }

  TruthNames <- .ObsParNames(Truth)
  RealNames  <- .ObsParNames(RealComp)

  if (length(TruthNames) && length(RealNames)) {
    Real <- .SplitParsCV(RealComp@Pars)
    for (nm in intersect(names(Real$Values), TruthNames)) {
      if (!is.finite(Real$Values[[nm]])) next
      Bias[[nm]] <- Ratio(Real$Values[[nm]], .SimMeans(Truth@Pars[[nm]], nSim))
      if (!is.null(Real$CV[[nm]]) && is.finite(Real$CV[[nm]]))
        CV[[nm]] <- Real$CV[[nm]]
    }
  } else if (!length(TruthNames) && !length(RealNames) &&
             .HasSlot(Truth, "MeanAtAge") && !is.null(Truth@MeanAtAge) &&
             .HasSlot(RealComp, "MeanAtAge") && !is.null(RealComp@MeanAtAge)) {
    Real  <- .RealSchedule(RealComp@MeanAtAge)
    Sched <- .SimSchedule(Truth@MeanAtAge, nSim)
    if (length(Real) == ncol(Sched))
      Bias[[".All"]] <- Ratio(sum(Real, na.rm = TRUE), rowSums(Sched, na.rm = TRUE))
  }

  Default <- if (length(.ObsExtraSlots[[comp]])) get(comp, envir = asNamespace("MSEtool"))()
  for (nm in .ObsExtraSlots[[comp]]) {
    if (!.HasSlot(RealComp, nm) || !.HasSlot(Truth, nm)) next
    if (identical(methods::slot(RealComp, nm), methods::slot(Default, nm))) next
    real <- suppressWarnings(mean(as.numeric(methods::slot(RealComp, nm)), na.rm = TRUE))
    if (!is.finite(real)) next
    Bias[[nm]] <- Ratio(real, .SimMeans(methods::slot(Truth, nm), nSim))
  }

  if (!length(Bias) && !length(CV)) return(NULL)
  list(Bias = Bias, CV = CV)
}

# Names the user's own Bias or CV covers
.UserCovers <- function(Spec, field, name, Implicit) {
  Top <- Spec[[field]]
  if (is.null(Top)) return(FALSE)
  Top <- .SpecAsList(Top)
  if (!is.list(Top)) return(name %in% Implicit)
  !is.null(Top[[name]])
}

# conditioned values for the names the user did not set
.StoreConditioned <- function(Spec, Cond, StockName, Implicit) {
  New <- list()
  for (field in c("Bias", "CV")) {
    Vals <- Cond[[field]]
    Vals <- Vals[!vapply(names(Vals), \(nm) .UserCovers(Spec, field, nm, Implicit), logical(1))]
    if (length(Vals)) New[[field]] <- Vals
  }
  Stock <- Spec[["Stock"]]
  if (is.null(Stock)) Stock <- list()
  Stock[[StockName]] <- if (length(New)) New else NULL
  Spec[["Stock"]] <- if (length(Stock)) Stock else NULL
  Spec
}

# the fleet whose Obs holds the life history specification of a complex
.ComplexLHFleet <- function(ObsList, FleetNms) {
  for (fl in intersect(FleetNms, names(ObsList)))
    if (!EmptyObject(ObsList[[fl]]@LifeHistory)) return(fl)
  FleetNms[1]
}

.ConditionObsLifeHistory <- function(Hist, FisheryData, HistYears, stocks, i) {
  OM       <- Hist@OM
  StockNms <- StockNames(OM)
  FleetNms <- FleetNames(OM)
  nSim     <- OM@nSim
  Year     <- max(HistYears)
  fl       <- .ComplexLHFleet(OM@Obs[[i]], FleetNms)
  LHObs    <- OM@Obs[[i]][[fl]]@LifeHistory
  Changed  <- FALSE

  for (s in stocks) {
    nm   <- StockNms[s]
    Real <- .StockLifeHistory(FisheryData, nm)
    if (is.null(Real)) next
    Stock <- OM@Stock[[s]]

    for (comp in .LHComponents) {
      RealComp <- methods::slot(Real, comp)
      if (EmptyObject(RealComp) || EmptyObject(methods::slot(Stock, comp))) next
      Truth <- .SnapshotYear(methods::slot(Stock, comp), Year)
      Cond  <- .ConditionComponent(comp, RealComp, Truth, nSim)
      if (is.null(Cond)) next
      Spec <- methods::slot(LHObs, comp)
      methods::slot(LHObs, comp) <- .StoreConditioned(Spec, Cond, nm, .ObsNames(Truth, comp)$Implicit)
      Changed <- TRUE
    }
  }

  if (Changed) Hist@OM@Obs[[i]][[fl]]@LifeHistory <- LHObs
  Hist
}

.ConditionObsExploitation <- function(Hist, FisheryData, HistYears, stocks, i) {
  OM       <- Hist@OM
  StockNms <- StockNames(OM)
  FleetNms <- FleetNames(OM)
  nSim     <- OM@nSim
  Year     <- max(HistYears)

  for (s in stocks) {
    nm <- StockNms[s]
    for (fl in intersect(FleetNms, names(OM@Obs[[i]]))) {
      Real <- .StockFleetExploitation(FisheryData, nm, fl)
      if (is.null(Real)) next
      Fleet <- OM@Fleet[[s]][[fl]]
      ExObs <- OM@Obs[[i]][[fl]]@Exploitation
      Changed <- FALSE

      for (comp in .ExComponents) {
        RealComp <- methods::slot(Real, comp)
        if (EmptyObject(RealComp) || EmptyObject(methods::slot(Fleet, comp))) next
        Truth <- .SnapshotYear(methods::slot(Fleet, comp), Year)
        Cond  <- .ConditionComponent(comp, RealComp, Truth, nSim)
        if (is.null(Cond)) next
        Spec <- methods::slot(ExObs, comp)
        methods::slot(ExObs, comp) <- .StoreConditioned(Spec, Cond, nm, .ObsNames(Truth, comp)$Implicit)
        Changed <- TRUE
      }

      if (Changed) Hist@OM@Obs[[i]][[fl]]@Exploitation <- ExObs
    }
  }
  Hist
}
