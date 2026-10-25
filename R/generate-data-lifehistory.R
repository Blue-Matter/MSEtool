
# Observed life history (Data@LifeHistory) and exploitation (Data@Exploitation).
#
# The observed value of each parameter is the operating-model value in the last
# historical time step multiplied by a per-sim bias and a per-sim lognormal
# error. The observed schedules are regenerated from the observed parameters
# with the Populate* functions, so dependent components (e.g. weight from
# length) follow the observed values.

.LHComponents <- c("Length", "Weight", "NaturalMortality", "Maturity", "Fecundity", "SRR", "Depletion")
.ExComponents <- c("Selectivity", "Retention", "DiscardMortality")

# slot values scaled by the multiplier of the same name
.ObsExtraSlots <- list(Length = "CVatAge", SRR = c("R0", "SD", "AC"), Depletion = "Final")

# slots of a component that are stored in Data
.ObsDataSlots <- list(
  Length           = c("Pars", "Model", "Units", "MeanAtAge", "CVatAge", "Dist", "TruncSD", "Timing", "Classes"),
  Weight           = c("Pars", "Model", "Units", "MeanAtAge", "MeanAtLength", "CVatAge", "Dist", "TruncSD", "Timing", "Classes"),
  NaturalMortality = c("Pars", "Model", "Units", "MeanAtAge", "MeanAtLength", "Classes"),
  Maturity         = c("Pars", "Model", "MeanAtAge", "MeanAtLength", "MeanAtWeight", "Classes"),
  Fecundity        = c("Pars", "Model", "Units", "MeanAtAge", "MeanAtLength", "Classes", "Timing"),
  SRR              = c("Pars", "Model", "R0", "SD", "AC", "TruncSD", "Units", "SpawnTimeFrac", "SpawnLag"),
  Depletion        = c("Final", "Reference"),
  Selectivity      = c("Pars", "Model", "isRel", "MeanAtAge", "MeanAtLength", "MeanAtWeight", "Classes", "isAtLength"),
  Retention        = c("Pars", "Model", "isRel", "MeanAtAge", "MeanAtLength", "MeanAtWeight", "Classes", "isAtLength"),
  DiscardMortality = c("MeanAtAge", "MeanAtLength", "Classes")
)

.ObsClass <- c(Length = "length", Weight = "weight", NaturalMortality = "naturalmortality",
               Maturity = "maturity", Fecundity = "fecundity", SRR = "srr", Depletion = "depletion",
               Selectivity = "selectivity", Retention = "retention",
               DiscardMortality = "discardmortality")

# components bounded by [0, 1] when a schedule is scaled
.ObsBounded <- c("Maturity", "Selectivity", "Retention", "DiscardMortality")

# components that cannot be zero at every age
.ObsPositive <- c("Maturity", "Selectivity", "Retention")

.MaxObsTry <- 10L


# ---- spec lookup and multipliers -------------------------------------------

.SimNamed <- function(x) {
  nms <- names(x)
  !is.null(nms) && all(grepl("^[0-9]+$", nms))
}

# a named atomic vector is a set of per-parameter values; unnamed or sim-named values apply to all
.SpecAsList <- function(x) {
  if (is.atomic(x) && !is.null(names(x)) && !.SimNamed(x)) return(as.list(x))
  x
}

# user value for one field ("Bias" or "CV") of one parameter, else the value conditioned on real data
.SpecLookup <- function(Spec, field, name, Stock, implicit = TRUE) {
  Top <- Spec[[field]]
  if (!is.null(Top)) {
    Top <- .SpecAsList(Top)
    if (!is.list(Top)) {
      if (implicit) return(Top)
    } else if (!is.null(Top[[name]])) {
      return(Top[[name]])
    }
  }
  Spec[["Stock"]][[Stock]][[field]][[name]]
}

# multiplicative bias per sim: a scalar is the CV of a lognormal, length 2 uniform bounds, length nSim values
.ExpandObsBias <- function(x, nSim, Key) {
  if (is.null(x) || !length(x)) return(rep(1, nSim))
  Direct <- .SimNamed(x) && length(x) == nSim
  x <- as.numeric(x)
  if (anyNA(x) || any(x < 0))
    cli::cli_abort("{.arg Bias} must contain non-negative values.", call = NULL)
  if (Direct) return(x)
  if (length(x) == 1)
    return(if (x == 0) rep(1, nSim) else
      .SimDraws(\(i, n) stats::rlnorm(n, mconv(1, x), sdconv(1, x)), nSim, 1, Key = Key)[, 1])
  if (length(x) == 2 && nSim != 2)
    return(.SimDraws(\(i, n) stats::runif(n, min(x), max(x)), nSim, 1, Key = Key)[, 1])
  if (length(x) < nSim)
    cli::cli_abort("{.arg Bias} must have length 1, 2 or {nSim}, not {length(x)}.", call = NULL)
  x[seq_len(nSim)]
}

# CV per sim: a scalar, length nSim values, or c(mean, sd) of a lognormal CV when length 2
.ExpandObsCV <- function(x, nSim, Key) {
  if (is.null(x) || !length(x)) return(rep(0, nSim))
  Direct <- .SimNamed(x) && length(x) == nSim
  x <- as.numeric(x)
  if (anyNA(x) || any(x < 0))
    cli::cli_abort("{.arg CV} must contain non-negative values.", call = NULL)
  if (Direct) return(x)
  if (length(x) == 1) return(rep(x, nSim))
  if (length(x) == 2 && nSim != 2) {
    if (x[2] == 0) return(rep(x[1], nSim))
    return(.SimDraws(\(i, n) stats::rlnorm(n, mconv(x[1], x[2]), sdconv(x[1], x[2])), nSim, 1,
                     Key = Key)[, 1])
  }
  if (length(x) < nSim)
    cli::cli_abort("{.arg CV} must have length 1, 2 or {nSim}, not {length(x)}.", call = NULL)
  x[seq_len(nSim)]
}

# per-sim multipliers (bias * lognormal error) and CVs for the named parameters
.DrawObsMult <- function(Spec, Stock, Names, nSim, KeyBase, Implicit = Names) {
  Mult <- CV <- stats::setNames(vector("list", length(Names)), Names)
  for (nm in Names) {
    Key <- paste(KeyBase, nm, sep = "_")
    imp  <- nm %in% Implicit
    Bias <- .ExpandObsBias(.SpecLookup(Spec, "Bias", nm, Stock, imp), nSim, paste(Key, "Bias"))
    cv   <- .ExpandObsCV(.SpecLookup(Spec, "CV", nm, Stock, imp), nSim, paste(Key, "CVdraw"))
    Err  <- if (any(cv > 0)) {
      .SimDraws(\(i, n) if (cv[i] > 0) stats::rlnorm(n, mconv(1, cv[i]), sdconv(1, cv[i])) else 1,
                nSim, 1, Key = paste(Key, "Error"))[, 1]
    } else rep(1, nSim)
    Mult[[nm]] <- Bias * Err
    CV[[nm]]   <- cv
  }
  list(Mult = Mult, CV = CV)
}

# scale the Sim dimension (first) of an array or per-sim vector
.MultSim <- function(x, mult) {
  if (is.null(x) || !length(x)) return(x)
  nSim <- length(mult)
  if (is.array(x) && "Sim" %in% names(dimnames(x))) {
    if (names(dimnames(x))[1] != "Sim") return(x)
    if (dim(x)[1] != nSim) x <- ExtendSims(x, nSim)
    x[] <- x * rep_len(mult, length(x))
    return(x)
  }
  if (is.numeric(x) && length(x) %in% c(1, nSim)) {
    x <- if (length(x) == 1) rep(x, nSim) else x
    return(x * mult)
  }
  x
}


# ---- snapshot of a component ----------------------------------------------

# the object at `Year`: the last Year column at or before it, the first Area, Year dimname set to `Year`
.SnapshotYear <- function(x, Year) {
  if (is.function(x) || is.null(x)) return(x)

  if (isS4(x)) {
    for (s in methods::slotNames(x)) {
      v <- methods::slot(x, s)
      if (!is.null(v) && !is.function(v))
        methods::slot(x, s, check = FALSE) <- .SnapshotYear(v, Year)
    }
    attr(x, "digest") <- NULL
    return(x)
  }

  if (is.list(x)) {
    x[] <- lapply(x, .SnapshotYear, Year = Year)
    return(x)
  }

  if (is.array(x) && !is.null(dimnames(x))) {
    dn <- dimnames(x)
    yi <- match("Year", names(dn))
    if (!is.na(yi)) {
      yrs <- suppressWarnings(as.numeric(dn[[yi]]))
      ind <- which(yrs <= Year + 1e-8)
      col <- if (length(ind)) max(ind) else 1L
      x   <- abind::asub(x, col, yi, drop = FALSE)
      dn[[yi]] <- as.character(Year)
      dimnames(x) <- dn
    }
    ai <- match("Area", names(dn))
    if (!is.na(ai) && dim(x)[ai] > 1) {
      x <- abind::asub(x, 1L, ai, drop = FALSE)
      dn[[ai]] <- dn[[ai]][1]
      dimnames(x) <- dn
    }
  }
  x
}

.HasSlot <- function(object, name) name %in% methods::slotNames(object)

.ObsParNames <- function(Obj) {
  if (!.HasSlot(Obj, "Pars") || .ParsEmpty(Obj@Pars)) return(character(0))
  Nms <- names(Obj@Pars)
  Nms[!.IsCVPar(Nms)]
}

.ObsExtraNames <- function(Obj, comp) {
  Ex <- .ObsExtraSlots[[comp]]
  Ex[vapply(Ex, \(s) .HasSlot(Obj, s) && length(methods::slot(Obj, s)) > 0 &&
              !all(is.na(methods::slot(Obj, s))), logical(1))]
}

# clear everything regenerated by Populate*
.ClearDerived <- function(Obj) {
  for (s in c("MeanAtAge", "MeanAtLength", "MeanAtWeight", "ALK", "AWK"))
    if (.HasSlot(Obj, s)) methods::slot(Obj, s, check = FALSE) <- NULL
  attr(Obj, "digest") <- NULL
  Obj
}

.ClampObsPar <- function(comp, Model, name, value) {
  if (comp == "SRR" && name == "h") {
    Upper <- if (is.character(Model) && identical(Model, "BevertonHolt")) 0.999 else Inf
    return(pmin(pmax(value, 0.2), Upper))
  }
  if (comp == "SRR" && name == "AC") return(pmin(pmax(value, -0.99), 0.99))
  value
}

# scale parameters (and extra slots) of a snapshot component by the multipliers
.ApplyObsMult <- function(Obj, comp, Mult) {
  Model <- if (.HasSlot(Obj, "Model")) Obj@Model
  if (.HasSlot(Obj, "Pars")) {
    for (nm in intersect(names(Obj@Pars), names(Mult))) {
      Par <- .MultSim(Obj@Pars[[nm]], Mult[[nm]])
      Obj@Pars[[nm]] <- .ClampObsPar(comp, Model, nm, Par)
    }
  }
  for (nm in intersect(.ObsExtraSlots[[comp]], names(Mult))) {
    Val <- .MultSim(methods::slot(Obj, nm), Mult[[nm]])
    methods::slot(Obj, nm, check = FALSE) <- .ClampObsPar(comp, Model, nm, Val)
  }
  Obj
}

# scale a schedule given only as MeanAt* arrays
.ApplyScheduleMult <- function(Obj, comp, mult) {
  for (s in intersect(c("MeanAtAge", "MeanAtLength", "MeanAtWeight"), methods::slotNames(Obj))) {
    Val <- .MultSim(methods::slot(Obj, s), mult)
    if (comp %in% .ObsBounded && !is.null(Val)) Val[] <- pmin(pmax(Val, 0), 1)
    methods::slot(Obj, s, check = FALSE) <- Val
  }
  Obj
}

# one logical per sim: schedules are finite and non-negative
.ValidObserved <- function(Obj, comp, nSim) {
  ok <- rep(TRUE, nSim)
  for (s in intersect(c("MeanAtAge", "MeanAtLength", "MeanAtWeight"), methods::slotNames(Obj))) {
    x <- methods::slot(Obj, s)
    if (is.null(x) || !length(x)) next
    if (!is.array(x) || is.null(dimnames(x)) || names(dimnames(x))[1] != "Sim") next
    okS <- apply(x, 1, \(v) all(is.finite(v)) && all(v >= -1e-8) &&
                   (!comp %in% .ObsPositive || any(v > 0)))
    ok <- ok & rep_len(okS, nSim)
  }
  ok
}


