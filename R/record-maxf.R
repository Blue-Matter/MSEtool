# Record of where the `OM@maxF` cap reduced fishing in the projection: the
# fraction of each fleet's requested effort it fished ([Sim, Year, Fleet], 1 =
# not capped). Kept only for MPs where the cap bound.

.InitMaxFRecord <- function(Proj, YearsProj) {
  DN <- list(Sim = seq_len(Proj@OM@nSim), Year = YearsProj, Fleet = FleetNames(Proj))
  array(1, unname(lengths(DN)), dimnames = DN)
}

# realised over requested effort after the engine has back-calculated the effort of capped fleets
.RecordMaxF <- function(Proj, ts, TSIndex, Requested) {
  Rec <- Proj@Misc$MaxF
  if (is.null(Rec) || is.na(TSIndex)) return(Proj)
  Realised <- Proj@Effort[, TSIndex, , drop = FALSE]
  Frac <- ifelse(is.finite(Requested) & Requested > 0, Realised / Requested, 1)
  Rec[, ts, ] <- pmin(Frac, 1)
  Proj@Misc$MaxF <- Rec
  Proj
}

.MaxFHit <- function(Rec) !is.na(Rec) & Rec < 1 - 1e-6

# One log warning per MP; drops the record when the cap never bound
.LogMaxF <- function(Proj, Seasons = 1) {
  Rec <- Proj@Misc$MaxF
  if (is.null(Rec)) return(Proj)
  Hit <- .MaxFHit(Rec)
  if (!any(Hit)) {
    Proj@Misc$MaxF <- NULL
    return(Proj)
  }
  AnyStep <- \(x) apply(x, c(1, 2), any)
  nHit   <- sum(AnyStep(Hit))
  nTotal <- sum(AnyStep(!is.na(Rec)))
  Unit   <- if (Seasons > 1) 'sim-time steps' else 'sim-years'
  msg <- paste0("Fishing mortality reached `OM@maxF` (", Proj@OM@maxF, ") in ", nHit, " of ", nTotal,
                " ", Unit, ", reducing effort by up to ", round(100 * (1 - min(Rec[Hit]))),
                "%; see `MaxFEffect()`")
  .CaptureLog(Proj, msg, name = 'MaxF', type = 'warning')
}
