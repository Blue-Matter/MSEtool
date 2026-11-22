# Effort controls in the projection ([Sim, Year, Fleet]); Advised is before implementation error
.InitEffortAttainment <- function(Proj, YearsProj) {
  DN  <- list(Sim = seq_len(Proj@OM@nSim), Year = YearsProj, Fleet = FleetNames(Proj))
  arr <- \(value) array(value, unname(lengths(DN)), dimnames = DN)
  list(Advised     = arr(NA_real_),
       Implemented = arr(NA_real_),
       Capped      = arr(NA_real_),
       Controlled  = arr(FALSE))
}

# each time step's effort is set by that step's update, so its record starts empty
.ClearEffortAttainment <- function(Proj, YearIndex) {
  Rec <- Proj@Misc$EffortAttainment
  if (is.null(Rec) || is.na(YearIndex) || YearIndex < 1) return(Proj)
  Rec$Advised[, YearIndex, ]     <- NA
  Rec$Implemented[, YearIndex, ] <- NA
  Rec$Capped[, YearIndex, ]      <- NA
  Rec$Controlled[, YearIndex, ]  <- FALSE
  Proj@Misc$EffortAttainment <- Rec
  Proj
}

# Advised, Implemented and Capped are [time step x fleet] for the projection time steps YearIndex
.RecordEffortAttainment <- function(Proj, sim, YearIndex, Advised, Implemented, Capped, Controlled) {
  Rec <- Proj@Misc$EffortAttainment
  if (is.null(Rec)) return(Proj)
  ok <- YearIndex >= 1 & YearIndex <= dim(Rec$Advised)[2]
  if (!any(ok)) return(Proj)
  YearIndex <- YearIndex[ok]
  Fleets    <- which(Controlled)
  Rec$Controlled[sim, YearIndex, ] <- matrix(Controlled, length(YearIndex), length(Controlled), byrow = TRUE)
  Rec$Advised[sim, YearIndex, ]     <- NA
  Rec$Implemented[sim, YearIndex, ] <- NA
  Rec$Capped[sim, YearIndex, ]      <- NA
  Rec$Advised[sim, YearIndex, Fleets]     <- Advised[ok, Fleets]
  Rec$Implemented[sim, YearIndex, Fleets] <- Implemented[ok, Fleets]
  Rec$Capped[sim, YearIndex, Fleets]      <- Capped[ok, Fleets]
  Proj@Misc$EffortAttainment <- Rec
  Proj
}

# Why a controlled fleet fished less than its implemented effort, in the order the limits apply
.EffortLimit <- function(Implemented, Capped, Effort, TimeFraction, MaxFHit, tol = 1e-6) {
  Short <- is.finite(Effort) & is.finite(Implemented) & Effort < Implemented * (1 - tol) - 1e-10
  out <- rep('Other', length(Short))
  out[MaxFHit] <- 'MaxF'
  out[!is.na(TimeFraction) & TimeFraction < 1 - tol] <- 'BagLimit'
  out[is.finite(Capped) & Capped < Implemented * (1 - tol)] <- 'Maximum'
  out[!Short] <- 'None'
  out[!is.finite(Effort) | !is.finite(Implemented)] <- NA_character_
  out
}

# One log warning per MP: sim-time steps where a fleet under an effort control fished less
# than its implemented effort
.LogEffortAttainment <- function(Proj, Seasons = 1) {
  Rec <- Proj@Misc$EffortAttainment
  if (is.null(Rec) || !any(Rec$Controlled, na.rm = TRUE)) return(Proj)
  YearsProj <- dimnames(Rec$Advised)$Year
  Effort <- Proj@Effort[, match(YearsProj, dimnames(Proj@Effort)$Year), , drop = FALSE] |>
    ExtendSims(dim(Rec$Advised)[1])
  TimeFraction <- if (is.null(Proj@Misc$BagLimit)) NA else Proj@Misc$BagLimit$TimeFraction
  MaxFHit <- if (is.null(Proj@Misc$MaxF)) FALSE else .MaxFHit(Proj@Misc$MaxF)
  Controlled <- !is.na(Rec$Controlled) & Rec$Controlled
  Limit <- .EffortLimit(Rec$Implemented, Rec$Capped, Effort, TimeFraction, MaxFHit)
  Limit <- array(Limit, dim(Rec$Advised))
  Limit[!Controlled] <- NA

  AnyStep <- \(x) apply(x, c(1, 2), any)
  nTotal  <- sum(AnyStep(Controlled))
  Causes  <- c(Maximum = 'effort maximum', BagLimit = 'bag limit', MaxF = 'maxF', Other = 'other')
  nCause  <- vapply(names(Causes), \(cs) sum(AnyStep(!is.na(Limit) & Limit == cs)), numeric(1))
  nShort  <- sum(AnyStep(!is.na(Limit) & Limit != 'None'))
  if (!nShort) return(Proj)
  Shown <- nCause > 0
  Unit  <- if (Seasons > 1) 'sim-time steps' else 'sim-years'
  msg <- paste0("Effort advice not fished in full in ", nShort, " of ", nTotal, " ", Unit, " (",
                paste(Causes[Shown], nCause[Shown], collapse = ', '),
                "); see `EffortAttainment()`")
  .CaptureLog(Proj, msg, name = 'EffortAttainment', type = 'warning')
}
