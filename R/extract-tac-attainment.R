#' Extract TAC Attainment from an MSE Object
#'
#' Reports, for every simulation, time step, stock complex, and fleet where a
#' TAC applied in the projection, the TAC the fleet fished to and the catch it
#' took.
#'
#' A TAC is a catch target: each fleet fishes until it has caught its share of
#' the TAC, or until its effort reaches a ceiling. The ceiling is the fleet's
#' maximum effort (`Effort@Maximum`, see [Effort()]) where one is set,
#' otherwise the effort at which the fishing mortality cap (`OM@maxF`) is
#' reached, and never more than any effort advice from the MP. A TAC that is
#' not caught is reported with the reason it was not caught (`Limit`).
#'
#' In seasonal operating models, each time step's TAC is that season's share of
#' the annual TAC (`OM@SeasonalAllocation`). A share that is not caught in its
#' season is not carried over to later seasons.
#'
#' @param MSE An [mse-class] object.
#'
#' @return A data frame with one row per simulation, stock complex, MP, time
#'   step, and fleet with a TAC:
#'   - `Sim`: simulation index.
#'   - `Stock`: stock complex name (`names(OM@Complexes)`).
#'   - `MP`: management procedure name.
#'   - `Year`: calendar year (or decimal date for sub-annual time steps).
#'   - `Fleet`: fleet name.
#'   - `TAC`: the fleet's TAC in this time step, after fleet and seasonal
#'     allocation and implementation error, in the units and catch type
#'     (removals or landings) of the advice.
#'   - `Catch`: the fleet's catch of the complex in this time step of the
#'     projection, in the same units and catch type.
#'   - `Attained`: `Catch / TAC`; `NA` where the TAC is zero.
#'   - `Limit`: `"None"` where the TAC was caught; `"Ceiling"` where the fleet
#'     was at its effort ceiling; `"MaxF"` where the fishing mortality cap
#'     (`OM@maxF`) reduced the fleet's effort (see [MaxFEffect()]); `"Choke"`
#'     where the fleet stopped because the TAC of another complex was reached
#'     first (multi-complex operating models, see `Imp@TAC@Compliance`);
#'     `"Solver"` where the fleet was below its ceiling because the effort
#'     solver did not converge (also reported by [SummarizeConvergence()]).
#'   - `Advised`: the fleet's TAC from the MP in this time step, after fleet
#'     and seasonal allocation and before implementation error.
#'   - `Binding`: the stock complex whose TAC set the fleet's effort. In
#'     multi-complex operating models, a fleet with `Limit = "Choke"` stopped
#'     at this complex's TAC.
#'
#' @seealso [TACs()], [PM_TACAttained()], [SummarizeConvergence()],
#'   [EffortAttainment()], [MaxFEffect()]
#' @export
TACAttainment <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  empty <- data.frame(Sim = numeric(0), Stock = character(0), MP = character(0),
                      Year = numeric(0), Fleet = character(0), TAC = numeric(0),
                      Catch = numeric(0), Attained = numeric(0), Limit = character(0),
                      Advised = numeric(0), Binding = character(0))

  Records <- MSE@Misc$TACAttainment
  if (!length(Records)) return(empty)

  nSim  <- nSim(MSE)
  CxNames <- names(MSE@OM@Complexes)
  Multi <- length(CxNames) > 1
  ToDF <- \(x, name) {
    df <- Array2DF(ExtendSims(x, nSim))
    names(df)[names(df) == 'Value'] <- name
    df
  }
  Match <- \(df, x, name) {
    if (is.null(x)) return(rep(NA, nrow(df)))
    y <- ToDF(x, name)
    y[[name]][match(.TACAttainKey(df), .TACAttainKey(y))]
  }

  out <- purrr::imap(Records, \(Rec, mp) {
    if (is.null(Rec) || !length(Rec$TAC)) return(NULL)
    MaxF <- MSE@Misc$MaxF[[mp]]

    purrr::imap(Rec$TAC, \(TAC, cx) {
      df <- ToDF(TAC, 'TAC')
      df$Catch      <- Match(df, .TACRecordCatch(Rec, cx), 'Catch')
      df$AtCeiling  <- Match(df, Rec$AtCeiling, 'AtCeiling')
      df$Advised    <- Match(df, Rec$Advised[[cx]], 'Advised')
      df$Unresolved <- Match(df, Rec$Unresolved[[cx]], 'Unresolved')
      df$Binding    <- Match(df, Rec$Binding, 'Binding')
      df$MaxF       <- Match(df, MaxF, 'MaxF')
      df <- df[is.finite(df$TAC), , drop = FALSE]
      df$Stock <- cx
      df$MP    <- mp
      df
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  if (!nrow(out)) return(empty)

  Short <- is.finite(out$Catch) & out$TAC - out$Catch > .TACShortfallTol(out$TAC)
  out$Limit <- .TACLimit(Short,
                         AtCeiling  = !is.na(out$AtCeiling) & out$AtCeiling,
                         MaxFHit    = .MaxFHit(out$MaxF),
                         Unresolved = out$Unresolved,
                         Multi      = Multi)
  out$Limit[is.na(out$Catch)] <- NA_character_
  out$Attained <- ifelse(out$TAC > 0, out$Catch / out$TAC, NA_real_)
  out$Fleet   <- as.character(out$Fleet)
  out$Advised <- as.numeric(out$Advised)
  out$Binding <- CxNames[as.integer(out$Binding)]

  out[, names(empty)] |>
    dplyr::arrange(.data$Sim, .data$Stock, .data$MP, .data$Year, .data$Fleet)
}

# Catch after the time step's dynamics where recorded, otherwise the solver's catch
.TACRecordCatch <- function(Rec, cx) {
  Catch    <- Rec$Catch[[cx]]
  Realised <- Rec$Realised[[cx]]
  if (is.null(Realised)) return(Catch)
  ok <- is.finite(Realised)
  Catch[ok] <- Realised[ok]
  Catch
}

# Why a TAC was not caught; a multi-complex shortfall from a resolved solve is a choke
.TACLimit <- function(Short, AtCeiling, MaxFHit, Unresolved, Multi) {
  Unresolved <- !is.na(Unresolved) & as.logical(Unresolved)
  out <- if (Multi) ifelse(Unresolved, 'Solver', 'Choke') else rep('Solver', length(Short))
  out[MaxFHit]   <- 'MaxF'
  out[AtCeiling] <- 'Ceiling'
  out[!Short]    <- 'None'
  out
}

.TACAttainKey <- function(df) paste(df$Sim, df$Year, df$Fleet, sep = '_')

# Catch short of the TAC by more than the effort solver's tolerance
.TACShortfallTol <- function(TAC) pmax(1e-3, 1e-4 * abs(TAC))
