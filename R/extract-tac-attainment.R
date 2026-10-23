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
#'   - `Catch`: the fleet's catch of the complex, in the same units and catch
#'     type.
#'   - `Attained`: `Catch / TAC`; `NA` where the TAC is zero.
#'   - `Limit`: `"None"` where the TAC was caught; `"Ceiling"` where the fleet
#'     was at its effort ceiling; `"Choke"` where the fleet stopped because
#'     the TAC of another complex was reached first (multi-complex operating
#'     models, see `Imp@TAC@Compliance`); `"Solver"` where, in a
#'     single-complex model, the fleet was below its ceiling because the
#'     effort solver did not converge (also reported by
#'     [SummarizeConvergence()]).
#'
#' @seealso [TACs()], [PM_TACAttained()], [SummarizeConvergence()]
#' @export
TACAttainment <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  empty <- data.frame(Sim = numeric(0), Stock = character(0), MP = character(0),
                      Year = numeric(0), Fleet = character(0), TAC = numeric(0),
                      Catch = numeric(0), Attained = numeric(0), Limit = character(0))

  Records <- MSE@Misc$TACAttainment
  if (!length(Records)) return(empty)

  nSim  <- nSim(MSE)
  Multi <- length(MSE@OM@Complexes) > 1

  out <- purrr::imap(Records, \(Rec, mp) {
    if (is.null(Rec) || !length(Rec$TAC)) return(NULL)
    AtCeiling <- Array2DF(ExtendSims(Rec$AtCeiling, nSim))
    names(AtCeiling)[names(AtCeiling) == 'Value'] <- 'AtCeiling'

    purrr::imap(Rec$TAC, \(TAC, cx) {
      df <- Array2DF(ExtendSims(TAC, nSim))
      names(df)[names(df) == 'Value'] <- 'TAC'
      Catch <- Array2DF(ExtendSims(Rec$Catch[[cx]], nSim))
      df$Catch <- Catch$Value[match(.TACAttainKey(df), .TACAttainKey(Catch))]
      df$AtCeiling <- AtCeiling$AtCeiling[match(.TACAttainKey(df), .TACAttainKey(AtCeiling))]
      df <- df[is.finite(df$TAC), , drop = FALSE]
      df$Stock <- cx
      df$MP    <- mp
      df
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  if (!nrow(out)) return(empty)

  Short <- is.finite(out$Catch) & out$TAC - out$Catch > .TACShortfallTol(out$TAC)
  AtCeil <- !is.na(out$AtCeiling) & out$AtCeiling
  out$Limit <- ifelse(!Short, 'None',
                      ifelse(AtCeil, 'Ceiling', if (Multi) 'Choke' else 'Solver'))
  out$Limit[is.na(out$Catch)] <- NA_character_
  out$Attained <- ifelse(out$TAC > 0, out$Catch / out$TAC, NA_real_)
  out$Fleet <- as.character(out$Fleet)

  out[, names(empty)] |>
    dplyr::arrange(.data$Sim, .data$Stock, .data$MP, .data$Year, .data$Fleet)
}

.TACAttainKey <- function(df) paste(df$Sim, df$Year, df$Fleet, sep = '_')

# Catch short of the TAC by more than the effort solver's tolerance
.TACShortfallTol <- function(TAC) pmax(1e-3, 1e-4 * abs(TAC))
