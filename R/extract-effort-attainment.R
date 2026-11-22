#' Extract Effort Attainment from an MSE Object
#'
#' Reports, for every simulation, time step, and fleet whose effort was set by
#' effort advice from the MP in the projection, the effort the MP advised and
#' the effort the fleet fished.
#'
#' Advised effort is fished in full unless a limit applies. The fleet's
#' maximum effort (`Effort@Maximum`, see [Effort()]) caps it; a bag limit with
#' `ClosureMode = "stop"` shortens the fishing time of each trip (see
#' [BagLimitEffect()]); and the cap on fishing mortality (`OM@maxF`) scales
#' down the fishing of all fleets in an area where it is exceeded (see
#' [MaxFEffect()]). A fleet with both effort and TAC advice fishes to its TAC,
#' with the effort advice as a ceiling, and is reported by [TACAttainment()].
#'
#' Relative effort advice (`EffType = "Rel"`) is reported as absolute effort.
#' In seasonal operating models, annual effort advice is reported as each time
#' step's share.
#'
#' @param MSE An [mse-class] object.
#'
#' @return A data frame with one row per simulation, MP, time step, and fleet
#'   with effort advice:
#'   - `Sim`: simulation index.
#'   - `MP`: management procedure name.
#'   - `Year`: calendar year (or decimal date for sub-annual time steps).
#'   - `Fleet`: fleet name.
#'   - `Advised`: the effort advised by the MP. Where several stock complexes
#'     advise effort for a fleet, the advice of the complex that sets its
#'     effort (see `Imp@Effort@Compliance`).
#'   - `Implemented`: the advised effort after implementation error
#'     (`Imp@Effort@Error`).
#'   - `Effort`: the effort the fleet fished (`Effort(MSE)`).
#'   - `Fished`: `Effort / Implemented`.
#'   - `Limit`: `"None"` where the implemented effort was fished in full;
#'     otherwise the first limit that reduced it: `"Maximum"`, `"BagLimit"`,
#'     or `"MaxF"`.
#'
#' @seealso [TACAttainment()], [MaxFEffect()], [BagLimitEffect()]
#' @export
EffortAttainment <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  empty <- data.frame(Sim = numeric(0), MP = character(0), Year = numeric(0),
                      Fleet = character(0), Advised = numeric(0), Implemented = numeric(0),
                      Effort = numeric(0), Fished = numeric(0), Limit = character(0))

  Records <- MSE@Misc$EffortAttainment
  if (!length(Records)) return(empty)
  nSim <- nSim(MSE)

  out <- purrr::imap(Records, \(Rec, mp) {
    if (is.null(Rec) || !mp %in% dimnames(MSE@Effort)$MP) return(NULL)
    Controlled <- ExtendSims(Rec$Controlled, nSim)
    if (!any(Controlled, na.rm = TRUE)) return(NULL)
    YearsProj <- dimnames(Rec$Advised)$Year
    Effort <- MSE@Effort[, match(YearsProj, dimnames(MSE@Effort)$Year), , mp, drop = FALSE] |>
      abind::adrop(4) |>
      ExtendSims(nSim)
    Bag  <- MSE@Misc$BagLimit[[mp]]$TimeFraction
    MaxF <- MSE@Misc$MaxF[[mp]]
    Implemented <- ExtendSims(Rec$Implemented, nSim)
    Limit <- .EffortLimit(Implemented,
                          ExtendSims(Rec$Capped, nSim),
                          Effort,
                          if (is.null(Bag)) NA else ExtendSims(Bag, nSim),
                          if (is.null(MaxF)) FALSE else .MaxFHit(ExtendSims(MaxF, nSim)))
    Limit <- array(Limit, dim(Implemented), dimnames = dimnames(Implemented))
    Fished <- Effort / Implemented
    df <- Array2DF(Controlled)
    df <- df[!is.na(df$Value) & df$Value, , drop = FALSE]
    if (!nrow(df)) return(NULL)
    Col <- \(x) {
      y <- Array2DF(x)
      y$Value[match(.TACAttainKey(df), .TACAttainKey(y))]
    }
    data.frame(Sim = as.numeric(df$Sim), MP = mp, Year = as.numeric(df$Year),
               Fleet = as.character(df$Fleet),
               Advised = Col(ExtendSims(Rec$Advised, nSim)),
               Implemented = Col(Implemented), Effort = Col(Effort),
               Fished = Col(Fished), Limit = Col(Limit))
  }) |> dplyr::bind_rows()

  if (!nrow(out)) return(empty)
  rownames(out) <- NULL
  out[, names(empty)] |>
    dplyr::arrange(.data$Sim, .data$MP, .data$Year, .data$Fleet)
}
