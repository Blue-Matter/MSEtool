#' Extract Where the Fishing Mortality Cap Reduced Fishing
#'
#' Reports, for every simulation, time step, and fleet where the cap on fishing
#' mortality (`OM@maxF`) reduced fishing in the projection, the fraction of the
#' fleet's effort that was fished.
#'
#' `OM@maxF` caps the total apical fishing mortality of all fleets within each
#' area. Where the effort of the fleets (from effort advice, status quo effort,
#' or the effort found to catch a TAC) would exceed it, the fishing mortality of
#' every fleet fishing in that area is scaled down so the total equals
#' `OM@maxF`, and the fleet's effort in the projection (`Effort(MSE)`) is the
#' effort that gives the reduced fishing mortality. Catches are reduced
#' accordingly. A warning in the MSE log (`Log(MSE)`) reports how often this
#' happened for each MP.
#'
#' @param MSE An [mse-class] object.
#'
#' @return A data frame with one row per simulation, MP, time step, and fleet
#'   where the cap reduced fishing:
#'   - `Sim`: simulation index.
#'   - `MP`: management procedure name.
#'   - `Year`: calendar year (or decimal date for sub-annual time steps).
#'   - `Fleet`: fleet name.
#'   - `EffortFraction`: the fleet's effort as a fraction of the effort it
#'     would have fished without the cap.
#'
#' @seealso [TACAttainment()], [BagLimitEffect()]
#' @export
MaxFEffect <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  empty <- data.frame(Sim = numeric(0), MP = character(0), Year = numeric(0),
                      Fleet = character(0), EffortFraction = numeric(0))

  Records <- MSE@Misc$MaxF
  if (!length(Records)) return(empty)
  nSim <- nSim(MSE)

  out <- purrr::imap(Records, \(Rec, mp) {
    if (is.null(Rec)) return(NULL)
    df <- Array2DF(ExtendSims(Rec, nSim))
    df <- df[.MaxFHit(df$Value), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    data.frame(Sim = as.numeric(df$Sim), MP = mp, Year = as.numeric(df$Year),
               Fleet = as.character(df$Fleet), EffortFraction = df$Value)
  }) |> dplyr::bind_rows()

  if (!nrow(out)) return(empty)
  rownames(out) <- NULL
  out
}
