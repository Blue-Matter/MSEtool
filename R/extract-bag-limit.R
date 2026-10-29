#' Extract the Effect of Bag Limits from an MSE Object
#'
#' Reports, for every simulation, time step, fleet, and stock where a bag limit
#' applied in the projection, the number of trips and how the bag limit
#' reduced the catch kept on them.
#'
#' A bag limit keeps a fraction \eqn{\rho = E[\min(X, B)] / \mu} of the catch
#' per trip that would be kept without it (see [Advice()]). With
#' `ClosureMode = "discard"` this fraction is applied to retention
#' (`RetentionFraction`); with `ClosureMode = "stop"` it is applied to the
#' fishing time of each trip (`TimeFraction`), so the fleet's effort in the
#' projection (`Effort(MSE)`) is `TimeFraction` times the effort of its trips.
#' Observed effort data generated in the projection are based on trips.
#'
#' @param MSE An [mse-class] object.
#'
#' @return A data frame with one row per simulation, MP, time step, fleet,
#'   and stock where a bag limit applied to the fleet:
#'   - `Sim`: simulation index.
#'   - `MP`: management procedure name.
#'   - `Year`: calendar year (or decimal date for sub-annual time steps).
#'   - `Fleet`: fleet name.
#'   - `Stock`: stock name.
#'   - `Trips`: the fleet's number of trips.
#'   - `TimeFraction`: fraction of each trip's fishing time used before the
#'     bag is reached (`1` without a stop-mode bag limit).
#'   - `RetentionFraction`: fraction of the stock's retained catch kept, the
#'     rest released (`1` without a discard-mode bag limit on the stock).
#'
#' @seealso [Advice()], [AggregateBagLimit()], [TACAttainment()]
#' @export
BagLimitEffect <- function(MSE) {
  .CheckClass(MSE, 'mse', 'MSE')

  empty <- data.frame(Sim = numeric(0), MP = character(0), Year = numeric(0),
                      Fleet = character(0), Stock = character(0), Trips = numeric(0),
                      TimeFraction = numeric(0), RetentionFraction = numeric(0))

  Records <- MSE@Misc$BagLimit
  if (!length(Records)) return(empty)
  nSim <- nSim(MSE)

  out <- purrr::imap(Records, \(Rec, mp) {
    if (is.null(Rec)) return(NULL)
    df <- Array2DF(ExtendSims(Rec$Trips, nSim))
    names(df)[names(df) == 'Value'] <- 'Trips'
    df <- df[is.finite(df$Trips), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    Key <- \(x) paste(x$Sim, x$Year, x$Fleet, sep = '_')
    TF <- Array2DF(ExtendSims(Rec$TimeFraction, nSim))
    df$TimeFraction <- TF$Value[match(Key(df), Key(TF))]
    purrr::imap(Rec$RetentionFraction, \(RF, st) {
      RF <- Array2DF(ExtendSims(RF, nSim))
      cbind(df, Stock = st, RetentionFraction = RF$Value[match(Key(df), Key(RF))])
    }) |> dplyr::bind_rows() |> cbind(MP = mp)
  }) |> dplyr::bind_rows()

  if (!nrow(out)) return(empty)
  out$Fleet <- as.character(out$Fleet)
  out$Stock <- as.character(out$Stock)

  out[, names(empty)] |>
    dplyr::arrange(.data$Sim, .data$MP, .data$Year, .data$Fleet, .data$Stock)
}
