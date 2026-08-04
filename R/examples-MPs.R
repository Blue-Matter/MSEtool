#' Example Management Procedures
#'
#' Simple management procedures (MPs) designed for testing and demonstration.
#' Each MP function accepts a [data-class] object and returns an [advice-class]
#' object. MPs are assigned `class "mp"` so they are recognised by the MSE
#' framework.
#'
#' - `CurrentEffort`: Fixes fishing effort at 1 (last historical effort) for
#'   all fleets, maintaining the spatial distribution of effort from the last
#'   historical timestep, and (in a seasonal model) each season's own share
#'   of that pattern - see *Seasonal models* below. The `Data` argument is
#'   accepted for framework compatibility but is not used.
#' - `CurrentCatch`: Sets the TAC for each fleet to the removals observed in
#'   the last historical year, in the same units as `Landings`. Returns
#'   an empty [advice-class] object if no landings data are available.
#' - `CurrentLandings`: Sets the TAC for each fleet to the landings observed in
#'   the last historical year, in the same units as `Landings`. Returns
#'   an empty [advice-class] object if no landings data are available.
#' - `AverageCatch`: Sets the TAC to the mean total removals (summed over
#'   fleets) across all historical years, in the same units as
#'   `Data@Landings`. Returns an empty [advice-class] object if no landings
#'   data are available.
#' - `ExampleMPs`: Returns a character vector naming all example MPs defined
#'   in this group, for use in batch testing or MP selection.
#'
#' ## Seasonal models
#'
#' `CurrentCatch`, `CurrentLandings`, and `AverageCatch` reference a fixed
#' historical baseline that does not depend on when the MP is called, so
#' each carries an `Interval` attribute of `1` (see [Interval()]) forcing the
#' framework to call them at every timestep - required so that, in a
#' seasonal model (`Seasons > 1`), each call can return the value for
#' the specific season it is advising, rather than the same figure being
#' reapplied to every season of the year.
#'
#' `CurrentEffort` always returns an empty [Advice()], which the framework's
#' default fallback (used whenever neither `TAC` nor `Effort` is set)
#' resolves to last year's effort, relative to the last complete historical
#' year - separately season-matched by the framework itself so, e.g., this
#' year's Q1 reuses last year's Q1 pattern rather than the historical
#' period's final season. `CurrentEffort` carries the same `Interval`
#' attribute as the other three for consistency, though - unlike them - its
#' output does not actually depend on how often it is called.
#'
#' @param Data A [data-class] object containing historical observations.
#'
#' @return
#' * `CurrentEffort`, `CurrentCatch`, `CurrentLandings`, `AverageCatch`: an
#'   [advice-class] object; see [Advice()] for constructor details.
#' * `ExampleMPs`: a character vector of the example MP names.
#'
#' @examples
#' \dontrun{
#' MSE <- runMSE(SingleStockOM, MPs=ExampleMPs())
#' }
#'
#'
#'
#' @seealso [Advice()], [CheckCatch()], [data-class], [advice-class]
#' @name ExampleMPs
NULL


#' @rdname ExampleMPs
#' @export
CurrentEffort <- function(Data) {
  Advice()
}
class(CurrentEffort) <- 'mp'
attr(CurrentEffort, 'Interval') <- 1

#' @rdname ExampleMPs
#' @export
CurrentCatch <- function(Data) {
  CheckCatch(Data)

  si <- MPSeasonIndex(Data)
  TargetRow <- LastHistYearInd(Data) - si$Seasons + si$SeasonInd

  LastHistLandings <- Data@Landings@Value[TargetRow, , drop=FALSE]

  if (is.null(LastHistLandings) || !length(LastHistLandings))
    return(Advice())

  LastHistDiscards <- Data@Discards@Value[TargetRow, , drop=FALSE]

  LastHistRemovals <- as.vector(replace(LastHistLandings, is.na(LastHistLandings), 0) +
                                  replace(LastHistDiscards,  is.na(LastHistDiscards),  0))
  Advice(TAC = LastHistRemovals, TACUnit = Data@Landings@Units)
}
class(CurrentCatch) <- 'mp'
attr(CurrentCatch, 'Interval') <- 1

#' @rdname ExampleMPs
#' @export
CurrentLandings <- function(Data) {
  CheckCatch(Data, slot_names = 'Landings')

  si <- MPSeasonIndex(Data)
  TargetRow <- LastHistYearInd(Data) - si$Seasons + si$SeasonInd

  LastHistLandings <- Data@Landings@Value[TargetRow, , drop=FALSE]

  if (is.null(LastHistLandings) || !length(LastHistLandings))
    return(Advice())

  Advice(TAC=colSums(LastHistLandings, na.rm=TRUE),
         TACType = 'Landings',
         TACUnit = Data@Landings@Units)
}
class(CurrentLandings) <- 'mp'
attr(CurrentLandings, 'Interval') <- 1

#' @rdname ExampleMPs
#' @export
AverageCatch <- function(Data) {
  CheckCatch(Data)

  si <- MPSeasonIndex(Data)
  HistRows <- seq(si$SeasonInd, LastHistYearInd(Data), by = si$Seasons)

  HistLandings <- Data@Landings@Value[HistRows, , drop=FALSE]
  if (is.null(HistLandings) || !length(HistLandings))
    return(Advice())

  HistDiscards <- Data@Discards@Value[HistRows, , drop=FALSE]

  TotalHistLandings <- rowSums(HistLandings, na.rm=TRUE)
  TotalHistDiscards <- rowSums(HistDiscards, na.rm=TRUE)
  TotalHistRemovals <- TotalHistLandings + TotalHistDiscards
  Advice(TAC=mean(TotalHistRemovals[TotalHistRemovals!=0]),
         TACUnit = Data@Landings@Units)
}
class(AverageCatch) <- 'mp'
attr(AverageCatch, 'Interval') <- 1

#' @rdname ExampleMPs
#' @export
ExampleMPs <- function() {
  c('CurrentEffort', 'CurrentCatch', 'AverageCatch')
}
