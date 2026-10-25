#' Example Management Procedures
#'
#' Simple management procedures (MPs) designed for testing and demonstration.
#' Each MP function accepts a [data-class] object and returns an [advice-class]
#' object. MPs are assigned `class "mp"` so they are recognised by the MSE
#' framework.
#'
#' - `CurrentEffort`: Keeps each fleet's fishing effort at its level in the
#'   last historical year (relative effort 1). Each time step, the effort is
#'   allocated across areas by the OM's spatial targeting (`Mode` and
#'   `Targeting` in [Effort()]), as for any effort advice without an area
#'   split. Fleets with a
#'   historical spatial distribution specified in the OM (`Distribution` in
#'   [Effort()]) keep their distribution from the last historical year. In a
#'   seasonal model, each season repeats that season's effort from the last
#'   historical year - see *Seasonal models* below. The `Data` argument is
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
#' each carries an `EverySeason` attribute of `TRUE`, forcing the framework
#' to call them at every timestep of the operating model regardless
#' of `Interval`. This is required so that, in a seasonal model (`Seasons > 1`),
#' each call can return the value for the specific season it is advising,
#' rather than the same figure being reapplied to every season of the year.
#'
#' `CurrentEffort` returns an empty [Advice()]. Any MP that returns neither
#' `TAC` nor `Effort` keeps status-quo effort: each fleet's effort in each
#' season equals its effort in the same season of the last historical year,
#' so this year's Q1 repeats last year's Q1. `CurrentEffort` also carries the
#' `EverySeason` attribute, although its advice is the same whichever
#' timestep it is called in.
#'
#' ## Data lag
#'
#' `CurrentCatch`, `CurrentLandings`, and `AverageCatch` carry a `DataLag`
#' attribute of `0`, so they always see the complete historical period
#' regardless of the OM's [DataLag()], and the same baseline catch applies
#' from the first projection year onward. `CurrentEffort` uses the last
#' historical effort directly and is likewise unaffected by `DataLag`.
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
attr(CurrentEffort, 'EverySeason') <- TRUE

#' @rdname ExampleMPs
#' @export
CurrentCatch <- function(Data) {
  CheckCatch(Data)

  si <- SeasonOfYear(Data)
  Season <- si$SeasonInd %% si$Seasons + 1L # managed season follows the last data time step
  TargetRow <- LastHistYearInd(Data) - si$Seasons + Season

  LastHistLandings <- Data@Landings@Value[TargetRow, , drop=FALSE]

  if (is.null(LastHistLandings) || !length(LastHistLandings))
    return(Advice())

  LastHistDiscards <- Data@Discards@Value[TargetRow, , drop=FALSE] %||% 0

  LastHistRemovals <- as.vector(replace(LastHistLandings, is.na(LastHistLandings), 0) +
                                  replace(LastHistDiscards,  is.na(LastHistDiscards),  0))
  Advice(TAC = LastHistRemovals, TACUnit = Data@Landings@Units)
}
class(CurrentCatch) <- 'mp'
attr(CurrentCatch, 'EverySeason') <- TRUE
attr(CurrentCatch, 'DataLag') <- 0

#' @rdname ExampleMPs
#' @export
CurrentLandings <- function(Data) {
  CheckCatch(Data, slot_names = 'Landings')

  si <- SeasonOfYear(Data)
  Season <- si$SeasonInd %% si$Seasons + 1L # managed season follows the last data time step
  TargetRow <- LastHistYearInd(Data) - si$Seasons + Season

  LastHistLandings <- Data@Landings@Value[TargetRow, , drop=FALSE]

  if (is.null(LastHistLandings) || !length(LastHistLandings))
    return(Advice())

  Advice(TAC=colSums(LastHistLandings, na.rm=TRUE),
         TACType = 'Landings',
         TACUnit = Data@Landings@Units)
}
class(CurrentLandings) <- 'mp'
attr(CurrentLandings, 'EverySeason') <- TRUE
attr(CurrentLandings, 'DataLag') <- 0

#' @rdname ExampleMPs
#' @export
AverageCatch <- function(Data) {
  CheckCatch(Data)

  si <- SeasonOfYear(Data)
  Season <- si$SeasonInd %% si$Seasons + 1L # managed season follows the last data time step
  HistRows <- seq(Season, LastHistYearInd(Data), by = si$Seasons)

  HistLandings <- Data@Landings@Value[HistRows, , drop=FALSE]
  if (is.null(HistLandings) || !length(HistLandings))
    return(Advice())

  HistDiscards <- Data@Discards@Value[HistRows, , drop=FALSE]

  TotalHistLandings <- rowSums(HistLandings, na.rm=TRUE)
  TotalHistDiscards <- if (is.null(HistDiscards)) 0 else rowSums(HistDiscards, na.rm=TRUE)
  TotalHistRemovals <- TotalHistLandings + TotalHistDiscards
  Advice(TAC=mean(TotalHistRemovals[TotalHistRemovals!=0]),
         TACUnit = Data@Landings@Units)
}
class(AverageCatch) <- 'mp'
attr(AverageCatch, 'EverySeason') <- TRUE
attr(AverageCatch, 'DataLag') <- 0

#' @rdname ExampleMPs
#' @export
ExampleMPs <- function() {
  c('CurrentEffort', 'CurrentCatch', 'AverageCatch')
}
