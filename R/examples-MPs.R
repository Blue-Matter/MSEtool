#' Example Management Procedures
#'
#' Simple management procedures (MPs) designed for testing and demonstration.
#' Each MP function accepts a [data-class] object and returns an [advice-class]
#' object. MPs are assigned `class "mp"` so they are recognised by the MSE
#' framework.
#'
#' - `CurrentEffort`: Fixes fishing effort at 1 (last historical effort) for
#'   all fleets, maintaining the spatial distribution of effort from the last
#'   historical timestep. The `Data` argument is accepted for framework
#'   compatibility but is not used.
#' - `CurrentCatch`: Sets the TAC for each fleet to the removals observed in
#'   the last historical year. Returns an empty [advice-class] object if no
#'   landings data are available.
#' - `CurrentLandings`: Sets the TAC for each fleet to the landings observed in
#'   the last historical year. Returns an empty [advice-class] object if no
#'   landings data are available.
#' - `AverageCatch`: Sets the TAC to the mean total removals (summed over
#'   fleets) across all historical years. Returns an empty [advice-class]
#'   object if no landings data are available.
#' - `ExampleMPs`: Returns a character vector naming all example MPs defined
#'   in this group, for use in batch testing or MP selection.
#'
#' @param Data A [data-class] object containing historical observations.
#'
#' @return
#' * `CurrentEffort`, `CurrentCatch`, `AverageCatch` — an [advice-class]
#'   object; see [Advice()] for constructor details.
#' * `ExampleMPs` — a character vector of the example MP names.
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

#' @rdname ExampleMPs
#' @export
CurrentCatch <- function(Data) {
  CheckCatch(Data) 
  
  LastHistLandings <- Data@Landings@Value[LastHistYearInd(Data), , drop=FALSE]
  
  if (is.null(LastHistLandings) || !length(LastHistLandings))
    return(Advice())
  
  LastHistDiscards <- Data@Discards@Value[LastHistYearInd(Data), , drop=FALSE]
  LastHistRemovals <- dplyr::bind_rows(
    as.data.frame(LastHistLandings), 
    as.data.frame(LastHistDiscards)
  )
  LastHistRemovals <- colSums(LastHistRemovals, na.rm=TRUE)
    
  Advice(TAC=LastHistRemovals)
}
class(CurrentCatch) <- 'mp'

#' @rdname ExampleMPs
#' @export
CurrentLandings <- function(Data) {
  CheckCatch(Data, slot_names = 'Landings') 
  
  LastHistLandings <- Data@Landings@Value[LastHistYearInd(Data), , drop=FALSE]
  
  if (is.null(LastHistLandings) || !length(LastHistLandings))
    return(Advice())
  
  Advice(TAC=colSums(LastHistLandings, na.rm=TRUE),
         TACType = 'Landings')
}
class(CurrentLandings) <- 'mp'

#' @rdname ExampleMPs
#' @export
AverageCatch <- function(Data) {
  CheckCatch(Data) 
  
  HistLandings <- Data@Landings@Value[1:LastHistYearInd(Data), , drop=FALSE]
  if (is.null(HistLandings) || !length(HistLandings))
    return(Advice())
  
  HistDiscards <- Data@Discards@Value[1:LastHistYearInd(Data), , drop=FALSE]
  
  TotalHistLandings <- rowSums(HistLandings, na.rm=TRUE)
  TotalHistDiscards <- rowSums(HistDiscards, na.rm=TRUE)
  TotalHistRemovals <- TotalHistLandings + TotalHistDiscards
  Advice(TAC=mean(TotalHistRemovals[TotalHistRemovals!=0]))
}
class(AverageCatch) <- 'mp'

#' @rdname ExampleMPs
#' @export
ExampleMPs <- function() {
  c('CurrentEffort', 'CurrentCatch', 'AverageCatch')
}
