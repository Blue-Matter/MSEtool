#' Example Management Procedures
#'
#' Simple management procedures (MPs) designed for testing and demonstration.
#' Each function accepts a [data-class] object and returns an [advice-class]
#' object. MPs are assigned `class "mp"` so they are recognised by the MSE
#' framework.
#'
#' - `StatusQuo`: Fixes fishing effort at 1 (i.e. last historical effort) for 
#' all fleets, and maintains the spatial distribution of fishing effort from the 
#' last historical timestep.
#' - `CurrentEffort`: Alias for `StatusQuo`.
#' - `CurrentCatch`: Sets the TAC for each fleet to the landings observed in
#'   the last historical year.
#'
#' @param Data A [data-class] object containing historical observations.
#'
#' @return An [advice-class] object. See [Advice()] for details.
#'
#' @seealso [Advice()], [data-class], [advice-class]
#' @name ExampleMPs
NULL


#' @rdname ExampleMPs
#' @export
StatusQuo <- function(Data) {
  Advice()
}
class(StatusQuo) <- 'mp'


#' @rdname ExampleMPs
#' @export
CurrentEffort <- function(Data) {
  StatusQuo(Data)
}
class(CurrentEffort) <- 'mp'

#' @rdname ExampleMPs
#' @export
CurrentCatch <- function(Data) {
  LastHistLandings <- Data@Landings@Value[LastHistYearInd(Data), ]
  Advice(TAC=LastHistLandings)
}
class(CurrentCatch) <- 'mp'


