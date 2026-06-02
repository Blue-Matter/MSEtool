#' LifeHistoryObs Constructor
#'
#' Construct a [lifehistoryobs-class] object defining observation error for
#' life-history parameters.
#'
#' @param Ages `list`. Observation error specification for age structure
#'   parameters. Default `list()`.
#' @param Length `list`. Observation error specification for growth parameters.
#'   Default `list()`.
#' @param Weight `list`. Observation error specification for weight-at-age
#'   or length-weight parameters. Default `list()`.
#' @param NaturalMortality `list`. Observation error specification for natural
#'   mortality. Default `list()`.
#' @param Maturity `list`. Observation error specification for maturity
#'   schedules. Default `list()`.
#' @param Fecundity `list`. Observation error specification for fecundity
#'   parameters. Default `list()`.
#' @param SRR `list`. Observation error specification for stock-recruitment
#'   parameters. Default `list()`.
#' @param Spatial `list`. Observation error specification for spatial
#'   parameters. Default `list()`.
#' @param Depletion `list`. Observation error specification for initial
#'   depletion. Default `list()`.
#' @param Misc `list`. Miscellaneous additional objects. Default `list()`.
#'
#' @details
#' **Placeholder.** This class and constructor are reserved for future
#' development and are not currently used. Each slot will
#' eventually mirror the structure of the corresponding slot in
#' [lifehistorydata-class], providing bias and CV specifications for
#' life-history parameter estimation. Slots currently accept untyped lists and
#' all values are silently ignored during simulation.
#'
#' When converted from a legacy [Obs-legacy-class] object via [ConvertObs()],
#' the life-history bias CVs (`Linfbiascv`, `Kbiascv`, `Mbiascv`, etc.) are
#' not yet mapped to this class and are silently dropped.
#'
#' @return A [lifehistoryobs-class] object.
#'
#' @seealso
#' - [lifehistoryobs-class] for the class definition.
#' - [Obs()] for the enclosing observation model constructor.
#' - [lifehistorydata-class] for the complementary observed-values class.
#'
#' @family obs
#'
#' @examples
#' lho <- LifeHistoryObs()
#'
#' @include class-unions.R
#' @name LifeHistoryObs
#' @export
LifeHistoryObs <- function(Ages             = list(),
                           Length           = list(),
                           Weight           = list(),
                           NaturalMortality = list(),
                           Maturity         = list(),
                           Fecundity        = list(),
                           SRR              = list(),
                           Spatial          = list(),
                           Depletion        = list(),
                           Misc             = list()) {
  methods::new(
    "lifehistoryobs",
    Ages             = Ages,
    Length           = Length,
    Weight           = Weight,
    NaturalMortality = NaturalMortality,
    Maturity         = Maturity,
    Fecundity        = Fecundity,
    SRR              = SRR,
    Spatial          = Spatial,
    Depletion        = Depletion,
    Misc             = Misc
  )
}