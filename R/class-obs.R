methods::setClassUnion(name="compobs.CompObs", members=c('compobs', 'CompObs'))

#' The `obs` S4 Class
#'
#' Defines the observation error structure applied to each data type in the
#' operating model. Each slot contains a sub-object specifying bias, CV, and
#' sampling structure for one category of observed data. Objects are typically
#' created via the [Obs()] constructor, which documents all parameters in
#' detail.
#'
#' @slot Name `character` or `NULL`. Unique identifier for this observation
#'   model. See [Obs()].
#' @slot LifeHistory A [lifehistoryobs-class] object. Observation error on
#'   life-history parameters (growth, maturity, natural mortality, etc.).
#'   See [LifeHistoryObs()].
#' @slot Exploitation An [exploitationobs-class] object. Observation error on
#'   exploitation processes (selectivity, retention, discard mortality).
#'   See [ExploitationObs()].
#' @slot Effort An [effortobs-class] object. Observation error on fishing
#'   effort. See [EffortObs()].
#' @slot Landings A [catchobs-class] object. Observation error on landed
#'   catch. See [CatchObs()].
#' @slot Discards A [catchobs-class] object. Observation error on discarded
#'   catch. See [CatchObs()].
#' @slot CPUE An [indicesobs-class] object. Observation error on
#'   catch-per-unit-effort indices. See [IndicesObs()].
#' @slot Survey An [indicesobs-class] object. Observation error on
#'   fishery-independent survey indices. See [IndicesObs()].
#' @slot LandingsAtAge A [compobs-class] object. Observation error on
#'   landed catch-at-age composition. See [CompObs()].
#' @slot DiscardsAtAge A [compobs-class] object. Observation error on
#'   discarded catch-at-age composition. See [CompObs()].
#' @slot LandingsAtSize A [compobs-class] object. Observation error on
#'   landed catch-at-length composition. See [CompObs()].
#' @slot DiscardsAtSize A [compobs-class] object. Observation error on
#'   discarded catch-at-length composition. See [CompObs()].
#' @slot Misc `list`. Miscellaneous additional objects.
#'
#' @details
#' An `obs` object defines how each data type is observed, i.e., the error
#' structure (CV, bias, selectivity).  
#' Observed values are held in [data-class] objects stored in `OM@Data`, `Hist@Data`,
#' and `MSE@PPD`.
#'
#' `OM@Obs` is a two-level named list indexed first by stock complex, then by
#' fleet or index name:
#' ```
#' OM@Obs[[stock_complex]][[fleet_name]]  →  obs-class object
#' ```
#' Stock complexes aggregate stocks whose data are reported together (e.g.,
#' combined landings across species). This indexing mirrors `OM@Data`.
#'
#' The slots `Effort`, `Landings`, `Discards`, `CPUE`, `Survey`,
#' `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, and `DiscardsAtSize`
#' are populated with empty sub-objects by [Obs()] when not supplied. The
#' `LifeHistory` and `Exploitation` slots are reserved for future use and are
#' not currently populated during model runs; see [LifeHistoryObs()] and
#' [ExploitationObs()].
#'
#' Direct construction via [methods::new()] is not recommended; use [Obs()]
#' instead, which initialises all sub-objects automatically.
#'
#' @seealso
#' - [Obs()] for the constructor and accessor.
#' - [CatchObs()], [EffortObs()], [IndicesObs()], [CompObs()] for
#'   sub-object constructors.
#' - [LifeHistoryObs()], [ExploitationObs()] for placeholder sub-objects.
#' - [data-class] and [Data()] for the complementary observed-values object.
#' - [OM()] for the operating model constructor.
#' - [ConvertObs()] for converting legacy [Obs-legacy-class] objects.
#'
#' @family obs
#'
#' @include class-unions.R
#' @include class-obs-lifehistory.R
#' @include class-obs-exploitation.R
#' @include class-obs-effort.R
#' @include class-obs-catch.R
#' @include class-obs-indices.R
#' @include class-obs-comp.R
#' @name obs-class
#' @rdname obs-class
setClass(
  "obs",
  slots = c(
    Name           = "char.null",
    LifeHistory    = "lifehistoryobs",
    Exploitation   = "exploitationobs",
    Effort         = "effortobs",
    Landings       = "catchobs",
    Discards       = "catchobs",
    CPUE           = "indicesobs",
    Survey         = "indicesobs",
    LandingsAtAge  = "compobs.CompObs",
    DiscardsAtAge  = "compobs.CompObs",
    LandingsAtSize = "compobs.CompObs",
    DiscardsAtSize = "compobs.CompObs",
    Misc           = "list"
  )
)

setValidity("obs", function(object) {
  # TODO
  TRUE
})



