#' Aggregate Bag Limit Constructor
#'
#' Construct an [aggbaglimit-class] object declaring an aggregate bag limit
#' that pools catch across several independently-modelled stocks for a
#' single fleet - management advice that does not belong to any one
#' stock's [Advice()] object. Used by `mmp`-class management procedures,
#' which return a list of these objects alongside the ordinary per-stock
#' `Advice` list.
#'
#' @param Fleet Character or numeric, length 1. The fleet the aggregate
#'   limit applies to, by name or position.
#' @param Stocks Character or numeric, length 2 or more. The stocks pooled
#'   under this limit, by name or position. A limit on a single stock does
#'   not need `AggregateBagLimit()` - set `BagLimit` directly on that
#'   stock's `Advice` object instead (see [Advice()]).
#' @param BagLimit Numeric, length 1. The aggregate limit, in fish per
#'   trip (per angler if `LimitType = "angler"`, per vessel if `"boat"`).
#' @param LimitType Character. `"boat"` (default) or `"angler"` - see
#'   [Advice()] for the shared definition. Also governs enforcement of any
#'   per-stock `BagLimit` set as a species-specific sub-cap on a stock
#'   included in `Stocks` (that stock's own `LimitType` is ignored).
#' @param ClosureMode Character. `"discard"` (default) or `"stop"` - see
#'   [Advice()] for the shared definition. Also governs enforcement of any
#'   per-stock `BagLimit` sub-cap on a stock included in `Stocks` (that
#'   stock's own `ClosureMode` is ignored).
#' @param Misc Miscellaneous list. Default `list()`.
#'
#' @details
#'
#' `AggregateBagLimit()` declares the one piece of a pooled bag-limit
#' regulation that cannot be expressed on any single stock's `Advice`
#' object: which stocks are pooled, and the ceiling that applies to their
#' combined catch. Everything else reuses the ordinary `Advice` mechanism:
#'
#' - A species-specific sub-cap for one stock within the group is just an
#'   ordinary per-stock `BagLimit`, set on that stock's own `Advice`
#'   object (see [Advice()]). No separate slot or argument is needed for
#'   this.
#' - `Theta`, `TripsScalar`, and `AnglerPerTrip` are read directly from
#'   the fleet's `Effort` object for the *first* stock listed in `Stocks`
#'   (`Fleet(OM)[[Stocks[1]]][[Fleet]]@Effort`) - not set here. This
#'   assumes the fleet's trip behaviour is consistent across the stocks it
#'   is pooled over, since these describe fleet activity rather than any
#'   one species.
#'
#' `mmp`-class management procedures return a list of `AggregateBagLimit()`
#' objects (one per pooled regulation) alongside the ordinary per-stock
#' `Advice` list.
#'
#' @return An [aggbaglimit-class] object.
#'
#' @seealso [aggbaglimit-class] for the class definition. [Advice()] for
#'   the per-stock advice object aggregate bag limits complement.
#'
#' @rdname AggregateBagLimit
#' @export
AggregateBagLimit <- function(Fleet,
                              Stocks,
                              BagLimit,
                              LimitType   = 'boat',
                              ClosureMode = 'discard',
                              Misc        = list()) {

  if (missing(Fleet) || is.null(Fleet) || length(Fleet) != 1)
    cli::cli_abort("`Fleet` must identify a single fleet")

  if (missing(Stocks) || is.null(Stocks) || length(Stocks) < 2)
    cli::cli_abort(paste(
      "`Stocks` must identify two or more stocks to pool - for a single",
      "stock, set `BagLimit` directly on that stock's `Advice()` object instead"
    ))

  if (missing(BagLimit) || !is.numeric(BagLimit) || length(BagLimit) != 1)
    cli::cli_abort("`BagLimit` must be a single numeric value")

  if (!is.na(BagLimit) && BagLimit < 0)
    cli::cli_abort("`BagLimit` must be non-negative")

  LimitType   <- match.arg(LimitType,   c('boat', 'angler'))
  ClosureMode <- match.arg(ClosureMode, c('discard', 'stop'))

  if (!is.list(Misc))
    cli::cli_abort("`Misc` must be a list")

  methods::new(
    "aggbaglimit",
    Fleet       = Fleet,
    Stocks      = Stocks,
    BagLimit    = BagLimit,
    LimitType   = LimitType,
    ClosureMode = ClosureMode,
    Misc        = Misc
  )
}
