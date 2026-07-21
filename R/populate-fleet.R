#' Populate a Fleet Object
#'
#' Populate a `Fleet` object by generating effort, catchability, selectivity,
#' retention, discard mortality, and spatial closures for a fleet.
#'
#' @param Fleet A [Fleet()] object to populate.
#' @param Stock A populated [Stock()] object.
#' @param seed Integer. Random seed used for stochastic generation.
#' @param silent Logical. If `TRUE`, suppress informational messages.
#' @param force Logical. If `TRUE`, force re-population even if digest indicates
#' the object is current.
#'
#' @details
#' `PopulateFleet()` performs the following steps:
#'
#' * Populates fleet effort for historical years.
#' * Populates fleet catchability for historical and projected years.
#' * Populates selectivity and retention objects.
#' * Populates discard mortality and spatial closures.
#' * Initializes `WeightFleetRetained` and `WeightFleetSelected` if not
#'   already defined, by weighting the stock's weight-at-length by
#'   selectivity-at-length (`WeightFleetSelected`) or by
#'   selectivity-at-length times retention-at-length (`WeightFleetRetained`)
#'   over the age-length distribution -- see `.CalcFleetWeightAtAge()`.
#'
#' @return
#' A populated [Fleet()] object.
#' 
#' @seealso [Populate()], [PopulateStock()]
#'
#' @examples
#' \dontrun{
#' F <- Fleet()
#' F_pop <- PopulateFleet(Fleet = F, Stock = Stock, seed = 123, nSim = 50)
#' }
#'
#' @export
PopulateFleet <- function(Fleet,
                          Stock,
                          seed = 103,
                          silent = FALSE,
                          force = FALSE) {
  Ages <- Stock@Ages
  Length <- Stock@Length
  Weight <- Stock@Weight
  Maturity <- Stock@Maturity
  RelativeSize <- Stock@Spatial@RelativeSize
  
  Fleet@CurrentYear <- Stock@CurrentYear
  Fleet@nSim <- Stock@nSim
  Fleet@Years <- Stock@Years
  Fleet@nYear <- Stock@nYear
  Fleet@pYear <- Stock@pYear
  Fleet@Seasons <- Stock@Seasons
  
  Fleet@Years <- CalcYears(
    nYear = Stock@nYear,
    pYear = Stock@pYear,
    CurrentYear = Stock@CurrentYear,
    Seasons = Stock@Seasons
  )
  
  nSim <- Fleet@nSim
  Years <- Fleet@Years
  HistYears <- Years(Fleet, "Historical")
  ProjYears <- Years[!Years %in% HistYears]
  nArea <- ncol(RelativeSize)
  
  argList <- list(Ages, Length, Weight, RelativeSize, nSim, Years, seed)
  
  if (EmptyObject(Fleet)) return(Fleet)
  
  if (.CheckDigest(Fleet, argList) & !force) return(Fleet)
  
  .SetSeed(seed)
  
  Fleet@Effort <- PopulateEffort(
    Effort = Fleet@Effort,
    HistYears = HistYears,
    ProjYears = ProjYears,
    nArea = nArea,
    nSim = nSim,
    seed = seed
  )
  
  Fleet@Catchability <- PopulateCatchability(
    Catchability = Fleet@Catchability,
    nSim = nSim,
    HistYears = HistYears,
    ProjYears = ProjYears,
    seed = seed,
    silent = silent
  )
  
  Fleet@Selectivity <- PopulateSelectivity(
    Selectivity = Fleet@Selectivity,
    Ages = Ages,
    Length = Length,
    Weight = Weight,
    Maturity = Maturity,
    nSim = nSim,
    Years = Years,
    nArea = nArea,
    CalcAtLength = TRUE,
    seed = seed,
    silent = silent
  )
  
  Fleet@Retention <- PopulateRetention(
    Retention = Fleet@Retention,
    Ages = Ages,
    Length = Length,
    Weight = Weight,
    Maturity = Maturity,
    Selectivity = Fleet@Selectivity,
    nSim = nSim,
    Years = Years,
    nArea = nArea,
    CalcAtLength = TRUE,
    seed = seed,
    silent = silent,
    force = force
  )
  
  Fleet@DiscardMortality <- PopulateDiscardMortality(
    DiscardMortality = Fleet@DiscardMortality,
    Ages = Ages,
    Length = Length,
    nSim = nSim,
    Years = Years,
    nArea = nArea,
    CalcAtLength = TRUE,
    seed = seed,
    silent = silent
  )
  
  Fleet@Closure <- PopulateClosure(
    Closure = Fleet@Closure,
    nArea = nArea,
    nSim = nSim,
    Years = Years,
    silent = silent
  )
  
  if (all(is.na(Fleet@WeightFleetSelected))) {
    Fleet@WeightFleetSelected <- .CalcFleetWeightAtAge(
      Selectivity = Fleet@Selectivity,
      Weight = Weight,
      Length = Length
    )
  }

  if (all(is.na(Fleet@WeightFleetRetained))) {
    Fleet@WeightFleetRetained <- .CalcFleetWeightAtAge(
      Selectivity = Fleet@Selectivity,
      Weight = Weight,
      Length = Length,
      Retention = Fleet@Retention
    )
  }

  Fleet@WeightFleetSelected <- .SetAgeDimnames(Fleet@WeightFleetSelected, Ages)
  Fleet@WeightFleetRetained <- .SetAgeDimnames(Fleet@WeightFleetRetained, Ages)


  .SetDigest(Fleet, argList)
}

#' Calculate Fleet-Specific Weight-at-Age from Selectivity- (and Retention-)
#' at-Length
#'
#' Derives a fleet's weight-at-age schedule by weighting weight-at-length by
#' selectivity-at-length (and, if `Retention` is supplied, retention-at-length
#' too) over the age-length distribution, i.e. `W_fleet(a) = sum_L[ P(L|a)
#' sel(L) ret(L) W(L) ] / sum_L[ P(L|a) sel(L) ret(L) ]` (`ret(L) = 1`
#' everywhere if `Retention` is not supplied). This mirrors how Stock
#' Synthesis derives its internal `SelWt`/`RetWt` weight-at-age-by-fleet
#' quantities, so that catch biomass matches between the two models -- see
#' `PopulateFleet()` for how the two calls (with and without `Retention`)
#' populate `WeightFleetSelected` and `WeightFleetRetained` respectively.
#' Falls back to `Weight@MeanAtAge` wherever the selectivity- or
#' weight-at-length schedules, or the age-length key, are unavailable.
#'
#' @param Selectivity A populated [selectivity-class] object.
#' @param Weight A populated [weight-class] object.
#' @param Length A populated [length-class] object supplying the age-length
#'   key (`Length@ALK`).
#' @param Retention A populated [retention-class] object, or `NULL` (default)
#'   to weight by selectivity only.
#'
#' @return `array`. `Sim x Age x Year` fleet weight-at-age.
#' @keywords internal
.CalcFleetWeightAtAge <- function(Selectivity, Weight, Length, Retention = NULL) {
  fallback <- Weight@MeanAtAge

  if (is.null(Length@ALK)) return(fallback)

  if (is.null(Weight@MeanAtLength)) {
    # Prefer evaluating the weight-length model directly at each length
    # class; only fall back to the (lossy, round-tripped) ALK-based
    # back-projection from MeanAtAge if the model has no Length argument.
    Years <- as.numeric(dimnames(Weight@MeanAtAge)$Year)
    Weight <- .PopulateMeanAtLength(Weight, Length = Length, Years = Years)
  }
  if (is.null(Weight@MeanAtLength))
    Weight <- .MeanAtAge2MeanAtLength(Weight, Length)

  sel_at_len <- Selectivity@MeanAtLength
  wt_at_len  <- Weight@MeanAtLength

  if (is.null(sel_at_len) || is.null(wt_at_len)) return(fallback)

  if ('Area' %in% names(dimnames(sel_at_len)))
    sel_at_len <- DropDimension(sel_at_len, 'Area', warn = FALSE)

  Weighting <- Selectivity
  Weighting@MeanAtLength <- sel_at_len

  if (!is.null(Retention)) {
    ret_at_len <- Retention@MeanAtLength
    if (!is.null(ret_at_len)) {
      if ('Area' %in% names(dimnames(ret_at_len)))
        ret_at_len <- DropDimension(ret_at_len, 'Area', warn = FALSE)
      Weighting@MeanAtLength <- ArrayMultiply(Weighting@MeanAtLength, ret_at_len)
    }
  }

  wt_fleet <- .WeightedAtSize2AtAge(Weight, Weighting, Length)@MeanAtAge
  bad <- !is.finite(wt_fleet)
  wt_fleet[bad] <- fallback[bad]
  wt_fleet
}
