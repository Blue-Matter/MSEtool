#' Generate Projected Index Data for a Stock/Complex
#'
#' Appends a new year of observed index data (`Value` and `CV`) to the
#' `CPUE` or `Survey` data object of every simulation for a given
#' stock/complex.
#'
#' @param Proj A `Hist` object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric. All calendar years in the historical
#'  and projection years
#' @param i Integer. Stock/complex index.
#' @param stocks `integer`. Stock indices used to subset population arrays.
#' @param StockNames Character. Names of all stocks in the model.
#' @param FleetNames Character. Names of all fleets, used to match
#'   fleet-specific selectivity from `Proj@OM@Fleet`.
#' @param nSim Integer. Number of simulation replicates.
#' @param type Character. One of `"CPUE"` or `"Survey"`.
#'
#' @details
#' The existing objects are returned unchanged if the index data of
#' simulation 1 is empty or already contains `DataYear`. For each simulation,
#' a fleet's index is not simulated if its last five values are all
#' non-finite.
#'
#' @return A list of length `nSim` of updated `CPUE` or `Survey` objects.
#' @keywords internal
.GenProjDataIndex <- function(Proj, DataYear, YearsAll, i, stocks, StockNames,
                                 FleetNames, nSim, type = c('CPUE', 'Survey')) {
  type <- match.arg(type)

  IndexData1 <- slot(Proj@Data[[1]][[i]], type)
  unchanged  <- EmptyObject(IndexData1) || DataYear %in% dimnames(IndexData1@Value)[[1]]
  if (unchanged)
    return(purrr::map(Proj@Data, \(DataList) slot(DataList[[i]], type)))

  TSIndex    <- match(DataYear, YearsAll)
  nArea      <- nArea(Proj)
  nFleet     <- ncol(IndexData1@Value)
  FleetIndex <- match(IndexData1@Name, names(Proj@OM@Obs[[i]]))

  if (length(FleetIndex) != nFleet)
    cli::cli_abort(
      "Mismatch in number of fleets in `Obs` and `Data[[x]]@{type}`",
      .internal = TRUE
    )

  IndexData1 <- .ResolveUnits(IndexData1, nFleet, valid = c("Biomass", "Number", "Recruitment"))

  Real_Pop_Number_All <- purrr::map(Proj@Number[stocks], \(stock_n) {
    stock_n[, , TSIndex, seq_len(nArea), drop = FALSE] |> abind::adrop(drop = 3)
  })

  NewValueAll <- matrix(NA_real_, nSim, nFleet)
  NewCVAll    <- matrix(NA_real_, nSim, nFleet)
  omData      <- Proj@OM@Data[[i]]

  for (fl in seq_len(nFleet)) {
    IndexObs <- slot(Proj@OM@Obs[[i]][[FleetIndex[fl]]], type)
    if (EmptyObject(IndexObs)) next
    if (is.null(IndexObs@Areas)) IndexObs@Areas <- seq_len(nArea)

    hasOMVal <- !is.null(omData) &&
      !is.null(slot(omData, type)@Value) &&
      nrow(slot(omData, type)@Value) >= TSIndex &&
      ncol(slot(omData, type)@Value) >= fl

    if (hasOMVal) {
      for (x in seq_len(nSim)) {
        Value_x <- slot(Proj@Data[[x]][[i]], type)@Value
        if (all(!is.finite(utils::tail(Value_x[, fl], 5)))) next
        NewValueAll[x, fl] <- slot(omData, type)@Value[TSIndex, fl]
        NewCVAll[x, fl]    <- .ResolveCV(Proj, type, i, fl, TSIndex,
                                        slot(Proj@Data[[x]][[i]], type), DataYear)
      }
      next
    }

    timing <- if (length(IndexData1@Timing) >= fl) IndexData1@Timing[fl] else NA_real_

    for (x in seq_len(nSim)) {
      Value_x <- slot(Proj@Data[[x]][[i]], type)@Value
      if (all(!is.finite(utils::tail(Value_x[, fl], 5)))) next

      Pop_Number <- purrr::map(Real_Pop_Number_All, \(a)
        a[x, , IndexObs@Areas %||NA% seq_len(nArea), drop = FALSE] |> abind::adrop(1)
      )

      real_nom_index <- .CalcNomIndex(
        Number_List      = Pop_Number,
        object           = Proj,
        stocks           = stocks,
        fleet            = IndexData1@Name[fl],
        IndexObs         = IndexObs,
        Years            = DataYear,
        SelectivityAtAge = IndexObs@Selectivity,
        sim              = x,
        timing           = timing,
        TSIndex          = TSIndex,
        Units            = IndexData1@Units[fl]
      )

      Beta <- if (is.null(IndexObs@Beta)) 1 else IndexObs@Beta[min(x, length(IndexObs@Beta))]

      NewValueAll[x, fl] <- real_nom_index^Beta * .ArraySubsetYear(IndexObs@Error, DataYear)[x] * IndexObs@Efficiency[x]
      NewCVAll[x, fl]    <- .ResolveCV(Proj, type, i, fl, TSIndex,
                                      slot(Proj@Data[[x]][[i]], type), DataYear)
    }
  }

  purrr::map(seq_len(nSim), \(x) {
    IndexData <- slot(Proj@Data[[x]][[i]], type)
    IndexData <- .ResolveUnits(IndexData, nFleet, valid = c("Biomass", "Number", "Recruitment"))
    NewValue <- .EmptyFleetArray(DataYear, IndexData@Name)
    NewCV    <- .EmptyFleetArray(DataYear, IndexData@Name)
    NewValue[1, ] <- NewValueAll[x, ]
    NewCV[1, ]    <- NewCVAll[x, ]
    IndexData@Value <- abind::abind(IndexData@Value, NewValue, along = 1, use.dnns = TRUE)
    if (!is.null(IndexData@CV))
      IndexData@CV <- abind::abind(IndexData@CV, NewCV, along = 1, use.dnns = TRUE)
    IndexData
  })
}
