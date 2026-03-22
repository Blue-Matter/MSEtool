#' Generate Projected Index Data for a Stock/Complex
#'
#' Appends a new year of observed index data (`Value` and `CV`) to the existing
#' `CPUE` or `Survey` data object for a given simulation and stock/complex.
#'
#' @param x Integer. Simulation index.
#' @param Proj A `Hist` object used in the projection.
#' @param DataYear Numeric. The calendar year to generate data for.
#' @param YearsAll Numeric. All calendar years in the historical
#'  and projection years
#' @param i Integer. Stock/complex index.
#' @param stocks `integer`. Stock indices used to subset population arrays.
#' @param StockNames Character. Names of all stocks in the model.
#' @param FleetNames Character. Names of all fleets, used to match
#'   fleet-specific selectivity from `Proj@OM@Fleet`.
#' @param type Character. One of `"CPUE"` or `"Survey"`.
#'
#' @return The updated `CPUE` or `Survey` object.
#' @keywords internal
GenProjData_Index <- function(x, 
                              Proj, 
                              DataYear,
                              YearsAll,
                              i,
                              stocks, 
                              StockNames,
                              FleetNames,
                              type=c('CPUE', 'Survey')) {
  
  # TODO hyperstability Beta not functional yet - ignored
  
  type      <- match.arg(type)
  IndexData <- slot(Proj@Data[[x]][[i]], type)
  
  if (EmptyObject(IndexData)) return(IndexData)
  if (DataYear %in% dimnames(IndexData@Value)[[1]]) return(IndexData)
  
  TSIndex    <- match(DataYear, YearsAll)
  nArea      <- nArea(Proj)
  Value      <- IndexData@Value
  CV         <- IndexData@CV
  nFleet     <- ncol(Value)
  FleetIndex <- match(IndexData@Name, names(Proj@OM@Obs[[i]]))
  
  if (length(FleetIndex) != nFleet)
    cli::cli_abort(
      "Mismatch in number of fleets in `Obs` and `Data[[x]]@{type}`",
      .internal = TRUE
    )
  
  IndexData  <- resolveUnits(IndexData, nFleet, valid=c("Biomass",
                                                        "Number",
                                                        "Recruitment"))
  
  NewValue <- emptyFleetArray(DataYear, IndexData@Name)
  NewCV    <- emptyFleetArray(DataYear, IndexData@Name)
  
  
  Real_Pop_Number <- purrr::map(Proj@Number[stocks], \(stock_n) {
    stock_n[x, , TSIndex, seq_len(nArea), drop = FALSE] |> abind::adrop(c(1, 3))
  })
  
  for (fl in seq_len(nFleet)) {
    IndexObs <- slot(Proj@OM@Obs[[i]][[FleetIndex[fl]]], type)
    if (EmptyObject(IndexObs)) next
    
    # TODO - make this an option
    # currently doesn't simulate index if last five data points were NAs
    if (all(!is.finite(utils::tail(Value[, fl], 5)))) next
    
    if (is.null(IndexObs@Areas)) IndexObs@Areas <- seq_len(nArea)
    
    omData <- Proj@OM@Data[[i]]
    
    if (!is.null(omData)) {
      omVal <- slot(omData, type)@Value
      if (!is.null(omVal) && nrow(omVal) >= TSIndex && ncol(omVal) >= fl) {
        NewValue[, fl] <- omVal[TSIndex, fl]
        next
      }
    }
    
    # simulate data
    if (!is.na(IndexData@Timing[fl]) && IndexData@Timing[fl] != 0)
      cli::cli_alert_warning(
        "`Index@Timing` currently not supported. Calculating from beginning of time step"
      )
    
    SelectivityAtAgeList <- resolveSelectivity(
      Proj, stocks, StockNames, IndexObs, FleetNames, fl, x, TSIndex, nArea
    )
    
    IndexAreas <- IndexObs@Areas %||% seq_len(nArea)
    
    Real_Pop_Number_Selected <- purrr::map2(
      Real_Pop_Number, SelectivityAtAgeList,
      \(num, sel) {
        ArrayMultiply(
          num[, IndexAreas, drop = FALSE],
          sel[, IndexAreas, drop = FALSE]
        ) |> SumOverArea()
      }
    )
    
    Units <- IndexData@Units[fl]
    
    real_nom_index <- switch(Units,
                             Number = purrr::map_dbl(Real_Pop_Number_Selected, sum) |> sum(),
                             
                             Biomass = {
                               WeightAtAgeList <- purrr::map(Proj@OM@Stock[stocks], \(stock) {
                                 stock@Weight@MeanAtAge[x, , TSIndex, drop = FALSE] |>
                                   DropDimension(c("Sim", "Year"))
                               })
                               purrr::map2(Real_Pop_Number_Selected, WeightAtAgeList, ArrayMultiply) |>
                                 List2Array("Stock") |>
                                 sum()
                             },
                             
                             Recruitment = purrr::map_dbl(Real_Pop_Number_Selected, \(pop_n) pop_n[1]) |> sum(),
                             
                             cli::cli_abort(
                               paste("Only {.val Biomass}, {.val Number}, and {.val Recruitment} are",
                                     "currently supported for {.val Units} in {.val Obs@{type}}"),
                               .internal = TRUE
                             )
    )
    
    NewValue[, fl] <- real_nom_index * ArraySubsetYear(IndexObs@Error, DataYear)[x] * IndexObs@Efficiency[x]
    NewCV[, fl] <- resolveCV(Proj, type, i, fl, TSIndex, IndexData, DataYear)
  }
  
  IndexData@Value <- abind::abind(Value, NewValue, along = 1, use.dnns = TRUE)
  if (!is.null(IndexData@CV))
    IndexData@CV <- abind::abind(CV, NewCV, along = 1, use.dnns = TRUE)
  
  IndexData
}