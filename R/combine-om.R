#' Combine Multiple Operating Models into One
#'
#' Combine multiple [`om-class`] objects into a single unified operating model.
#' Each element of `OM_List` represents a distinct stock or stock complex
#' (e.g., sex-structured populations). The resulting object spans the full union
#' of historical and projection years across all inputs.
#'
#' @param OM_List Named list of [`om-class`] objects. All elements must share
#'   the same `nSim`, `Seasons`, and fleet names in identical order. See
#'   [FleetNames()].
#' @param Name `character(1)`. Name assigned to the combined OM.
#'   Default `"Combined OM"`
#' @param FillEffort List controlling effort forward-filling. See `Details`.
#' @param FillEfficiency List controlling efficiency forward-filling. See `Details`.
#' @param StandardizeEffort `logical(1)`. If `TRUE` (default), calls
#'   [StandardizeEffort()] after combining, which equalises effort across
#'   stocks for each fleet and back-calculates stock targeting weights.
#' @param record_assumption `logical(1)`. Passed to [StandardizeEffort()].
#'   Record the effort-standardization as an `"assumption"` in `OM@Log`?
#'   Default `TRUE`. Ignored if `StandardizeEffort = FALSE`.
#' @param silent `logical(1)`. If `TRUE`, suppresses informational messages. Default `FALSE`.
#'
#' @return An [`om-class`] object with:
#'   - `@Stock`: concatenation of all stocks across input OMs, in list order.
#'   - `@Fleet`: concatenation of all fleet lists across input OMs.
#'   - `@nYear`: length of the unified historical year range.
#'   - `@pYear`: number of projection years strictly after the latest
#'     historical year.
#'   - `@CurrentYear`: the latest historical year across all input OMs.
#'   - Administrative metadata (`Agency`, `Author`, `nSim`, `Seed`, etc.)
#'     copied from `OM_List[[1]]`.
#' 
#' @details
#' 
#' ## Processing steps
#'
#' 1. Validate that all OMs share `nSim`, `Seasons`, and fleet names.
#' 2. Standardize each OM via [PopulateOM()].
#' 3. Construct unified historical and projection year ranges: historical years
#'    are the union across all OMs; projection years are those strictly greater
#'    than the maximum historical year.
#' 4. Extend every stock and fleet object to the unified year range.
#'    Missing early years are back-filled; missing later historical years are
#'    forward-filled using `FillEffort` and `FillEfficiency`.
#' 5. Concatenate stocks and fleets.
#' 6. Optionally standardize effort across stocks,
#'    which populates `@StockTargeting`
#'
#' Correlated projection recruitment deviations are not generated here --
#' they are generated automatically (and lazily) the next time the combined
#' `OM` is populated, e.g. via [PopulateOM()] or [Simulate()]. See
#' [GenMultiStockRecDevs()] and `OM@Control$CorrelatedRecDevs`.
#'    
#' ## Fill Controls
#'
#' Both `FillEffort` and `FillEfficiency` accept:
#'
#' - `nYears` *(integer)*: Number of trailing historical years used to compute a reference mean  
#' - `SD` *(numeric)*: Lognormal standard deviation applied during stochastic filling  
#' - `Mean` *(numeric)*: Optional override (length 1 or `nSim`)  
#' - `Values` *(array)*: Optional `nSim x n_fill_years` matrix used directly  
#'
#' If `Values` is supplied, all other parameters are ignored.
#' 
#' ## Metadata
#'
#' The following slots are copied verbatim from `OM_List[[1]]`:
#' `Agency`, `Author`, `Email`, `Region`, `Latitude`, `Longitude`,
#' `Sponsor`, `nSim`, `Seasons`, `DataLag`, `Interval`, `nReps`,
#' `pStar`, `maxF`, `Seed`, `Control`. Ensure the first OM contains
#' the desired global configuration.
#' 
#' 
#' @seealso [GenMultiStockRecDevs()]
#'
#' @examples
#' \dontrun{
#' combined <- CombineOMs(
#'   OM_List = list(StockA = om_a, StockB = om_b),
#'   Name = "Combined OM",
#'   FillEffort = list(nYears = 5, SD = 0.05),
#'   FillEfficiency = list(nYears = 5, SD = 0.05)
#' )
#' }
#'
#' @export
CombineOMs <- function(
    OM_List,
    Name = "Combined OM",
    FillEffort = list(nYears = 3, SD = 0.1, Mean = NULL, Values = NULL),
    FillEfficiency = list(nYears = 3, SD = 0.1, Mean = NULL, Values = NULL),
    StandardizeEffort = TRUE,
    record_assumption = TRUE,
    silent = FALSE) {
  
  .ValidateOMList(OM_List)
  
  OM_List <- purrr::map(OM_List, \(OM) UpdateObject(OM) |> 
                          PopulateOM(silent = TRUE, standardize_effort = FALSE)
                        )
  
  yrs <- .GetUnifiedYears(OM_List)
  HistYears <- yrs$HistYears
  ProjYears <- yrs$ProjYears
  
  OM_Out <- .InitializeCombinedOM(OM_List[[1]], Name, HistYears, ProjYears)
  
  extended <- purrr::map(OM_List, \(OM) 
                         .OMExtend(OM, 
                                   HistYears = HistYears,
                                   ProjYears = ProjYears,
                                   FillEffort = FillEffort,
                                   FillEfficiency = FillEfficiency
                         )
  )
  
  OM_Out <- .CombineStocksFleets(OM_Out, extended)
  
  OM_Out <- .CombineStocksData(OM_Out, OM_List)

  OM_Out <- .CombineStocksObs(OM_Out, OM_List)

  OM_Out@EFactor <- purrr::map(OM_List, slot, 'EFactor')

  if (StandardizeEffort)
    OM_Out <- StandardizeEffort(OM_Out, populate = FALSE, record_assumption = record_assumption)
  
  OM_Out
  
}


.CombineStocksData <- function(OM, OM_List) {
  HistYears <- Years(OM, 'H')
  
  for (i in seq_along(OM_List)) {
    StockDataList <- OM_List[[i]]@Data
    for (j in seq_along(StockDataList)) {
      Data <- StockDataList[[j]]
      Data <- Extend(Data, 
                     Years     = HistYears,       
                     default   = NA, 
                     backfill  = TRUE, 
                     skip_data = FALSE)
      
      Data@Years <- HistYears
      Data@YearLH <- max(HistYears)
      
      StockDataList[[j]] <- Data
      
    }
    OM_List[[i]]@Data <- StockDataList
  }
  
  OM@Data <- purrr::list_flatten(
    purrr::map(OM_List, slot, 'Data'),
    name_spec = '{inner}'
  )
  OM
}

.CombineStocksObs <- function(OM, OM_List) {
  OM@Obs <- purrr::list_flatten(
    purrr::map(OM_List, slot, 'Obs'),
    name_spec = '{inner}'
  )
  OM
}

.ValidateOMList <- function(OM_List) {
  
  if (!all(purrr::map_lgl(OM_List, inherits, "om"))) {
    cli::cli_abort("`OM_List` must contain only `om` objects")
  }
  
  nSimList <- purrr::map_int(OM_List, nSim)
  if (!all(nSimList == nSimList[1])) {
    cli::cli_abort("All OMs must have identical `nSim`")
  }
  
  SeasonsList <- purrr::map_int(OM_List, Seasons)
  if (!all(SeasonsList == SeasonsList[1])) {
    cli::cli_abort("All OMs must have identical `Seasons`")
  }
  
  .ValidateFleetNames(purrr::map(OM_List, FleetNames))
}
  
.ValidateFleetNames <- function(x, label='FleetNames') {
  if (length(x) < 2) return(invisible(TRUE))
  ref <- x[[1]]
  ref_name <- names(x)[1]
  for (i in 2:length(x)) {
    if (!identical(ref, x[[i]])) {
      cli::cli_abort(c(
        'x' = 'All elements of `{label}` must be identical.',
        'i' = 'Element {.val {ref_name}} : {.val {ref}}',
        'i' = 'Element {.val {names(x)[i]}}: {.val {x[[i]]}}'
      ))
    }
  }
  invisible(TRUE)
}

.GetUnifiedYears <- function(OM_List) {
  
  HistYears <- purrr::map(OM_List, Years, "H") |>
    unlist() |> unique() |> sort()
  
  ProjYears <- purrr::map(OM_List, Years, "P") |>
    unlist() |> unique() |> sort()
  
  ProjYears <- ProjYears[ProjYears > max(HistYears)]
  
  list(HistYears = HistYears, ProjYears = ProjYears)
}

.InitializeCombinedOM <- function(RefOM, Name, HistYears, ProjYears) {
  
  OM_Out <- OM(Name = Name)
  
  copy_slots <- c(
    "Agency", "Author", "Email", "Region", "Latitude", "Longitude",
    "Sponsor", "nSim", "Seasons", "DataLag", "Interval", "nReps",
    "pStar", "maxF", "Seed", "Control"
  )
  
  OM_Out <- .CopySlots(RefOM, OM_Out, copy_slots)
  
  OM_Out@CurrentYear <- max(HistYears)
  OM_Out@nYear <- length(HistYears)
  OM_Out@pYear <- length(ProjYears)
  
  OM_Out
}

.OMExtend <- function(OM, HistYears, ProjYears, FillEffort, FillEfficiency) {
  
  OM@CurrentYear <- max(HistYears)
  OM@nYear <- length(HistYears)
  OM@pYear <- length(ProjYears)
  
  OM@Stock <- purrr::map(OM@Stock, .ExtendStockYears,
                         HistYears = HistYears,
                         ProjYears = ProjYears)
  
  OM@Fleet <- purrr::map(
    OM@Fleet,
    ~ purrr::map(
      .x,
      .ExtendFleetYears,
      HistYears = HistYears,
      ProjYears = ProjYears,
      nSim = nSim(OM),
      FillEffort = FillEffort,
      FillEfficiency = FillEfficiency
    )
  )
  
  OM
}

.CombineStocksFleets <- function(OM_Out, OM_List) {
  
  
  StockList <- purrr::map(OM_List, \(OM) Stock(OM))
  names(StockList) <- NULL
  OM_Out@Stock <- purrr::list_flatten(StockList)
  
  FleetList <- purrr::map(OM_List, \(OM) Fleet(OM))
  names(FleetList) <- NULL
  OM_Out@Fleet <- purrr::list_flatten(FleetList)
  OM_Out
}

.ExtendFleetYears <- function(Fleet, HistYears, ProjYears, nSim, FillEffort, FillEfficiency) {
  
  Fleet@Effort@Effort <- .ForwardFillArray(Fleet@Effort@Effort, HistYears, FillEffort, nSim)
  Fleet@Effort@Effort <- Extend(Fleet@Effort@Effort, Years = HistYears, backfill = TRUE, default = 0)
  
  Fleet@Effort@Distribution <- Extend(Fleet@Effort@Distribution, Years = HistYears, backfill = TRUE) |>
    ReduceDims(IncYear = TRUE)
  
  Fleet@Catchability@Efficiency <- .ForwardFillArray(Fleet@Catchability@Efficiency, HistYears, FillEfficiency, nSim)
  Fleet@Catchability@Efficiency <- Extend(Fleet@Catchability@Efficiency, Years = HistYears, backfill = TRUE, default = 0)
  
  Fleet@Effort@Targeting <- Extend(Fleet@Effort@Targeting,
                                   Years = HistYears, backfill = TRUE) |>
    ReduceDims(IncYear = TRUE)

  Fleet@Effort@StockTargetingLambda <- Extend(Fleet@Effort@StockTargetingLambda,
                                              Years = HistYears, backfill = TRUE) |>
    ReduceDims(IncYear = TRUE)
  
  Fleet@Selectivity <- Extend(Fleet@Selectivity, Years=HistYears, backfill=TRUE)
  Fleet@Retention <- Extend(Fleet@Retention, Years=HistYears, backfill=TRUE)
  Fleet@DiscardMortality <- Extend(Fleet@DiscardMortality, Years=HistYears, backfill=TRUE)
  Fleet@Closure <- Extend(Fleet@Closure, Years=HistYears, backfill=TRUE) |>
    ReduceDims(IncYear=TRUE)
  
  Fleet@WeightFleetRetained <- Extend(Fleet@WeightFleetRetained, Years=HistYears, backfill=TRUE) |>
    ReduceDims(IncYear=TRUE)
  Fleet@WeightFleetSelected <- Extend(Fleet@WeightFleetSelected, Years=HistYears, backfill=TRUE) |>
    ReduceDims(IncYear=TRUE)

  Fleet
}

.ExtendStockYears <- function(Stock, HistYears, ProjYears) {
  
  slots <- c("Length", "Weight", "NaturalMortality", "Maturity", "Fecundity", "Spatial")
  
  for (nm in slots) {
    slot(Stock, nm) <- Extend(slot(Stock, nm), Years = HistYears, backfill = TRUE) |>
      ReduceDims(IncYear = TRUE)
  }
  
  Stock <- .ExtendSRRYears(Stock, HistYears, ProjYears)
  
  Stock@nYear <- length(HistYears)
  Stock@Years <- c(HistYears, ProjYears)
  Stock@CurrentYear <- max(HistYears)
  Stock@pYear <- length(ProjYears)
  
  Stock
}

.ExtendSRRYears <- function(Stock, HistYears, ProjYears) {
  
  dummy <- NA
  
  Stock@SRR@Pars <- purrr::map(Stock@SRR@Pars, ~ Extend(.x, 
                                                        Years = HistYears, 
                                                        backfill = TRUE) |>
                                 ReduceDims(IncYear=TRUE))
  
  for (nm in c("R0", "SD", "AC")) {
    slot(Stock@SRR, nm) <- Extend(slot(Stock@SRR, nm), 
                                  Years = HistYears, 
                                  backfill = TRUE) |>
      ReduceDims(IncYear=TRUE)
  }
  
  RecDevHist <- Stock@SRR@RecDevHist
  stock_years <- dimnames(RecDevHist)[["Year"]]
  
  back_years <- HistYears[HistYears < min(stock_years)]
  forward_years <- HistYears[HistYears > max(stock_years)]
  
  # Backfill
  if (length(back_years)) {
    RecDevHist <- Extend(RecDevHist, 
                         Years = back_years,
                         backfill = TRUE, 
                         default = 1)
  }
  
  # Forward fill
  if (length(forward_years)) {
    
    RecDevHist <- .SimulateRecDevAR1(
      RecDevHist = RecDevHist,
      forward_years = forward_years,
      HistYears = HistYears,
      Stock = Stock
    )
  }
  
  Stock@SRR@RecDevHist <- RecDevHist
  Stock@SRR@RecDevProj[] <- dummy
  Stock@Misc$InitYear <- min(Stock@Years)
  
  Stock
}

.SimulateRecDevAR1 <- function(RecDevHist, forward_years, HistYears, Stock) {
  
  n_forward <- length(forward_years)
  nSim <- Stock@nSim
  
  years_ind <- match(forward_years, HistYears)
  
  sd <- Stock@SRR@SD[,1, drop=FALSE] |>
    DropDimension("Year") |>
    as.numeric()
  
  ac <- Stock@SRR@AC[,1, drop=FALSE] |>
    DropDimension("Year") |>
    as.numeric()
  
  sd <- rep(sd, nSim)[seq_len(nSim)]
  ac <- rep(ac, nSim)[seq_len(nSim)]
  
  ac[!is.finite(ac)] <- 0
  
  mu <- -0.5 * sd^2 * (1 - ac) / sqrt(1 - ac^2)
  
  lower <- mu - Stock@SRR@TruncSD * sd
  upper <- mu + Stock@SRR@TruncSD * sd
  
  RecDevHist <- Extend(RecDevHist, nSim = nSim, Years=forward_years, default=NA)
  
  logrecdev <- array(
    .Rtnorm(nSim * n_forward, 
           mu = mu, 
           sigma = sd, 
           lower = lower, 
           upper = upper),
    dim = c(nSim, n_forward),
    dimnames = list(Sim=seq_len(nSim), Year=forward_years)
  )
  
  RecDevHist[, years_ind] <- exp(logrecdev)
  
  # Apply AR1 structure
  for (i in seq_len(nSim)) {
    for (t in years_ind) {
      RecDevHist[i, t] <- exp(
        ac[i] * log(RecDevHist[i, t - 1]) +
          log(RecDevHist[i, t]) * sqrt(1 - ac[i]^2)
      )
    }
  }
  
  RecDevHist
}

.ForwardFillArray <- function(array, HistYears, FillList, nSim) {
  
  array_years <- dimnames(array)[["Year"]]
  fill_years <- HistYears[HistYears > max(array_years)]
  n_fill <- length(fill_years)
  
  if (!n_fill) return(array)
  
  array <- Extend(array, nSim = nSim)
  
  Values <- FillList$Values
  
  if (is.null(Values)) {
    
    Mean <- FillList$Mean
    SD <- FillList$SD
    
    if (is.null(Mean)) {
      ref <- utils::tail(array_years, FillList$nYears)
      ref_vals <- Subset(array, Years = ref)
      
      Mean <- apply(ref_vals, "Sim", mean)
    }
    
    mu <- log(Mean) - 0.5 * SD^2
    
    Values <- array(
      exp(rnorm(n_fill * nSim, rep(mu, n_fill), rep(SD, n_fill))),
      dim = c(nSim, n_fill)
    )
  }
  
  .ValidateFillValues(Values, nSim, n_fill)
  
  dimnames(Values) <- list(Sim = seq_len(nSim), Year = fill_years)
  
  abind::abind(array, Values, use.dnns = TRUE)
}

.ValidateFillValues <- function(Values, nSim, nFill) {
  dd <- dim(Values)
  if (is.null(dd) || dd[1] != nSim || dd[2] != nFill) {
    cli::cli_abort("Fill values must have dimensions `nSim x n_fill_years`")
  }
}
