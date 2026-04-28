#' Combine Multiple Operating Models into One
#'
#' Combines a list of [om-class] objects into a single `om` object. Each OM in
#' `OM_List` represents a distinct stock or a set of stocks (e.g. female & male). 
#' All OMs are merged into a unified operating model that spans the full 
#' historical and projection year range across all input OMs.
#'
#'
#' @param OM_List A named list of [om-class] objects to be combined. Each element
#'   represents a separate stock. All OMs must share:
#'   - The same number of simulations (`nSim`)
#'   - The same number of seasons (`Seasons`)
#'   - Identical fleet names in identical order (see [FleetNames()])
#'   
#' @param Name A character string giving the name of the combined OM.
#'   Defaults to `"Combined OM"`.
#' @param FillEffort A named list controlling how effort values are forward-filled
#'   for years that exist in the combined year range but not in an individual OM.
#'   Elements:
#'   \describe{
#'     \item{`nYears`}{Integer. Number of trailing historical years to use as the
#'       reference when computing a fill mean. Defaults to `3`.}
#'     \item{`SD`}{Numeric. Log-scale standard deviation of the lognormal noise
#'       added to filled values. Set to `0` for deterministic filling. Automatically
#'       set to `0` for simulations where effort was constant over the reference
#'       period. Defaults to `0.1`.}
#'     \item{`Mean`}{Optional numeric vector of length 1 or `nSim`. If supplied,
#'       overrides the mean computed from reference years. Defaults to `NULL`.}
#'     \item{`Values`}{Optional numeric array of dimensions `nSim x n_fill_years`.
#'       If supplied, these values are used directly and `Mean`/`SD`/`nYears` are
#'       ignored. Defaults to `NULL`.}
#'   }
#' @param FillEfficiency A named list with the same structure as `FillEffort`,
#'   controlling forward-filling of catchability efficiency values.
#'
#' The combined year range is determined by taking the union of historical years
#' across all OMs, sorted in ascending order. Projection years are similarly
#' unioned, retaining only years beyond the maximum historical year.
#' 
#' Before combining, each OM is populated via [PopulateOM()], and all stock and
#' fleet data are extended or back-filled to cover the unified year range.
#'
#' Stock and fleet arrays that do not span the full combined year range are
#' extended using [Extend()] and [Subset()]. Years preceding a stock's original
#' start year are back-filled; years beyond are forward-filled using the
#' `FillEffort` / `FillEfficiency` parameters.
#'
#' Administrative metadata (`Agency`, `Author`, `Email`, `Region`, `Latitude`,
#' `Longitude`, `Sponsor`, `nSim`, `Seasons`, `DataLag`, `Interval`, `nReps`,
#' `pStar`, `maxF`, `Seed`, `Control`) is copied from the **first** OM in
#' `OM_List`. Ensure this OM carries the correct global settings.
#'
#' ## Validation:
#' The function will abort with an informative error if:
#' - Any element of `OM_List` is not an [om-class] object
#' - OMs do not share the same `nSim`
#' - OMs do not share the same `Seasons`
#' - Fleet names are not identical across all OMs
#' - `FillEffort$Values` or `FillEfficiency$Values` dimensions do not match
#'   `nSim x n_fill_years`
#'
#' @return An [om-class] object containing all stocks and fleets from `OM_List`,
#'   with:
#'     - `@@Stock` set to the concatenated stocks across all input OMs
#'     - `@@Fleet` set to the concatenated fleets across all input OMs
#'     - `@@nYear` and `@@pYear` reflecting the unified historical and
#'       projection year ranges
#'     - `@@CurrentYear` set to the maximum historical year across all OMs
#'     -All administrative slots (`Agency`, `Author`, `nSim`, etc.) copied
#'       from the first OM in `OM_List`
#'   }
#'   
#' @seealso [PopulateOM()], [Extend()], [Subset()], [om-class], [FleetNames()]
#'
#' @examples
#' \dontrun{
#' # Combine two OMs with matching fleet structures
#' combined <- CombineOMs(
#'   OM_List = list(StockA = om_a, StockB = om_b),
#'   Name = "My Combined OM",
#'   FillEffort = list(nYears = 5, SD = 0.05, Mean = NULL, Values = NULL),
#'   FillEfficiency = list(nYears = 5, SD = 0.05, Mean = NULL, Values = NULL)
#' )
#' }
#'
#' @export
CombineOMs <- function(OM_List,
                       Name='Combined OM',
                       FillEffort = list(nYears = 3,
                                          SD = 0.1,
                                          Mean = NULL,
                                          Values = NULL),
                       FillEfficiency = list(nYears = 3,
                                             SD = 0.1,
                                             Mean = NULL,
                                             Values = NULL)) {
  
  chk <- purrr::map_lgl(OM_List, inherits, 'om')
  if (!all(chk))
    cli::cli_abort('`OM_List` must be a list of `OM` objects')
  
  nSimList <- purrr::map_int(OM_List, \(stock) nSim(stock))
  if (!all(nSimList == nSimList[1]))
    cli::cli_abort("`nSim` must be the same for all OMs")
  
  SeasonList <- purrr::map_int(OM_List, \(stock) Seasons(stock))
  if (!all(SeasonList == SeasonList[1]))
    cli::cli_abort("`Seasons` must be the same for all OMs")
  
  fleetnames <- purrr::map(OM_List, \(stock) FleetNames(stock))
  check_fleet_names_list(fleetnames)
  
  OM_List <- purrr::map(OM_List, \(OM) PopulateOM(OM, silent=TRUE))
  
  HistYears <- purrr::map(OM_List, \(stock) Years(stock, 'H')) |>
    unlist() |> unique() |> sort()
  
  ProjYears <- purrr::map(OM_List, \(stock) Years(stock, 'P')) |>
    unlist() |> unique() |> sort()
  ProjYears <- ProjYears[ProjYears>max(HistYears)]
  
  nYear <- length(HistYears)
  pYear <- length(ProjYears)
  
  Ref_OM <- OM_List[[1]]
  OM_Out <- OM(Name=Name)
  
  slots <- c("Agency", "Author", "Email", "Region", "Latitude", "Longitude",
             "Sponsor", "nSim",'Seasons', 'DataLag', 'Interval', 'nReps',
             'pStar', 'maxF', 'Seed', 'Control')
  
  for (slot_name in slots)
    slot(OM_Out, slot_name) <- slot(Ref_OM, slot_name)
  
  OM_Out@CurrentYear <- max(HistYears)
  OM_Out@nYear <- nYear
  OM_Out@pYear <- pYear
  
  # Loop over OMs and extend years
  for (st in seq_along(OM_List)) {
    OM <- OM_List[[st]]
    name <- OM@Name
    OM@CurrentYear <- max(HistYears)
    OM@nYear <- nYear
    OM@pYear <- pYear
    
    OM@Stock <- purrr::map(OM@Stock, \(Stock)
                           extend_stock_years(Stock, HistYears, ProjYears))
    
    OM@Fleet <- purrr::map(OM@Fleet, \(FleetList)
                           purrr::map(FleetList, \(Fleet)
                                      extend_fleet_years(Fleet, HistYears, ProjYears,
                                                         nSim = nSim(OM),
                                                         FillEffort = FillEffort,
                                                         FillEfficiency = FillEfficiency)))
    
    OM_Out@Stock <- c(OM_Out@Stock, OM@Stock)
    OM_Out@Fleet <- c(OM_Out@Fleet, OM@Fleet)
  }
  OM_Out
}



extend_fleet_years <- function(Fleet, HistYears, ProjYears, nSim,
                               FillEffort, FillEfficiency) {
  
  Fleet@Effort@Effort <- forward_fill_array(Fleet@Effort@Effort, HistYears,
                                            FillEffort, nSim)
  Fleet@Effort@Effort <- Extend(Fleet@Effort@Effort,
                                Years = HistYears, backfill = TRUE, default = 0)
  Fleet@Effort@Distribution <- Extend(Fleet@Effort@Distribution,
                                      Years = HistYears, backfill = TRUE) |>
    ReduceDims(IncYear = TRUE)
  
  Fleet@Catchability@Efficiency <- forward_fill_array(Fleet@Catchability@Efficiency,
                                                      HistYears, FillEfficiency, nSim)
  Fleet@Catchability@Efficiency <- Extend(Fleet@Catchability@Efficiency,
                                          Years = HistYears, backfill = TRUE, default = 0)
  
  Fleet@Effort@Targeting <- Extend(Fleet@Effort@Targeting,
                                   Years = HistYears, backfill = TRUE) |>
    ReduceDims(IncYear = TRUE)
  
  Fleet@Selectivity <- Extend(Fleet@Selectivity, Years=HistYears, backfill=TRUE)
  Fleet@Retention <- Extend(Fleet@Retention, Years=HistYears, backfill=TRUE)
  Fleet@DiscardMortality <- Extend(Fleet@DiscardMortality, Years=HistYears, backfill=TRUE)
  Fleet@Closure <- Extend(Fleet@Closure, Years=HistYears, backfill=TRUE) |>
    ReduceDims(IncYear=TRUE)
  
  Fleet 
  
}

check_fill_values <- function(Values, nSim, n_fill_years, fill_years) {
  dd <- dim(Values)
  
  if (is.null(dd) || dd[1]!=nSim || dd[2] !=n_fill_years )
    cli::cli_abort(c('x' = "`FillEffort$Values` must be an array with dimensions `nSim` x `n_fill_years`",
                     'i' = '`nSim` = {.val {nSim}}',
                     'i' = '`fill_years` = {.val {fill_years}}',
                     'i' = '`n_fill_years` = {.val {n_fill_years}}'
    ))
}

forward_fill_array <- function(array, HistYears, FillList, nSim) {
  array_years <- dimnames(array)[['Year']]
  
  ind <- which(HistYears > max(array_years))
  fill_years <- HistYears[ind]
  n_fill_years <- length(ind)
  
  if (!n_fill_years) return(array)
  
  array <- Extend(array, nSim=nSim)
  
  nYears <- FillList$nYears
  SD <- FillList$SD
  Mean <- FillList$Mean
  Values <- FillList$Values
  
  if (is.null(Values)) {
    # generate Values
    if (is.null(Mean)) {
      ref_years <- utils::tail(array_years, nYears)
      value_ref_years <- Subset(array, Years=ref_years)
      result <- apply(value_ref_years, 1, function(row) all(row == row[1]))
      SD <- rep(SD, nSim)[seq_len(nSim)]
      SD[result] <- 0
      Mean <- Subset(value_ref_years, Years=ref_years) |>
        apply("Sim", mean)
    }
    if (length(Mean) != nSim && length(Mean)!=1)
      cli::cli_abort(c('x'="`FillList$Mean` must be vector of length 1 or length `nSim`"))
    
    mu <- log(Mean) -0.5 * SD^2
    Values <- array(
      exp(rnorm(n_fill_years * nSim, rep(mu, n_fill_years), rep(SD, n_fill_years))),
      dim = c(nSim, n_fill_years)
    )
  }
  check_fill_values(Values, nSim, n_fill_years, fill_years)
  dimnames(Values) <- list(Sim = seq_len(nSim), Year = fill_years)
  abind::abind(array, Values, use.dnns = TRUE) 
}  

extend_srr_years <- function(Stock, HistYears, ProjYears) {
  
  Stock@SRR@Pars <- purrr::map(Stock@SRR@Pars, \(par) {
    Extend(par, Years=HistYears, backfill = TRUE) |> ReduceDims(IncYear=TRUE)
  })
  
  slot_names <- c('R0', 'SD', 'AC')
  
  for (name in slot_names) {
    slot(Stock@SRR, name) <- Extend(slot(Stock@SRR, name), 
                                    Years=HistYears, 
                                    backfill = TRUE) |> 
      ReduceDims(IncYear=TRUE)
  }
  
  RecDevHist <- Subset(Stock@SRR@RecDevHist, Years=HistYears) |>
    Extend(Years=HistYears, backfill = TRUE, default=1)
  
  
  RecDevProj <- Subset(Stock@SRR@RecDevProj, Years=ProjYears) |>
    Extend(Years=ProjYears, default=NA)
  
  Stock@Misc$InitYear <- min(Stock@Years)
  Stock@SRR@RecDevHist <- RecDevHist
  Stock@SRR@RecDevProj <- RecDevProj
  Stock
}

extend_stock_years <- function(Stock, HistYears, ProjYears) {
  
  slot_names <- c('Length', 'Weight', 'NaturalMortality', 'Maturity',
                 'Fecundity', 'Spatial')
  
  for (name in slot_names) {
    slot(Stock, name) <- Extend(slot(Stock, name), Years=HistYears, backfill = TRUE) |> ReduceDims(IncYear=TRUE)
  }
  
  Stock <- extend_srr_years(Stock, HistYears, ProjYears)
  Stock@nYear <- length(HistYears)
  Stock@Years <- c(HistYears, ProjYears)
  Stock@CurrentYear <- max(HistYears)
  Stock@pYear <- length(ProjYears)
  
  Stock 
}

check_fleet_names_list <- function(x, label='FleetNames') {
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






