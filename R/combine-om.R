# #' @param OM_List A list of [om-class] objects.
# #'

CombineOMs <- function(OM_List, Name='Combined OM') {
  
  chk <- purrr::map_lgl(OM_List, inherits, 'om')
  if (!all(chk))
    cli::cli_abort('`OM_List` must be a list of `OM` objects')
  
  # check nSim
  nSimList <- purrr::map_int(OM_List, \(stock) nSim(stock))
  if (!all(nSimList == nSimList[1]))
    cli::cli_abort("`nSim` must be the same for all OMs")
  
  # check seasons
  SeasonList <- purrr::map_int(OM_List, \(stock) Seasons(stock))
  if (!all(SeasonList == SeasonList[1]))
    cli::cli_abort("`Seasons` must be the same for all OMs")
  
  # Check fleet name and order
  fleetnames <- purrr::map(OM_List, \(stock) FleetNames(stock))
  check_fleet_names_list(fleetnames)
  
  # Populate all OMs
  OM_List <- purrr::map(OM_List, \(OM) PopulateOM(OM, silent=TRUE))
  
  # Get Year range
  HistYearsList <- purrr::map(OM_List, \(stock) Years(stock, 'H'))
  ProjYearsList <- purrr::map(OM_List, \(stock) Years(stock, 'P'))
  
  HistYears <- unlist(HistYearsList) |> unique() |> sort()
  nYear <- length(HistYears)
  ProjYears <- unlist(ProjYearsList) |> unique() |> sort()
  ProjYears <- ProjYears[ProjYears>max(HistYears)]
  pYear <- length(ProjYears)
  
  OM_Out <- OM(Name=Name)
  Ref_OM <- OM_List[[1]]
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
    
    ## Stock 
    OM@Stock <- purrr::map(OM@Stock, \(Stock) extend_stock_years(Stock, HistYears, ProjYears))
    
    if (is.null(OM_Out@Stock)) {
      OM_Out@Stock <- OM@Stock
    } else {
      OM_Out@Stock <- c(OM_Out@Stock, OM@Stock)
    }
    
    ## Fleet
    
    ## Effort & Catchability
    OM@Fleet[[1]]$cHL@Effort@Effort
    OM@Fleet[[1]]$cHL@Catchability@Efficiency
    
    stop()

    
    OM@Fleet <- purrr::map(OM@Fleet, \(FleetList) {
      purrr::map(FleetList, \(Fleet) {
        extend_fleet_years(Fleet, HistYears, ProjYears)
      })
    })
    
    if (is.null(OM_Out@Fleet)) {
      OM_Out@Fleet <- OM@Fleet
    } else {
      OM_Out@Fleet <- c(OM_Out@Fleet, OM@Fleet)
    }
  }
  OM_Out
  
}



extend_fleet_years <- function(Fleet, HistYears, ProjYears) {
  Fleet@Effort@Effort <- Extend(Fleet@Effort@Effort, Years=HistYears, backfill=TRUE, default=0)
  
  
  Fleet@Effort@Distribution <- Extend(Fleet@Effort@Distribution, Years=HistYears, backfill=TRUE) |>
    ReduceDims(IncYear=TRUE)
  
  Fleet@Effort@Targeting <- Extend(Fleet@Effort@Targeting, Years=HistYears, backfill=TRUE) |>
    ReduceDims(IncYear=TRUE)
  
  Fleet@Catchability@Efficiency <- Extend(Fleet@Catchability@Efficiency,
                                          Years=HistYears, backfill=TRUE, default=0)
  
  Fleet@Selectivity <- Extend(Fleet@Selectivity, Years=HistYears, backfill=TRUE)
  Fleet@Retention <- Extend(Fleet@Retention, Years=HistYears, backfill=TRUE)
  Fleet@DiscardMortality <- Extend(Fleet@DiscardMortality, Years=HistYears, backfill=TRUE)
  Fleet@Closure <- Extend(Fleet@Closure, Years=HistYears, backfill=TRUE) |>
    ReduceDims(IncYear=TRUE)
  
  Fleet 
  
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
