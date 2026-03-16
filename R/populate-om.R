#' Populate an Operating Model
#'
#' Populate a complete operating model (`om` object) by populating all
#' component objects (stocks, fleets, observation models, and derived
#' structures), performing internal consistency checks, and initializing
#' simulation-specific values.
#'
#' @param OM An [OM()] object to populate.
#' @param silent Logical. If `TRUE`, suppress informational messages and
#'   warnings during population.
#' @param force Logical. If `TRUE`, force re-population even if the internal
#'   object digest indicates no changes since the last call.
#'
#' @details
#' 
#' This function is typically called internally, but may also be called directly.
#' 
#' `PopulateOM()` is the top-level population routine for operating models.
#' It orchestrates population of all model components, including:
#'
#' * Stocks via [PopulateStock()]
#' * Fleets via [PopulateFleet()]
#' * Implementation models (`imp`)
#' * Observation models (`obs`)
#' 
#' To avoid unnecessary recomputation, a digest of the operating model is
#' checked before population. If the digest is unchanged and `force = FALSE`,
#' the input object is returned unchanged.
#'
#' @return
#' An [OM()] object populated with simulation- and time-specific values.
#'
#' @seealso
#' [Populate()], [PopulateStock()], [PopulateFleet()]
#'
#' @examples
#' \dontrun{
#' OM <- PopulateOM(OM)
#'
#' }
#'
#' @export
PopulateOM <- function(OM, silent = FALSE, force = FALSE) {
  
  CheckClass(OM)
  OM <- UpdateObject(OM)
  
  if (!length(OM@maxF))
    OM@maxF <- 3

  if (EmptyObject(OM)) {
    return(OM)
  }

  if (CheckDigest(OM) & !force) {
    return(OM)
  }
  
  if (!silent)
    cli::cli_alert_info('Populating OM {.val {OM@Name}}')

  if (is.null(OM@Stock)) {
    cli::cli_abort(c(
      "x" = "{.var OM} must have at least one stock",
      "i" = "See {.help MSEtool::OM} and {.help MSEtool::Stock}"
    ))
  }

  if (is.null(OM@Fleet)) {
    cli::cli_abort(c(
      "x" = "{.var OM} must have at least one fleet",
      "i" = "See {.help MSEtool::OM} and {.help MSEtool::Fleet}"
    ))
  }

  OM <- OM |>
    PopulateStockList(silent = silent, force = force) |>
    PopulateFleetList(silent = silent, force = force) |>
    PopulateImpList(silent = silent) |>
    PopulateComplexes() |>
    ProcessData() |>
    PopulateObsList(silent = silent) |>
    UpdateSPFrom() |>   # TODO
    ShareParameters() |> # TODO
    StartMessages()

  # CheckCatchFrac() |> # TODO - auto-populate CatchFrac if OM@Data exists
  # CheckAllocation()

  SetDigest(OM)
}

ProcessData <- function(OM) {
  if (is.null(OM@Data)) {
    return(OM)
  }
  stocknames <- StockNames(OM)

  if (isS4(OM@Data)) {
    if (length(stocknames) > 1) {
      stocknames <- paste(stocknames, collape = "-")
    }
    OM@Data <- MakeNamedList(stocknames, OM@Data)
  }

  if (is.list(OM@Data) & is.null(names(OM@Data))) {
    names(OM@Data) <- stocknames
  }

  OM
}

PopulateStockList <- function(OM, silent = FALSE, force = FALSE) {
  if (is.null(OM@Stock)) {
    return(OM)
  }
  
  nStock <- nStock(OM)
  if (inherits(OM@Stock, 'stock')) {
    StockList <- list(OM@Stock)
  } else if (is.list(OM@Stock)) {
    StockList <- OM@Stock
  } else {
    cli::cli_abort(
      "`OM@Stock` must be an S4 Stock or a list of `Stock` objects"
    )
  }
  class(StockList) <- "StockList"
  
  for (st in seq_len(nStock)) {
    Stock <- StockList[[st]]
    Stock@nSim <- OM@nSim
    StockList[[st]] <- PopulateStock(
      Stock = Stock,
      nYear = OM@nYear,
      pYear = OM@pYear,
      CurrentYear = OM@CurrentYear,
      nSim = OM@nSim,
      Seasons = OM@Seasons,
      seed = OM@Seed + st,
      silent = silent,
      force = force
    )
    names(StockList)[st] <- StockList[[st]]@Name
  }
  
  OM@Stock <- StockList
  OM
}


PopulateFleetList <- function(OM, silent = FALSE, force = FALSE) {
  
  if (is.null(OM@Fleet)) 
    return(OM)
  
  StockList <- OM@Stock
  nStocks <- nStock(OM)
  nFleets <- nFleet(OM)
  
  FleetInput <- OM@Fleet
  if (isS4(FleetInput)) {
    # OM@Fleet = single `fleet` object: replicate across stocks
    FleetInput <- replicate(nStocks, list(FleetInput), simplify = FALSE)
  } else if (inherits(FleetInput, "FleetList")) {
    # OM@Fleet = list of `fleet` objects: replicate across stocks
    FleetInput <- replicate(nStocks, FleetInput, simplify = FALSE)
  } else {
    if (length(FleetInput) != 1 && length(FleetInput) != nStocks) {
      cli::cli_abort("`OM@Fleet` must be a list of length 1 or `nStock` ({.val {nStocks}})")
    }
    
    if (length(FleetInput) == 1) {
      # If OM@Fleet is a list of `fleet` objects but only provided for one stock,
      # replicate the FleetList over stocks
      FleetInput <- replicate(nStocks, FleetInput[[1]], simplify = FALSE)
    }
  }

  nFleetperStock <- lengths(FleetInput)
  if (length(unique(nFleetperStock)) != 1) {
    cli::cli_abort(
      "All stocks must have the same number of fleets. Currently: {.val {nFleetperStock}}"
    )
  }
  
  FleetList <- vector("list", nStocks)
  class(FleetList) <- "StockFleetList"
  names(FleetList) <- names(StockList)
  for (st in seq_len(nStocks)) {
    FleetList[[st]] <- FleetInput[[st]]
    class(FleetList[[st]]) <- "FleetList"
  }
  
  for (st in seq_len(nStocks)) {
    for (fl in seq_len(nFleets)) {
      FleetList[[st]][[fl]] <- PopulateFleet(
        Fleet = FleetList[[st]][[fl]],
        Stock = StockList[[st]],
        seed = OM@Seed + st + fl,
        silent = silent,
        force  = force
      )
      names(FleetList[[st]])[fl] <- FleetList[[st]][[fl]]@Name
    }
  }
  
  OM@Fleet  <- ProcessFleetEffort(FleetList, silent=TRUE)
  OM
}

ProcessFleetEffort <- function(FleetList, silent=FALSE) {
  
  # For any given Fleet, Effort should be the same for all Stocks 
  # and all areas.
  # Deviations in Effort assumed to be differences in stock- and/or time-varying
  # fishing efficiency (catchability)
  
  nStock <- length(FleetList)
  if (nStock == 1) {
    return(FleetList)
  }
  
  nFleet <- length(FleetList[[1]])
  if (nFleet == 1) {
    return(FleetList)
  }
  
  for (fl in seq_len(nFleet)) {
    Fleet_fl_all_stocks <- purrr::map(FleetList, `[[`, fl)
    
    # List of Effort Objects
    EffortObjectList <- purrr::map(Fleet_fl_all_stocks, slot, 'Effort')
    qObjectList <- purrr::map(Fleet_fl_all_stocks, slot, 'Catchability')
    
    # Check Effort is the same for all 
    EffortArrayList <- purrr::map(EffortObjectList, slot, "Effort")
    qArrayList <- purrr::map(qObjectList, slot, "Efficiency")
    DistArrayList <- purrr::map(EffortObjectList, slot, "Distribution")
    
    # Get Effort and Catchability dimensions and expand if needed
    EffortDims <- purrr::map(EffortArrayList, dim)
    qDims <- purrr::map(qArrayList, dim)
    
    # Sim 
    EffSim <- vapply(EffortDims, `[`, numeric(1), 1L) |> max()
    qSim <- vapply(qDims, `[`, numeric(1), 1L) |> max()
    nSim <- max(c(EffSim, qSim))
    
    # Year
    EffYears <- purrr::map(EffortArrayList, \(stock) {
      dimnames(stock)[["Year"]]
    }) |> unlist() |> unique()
    
    qYears <- purrr::map(qArrayList, \(stock) {
      dimnames(stock)[["Year"]]
    }) |> unlist() |> unique()
    
    Years <- c(EffYears, qYears) |> unique() |> as.numeric() |> sort() 
    
    EffortArrayList <- purrr::map(EffortArrayList, \(stock) {
      Extend(stock, nSim, NULL, Years)
    })
    qArrayList <- purrr::map(qArrayList, \(stock) {
      Extend(stock, nSim, NULL, Years)
    })
    
    tol <- 1e-4  
    # Stock 1 Effort assumed real Effort
    for (st in 2:nStock) {
      # Check Distribution - if populated, should be identical across stocks
      if (!all(dim(DistArrayList[[st]]) == dim(DistArrayList[[1]]))) {
        cli::cli_abort("Effort Disribution array (`Fleet |> Effort() |> Distribution()`) must be identical across stocks")
      }
      dev <- DistArrayList[[st]] - DistArrayList[[1]]
      
      if (!any(is.na(dev)) && any(abs(dev) > tol)) {
        cli::cli_abort("Effort Disribution array (`Fleet |> Effort() |> Distribution()`) must be identical across stocks")
      }
      
      # Check and update effort
      Effort_nominal <- EffortArrayList[[st]]
      q_nominal <- qArrayList[[st]]
      dev <- Effort_nominal - EffortArrayList[[1]]
      
      if (any(abs(dev) > tol)) {
        if (!silent) {
          cli::cli_alert_warning("Note: `Effort` values for Fleet {.val {fl}} are not the same across stocks")
          cli::cli_alert("Setting Effort for all Stocks to {.val {names(FleetList)[1]}} (Stock 1) effort and adding deviations to {.val Catchability}")
        }
        # Differences in effective effort assumed deviations in efficiency
        Effort_updated <- Effort_nominal - dev
        q_updated <- q_nominal
        ok <- abs(Effort_updated) > tol
        
        q_updated[ok] <- (Effort_nominal[ok] * q_nominal[ok]) / Effort_updated[ok]
        q_updated[!ok] <- q_nominal[!ok]
        EffortArrayList[[st]] <- Effort_updated
        qArrayList[[st]] <- q_updated
      }
      
      FleetList[[st]][[fl]]@Effort@Effort <- EffortArrayList[[st]]
      FleetList[[st]][[fl]]@Catchability@Efficiency <- qArrayList[[st]]
    }
  }
  FleetList
}



PopulateComplexes <- function(OM) {
  if (length(OM@Complexes) > 0) {
    return(OM)
  }

  # TODO validation for Complexes

  if (nStock(OM) == 1) {
    OM@Complexes <- MakeNamedList(StockNames(OM), 1)
    return(OM)
  }

  if (length(OM@Data) > 0) {
    if (length(OM@Data) == 1) {
      OM@Complexes <- MakeNamedList(names(OM@Data), 1:nStock(OM))
      return(OM)
    }

    if (length(OM@Data) == nStock(OM)) {
      OM@Complexes <- list()
      for (i in seq_along(OM@Stock)) {
        OM@Complexes[[i]] <- i
      }
      names(OM@Complexes) <- StockNames(OM)
      return(OM)
    }
  }
  
  # each stock is managed separately 
  stocks <- StockNames(OM)
  OM@Complexes <- setNames(
    as.list(seq_along(stocks)),
    stocks
  )
  
  OM
}

PopulateImpList <- function(OM, silent = FALSE) {
  nStocks <- nStock(OM)
  nFleets <- nFleet(OM)
  ImpList <- MakeNamedList(
    StockNames(OM),
    MakeNamedList(
      FleetNames(OM),
      new("imp")
    )
  )

  if (length(OM@Imp)) {
    for (st in 1:nStocks) {
      for (fl in 1:nFleets) {
        if (isS4(OM@Imp)) {
          Imp <- OM@Imp
        } else {
          if (length(OM@Imp) < st) {
            if (length(OM@Imp) > 1) {
              cli::cli_abort("`OM@Imp` must be a list length 1 or length `nStock` ({.val {nStocks}})")
            }
            Imp <- OM@Imp[[1]][[fl]]
          } else {
            Imp <- OM@Imp[[st]][[fl]]
          }
        }
        ImpList[[st]][[fl]] <- Imp
      }
    }
  }

  OM@Imp <- ImpList
  OM
}

PopulateObsList <- function(OM, silent = FALSE) {
  Complexes <- Complexes(OM)
  ComplexNames <- names(Complexes)
  FleetNames <- FleetNames(OM)
  nFleet <- length(FleetNames)

  if (EmptyObject(OM@Obs)) {
    # initialize Obs object for conditioning
    if (is.null(FleetNames)) {
      return(OM)
    }

    OM@Obs <- MakeNamedList(
      ComplexNames,
      MakeNamedList(FleetNames, new("obs"))
    )
    return(OM)
  }

  # Recycles over both stocks and fleets
  if (inherits(OM@Obs, "obs")) {
    OM@Obs <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, OM@Obs))
  }

  if (!is.list(OM@Obs)) {
    cli::cli_abort("`OM@Obs` must be a list or an object of class `obs`")
  }

  # Prep Obs List
  nComplex <- length(ComplexNames)
  nFleets <- nFleet(OM)
  ObsList <- vector("list", nComplex)
  names(ObsList) <- names(Complexes)

  for (st in 1:nComplex) {
    ObsList[[st]] <- vector("list", nFleet)
    names(ObsList[[st]]) <- FleetNames
    if (isS4(OM@Obs)) {
      ObsList[[st]] <- list(OM@Obs)
      next()
    }

    if (inherits(OM@Obs[[st]], "list")) {
      ObsList[[st]] <- OM@Obs[[st]]
      next()
    }

    for (fl in 1:nFleets) {
      if (length(OM@Obs) < st) {
        if (length(OM@Obs) > 1) {
          cli::cli_abort("`OM@Obs` must be a list length 1 or length `nStock` ({.val {nStocks}})")
        }
        ObsList[[st]][[fl]] <- OM@Obs[[1]][[fl]]
      } else {
        ObsList[[st]][[fl]] <- OM@Obs[[st]][[fl]]
      }
    }
  }

  HistYears <- Years(OM, "H")
  ProjYears <- Years(OM, "P")

  for (st in 1:length(ObsList)) {
    for (fl in 1:length(ObsList[[1]])) {
      SetSeed(OM@Seed + st + fl)

      ObsList[[st]][[fl]]@Effort <- PopulateEffortObs(
        Effort = ObsList[[st]][[fl]]@Effort,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )

      ObsList[[st]][[fl]]@Landings <- PopulateCatchObs(
        Catch = ObsList[[st]][[fl]]@Landings,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )

      ObsList[[st]][[fl]]@Discards <- PopulateCatchObs(
        Catch = ObsList[[st]][[fl]]@Discards,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )

      ObsList[[st]][[fl]]@CPUE <- PopulateIndexObs(
        Index = ObsList[[st]][[fl]]@CPUE,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )

      ObsList[[st]][[fl]]@Survey <- PopulateIndexObs(
        Index = ObsList[[st]][[fl]]@Survey,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )

      # OM@Obs[[st]][[fl]]@CAA

      # OM@Obs[[st]][[fl]]@CAL
    }
  }

  OM@Obs <- ObsList
  OM
}

UpdateSPFrom <- function(OM) {
  stocknames <- StockNames(OM) 
  if (length(stocknames) != nStock(OM)) {
    cli::cli_abort(c('{.var Name} must be unique for each Stock.',
                     'i'='Current Stock Names are {.val {stocknames}}'))
  }
  if (length(stocknames)==1)
    return(OM)
  for (st in 1:nStock(OM)) {
    if (!is.null(OM@Stock[[st]]@SRR@SPFrom)) {
      if (is.numeric(OM@Stock[[st]]@SRR@SPFrom))
        OM@Stock[[st]]@SRR@SPFrom <- stocknames[OM@Stock[[st]]@SRR@SPFrom]
    }
  }
  OM
}




ShareParameters <- function(OM) {
  
  return(OM)
  # 
  # # TODO
  # 
  # 
  # if (length(OM@Herm)) {
  #   stop('Herm not done yet!')
  #   # SexPars$Herm <- checkHerm(SexPars$Herm, maxage, nSim, nyears, proyears)
  # }
  # 
  # # TODO - remove SPFrom if it remains in SRR
  # if (!length(OM@SPFrom))
  #   return(OM)
  # 
  # if (isFALSE(OM@SharePar))
  #   return(OM)
  # 
  # # sexmatches <- sapply(1:nrow(OM@SPFrom), function(x) 
  # #   paste(OM@SPFrom[x, ], collapse = "_"))
  # # 
  # # parcopy <- match(sexmatches, sexmatches)
  # 
  # 
  # # if (!silent)  {
  # 
  # cli::cli_alert_info("You have specified sex-specific dynamics, these parameters will be mirrored across sex types according to `SPFrom(OM)`:")
  # cli::cli_ul()
  # cli::cli_li(OM@SexPars@Misc$Stock)
  # cli::cli_li(OM@SexPars@Misc$Fleet)
  # cli::cli_li('Obs: All parameters')
  # cli::cli_li('Imp: All parameters')
  # cli::cli_end()
  # # }
  # 
  # 
  # for (s in 1:nStock(OM)) {
  #   # Stock
  #   for (sl in OM@SexPars@Misc$Stock) 
  #     slot(OM@Stock[[s]], sl) <- slot(OM@Stock[[parcopy[s]]], sl)
  #   
  #   for (fl in 1:nFleet(OM)) {
  #     # Fleet
  #     for (sl in OM@SexPars@Misc$Fleet) 
  #       slot(OM@Fleet[[s]][[fl]], sl) <- slot(OM@Fleet[[parcopy[s]]][[fl]], sl)
  #     
  #     # Obs
  #     if (OM@SexPars@Misc$Obs) {
  #       for (sl in slotNames(OM@Obs[[s]][[fl]]))
  #         slot(OM@Obs[[s]][[fl]], sl) <- slot(OM@Obs[[parcopy[s]]][[fl]], sl)
  #     }
  #     
  #     # Imp
  #     if (OM@SexPars@Misc$Imp) {
  #       for (sl in slotNames(OM@Imp[[s]][[fl]]))
  #         slot(OM@Imp[[s]][[fl]], sl) <- slot(OM@Imp[[parcopy[s]]][[fl]], sl)
  #     }
  #   }
  # }
  # OM
}

