PopulateOM <- function(OM, silent = FALSE) {
  CheckClass(OM)
  # if (CheckDigest(OM) | EmptyObject(OM))
  if (EmptyObject(OM)) {
    return(OM)
  }

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

  OM@Stock <- PopulateStockList(OM, silent)
  OM@Fleet <- PopulateFleetList(OM, silent)
  OM@Imp <- PopulateImpList(OM, silent)

  OM <- OM |>
    PopulateObs() |>
    # CheckCatchFrac() |> # TODO - auto-populate CatchFrac if OM@Data exists
    # CheckAllocation() |>
    PopulateComplexes() |>
    UpdateSPFrom() |> # TODO
    ShareParameters() |> # share parameters for two-sex stocks TODO
    StartMessages()

  # Imp

  # Complexes
  OM <- ProcessData(OM)

  SetDigest(OM)
}

ProcessData <- function(OM) {
  if (is.null(OM@Data)) {
    return(OM)
  }
  stocknames <- StockNames(OM)

  if (isS4(OM@Data)) {
    if (length(stocknames) > 1) {
      stocknames <- paste(stocknames, collapes = "-")
    }
    OM@Data <- MakeNamedList(stocknames, OM@Data)
  }

  if (is.list(OM@Data) & is.null(names(OM@Data))) {
    names(OM@Data) <- stocknames
  }

  OM
}

PopulateStockList <- function(OM, silent = FALSE) {
  nStocks <- nStock(OM)
  StockList <- vector("list", nStocks)
  names(StockList) <- paste("Stock", 1:nStocks)
  class(StockList) <- "StockList"

  for (st in 1:nStocks) {
    if (isS4(OM@Stock)) {
      Stock <- OM@Stock
    } else {
      Stock <- OM@Stock[[st]]
    }
    Stock@nSim <- OM@nSim
    Stock@nYear <- OM@nYear
    Stock@pYear <- OM@pYear
    Stock@CurrentYear <- OM@CurrentYear
    Stock@Seasons <- OM@Seasons
    StockList[[st]] <- PopulateStock(Stock,
      seed = OM@Seed + st,
      silent = silent
    )
    names(StockList)[st] <- Stock@Name
  }
  StockList
}


PopulateFleetList <- function(OM, silent = FALSE) {
  StockList <- OM@Stock
  nStocks <- nStock(OM)
  nFleets <- nFleet(OM)
  FleetList <- vector("list", nStocks)
  class(FleetList) <- "StockFleetList"
  names(FleetList) <- paste("Stock", 1:nStocks)

  for (st in 1:nStocks) {
    names(FleetList)[st] <- names(StockList)[st]
    FleetList[[st]] <- list()
    class(FleetList[[st]]) <- "FleetList"

    for (fl in 1:nFleets) {
      if (is.null(OM@Fleet)) {
        next()
      }
      if (isS4(OM@Fleet)) {
        Fleet <- OM@Fleet
      } else if (inherits(OM@Fleet, "FleetList")) {
        Fleet <- OM@Fleet[[fl]]
      } else {
        if (!length(OM@Fleet[[st]])) {
          next()
        }
        Fleet <- OM@Fleet[[st]][[fl]]
      }

      Stock <- StockList[[st]]

      FleetList[[st]][[fl]] <- PopulateFleet(Fleet,
        Stock,
        seed = OM@Seed + st + fl,
        silent = silent
      )

      names(FleetList[[st]])[fl] <- FleetList[[st]][[fl]]@Name
    }
  }
  ProcessFleetEffort(FleetList, TRUE)
}

ProcessFleetEffort <- function(FleetList, silent = FALSE) {
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
    EffortArrayList <- purrr::map(Fleet_fl_all_stocks, slot, "Effort")
    qArrayList <- purrr::map(Fleet_fl_all_stocks, slot, "Catchability")
    DistArrayList <- purrr::map(Fleet_fl_all_stocks, slot, "Distribution")

    # Check Effort is the same for all
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
    }) |>
      unlist() |>
      unique()

    qYears <- purrr::map(qArrayList, \(stock) {
      dimnames(stock)[["Year"]]
    }) |>
      unlist() |>
      unique()

    Years <- c(EffYears, qYears) |>
      unique() |>
      sort()

    EffortArrayList <- purrr::map(EffortArrayList, \(stock) {
      ArrayExpand(stock, nSim, NULL, Years)
    })
    qArrayList <- purrr::map(qArrayList, \(stock) {
      ArrayExpand(stock, nSim, NULL, Years)
    })

    tol <- 1e-4
    # Stock 1 Effort assumed real Effort
    for (st in 2:nStock) {
      # Check Distribution - if populated, should be identical across stocks
      if (!all(dim(DistArrayList[[st]]) == dim(DistArrayList[[1]]))) {
        cli::cli_abort("Effort Disribution array (`Fleet |> Effort() |> Distribution()`) must be identical across stocks")
      }
      dev <- DistArrayList[[st]] - DistArrayList[[1]]
      if (any(abs(dev) > tol)) {
        cli::cli_abort("Effort Disribution array (`Fleet |> Effort() |> Distribution()`) must be identical across stocks")
      }

      # Check and update effort
      Effort_nominal <- EffortArrayList[[st]]
      q_nominal <- qArrayList[[st]]
      dev <- Effort_nominal - EffortArrayList[[1]]

      if (any(abs(dev) > tol)) {
        if (!silent) {
          cli::cli_alert_warning("Note: `Effort` values for Fleet {.val {fl}} are not the same across stocks")
          cli::cli_alert("Setting Effort for all Stocks to Stock 1 effort and adding deviations to Catchability")
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

      FleetList[[st]][[fl]]@Effort <- EffortArrayList[[st]]
      FleetList[[st]][[fl]]@Catchability <- qArrayList[[st]]
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
          Imp <- OM@Imp[[st]][[fl]]
        }
        ImpList[[st]][[fl]] <- Imp
      }
    }
  }

  ImpList
}
