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
#' @param standardize_effort Logical. Used internally. Apply [StandardizeEffort()]?
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
PopulateOM <- function(OM, silent = FALSE, force = FALSE, standardize_effort = TRUE) {
  
  CheckClass(OM)
  
  OM <- UpdateObject(OM)
  if (!length(OM@maxF)) OM@maxF <- 3
  
  if (EmptyObject(OM)) return(OM)
  if (CheckDigest(OM) & !force) return(OM)
  
  if (is.null(OM@Stock)) 
    cli::cli_abort(c(
      "x" = "{.var OM} must have at least one stock",
      "i" = "See {.help MSEtool::OM} and {.help MSEtool::Stock}"
    ))
  
  if (is.null(OM@Fleet)) 
    cli::cli_abort(c(
      "x" = "{.var OM} must have at least one fleet",
      "i" = "See {.help MSEtool::OM} and {.help MSEtool::Fleet}"
    ))

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
  
  if (standardize_effort)
    OM <- StandardizeEffort(OM, populate=FALSE)

  if (!silent)
    cli::cli_alert_success('Populated OM {.val {OM@Name}}')
  
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
  if (is.null(OM@Stock)) 
    return(OM)

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
  
  if (is.null(OM@Fleet)) return(OM)
  
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
  if (length(unique(nFleetperStock)) != 1) 
    cli::cli_abort(
      "All stocks must have the same number of fleets. Currently: {.val {nFleetperStock}}"
    )
  
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
      
      # extract warning logs - only selectivity for now 
      if (!is.null(FleetList[[st]][[fl]]@Selectivity@Misc$warning)) {
        warns <- FleetList[[st]][[fl]]@Selectivity@Misc
        names(warns[[1]])[1] <- paste(FleetList[[st]][[fl]]@Name, names(warns[[1]])[1], sep = ' - ')
        OM@Log <- c(OM@Log, warns)
      }
      
      names(FleetList[[st]])[fl] <- FleetList[[st]][[fl]]@Name
    }
  }
  
  OM@Fleet <- FleetList
  OM
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
  Complexes    <- Complexes(OM)
  nComplex     <- length(Complexes)
  ComplexNames <- names(Complexes)
  
  FleetNames <- FleetNames(OM)
  nFleet     <- length(FleetNames)
  if (is.null(FleetNames) || nFleet < 1) return(OM)
  
  # Empty object — initialise with fleet-only obs for conditioning
  if (EmptyObject(OM@Obs)) {
    OM@Obs <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, new("obs")))
    return(OM)
  }
  
  # Single obs object — replicate over complexes and fleets
  if (inherits(OM@Obs, "obs"))
    OM@Obs <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, OM@Obs))
  
  # Validate complex length
  if (length(OM@Obs) != nComplex)
    cli::cli_abort(c(
      'x' = "`OM@Obs` must be length `length(Complexes(OM))` ({nComplex})",
      'i' = "Currently length {.val {length(OM@Obs)}}"
    ))
  
  # Validate each complex has at least nFleet elements
  complex_lengths <- purrr::map_int(OM@Obs, length)
  if (any(complex_lengths < nFleet))
    cli::cli_abort(c(
      'x' = 'Each element of `OM@Obs` must have at least `nFleet(OM)` ({nFleet}) element{?s}',
      'i' = 'Currently length {.val {complex_lengths}}'
    ))
  
  # Validate all elements are obs objects
  cls <- purrr::map(OM@Obs, \(st) purrr::map_chr(st, class)) |> unlist()
  if (any(cls != 'obs'))
    cli::cli_abort(c(
      'x' = 'Each element of `OM@Obs[[st]][[fl]]` must be a {.help MSEtool::Obs} object',
      'i' = 'Currently class {.val {cls}}'
    ))
  
  # Validate survey names: elements beyond nFleet must be explicitly named
  # and all names within each complex must be unique
  for (st in seq_len(nComplex)) {
    obs_names  <- names(OM@Obs[[st]])
    nSurvey    <- length(OM@Obs[[st]]) - nFleet
    
    if (nSurvey > 0) {
      survey_names <- obs_names[seq(nFleet + 1, length(obs_names))]
      
      if (any(is.null(survey_names)) || any(nchar(survey_names) == 0))
        cli::cli_abort(c(
          'x' = 'Survey `Obs` objects in complex {.val {ComplexNames[[st]]}} must be explicitly named',
          'i' = 'Provide unique names for elements {nFleet + 1} to {length(obs_names)}'
        ))
      
      if (anyDuplicated(obs_names))
        cli::cli_abort(c(
          'x' = 'All names in `OM@Obs[[{st}]]` must be unique across fleets and surveys',
          'i' = 'Duplicated name{?s}: {.val {obs_names[duplicated(obs_names)]}}'
        ))
    }
  }
  
  # Build populated obs list, preserving fleet + survey structure per complex
  ObsList <- vector("list", nComplex)
  names(ObsList) <- ComplexNames
  
  for (st in seq_len(nComplex)) {
    obs_names     <- names(OM@Obs[[st]])
    nObs          <- length(obs_names)
    ObsList[[st]] <- vector("list", nObs)
    names(ObsList[[st]]) <- obs_names
    
    AgeClasses <- OM@Stock[[st]]@Ages@Classes
    
    for (fl in seq_len(nObs)) {
      SetSeed(OM@Seed + st + fl)
      
      # Fleet obs use fleet-specific size classes; survey obs use NULL
      SizeClasses <- if (fl <= nFleet) {
        OM@Fleet[[st]][[fl]]@Selectivity@Classes
      } else {
        NULL
      }
      
      ObsList[[st]][[fl]] <- PopulateObs(Obs       = OM@Obs[[st]][[fl]],
                                         HistYears = Years(OM, "H"),
                                         ProjYears = Years(OM, "P"),
                                         nSim      = OM@nSim,
                                         AgeBins   = AgeClasses,
                                         SizeBins  = SizeClasses)
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

