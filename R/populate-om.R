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
#' @param adjust_fecundity  Logical. Used internally. Apply [AdjustSeasonalFecundity()]?
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
#' For multi-stock `OM`s, projection recruitment deviations are additionally
#' correlated across stocks based on historical covariance (see
#' [GenMultiStockRecDevs()]), unless disabled via
#' `OM@Control$CorrelatedRecDevs <- FALSE`.
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
PopulateOM <- function(OM, 
                       silent = FALSE, 
                       force = FALSE, 
                       standardize_effort = TRUE,
                       adjust_fecundity = TRUE) {
  
  .CheckClass(OM)
  
  OM <- UpdateObject(OM)
  if (!length(OM@maxF)) OM@maxF <- 3
  
  if (EmptyObject(OM)) return(OM)
  if (.CheckDigest(OM) & !force) return(OM)
  
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

  StockListRaw <- if (inherits(OM@Stock, 'stock')) list(OM@Stock) else OM@Stock
  auto_proj <- purrr::map_lgl(StockListRaw, \(st) EmptyObject(st@SRR@RecDevProj))

  OM <- OM |>
    .PopulateStockList(silent = silent, force = force) |>
    .GenerateMultiStockRecDevs(overwrite = auto_proj, silent = silent) |>
    .PopulateFleetList(silent = silent, force = force) |>
    .PopulateComplexes() |>
    .PopulateImpList(silent = silent) |>
    .ProcessData() |>
    .PopulateObsList(silent = silent) |>
    .UpdateSPFrom() |>
    .ValidateSPFrom() |>
    .StartMessages()

  .CheckOMReady(OM)

  if (adjust_fecundity)
    OM <- AdjustSeasonalFecundity(OM, silent = silent)

  if (standardize_effort)
    OM <- StandardizeEffort(OM, populate=FALSE)

  if (!silent)
    cli::cli_alert_success('Populated OM {.val {OM@Name}}')

  .SetDigest(OM)
}

.ProcessData <- function(OM) {
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

.PopulateStockList <- function(OM, silent = FALSE, force = FALSE) {
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


#' Generate correlated multi-stock projection recruitment deviations
#'
#' Internal `PopulateOM()` pipeline step. Controlled by
#' `OM@Control$CorrelatedRecDevs` (`logical(1)`, default `TRUE`): whether
#' multi-stock operating models get projection recruitment deviations
#' correlated with historical cross-stock covariance (via
#' [GenMultiStockRecDevs()]), rather than independent per-stock deviations.
#' A property of the OM itself -- not a [SimControl()] toggle -- since it
#' changes simulated dynamics, not just what gets computed/reported.
#'
#' Runs after `.PopulateStockList()` has generated each stock's (independent)
#' `RecDevProj`, then overwrites it where applicable; stocks whose
#' `RecDevProj` was user-supplied before population (`overwrite = FALSE`)
#' are left untouched.
#'
#' @keywords internal
.GenerateMultiStockRecDevs <- function(OM, overwrite, silent = FALSE) {
  if (nStock(OM) < 2) return(OM)
  if (isFALSE(OM@Control$CorrelatedRecDevs)) return(OM)
  GenMultiStockRecDevs(OM, silent = silent, overwrite = overwrite)
}

.PopulateFleetList <- function(OM, silent = FALSE, force = FALSE) {
  
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
        warns$warning[[1]]$name <- paste(FleetList[[st]][[fl]]@Name, warns$warning[[1]]$name, sep = ' - ')
        OM@Log <- .JoinLog(OM@Log, warns)
      }
      
      names(FleetList[[st]])[fl] <- FleetList[[st]][[fl]]@Name
    }
  }
  
  OM@Fleet <- FleetList
  OM
}


.PopulateComplexes <- function(OM) {
  if (length(OM@Complexes) > 0) {
    # Validate the user-supplied Complexes before proceeding.
    cx   <- OM@Complexes
    nStk <- nStock(OM)

    # Must be a fully named list.
    cx_nms <- names(cx)
    if (is.null(cx_nms) || any(cx_nms == "")) {
      bad <- which(is.null(cx_nms) | cx_nms == "")
      cli::cli_abort(c(
        "x" = "`OM@Complexes` must be a fully named list.",
        "i" = "Element{?s} {bad} {?is/are} unnamed."
      ))
    }

    # All index values must be integers in 1:nStock.
    all_idx <- unlist(cx)
    if (!is.numeric(all_idx) || any(all_idx != as.integer(all_idx))) {
      cli::cli_abort(c(
        "x" = "All values in `OM@Complexes` must be integer stock indices.",
        "i" = "Found non-integer value{?s}: {.val {unique(all_idx[all_idx != as.integer(all_idx)])}}."
      ))
    }
    all_idx <- as.integer(all_idx)
    if (any(all_idx < 1L) || any(all_idx > nStk)) {
      bad_idx <- sort(unique(all_idx[all_idx < 1L | all_idx > nStk]))
      cli::cli_abort(c(
        "x" = "`OM@Complexes` contains stock {?index/indices} out of range.",
        "i" = "`OM` has {nStk} stock{?s} (indices 1:{nStk}); found {.val {bad_idx}}."
      ))
    }

    # Every stock must belong to exactly one complex.
    if (length(all_idx) != nStk || !setequal(all_idx, seq_len(nStk))) {
      missing_idx <- setdiff(seq_len(nStk), all_idx)
      dup_idx     <- all_idx[duplicated(all_idx)]
      msgs <- character(0)
      if (length(missing_idx))
        msgs <- c(msgs, "i" = "Stock {?index/indices} not assigned to any complex: {.val {missing_idx}}.")
      if (length(dup_idx))
        msgs <- c(msgs, "i" = "Stock {?index/indices} assigned to more than one complex: {.val {unique(dup_idx)}}.")
      cli::cli_abort(c("x" = "Every stock must belong to exactly one complex.", msgs))
    }

    return(OM)
  }

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

.PopulateImpList <- function(OM, silent = FALSE) {
  Complexes    <- Complexes(OM)
  nComplex     <- length(Complexes)
  ComplexNames <- names(Complexes)

  FleetNames <- FleetNames(OM)
  nFleet     <- length(FleetNames)
  if (is.null(FleetNames) || nFleet < 1) return(OM)

  HistYears <- Years(OM, "H")
  ProjYears <- Years(OM, "P")

  # Empty object — initialise with default imp for each complex and fleet
  if (EmptyObject(OM@Imp)) {
    OM@Imp <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, new("imp")))
    return(OM)
  }

  # Single imp object — replicate over complexes and fleets
  if (inherits(OM@Imp, "imp"))
    OM@Imp <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, OM@Imp))

  # Validate complex length
  if (length(OM@Imp) != nComplex)
    cli::cli_abort(c(
      "x" = "`OM@Imp` must be length `length(Complexes(OM))` ({nComplex})",
      "i" = "Currently length {.val {length(OM@Imp)}}"
    ))

  # Validate each complex has at least nFleet elements
  complex_lengths <- purrr::map_int(OM@Imp, length)
  if (any(complex_lengths < nFleet))
    cli::cli_abort(c(
      "x" = "Each element of `OM@Imp` must have at least `nFleet(OM)` ({nFleet}) element{?s}",
      "i" = "Currently length {.val {complex_lengths}}"
    ))

  # Validate all elements are imp objects
  cls <- purrr::map(OM@Imp, \(cx) purrr::map_chr(cx, class)) |> unlist()
  if (any(cls != "imp"))
    cli::cli_abort(c(
      "x" = "Each element of `OM@Imp[[cx]][[fl]]` must be a {.help MSEtool::Imp} object",
      "i" = "Currently class {.val {cls}}"
    ))

  # Build populated imp list indexed by complex then fleet
  ImpList <- vector("list", nComplex)
  names(ImpList) <- ComplexNames

  for (cx in seq_len(nComplex)) {
    imp_nms          <- names(OM@Imp[[cx]])
    ImpList[[cx]]    <- vector("list", nFleet)
    names(ImpList[[cx]]) <- FleetNames
    for (fl in seq_len(nFleet)) {
      ImpList[[cx]][[fl]] <- PopulateImp(OM@Imp[[cx]][[fl]], OM@nSim, HistYears, ProjYears)
    }
  }

  OM@Imp <- ImpList
  OM
}

.PopulateObsList <- function(OM, silent = FALSE) {
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
    obs_names            <- names(OM@Obs[[st]])
    nObs                 <- length(obs_names)
    ObsList[[st]]        <- vector("list", nObs)
    names(ObsList[[st]]) <- obs_names
    AgeClasses           <- OM@Stock[[st]]@Ages@Classes
    
    for (fl in seq_len(nObs)) {
      .SetSeed(OM@Seed + st + fl)
      
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


.UpdateSPFrom <- function(OM) {
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




.ValidateSPFrom <- function(OM) {
  stocknames <- StockNames(OM)
  SPFrom <- purrr::map_chr(OM@Stock, \(st) {
    x <- st@SRR@SPFrom
    if (is.null(x)) NA_character_ else as.character(x)
  })
  names(SPFrom) <- stocknames

  for (st in stocknames) {
    src <- SPFrom[[st]]
    if (is.na(src) || identical(src, st)) next

    if (!src %in% stocknames)
      cli::cli_abort(c(
        "x" = "Stock {.val {st}} has {.field SRR@SPFrom} = {.val {src}}, which is not a known stock.",
        "i" = "Known stocks are {.val {stocknames}}."
      ))

    srcOfSrc <- SPFrom[[src]]
    if (!is.na(srcOfSrc) && !identical(srcOfSrc, src))
      cli::cli_abort(c(
        "x" = "{.var SRR@SPFrom} only supports one-hop sourcing, but {.val {st}} -> {.val {src}} -> {.val {srcOfSrc}} is a longer chain (or a cycle).",
        "i" = "Stock {.val {src}} must not itself have a non-self {.field SPFrom}."
      ))
  }
  OM
}

.CheckOMReady <- function(OM) {
  problems <- character(0)

  scalar_slots <- c("nSim", "nYear", "pYear", "CurrentYear", "Seasons", "maxF")
  missing_scalars <- scalar_slots[!purrr::map_lgl(scalar_slots, \(sl) length(slot(OM, sl)) > 0)]
  if (length(missing_scalars))
    problems <- c(problems, "x" = cli::format_inline("{.var OM} is missing required value{?s} for: {.val {missing_scalars}}"))

  if (!length(OM@Complexes))
    problems <- c(problems, "x" = cli::format_inline("{.var OM@Complexes} has not been populated"))

  stock_required <- list(
    Ages             = "Classes",
    Weight           = "MeanAtAge",
    NaturalMortality = "MeanAtAge",
    Maturity         = "MeanAtAge",
    SRR              = c("Model", "R0")
  )
  stocknames <- StockNames(OM)
  for (st in seq_along(OM@Stock)) {
    nm <- stocknames[st] %||% paste("Stock", st)
    missing <- character(0)
    for (comp in names(stock_required)) {
      obj <- slot(OM@Stock[[st]], comp)
      if (any(purrr::map_lgl(stock_required[[comp]], \(ss) EmptyObject(slot(obj, ss)))))
        missing <- c(missing, comp)
    }
    if (length(missing))
      problems <- c(problems, "x" = cli::format_inline("Stock {.val {nm}} is missing required component{?s}: {.val {missing}}"))
  }

  fleet_required <- list(
    Effort      = "Effort",
    Selectivity = "MeanAtAge"
  )
  fleetnames <- FleetNames(OM)
  for (st in seq_along(OM@Fleet)) {
    nm_stock <- stocknames[st] %||% paste("Stock", st)
    for (fl in seq_along(OM@Fleet[[st]])) {
      nm_fleet <- fleetnames[fl] %||% paste("Fleet", fl)
      missing <- character(0)
      for (comp in names(fleet_required)) {
        obj <- slot(OM@Fleet[[st]][[fl]], comp)
        if (any(purrr::map_lgl(fleet_required[[comp]], \(ss) EmptyObject(slot(obj, ss)))))
          missing <- c(missing, comp)
      }
      if (length(missing))
        problems <- c(problems, "x" = cli::format_inline("Fleet {.val {nm_fleet}} (stock {.val {nm_stock}}) is missing required component{?s}: {.val {missing}}"))
    }
  }

  ComplexNames <- names(Complexes(OM))
  nFl <- nFleet(OM)
  for (cx in seq_along(ComplexNames)) {
    obs_cx <- OM@Obs[[cx]]
    imp_cx <- OM@Imp[[cx]]
    if (length(obs_cx) < nFl || !all(purrr::map_chr(obs_cx, class) == "obs"))
      problems <- c(problems, "x" = cli::format_inline("{.var OM@Obs} for complex {.val {ComplexNames[cx]}} is incomplete"))
    if (length(imp_cx) < nFl || !all(purrr::map_chr(imp_cx, class) == "imp"))
      problems <- c(problems, "x" = cli::format_inline("{.var OM@Imp} for complex {.val {ComplexNames[cx]}} is incomplete"))
  }

  if (length(problems)) {
    problems <- c(problems, "i" = "Supply the missing component(s) and re-run.")
    cli::cli_abort(problems, call = NULL)
  }

  invisible(OM)
}
