.CheckHistMisc <- function(Hist, Period = c('Historical', 'Projection')) {
  Period <- match.arg(Period)
  
  nSim   <- nSim(Hist)
  nStock <- nStock(Hist)
  nFleet <- nFleet(Hist)
  nArea  <- nArea(Hist)
  
  HistYears <- Years(Hist, 'H')
  ProjYears <- Years(Hist, 'P')
  Years <- if (Period == 'Historical') HistYears else c(HistYears, ProjYears)
  nyears <- length(Years)
  
  Misc <- Hist@Misc
  
  # error accumulator environment
  .err <- new.env(parent = emptyenv())
  .err$msgs <- character(0)
  
  add_err <- function(...) {
    .err$msgs <- c(.err$msgs, paste0(...))
  }
  
  flush_errors <- function(where = "") {
    if (length(.err$msgs) == 0L) return(invisible(NULL))
    msg <- .err$msgs
    .err$msgs <- character(0)
    bullets <- stats::setNames(msg, rep("x", length(msg)))
    cli::cli_abort(
      c(if (nzchar(where)) paste0(".CheckHistMisc failed [", where, "]") else
        ".CheckHistMisc failed", bullets),
      .internal = TRUE
    )
  }
  
  # helpers
  check_array <- function(x, name, expected, Years = NULL) {
    if (is.null(x)) {
      add_err(name, ": is NULL")
      return(invisible(FALSE))
    }
    if (!is.numeric(x)) {
      add_err(name, ": must be numeric, got ", class(x)[1])
      return(invisible(FALSE))
    }
    dd <- dim(x)
    if (is.null(dd)) {
      add_err(name, ": has no dim attribute (not an array)")
      return(invisible(FALSE))
    }
    ndim <- length(expected)
    if (length(dd) != ndim) {
      add_err(name, ": expected ", ndim, "D array, got ", length(dd), "D",
              " [", paste(dd, collapse = " x "), "]")
      return(invisible(FALSE))
    }
    
    # dim[1] = Sim: must be 1 (broadcast) or nSim 
    if (dd[1L] != 1L && dd[1L] != nSim)
      add_err(name, ": dim[1] (Sim) = ", dd[1L],
              "; must be 1 (broadcast) or nSim (", nSim, ")")
    
    # dim[2..N]: must match exactly: no broadcasting on non-Sim dimensions
    if (ndim > 1L) {
      bad <- which(!dd[-1L] >= expected[-1L]) + 1L   # shift back to original index
      if (length(bad) > 0)
        add_err(name, ": dim(s) [", paste(bad, collapse = ", "), "] wrong.",
                " Got [", paste(dd, collapse = " x "), "]",
                " expected [", paste(expected, collapse = " x "), "]")
    }
    
    # Year dimname check
    if (!is.null(Years) && !is.null(dimnames(x))) {
      dn <- dimnames(x)
      yr_axis <- which(names(dn) == "Year")
      if (length(yr_axis) == 1L) {
        missing_yrs <- setdiff(as.character(Years), dn[[yr_axis]])
        if (length(missing_yrs) > 0)
          add_err(name, ": missing Year(s) in dimnames: ",
                  paste(missing_yrs, collapse = ", "))
      }
    }
    invisible(TRUE)
  }
  
  # list that must have nStock elements
  check_stock_list <- function(lst, name, fn) {
    if (is.null(lst))  { add_err(name, ": is NULL");                          return(invisible(FALSE)) }
    if (!is.list(lst)) { add_err(name, ": must be a list, got ", class(lst)); return(invisible(FALSE)) }
    if (length(lst) != nStock)
      add_err(name, ": length = ", length(lst), ", expected nStock = ", nStock)
    for (st in seq_len(min(length(lst), nStock)))
      fn(lst[[st]], st)
    invisible(TRUE)
  }
  
  # Value-range checks 
  check_values <- function(x, name, allow_neg = TRUE, allow_inf = FALSE,
                           allow_na = FALSE, range = NULL) {
    if (!is.numeric(x) || length(x) == 0L) return(invisible(NULL))
    if (!allow_na  && anyNA(x))
      add_err(name, ": contains NA/NaN (will cause C++ undefined behaviour)")
    if (!allow_inf && any(is.infinite(x)))
      add_err(name, ": contains Inf/-Inf values")
    if (!allow_neg && any(x < 0, na.rm = TRUE))
      add_err(name, ": contains negative values")
    if (!is.null(range)) {
      lo <- range[1L]; hi <- range[2L]
      if (any(x < lo | x > hi, na.rm = TRUE))
        add_err(name, ": values outside [", lo, ", ", hi, "]")
    }
    invisible(NULL)
  }
  
  # required Misc objects
  required_objects <- c(
    "maxF", "SPFrom", "PlusGroup", "Mode", "RelSize", "SpawnTimeFrac",
    "LengthList", "WeightList", "NaturalMortalityList", "MaturityList",
    "SemelparousList", "FecundityList", "MovementList",
    "SRR_Pars", "RecDevs", "RecLag", "SRR_Model",
    "SP0", "R0", "RecDist",
    "Catchability", "Closure", "Spatial_Targeting",
    "StockTargeting", "StockTargetingFlag",
    "WeightFleetRetainedList", "WeightFleetSelectedList", "SelAgeList", "RetAgeList", "DiscMortList")
  
    # "SelSizeList", "RetSizeList", "DiscMortSizeList"
  
  missing_objects <- setdiff(required_objects, names(Misc))
  if (length(missing_objects) > 0)
    add_err("Hist@Misc is missing required element(s): ",
            paste(missing_objects, collapse = ", "))
  flush_errors("required Misc keys")
  
  # scalars
  if (!is.numeric(Misc$maxF) || length(Misc$maxF) != 1L ||
      !is.finite(Misc$maxF) || Misc$maxF <= 0)
    add_err("Hist@Misc$maxF: must be a single finite positive number, got ", Misc$maxF)

  if (!is.logical(Misc$StockTargetingFlag) &&
      !Misc$StockTargetingFlag %in% c(0, 1))
    add_err("Hist@Misc$StockTargetingFlag: must be logical or 0/1")
  
  flush_errors("scalars")
  
  # 1D vectors 
  
  # SPFrom: length nStock, values in [1, nStock]
  if (!is.numeric(Misc$SPFrom) || length(Misc$SPFrom) != nStock)
    add_err("Hist@Misc$SPFrom: must be numeric of length nStock (", nStock,
            "), got length ", length(Misc$SPFrom))
  if (is.numeric(Misc$SPFrom)) {
    bad <- which(Misc$SPFrom < 1 | Misc$SPFrom > nStock | is.na(Misc$SPFrom))
    if (length(bad) > 0)
      add_err("Hist@Misc$SPFrom: values must be in [1, nStock=", nStock,
              "]; bad stock(s): ", paste(bad, collapse = ","))
  }
  
  # PlusGroup: length nStock, values 0 or 1
  if (!is.numeric(Misc$PlusGroup) || length(Misc$PlusGroup) != nStock)
    add_err("Hist@Misc$PlusGroup: must be numeric of length nStock (", nStock,
            "), got length ", length(Misc$PlusGroup))
  if (is.numeric(Misc$PlusGroup) &&
      !all(Misc$PlusGroup %in% c(0, 1), na.rm = TRUE))
    add_err("Hist@Misc$PlusGroup: all values must be 0 or 1")
  
  # RecLag: length nStock, non-negative integers
  if (!is.numeric(Misc$RecLag) || length(Misc$RecLag) != nStock)
    add_err("Hist@Misc$RecLag: must be numeric of length nStock (", nStock,
            "), got ", class(Misc$RecLag), " of length ", length(Misc$RecLag))
  if (is.numeric(Misc$RecLag) && any(Misc$RecLag < 0, na.rm = TRUE))
    add_err("Hist@Misc$RecLag: negative lag(s) not allowed")
  
  # SRR_Model: length nStock, values in {0, 1, 2}
  if (!is.numeric(Misc$SRR_Model) || length(Misc$SRR_Model) != nStock)
    add_err("Hist@Misc$SRR_Model: must be numeric of length nStock (", nStock,
            "), got length ", length(Misc$SRR_Model))
  if (is.numeric(Misc$SRR_Model)) {
    bad_models <- which(!Misc$SRR_Model %in% c(0, 1, 2))
    if (length(bad_models) > 0)
      add_err("Hist@Misc$SRR_Model: invalid code(s) for stock(s) ",
              paste(bad_models, collapse = ","),
              "; must be 0 (BH), 1 (Ricker), 2 (HockeyStick)")
  }
  
  # Mode: length nFleet, values 0/1 (UseDensity flags)
  if (length(Misc$Mode) != nFleet)
    add_err("Hist@Misc$Mode: length = ", length(Misc$Mode),
            ", expected nFleet = ", nFleet)
  if (!all(Misc$Mode %in% c(0L, 1L, TRUE, FALSE), na.rm = TRUE))
    add_err("Hist@Misc$Mode: values must be 0/1 or logical (UseDensity flags)")
  
  flush_errors("1D vectors")
  
  # 2D Misc arrays
  
  # RelSize: (sim, area)  each row must sum to 1
  check_array(Misc$RelSize, "Hist@Misc$RelSize", c(nSim, nArea))
  check_values(Misc$RelSize, "Hist@Misc$RelSize", allow_neg = FALSE)
  if (is.numeric(Misc$RelSize) && !is.null(dim(Misc$RelSize))) {
    bad_rows <- which(abs(apply(Misc$RelSize, 1, sum) - 1.0) > 1e-6)
    if (length(bad_rows) > 0)
      add_err("Hist@Misc$RelSize: rows (across areas) must sum to 1; ",
              "bad sim row(s): ", paste(bad_rows, collapse = ","))
  }
  
  # SpawnTimeFrac: (sim, stock)  values in [0, 1]
  check_array(Misc$SpawnTimeFrac, "Hist@Misc$SpawnTimeFrac", c(nSim, nStock))
  check_values(Misc$SpawnTimeFrac, "Hist@Misc$SpawnTimeFrac",
               allow_neg = FALSE, range = c(0, 1))

  flush_errors("2D Misc arrays")
  
  # 3D Misc arrays 
  
  # SP0: (sim, stock, year)
  check_array(Misc$SP0, "Hist@Misc$SP0", c(nSim, nStock, nyears), Years)
  check_values(Misc$SP0, "Hist@Misc$SP0", allow_neg = FALSE)
  
  # R0: (sim, stock, year)
  check_array(Misc$R0, "Hist@Misc$R0", c(nSim, nStock, nyears), Years)
  check_values(Misc$R0, "Hist@Misc$R0", allow_neg = FALSE)
  
  # Spatial_Targeting: (sim, year, fleet)
  check_array(Misc$Spatial_Targeting, "Hist@Misc$Spatial_Targeting",
              c(nSim, nyears, nFleet), Years)
  check_values(Misc$Spatial_Targeting, "Hist@Misc$Spatial_Targeting",
               allow_neg = FALSE)
  
  flush_errors("3D Misc arrays")
  
  # 4D Misc arrays 
  
  # Catchability (q): (sim, stock, year, fleet)
  check_array(Misc$Catchability, "Hist@Misc$Catchability",
              c(nSim, nStock, nyears, nFleet), Years)
  check_values(Misc$Catchability, "Hist@Misc$Catchability", allow_neg = FALSE)
  
  # RecDist: (sim, stock, year, area)  area proportions must sum to 1
  check_array(Misc$RecDist, "Hist@Misc$RecDist",
              c(nSim, nStock, nyears, nArea), Years)
  check_values(Misc$RecDist, "Hist@Misc$RecDist", allow_neg = FALSE)
  if (is.numeric(Misc$RecDist) && length(dim(Misc$RecDist)) == 4L) {
    n_bad <- sum(abs(apply(Misc$RecDist, c(1, 2, 3), sum) - 1.0) > 1e-6)
    if (n_bad > 0)
      add_err("Hist@Misc$RecDist: area proportions must sum to 1 for each ",
              "(sim, stock, year); ", n_bad, " combination(s) out of range")
  }
  
  # StockTargeting: (sim, stock, fleet, year)  C++ indexes (sim_st, st, fl, y)
  if (isTRUE(Misc$StockTargetingFlag) || identical(Misc$StockTargetingFlag, 1)) {
    check_array(Misc$StockTargeting, "Hist@Misc$StockTargeting",
                c(nSim, nStock, nFleet, nyears), Years)
    check_values(Misc$StockTargeting, "Hist@Misc$StockTargeting",
                 allow_neg = FALSE)
    dn <- dimnames(Misc$StockTargeting)
    if (!is.null(dn) && !is.null(names(dn))) {
      expected_order <- c("Sim", "Stock", "Fleet", "Year")
      if (!identical(names(dn), expected_order))
        add_err("Hist@Misc$StockTargeting: dim name order is [",
                paste(names(dn), collapse = ", "), "]",
                "; C++ expects [", paste(expected_order, collapse = ", "), "]",
                " - check for transposed array")
    }
  }
  
  flush_errors("4D Misc arrays")
  
  # 5D Misc arrays 
  
  # Closure: (sim, stock, year, fleet, area)  values must be 0 or 1
  check_array(Misc$Closure, "Hist@Misc$Closure",
              c(nSim, nStock, nyears, nFleet, nArea), Years)
  if (is.numeric(Misc$Closure) &&
      !all(Misc$Closure %in% c(0, 1), na.rm = TRUE))
    add_err("Hist@Misc$Closure: expected 0/1 values (closure flags)")
  
  flush_errors("5D Misc arrays")
  
  # per-stock biological lists 
  
  for (lnm in c("LengthList", "WeightList", "NaturalMortalityList",
                "MaturityList", "SemelparousList", "FecundityList")) {
    check_stock_list(Misc[[lnm]], paste0("Hist@Misc$", lnm), function(arr, st) {
      nages <- nAge(Hist@OM@Stock[[st]])
      if (lnm == 'LengthList') {
        # Length-at-age is technically optional
        if (dim(arr)[2] == 1)
          nages <- 1
        if (dim(arr)[3] == 1)
          nyears <- 1
      }
      
      nm    <- paste0("Hist@Misc$", lnm, "[[", st, "]]")
      check_array(arr, nm, c(nSim, nages, nyears), Years)
    })
  }
  
  # NaturalMortality: non-negative
  check_stock_list(Misc$NaturalMortalityList, "Hist@Misc$NaturalMortalityList",
                   function(arr, st)
                     check_values(arr, paste0("Hist@Misc$NaturalMortalityList[[", st, "]]"),
                                  allow_neg = FALSE))
  
  # MovementList: (sim, fromArea, toArea, age, year)
  # from-area rows must sum to 1 across to-areas
  check_stock_list(Misc$MovementList, "Hist@Misc$MovementList", function(arr, st) {
    nages <- nAge(Hist@OM@Stock[[st]])
    nm    <- paste0("Hist@Misc$MovementList[[", st, "]]")
    check_array(arr, nm, c(nSim, nArea, nArea, nages, nyears), Years)
    check_values(arr, nm, allow_neg = FALSE)
    if (is.numeric(arr) && length(dim(arr)) == 5L && nArea > 1L) {
      n_bad <- sum(abs(apply(arr, c(1, 2, 4, 5), sum) - 1.0) > 1e-6)
      if (n_bad > 0)
        add_err(nm, ": from-area rows must sum to 1 across to-areas; ",
                n_bad, " (sim, from, age, year) combination(s) out of range")
    }
  })
  
  flush_errors("stock biological lists")
  
  # SRR 
  
  # SRR_Pars: list[nStock] of list[nPar] of 2D arrays (sim, year)
  check_stock_list(Misc$SRR_Pars, "Hist@Misc$SRR_Pars", function(pars_st, st) {
    nm_base <- paste0("Hist@Misc$SRR_Pars[[", st, "]]")
    if (!is.list(pars_st) || length(pars_st) < 1L) {
      add_err(nm_base, ": must be a non-empty list of parameter arrays")
      return(invisible(NULL))
    }
    for (p in seq_along(pars_st)) {
      arr <- pars_st[[p]]
      nm  <- paste0(nm_base, "[[", p, "]]")
      check_array(arr, nm, c(nSim, nyears))
      check_values(arr, nm)
    }
  })
  
  # RecDevs: list[nStock] of 2D (sim, year)
  # Year dim must be >= nyears + RecLag[st] because rec_y = y + lag in C++
  check_stock_list(Misc$RecDevs, "Hist@Misc$RecDevs", function(arr, st) {
    nm     <- paste0("Hist@Misc$RecDevs[[", st, "]]")
    lag    <- if (length(Misc$RecLag) >= st) Misc$RecLag[st] else 0L
    min_yr <- nyears + lag
    if (!is.numeric(arr) || is.null(dim(arr)) || length(dim(arr)) != 2L) {
      add_err(nm, ": must be a 2D numeric array (sim, year)")
      return(invisible(NULL))
    }
    # Sim dim: broadcast rule
    if (dim(arr)[1L] != 1L && dim(arr)[1L] != nSim)
      add_err(nm, ": dim[1] (Sim) = ", dim(arr)[1L],
              "; must be 1 (broadcast) or nSim (", nSim, ")")
    # Year dim: must cover nyears + lag (exact lower bound, not nSim broadcast)
    if (dim(arr)[2L] < min_yr)
      add_err(nm, ": dim[2] (year) = ", dim(arr)[2L],
              "; must be >= nyears + RecLag[", st, "] = ", min_yr,
              " (recruits appear at y + RecLag)")
    check_values(arr, nm, allow_neg = FALSE)
  })
  
  flush_errors("SRR")
  
  # fleet lists
  
  # WeightFleetRetainedList / WeightFleetSelectedList: list[nStock] of 4D (sim, age, year, fleet)
  check_stock_list(Misc$WeightFleetRetainedList, "Hist@Misc$WeightFleetRetainedList",
                   function(arr, st) {
                     nages <- nAge(Hist@OM@Stock[[st]])
                     nm    <- paste0("Hist@Misc$WeightFleetRetainedList[[", st, "]]")
                     check_array(arr, nm, c(nSim, nages, nyears, nFleet), Years)
                     check_values(arr, nm, allow_neg = FALSE)
                   })

  check_stock_list(Misc$WeightFleetSelectedList, "Hist@Misc$WeightFleetSelectedList",
                   function(arr, st) {
                     nages <- nAge(Hist@OM@Stock[[st]])
                     nm    <- paste0("Hist@Misc$WeightFleetSelectedList[[", st, "]]")
                     check_array(arr, nm, c(nSim, nages, nyears, nFleet), Years)
                     check_values(arr, nm, allow_neg = FALSE)
                   })
  
  # SelAgeList: list[nStock] of 5D (sim, age, year, fleet, area)  [0, 1]
  check_stock_list(Misc$SelAgeList, "Hist@Misc$SelAgeList", function(arr, st) {
    nages <- nAge(Hist@OM@Stock[[st]])
    nm    <- paste0("Hist@Misc$SelAgeList[[", st, "]]")
    check_array(arr, nm, c(nSim, nages, nyears, nFleet, nArea), Years)
    check_values(arr, nm, allow_neg = FALSE, range = c(0, 1))
  })
  
  # RetAgeList: list[nStock] of 5D (sim, age, year, fleet, area)  [0, 1]
  check_stock_list(Misc$RetAgeList, "Hist@Misc$RetAgeList", function(arr, st) {
    nages <- nAge(Hist@OM@Stock[[st]])
    nm    <- paste0("Hist@Misc$RetAgeList[[", st, "]]")
    check_array(arr, nm, c(nSim, nages, nyears, nFleet, nArea), Years)
    check_values(arr, nm, allow_neg = FALSE, range = c(0, 1))
  })
  
  # DiscMortList: list[nStock] of 5D (sim, age, year, fleet, area)  [0, 1]
  check_stock_list(Misc$DiscMortList, "Hist@Misc$DiscMortList", function(arr, st) {
    nages <- nAge(Hist@OM@Stock[[st]])
    nm    <- paste0("Hist@Misc$DiscMortList[[", st, "]]")
    check_array(arr, nm, c(nSim, nages, nyears, nFleet, nArea), Years)
    check_values(arr, nm, allow_neg = FALSE, range = c(0, 1))
  })
  
  # SelSizeList / RetSizeList / DiscMortSizeList:
  # list[nStock] of list[nFleet] of 4D (sim, class, year, area)
  
  # these aren't used in C++
  
  # for (lnm in c("SelSizeList", "RetSizeList", "DiscMortSizeList")) {
  #   check_stock_list(Misc[[lnm]], paste0("Hist@Misc$", lnm),
  #                    function(fleet_lst, st) {
  #                      nm_st <- paste0("Hist@Misc$", lnm, "[[", st, "]]")
  #                      if (!is.list(fleet_lst)) {
  #                        add_err(nm_st, ": must be a list (one per fleet), got ", class(fleet_lst))
  #                        return(invisible(NULL))
  #                      }
  #                      if (length(fleet_lst) != nFleet)
  #                        add_err(nm_st, ": length = ", length(fleet_lst),
  #                                ", expected nFleet = ", nFleet)
  #                      for (fl in seq_len(min(length(fleet_lst), nFleet))) {
  #                        arr <- fleet_lst[[fl]]
  #                        nm  <- paste0(nm_st, "[[", fl, "]]")
  #                        if (is.null(arr) || !is.numeric(arr)) {
  #                          add_err(nm, ": must be a numeric array"); next
  #                        }
  #                        nclass <- dim(arr)[2L]
  #                        check_array(arr, nm, c(nSim, nclass, nyears, nArea), Years)
  #                      }
  #                    })
  # }
  
  flush_errors("fleet lists")
  
  # Hist slots accessed directly by C++ 

  get_slot <- function(slot_name) {
    tryCatch(
      slot(Hist, slot_name),
      error = function(e) { add_err("Hist@", slot_name, ": slot missing"); NULL }
    )
  }
  
  # 3D output slots: (sim, stock, year)
  for (sn in c("Biomass", "SBiomass", "SProduction")) {
    arr <- get_slot(sn)
    if (!is.null(arr))
      check_array(arr, paste0("Hist@", sn), c(nSim, nStock, nyears), Years)
  }
  
  # Effort: (sim, year, fleet)  non-negative, no Inf
  Effort <- get_slot("Effort")
  if (!is.null(Effort)) {
    check_array(Effort, "Hist@Effort", c(nSim, nyears, nFleet), Years)
    check_values(Effort, "Hist@Effort", allow_neg = FALSE, allow_inf = FALSE)
  }
  
  # Distribution: (sim, year, fleet, area)
  Dist <- get_slot("Distribution")
  if (!is.null(Dist)) {
    check_array(Dist, "Hist@Distribution", c(nSim, nyears, nFleet, nArea), Years)
    if (nFleet > 1L && is.numeric(Dist) && length(dim(Dist)) == 4L) {
      n_nan <- sum(is.nan(Dist[, 1L, , , drop = FALSE]))
      if (n_nan > 0L)
        add_err("Hist@Distribution: ", n_nan, " NaN value(s) in year-1 slice;",
                " the competitor-fleet lag term reads Distribution[sim, y-1, fl, ar]",
                " when y > 0, so year 1 must be initialised (0 is fine, NaN is not)")
    }
  }
  
  # 4D output slots: (sim, stock, year, fleet)
  for (sn in c("Interactions", "Landings", "Discards",
               "FInteract", "FDead", "FRetain")) {
    arr <- get_slot(sn)
    if (!is.null(arr))
      check_array(arr, paste0("Hist@", sn), c(nSim, nStock, nyears, nFleet), Years)
  }
  
  # Number: list[nStock] of 4D (sim, age, year, area)
  Number_lst <- get_slot("Number")
  if (!is.null(Number_lst)) {
    if (!is.list(Number_lst) || length(Number_lst) != nStock) {
      add_err("Hist@Number: must be a list of length nStock = ", nStock,
              ", got ", if (is.list(Number_lst)) length(Number_lst) else class(Number_lst))
    } else {
      for (st in seq_len(nStock)) {
        arr   <- Number_lst[[st]]
        nm    <- paste0("Hist@Number[[", st, "]]")
        nages <- nAge(Hist@OM@Stock[[st]])
        check_array(arr, nm, c(nSim, nages, nyears, nArea), Years)
        check_values(arr, nm, allow_neg = FALSE)
      }
    }
  }
  
  # Per-stock 5D slots: (sim, age, year, fleet, area)
  for (sn in c("FInteractArea", "FDeadArea", "FRetainArea",
               "InteractAtAge", "LandingsAtAge", "DiscardsAtAge")) {
    lst <- get_slot(sn)
    if (!is.null(lst)) {
      if (!is.list(lst) || length(lst) != nStock) {
        add_err("Hist@", sn, ": must be a list of length nStock = ", nStock)
      } else {
        for (st in seq_len(nStock)) {
          arr   <- lst[[st]]
          nm    <- paste0("Hist@", sn, "[[", st, "]]")
          nages <- nAge(Hist@OM@Stock[[st]])
          check_array(arr, nm, c(nSim, nages, nyears, nFleet, nArea), Years)
        }
      }
    }
  }
  
  # LandingsAtSize / DiscardsAtSize: list[nStock] of list[nFleet] of 4D
  for (sn in c("LandingsAtSize", "DiscardsAtSize")) {
    lst <- get_slot(sn)
    if (!is.null(lst)) {
      if (!is.list(lst) || length(lst) != nStock) {
        add_err("Hist@", sn, ": must be a list of length nStock = ", nStock)
      } else {
        for (st in seq_len(nStock)) {
          fl_lst <- lst[[st]]
          nm_st  <- paste0("Hist@", sn, "[[", st, "]]")
          if (!is.list(fl_lst) || length(fl_lst) != nFleet) {
            add_err(nm_st, ": must be a list of length nFleet = ", nFleet)
          } else {
            for (fl in seq_len(nFleet)) {
              arr <- fl_lst[[fl]]
              nm  <- paste0(nm_st, "[[", fl, "]]")
              if (!is.null(arr) && is.numeric(arr) && !is.null(dim(arr))) {
                d1 <- dim(arr)[1L]
                if (d1 != 1L && d1 != nSim)
                  add_err(nm, ": dim[1] (Sim) = ", d1,
                          "; must be 1 (broadcast) or nSim (", nSim, ")")
              }
            }
          }
        }
      }
    }
  }
  
  flush_errors("Hist slots")
  
  # cross-array consistency 
  
  # Age dimension (dim[2]) must be consistent across all arrays for each stock
  for (st in seq_len(nStock)) {
    nages_stock <- nAge(Hist@OM@Stock[[st]])
    age_arrays <- list(
      WeightList           = Misc$WeightList[[st]],
      NaturalMortalityList = Misc$NaturalMortalityList[[st]],
      MaturityList         = Misc$MaturityList[[st]],
      SemelparousList      = Misc$SemelparousList[[st]],
      FecundityList        = Misc$FecundityList[[st]],
      WeightFleetRetainedList = Misc$WeightFleetRetainedList[[st]],
      WeightFleetSelectedList = Misc$WeightFleetSelectedList[[st]],
      SelAgeList           = Misc$SelAgeList[[st]],
      RetAgeList           = Misc$RetAgeList[[st]],
      DiscMortList         = Misc$DiscMortList[[st]]
    )
    if (!is.null(Number_lst) && is.list(Number_lst) && length(Number_lst) >= st)
      age_arrays[["Number"]] <- Number_lst[[st]]
    
    for (anm in names(age_arrays)) {
      arr <- age_arrays[[anm]]
      if (!is.null(arr) && !is.null(dim(arr)) && length(dim(arr)) >= 2L) {
        got <- dim(arr)[2L]
        if (!is.na(got) && got != nages_stock)
          add_err("Stock ", st, ": age dimension mismatch in ", anm,
                  ": dim[2] = ", got, ", expected nAge = ", nages_stock)
      }
    }
  }
  
  flush_errors("cross-array consistency")
  
  invisible(TRUE)
}
