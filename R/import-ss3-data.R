#' Import SS3 data into a Data object
#'
#' Reads a parsed SS3 report and populates a [data-class] object with catch,
#' index, and composition data.
#'
#' Compositions of fishing fleets go to `LandingsAtAge`, `DiscardsAtAge`,
#' `LandingsAtSize`, and `DiscardsAtSize`, and compositions of survey fleets
#' to `SurveyAtAge` and `SurveyAtSize`, in the SS3 data bins (the age and
#' length bins of the data file, not the population bins; see the Bins
#' section of [CompData()]). For a two-sex model, compositions
#' reported by sex are stored in a list named by stock (see [data-class]):
#' SS3 `Sexes = 0` compositions are held under the complex name, `Sexes = 1`
#' and `2` under the female or male stock, and `Sexes = 3` under both stocks
#' with the fleet `Joint`.
#'
#' @param SSDir Character vector of SS3 report directories, or a pre-parsed
#'   `r4ss::SS_output` list or `RepList` object returned by [ImportSSReport()].
#' @param Name Character; name for the `Data` object
#' @param CommonName Character; common name of the species
#' @param Species Character; scientific name of the species
#' @param LengthUnits Character string giving the physical unit of length
#'   composition bins (e.g., `"cm"`, `"mm"`, `"inch"`; see [ValidUnits()]).
#'   Not reported by SS3/`r4ss`; `"cm"` is the near-universal SS3 convention.
#'   Default `"cm"`.
#' @param CatchUnits Character. `'SS3'` (default) imports the catch of each
#'   fleet in its SS3 catch units; `'Biomass'` imports the catch of every
#'   fleet in biomass. See [ImportSS()].
#' @param FleetNames Optional character vector of names for the SS3 fishing
#'   fleets, in SS3 fleet order, used for their catch, CPUE, and composition
#'   data. If `NULL`, the SS3 fleet names are used.
#' @param StockName Optional character vector of stock names (see
#'   [ImportSS()]), used to name sex-specific composition data. If `NULL`,
#'   `"Female"` and `"Male"` for a two-sex model.
#' @param silent Logical; suppress progress messages (default FALSE)
#' @param ... Additional arguments passed to `ImportSSReport`
#'
#' @return A populated [Data()] object
#' @export
ImportSSData <- function(SSDir,
                         Name       = "Imported by ImportSSData",
                         CommonName = "",
                         Species    = "",
                         LengthUnits = "cm",
                         CatchUnits = c('SS3', 'Biomass'),
                         FleetNames = NULL,
                         StockName  = NULL,
                         silent     = FALSE,
                         ...) {
  .OnExit()
  CatchUnits <- match.arg(CatchUnits)
  RepList   <- ImportSSReport(SSDir, silent = silent, ...)
  replist   <- RepList[[1]]
  nStock    <- replist$nsexes
  YearsList <- .GetSSYears(RepList[[1]], 1)
  
  # Create Data object
  Data            <- Data(Name = Name)
  Data@Name       <- Name
  Data@CommonName <- CommonName
  Data@Species    <- Species
  
  # TODO: Data@Agency, @Author, @Email, @Region, @Latitude, @Longitude
  #       Data@LifeHistory, Data@Exploitation
  
  Data@Years   <- .GetSSDataYears(replist, YearsList)
  Data@YearLH  <- YearsList$CurrentYear
  Data@Seasons <- YearsList$Seasons
  Data@nArea   <- 1
  
  FleetNames <- .ProcessSSFleetNames(FleetNames, .SSFishFleets(replist)$SSName)
  Landings_Discards <- .ImportSSDataCatch(replist, silent, CatchUnits = CatchUnits, FleetNames = FleetNames)
  Data@Landings     <- Landings_Discards$Landings
  Data@Discards     <- Landings_Discards$Discards
  
  Data@CPUE   <- .ImportSSDataIndex(replist, "CPUE", FleetNames = FleetNames)
  Data@Survey <- .ImportSSDataIndex(replist, "Survey")
  
  StockName <- .ProcessSSStockName(StockName, nStock)
  AtAge  <- .ImportSSDataComp(replist, "Age", FleetNames, StockName = StockName)
  AtSize <- .ImportSSDataComp(replist, "Size", FleetNames, LengthUnits, StockName = StockName)
  Data@LandingsAtAge  <- AtAge$Landings
  Data@DiscardsAtAge  <- AtAge$Discards
  Data@SurveyAtAge    <- AtAge$Survey
  Data@LandingsAtSize <- AtSize$Landings
  Data@DiscardsAtSize <- AtSize$Discards
  Data@SurveyAtSize   <- AtSize$Survey
  
  Data
}

.ImportSSDataCatch <- function(replist, silent = FALSE, CatchUnits = 'SS3', FleetNames = NULL) {
  
  dead_bio <- dead_num <- kill_bio <- kill_num <- ret_bio <- ret_num <- NULL
  
  YearsList <- .GetSSYears(replist, pYear = 1)
  YearsHist <- YearsList$YearsHist
  nTS <- length(YearsHist)
  FishFleets <- .SSFishFleets(replist)$SS
  FleetNames <- FleetNames %||% .SSFishFleets(replist)$SSName
  nFleet <- length(FleetNames)
  
  # Landings, Dead Discards
  Landings <- new("catchdata")
  Landings@Name <- FleetNames
  Landings@Value <- Landings@CV <- array(NA_real_,
                                         dim = c(nTS, nFleet),
                                         dimnames = list(
                                           Year = YearsHist,
                                           Fleet = FleetNames
                                         )
  )
  Landings@Units <- sapply(replist$catch_units[replist$IsFishFleet], function(x) {
    switch(x,
           "1" = "Biomass",
           "2" = "Number"
    )
  })
  if (CatchUnits == 'Biomass')
    Landings@Units[] <- 'Biomass'

  Discards <- Landings
  Discards@CV <- .SSDiscardCV(replist, YearsHist, FishFleets, FleetNames)
  
  CatchColNames <- names(replist$catch)
  CatchDF <- replist$catch |> dplyr::filter(Yr %in% YearsHist)
  
  if (length(unique(CatchDF$Area)) > 1) {
    cli::cli_abort("`ImportSSData` currently does not support multiple areas")
  }
  
  # add missing column names
  if (!"kill_bio" %in% CatchColNames) {
    CatchDF <- CatchDF |> dplyr::mutate(kill_bio = dead_bio)
  }
  
  if (!"kill_num" %in% CatchColNames) {
    CatchDF <- CatchDF |> dplyr::mutate(kill_num = dead_num)
  }
  
  if (!"ret_bio" %in% CatchColNames) {
    cli::cli_abort("'ret_bio' not found in `replist$catch` for this SS3 output")
  }
  
  # Loop over fleets
  for (fl in seq_along(FleetNames)) {
    units <- Landings@Units[fl]
    
    if (units == "Biomass") {
      Dead <- CatchDF |>
        dplyr::filter(Fleet == FishFleets[fl]) |>
        dplyr::select(Year = Yr, Obs = kill_bio)
    } else if (units == "Number") {
      Dead <- CatchDF |>
        dplyr::filter(Fleet == FishFleets[fl]) |>
        dplyr::select(Year = Yr, Obs = kill_num)
    } else {
      cli::cli_alert_warning("Units {.val {units}} not supported for landings & discards")
    }
    
    if (units == "Biomass") {
      Retain <- CatchDF |>
        dplyr::filter(Fleet == FishFleets[fl]) |>
        dplyr::select(Year = Yr, Obs = ret_bio)
    } else if (units == "Number") {
      Retain <- CatchDF |>
        dplyr::filter(Fleet == FishFleets[fl]) |>
        dplyr::select(Year = Yr, Obs = ret_num)
    } else {
      cli::cli_alert_warning("Units {.val {units}} not supported for landings & discards")
    }
    
    
    Landings@Value[, fl] <- Retain$Obs
    Discards@Value[, fl] <- Dead$Obs - Retain$Obs

    if ("se" %in% CatchColNames) {
      FleetCatch <- CatchDF |> dplyr::filter(Fleet == FishFleets[fl])
      Landings@CV[, fl] <- .SSFillTimeSteps(.SSLogSE2CV(FleetCatch$se),
                                            FleetCatch$Yr, FleetCatch$Seas, YearsHist)
    }
  }
  list(
    Landings = Landings,
    Discards = Discards
  )
}


# CV of a lognormal observation from its SS3 log-scale SE; NA where SS3 gives no SE (missing or 0)
.SSLogSE2CV <- function(se) {
  se  <- suppressWarnings(as.numeric(se))
  out <- sqrt(exp(se^2) - 1)
  out[!is.finite(out) | se <= 0] <- NA_real_
  out
}

# length(YearsHist) vector of `x` placed at the time steps of SS3 rows `Yr`, `Seas`; NA elsewhere
.SSFillTimeSteps <- function(x, Yr, Seas, YearsHist) {
  out <- rep(NA_real_, length(YearsHist))
  ind <- match(Yr, YearsHist) + Seas - 1
  ok  <- !is.na(ind) & ind >= 1 & ind <= length(YearsHist)
  out[ind[ok]] <- x[ok]
  out
}

# [nTS x nFleet] CV of the SS3 discard observations, from the SD used in the fit (`Std_use`) and the fleet's discard error type
.SSDiscardCV <- function(replist, YearsHist, FishFleets, FleetNames) {
  CV <- array(NA_real_, dim = c(length(YearsHist), length(FleetNames)),
              dimnames = list(Year = YearsHist, Fleet = FleetNames))
  Disc <- replist$discard
  if (!is.data.frame(Disc) || !nrow(Disc) || !all(c("Fleet", "Yr", "Obs") %in% names(Disc)) ||
      !any(c("Std_use", "Std_in") %in% names(Disc)))
    return(CV)
  if (!"Seas" %in% names(Disc)) Disc$Seas <- 1
  Num  <- function(x) suppressWarnings(as.numeric(x))
  Spec <- replist$discard_spec
  for (fl in seq_along(FishFleets)) {
    Rows <- Disc[Num(Disc$Fleet) %in% FishFleets[fl], , drop = FALSE]
    if (!nrow(Rows)) next
    ErrType <- if (is.data.frame(Spec) && "errtype" %in% names(Spec))
      Spec$errtype[match(FishFleets[fl], Num(Spec$Fleet))] else replist$DF_discard
    ErrType <- Num(ErrType)
    if (!length(ErrType) || is.na(ErrType)) ErrType <- 0
    Obs <- Num(Rows$Obs)
    # SD in log space for lognormal (-2), natural space otherwise; Std_in is a CV for errtype >= 0 and -3
    SD <- if ("Std_use" %in% names(Rows)) Num(Rows$Std_use) else
      if (ErrType >= 0 || ErrType == -3) Num(Rows$Std_in) * Obs else Num(Rows$Std_in)
    cv <- if (ErrType == -2) .SSLogSE2CV(SD) else SD / Obs
    if ("Use" %in% names(Rows)) cv[Num(Rows$Use) < 0] <- NA_real_
    cv[!is.finite(cv) | cv <= 0] <- NA_real_
    CV[, fl] <- .SSFillTimeSteps(cv, Num(Rows$Yr), Num(Rows$Seas), YearsHist)
  }
  CV
}

# TRUE for the dummy observations of an SS3 super-period (value ignored by SS3, SE field holds a weight)
.SSSuperPeriodDummy <- function(x) {
  if (!all(c("SuprPer", "Use") %in% names(x))) return(rep(FALSE, nrow(x)))
  Dummy <- !is.na(x$SuprPer) & !x$SuprPer %in% c("_", "No") & suppressWarnings(as.numeric(x$Use)) < 0
  Dummy %in% TRUE
}

# CV of SS3 index observations; `SE` is log-scale for lognormal and T errors, natural-scale for normal (errtype < 0);
# NA for super-period dummy observations
.SSIndexCV <- function(cpue, replist) {
  Num <- function(x) suppressWarnings(as.numeric(x))
  ErrType <- Num(replist$survey_error %||% 0)[cpue$Fleet]
  SE  <- Num(cpue$SE)
  cv  <- .SSLogSE2CV(SE)
  Normal <- !is.na(ErrType) & ErrType < 0
  cv[Normal] <- SE[Normal] / Num(cpue$Obs)[Normal]
  cv[.SSSuperPeriodDummy(cpue)] <- NA_real_
  cv[!is.finite(cv) | cv <= 0] <- NA_real_
  cv
}

# Time steps of the SS3 data: the historical time steps, extended past endyr
# through the last year with a finite index or composition observation (SS3
# reports observations in forecast years with Use = -1). `YearLH` stays at
# endyr, so these years are observed data for the projection period; the
# projection uses them in place of simulated data (see `.OMDataRow()`).
# Catch is not extended: SS3 reports forecast catch in `timeseries`, not as
# observations.
.GetSSDataYears <- function(replist, YearsList) {
  YearsHist <- YearsList$YearsHist
  ObsYr <- purrr::map(c('cpue', 'agedbase', 'lendbase'), \(tb) {
    x <- replist[[tb]]
    if (!is.data.frame(x) || !all(c('Yr', 'Obs') %in% names(x))) return(NULL)
    x$Yr[is.finite(x$Obs)]
  }) |> unlist()
  ObsYr <- ObsYr[is.finite(ObsYr)]
  if (!length(ObsYr) || max(ObsYr) <= YearsList$CurrentYear)
    return(YearsHist)
  nExtra <- max(ObsYr) - YearsList$CurrentYear
  c(YearsHist, CalcYears(YearsList$nYear, nExtra, YearsList$CurrentYear,
                         YearsList$Seasons, "Projection"))
}

.ImportSSDataIndex <- function(replist, Type = c("CPUE", "Survey"), FleetNames = NULL) {
  Type <- match.arg(Type, c("CPUE", "Survey"))
  
  YearsList <- .GetSSYears(replist, pYear = 1)
  DataYears <- .GetSSDataYears(replist, YearsList)
  nTS <- length(DataYears)
  
  Indices <- new("indicesdata")
  CPUE <- replist$cpue
  
  if (!nrow(CPUE)) {
    return(Indices)
  }
  
  if (Type == "CPUE") {
    IndFleets <- which(replist$IsFishFleet)
  } else {
    IndFleets <- which(!replist$IsFishFleet)
  }
  
  if (length(IndFleets) < 1) {
    return(Indices)
  }
  
  CPUE <- CPUE |> dplyr::filter(Fleet %in% IndFleets)

  # drop observations outside the data time steps (e.g. before startyr, or
  # a non-finite observation after endyr)
  TSInd <- match(CPUE$Yr, DataYears) + CPUE$Seas - 1
  CPUE  <- CPUE[!is.na(TSInd) & TSInd >= 1 & TSInd <= nTS, , drop = FALSE]

  if (!nrow(CPUE)) {
    return(Indices)
  }
  
  CPUE_Ind <- CPUE$Fleet |> unique() |> sort()
  
  CPUE_Split <- CPUE |>
    dplyr::group_by(Fleet) |>
    dplyr::group_split()
  
  CPUENames <- purrr::map(CPUE_Split, \(cpue) {
    out <- unique(cpue$Fleet_name)
    ifelse(length(out) == 1, out, NA_character_)
    out
  }) |>
    unlist() |>
    as.character()
  # CPUE of a fishing fleet takes that fleet's (possibly user-supplied) name
  if (Type == "CPUE" && !is.null(FleetNames))
    CPUENames <- FleetNames[match(CPUE_Ind, .SSFishFleets(replist)$SS)]
  
  Indices@Name <- CPUENames
  names(CPUE_Split) <- CPUENames
  nIndex <- length(CPUENames)
  
  Value <- purrr::imap(CPUE_Split, \(cpue, idx) {
    ind <- match(cpue$Yr, DataYears) + cpue$Seas - 1
    Years <- DataYears[ind]
    index <- array(ifelse(.SSSuperPeriodDummy(cpue), NA_real_, cpue$Obs), c(length(Years), 1),
                   dimnames = list(
                     Year = Years,
                     Fleet = idx
                   )
    )
    index
  })
  
  CV <- purrr::imap(CPUE_Split, \(cpue, idx) {
    ind <- match(cpue$Yr, DataYears) + cpue$Seas - 1
    Years <- DataYears[ind]
    index <- array(.SSIndexCV(cpue, replist), c(length(Years), 1),
                   dimnames = list(
                     Year = Years,
                     Fleet = idx
                   )
    )
    index
  })
  
  Indices@Value <- Indices@CV <- array(NA,
                                       dim = c(nTS, nIndex),
                                       dimnames = list(
                                         Year = DataYears,
                                         Fleet = as.character(CPUENames)
                                       )
  )
  
  Indices@Timing <- purrr::map2_dbl(CPUE_Split, CPUENames, \(cpue, nm) .SSIndexTiming(cpue, replist, nm))
  
  for (i in seq_along(Value)) {
    ArrayFill(Indices@Value) <- Value[[i]]
  }
  for (i in seq_along(CV)) {
    ArrayFill(Indices@CV) <- CV[[i]]
  }
  
  # https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#surveys-and-indices
  Indices@Units <- sapply(replist$survey_units[CPUE_Ind], function(x) {
    switch(as.character(x),
           "0" = "Number",
           "1" = "Biomass",
           "2" = "F",
           "30" = "Biomass", # spawning production: SBiomass selectivity set on Obs
           "31" = "Expected Recruitment Deviation",
           "32" = "Spawning Production * exp(recruitment deviation)",
           "33" = "Recruitment",
           "34" = "Depletion (spawning biomass/virgin spawning biomass)",
           "35" = "Survey of a Deviation Vector",
           "36" = "Recruitment Deviation"
    )
  }) |>
    unlist()
  
  if (length(Indices@Units) != length(CPUENames)) {
    cli::cli_abort(c(
      "x" = "CPUE/Survey units do not match fleets"
    ), .internal = TRUE)
  }
  Indices
}

# timing of an SS3 index within its time step, from the observation month
.SSIndexTiming <- function(cpue, replist, Name) {
  if (!'Month' %in% names(cpue)) return(0)
  SeasDur   <- (replist$seasdurations %||% rep(1 / replist$nseasons, replist$nseasons)) * 12
  SeasStart <- 1 + c(0, cumsum(SeasDur))[seq_along(SeasDur)]
  Timing <- (cpue$Month - SeasStart[cpue$Seas]) / SeasDur[cpue$Seas]
  Timing <- pmin(pmax(Timing, 0), 1)
  if (length(unique(round(Timing, 3))) > 1)
    cli::cli_alert_info("SS3 index {.val {Name}} is observed at different times within the time step; using the median timing ({round(stats::median(Timing), 3)}).")
  stats::median(Timing)
}


# SS3 data bins (lower bounds) of age or length compositions; the population
# classes when the report has none
.GetSSDataBins <- function(replist, isAge) {
  Bins <- as.numeric(replist[[if (isAge) "agebins" else "lbins"]])
  if (length(Bins) && all(is.finite(Bins)))
    return(Bins)
  if (isAge) .GetSSAgeClasses(replist) else .GetSSLengthClasses(replist)
}

# Age or length compositions of SS3 fishing fleets (`Landings`, `Discards`) and
# survey fleets (`Survey`). Each slot is a `compdata`, or a list named by the
# complex and/or stocks when SS3 reports sex-specific compositions: `Sexes = 0`
# goes to the complex element, `1` and `2` to that sex's stock, and `3` to both
# stocks with the fleet `Joint`.
.ImportSSDataComp <- function(replist, Type = c("Age", "Size"), FleetNames = NULL,
                              LengthUnits = "cm", StockName = NULL, ComplexName = NULL) {
  Type      <- match.arg(Type)
  isAge     <- Type == "Age"
  Empty     <- list(Landings = new("compdata"), Discards = new("compdata"), Survey = new("compdata"))
  DB        <- replist[[if (isAge) "agedbase" else "lendbase"]]
  if (is.null(DB) || !nrow(DB))
    return(Empty)

  StockName   <- StockName %||% .ProcessSSStockName(NULL, replist$nsexes)
  ComplexName <- ComplexName %||% paste(StockName, collapse = " ")
  DataYears   <- .GetSSDataYears(replist, .GetSSYears(replist, pYear = 1))
  nTS         <- length(DataYears)
  Classes     <- .GetSSDataBins(replist, isAge)
  nClass      <- length(Classes)
  Fish        <- .SSFishFleets(replist)
  FleetNames  <- FleetNames %||% Fish$SSName
  SurveyFl    <- which(!replist$IsFishFleet)

  nsamp_in_valid <- !is.null(DB$Nsamp_in) && any(DB$Nsamp_in > 1, na.rm = TRUE)
  DB$Count   <- DB$Obs * (if (nsamp_in_valid) DB$Nsamp_in else DB$Nsamp_adj)
  Sexed      <- replist$nsexes > 1 && !is.null(DB$Sexes)
  DB$Element <- if (Sexed) ifelse(DB$Sexes == 0, ComplexName, StockName[DB$Sex]) else ComplexName
  DB$Joint   <- Sexed & DB$Sexes == 3
  DB$TS      <- match(DB$Yr, DataYears) + DB$Seas - 1
  DB$Class   <- findInterval(DB$Bin + 1e-8, Classes)
  DB <- DB[!is.na(DB$TS) & DB$TS >= 1 & DB$TS <= nTS & !is.na(DB$Class) & DB$Class >= 1, , drop = FALSE]

  CompMatrix <- function(df) {
    mat <- matrix(NA_real_, nTS, nClass)
    if (!nrow(df)) return(mat)
    agg <- stats::aggregate(Count ~ TS + Class, data = df, FUN = sum)
    mat[cbind(agg$TS, agg$Class)] <- agg$Count
    mat
  }

  # rows of each fleet in each slot
  Rows <- list(Landings = list(), Discards = list(), Survey = list())
  for (fl in seq_len(nrow(Fish))) {
    db    <- DB[DB$Fleet == Fish$SS[fl], , drop = FALSE]
    Parts <- unique(db$Part)
    Rows$Landings[[FleetNames[fl]]] <- db[db$Part == (if (any(Parts %in% c(0, 1))) 0 else 2), , drop = FALSE]
    Rows$Discards[[FleetNames[fl]]] <- db[db$Part == 1, , drop = FALSE]
  }
  for (fl in SurveyFl)
    Rows$Survey[[replist$FleetNames[fl]]] <- DB[DB$Fleet == fl, , drop = FALSE]

  purrr::imap(Rows, \(ByFleet, Slot) {
    Elements <- unique(unlist(purrr::map(ByFleet, \(db) db$Element)))
    if (!length(Elements)) return(new("compdata"))
    Out <- purrr::map(Elements, \(el) {
      Names <- names(ByFleet)[purrr::map_lgl(ByFleet, \(db) any(db$Element == el))]
      if (el == ComplexName && Slot != "Survey") Names <- names(ByFleet)
      Value <- array(NA_real_, c(nTS, length(Names), nClass),
                     stats::setNames(list(DataYears, Names, Classes),
                                     c("Year", "Fleet", if (isAge) "Age" else "Class")))
      Joint <- logical(0)
      for (nm in Names) {
        db <- ByFleet[[nm]][ByFleet[[nm]]$Element == el, , drop = FALSE]
        Value[, nm, ] <- CompMatrix(db)
        if (any(db$Joint)) Joint[nm] <- TRUE
      }
      CompData <- new("compdata")
      CompData@Name    <- Names
      CompData@Value   <- Value
      CompData@Classes <- if (isAge) as.numeric(Classes) else
        stats::setNames(rep(list(as.numeric(Classes)), length(Names)), Names)
      CompData@Units   <- if (isAge) "years" else LengthUnits
      CompData@Joint   <- Joint
      CompData
    }) |> stats::setNames(Elements)
    .ElementsToSlot(Out, ComplexName)
  })
}
