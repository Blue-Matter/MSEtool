#' Import SS3 data into a Data object
#'
#' Reads a parsed SS3 report and populates a [data-class] object with catch,
#' index, and composition data.
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
  
  Data@Years   <- YearsList$YearsHist
  Data@YearLH  <- YearsList$CurrentYear
  Data@Seasons <- YearsList$Seasons
  Data@nArea   <- 1
  
  FleetNames <- .ProcessSSFleetNames(FleetNames, .SSFishFleets(replist)$SSName)
  Landings_Discards <- .ImportSSDataCatch(replist, silent, CatchUnits = CatchUnits, FleetNames = FleetNames)
  Data@Landings     <- Landings_Discards$Landings
  Data@Discards     <- Landings_Discards$Discards
  
  Data@CPUE   <- .ImportSSDataIndex(replist, "CPUE", FleetNames = FleetNames)
  Data@Survey <- .ImportSSDataIndex(replist, "Survey")
  
  AtAge              <- .ImportSSDataAtAge(replist, silent, FleetNames = FleetNames)
  Data@LandingsAtAge <- AtAge$Landings
  Data@DiscardsAtAge <- AtAge$Discards
  
  AtSize              <- .ImportSSDataAtSize(replist, silent, LengthUnits = LengthUnits, FleetNames = FleetNames)
  Data@LandingsAtSize <- AtSize$Landings
  Data@DiscardsAtSize <- AtSize$Discards
  
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

.ImportSSDataIndex <- function(replist, Type = c("CPUE", "Survey"), FleetNames = NULL) {
  Type <- match.arg(Type, c("CPUE", "Survey"))
  
  YearsList <- .GetSSYears(replist, pYear = 1)
  YearsHist <- YearsList$YearsHist
  nTS <- length(YearsHist)
  
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
    ind <- match(cpue$Yr, YearsHist) + cpue$Seas - 1
    Years <- YearsHist[ind]
    index <- array(ifelse(.SSSuperPeriodDummy(cpue), NA_real_, cpue$Obs), c(length(Years), 1),
                   dimnames = list(
                     Year = Years,
                     Fleet = idx
                   )
    )
    index
  })
  
  CV <- purrr::imap(CPUE_Split, \(cpue, idx) {
    ind <- match(cpue$Yr, YearsHist) + cpue$Seas - 1
    Years <- YearsHist[ind]
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
                                         Year = YearsHist,
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


.ImportSSDataAtAge <- function(replist, silent = FALSE, FleetNames = NULL) {

  YearsList     <- .GetSSYears(replist, pYear = 1)
  YearsHist     <- YearsList$YearsHist
  nTS           <- length(YearsHist)
  FishFleets    <- .SSFishFleets(replist)$SS
  FleetNames    <- FleetNames %||% .SSFishFleets(replist)$SSName
  nFleet        <- length(FishFleets)
  AgeClasses    <- .GetSSAgeClasses(replist)
  nAge          <- length(AgeClasses)

  AgeDB <- replist$agebase

  if (is.null(AgeDB) || !nrow(AgeDB))
    return(list(Landings = new("compdata"), Discards = new("compdata")))

  nsamp_in_valid <- !is.null(AgeDB$Nsamp_in) &&
    any(AgeDB$Nsamp_in > 1, na.rm = TRUE)

  AgeDB$N     <- if (nsamp_in_valid) AgeDB$Nsamp_in else AgeDB$Nsamp_adj
  AgeDB$Count <- AgeDB$Obs * AgeDB$N

  make_age_matrix <- function(df) {
    mat <- matrix(
      NA_real_,
      nrow     = nTS,
      ncol     = nAge,
      dimnames = list(Year = YearsHist, Age = AgeClasses)
    )
    if (!nrow(df)) return(mat)

    agg <- df |>
      dplyr::summarise(Count = sum(Count), .by = c(Yr, Seas, Bin))

    yr_ind <- match(agg$Yr, YearsHist) + agg$Seas - 1
    yr_ind[!is.na(yr_ind) & (yr_ind < 1 | yr_ind > nTS)] <- NA

    age_ind <- match(agg$Bin, AgeClasses)

    valid <- !is.na(yr_ind) & !is.na(age_ind)

    mat[cbind(yr_ind[valid], age_ind[valid])] <- agg$Count[valid]
    mat
  }

  LandingsArr <- DiscardsArr <- array(
    NA_real_,
    dim      = c(nTS, nFleet, nAge),
    dimnames = list(
      Year   = YearsHist,
      Fleet  = FleetNames,
      Age    = AgeClasses
    )
  )

  for (fl in seq_along(FishFleets)) {
    fleet_idx <- FishFleets[fl]

    fleet_db <- AgeDB |>
      dplyr::filter(Fleet == fleet_idx, Yr %in% YearsHist)

    if (!nrow(fleet_db)) next

    parts <- unique(fleet_db$Part)

    if (any(parts %in% c(0, 1))) {
      LandingsArr[, fl, ] <- make_age_matrix(
        df = dplyr::filter(fleet_db, Part == 0)
      )
      DiscardsArr[, fl, ] <- make_age_matrix(
        dplyr::filter(fleet_db, Part == 1)
      )
    } else if (any(parts == 2)) {
      LandingsArr[, fl, ] <- make_age_matrix(
        dplyr::filter(fleet_db, Part == 2)
      )
    }
  }

  Landings         <- new("compdata")
  Landings@Name    <- FleetNames
  Landings@Value   <- LandingsArr
  Landings@Classes <- as.numeric(AgeClasses)
  Landings@Units   <- "years"

  Discards         <- new("compdata")
  Discards@Name    <- FleetNames
  Discards@Value   <- DiscardsArr
  Discards@Classes <- as.numeric(AgeClasses)
  Discards@Units   <- "years"

  list(Landings = Landings, Discards = Discards)
}


.ImportSSDataAtSize <- function(replist, silent = FALSE, LengthUnits = "cm", FleetNames = NULL) {
  
  YearsList     <- .GetSSYears(replist, pYear = 1)
  YearsHist     <- YearsList$YearsHist
  nTS           <- length(YearsHist)
  FishFleets    <- .SSFishFleets(replist)$SS
  FleetNames    <- FleetNames %||% .SSFishFleets(replist)$SSName
  nFleet        <- length(FishFleets)
  LengthClasses <- .GetSSLengthClasses(replist) 
  nLength       <- length(LengthClasses)
  
  LenDB <- replist$lendbase
  
  if (is.null(LenDB) || !nrow(LenDB))
    return(list(Landings = new("compdata"), Discards = new("compdata")))
  
  nsamp_in_valid <- !is.null(LenDB$Nsamp_in) && 
    any(LenDB$Nsamp_in > 1, na.rm = TRUE)
  
  LenDB$N     <- if (nsamp_in_valid) LenDB$Nsamp_in else LenDB$Nsamp_adj
  LenDB$Count <- LenDB$Obs * LenDB$N
  LenDB$BinMid <- LenDB$Bin

  make_length_matrix <- function(df) {
    mat <- matrix(
      NA_real_,
      nrow     = nTS,
      ncol     = nLength,
      dimnames = list(Year = YearsHist, Class = LengthClasses)
    )
    if (!nrow(df)) return(mat)

    agg <- df |>
      dplyr::summarise(Count = sum(Count), .by = c(Yr, Seas, BinMid))

    yr_ind <- match(agg$Yr, YearsHist) + agg$Seas - 1
    yr_ind[!is.na(yr_ind) & (yr_ind < 1 | yr_ind > nTS)] <- NA

    bin_ind <- findInterval(agg$BinMid, LengthClasses)
    bin_ind[bin_ind < 1 | bin_ind > nLength] <- NA

    valid <- !is.na(yr_ind) & !is.na(bin_ind)

    mat[cbind(yr_ind[valid], bin_ind[valid])] <- agg$Count[valid]
    mat
  }
  
  LandingsArr <- DiscardsArr <- array(
    NA_real_,
    dim      = c(nTS, nFleet, nLength),
    dimnames = list(
      Year   = YearsHist,
      Fleet  = FleetNames,
      Class = LengthClasses
    )
  )
    
  for (fl in seq_along(FishFleets)) {
    fleet_idx <- FishFleets[fl]
    
    fleet_db <- LenDB |>
      dplyr::filter(Fleet == fleet_idx, Yr %in% YearsHist)
    
    if (!nrow(fleet_db)) next
    
    parts <- unique(fleet_db$Part)
    
    if (any(parts %in% c(0, 1))) {
      LandingsArr[, fl, ] <- make_length_matrix(
        df = dplyr::filter(fleet_db, Part == 0)
      )
      DiscardsArr[, fl, ] <- make_length_matrix(
        dplyr::filter(fleet_db, Part == 1)
      )
    } else if (any(parts == 2)) {
      LandingsArr[, fl, ] <- make_length_matrix(
        dplyr::filter(fleet_db, Part == 2)
      )
    }
  }
  
  
  # SS3/`r4ss` does not report the physical unit of length bins; `LengthUnits`
  # (passed in from `ImportSSData()`/`ImportSS()`) is the caller's assumption.
  units_label <- LengthUnits

  # `Classes` is a named list, one vector per fleet (see compdata-class),
  # even though every fleet gets the same grid here: SS3 reports length
  # composition data against one model-wide population length-bin
  # structure (`lbinspop`), not a fleet-specific one, so `LengthClasses` is
  # already the shared bin grid for every fleet. Storing it as a per-fleet
  # list rather than a flat vector keeps this consistent with
  # `LandingsAtSize`/`DiscardsAtSize` built elsewhere (e.g.
  # `.GenHistDataSizeComp()`), where fleets are not required to share a
  # grid.
  ClassesList <- stats::setNames(rep(list(as.numeric(LengthClasses)), nFleet), FleetNames)

  Landings         <- new("compdata")
  Landings@Name    <- FleetNames
  Landings@Value   <- LandingsArr
  Landings@Classes <- ClassesList
  Landings@Units   <- units_label

  Discards         <- new("compdata")
  Discards@Name    <- FleetNames
  Discards@Value   <- DiscardsArr
  Discards@Classes <- ClassesList
  Discards@Units   <- units_label

  list(Landings = Landings, Discards = Discards)

}
