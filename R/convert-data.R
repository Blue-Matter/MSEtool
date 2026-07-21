# TODO - legacy `Data` slots not yet mapped by `.ConvertDataToLegacy()` (new -> legacy
# direction); left `NA` and recorded in `Data@Misc$UnmappedSlots`:
#
# Data@SpInd, Data@CV_SpInd
# Data@VInd, Data@CV_VInd
# Data@AddInd, Data@AddIndType, Data@AddIndV, Data@AddIunits, Data@CV_AddInd
# Data@Rec, Data@CV_Rec
# Data@ML, Data@Lc, Data@Lbar
# Data@Abun, Data@CV_Abun, Data@SpAbun, Data@CV_SpAbun
# Data@FMSY_M, Data@CV_FMSY_M, Data@BMSY_B0, Data@CV_BMSY_B0
# Data@Bref, Data@CV_Bref
# Data@t, Data@AvC, Data@CV_AvC
# Data@Dt, Data@CV_Dt
# Data@Ref, Data@Ref_type
#
# TODO - legacy `Data` slots not yet mapped by `.Data2data()` (legacy -> new
# direction, pre-existing gap):
#
# data@DiscardsAtAge / DiscardsAtSize (.Data2CAA/.Data2CAL only populate
#   Landings* - legacy `Data@CAA`/`Data@CAL` don't distinguish landed vs
#   discarded catch)
# data@Survey (intentionally empty for legacy-sourced objects, see
#   .Data2Survey - all indices are folded into `CPUE` instead)


#' Convert Between Legacy `Data` and New `data` Classes
#'
#' Converts a legacy [Data-legacy-class] object to the current [data-class],
#' or a [data-class] object (or list of [data-class] objects, one per
#' simulation) to a legacy [Data-legacy-class] object. Direction is
#' determined automatically from the class of `x`.
#'
#' @param x A [Data-legacy-class] object, a [data-class] object, or a `list`
#'   of [data-class] objects (one per simulation, to be combined into a
#'   single multi-sim legacy `Data` object via [joinData()]).
#' @param Seasons Integer. Number of seasons per year. Only used when
#'   converting legacy `Data` -> `data`. Default `1`.
#' @param sim Integer. Simulation index to extract from the legacy object.
#'   Only used when converting legacy `Data` -> `data`. Default `1`.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' ## Legacy `Data` -> `data`
#' Legacy `Data` objects do not distinguish between landings and discards.
#' `Data@Cat` is mapped to `Landings` only; `Discards` will be empty in the
#' resulting [data-class] object. If `Data@Cat` represents total removals,
#' the `Discards` slot must be populated manually after conversion.
#'
#' `Ind`, `SpInd`, `VInd`, and every column of `AddInd` are folded into one
#' multi-column `data@CPUE` (there is no fishery-dependent/-independent
#' distinction in legacy `Data`); the tracked biomass component (total,
#' spawning, vulnerable) is recorded in `CPUE@Misc$IndexType`. `data@Survey`
#' is intentionally left empty. `FMSY_M` is converted to an absolute
#' `Reference@FMSY` via `FMSY_M * Mort`. Slots with no structural equivalent
#' anywhere in `data` (`ML`, `Lc`, `Lbar`, `Abun`, `SpAbun`, `BMSY_B0`,
#' `Bref`, `Rec`, `AvC`, `Dt`, `Ref`, `Ref_type`) are stashed under
#' `data@Misc`/`data@Reference@Misc` rather than dropped.
#'
#' ## `data` -> Legacy `Data`
#' A [data-class] object holds a single realization (no `Sim` dimension), so
#' the result always has `nsim = 1` - callers of a legacy MP must use
#' `x = 1`. Multi-fleet time series and composition data are aggregated to
#' the single-fleet legacy shape (landings + discards summed across fleets
#' for `Cat`; effort summed across fleets; age/size composition summed
#' across fleets, with size-composition bins rebinned onto a common grid
#' first). When multiple `CPUE`/`Survey` indices are present, one is
#' selected for `Data@Ind` (`Survey` preferred over `CPUE`, then most
#' complete, then longest span). Slots with no equivalent in `data` (see
#' `Data@Misc$UnmappedSlots` on the result) are left `NA`. Every lossy or
#' approximated mapping is recorded in `Data@Log` (see [Log()]).
#'
#' @return
#' - Legacy `Data` -> `data`: a [data-class] object populated from
#'   simulation `sim` of `x`.
#' - `data` (or list of `data`) -> legacy `Data`: a [Data-legacy-class]
#'   object with `nsim` equal to `1` (single `data` object) or
#'   `length(x)` (list of `data` objects).
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()], [joinData()]
#'
#' @examples
#' \dontrun{
#' Datalegacy <- readRDS("MyLegacyData.rds")
#' data_new <- ConvertData(Datalegacy, sim = 1)
#' Data_again <- ConvertData(data_new)
#' }
#'
#' @export
ConvertData <- function(x, Seasons = 1, sim = 1, silent = FALSE) {
  .CheckClass(x, c("Data", "data", "list"), "x")

  if (is.list(x) && !inherits(x, "data"))
    return(.ConvertDataToLegacy(x, silent = silent))

  if (inherits(x, "data"))
    return(.ConvertDataToLegacy(x, silent = silent))

  .Data2data(x, Seasons = Seasons, sim = sim, silent = silent)
}

.Data2data <- function(Data, Seasons = 1, sim = 1, silent = FALSE) {
  .CheckClass(Data, "Data", "Data")

  if (!silent)
    cli::cli_alert("Converting object of class {.cls Data} (Simulation {.val {sim}}) to class {.cls data}.")

  YearsInfo <- .Data2Years(Data, Seasons)

  data <- Data(
    Name       = Data@Name,
    CommonName = Data@Common_Name,
    Species    = Data@Species,
    Region     = Data@Region,
    Years      = YearsInfo$Years,
    YearLH     = YearsInfo$LHYear,
    Seasons    = Seasons,
    nArea      = Data@nareas
  )

  data <- .Data2LifeHistory(Data, data, sim)
  data <- .Data2Exploitation(Data, data, sim)
  data <- .Data2Effort(Data, data, sim)
  data <- .Data2Landings(Data, data, sim)
  data <- .Data2CPUE(Data, data, sim)
  data <- .Data2Survey(Data, data, sim)
  data <- .Data2CAA(Data, data, sim)
  data <- .Data2CAL(Data, data, sim)
  data <- .Data2Advice(Data, data, sim)
  data <- .Data2LegacyExtras(Data, data, sim)
  data
}

.Data2Years <- function(Data, Seasons = 1) {
  YearsInfo <- list(
    Years  = Data@Year,
    LHYear = Data@LHYear
  )

  if (all(YearsInfo$Years < 1000) && Seasons == 1) {
    YearsInfo$LHYear <- as.numeric(format(Sys.Date(), "%Y"))
    YearsInfo$Years  <- seq(
      from       = YearsInfo$LHYear - length(YearsInfo$Years) + 1,
      by         = 1,
      length.out = length(YearsInfo$Years)
    )
  }

  if (Seasons == 1)
    return(YearsInfo)

  YearsInfo$Years <- CalcYears(
    nYear       = length(Data@Year),
    pYear       = 0,
    CurrentYear = Data@LHYear,
    Seasons     = Seasons
  )
  YearsInfo
}

.Data2LifeHistory <- function(Data, data, sim) {

  data@LifeHistory@Ages <- Ages(
    MaxAge = Data@MaxAge,
    Units  = CalcTSUnits(data@Seasons)
  )

  if (!is.na(Data@vbLinf[sim])) {
    data@LifeHistory@Length@Pars <- list(
      Linf    = Data@vbLinf[sim],
      Linf_CV = Data@CV_vbLinf[sim],
      K       = Data@vbK[sim],
      K_CV    = Data@CV_vbK[sim],
      t0      = Data@vbt0[sim],
      t0_CV   = Data@CV_vbt0[sim]
    )
    data@LifeHistory@Length@Model  <- 'vonBert'
    data@LifeHistory@Length@CVatAge <- Data@LenCV[sim]
  }

  if (!is.na(Data@wla[sim])) {
    data@LifeHistory@Weight@Pars <- list(
      alpha    = Data@wla[sim],
      alpha_CV = Data@CV_wla[sim],
      beta     = Data@wlb[sim],
      beta_CV  = Data@CV_wlb[sim]
    )
  }

  if (!is.na(Data@Mort[sim])) {
    data@LifeHistory@NaturalMortality@Pars <- list(
      M    = Data@Mort[sim],
      CV_M = Data@CV_Mort[sim]
    )
  }

  if (!is.na(Data@L50[sim])) {
    data@LifeHistory@Maturity@Pars <- list(
      L50    = Data@L50[sim],
      L50_CV = Data@CV_L50[sim],
      L50_95 = Data@L95[sim] - Data@L50[sim]
    )
  }

  if (!is.na(Data@steep[sim])) {
    data@LifeHistory@SRR@Pars <- list(
      h    = Data@steep[sim],
      h_CV = Data@CV_steep[sim]
    )
  }


  if (!is.na(Data@sigmaR[sim])) {
    data@LifeHistory@SRR@SD <- Data@sigmaR[sim]
  }

  if (!is.na(Data@Dep[sim])) {
    data@LifeHistory@Depletion@Final <- Data@Dep[sim]
  }

  data
}

.Data2Exploitation <- function(Data, data, sim) {
  if (!is.na(Data@LFC[sim])) {
    data@Exploitation@Selectivity@Pars <- list(
      L5       = Data@LFC[sim],
      L5_CV    = Data@CV_LFC[sim],
      LFS      = Data@LFS[sim],
      LFS_CV   = Data@CV_LFS[sim],
      Vmaxlen  = Data@Vmaxlen[sim]
    )
  }
  data
}

.AddYearFleetArray <- function(Value, data, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  nYears <- length(data@Years)
  out <- array(Value, dim = c(nYears, 1))
  dimnames(out) <- list(Year = data@Years, Fleet = fleet_name)
  out
}

.AddFleetArray <- function(Value, data, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  out <- array(Value, dim = 1)
  dimnames(out) <- list(Fleet = fleet_name)
  out
}

.AddYearAgeFleetArray <- function(Value, data, AgeClasses, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  nYears <- length(data@Years)
  # compdata@Value convention is [nYear x nFleet x nClass]; a size-1 Fleet
  # dim inserted between Year and Age preserves flat (year,age) order exactly
  out <- array(Value, dim = c(nYears, 1, length(AgeClasses)))
  dimnames(out) <- list(Year = data@Years, Fleet = fleet_name, Age = AgeClasses)
  out
}

.AddYearLengthFleetArray <- function(Value, data, LengthClasses, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  nYears <- length(data@Years)
  out <- array(Value, dim = c(nYears, 1, length(LengthClasses)))
  dimnames(out) <- list(Year = data@Years, Fleet = fleet_name, Length = LengthClasses)
  out
}

.Data2Effort <- function(Data, data, sim) {
  if (all(is.na(Data@Effort)))
    return(data)
  data@Effort@Value <- .ValorNULL(Data@Effort[sim, ]) |> .AddYearFleetArray(data)
  if (!is.null(data@Effort@Value))
    data@Effort@CV <- .ValorNULL(Data@CV_Effort[sim, ]) |> .AddYearFleetArray(data)
  data
}

.Data2Landings <- function(Data, data, sim) {
  if (all(is.na(Data@Cat)))
    return(data)
  data@Landings@Value <- .ValorNULL(Data@Cat[sim, ]) |> .AddYearFleetArray(data)
  if (is.null(data@Landings@Value))
    return(data)
  if (all(is.na(Data@CV_Cat)))
    return(data)
  data@Landings@CV    <- .ValorNULL(Data@CV_Cat[sim, ]) |> .AddYearFleetArray(data)
  data@Landings@Units <- Data@Units
  data@Landings@Ref   <- Data@Cref[sim]    |> .AddFleetArray(data)
  data@Landings@RefCV <- Data@CV_Cref[sim] |> .AddFleetArray(data)
  data
}

.AddYearIndexArray <- function(cols, data) {
  if (!length(cols)) return(NULL)
  out <- do.call(cbind, cols)
  dimnames(out) <- list(Year = data@Years, Index = names(cols))
  out
}

# Legacy `Data` has no fishery-dependent (CPUE) vs -independent (Survey)
# distinction - only *which biomass component* an index tracks, via
# `AddIndType` (1 = total, 2 = spawning, 3 = vulnerable) plus the dedicated
# `SpInd`/`VInd` slots. All indices are folded into one multi-column `CPUE`;
# `Survey` is intentionally left empty for legacy-sourced objects (see
# `.Data2Survey`), and the tracked component is recorded in `CPUE@Misc$IndexType`.
.Data2CPUE <- function(Data, data, sim) {
  cols   <- list()
  cvcols <- list()
  types  <- character()
  typeMap <- c("total", "spawning", "vulnerable")

  if (!all(is.na(Data@Ind[sim, ]))) {
    cols[["Ind"]]   <- Data@Ind[sim, ]
    cvcols[["Ind"]] <- Data@CV_Ind[sim, ]
    types["Ind"]    <- "total"
  }
  if (!all(is.na(Data@SpInd[sim, ]))) {
    cols[["SpInd"]]   <- Data@SpInd[sim, ]
    cvcols[["SpInd"]] <- Data@CV_SpInd[sim, ]
    types["SpInd"]    <- "spawning"
  }
  if (!all(is.na(Data@VInd[sim, ]))) {
    cols[["VInd"]]   <- Data@VInd[sim, ]
    cvcols[["VInd"]] <- Data@CV_VInd[sim, ]
    types["VInd"]    <- "vulnerable"
  }
  if (length(dim(Data@AddInd)) == 3 && !all(is.na(Data@AddInd))) {
    nInd <- dim(Data@AddInd)[2]
    for (j in seq_len(nInd)) {
      v <- Data@AddInd[sim, j, ]
      if (all(is.na(v))) next
      nm <- paste0("AddInd", j)
      cols[[nm]]   <- v
      cvcols[[nm]] <- if (length(dim(Data@CV_AddInd)) == 3) Data@CV_AddInd[sim, j, ] else rep(NA_real_, length(v))
      code <- Data@AddIndType[j]
      types[nm] <- if (!is.na(code) && code %in% 1:3) typeMap[code] else "unknown"
    }
  }

  if (!length(cols)) return(data)

  data@CPUE@Value <- .AddYearIndexArray(cols, data)
  data@CPUE@CV    <- .AddYearIndexArray(cvcols, data)
  data@CPUE@Misc$IndexType <- types

  if (!is.na(Data@Iref[sim])) {
    ref <- stats::setNames(rep(NA_real_, length(cols)), names(cols))
    ref["Ind"] <- Data@Iref[sim]
    data@CPUE@Ref <- ref
  }
  if (!is.na(Data@CV_Iref[sim])) {
    refcv <- rep(NA_real_, length(cols))
    refcv[match("Ind", names(cols))] <- Data@CV_Iref[sim]
    data@CPUE@RefCV <- array(refcv, dim = length(cols), dimnames = list(Index = names(cols)))
  }
  data
}

.Data2Survey <- function(Data, data, sim) {
  # legacy `Data` has no fishery-independent survey concept distinct from
  # `Ind`/`SpInd`/`VInd`/`AddInd` - all of those are folded into `CPUE` by
  # `.Data2CPUE`; `Survey` is intentionally left empty here, by design.
  data
}

.Data2CAA <- function(Data, data, sim) {
  CAAData <- Data@CAA
  if (all(is.na(CAAData)))
    return(data)
  data@LandingsAtAge@Name    <- "Fleet 1"
  data@LandingsAtAge@Classes <- seq(0, Data@MaxAge)
  data@LandingsAtAge@Value   <- abind::adrop(CAAData[sim, , , drop = FALSE], 1) |>
    .AddYearAgeFleetArray(data, data@LandingsAtAge@Classes)
  data
}

.Data2CAL <- function(Data, data, sim) {
  CALData <- Data@CAL
  if (all(is.na(CALData)) || !length(Data@CAL_bins))
    return(data)
  Classes <- Data@CAL_bins[-length(Data@CAL_bins)]
  data@LandingsAtSize@Name    <- "Fleet 1"
  data@LandingsAtSize@Classes <- list("Fleet 1" = Classes)
  data@LandingsAtSize@Value   <- abind::adrop(CALData[sim, , , drop = FALSE], 1) |>
    .AddYearLengthFleetArray(data, Classes)
  data
}

.Data2Advice <- function(Data, data, sim) {
  data@Advice@TAC    <- array(Data@MPrec[sim], 1, dimnames = list(Year = data@YearLH))
  data@Advice@Effort <- array(Data@MPeff[sim], 1, dimnames = list(Year = data@YearLH))
  data
}

# reference points and legacy slots with no structural equivalent in `data` -
# stashed under `data@Misc`/`data@Reference@Misc` rather than dropped, so no
# information is silently lost on conversion
.Data2LegacyExtras <- function(Data, data, sim) {
  if (!is.na(Data@FMSY_M[sim]) && !is.na(Data@Mort[sim])) {
    data@Reference@FMSY <- Data@FMSY_M[sim] * Data@Mort[sim]
    data@Reference@Misc$FMSY_Assumption <- "Derived as FMSY_M * Mort (legacy ratio x collapsed scalar Mort)."
  }

  refMisc <- list()
  if (!is.na(Data@BMSY_B0[sim]))  refMisc$BMSY_B0    <- list(Value = Data@BMSY_B0[sim],  CV = Data@CV_BMSY_B0[sim])
  if (!is.na(Data@Bref[sim]))     refMisc$Bref       <- list(Value = Data@Bref[sim],     CV = Data@CV_Bref[sim])
  if (length(refMisc)) data@Reference@Misc$LegacyReferenceRatios <- refMisc

  if (!is.na(Data@Abun[sim]) || !is.na(Data@SpAbun[sim]))
    data@Misc$LegacyAbundance <- list(
      Abun = Data@Abun[sim], CV_Abun = Data@CV_Abun[sim],
      SpAbun = Data@SpAbun[sim], CV_SpAbun = Data@CV_SpAbun[sim]
    )

  if (!all(is.na(Data@Rec[sim, ])))
    data@Misc$LegacyRecruitment <- list(Rec = Data@Rec[sim, ], CV_Rec = Data@CV_Rec[sim, ])

  extras <- list()
  if (!is.na(Data@AvC[sim]))       extras$AvC       <- list(Value = Data@AvC[sim], CV = Data@CV_AvC[sim])
  if (!is.na(Data@Dt[sim]))        extras$Dt        <- list(Value = Data@Dt[sim],  CV = Data@CV_Dt[sim], t = Data@t[sim])
  if (length(Data@Ref) >= sim && !is.na(Data@Ref[sim])) extras$Ref <- list(Value = Data@Ref[sim], Type = Data@Ref_type)
  if (length(extras)) data@Misc$LegacyReference <- extras

  data
}


# ---- data -> legacy Data ----------------------------------------------

#' @keywords internal
.ConvertDataToLegacy <- function(x, silent = FALSE) {
  if (is.list(x)) {
    if (!length(x))
      cli::cli_abort("`x` is an empty list")
    is_data <- vapply(x, inherits, logical(1), what = "data")
    if (!all(is_data))
      cli::cli_abort("Every element of list `x` must be class {.cls data}")
    if (length(x) == 1)
      return(.ConvertOneData(x[[1]], silent = silent))
    DataList <- lapply(x, .ConvertOneData, silent = silent)
    return(joinData(DataList))
  }

  .CheckClass(x, "data", "x")
  .ConvertOneData(x, silent = silent)
}

.ConvertOneData <- function(data, silent = FALSE) {
  if (!silent)
    cli::cli_alert("Converting object of class {.cls data} to class {.cls Data} (nsim = 1).")

  Data <- methods::new("Data")

  if (!is.null(data@Seasons) && length(data@Seasons) && data@Seasons != 1)
    Data <- .CaptureLog(
      Data,
      sprintf("Source `data` object has %s seasons per year; `.ConvertDataToLegacy()` produces an annual legacy `Data` object - sub-annual time steps are not resolved.", data@Seasons),
      name = ".ConvertDataToLegacy::Years", type = "warning"
    )

  Data <- .LegacyYears(data, Data)
  Data <- .LegacyLifeHistory(data, Data)
  Data <- .LegacyExploitation(data, Data)
  Data <- .LegacyEffort(data, Data)
  Data <- .LegacyLandings(data, Data)
  Data <- .LegacyCPUESurvey(data, Data)
  Data <- .LegacyCAA(data, Data)
  Data <- .LegacyCAL(data, Data)
  Data <- .LegacyAdvice(data, Data)
  Data <- .FinalizeUnmappedSlots(Data)
  Data
}

.LegacyYears <- function(data, Data) {
  Years <- data@Years
  YearLH <- data@YearLH
  if (is.null(YearLH) || !length(YearLH))
    YearLH <- floor(max(Years))

  if (!is.null(Years) && length(Years))
    Data@Year <- sort(unique(floor(Years)))

  Data@LHYear <- YearLH
  if (!is.null(data@Name) && length(data@Name))       Data@Name        <- data@Name
  if (!is.null(data@CommonName) && length(data@CommonName)) Data@Common_Name <- data@CommonName
  if (!is.null(data@Species) && length(data@Species))  Data@Species     <- data@Species
  if (!is.null(data@Region) && length(data@Region))    Data@Region      <- data@Region
  if (!is.null(data@nArea) && length(data@nArea))      Data@nareas      <- data@nArea
  Data
}

# ---- biology scalar-reduction ----

.LegacyLifeHistory <- function(data, Data) {
  LH <- data@LifeHistory

  MaxAge <- LH@Ages@MaxAge
  if (!length(MaxAge)) {
    ageClasses <- c(data@LandingsAtAge@Classes, data@DiscardsAtAge@Classes)
    if (length(ageClasses))
      MaxAge <- max(ageClasses, na.rm = TRUE)
  }
  if (length(MaxAge))
    Data@MaxAge <- MaxAge

  Data <- .LegacyLength(LH@Length, Data)
  Data <- .LegacyWeight(LH@Weight, Data)
  Data <- .LegacyMort(LH@NaturalMortality, Data)
  Data <- .LegacyMaturity(LH@Maturity, Data, Data@MaxAge)
  Data <- .LegacySRR(LH@SRR, Data)
  Data <- .LegacyDepletion(LH@Depletion, Data)
  Data
}

.LegacyLength <- function(Length, Data) {
  Pars <- Length@Pars
  if (length(Pars) && identical(Length@Model, 'vonBert') && !is.null(Pars$Linf)) {
    Data@vbLinf     <- Pars$Linf
    Data@CV_vbLinf  <- Pars$Linf_CV %||% NA_real_
    Data@vbK        <- Pars$K
    Data@CV_vbK     <- Pars$K_CV %||% NA_real_
    Data@vbt0       <- Pars$t0 %||% 0
    Data@CV_vbt0    <- Pars$t0_CV %||% NA_real_
    if (length(Length@CVatAge))
      Data@LenCV    <- .TerminalValue(Length@CVatAge)
    return(Data)
  }

  if (length(Length@MeanAtAge)) {
    LAA <- .TerminalValue(Length@MeanAtAge)
    Ages <- as.numeric(names(LAA))
    if (is.null(Ages) || any(is.na(Ages))) Ages <- seq_along(LAA) - 1
    fit <- .FitVonBert(Ages, LAA)
    Data@vbLinf <- fit$Linf
    Data@vbK    <- fit$K
    Data@vbt0   <- fit$t0
    Data@CV_vbLinf <- NA_real_
    Data@CV_vbK    <- NA_real_
    Data@CV_vbt0   <- NA_real_
    Data <- .CaptureLog(
      Data, "`vbLinf`/`vbK`/`vbt0` estimated by refitting a von Bertalanffy curve to the terminal-year `Length@MeanAtAge` schedule (no CVs available).",
      name = ".ConvertDataToLegacy::Length", type = "assumption"
    )
  }
  Data
}

.LegacyWeight <- function(Weight, Data) {
  Pars <- Weight@Pars
  if (length(Pars) && !is.null(Pars$alpha) && !is.null(Pars$beta)) {
    Data@wla    <- Pars$alpha
    Data@CV_wla <- Pars$alpha_CV %||% NA_real_
    Data@wlb    <- Pars$beta
    Data@CV_wlb <- Pars$beta_CV %||% NA_real_
    return(Data)
  }

  if (length(Weight@MeanAtLength) && !is.null(Weight@Classes)) {
    WAL <- .TerminalValue(Weight@MeanAtLength)
    Mids <- Weight@Classes
    fit <- .FitLengthWeight(Mids[seq_along(WAL)], WAL)
    Data@wla <- fit$alpha
    Data@wlb <- fit$beta
    Data@CV_wla <- NA_real_
    Data@CV_wlb <- NA_real_
    Data <- .CaptureLog(
      Data, "`wla`/`wlb` estimated by refitting a length-weight regression to the terminal-year `Weight@MeanAtLength` schedule (no CVs available).",
      name = ".ConvertDataToLegacy::Weight", type = "assumption"
    )
  }
  Data
}

.LegacyMort <- function(NM, Data) {
  Pars <- NM@Pars
  if (length(Pars) && !is.null(Pars$M)) {
    Data@Mort    <- Pars$M
    Data@CV_Mort <- Pars$CV_M %||% NA_real_
    return(Data)
  }

  if (length(NM@MeanAtAge)) {
    MAA <- .TerminalValue(NM@MeanAtAge)
    Data@Mort    <- mean(MAA, na.rm = TRUE)
    Data@CV_Mort <- NA_real_
    Data <- .CaptureLog(
      Data, "`Mort` set to the mean of the terminal-year, age-varying `NaturalMortality@MeanAtAge` schedule (age structure lost, no CV available).",
      name = ".ConvertDataToLegacy::NaturalMortality", type = "assumption"
    )
  }
  Data
}

.LegacyMaturity <- function(Mat, Data, MaxAge) {
  Pars <- Mat@Pars
  if (length(Pars) && !is.null(Pars$L50)) {
    Data@L50    <- Pars$L50
    Data@CV_L50 <- Pars$L50_CV %||% NA_real_
    Data@L95    <- Pars$L50 + (Pars$L50_95 %||% 0)
    return(Data)
  }

  if (length(Mat@MeanAtLength) && !is.null(Mat@Classes)) {
    Ogive <- .TerminalValue(Mat@MeanAtLength)
    Mids  <- Mat@Classes[seq_along(Ogive)]
    Data@L50 <- .InterpolateOgive(Mids, Ogive, 0.5)
    Data@L95 <- .InterpolateOgive(Mids, Ogive, 0.95)
    Data@CV_L50 <- NA_real_
    Data <- .CaptureLog(
      Data, "`L50`/`L95` interpolated from the terminal-year empirical `Maturity@MeanAtLength` ogive (no CV available).",
      name = ".ConvertDataToLegacy::Maturity", type = "assumption"
    )
    return(Data)
  }

  if (length(Mat@MeanAtAge) && length(Data@vbLinf) && !is.na(Data@vbLinf)) {
    Ogive <- .TerminalValue(Mat@MeanAtAge)
    Ages  <- seq(0, MaxAge, length.out = length(Ogive))
    A50   <- .InterpolateOgive(Ages, Ogive, 0.5)
    A95   <- .InterpolateOgive(Ages, Ogive, 0.95)
    Data@L50 <- .VonBertLength(A50, Data@vbLinf, Data@vbK, Data@vbt0)
    Data@L95 <- .VonBertLength(A95, Data@vbLinf, Data@vbK, Data@vbt0)
    Data@CV_L50 <- NA_real_
    Data <- .CaptureLog(
      Data, "`L50`/`L95` converted from an age-based `Maturity@MeanAtAge` ogive via the fitted/estimated growth curve (no CV available).",
      name = ".ConvertDataToLegacy::Maturity", type = "assumption"
    )
  }
  Data
}

.LegacySRR <- function(SRR, Data) {
  Pars <- SRR@Pars
  Model <- SRR@Model
  if (length(Pars) && (identical(Model, 'BevertonHolt') || is.null(Model)) && !is.null(Pars$h)) {
    Data@steep    <- Pars$h
    Data@CV_steep <- Pars$h_CV %||% NA_real_
  } else if (length(Pars) && !is.null(Model) && !identical(Model, 'BevertonHolt')) {
    Data <- .CaptureLog(
      Data, sprintf("`SRR@Model` is '%s'; not directly convertible to legacy Beverton-Holt-style `steep` - left NA.", .ModelName(Model)),
      name = ".ConvertDataToLegacy::SRR", type = "assumption"
    )
  }

  if (length(SRR@SD)) {
    Data@sigmaR <- SRR@SD[1]
  }
  Data
}

.LegacyDepletion <- function(Dep, Data) {
  if (length(Dep@Final)) {
    Data@Dep <- Dep@Final[1]
  }
  Data
}

.LegacyExploitation <- function(data, Data) {
  Pars <- data@Exploitation@Selectivity@Pars
  if (length(Pars) && !is.null(Pars$L5)) {
    Data@LFC     <- Pars$L5
    Data@CV_LFC  <- Pars$L5_CV %||% NA_real_
    Data@LFS     <- Pars$LFS
    Data@CV_LFS  <- Pars$LFS_CV %||% NA_real_
    Data@Vmaxlen <- Pars$Vmaxlen %||% NA_real_
    return(Data)
  }

  Sel <- data@Exploitation@Selectivity
  if (length(Sel@MeanAtLength) && !is.null(Sel@Classes)) {
    Ogive <- .TerminalValue(Sel@MeanAtLength)
    Mids  <- Sel@Classes[seq_along(Ogive)]
    Data@LFC <- .InterpolateOgive(Mids, Ogive, 0.05)
    Data@LFS <- Mids[which.max(Ogive)]
    Data <- .CaptureLog(
      Data, "`LFC`/`LFS` interpolated from the terminal-year empirical selectivity-at-length ogive (no CV available).",
      name = ".ConvertDataToLegacy::Exploitation", type = "assumption"
    )
  }
  Data
}

# ---- fleet aggregation ----

.CollapseFleetArray <- function(Value, FUN = sum) {
  if (is.null(Value)) return(NULL)
  if (is.null(dim(Value)) || length(dim(Value)) < 2)
    return(as.numeric(Value))
  apply(Value, 1, FUN, na.rm = TRUE)
}

.FleetNamesOf <- function(catchOrEffort) {
  nm <- dimnames(catchOrEffort@Value)
  if (!is.null(nm) && !is.null(nm[[2]])) return(nm[[2]])
  if (!is.null(catchOrEffort@Name)) return(catchOrEffort@Name)
  if (!is.null(catchOrEffort@Value)) return(paste0("Fleet", seq_len(ncol(catchOrEffort@Value))))
  character()
}

.AlignFleetArray <- function(Value, allFleets, fleetNames) {
  if (is.null(Value)) return(NULL)
  nYear <- nrow(Value)
  out <- matrix(0, nYear, length(allFleets), dimnames = list(NULL, allFleets))
  out[, fleetNames] <- Value
  out
}

.WeightedMeanCV <- function(cvList, valList) {
  valList <- Filter(Negate(is.null), valList)
  cvList  <- Filter(Negate(is.null), cvList)
  if (!length(valList)) return(NULL)
  nYear <- nrow(valList[[1]])
  out <- rep(NA_real_, nYear)
  for (yr in seq_len(nYear)) {
    vals <- unlist(lapply(valList, function(v) v[yr, ]))
    cvs  <- unlist(lapply(cvList,  function(v) v[yr, ]))
    ok <- !is.na(vals) & !is.na(cvs) & vals > 0
    if (any(ok)) out[yr] <- stats::weighted.mean(cvs[ok], w = vals[ok])
  }
  out
}

.LegacyEffort <- function(data, Data) {
  V <- data@Effort@Value
  if (is.null(V)) return(Data)

  Data@Effort <- matrix(.CollapseFleetArray(V), nrow = 1)
  cv <- data@Effort@CV
  if (!is.null(cv))
    Data@CV_Effort <- matrix(.WeightedMeanCV(list(cv), list(V)), nrow = 1)

  if (ncol(V) > 1)
    Data <- .CaptureLog(
      Data, sprintf("Effort summed across %d fleets (units: %s); cross-fleet effort units may not be directly additive.",
                    ncol(V), paste(unique(data@Effort@Units), collapse = ", ")),
      name = ".ConvertDataToLegacy::Effort", type = "warning"
    )
  Data
}

.LegacyLandings <- function(data, Data) {
  L <- data@Landings@Value
  D <- data@Discards@Value
  if (is.null(L) && is.null(D)) return(Data)

  fleetsL <- if (!is.null(L)) .FleetNamesOf(data@Landings) else character()
  fleetsD <- if (!is.null(D)) .FleetNamesOf(data@Discards) else character()
  allFleets <- union(fleetsL, fleetsD)

  Lal <- .AlignFleetArray(L, allFleets, fleetsL)
  Dal <- .AlignFleetArray(D, allFleets, fleetsD)

  unitsL <- unique(data@Landings@Units)
  unitsD <- unique(data@Discards@Units)
  if ((length(unitsL) > 1) || (length(unitsD) > 1) ||
      (length(unitsL) && length(unitsD) && !identical(unitsL, unitsD))) {
    Data <- .CaptureLog(
      Data, sprintf("Landings units (%s) and/or Discards units (%s) are not uniform; summed assuming compatible units.",
                    paste(unitsL, collapse = ","), paste(unitsD, collapse = ",")),
      name = ".ConvertDataToLegacy::Landings", type = "warning"
    )
  }

  total <- (if (!is.null(Lal)) Lal else 0) + (if (!is.null(Dal)) Dal else 0)
  Data@Cat <- matrix(rowSums(total, na.rm = TRUE), nrow = 1)
  Data@Units <- if (length(unitsL)) unitsL[1] else if (length(unitsD)) unitsD[1] else NA_character_

  cv <- .WeightedMeanCV(list(data@Landings@CV, data@Discards@CV), list(L, D))
  if (!is.null(cv)) {
    Data@CV_Cat <- matrix(cv, nrow = 1)
    Data <- .CaptureLog(
      Data, "`CV_Cat` derived via value-weighted average of Landings/Discards CVs across fleets, not a full error-propagation.",
      name = ".ConvertDataToLegacy::Landings", type = "assumption"
    )
  }

  if (!is.null(data@Landings@Ref)) {
    Data@Cref    <- sum(data@Landings@Ref, na.rm = TRUE)
    if (!is.null(data@Landings@RefCV))
      Data@CV_Cref <- mean(data@Landings@RefCV, na.rm = TRUE)
  }

  Data <- .CaptureLog(
    Data, "`Cat` = sum of Landings + Discards across fleets (legacy `Cat` treated as total removals).",
    name = ".ConvertDataToLegacy::Landings", type = "assumption"
  )
  Data
}

# ---- index selection ----

.LegacyCPUESurvey <- function(data, Data) {
  candidates <- list()
  for (src in c("Survey", "CPUE")) {
    ind <- methods::slot(data, src)
    if (is.null(ind@Value)) next
    for (j in seq_len(ncol(ind@Value))) {
      v <- ind@Value[, j]
      nComplete <- sum(!is.na(v))
      if (nComplete == 0) next
      idx <- which(!is.na(v))
      span <- if (length(idx) > 1) diff(range(idx)) else 0
      nm <- dimnames(ind@Value)
      idxName <- if (!is.null(nm) && !is.null(nm[[2]])) nm[[2]][j] else paste0(src, j)
      candidates[[length(candidates) + 1]] <- list(
        source = src, col = j, name = idxName,
        nComplete = nComplete, span = span,
        value = v,
        cv = if (!is.null(ind@CV)) ind@CV[, j] else rep(NA_real_, length(v)),
        ref = if (!is.null(ind@Ref)) ind@Ref[j] else NA_real_,
        refcv = if (!is.null(ind@RefCV)) ind@RefCV[j] else NA_real_
      )
    }
  }
  if (!length(candidates)) return(Data)

  tierRank <- vapply(candidates, function(c) if (c$source == "Survey") 1 else 2, numeric(1))
  nComp    <- vapply(candidates, `[[`, numeric(1), "nComplete")
  spanVec  <- vapply(candidates, `[[`, numeric(1), "span")
  ord <- order(tierRank, -nComp, -spanVec)
  chosen <- candidates[[ord[1]]]

  Data@Ind    <- matrix(chosen$value, nrow = 1)
  Data@CV_Ind <- matrix(chosen$cv, nrow = 1)
  if (!is.na(chosen$ref))   Data@Iref    <- chosen$ref
  if (!is.na(chosen$refcv)) Data@CV_Iref <- chosen$refcv

  Data <- .CaptureLog(
    Data, sprintf("Selected index '%s' (%s, %d/%d years non-NA) for `Ind` from %d candidate series.",
                  chosen$name, chosen$source, chosen$nComplete, length(chosen$value), length(candidates)),
    name = ".ConvertDataToLegacy::Index", type = "assumption"
  )
  if (length(candidates) > 1)
    Data <- .CaptureLog(
      Data, sprintf("%d unselected index series were not mapped; consider `Data@AddInd` manually.", length(candidates) - 1),
      name = ".ConvertDataToLegacy::Index", type = "assumption"
    )
  Data
}

# ---- composition: age (shared grid) and size (per-fleet grid, rebinned) ----

.AlignAgeGrid <- function(Value, fromAges, toAges) {
  if (is.null(Value)) return(NULL)
  out <- matrix(0, nrow(Value), length(toAges), dimnames = list(NULL, toAges))
  for (j in seq_along(fromAges)) {
    age <- fromAges[j]
    target <- if (age >= max(toAges)) as.character(max(toAges)) else as.character(age)
    if (!target %in% colnames(out)) next
    out[, target] <- out[, target] + Value[, j]
  }
  out
}

.LegacyCAA <- function(data, Data) {
  L <- data@LandingsAtAge@Value
  D <- data@DiscardsAtAge@Value
  if (is.null(L) && is.null(D)) return(Data)

  agesFrom <- if (!is.null(L)) data@LandingsAtAge@Classes else data@DiscardsAtAge@Classes
  if (!length(Data@MaxAge) || is.na(Data@MaxAge)) {
    Data <- .CaptureLog(
      Data, "`CAA` not converted - `MaxAge` could not be resolved from `LifeHistory@Ages` or age-composition `Classes`.",
      name = ".ConvertDataToLegacy::CAA", type = "warning"
    )
    return(Data)
  }
  agesFull <- seq(0, Data@MaxAge)

  Lsum <- if (!is.null(L)) apply(L, c(1, 3), sum, na.rm = TRUE) else NULL
  Dsum <- if (!is.null(D)) apply(D, c(1, 3), sum, na.rm = TRUE) else NULL
  total <- (if (!is.null(Lsum)) Lsum else 0) + (if (!is.null(Dsum)) Dsum else 0)

  aligned <- .AlignAgeGrid(total, agesFrom, agesFull)
  Data@CAA <- array(aligned, dim = c(1, nrow(aligned), length(agesFull)))
  Data
}

.OverlapWeights <- function(fromBins, toBins) {
  nFrom <- length(fromBins)
  nTo   <- length(toBins) - 1
  W <- matrix(0, nFrom, nTo)
  fromUpper <- c(fromBins[-1], Inf)
  toUpper   <- toBins[-1]
  for (i in seq_len(nFrom)) {
    lo <- fromBins[i]; hi <- fromUpper[i]
    width <- if (is.finite(hi)) hi - lo else toUpper[nTo] - lo
    if (width <= 0) next
    for (k in seq_len(nTo)) {
      overlap <- min(hi, toUpper[k]) - max(lo, toBins[k])
      if (overlap > 0) W[i, k] <- overlap / width
    }
  }
  W
}

.LegacyCAL <- function(data, Data) {
  fleetSets <- list(data@LandingsAtSize, data@DiscardsAtSize)
  classesAll <- unlist(lapply(fleetSets, function(cd) cd@Classes), recursive = FALSE)
  if (!length(classesAll)) return(Data)

  binWidth <- min(vapply(classesAll, function(cl) if (length(cl) > 1) min(diff(cl)) else 1, numeric(1)))
  rng <- range(unlist(classesAll))
  CAL_bins <- seq(rng[1], rng[2] + binWidth, by = binWidth)
  CAL_mids <- CAL_bins[-length(CAL_bins)] + binWidth / 2

  RebinSum <- function(cd) {
    if (is.null(cd@Value)) return(NULL)
    nYear <- dim(cd@Value)[1]
    out <- matrix(0, nYear, length(CAL_mids))
    for (fl in seq_along(cd@Classes)) {
      fromBins <- cd@Classes[[fl]]
      W <- .OverlapWeights(fromBins, CAL_bins)
      out <- out + cd@Value[, fl, seq_len(nrow(W)), drop = TRUE] %*% W
    }
    out
  }

  Lr <- RebinSum(data@LandingsAtSize)
  Dr <- RebinSum(data@DiscardsAtSize)
  total <- (if (!is.null(Lr)) Lr else 0) + (if (!is.null(Dr)) Dr else 0)

  Data@CAL      <- array(total, dim = c(1, nrow(total), length(CAL_mids)))
  Data@CAL_bins <- CAL_bins
  Data@CAL_mids <- CAL_mids
  Data <- .CaptureLog(
    Data, sprintf("CAL rebinned onto a common %g-unit grid across %d fleet size-composition series (uniform-within-bin allocation assumed).",
                  binWidth, length(classesAll)),
    name = ".ConvertDataToLegacy::CAL", type = "assumption"
  )
  Data
}

.LegacyAdvice <- function(data, Data) {
  tac <- data@Advice@TAC
  eff <- data@Advice@Effort
  if (!is.null(tac) && length(tac)) {
    tacVals <- unlist(tac)
    tacVals <- tacVals[!is.na(tacVals)]
    if (length(tacVals)) Data@MPrec <- utils::tail(tacVals, 1)
  }
  if (!is.null(eff) && length(eff)) {
    effVals <- unlist(eff)
    effVals <- effVals[!is.na(effVals)]
    if (length(effVals)) Data@MPeff <- utils::tail(effVals, 1)
  }
  Data
}

.FinalizeUnmappedSlots <- function(Data) {
  gapSlots <- c(
    "SpInd", "CV_SpInd", "VInd", "CV_VInd",
    "AddInd", "CV_AddInd", "AddIndV", "AddIunits", "AddIndType",
    "ML", "Lc", "Lbar",
    "Abun", "CV_Abun", "SpAbun", "CV_SpAbun",
    "FMSY_M", "CV_FMSY_M", "BMSY_B0", "CV_BMSY_B0",
    "Bref", "CV_Bref",
    "Rec", "CV_Rec", "AvC", "CV_AvC", "Dt", "CV_Dt",
    "Ref", "Ref_type"
  )
  isNA <- vapply(gapSlots, function(s) all(is.na(methods::slot(Data, s))), logical(1))
  Data@Misc$UnmappedSlots <- gapSlots[isNA]
  Data@Misc$ConversionInfo <- list(
    SourceClass = "data",
    ConvertedAt = Sys.time(),
    Function    = ".ConvertDataToLegacy"
  )
  Data
}

# ---- small numeric helpers ----

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

.TerminalValue <- function(arr) {
  if (is.null(arr)) return(NULL)
  d <- dim(arr)
  if (is.null(d)) return(arr)
  nd <- length(d)
  idx <- rep(list(1), nd)
  idx[[nd]] <- d[nd]
  do.call(`[`, c(list(arr), idx, list(drop = TRUE)))
}

.ModelName <- function(Model) {
  if (is.function(Model)) return(deparse(substitute(Model)))
  as.character(Model)
}

.VonBertLength <- function(Age, Linf, K, t0) {
  Linf * (1 - exp(-K * (Age - t0)))
}

.FitVonBert <- function(Ages, LAA) {
  ok <- !is.na(Ages) & !is.na(LAA)
  Ages <- Ages[ok]; LAA <- LAA[ok]
  start <- list(Linf = max(LAA) * 1.05, K = 0.3, t0 = 0)
  fit <- tryCatch(
    stats::nls(LAA ~ Linf * (1 - exp(-K * (Ages - t0))), start = start),
    error = function(e) NULL
  )
  if (is.null(fit))
    return(list(Linf = max(LAA), K = 0.3, t0 = 0))
  co <- stats::coef(fit)
  list(Linf = unname(co["Linf"]), K = unname(co["K"]), t0 = unname(co["t0"]))
}

.FitLengthWeight <- function(Lengths, Weights) {
  ok <- !is.na(Lengths) & !is.na(Weights) & Lengths > 0 & Weights > 0
  Lengths <- Lengths[ok]; Weights <- Weights[ok]
  fit <- tryCatch(
    stats::lm(log(Weights) ~ log(Lengths)),
    error = function(e) NULL
  )
  if (is.null(fit) || length(stats::coef(fit)) < 2)
    return(list(alpha = NA_real_, beta = NA_real_))
  co <- stats::coef(fit)
  list(alpha = exp(unname(co[1])), beta = unname(co[2]))
}

.InterpolateOgive <- function(x, y, target) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]; y <- y[ok]
  if (!length(x)) return(NA_real_)
  if (all(y < target)) return(max(x))
  if (all(y > target)) return(min(x))
  stats::approx(y, x, xout = target, ties = "ordered")$y
}
