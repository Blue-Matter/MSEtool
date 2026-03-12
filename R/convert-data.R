# TODO
# data@Reference
# 
# Data@SpInd
# Data@CV_SpInd
# 
# Data@VInd
# Data@CV_VInd
# 
# Data@AddInd
# Data@AddIndType
# Data@AddIndV
# Data@AddIunits
# Data@CV_AddInd
# 
# Data@Rec
# Data@CV_Rec
# 
# Data@ML
# Data@Lc
# Data@Lbar
# 
# 
# Data@Vuln_CAL
# Data@CAL
# Data@CAL_bins
# Data@CAL_mids
# 
# Data@Dep
# Data@CV_Dep
# 
# Data@Abun
# Data@CV_SpAbun
# Data@SpAbun
# Data@CV_SpAbun
# Data@FMSY_M
# Data@CV_FMSY_M
# Data@BMSY_B0
# Data@CV_BMSY_B0
# 
# 
# Data@Bref
# Data@CV_Bref
# 
# Data@Iref
# Data@CV_Iref
# 
# Data@t
# Data@AvC
# Data@CV_AvC
# 
# Data@Dt
# Data@CV_Dt
# 
# Data@Ref
# Data@Ref_type


#' Convert a Legacy Data Object to a New data Class
#'
#' Converts a legacy [Data-legacy-class] object to the current [data-class]
#' for a single simulation, mapping observations, life history parameters, and
#' advice to their corresponding new S4 slots.
#'
#' @param Data A [Data-legacy-class] object to convert.
#' @param Seasons Integer. Number of seasons per year. Default `1`.
#' @param sim Integer. Simulation index to extract from the legacy object.
#'   Default `1`.
#' @param silent Logical. If `TRUE`, suppresses progress messages. Default
#'   `FALSE`.
#'
#' @details
#' Legacy `Data` objects do not distinguish between landings and discards.
#' `Data@Cat` is mapped to `Landings` only; `Discards` will be empty in the
#' resulting [data-class] object. If `Data@Cat` represents total removals,
#' the `Discards` slot must be populated manually after conversion.
#'
#' The following legacy slots are not currently converted and are silently
#' dropped:
#'
#' - Additional indices: `SpInd`, `VInd`, `AddInd`, `AddIndType`, `AddIndV`,
#'   `AddIunits` (a warning is emitted if `AddInd` is non-empty)
#' - Mean lengths: `ML`, `Lc`, `Lbar`
#' - Abundance: `Abun`, `SpAbun`
#' - Reference points: `FMSY_M`, `BMSY_B0`, `Bref`, `Iref`
#' - Recruitment: `Rec`
#' - Average catch/depletion: `AvC`, `Dt`
#' - Ref slot: `Ref`, `Ref_type`
#'
#' @return A [data-class] object populated from simulation `sim` of `Data`.
#'
#' @seealso [Convert()], [ConvertOM()], [ConvertMOM()]
#'
#' @examples
#' \dontrun{
#' Datalegacy <- readRDS("MyLegacyData.rds")
#' data_new <- ConvertData(Datalegacy, sim = 1)
#' }
#'
#' @export
ConvertData <- function(Data, Seasons = 1, sim = 1, silent = FALSE) {
  CheckClass(Data, "Data", "Data")

  if (!silent) 
    cli::cli_alert("Converting object of class {.cls Data} (Simulation {.val {sim}}) to class {.cls data}.")
  
  YearsInfo <- Data2Years(Data, Seasons)
  
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
  
  data <- Data2LifeHistory(Data, data, sim)
  data <- Data2Exploitation(Data, data, sim)
  data <- Data2Effort(Data, data, sim)
  data <- Data2Landings(Data, data, sim)
  data <- Data2CPUE(Data, data, sim)
  data <- Data2Survey(Data, data, sim)
  data <- Data2CAA(Data, data, sim)
  data <- Data2CAL(Data, data, sim)
  data <- Data2Advice(Data, data, sim)
  data
}

Data2Years <- function(Data, Seasons = 1) {
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

Data2LifeHistory <- function(Data, data, sim) {
  
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
    data@LifeHistory@SRR@SD <- list(
      Value = Data@sigmaR[sim],
      CV    = Data@CV_sigmaR[sim]
    )
  }
  
  if (!is.na(Data@Dep[sim])) {
    data@LifeHistory@Depletion@Final <- list(
      Value = Data@Dep[sim],
      CV    = Data@CV_Dep[sim]
    )
  }
  
  data
}

Data2Exploitation <- function(Data, data, sim) {
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

AddYearFleetArray <- function(Value, data, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  nYears <- length(data@Years)
  out <- array(Value, dim = c(nYears, 1))
  dimnames(out) <- list(Year = data@Years, Fleet = fleet_name)
  out
}

AddFleetArray <- function(Value, data, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  out <- array(Value, dim = 1)
  dimnames(out) <- list(Fleet = fleet_name)
  out
}

AddYearAgeFleetArray <- function(Value, data, AgeClasses, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  nYears <- length(data@Years)
  out <- array(Value, dim = c(nYears, length(AgeClasses), 1))
  dimnames(out) <- list(Year = data@Years, Age = AgeClasses, Fleet = fleet_name)
  out
}

AddYearLengthFleetArray <- function(Value, data, LengthClasses, fleet_name = "Fleet 1") {
  if (is.null(Value)) return(Value)
  nYears <- length(data@Years)
  out <- array(Value, dim = c(nYears, length(LengthClasses), 1))
  dimnames(out) <- list(Year = data@Years, Length = LengthClasses, Fleet = fleet_name)
  out
}

Data2Effort <- function(Data, data, sim) {
  if (all(is.na(Data@Effort)))
    return(data)
  data@Effort@Value <- ValorNULL(Data@Effort[sim, ]) |> AddYearFleetArray(data)
  if (!is.null(data@Effort@Value))
    data@Effort@CV <- ValorNULL(Data@CV_Effort[sim, ]) |> AddYearFleetArray(data)
  data
}

Data2Landings <- function(Data, data, sim) {
  if (all(is.na(Data@Cat)))
    return(data)
  data@Landings@Value <- ValorNULL(Data@Cat[sim, ]) |> AddYearFleetArray(data)
  if (is.null(data@Landings@Value))
    return(data)
  if (all(is.na(Data@CV_Cat)))
    return(data)
  data@Landings@CV    <- ValorNULL(Data@CV_Cat[sim, ]) |> AddYearFleetArray(data)
  data@Landings@Units <- Data@Units
  data@Landings@Ref   <- Data@Cref[sim]    |> AddFleetArray(data)
  data@Landings@RefCV <- Data@CV_Cref[sim] |> AddFleetArray(data)
  data
}

Data2CPUE <- function(Data, data, sim) {
  if (all(is.na(Data@Ind)))
    return(data)
  data@CPUE@Value <- ValorNULL(Data@Ind[sim, ]) |> AddYearFleetArray(data)
  if (is.null(data@CPUE@Value))
    return(data)
  if (all(is.na(Data@CV_Ind)))
    return(data)
  data@CPUE@CV    <- ValorNULL(Data@CV_Ind[sim, ]) |> AddYearFleetArray(data)
  if (!is.na(Data@Iref[sim])) 
    data@CPUE@Ref   <- Data@Iref[sim] |> AddFleetArray(data)
  if (!is.na(Data@CV_Iref[sim])) 
    data@CPUE@RefCV <- Data@CV_Iref[sim] |> AddFleetArray(data)
  
  if (!all(is.na(Data@AddInd))) {
    cli::cli_alert_warning(
      "`Data@AddInd` is not currently converted. Manually add to `CPUE(data)` or `Survey(data)` as appropriate."
    )
  }
  data
}

Data2Survey <- function(Data, data, sim) {
  cli::cli_alert_info("`Data2Survey` is not yet implemented. `Survey` slot will be empty.")
  data
}

Data2CAA <- function(Data, data, sim) {
  cli::cli_alert_info("`Data2CAA` is not yet implemented.")
  return(data)
  
  CAAData <- Data@CAA
  if (all(is.na(CAAData)))
    return(data)
  data@CAA@Classes <- seq(0, Data@MaxAge)
  data@CAA@Value   <- abind::adrop(CAAData[sim, , , drop = FALSE], 1) |>
    AddYearAgeFleetArray(data, data@CAA@Classes)
  data
}

Data2CAL <- function(Data, data, sim) {
  cli::cli_alert_info("`Data2CAL` is not yet implemented.")
  return(data)
  
  CALData <- Data@CAL
  if (all(is.na(CALData)))
    return(data)
  data@CAL@Classes <- Data@CAL_mids
  data@CAL@Value   <- abind::adrop(CALData[sim, , , drop = FALSE], 1) |>
    AddYearLengthFleetArray(data, data@CAL@Classes)
  data
}

Data2Advice <- function(Data, data, sim) {
  data@Advice@TAC    <- array(Data@MPrec[sim], 1, dimnames = list(Year = data@YearLH))
  data@Advice@Effort <- array(Data@MPeff[sim], 1, dimnames = list(Year = data@YearLH))
  data
}

