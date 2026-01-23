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


#' @rdname Convert
#' @param Data A [Data-class] object
#' @param Sim The simulation number to convert to the new `Data` object
#' @export
ConvertData <- function(Data, Sim = 1, silent = FALSE) {
  CheckClass(Data, "Data", "Data")

  # TODO - DataList??
  if (!silent) {
    cli::cli_alert("Converting object of class {.cls Data} (Simulation {.val {Sim}}) to class {.cls data}.")
  }

  data <- Data2Name(Data)
  data <- Data2LifeHistory(Data, data, Sim)
  data <- Data2Exploitation(Data, data, Sim)

  data <- Data2Effort(Data, data, Sim)
  data <- Data2Landings(Data, data, Sim)
  data <- Data2CPUE(Data, data, Sim)
  data <- Data2Survey(Data, data, Sim) # TODO
  
  data <- Data2CAA(Data, data, Sim)
  data <- Data2CAL(Data, data, Sim)
  data <- Data2Advice(Data, data, Sim)
  

  data
}
Data2Name <- function(Data) {
  data <- Data()
  data@Name <- Data@Name
  data@CommonName <- Data@Common_Name
  data@Species <- Data@Species
  data@Region <- Data@Region

  data@Years <- Data@Year
  data@YearLH <- Data@LHYear

  if (all(data@Years < 1000)) {
    data@YearLH <- as.numeric(format(Sys.Date(), "%Y"))
    data@Years <- seq(data@YearLH, by = -1, length.out = length(data@Years)) |> rev()
  }

  data@Seasons <- 1
  data@nArea <- Data@nareas
  data
}

ValorNULL <- function(Value) {
  if (all(is.na(Value))) {
    return(NULL)
  }
  if (length(Value) < 1) {
    return(NULL)
  }
  Value
}

Data2LifeHistory <- function(Data, data, Sim) {
  data@LifeHistory@Ages@MaxAge <- Data@MaxAge
  data@LifeHistory@Length@Pars <- list(Linf=Data@vbLinf[Sim],
                                       Linf_CV=Data@CV_vbLinf[Sim],
                                       K=Data@vbK[Sim],
                                       K_CV=Data@CV_vbK[Sim],
                                       t0=Data@vbt0[Sim],
                                       t0_CV=Data@CV_vbt0[Sim])
  data@LifeHistory@Length@Model <- 'vonBert'
  data@LifeHistory@Length@CVatAge <- Data@LenCV[Sim]
  
  data@LifeHistory@Weight@Pars <- list(
    alpha=Data@wla[Sim],
    alpha_CV=Data@CV_wla[Sim],
    beta=Data@wlb[Sim],
    beta_CV=Data@CV_wlb[Sim]
  )
  
  data@LifeHistory@NaturalMortality@Pars <- list(
    M=Data@Mort[Sim],
    CV_M=Data@CV_Mort[Sim]
  )
  
  data@LifeHistory@Maturity@Pars <- list(
    L50=Data@L50[Sim],
    L50_CV=Data@CV_L50[Sim],
    L50_95=Data@L95[Sim]-Data@L50[Sim]
  )
    
  data@LifeHistory@SRR@Pars <- list(
    h=Data@steep[Sim],
    h_CV=Data@CV_steep[Sim]
  )
  
  data@LifeHistory@SRR@SD <- list(Value=Data@sigmaR[Sim],
                                  CV=Data@CV_sigmaR[Sim])
  
  data@LifeHistory@Depletion@Final <- list(Value=Data@Dep[Sim],
                                           CV=Data@CV_Dep[Sim])
  
  data
}

Data2Exploitation <- function(Data, data, Sim) {
  data@Exploitation@Selectivity@Pars <- list(
    L5=Data@LFC[Sim],
    L5_CV=Data@CV_LFC[Sim],
    LFS=Data@LFS[Sim],
    LFS_CV=Data@CV_LFS[Sim],
    Vmaxlen=Data@Vmaxlen[Sim]
  )
  data
} 

AddYearFleetArray <- function(Value, data) {
  if (is.null(Value)) {
    return(Value)
  }
  nYears <- data@Years |> length()
  out <- array(Value, dim = c(nYears, 1))
  dimnames(out) <- list(
    Year = data@Years,
    Fleet = "Fleet 1"
  )
  out
}

AddFleetArray <- function(Value, data) {
  if (is.null(Value)) {
    return(Value)
  }
  
  out <- array(Value, dim = 1)
  dimnames(out) <- list(
    Fleet = "Fleet 1"
  )
  out
}

Data2Effort <- function(Data, data, Sim) {
  data@Effort@Value <- ValorNULL(Data@Effort[Sim, ]) |> AddYearFleetArray(data)
  if (!is.null(data@Effort@Value)) {
    data@Effort@CV <- ValorNULL(Data@CV_Effort[Sim, ]) |> AddYearFleetArray(data)
  }
  data
}

Data2Landings <- function(Data, data, Sim) {
  data@Landings@Value <- ValorNULL(Data@Cat[Sim, ]) |> AddYearFleetArray(data)
  if (is.null(data@Landings@Value)) {
    return(data)
  }
  data@Landings@CV <- ValorNULL(Data@CV_Cat[Sim, ]) |> AddYearFleetArray(data)
  data@Landings@Units <- Data@Units
  data@Landings@Ref <- Data@Cref[Sim] |> AddFleetArray(data)
  data@Landings@RefCV <- Data@CV_Cref[Sim] |> AddFleetArray(data)
  data
}

Data2CPUE <- function(Data, data, Sim) {
  data@CPUE@Value <- ValorNULL(Data@Ind[Sim, ]) |> AddYearFleetArray(data)
  if (is.null(data@CPUE@Value)) {
    return(data)
  }
  data@CPUE@CV <- ValorNULL(Data@CV_Ind[Sim, ]) |> AddYearFleetArray(data)
  data@CPUE@Ref <- Data@Iref[Sim] |> AddFleetArray(data)
  data@CPUE@RefCV <- Data@CV_Iref[Sim] |> AddFleetArray(data)
  
  if (!all(is.na(Data@AddInd))) {
    cli::cli_alert_warning('`Data@AddInd` is currently not converted to new `Data` object. \nManually add to `CPUE(Data)` or `Survey(Data)` as appropriate ')
  }
  data
}

Data2Survey <- function(Data, data, Sim) { 
  data
}

AddYearAgeFleetArray <- function(Value, data, AgeClasses) {
  if (is.null(Value)) {
    return(Value)
  }
  nYears <- data@Years |> length()
  out <- array(Value, dim = c(nYears, length(AgeClasses), 1))
  dimnames(out) <- list(
    Year = data@Years,
    Age = AgeClasses,
    Fleet = "Fleet 1"
  )
  out
}

AddYearLengthFleetArray <- function(Value, data, LengthClasses) {
  if (is.null(Value)) {
    return(Value)
  }
  nYears <- data@Years |> length()
  out <- array(Value, dim = c(nYears, length(LengthClasses), 1))
  dimnames(out) <- list(
    Year = data@Years,
    Length = LengthClasses,
    Fleet = "Fleet 1"
  )
  out
}


Data2CAA <- function(Data, data, Sim) {
  CAAData <- Data@CAA 
  if (all(is.na(CAAData))) {
    return(data)
  }
  data@CAA@Classes <- 0:Data@MaxAge
  
  data@CAA@Value <- abind::adrop(CAAData[Sim,,, drop=FALSE],1) |> 
    AddYearAgeFleetArray(data, data@CAA@Classes)
  
  data
}

Data2CAL <- function(Data, data, Sim) {
  CALData <- Data@CAL
  if (all(is.na(CALData))) {
    return(data)
  }
  data@CAL@Classes <- Data@CAL_mids
  
  data@CAL@Value <- abind::adrop(CALData[Sim,,, drop=FALSE],1) |> 
    AddYearLengthFleetArray(data, data@CAL@Classes)
  data
}

Data2Advice <- function(Data, data, Sim) {
  data@Advice@TAC <- array(Data@MPrec[Sim], 1, dimnames = list(Year=data@YearLH))
  data@Advice@Effort  <- array(Data@MPeff[Sim], 1, dimnames = list(Year=data@YearLH))
  data
}
