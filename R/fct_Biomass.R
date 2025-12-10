# TODO - make compatible with MSE, MMSE, Hist, and MultiHist

# TODO - check for failed sims and drop those from the data frames


# Hist <- MSE@Hist

#' Extract values from an MSE or Hist object
#' 
#' Returns as a data.frame
#' 
#' @param MSE An `mse` class or `hist` object
#' @name Biomass
NULL 


#' 
#' @export
Array2DF <- function(array) {
  if (!inherits(array, c('matrix', 'array'))) 
    cli::cli_abort('`array` in not class `matrix` or `array`')
  
  array2DF(array) |> ConvertDF()
}

MakeFactor <- function(x) {
  factor(x, ordered = TRUE, levels=unique(x))
}


#' @export
DF2Array <- function(DF) {
  if (!inherits(DF, 'data.frame'))
    cli::cli_abort("`DF` is not a data.frame")
  
  nms <- names(DF)
  PosNames <- c("Sim", "Stock", "Age", "Year", "Fleet", "Area") 
  DFNames <- nms[nms %in% PosNames]
  PosNames <- PosNames[PosNames %in% DFNames]
  
  df <- DF |> 
    dplyr::select(dplyr::all_of(DFNames), 'Value') |>
    dplyr::relocate(dplyr::all_of(PosNames)) 
  
  temp <- df
  temp$Value <- NULL
  DimNames <- apply(temp, 2, unique, simplify = FALSE)
  Dim <- lapply(DimNames, length)
  array(df$Value, dim=Dim, dimnames=DimNames)
}

ArrangeDF <- function(df) {
  
  cnames <- colnames(df)
  colInd <- c('Sim', 'Year', 'Age') %in% cnames
  
  if (prod(colInd))
    return(
      df |> dplyr::arrange(Sim, Year, Age)
    )
    
  if (prod(colInd[1:2]))
    return(
      df |> dplyr::arrange(Sim, Year)
    )
  
  if (prod(colInd[c(1,3)]))
    return(
      df |> dplyr::arrange(Sim, Age)
    )
  
  if (prod(colInd[c(2,3)]))
    return(
      df |> dplyr::arrange(Year, Age)
    )
  
 df
}

ConvertDF <- function(df) {
  nms <- colnames(df)
  if ('Sim' %in% nms)
    df$Sim <- as.numeric(df$Sim)
  if ('Age' %in% nms)
    df$Age <- as.numeric(df$Age)
  if ('Class' %in% nms)
    df$Class <- as.numeric(df$Class)
  if ('Stock' %in% nms)
    df$Stock <- MakeFactor(df$Stock)
  if ('Fleet' %in% nms)
    df$Fleet <- MakeFactor(df$Fleet)
  # if ('MP' %in% nms)
  #   df$MP <- MakeFactor(df$MP)
  
  if ('Year' %in% nms)
    df$Year <- as.numeric(df$Year)
  if ('Year' %in% nms)
    df$Year <- as.numeric(df$Year)
  if ('Value' %in% nms)
    df$Value <- as.numeric(df$Value)
  
  df |> tibble::as_tibble()
}




YearsDF <- function(MSE) {
  if (inherits(MSE, 'mse') | inherits(MSE, 'hist')) 
    return(
      dplyr::bind_rows(data.frame(Year=Years(MSE@OM, 'Historical'), Period='Historical'),
                       data.frame(Year=Years(MSE@OM, 'Projection'), Period='Projection'))
    )
  if (inherits(MSE, 'om')) 
    return(
      dplyr::bind_rows(data.frame(Year=Years(OM, 'Historical'), Period='Historical'),
                       data.frame(Year=Years(OM, 'Projection'), Period='Projection'))
    )
}

AdjustYears <- function(MSE, Years, YearsDF) {
  if (inherits(MSE, 'hist') & !is.null(Years))
    Years <- Years[Years %in% (YearsDF |> dplyr::filter(Period =='Historical') |> dplyr::pull('Year'))]
  Years
}

# DF_ExpandSim <- function(DF, Sims) {
#   missingSims <- Sims[!Sims %in% DF$Sim]
#   if (!length(missingSims))
#     return(DF)
#   DF_List <- purrr::map(Sims, \(value)  
#                         DF |> dplyr::mutate(!!var := value)
#   )
#   do.call('rbind', DF_List) 
# }

GetMSYRefValue <- function(MSE, Metric='FMSY', Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  if (Ref !="Equilibrium")
    cli::cli_alert_warning("Only Equilibrium msy reference points are currently working")
  
  RefValue <- try(slot(MSE@RefPoints, Metric), silent=TRUE)
  if (inherits(RefValue, 'try-error')) {
    RefValue <- try(slot(MSE@RefPoints@MSY, Metric), silent=TRUE)
  }

  
  YearsDF <- YearsDF(MSE) 
  if (is.null(Years))
    Years <- YearsDF$Year
  Years <- AdjustYears(MSE, Years, YearsDF)
  
  if (Expand) {
    RefValue <- RefValue |> ArrayExpand(MSE@OM@nSim, NULL,  Years)
  }
  
  RefValue |> ArraySubsetYear(Years) |> 
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable=Metric) |>
    dplyr::left_join(YearsDF, by='Year') |>
    dplyr::select("Sim", "Stock", "Year", "Period", "Value", "Variable")
  
}

# ----- Fishing Mortality ----
#' @describeIn Biomass Apical Fishing Mortality
#' @export
apicalF <- function(MSE, Type=c('Dead', 'Retain'), byFleet=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')

  Type <- match.arg(Type)
  
  if (inherits(MSE, 'hist')) {
    return(apicalFHist(MSE, Type, byFleet))
  }
    
  apicalFHist <- apicalFHist(MSE, Type, byFleet)
  apicalFHist$MP <- 'Historical'
  
  if (Type=='Dead') {
    Values <- MSE@FDead
  } else {
    Values <- MSE@FRetain
  }
  
  apicalF <- purrr::map(Values, \(stock) {
    if (byFleet) {
      apply(stock, c('Sim', 'Age', 'Year', 'Fleet', 'MP'), sum) |>
        apply(c('Sim', 'Year', 'Fleet', 'MP'), max)  
    } else {
      apply(stock, c('Sim', 'Age', 'Year', 'MP'), sum) |>
        apply(c('Sim', 'Year', 'MP'), max)
    }
  }) |> List2Array('Stock') |>
    array2DF()
  
  apicalF$Period <- 'Projection'
  apicalF$Variable <- "apicalF"
  apicalF <- ConvertDF(apicalF)

  apicalF <- dplyr::bind_rows(apicalFHist, apicalF) |>
    dplyr::arrange(Sim, Year, Period)
  
  ConvertDF(apicalF)
}

apicalFHist <- function(Hist, Type=c('Dead', 'Retain'), byFleet=FALSE) {
  CheckClass(Hist, c('hist', 'mse'))
  Type <- match.arg(Type)
  
  if (inherits(Hist, 'mse')) {
    if (Type=='Dead') {
      Values <- Hist@Hist@FDead
    } else {
      Values <- Hist@Hist@FRetain
    }
  } else {
    if (Type=='Dead') {
      Values <- Hist@FDead
    } else {
      Values <- Hist@FRetain
    }
  }

  apicalF <- purrr::map(Values, \(stock) {
    if (byFleet) {
      apply(stock, c('Sim', 'Age', 'Year', 'Fleet'), sum) |>
        apply(c('Sim', 'Year', 'Fleet'), max)  
    } else {
      apply(stock, c('Sim', 'Age', 'Year'), sum) |>
        apply(c('Sim', 'Year'), max)
    }
  }) |> List2Array('Stock') |>
    array2DF() 
  
  if (byFleet) {
    apicalF <- apicalF |> dplyr::select(c('Sim', 'Stock', 'Year', 'Fleet', 'Value'))
  } else {
    apicalF <- apicalF |>  dplyr::select(c('Sim', 'Stock', 'Year', 'Value'))
  }
  apicalF$Period <- 'Historical'
  apicalF$Variable <- "apicalF"
  
  ConvertDF(apicalF)
}

#' @describeIn Biomass FMSY
#' @export
FMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  GetMSYRefValue(MSE, Metric='FMSY', Ref, Years, Expand)
}

#' @describeIn Biomass F_FMSY
#' @export
F_FMSY <- function(MSE, Years=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  RefValue <- FMSY(MSE, Years=Years, Expand=TRUE) |>
    dplyr::rename(FMSY=Value) |>
    dplyr::select(-Variable)
  
  apicalF(MSE) |> 
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, Year, Period)) |>
    dplyr::mutate(Value=Value/FMSY,
                  Variable='F_FMSY') |>
    ConvertDF()
}

# ----- Biomass ----

#' @describeIn Biomass Total Biomass
#' @export
Biomass <- function(MSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  if (inherits(MSE, 'hist')) 
    return(BiomassHist(MSE))
  
  HistBiomass <- BiomassHist(MSE)
  HistBiomass$MP <- 'Historical'
  
  ProjBiomass <- array2DF(MSE@Biomass)
  ProjBiomass$Period <- 'Projection'
  ProjBiomass$Variable <- "Biomass"
  ProjBiomass <- ConvertDF(ProjBiomass)
  
  units <- lapply(MSE@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |>  unlist() 
  
  ProjBiomass <- ProjBiomass |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
 
  ProjBiomass <- dplyr::bind_rows(HistBiomass, ProjBiomass) |>
    dplyr::arrange(Sim, Year, Period)
  
  ConvertDF(ProjBiomass) 
}

BiomassHist <- function(Hist) {
  CheckClass(Hist, c('hist', 'mse'), 'Hist')
  HistYear <- Years(Hist@OM, "Historical")
  
  if (inherits(Hist, 'mse')) {
    hist <- array2DF(Hist@Hist@Biomass)
  } else {
    hist <- array2DF(Hist@Biomass)  
  }
  
  hist$Period <- 'Historical'
  hist$Variable <- "Biomass"
  hist <- ConvertDF(hist)
  
  units <- lapply(Hist@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist()
  
  hist <- hist |>
    dplyr::filter(Year%in%HistYear) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  hist
}

#' @describeIn Biomass Unfished Biomass 
#' @param Ref Character string specifying the reference point to use `('Equilibrium', 'Dynamic)` 
#' @param Years Numeric value specifying the time step(s) to use for the reference point. Defaults to all time-steps.
#' @export
B0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  if (Ref=='Equilibrium') {
    RefValue <- MSE@Unfished@Equilibrium@Biomass   
  } else {
    RefValue <- MSE@Unfished@Dynamic@Biomass 
  }
  
  YearsDF <- YearsDF(MSE) 
  if (is.null(Years))
    Years <- YearsDF$Year
  
  Years <- AdjustYears(MSE, Years, YearsDF)
  
  if (Expand) {
    RefValue <- RefValue |> ArrayExpand(MSE@OM@nSim, NULL,  Years)
  }
  
  RefValue |> ArraySubsetYear(Years) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable='B0') |>
    dplyr::left_join(YearsDF, by='Year') |>
    dplyr::select("Sim" ,"Stock", "Year", "Period", "Value", "Variable")
}

#' @describeIn Biomass Total Biomass relative to Unfished Biomass 
#' @param Ref Character string specifying the reference point to use `('Equilibrium', 'Dynamic)` 
#' @param Years Numeric value specifying the time step(s) to use for the reference point. Defaults to all time-steps.
#' @export
B_B0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- B0(MSE, Ref, Years, Expand=TRUE) |>
    dplyr::rename(B0=Value) |>
    dplyr::select(-Variable)
  
  Biomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, Year, Period)) |> 
    dplyr::mutate(Value=Value/B0,
                  Variable='B_B0') |>
    ConvertDF()
}

#' @describeIn Biomass BMSY
#' @export
BMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  GetMSYRefValue(MSE, Metric='BMSY', Ref, Years, Expand)
}

#' @describeIn Biomass Total Biomass relative to Biomass corresponding with MSY
#' @export
B_BMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- BMSY(MSE, Ref, Years, Expand=TRUE) |>
    dplyr::rename(BMSY=Value) |>
    dplyr::select(-Variable)
  
  Biomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, Year, Period)) |> 
    dplyr::mutate(Value=Value/BMSY,
                  Variable='B_BMSY') |>
    ConvertDF()
  
}

# ---- Spawning Biomass ----

#' @describeIn Biomass Spawning Biomass
#' @export
SBiomass <- function(MSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  if (inherits(MSE, 'hist')) 
    return(SBiomassHist(MSE))
  
  HistSBiomass <- SBiomassHist(MSE)
  HistSBiomass$MP <- 'Historical'
  
  ProjSBiomass <- array2DF(MSE@SBiomass)
  ProjSBiomass$Period <- 'Projection'
  ProjSBiomass$Variable <- "SBiomass"
  ProjSBiomass <- ConvertDF(ProjSBiomass)
  
  units <- lapply(MSE@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist() 
  ProjSBiomass <- ProjSBiomass |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
  ProjSBiomass <- dplyr::bind_rows(HistSBiomass, ProjSBiomass) |>
    dplyr::arrange(Sim, Year, Period)
  
  ConvertDF(ProjSBiomass) 
}

SBiomassHist <- function(Hist) {
  CheckClass(Hist, c('hist', 'mse'))
  HistYear <- Years(Hist@OM, "Historical")
  if (inherits(Hist,'mse')) {
    hist <- array2DF(Hist@Hist@SBiomass)
  } else {
    hist <- array2DF(Hist@SBiomass)  
  }
  
  hist$Period <- 'Historical'
  hist$Variable <- "SBiomass"
  hist <- ConvertDF(hist)
  
  units <- lapply(Hist@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist()
  
  hist <- hist |>
    dplyr::filter(Year%in%HistYear) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  hist
}

#' @describeIn Biomass Unfished Spawning Biomass 
#' @export
SB0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)

  if (Ref=='Equilibrium') {
    RefValue <- MSE@Unfished@Equilibrium@SBiomass 
  } else {
    RefValue <- MSE@Unfished@Dynamic@SBiomass
  }

  YearsDF <- YearsDF(MSE) 
  if (is.null(Years))
    Years <- YearsDF$Year
  
  Years <- AdjustYears(MSE, Years, YearsDF)
  
  if (Expand) {
    RefValue <- RefValue |> ArrayExpand(MSE@OM@nSim, NULL,  Years)
  }
  
  RefValue |> ArraySubsetYear(Years) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable='SB0') |>
    dplyr::left_join(YearsDF, by='Year') |>
    dplyr::select("Sim" ,"Stock", "Year",  "Period", "Value", "Variable")
}

#' @describeIn Biomass Spawning Biomass relative to Unfished Spawning Biomass 
#' @export
SB_SB0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SB0(MSE, Ref, Years, Expand=TRUE) |>
    dplyr::rename(SB0=Value) |>
    dplyr::select(-Variable)
  
  SBiomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, Year, Period)) |> 
    dplyr::mutate(Value=Value/SB0,
                  Variable='SB_SB0') |>
    ConvertDF()
}

#' @describeIn Biomass SBMSY
#' @export
SBMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  GetMSYRefValue(MSE, Metric='SBMSY', Ref, Years, Expand)
}

#' @describeIn Biomass Spawning Biomass relative to Spawning Biomass corresponding with MSY
#' @export
#' 
SB_SBMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SBMSY(MSE, Ref, Years, Expand=TRUE) |>
    dplyr::rename(SBMSY=Value) |>
    dplyr::select(-Variable)
  
  SBiomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, Year, Period)) |> 
    dplyr::mutate(Value=Value/SBMSY,
                  Variable='SB_SBMSY') |>
    ConvertDF()
}
# ---- Spawning Production ----


#' @describeIn Biomass Spawning Production (e.g. eggs)
#' @export
SProduction <- function(MSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  if (inherits(MSE, 'hist')) 
    return(SProductionHist(MSE))
  
  HistSProduction <- SProductionHist(MSE)
  HistSProduction$MP <- 'Historical'
  
  ProjSProduction <- array2DF(MSE@SProduction)
  ProjSProduction$Period <- 'Projection'
  ProjSProduction$Variable <- "SProduction"
  ProjSProduction <- ConvertDF(ProjSProduction)
  
  units <- lapply(MSE@OM@Stock, slot, 'Fecundity') |> 
    lapply(Units) |> 
    unlist() 
  ProjSProduction <- ProjSProduction |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
  dplyr::bind_rows(HistSProduction, ProjSProduction) |>
    dplyr::arrange(Sim, Year, Period) |>
    ConvertDF()
}

SProductionHist <- function(Hist) {
  CheckClass(Hist, 'hist', 'Hist')
  HistYear <- Years(Hist@OM, "Historical")
  hist <- array2DF(Hist@SProduction)
  hist$Period <- 'Historical'
  hist$Variable <- "SProduction"
  hist <- ConvertDF(hist)
  
  units <- lapply(Hist@OM@Stock, slot, 'Fecundity') |> 
    lapply(Units) |> 
    unlist()
  
  hist |>
    dplyr::filter(Year%in%HistYear) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
}

#' @describeIn Biomass Unfished Spawning Production 
#' @export
SP0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)

  if (Ref=='Equilibrium') {
    RefValue <- MSE@Unfished@Equilibrium@SProduction 
  } else {
    RefValue <- MSE@Unfished@Dynamic@SProduction 
  }
  
  YearsDF <- YearsDF(MSE) 
  if (is.null(Years))
    Years <- YearsDF$Year
  
  Years <- AdjustYears(MSE, Years, YearsDF)
  
  if (Expand) {
    RefValue <- RefValue |> ArrayExpand(MSE@OM@nSim, NULL,  Years)
  }
  
  RefValue |> ArraySubsetYear(Years) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable='SP0') |>
    dplyr::left_join(YearsDF, by='Year') |>
    dplyr::select("Sim" ,"Stock", "Year", "Period", "Value",  "Variable")
}

#' @describeIn Biomass Spawning Production relative to Unfished Spawning Production 
#' @export
SP_SP0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SP0(MSE, Ref, Years, Expand=TRUE) |>
    dplyr::rename(SP0=Value) |>
    dplyr::select(-Variable)
  
  SBiomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, Year, Period)) |> 
    dplyr::mutate(Value=Value/SP0,
                  Variable='SP_SP0') |>
    ConvertDF()
}

#' @describeIn Biomass SPMSY
#' @export
SPMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  GetMSYRefValue(MSE, Metric='SPMSY', Ref, Years, Expand)
}

#' @describeIn Biomass Spawning Production relative to Spawning Production corresponding with MSY
#' @export
SP_SPMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SPMSY(MSE, Ref, Years, Expand=TRUE) |>
    dplyr::rename(SPMSY=Value) |>
    dplyr::select(-Variable)
  
  SProduction(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, Year, Period)) |> 
    dplyr::mutate(Value=Value/SPMSY,
                  Variable='SP_SPMSY') |>
    ConvertDF()
}

# ---- SPR ----

#' @describeIn Biomass SBMSY
#' @export
SPRMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), Years=NULL, Expand=FALSE) {
  GetMSYRefValue(MSE, Metric='SPRMSY', Ref, Years, Expand)
}


# ---- Landings ----

CatchHist <- function(Hist, byAge=FALSE, byFleet=FALSE, byArea=FALSE, 
                      type=c('Landings', 'Discards'),
                      disctype=c('dead', 'alive', 'all')) {
  type <- match.arg(type)
  disctype <- match.arg(disctype)
  
  CheckClass(Hist, c('hist', 'mse'))
  
  HistYear <- Years(Hist@OM, "Historical")
  
  if (inherits(Hist,'mse')) {
    Value <- slot(Hist@Hist, type)
  } else {
    Value <- slot(Hist,type)
  }
  
  varname <- type
  if (type=='Discards') {
    varname <- paste0(type, ' (',disctype,')')
  }
  
  if (disctype=='alive' | disctype=='all') {
    DiscardMortality <- purrr::map(Hist@OM@Fleet, \(fleet) fleet@DiscardMortality@MeanAtAge |>
                                     ArraySubsetYear(HistYear) |>
                                     AddDimension('Area')
    )
    DiscardsAll <- purrr::map2(Value, DiscardMortality, ArrayDivide)
    DiscardsAlive <- purrr::map2(DiscardsAll, Value, ArraySubtract)
    
    if (disctype=='alive') {
      Value <- DiscardsAlive
    } else {
      Value <- DiscardsAll
    }
  }
  
  Value <- purrr::map(Value, \(stock) {
    if (!byFleet & byAge & byArea) {
      stock <- apply(stock, c('Sim',  'Age', 'Year', 'Area'), sum) 
    } 
    if (byFleet & !byAge & byArea) {
      stock <- apply(stock, c('Sim',  'Year', 'Fleet', 'Area'), sum) 
    }
    if (!byFleet & !byAge & byArea) {
      stock <- apply(stock, c('Sim',  'Year', 'Area'), sum)
    }
    if (byFleet & byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Age', 'Year', 'Fleet'), sum)
    }
    if (!byFleet & byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Age', 'Year'), sum)
    }
    if (byFleet & !byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Year', 'Fleet'), sum)
    }
    if (!byFleet & !byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Year'), sum)
    }
    stock
  }) |> List2Array('Stock') 
  
  dnames <- c('Stock', Value |> dimnames() |> names())
  order <- c('Sim', 'Stock', 'Age', 'Year', 'Fleet', 'Area')
  order <- order[order %in% dnames]
  
  Value <- Value |>
    aperm(order) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable=varname, Period='Historical')
  
  units <- lapply(Hist@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist()
  
  Value |>
    dplyr::filter(Year%in%HistYear) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
}

CatchValues <- function(MSE, byAge=FALSE, byFleet=FALSE, byArea=FALSE, type=c('Landings', 'Discards'),
                        disctype=c('dead', 'alive', 'all')) {
  
  type <- match.arg(type,c('Landings', 'Discards'))
  disctype <- match.arg(disctype, c('dead', 'alive', 'all'))
  
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  HistValues <- CatchHist(MSE, byAge, byFleet, byArea,type, disctype)
  HistYear <- Years(MSE@OM, "Historical")
  
  if (inherits(MSE, 'hist')) 
    return(HistValues)
  
  ProjYear <- Years(MSE@OM, "Projection")
  HistValues$MP <- 'Historical'
  
  Value <- slot(MSE,type)
  
  varname <- type
  if (type=='Discards') {
    varname <- paste0(type, ' (', disctype, ')')
  }
  
  if (disctype=='alive' | disctype=='all') {
    DiscardMortality <- purrr::map(MSE@OM@Fleet, \(fleet) fleet@DiscardMortality@MeanAtAge |>
                                     ArraySubsetYear(HistYear) |>
                                     AddDimension('Area') |>
                                     AddDimension('MP') |>
                                     ArraySubsetYear(ProjYear)
    )
    DiscardsAll <- purrr::map2(Value, DiscardMortality, ArrayDivide)
    DiscardsAlive <- purrr::map2(DiscardsAll, Value, ArraySubtract)
    
    if (disctype=='alive') {
      Value <- DiscardsAlive
    } else {
      Value <- DiscardsAll
    }
  }
  
  Value <- purrr::map(Value, \(stock) {
    if (!byFleet & byAge & byArea) {
      stock <- apply(stock, c('Sim',  'Age', 'Year', 'Area', 'MP'), sum) 
    } 
    if (byFleet & !byAge & byArea) {
      stock <- apply(stock, c('Sim',  'Year', 'Fleet', 'Area', 'MP'), sum) 
    }
    if (!byFleet & !byAge & byArea) {
      stock <- apply(stock, c('Sim',  'Year', 'Area', 'MP'), sum)
    }
    if (byFleet & byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Age', 'Year', 'Fleet', 'MP'), sum)
    }
    if (!byFleet & byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Age', 'Year', 'MP'), sum)
    }
    if (byFleet & !byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Year', 'Fleet', 'MP'), sum)
    }
    if (!byFleet & !byAge & !byArea) {
      stock <- apply(stock, c('Sim', 'Year', 'MP'), sum)
    }
    stock
  }) |> 
    List2Array('Stock') 
  
  dnames <- c('Stock', Value |> dimnames() |> names())
  order <- c('Sim', 'Stock', 'Age', 'Year', 'Fleet', 'Area', 'MP')
  order <- order[order %in% dnames]
  
  Value <- Value |>
    aperm(order) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable=varname, Period='Projection')
  
  units <- lapply(MSE@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist() 
  
  Value <- Value |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock')
  
  MPs <- unique(Value$MP)
  nMPs <- length(MPs)
  
  HistValuesList <- replicate(nMPs, HistValues, simplify = FALSE)
  for (i in seq_along(HistValuesList)){
    HistValuesList[[i]]$MP <- MPs[i]
  }
  HistValues <- do.call('rbind', HistValuesList)
  
  dplyr::bind_rows(HistValues, Value) |>  ConvertDF() 
  
}

#' @describeIn Biomass Landings
#' @export
Landings <- function(MSE, byAge=FALSE, byFleet=FALSE, byArea=FALSE) {
  CatchValues(MSE, byAge, byFleet, byArea, 'Landings')
}

# ---- Discards ----

#' @describeIn Biomass Discards
#' @export
Discards <- function(MSE, byAge=FALSE, byFleet=FALSE, byArea=FALSE, type=c('dead', 'alive', 'all')) {
  CatchValues(MSE, byAge, byFleet, byArea, 'Discards', disctype=type)
}

# ---- Removals ----

#' @describeIn Biomass Dead Removals (Landings + Dead Discards)
#' @export
Removals <- function(MSE,  byAge=FALSE, byFleet=FALSE, byArea=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  Removals <- Landings(MSE, byAge, byFleet, byArea)
  Discards <- Discards(MSE, byAge, byFleet, byArea)
  
  Removals$Value <- Removals$Value + Discards$Value
  DF <- suppressMessages(dplyr::left_join(Removals, Discards))
  DF$Variable <- 'Removals'
  DF
}



# ---- MSY ----

#' @describeIn Biomass MSY
#' @export
MSY <- function(MSE, Years=NULL, 
                type=c('Removals', 'Landings')) {
  type <- match.arg(type)
  vals <- GetMSYRefValue(MSE, Metric=paste0('MSY', type), Ref='Equilibrium', Years)
  if (all(is.na(vals$Value))) {
    vals <- GetMSYRefValue(MSE, Metric=paste0('MSY', 'Removals'), Ref='Equilibrium', Years)
    vals$Variable <- paste0('MSY', type)
  }
  vals
}

