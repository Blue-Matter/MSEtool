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


MakeFactor <- function(x) {
  factor(x, ordered = TRUE, levels=unique(x))
}

#' @export
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
  if ('TimeStep' %in% nms)
    df$TimeStep <- as.numeric(df$TimeStep)
  if ('Value' %in% nms)
    df$Value <- as.numeric(df$Value)
  
  df |> tibble::as_tibble()
}


TimeStepsDF <- function(MSE) {
  if (inherits(MSE, 'mse') | inherits(MSE, 'hist')) 
    return(
      dplyr::bind_rows(data.frame(TimeStep=TimeSteps(MSE@OM, 'Historical'), Period='Historical'),
                       data.frame(TimeStep=TimeSteps(MSE@OM, 'Projection'), Period='Projection'))
    )
  if (inherits(MSE, 'om')) 
    return(
      dplyr::bind_rows(data.frame(TimeStep=TimeSteps(OM, 'Historical'), Period='Historical'),
                       data.frame(TimeStep=TimeSteps(OM, 'Projection'), Period='Projection'))
    )
}

AdjustTimeSteps <- function(MSE, TimeSteps, TimeStepsDF) {
  if (inherits(MSE, 'hist') & !is.null(TimeSteps))
    TimeSteps <- TimeSteps[TimeSteps %in% (TimeStepsDF |> dplyr::filter(Period =='Historical') |> dplyr::pull('TimeStep'))]
  TimeSteps
}

GetMSYRefValue <- function(MSE, Metric='FMSY', Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  if (Ref !="Equilibrium")
    cli::cli_alert_warning("Only Equilibrium msy reference points are currently working")
  
  RefValue <- try(slot(MSE@RefPoints@MSYRefPoints, Metric), silent=TRUE)
  if (inherits(RefValue, 'try-error')) {
    RefValue <- try(slot(MSE@RefPoints@MSY, Metric), silent=TRUE)
  }
  
  TimeStepsDF <- TimeStepsDF(MSE) 
  if (is.null(TimeSteps))
    TimeSteps <- TimeStepsDF$TimeStep
  TimeSteps <- AdjustTimeSteps(MSE, TimeSteps, TimeStepsDF)
  
  RefValue |> ArraySubsetTimeStep(TimeSteps) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable=Metric) |>
    dplyr::left_join(TimeStepsDF, by='TimeStep') |>
    dplyr::select("Sim", "Stock", "TimeStep", "Period", "Value", "Variable")
  
}

# ----- Fishing Mortality ----
#' @describeIn Biomass Apical Fishing Mortality
#' @export
apicalF <- function(MSE, Type=c('Dead', 'Retain'), ByFleet=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')

  Type <- match.arg(Type)
  
  if (inherits(MSE, 'hist')) {
    return(apicalFHist(MSE, Type, ByFleet))
  }
    
  apicalFHist <- apicalFHist(MSE, Type, ByFleet)
  apicalFHist$MP <- 'Historical'
  
  if (Type=='Dead') {
    Values <- MSE@FDeadAtAge
  } else {
    Values <- MSE@FRetainAtAge
  }
  
  apicalF <- purrr::map(Values, \(stock) {
    if (ByFleet) {
      apply(stock, c('Sim', 'Age', 'TimeStep', 'Fleet', 'MP'), sum) |>
        apply(c('Sim', 'TimeStep', 'Fleet', 'MP'), max)  
    } else {
      apply(stock, c('Sim', 'Age', 'TimeStep', 'MP'), sum) |>
        apply(c('Sim', 'TimeStep', 'MP'), max)
    }
  }) |> List2Array('Stock') |>
    array2DF()
  
  apicalF$Period <- 'Projection'
  apicalF$Variable <- "apicalF"
  apicalF <- ConvertDF(apicalF)

  apicalF <- dplyr::bind_rows(apicalFHist, apicalF) |>
    dplyr::arrange(Sim, TimeStep, Period)
  
  ConvertDF(apicalF)
}

apicalFHist <- function(Hist, Type=c('Dead', 'Retain'), ByFleet=FALSE) {
  CheckClass(Hist, c('hist', 'mse'))
  Type <- match.arg(Type)
  
  if (inherits(Hist, 'mse')) {
    if (Type=='Dead') {
      Values <- Hist@Hist@FDeadAtAge
    } else {
      Values <- Hist@Hist@FRetainAtAge
    }
  } else {
    if (Type=='Dead') {
      Values <- Hist@FDeadAtAge
    } else {
      Values <- Hist@FRetainAtAge
    }
  }

  apicalF <- purrr::map(Values, \(stock) {
    if (ByFleet) {
      apply(stock, c('Sim', 'Age', 'TimeStep', 'Fleet'), sum) |>
        apply(c('Sim', 'TimeStep', 'Fleet'), max)  
    } else {
      apply(stock, c('Sim', 'Age', 'TimeStep'), sum) |>
        apply(c('Sim', 'TimeStep'), max)
    }
  }) |> List2Array('Stock') |>
    array2DF() 
  
  if (ByFleet) {
    apicalF <- apicalF |> dplyr::select(c('Sim', 'Stock', 'TimeStep', 'Fleet', 'Value'))
  } else {
    apicalF <- apicalF |>  dplyr::select(c('Sim', 'Stock', 'TimeStep', 'Value'))
  }
  apicalF$Period <- 'Historical'
  apicalF$Variable <- "apicalF"
  
  ConvertDF(apicalF)
}

#' @describeIn Biomass FMSY
#' @export
FMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  GetMSYRefValue(MSE, Metric='FMSY', Ref, TimeSteps)
}

#' @describeIn Biomass F_FMSY
#' @export
F_FMSY <- function(MSE, TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  RefValue <- FMSY(MSE, TimeSteps) |>
    dplyr::rename(FMSY=Value) |>
    dplyr::select(-Variable)

  apicalF <- apicalF(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, TimeStep, Period))
  
  apicalF |> 
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
    lapply(Units) |> 
    unlist() 
  ProjBiomass <- ProjBiomass |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
 
  ProjBiomass <- dplyr::bind_rows(HistBiomass, ProjBiomass) |>
    dplyr::arrange(Sim, TimeStep, Period)
  
  ConvertDF(ProjBiomass) 
}

BiomassHist <- function(Hist) {
  CheckClass(Hist, c('hist', 'mse'), 'Hist')
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  
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
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  hist
}

#' @describeIn Biomass Unfished Biomass 
#' @param Ref Character string specifying the reference point to use `('Equilibrium', 'Dynamic)` 
#' @param TimeSteps Numeric value specifying the time step(s) to use for the reference point. Defaults to all time-steps.
#' @export
B0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  if (Ref=='Equilibrium') {
    RefValue <- MSE@Unfished@Equilibrium@Biomass   
  } else {
    RefValue <- MSE@Unfished@Dynamic@Biomass 
  }
  
  TimeStepsDF <- TimeStepsDF(MSE) 
  if (is.null(TimeSteps))
    TimeSteps <- TimeStepsDF$TimeStep
  
  TimeSteps <- AdjustTimeSteps(MSE, TimeSteps, TimeStepsDF)
  
  RefValue |> ArraySubsetTimeStep(TimeSteps) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable='B0') |>
    dplyr::left_join(TimeStepsDF, by='TimeStep') |>
    dplyr::select("Sim" ,"Stock", "TimeStep", "Period", "Value", "Variable")
}

#' @describeIn Biomass Total Biomass relative to Unfished Biomass 
#' @param Ref Character string specifying the reference point to use `('Equilibrium', 'Dynamic)` 
#' @param TimeSteps Numeric value specifying the time step(s) to use for the reference point. Defaults to all time-steps.
#' @export
B_B0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- B0(MSE, Ref, TimeSteps) |>
    dplyr::rename(B0=Value) |>
    dplyr::select(-Variable)
  
  Biomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, TimeStep, Period)) |> 
    dplyr::mutate(Value=Value/B0,
                  Variable='B_B0') |>
    ConvertDF()
}

#' @describeIn Biomass BMSY
#' @export
BMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  GetMSYRefValue(MSE, Metric='BMSY', Ref, TimeSteps)
}

#' @describeIn Biomass Total Biomass relative to Biomass corresponding with MSY
#' @export
B_BMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- BMSY(MSE, Ref, TimeSteps) |>
    dplyr::rename(BMSY=Value) |>
    dplyr::select(-Variable)
  
  Biomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, TimeStep, Period)) |> 
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
    dplyr::arrange(Sim, TimeStep, Period)
  
  ConvertDF(ProjSBiomass) 
}


SBiomassHist <- function(Hist) {
  CheckClass(Hist, c('hist', 'mse'))
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
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
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  hist
}

#' @describeIn Biomass Unfished Spawning Biomass 
#' @export
SB0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)

  if (Ref=='Equilibrium') {
    RefValue <- MSE@Unfished@Equilibrium@SBiomass 
  } else {
    RefValue <- MSE@Unfished@Dynamic@SBiomass
  }

  TimeStepsDF <- TimeStepsDF(MSE) 
  if (is.null(TimeSteps))
    TimeSteps <- TimeStepsDF$TimeStep
  
  TimeSteps <- AdjustTimeSteps(MSE, TimeSteps, TimeStepsDF)
  
  RefValue |> ArraySubsetTimeStep(TimeSteps) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable='SB0') |>
    dplyr::left_join(TimeStepsDF, by='TimeStep') |>
    dplyr::select("Sim" ,"Stock", "TimeStep",  "Period", "Value", "Variable")
}

#' @describeIn Biomass Spawning Biomass relative to Unfished Spawning Biomass 
#' @export
SB_SB0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SB0(MSE, Ref, TimeSteps) |>
    dplyr::rename(SB0=Value) |>
    dplyr::select(-Variable)
  
  SBiomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, TimeStep, Period)) |> 
    dplyr::mutate(Value=Value/SB0,
                  Variable='SB_SB0') |>
    ConvertDF()
}

#' @describeIn Biomass SBMSY
#' @export
SBMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  GetMSYRefValue(MSE, Metric='SBMSY', Ref, TimeSteps)
}

#' @describeIn Biomass Spawning Biomass relative to Spawning Biomass corresponding with MSY
#' @export
#' 
SB_SBMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SBMSY(MSE, Ref, TimeSteps) |>
    dplyr::rename(SBMSY=Value) |>
    dplyr::select(-Variable)
  
  SBiomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, TimeStep, Period)) |> 
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
    dplyr::arrange(Sim, TimeStep, Period) |>
    ConvertDF()
}


SProductionHist <- function(Hist) {
  CheckClass(Hist, 'hist', 'Hist')
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  hist <- array2DF(Hist@SProduction)
  hist$Period <- 'Historical'
  hist$Variable <- "SProduction"
  hist <- ConvertDF(hist)
  
  units <- lapply(Hist@OM@Stock, slot, 'Fecundity') |> 
    lapply(Units) |> 
    unlist()
  
  hist |>
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
}

#' @describeIn Biomass Unfished Spawning Production 
#' @export
SP0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)

  if (Ref=='Equilibrium') {
    RefValue <- MSE@Unfished@Equilibrium@SProduction 
  } else {
    RefValue <- MSE@Unfished@Dynamic@SProduction 
  }
  
  TimeStepsDF <- TimeStepsDF(MSE) 
  if (is.null(TimeSteps))
    TimeSteps <- TimeStepsDF$TimeStep
  
  TimeSteps <- AdjustTimeSteps(MSE, TimeSteps, TimeStepsDF)
  
  RefValue |> ArraySubsetTimeStep(TimeSteps) |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable='SP0') |>
    dplyr::left_join(TimeStepsDF, by='TimeStep') |>
    dplyr::select("Sim" ,"Stock", "TimeStep", "Period", "Value",  "Variable")
}

#' @describeIn Biomass Spawning Production relative to Unfished Spawning Production 
#' @export
SP_SP0 <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SP0(MSE, Ref, TimeSteps) |>
    dplyr::rename(SP0=Value) |>
    dplyr::select(-Variable)
  
  SBiomass(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, TimeStep, Period)) |> 
    dplyr::mutate(Value=Value/SP0,
                  Variable='SP_SP0') |>
    ConvertDF()
}

#' @describeIn Biomass SPMSY
#' @export
SPMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  GetMSYRefValue(MSE, Metric='SPMSY', Ref, TimeSteps)
}

#' @describeIn Biomass Spawning Production relative to Spawning Production corresponding with MSY
#' @export
SP_SPMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  Ref <- match.arg(Ref)
  
  RefValue <- SPMSY(MSE, Ref, TimeSteps) |>
    dplyr::rename(SPMSY=Value) |>
    dplyr::select(-Variable)
  
  SProduction(MSE) |>
    dplyr::left_join(RefValue, by = dplyr::join_by(Sim, Stock, TimeStep, Period)) |> 
    dplyr::mutate(Value=Value/SPMSY,
                  Variable='SP_SPMSY') |>
    ConvertDF()
}

# ---- SPR ----

#' @describeIn Biomass SBMSY
#' @export
SPRMSY <- function(MSE, Ref=c('Equilibrium', 'Dynamic'), TimeSteps=NULL) {
  GetMSYRefValue(MSE, Metric='SPRMSY', Ref, TimeSteps)
}

# ---- Removals ----





#' @describeIn Biomass Dead Removals (Landings + Discards)
#' @export
Removals <- function(MSE, ByFleet=FALSE, ByAge=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  if (inherits(MSE, 'hist')) 
    return(RemovalsHist(MSE, ByFleet, ByAge))
  
  HistRemovals <- RemovalsHist(MSE, ByFleet, ByAge)
  HistRemovals$MP <- 'Historical'
  
  ProjRemovals <- MSE@Landings + MSE@Discards
  
  if (!ByFleet) {
    ProjRemovals <- ProjRemovals |>
      apply(c('Sim', 'Stock', 'TimeStep', 'MP'), sum)
  } 
  
  ProjRemovals <- array2DF(ProjRemovals) |> 
    dplyr::mutate(Period='Projection', Variable='Removals') |>
    ConvertDF()
 
  units <- lapply(MSE@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist() 
  
  ProjRemovals <- ProjRemovals |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
    
  dplyr::bind_rows(HistRemovals, ProjRemovals) |>
    dplyr::arrange(Sim, TimeStep, Period) |>
    ConvertDF()
}

RemovalsHist <- function(Hist, ByFleet=FALSE, ByAge=FALSE) {
  CheckClass(Hist, c('hist', 'mse'))
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  
  if (inherits(Hist,'mse')) {
    Value <- purrr::map2(Hist@Hist@Landings, Hist@Hist@Discards, \(landings, discards)
                         landings+discards)
  
  } else {
    Value <- purrr::map2(Hist@Landings, Hist@Discards, \(landings, discards)
                         landings+discards)
  }
  
  Removals <- purrr::map(Value, \(stock) {
    if (ByFleet & ByAge) {
      apply(stock, c('Sim',  'Age', 'TimeStep', 'Fleet'), sum) 
    } else if (ByFleet & !ByAge) {
      apply(stock, c('Sim', 'TimeStep', 'Fleet'), sum) 
    } else if (!ByFleet & ByAge) {
      apply(stock, c('Sim', 'Age', 'TimeStep'), sum) 
    } else if (!ByFleet & !ByAge) {
      apply(stock, c('Sim', 'TimeStep'), sum) 
    }
  }) |> List2Array('Stock') |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable="Removals", Period='Historical')
  
  
  units <- lapply(Hist@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist()
  
  Removals |>
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
}


# ---- Landings ----
#' @describeIn Biomass Landings
#' @export
Landings <- function(MSE, ByFleet=FALSE, ByAge=FALSE) {
  CheckClass(MSE, c('mse', 'hist'), 'MSE')
  
  if (inherits(MSE, 'hist')) 
    return(LandingsHist(MSE, ByFleet, ByAge))
  
  HistLandings <- LandingsHist(MSE, ByFleet, ByAge)
  HistLandings$MP <- 'Historical'
  
  ProjLandings <- MSE@Landings
  
  if (!ByFleet) {
    ProjLandings <- ProjLandings |>
      apply(c('Sim', 'Stock', 'TimeStep', 'MP'), sum)
  } 
  
  ProjLandings <- array2DF(ProjLandings) |> 
    dplyr::mutate(Period='Projection', Variable='Landings') |>
    ConvertDF()
  
  units <- lapply(MSE@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist() 
  
  ProjLandings <- ProjLandings |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
  dplyr::bind_rows(HistLandings, ProjLandings) |>
    dplyr::arrange(Sim, TimeStep, Period) |>
    ConvertDF()
}

LandingsHist <- function(Hist, ByFleet=FALSE, ByAge=FALSE) {
  CheckClass(Hist, c('hist', 'mse'))
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  
  if (inherits(Hist,'mse')) {
    Value <- Hist@Hist@Landings
  } else {
    Value <- Hist@Landings
  }
  
  
  Landings <- purrr::map(Value, \(stock) {
    if (ByFleet & ByAge) {
      apply(stock, c('Sim',  'Age', 'TimeStep', 'Fleet'), sum) 
    } else if (ByFleet & !ByAge) {
      apply(stock, c('Sim', 'TimeStep', 'Fleet'), sum) 
    } else if (!ByFleet & ByAge) {
      apply(stock, c('Sim', 'Age', 'TimeStep'), sum) 
    } else if (!ByFleet & !ByAge) {
      apply(stock, c('Sim', 'TimeStep'), sum) 
    }
  }) |> List2Array('Stock') |>
    array2DF() |>
    ConvertDF() |>
    dplyr::mutate(Variable="Landings", Period='Historical')
  
  
  units <- lapply(Hist@OM@Stock, slot, 'Weight') |> 
    lapply(Units) |> 
    unlist()
  
  Landings |>
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
}


#' @describeIn Biomass MSY
#' @export
MSY <- function(MSE, TimeSteps=NULL, 
                type=c('Removals', 'Landings')) {
  type <- match.arg(type)
  vals <- GetMSYRefValue(MSE, Metric=paste0('MSY', type), Ref='Equilibrium', TimeSteps)
  if (all(is.na(vals$Value))) {
    vals <- GetMSYRefValue(MSE, Metric=paste0('MSY', 'Removals'), Ref='Equilibrium', TimeSteps)
    vals$Variable <- paste0('MSY', type)
  }
  vals
}

