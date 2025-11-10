
# TODO
# - make logs informative errors for interactive application of CalcAdvice
#   and store as logged error messages for MSE runs

# TODO
# - apply BioEconomic to Effort
ApplyMPAdvice <- function(ProjSim, MP, Year, YearsHist, YearsProj, ManagementYears, Sim) {
  
  YearsAll <- c(YearsHist, YearsProj) 
  TSIndex <- match(Year, YearsAll)
  MPAdvicePrevious <- GetPreviousMPAdvice(ProjSim)
  
  if (!Year %in% ManagementYears) {
    MPAdviceList <- MPAdvicePrevious
  } else {
    MPData <- GetMPData(ProjSim, Year, YearsAll, ManagementYears)
    MPAdviceList <- CalcAdvice(MP, MPData, Sim, Year)
  }
  
  # loop over stocks/complexes  
  for (st in seq_along(MPAdviceList)) { 
    MPAdvice <- MPAdviceList[[st]]
    ProjSim@Data[[st]]@Misc <- MPAdvice@Misc
    
    ProjSim <- ProjSim |>
      MPLog(MP, MPAdvice, Year) |>
      SaveMPTAC(MPAdviceList, st, Year, YearsProj) |> 
      SaveMPAdvice(MPAdvice, Year) 
    
    ProjSim <- ProjSim |>
      UpdateSpatial(MPAdvice, PreviousMPAdvice, Year, YearsProj, st) |>
      UpdateSelectivity(MPAdvice, PreviousMPAdvice, YearsAll, TSIndex, st) |>
      UpdateRetention(MPAdvice, PreviousMPAdvice, YearsAll, TSIndex, st) |>
      UpdateDiscardMortality(MPAdvice, PreviousMPAdvice, YearsAll, TSIndex, st) |>
      UpdateTAC(MPAdvice, TSIndex, st) |>
      UpdateApicalF(MPAdvice, Year, TSIndex, st) |>
      UpdateEffort(MPAdvice, PreviousMPAdvice, YearsAll, YearsHist, TSIndex, st) 
  }
  ProjSim
}


GetPreviousMPAdvice <- function(ProjSim) {
  if (!is.null(ProjSim@Misc$MPAdvice) & length(ProjSim@Misc$MPAdvice)>0) {
    # Has MPAdvice Changed from last time 
    PreviousMPAdvice <- ProjSim@Misc$MPAdvice[[length(ProjSim@Misc$MPAdvice)]] 
  } else {
    PreviousMPAdvice <- NULL
  }
  PreviousMPAdvice
}

GetMPData <- function(ProjSim, Year, YearsAll, ManagementYears) {
  TSIndex <- match(Year, YearsAll)
  DataYear <- YearsAll[TSIndex - (ProjSim@OM@DataLag+1)]
  
  MPData <- purrr::map(ProjSim@Data, \(Data) 
                       DataTrim(Data, Year=DataYear)
  )
  MPData
}

CalcAdvice <- function(MP, Data, Sim=NULL, Year=NULL) {
  MPFunction <- get(MP)
  
  if (inherits(MPFunction, 'mmp'))
    return(CalcAdvice_MMP(MP, Data, Sim, Year))
  
  CalcAdvice_MP(MP, Data, Sim, Year) 
}

CalcAdvice_MP <- function(MP, Data, Sim=NULL, Year=NULL) {
  MPAdviceList <- MakeNamedList(names(MPData))
  MPFunction <- get(MP)
  for (i in seq_along(MPData)) { 
    MPAdvice <- try(MPFunction(Data=MPData[[i]]), silent=TRUE)
    MPErrorLog(MPAdvice, Sim, Year)
    MPAdviceList[[i]] <- MPAdvice
  }
  MPAdviceList
}

CalcAdvice_MMP <- function(MP, Data) {
  # TODO 
  cli::cli_abort("MP class `mmp` currently not supported", call=NULL)
  
  MPAdvice <- MakeNamedList(names(MPData))
  for (i in seq_along(MPData)) { 
    MPAdvice[[i]] <- try(MPFunction(Data=MPData[[i]]), silent=TRUE)
  }
  MPAdvice
}

SaveMPTAC <- function(ProjSim, MPAdviceList, i, Year, YearsProj) {
  if (!length(ProjSim@Data[[i]]@TAC)) 
    ProjSim@Data[[i]]@TAC <- array(NA, length(YearsProj), dimnames = list(Year=YearsProj))
  ProjSim@Data[[i]]@TAC[match(Year, YearsProj)] <- ifelse(is.null(MPAdvice@TAC), 
                                                          NA, 
                                                          MPAdvice@TAC)
  
  ProjSim
}

SaveMPAdvice <- function(ProjSim, MPAdvice, Year) {
  if (is.null(ProjSim@Misc$MPAdvice))
    ProjSim@Misc$MPAdvice <- list()
  ProjSim@Misc$MPAdvice[[as.character(Year)]] <- MPAdvice
  ProjSim
}


MPErrorLog <- function(MPAdvice, Sim=NULL, Year=NULL) {
  if (!inherits(MPAdvice, 'advice')) 
    if (is.null(Sim) || is.null(Year))  {
      cli::cli_abort(c("x"= "MP {.val {MP}} did not return an object of class {.cls advice}",
                       ">"= " Returned object:  {MPAdvice}"), 
                     call=NULL)
    } else {
      cli::cli_abort(c("x"= "MP {.val {MP}} did not return an object of class {.cls advice}",
                       "i"= "Year: {Year}",
                       "i"= "Sim: {Sim}",
                       ">"= " Returned object:  {MPAdvice}"), 
                     call=NULL)
    }
}

MPLog <- function(ProjSim, MP, MPAdvice, Year) {
  if (!length(MPAdvice@Log))
    return(ProjSim)
  if (!length(ProjSim@Log)) 
    ProjSim@Log[[as.character(Year)]] <- list()
  ProjSim@Log[[as.character(Year)]] <- MPAdvice@Log
  ProjSim
}


