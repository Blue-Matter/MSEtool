
# TODO
# - make logs informative errors for interactive application of CalcAdvice
#   and store as logged error messages for MSE runs

# TODO
# - apply BioEconomic to Effort
ApplyMPAdvice <- function(ProjSim, MP, Year, YearsHist, YearsProj, ManagementYears, Sim) {
  
  YearsAll <- c(YearsHist, YearsProj) 

  MPAdviceList_Previous <- GetPreviousMPAdvice(ProjSim)
  Complexes <- ProjSim@OM@Complexes
  MPData <- GetMPData(ProjSim, Year, YearsAll)
  CheckDataLength(MPData, Complexes)
  
  if (!Year %in% ManagementYears) {
    MPAdviceList <- MPAdviceList_Previous
  } else {
    MPAdviceList <- CalcAdvice(MP, MPData, Sim, Year)
  }
  
  # loop over stocks/complexes
  # TODO - test and clean up
  # for (st in seq_along(MPAdviceList)) { 
  #   MPAdvice <- MPAdviceList[[st]]
  #   ProjSim@Data[[st]]@Misc <- MPAdvice@Misc
  #   
  #   ProjSim <- ProjSim |>
  #     MPLog(MP, MPAdvice, Year) |>
  #     SaveMPTAC(MPAdviceList, st, Year, YearsProj) |> 
  #     SaveMPAdvice(MPAdvice, Year) 
  #   
  # }
  
  ProjSim <- ProjSim |>
    UpdateClosure(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateSelectivity(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateRetention(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateDiscardMortality(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateTAC(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    
    UpdateEffort(MPAdvice, PreviousMPAdvice, YearsAll, YearsHist, TSIndex, st) 
  

      # UpdateApicalF(MPAdvice, Year, TSIndex, st) |>
     
  
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

GetMPData <- function(ProjSim, Year, YearsAll) {
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

SaveMPAdvice <- function(ProjSim, MPAdviceList, Year) {
  if (is.null(ProjSim@Misc$MPAdvice))
    ProjSim@Misc$MPAdvice <- list()
  ProjSim@Misc$MPAdvice[[as.character(Year)]] <- MPAdviceList
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

CheckDataLength <- function(MPData, Complexes) {
  l1 <- length(MPData)
  l2 <- length(Complexes)
  
  if (l1!=l2)
    cli::cli_abort("length(MPData) != length(Complexes)")
}

