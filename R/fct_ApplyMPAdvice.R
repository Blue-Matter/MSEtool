GetPreviousMPAdvice <- function(ProjSim) {
  if (is.null(ProjSim@Misc$MPAdvice))
    return(NULL)
  
  ProjSim@Misc$MPAdvice[[length(ProjSim@Misc$MPAdvice)]]
}
  
  
# TODO
# - make logs informative errors for interactive application of CalcAdvice
#   and store as logged error messages for MSE runs

# TODO
# - apply BioEconomic to Effort
ApplyMPAdvice <- function(ProjSim, MP, Year, YearsHist, YearsProj, ManagementYears) {
  
  YearsAll <- c(YearsHist, YearsProj) 

  MPAdviceList_Previous <- GetPreviousMPAdvice(ProjSim)
  Complexes <- ProjSim@OM@Complexes
  MPData <- GetMPData(ProjSim, Year, YearsAll)
  CheckDataLength(MPData, Complexes)
  
  if (!Year %in% ManagementYears) {
    MPAdviceList <- MPAdviceList_Previous
  } else {
    MPAdviceList <- CalcAdvice(MP, MPData, Year)
  }
  
  ProjSim <- SaveMPAdvice(ProjSim, MPAdviceList, Year) 
  
  # loop over stocks/complexes
  for (st in seq_along(MPAdviceList)) {
    MPAdvice <- MPAdviceList[[st]]
    ProjSim@Data[[st]]@Misc <- MPAdvice@Misc
    ProjSim <- ProjSim |>
      MPLog(MP, MPAdvice, Year) |>
      SaveMPTAC(MPAdviceList, st, Year, YearsProj) 

  }
  
  ProjSim <- ProjSim |>
    UpdateClosure(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateSelectivity(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateRetention(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateDiscardMortality(MPAdviceList, MPAdviceList_Previous, Year, YearsProj) |>
    UpdateEffort(MPAdviceList, MPAdviceList_Previous, Year, YearsHist, YearsProj) |>
    UpdateTAC(MPAdviceList, MPAdviceList_Previous, Year, YearsAll) 
   
  
  ProjSim
}


GetMPData <- function(ProjSim, Year, YearsAll) {
  TSIndex <- match(Year, YearsAll)
 
  DataYear <- YearsAll[TSIndex - (ProjSim@OM@DataLag+ProjSim@OM@Seasons)]
  
  MPData <- purrr::map(ProjSim@Data, \(Data) {
    if (DataYear<Data@YearLH)
      return(Data)
    DataTrim(Data, Year=DataYear) 
  })
  MPData
}

CalcAdvice <- function(MP, Data, Year=NULL) {
  MPFunction <- get(MP)
  
  if (inherits(MPFunction, 'mmp'))
    return(CalcAdvice_MMP(MP, Data, Year))
  
  CalcAdvice_MP(MP, Data, Year) 
}

CalcAdvice_MP <- function(MP, Data, Year=NULL) {
  MPAdviceList <- MakeNamedList(names(Data))
  MPFunction <- get(MP)
  for (i in seq_along(Data)) { 
    MPAdvice <- try(MPFunction(Data=Data[[i]]), silent=TRUE)
    MPErrorLog(MPAdvice, Year)
    MPAdviceList[[i]] <- MPAdvice
  }
  MPAdviceList
}

CalcAdvice_MMP <- function(MP, Data, Year=NULL) {
  # TODO 
  cli::cli_abort("MP class `mmp` currently not supported", call=NULL)
  
  MPAdvice <- MakeNamedList(names(Data))
  for (i in seq_along(Data)) { 
    MPAdvice[[i]] <- try(MPFunction(Data=Data[[i]]), silent=TRUE)
  }
  MPAdvice
}

SaveMPTAC <- function(ProjSim, MPAdviceList, i, Year, YearsProj) {
  if (!length(ProjSim@Data[[i]]@TAC)) 
    ProjSim@Data[[i]]@TAC <- array(NA, length(YearsProj), dimnames = list(Year=YearsProj))
  ProjSim@Data[[i]]@TAC[match(Year, YearsProj)] <- ifelse(is.null(MPAdviceList[[i]]@TAC), 
                                                          NA, 
                                                          MPAdviceList[[i]]@TAC)
  
  ProjSim
}

SaveMPAdvice <- function(ProjSim, MPAdviceList, Year) {
  if (is.null(ProjSim@Misc$MPAdvice))
    ProjSim@Misc$MPAdvice <- list()
  ProjSim@Misc$MPAdvice[[as.character(Year)]] <- MPAdviceList
  ProjSim
}


MPErrorLog <- function(MPAdvice, Year=NULL) {
  if (inherits(MPAdvice, 'advice')) 
    return(NULL)
  
  if (is.null(Year))  {
    stop(MPAdvice)
    # don't use cli for logging
    # cli::cli_abort(c("x"= "MP {.val {MP}} did not return an object of class {.cls advice}",
    #                  ">"= " Returned object:  {MPAdvice}"), 
    #                call=NULL)
  } else {
    stop(c(MPAdvice, paste('Year = ', Year)))
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

