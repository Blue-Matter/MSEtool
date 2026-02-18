
# TODO add option to specify Interval by MP
CalcManagementYears <- function(YearsProj, Interval) {
  ind <- seq(1, by = Interval, to = length(YearsProj))
  YearsProj[ind]
}

GetLastMPAdvice <- function(Proj) {
  adv <- Proj@Misc$MPAdvice
  if (is.null(adv)) 
    return(NULL)
  adv[[length(adv)]]
}

CalcDataYear <- function(Year, YearsAll, DataLag, Seasons) {
  TSIndex <- match(Year, YearsAll)
  YearsAll[TSIndex - (DataLag + Seasons)]
}

TrimMPData <- function(Proj, DataYear) {
  purrr::map(Proj@Data, \(DataSim) {
    purrr::map(DataSim, \(Data) {
      if (DataYear < Data@YearLH)
        return(Data)
      DataTrim(Data, Year = DataYear)
    })
  })
}

CheckMPDataCompleteness <- function(DataSimList, Complexes) {
  purrr::walk(DataSimList, \(DataComplex) {
    if (length(DataComplex) != length(Complexes)) {
      cli::cli_abort(
        "length(MPData) != length(Complexes)"
      )
    }
  })
  invisible(TRUE)
}

RunMPIfNeeded <- function(Year,
                          ManagementYears,
                          LastAdviceSimList,
                          MPName,
                          MPfunction,
                          DataSimList,
                          Proj,
                          YearsProj,
                          mp,
                          FleetNames,
                          Areas) {
  if (!Year %in% ManagementYears) {
    return(LastAdviceSimList)
  }
  
  CalcAdvice(
    MPName,
    MPfunction,
    DataSimList,
    Year,
    Proj,
    YearsProj,
    mp,
    FleetNames,
    Areas
  )
}

StoreMPAdvice <- function(Proj, Year, AdviceSimList) {
  if (is.null(Proj@Misc$MPAdvice)) {
    Proj@Misc$MPAdvice <- list()
  }
  Proj@Misc$MPAdvice[[as.character(Year)]] <- AdviceSimList
  Proj
}

ApplyAdviceMiscToData <- function(DataList, AdviceList) {
  purrr::map2(DataList, AdviceList, \(Data, Advice) {
    Data@Misc <- Advice@Misc
    Data
  })
}

ExtractAdviceLogs <- function(AdviceSimList) {
  purrr::map(AdviceSimList, \(AdviceList) {
    purrr::map(AdviceList, \(Advice) {
      log <- Advice@Log
      if (!length(log)) 
        return(NULL)
      log
    })
  })
}

UpdateAdviceArray <- function(Current, New, Year) {
  New <- AddDimension(New, 'Year', Year, pos = 1)
  if (is.null(Current)) {
    return(New)
  }
  if (is.null(dimnames(Current)))
    return(Current)
  
  ArrayFill(Current) <- New
  Current
}

AddAdviceToData <- function(Data, Advice, Year) {
  Data@Advice@TAC <- UpdateAdviceArray(Current=Data@Advice@TAC, New=Advice@TAC, Year)
  Data@Advice@Effort <- UpdateAdviceArray(Data@Advice@Effort, Advice@Effort, Year)
  Data
}

UnchangedManagement <- function(Current, Previous, slotName) {
  
  if (is.null(Current))
    return(TRUE)
  
  if (!.hasSlot(Current, slotName))
    return(TRUE)
  
  cur <- slot(Current, slotName)
  
  if (is.null(cur) || !length(cur))
    return(TRUE)
  
  if (is.null(Previous) || !.hasSlot(Previous, slotName))
    return(FALSE)
  
  IdenticalS4(cur, slot(Previous, slotName))
}