
.CalcManagementYears <- function(YearsProj, Interval) {
  ind <- seq(1, by = Interval, to = length(YearsProj))
  YearsProj[ind]
}

.ResolveInterval <- function(OMInterval, MPName, MPfunction) {

  nms <- names(OMInterval)

  if (!is.null(nms) && MPName %in% nms)
    return(unname(OMInterval[[MPName]]))

  MPDefault <- attr(MPfunction, 'Interval')
  if (!is.null(MPDefault))
    return(MPDefault)

  if (!is.null(nms)) {
    unnamed <- OMInterval[nms == '']
    if (length(unnamed))
      return(unname(unnamed[[1]]))
  }

  unname(OMInterval[[1]])
}

.ResolveDataOM <- function(OMControlDataOM, MPName, MPfunction) {
  nms <- names(OMControlDataOM)

  if (!is.null(nms) && MPName %in% nms)
    return(OMControlDataOM[[MPName]])

  attr(MPfunction, 'DataOM')
}

.GetLastMPAdvice <- function(Proj) {
  adv <- Proj@Misc$MPAdvice
  if (is.null(adv))
    return(NULL)
  adv[[length(adv)]]
}

.GetLastMPAggBagLimit <- function(Proj) {
  agg <- Proj@Misc$MPAggBagLimit
  if (is.null(agg))
    return(NULL)
  agg[[length(agg)]]
}

.CalcDataYear <- function(Year, YearsAll, DataLag, Seasons) {
  nYears <- length(YearsAll)
  lagYear <- nYears - (DataLag * Seasons)
  YearsAll[lagYear]
}

.TrimMPData <- function(Proj, DataYear) {
  Proj <- .AddSimNumber(Proj)
  purrr::map(Proj@Data, \(DataSim) {
    purrr::map(DataSim, \(Data) {
      if (DataYear < Data@YearLH)
        return(Data)
      DataTrim(Data, Year = DataYear)
    })
  })
}

.CheckMPDataCompleteness <- function(DataSimList, Complexes) {
  purrr::walk(DataSimList, \(DataComplex) {
    if (length(DataComplex) != length(Complexes)) {
      cli::cli_abort(
        "length(MPData) != length(Complexes)"
      )
    }
  })
  invisible(TRUE)
}

.StoreMPAdvice <- function(Proj, Year, AdviceSimList, AggBagLimitSimList = NULL) {
  if (is.null(Proj@Misc$MPAdvice)) {
    Proj@Misc$MPAdvice <- list()
  }
  Proj@Misc$MPAdvice[[as.character(Year)]] <- AdviceSimList

  if (is.null(Proj@Misc$MPAggBagLimit)) {
    Proj@Misc$MPAggBagLimit <- list()
  }
  Proj@Misc$MPAggBagLimit[[as.character(Year)]] <- AggBagLimitSimList

  Proj
}

.ApplyAdviceMiscToData <- function(DataList, AdviceList) {
  if (!is.list(AdviceList))
    return(DataList)
    
  purrr::map2(DataList, AdviceList, \(Data, Advice) {
    if (inherits(Advice, 'advice'))
      Data@Misc <- Advice@Misc
    Data
  })
}

#' Extract Advice Logs and Detect Total MP Failure
#'
#' Flattens per-sim, per-stock `Advice@Log` entries (and any `try-error`/
#' non-`advice` results) into `Proj@Log`, tagging each entry with `sim`,
#' `year`, and `mp` for later filtering. Also determines, directly from
#' `AdviceSimList` rather than from the Log content, whether every sim/stock
#' failed this year - keeping that control-flow decision independent of
#' what an MP chooses to log as a warning or assumption on its own `Advice`.
#'
#' @param AdviceSimList Nested list of `Advice` objects (or `try-error`/
#'   character failures), one per sim, then per stock/complex.
#' @param Proj `Hist` object to record log entries onto.
#' @param Year Integer. Current projection year.
#' @param MPName Character. Name of the management procedure.
#'
#' @return A list with `Proj` (updated) and `AllFailed` (logical - `TRUE` if
#'   every sim/stock combination in `AdviceSimList` was a failure).
#' @keywords internal
.ExtractAdviceLogs <- function(AdviceSimList, Proj, Year, MPName = NULL) {

  IsFailure <- function(Advice) inherits(Advice, c('try-error', 'character'))

  CollectType <- function(type) {
    entries <- list()
    for (sim in seq_along(AdviceSimList)) {
      AdviceList <- AdviceSimList[[sim]]

      if (inherits(AdviceList, 'try-error')) {
        if (type == 'error')
          entries <- c(entries, list(.NewLogEntry(as.character(AdviceList), sim = sim, year = Year, mp = MPName)))
        next
      }

      nms <- names(AdviceList)
      for (j in seq_along(AdviceList)) {
        Advice    <- AdviceList[[j]]
        stockName <- if (!is.null(nms)) nms[j] else ''

        if (inherits(Advice, 'advice')) {
          log <- Advice@Log[[type]]
          for (entry in log) {
            msg <- .LogEntryMessage(entry)
            nm  <- .LogEntryName(entry)
            nm  <- if (nchar(nm)) paste(stockName, nm, sep = ' - ') else stockName
            entries <- c(entries, list(.NewLogEntry(msg, name = nm, sim = sim, year = Year, mp = MPName)))
          }
        } else if (IsFailure(Advice) && type == 'error') {
          entries <- c(entries, list(.NewLogEntry(as.character(Advice), name = stockName, sim = sim, year = Year, mp = MPName)))
        }
      }
    }
    entries
  }

  for (type in c('error', 'warning', 'assumption')) {
    entries <- CollectType(type)
    if (length(entries))
      Proj@Log[[type]] <- c(Proj@Log[[type]], entries)
  }

  AllFailed <- length(AdviceSimList) > 0 && all(purrr::map_lgl(AdviceSimList, \(AdviceList) {
    if (inherits(AdviceList, 'try-error')) return(TRUE)
    if (!length(AdviceList)) return(FALSE)
    all(purrr::map_lgl(AdviceList, IsFailure))
  }))

  list(Proj = Proj, AllFailed = AllFailed)
}

.UpdateAdviceArray <- function(Current, New, Year) {
  if (!length(New))
    return(Current)

  New <- AddDimension(New, 'Year', Year, pos = 1)
  if (is.null(Current)) {
    return(New)
  }
  if (is.null(dimnames(Current)))
    return(Current)

  if (!identical(names(dimnames(Current)), names(dimnames(New)))) {
    reconciled <- .ReconcileAdviceShapes(Current, New)
    Current <- reconciled$Current
    New     <- reconciled$New
  }

  ArrayFill(Current) <- New
  Current
}

.ReconcileAdviceShapes <- function(Current, New) {
  nmC <- names(dimnames(Current))
  nmN <- names(dimnames(New))

  if (identical(c(nmC, "Area"), nmN) && !"Area" %in% nmC)
    return(list(Current = AddDimension(Current, "Area", val = dimnames(New)[["Area"]]), New = New))

  if (identical(c(nmN, "Area"), nmC) && !"Area" %in% nmN)
    return(list(Current = Current, New = AddDimension(New, "Area", val = dimnames(Current)[["Area"]])))

  cli::cli_abort(c(
    "Cannot reconcile `Advice` record shapes across years.",
    "x" = "Dimensions are {.val {nmC}} vs {.val {nmN}}."
  ))
}

.AddAdviceToData <- function(Data, Advice, Year) {
  if (!inherits(Advice, 'advice'))
    return(Data)
  Data@Advice@TAC <- .UpdateAdviceArray(Current=Data@Advice@TAC, New=Advice@TAC, Year)
  Data@Advice@Effort <- .UpdateAdviceArray(Data@Advice@Effort, Advice@Effort, Year)
  Data
}

.UnchangedManagement <- function(Current, Previous, slotName) {
  
  if (is.null(Current))
    return(TRUE)
  
  if (!.hasSlot(Current, slotName))
    return(TRUE)
  
  cur <- slot(Current, slotName)
  
  if (is.null(cur) || !length(cur))
    return(TRUE)
  
  if (is.null(Previous) || !.hasSlot(Previous, slotName))
    return(FALSE)
  
  .IdenticalS4(cur, slot(Previous, slotName))
}



.CheckTACEffort <- function(AdviceSimList, Proj, LHInd, FleetNames) {
  purrr::imap(AdviceSimList, \(AdviceSim, sim) {
    purrr::map(AdviceSim, \(Advice) {
      if (!inherits(Advice, 'advice')) return(Advice)
      if (is.null(Advice@TAC) && is.null(Advice@Effort)) {
          lastdist <- abind::adrop(Proj@Distribution[sim, LHInd,,,drop=FALSE], 1:2)
          lasteff <- array(Proj@Effort[sim, LHInd,], 
                           dimnames = list(
                             Fleet=FleetNames)
                           )  |> AddDimension('Area')
          Advice@Effort <- ArrayMultiply(lastdist, lasteff)
      }
      
      Advice
    })
  })
  
}


.ProcessSelectMeanAt <- function(select, Classes, nArea, type, Year,
                                slot_name, dim_name) {
  
  Values <- slot(select, slot_name)
  if (is.null(Values))
    return(select)
  
  nClass     <- length(Classes)
  dim_names  <- list(Classes, Area=seq_len(nArea))
  names(dim_names)[1] <- dim_name
  
  if (is.array(Values) && length(dim(Values)) == 2) {
    dd <- dim(Values)
    if (!all(dd == c(nClass, nArea)))
      cli::cli_abort(c(
        "{type} {slot_name} array has incorrect dimensions.",
        "x" = "Expected {nClass} rows ({.field nClass}) and {nArea} columns ({.field nArea}).",
        "i" = "Current dimensions: {paste(dd, collapse = ' x ')}."
      ))
    dimnames(Values) <- dim_names
    Values <- ExtendAreas(Values, seq_len(nArea))
    
  } else {
    # vector or 1-D array - validate length then broadcast over areas
    if (length(Values)==1) {
      Values <- rep(Values, nClass)
    }
    if (length(Values) != nClass ) {
      cli::cli_abort(c('x' = "{type}@{slot_name} must be length `nClass` ({.val {nClass}})",
                       'i' = "Currently: {.val {length(Values)}}")
      )
      # Values <- rep(Values, nClass)[seq_len(nClass)]
    }
      
    Values <- array(as.numeric(Values),
                    dim      = c(nClass, 1),
                    dimnames = list(Classes, Area = 1)) |>
      ExtendAreas(seq_len(nArea))
    names(dimnames(Values))[1] <- dim_name
  }
  
  slot(select, slot_name) <- Values |>
    AddDimension('Sim', pos=1) |>
    AddDimension('Year', val=Year, pos=3)
  
  if (type!='DiscardMortality')
    select@Pars <- list()
  select
}

.ProcessSelectMeanAtAge <- function(select, Ages, nArea, type, Year) {
  
  
  .ProcessSelectMeanAt(select,
                      Classes   = Ages@Classes,
                      nArea     = nArea,
                      type      = type,
                      Year      = Year,
                      slot_name = 'MeanAtAge',
                      dim_name  = 'Age')
}

.ProcessSelectMeanAtLength <- function(select, Length, nArea, type, Year) {
  if (is.null(select@Classes))
    select@Classes <- Length@Classes
  .ProcessSelectMeanAt(select,
                      Classes   = select@Classes,
                      nArea     = nArea,
                      type      = type,
                      Year      = Year,
                      slot_name = 'MeanAtLength',
                      dim_name  = 'Class')
}

.ProcessSelectMeanAtWeight <- function(select, Weight, nArea, type, Year) {
  if (is.null(select@Classes))
    select@Classes <- Weight@Classes
  .ProcessSelectMeanAt(select,
                      Classes   = select@Classes,
                      nArea     = nArea,
                      type      = type,
                      Year      = Year,
                      slot_name = 'MeanAtWeight',
                      dim_name  = 'Class')
}
