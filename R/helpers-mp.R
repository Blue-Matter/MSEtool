
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

#' Reconcile Mismatched `Data@Advice` Record Shapes Across Years
#'
#' `Data@Advice@TAC`/`@Effort` accumulate across years via [ArrayFill<-()],
#' which requires matching dimension names throughout. In practice, a plain
#' per-fleet `Effort` value (`Fleet` only) and the
#' freeze-last-effort default (`Fleet` x `Area`, always built regardless of
#' whether the MP's own advice carried area detail) can legitimately occur
#' in different years for the same MP, which would otherwise abort the
#' whole projection.
#'
#' This is a record-keeping reconciliation only (`Data@Advice` is read by
#' MPs for introspection of past decisions; it does not feed population
#' dynamics) - the smaller array is broadcast across the extra dimension's
#' levels rather than an attempt to reconstruct any real spatial split.
#'
#' @param Current Existing accumulated array (with a `Year` dimension).
#' @param New     This year's array (with a `Year` dimension already added).
#'
#' @return A list with elements `Current` and `New`, each with identical
#'   dimension names (and order), ready for [ArrayFill<-()].
#' @keywords internal
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


#' Process Mean-At-X Slot for a Selectivity or Retention Object
#'
#' Validates and reshapes `MeanAtAge`, `MeanAtLength`, or `MeanAtWeight` slots
#' of a selectivity or retention S4 object to a standardised `[nClass, nArea]`
#' array with `Sim` and `Year` dimensions added. Handles vector, 1-D array,
#' and 2-D array inputs. Clears `@Pars` when a mean-at-x slot is set, as
#' parametric selectivity is no longer used.
#'
#'
#' @param select A `selectivity` or `retention` S4 object.
#' @param Classes Character or numeric vector of class labels (ages, lengths,
#'   or weights).
#' @param nArea Integer. Number of areas.
#' @param type Character. Used in error messages e.g. `'Selectivity'`.
#' @param Year Character or numeric. Current management year.
#' @param slot_name Character. Name of the slot to process: `'MeanAtAge'`,
#'   `'MeanAtLength'`, or `'MeanAtWeight'`.
#' @param dim_name Character. Label for the first dimension in `dimnames`,
#'   e.g. `'Age'` or `'Class'`.
#'
#' @return The `select` object with the specified slot reshaped to
#'   `[Sim, nClass, Year, nArea]` and `@Pars` cleared, or the original
#'   object if the slot is `NULL`.
#' @keywords internal
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

#' @describeIn dot-ProcessSelectMeanAt Process `MeanAtAge` slot.
#' @param Ages An `ages` object with a `@Classes` slot.
#' @keywords internal
.ProcessSelectMeanAtAge <- function(select, Ages, nArea, type, Year) {
  
  
  .ProcessSelectMeanAt(select,
                      Classes   = Ages@Classes,
                      nArea     = nArea,
                      type      = type,
                      Year      = Year,
                      slot_name = 'MeanAtAge',
                      dim_name  = 'Age')
}

#' @describeIn dot-ProcessSelectMeanAt Process `MeanAtLength` slot.
#' @param Length A `length` object with a `@Classes` slot.
#' @keywords internal
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

#' @describeIn dot-ProcessSelectMeanAt Process `MeanAtWeight` slot.
#' @param Weight A `weight` object with a `@Classes` slot.
#' @keywords internal
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
