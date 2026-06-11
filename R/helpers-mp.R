
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
  nYears <- length(YearsAll)
  lagYear <- nYears - (DataLag * Seasons)
  YearsAll[lagYear]
}

TrimMPData <- function(Proj, DataYear) {
  Proj <- AddSimNumber(Proj)
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

StoreMPAdvice <- function(Proj, Year, AdviceSimList) {
  if (is.null(Proj@Misc$MPAdvice)) {
    Proj@Misc$MPAdvice <- list()
  }
  Proj@Misc$MPAdvice[[as.character(Year)]] <- AdviceSimList
  Proj
}

ApplyAdviceMiscToData <- function(DataList, AdviceList) {
  if (!is.list(AdviceList))
    return(DataList)
    
  purrr::map2(DataList, AdviceList, \(Data, Advice) {
    if (inherits(Advice, 'advice'))
      Data@Misc <- Advice@Misc
    Data
  })
}

ExtractAdviceLogs <- function(AdviceSimList, Proj, Year) {
  
  out <- purrr::map(AdviceSimList, \(AdviceList) {
    if (inherits(AdviceList, 'try-error'))
      return(AdviceList)
    
    purrr::map(AdviceList, \(Advice) {
      if (inherits(Advice, 'advice')) {
        log <- Advice@Log
        if (!length(log)) 
          return(NULL)  
        log
      }
      if (inherits(Advice, 'try-error')) {
        return(as.character(Advice))
        
      }
      if (inherits(Advice, 'character')) {
        return(Advice)
        
      }
    })
  })
  
  if (!length(unlist(out)))
    return(Proj)
  
  if (is.null(Proj@Log$error))
    Proj@Log$error <- list()
  
  Proj@Log$error[[as.character(Year)]] <- out
  Proj
}

UpdateAdviceArray <- function(Current, New, Year) {
  if (!length(New))
    return(Current)
  
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
  if (!inherits(Advice, 'advice'))
    return(Data)
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



CheckTACEffort <- function(AdviceSimList, Proj, LHInd, FleetNames) {
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
ProcessSelectMeanAt <- function(select, Classes, nArea, type, Year,
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
       # cli::cli_warn("{type}@{slot_name} must be length `nClass` ({.val {nClass}}). Currently: {.val {length(Values)}}")
       Values <- rep(Values, nClass)[seq_len(nClass)]
    }
      
    Values <- array(as.numeric(Values),
                    dim      = c(nClass, 1),
                    dimnames = list(Classes, Area=1)) |>
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

#' @describeIn ProcessSelectMeanAt Process `MeanAtAge` slot.
#' @param Ages An `ages` object with a `@Classes` slot.
#' @keywords internal
ProcessSelectMeanAtAge <- function(select, Ages, nArea, type, Year) {
  
  
  ProcessSelectMeanAt(select,
                      Classes   = Ages@Classes,
                      nArea     = nArea,
                      type      = type,
                      Year      = Year,
                      slot_name = 'MeanAtAge',
                      dim_name  = 'Age')
}

#' @describeIn ProcessSelectMeanAt Process `MeanAtLength` slot.
#' @param Length A `length` object with a `@Classes` slot.
#' @keywords internal
ProcessSelectMeanAtLength <- function(select, Length, nArea, type, Year) {
  if (is.null(select@Classes))
    select@Classes <- Length@Classes
  ProcessSelectMeanAt(select,
                      Classes   = select@Classes,
                      nArea     = nArea,
                      type      = type,
                      Year      = Year,
                      slot_name = 'MeanAtLength',
                      dim_name  = 'Class')
}

#' @describeIn ProcessSelectMeanAt Process `MeanAtWeight` slot.
#' @param Weight A `weight` object with a `@Classes` slot.
#' @keywords internal
ProcessSelectMeanAtWeight <- function(select, Weight, nArea, type, Year) {
  if (is.null(select@Classes))
    select@Classes <- Weight@Classes
  ProcessSelectMeanAt(select,
                      Classes   = select@Classes,
                      nArea     = nArea,
                      type      = type,
                      Year      = Year,
                      slot_name = 'MeanAtWeight',
                      dim_name  = 'Class')
}