
#' Get Values from OM, Hist, and MSE Objects
#' 
#' Returned as a data frame
#' @name Get
#' 
NULL

# ---- Get At-Age/At-Length Schedules ----

GetSchedule <- function(x, Variable='Length', Slot='MeanAtAge', df=TRUE) {
  CheckClass(x, c('om', 'hist', 'mse'))
  
  isStock <- Variable %in% slotNames('stock')
  isFleet <- Variable %in% slotNames('fleet')
  
  if (!isStock & !isFleet)
    cli::cli_abort('{.val {Variable}} is not a slot in `stock` or `fleet` class objects')
  
  if (inherits(x, 'om')) {
    StockList <- GetScheduleOM(OM=x, Variable, Slot, isStock)
  } else if (inherits(x, 'hist')) {
    StockList <- GetScheduleHist(Hist=x, Variable, Slot, isStock) 
  } else {
    StockList <- GetScheduleMSE(MSE=x, Variable, Slot, isStock)
  }    

  DF <- purrr::map2(StockList, names(StockList), \(stock, name) 
                      stock |> array2DF() |> 
                        ConvertDF() |> dplyr::mutate(Variable=Variable, Stock=name)
  ) |> dplyr::bind_rows()
  
  if (!df)
    return(DF)

  ColOrder <- c('Sim', 'Stock', 'Age', 'Class', 'TimeStep', 'Fleet', 'Value', 'Variable', 'MP')
  ColOrder <- ColOrder[ColOrder %in% colnames(DF)]
  
  DF <- DF |> dplyr::select(dplyr::all_of(ColOrder))
  class(DF) <- c('Schedule', class(DF))
  DF
  
}

GetScheduleOM <- function(OM, Variable, Slot, isStock) {
  CheckClass(OM, c('om', 'hist', 'mse'))
  OM <- PopulateOM(OM, silent=TRUE)
  if (isStock)
    return(
      purrr::map(OM@Stock, \(Stock)
                 Stock |> slot(Variable) |> slot(Slot) |>
                   ArrayReduceDims()) 
    )
  purrr::map(OM@Fleet, \(Stock) {
    purrr::map(Stock, \(Fleet) Fleet |> slot(Variable) |> slot(Slot) |>
                 ArrayReduceDims()) |>
      List2Array("Fleet")
  }) 
}

GetScheduleHist <- function(Hist, Variable, Slot, isStock) {
  CheckClass(Hist, c('om', 'hist', 'mse'))
  OM <- Hist@OM
  if (isStock)
    return(
      purrr::map(OM@Stock, \(Stock)
                 Stock |> slot(Variable) |> slot(Slot) |>
                   ArrayReduceDims())
    )
  
  purrr::map(OM@Fleet, \(Stock)
             Stock |> slot(Variable) |> slot(Slot) |>
               ArrayReduceDims())
}

GetScheduleMSE <- function(MSE, Variable, Slot, isStock) {
  CheckClass(MSE, c('hist', 'mse'))
  OM <- MSE@OM
  if (isStock) {
    # TODO - look up MP specific values in Misc
    StockList <- purrr::map(OM@Stock, \(Stock)
                            Stock |> slot(Variable) |> 
                              slot(Slot) |>
                              ArrayReduceDims()
    )
  } else {
    StockList <- purrr::map(OM@Fleet, \(Stock)
                            Stock |> slot(Variable) |>
                              slot(Slot) |>
                              ArrayReduceDims()
    )
  }

  MPspecific <- MSE@Misc[[Variable]]
  
  if (is.null(MPspecific)) {
    return(StockList)                    
  }
  
  MPspecific <- purrr::map(MPspecific, \(stock) 
                           purrr::map(stock, ArrayReduceDims)
  )

  # Match nSim 
  nSimOM <- purrr::map(StockList, \(stock) {
    dd <- dim(stock)
    ind <- which(names(dimnames(stock))=='Sim')
    dd[ind]
  }) |> unlist() |> max()
  
  nSimMP <- purrr::map(MPspecific, \(stock) {
    purrr::map(stock, \(mp) {
      dd <- dim(mp)
      ind <- which(names(dimnames(mp))=='Sim')
      dd[ind]
    })
  }) |> unlist() |> max()
  
  nsim <- max(c(nSimOM, nSimMP))
  
  # Match TimeSteps 
  TSOM <- purrr::map(StockList, \(stock) {
    dd <- dim(stock)
    ind <- which(names(dimnames(stock))=='TimeStep')
    dimnames(stock)[[ind]]
  }) |> unlist() |> unique()
  
  TSMP <- purrr::map(MPspecific, \(stock) {
    purrr::map(stock, \(mp) {
      dd <- dim(mp)
      ind <- which(names(dimnames(mp))=='TimeStep')
      dimnames(mp)[[ind]]
    })
  }) |> unlist() |>  unique()
  
  TimeSteps <- c(TSOM, TSMP) |> unique() |> as.numeric() |> sort()

  nAgesList <- purrr::map(MSE@OM@Stock, nAge)
  HistTimeSteps <- TimeSteps(MSE@OM, 'Historical') 
  HistTimeSteps <- HistTimeSteps[HistTimeSteps %in% TimeSteps]
  
  
  # Expand Dims 
  StockList <- purrr::map2(StockList, nAgesList, \(stock, nage) {
    ArrayExpand(stock, nSim=nsim, nAges=nage, TimeSteps)
  })
  
  MPArray <- purrr::map2(MPspecific, nAgesList, \(stock, nage) {
    purrr::map(stock, \(mp)
               ArrayExpand(mp, nSim=nsim, nAges=nage, TimeSteps)
               )
  })
  
  # Replace hist values 
  HistStockList <- purrr::map(StockList, \(stock) {
    ArraySubsetTimeStep(stock, HistTimeSteps)
  })
  
  MPArray2 <- purrr::map2(MPArray, HistStockList, \(stock, hist) {
    purrr::map(stock, \(mp) {
      ArrayFill(mp) <- hist
      mp
    })
  }) |> ReverseList()
  

  MPArray3 <- purrr::map2(StockList, MPArray2, \(stock, stock2) {
    stock <- AddDimension(stock, 'MP', 'OM')
    nms <- names(stock2)
    for (i in seq_along(nms)) {
      stock2[[i]] <- AddDimension(stock2[[i]], 'MP', nms[i])
      stock <- abind::abind(stock, stock2[[i]], use.dnns=TRUE) 
    }
    stock
  }) 

  purrr::map(MPArray3, ArrayReduceDims)
}


# GetAtLength <- function(x, Variable, df=TRUE) {
#   GetSchedule(x, Variable, Slot='MeanAtLength', df=TRUE)
# }
# 
# GetAtAge <- function(x, Variable, df=TRUE) {
#   GetSchedule(x, Variable, Slot='MeanAtAge', df=TRUE)
# }

#' @rdname Get
#' @export
GetNaturalMortalityAtAge <- function(x, df=TRUE) {
  GetSchedule(x, "NaturalMortality", df=df)
}

#' @rdname Get
#' @export
GetLengthAtAge <- function(x, df=TRUE) {
  GetSchedule(x, "Length", df=df)
}

#' @rdname Get
#' @export
GetWeightAtAge <- function(x, df=TRUE) {
  GetSchedule(x, "Weight", df=df)
}

#' @rdname Get
#' @export
GetMaturityAtAge <- function(x, df=TRUE) {
  GetSchedule(x, "Maturity", df=df)
}

#' @rdname Get
#' @export
GetFecundityAtAge <- function(x, df=TRUE) {
  GetSchedule(x, "Fecundity", df=df)
}


#' @rdname Get
#' @export
GetSelectivityAtAge <- function(x, df=TRUE) {
  GetSchedule(x, "Selectivity", df=df)
}

#' @rdname Get
#' @export
GetRetentionAtAge <- function(OM, df=TRUE) {
  GetSchedule(OM, "Retention", df=df)
}

#' @rdname Get
#' @export
GetSelectivityAtLength <- function(x, df=TRUE) {
  GetSchedule(x, "Selectivity", 'MeanAtLength', df=df)
}

#' @rdname Get
#' @export
GetRetentionAtLength <- function(OM, df=TRUE) {
  GetSchedule(OM, "Retention", 'MeanAtLength', df=df)
}

#' @rdname Get
#' @export
GetDiscardMortalityAtAge <- function(x, df=TRUE) {
  GetSchedule(x, "DiscardMortality", 'MeanAtAge', df=df)
}

#' @rdname Get
#' @export
GetDiscardMortalityAtLength <- function(x, df=TRUE) {
  GetSchedule(x, "DiscardMortality", 'MeanAtLength', df=df)
}
