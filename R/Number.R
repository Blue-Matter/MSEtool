
#' Return Number from Hist or MSE object
#' 
#' Returns `Number` 
#' @param object A [Hist()] or [MSE()] object
#' @return A [data.frame()] or a multi-dimensional [array()] if `df=FALSE`
#' @export
Number <- function(object, df=TRUE, hist=TRUE, byArea=FALSE, byAge=FALSE) {
  CheckClass(object, c('mse', 'hist'), 'MSE')
  if (inherits(object, 'hist')) {
    return(NumberHist(object, df, byArea, byAge))
  }
  if (!df)
    return(object@Number)
  
  
  projN <- list()
  for (i in seq_along(object@Number)) {
    n <- object@Number[[i]] 
    n <- array2DF(n)
    
    if (!byArea & !byAge) {
      n <- n |> dplyr::group_by(Sim, TimeStep, MP) |>
        dplyr::summarise(Value=sum(Value), .groups='drop')
    } else if (!byArea) {
      n <- n |> dplyr::group_by(Sim, TimeStep, Age, MP) |>
        dplyr::summarise(Value=sum(Value), .groups='drop')
    } else if (!byAge) {
      n <- n |> dplyr::group_by(Sim, TimeStep, Area, MP) |>
        dplyr::summarise(Value=sum(Value), .groups='drop')
    }
    
    n$Stock <- names(object@Number)[i]
    n$Period <- 'Projection'
    n$Variable <- "Number"
    projN[[i]] <- n
  }
  
  if (length(projN)>1) {
    proj <- do.call('rbind', projN)
  } else {
    proj <- projN[[1]]
  }
  
  proj <- ConvertDF(proj)
  
  if (hist) {
    temp <- new('hist')
    temp@OM <- object@OM
    temp@Number <- object@Hist@Number
    hist <- NumberHist(temp, df, byArea, byAge)
    proj <- dplyr::bind_rows(hist, proj) |>
      dplyr::arrange(Sim, TimeStep, Period, MP)
  }
  class(proj) <- c("number", class(proj))
  proj
}

NumberHist <- function(Hist, df=TRUE, byArea=FALSE, byAge=FALSE) {
  CheckClass(Hist, 'hist', 'Hist')
  if (!df)
    return(Hist@Number)
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  histN <- list()
  for (i in seq_along(Hist@Number)) {
    n <- Hist@Number[[i]] |> ArraySubsetTimeStep(HistTimeStep)
    n <- array2DF(n)
    
    if (!byArea & !byAge) {
      n <- n |> dplyr::group_by(Sim, TimeStep) |>
        dplyr::summarise(Value=sum(Value), .groups='drop')
    } else if (!byArea) {
      n <- n |> dplyr::group_by(Sim, TimeStep, Age) |>
        dplyr::summarise(Value=sum(Value), .groups='drop')
    } else if (!byAge) {
      n <- n |> dplyr::group_by(Sim, TimeStep, Area) |>
        dplyr::summarise(Value=sum(Value), .groups='drop')
    }
    
    n$Stock <- names(Hist@Number)[i]
    n$Period <- 'Historical'
    n$Variable <- "Number"
    histN[[i]] <- n
  }
  if (length(histN)>1) {
    hist <- do.call('rbind', histN)
  } else {
    hist <- histN[[1]]
  }
  
  hist <- ConvertDF(hist)
  class(hist) <- c("number", class(hist))
  hist
}
