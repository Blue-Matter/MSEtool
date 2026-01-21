#' Return Effort from Hist or MSE object
#' 
#' Returns `Effort` 
#' @param object A [Hist] or [MSE] object
#' @return A [data.frame()] or a multi-dimensional [array()] if `df=FALSE`
#' @export
ReturnEffort <- function(object, df=TRUE, hist=TRUE) {
  CheckClass(object, c('mse', 'hist'), 'MSE')
  if (inherits(object, 'hist')) {
    return(EffortHist(object, df))
  }
  if (!df)
    return(object@Effort)
  
  proj <- EffortProj(object, df, hist)
  
  if (hist) {
    temp <- new('hist')
    temp@OM <- object@OM
    temp@Effort <- object@Hist@Effort
    hist <-EffortHist(temp, df)
    proj <- dplyr::bind_rows(hist, proj) |>
      dplyr::arrange(Sim, Year, Period, MP)
  }
  class(proj) <- c("removals", class(proj))
  proj
}


EffortProj <- function(object, df=TRUE, hist=TRUE, slot='Effort') {
  projN <- list()
  val <- slot(object, slot)
  proj <- array2DF(val)
  proj$Period <- 'Projection'
  proj$Variable <- slot
  proj <- ConvertDF(proj) |>
    dplyr::arrange(Sim, Year, Period, MP)
  proj
  
}

EffortHist <- function(Hist, df=TRUE, slot='Effort') {
  CheckClass(Hist, 'hist', 'Hist')
  # units <- match.arg(units) # TODO - option to return removals in number instead of weight
  
  if (!df)
    return(slot(Hist, slot))
  
  HistYear <- Years(Hist@OM, "Historical")
  histN <- list()
  val <- slot(Hist, slot)
  if (is.list(val)) {
    for (i in seq_along(val)) {
      n <- val[[i]] |> ArraySubsetYear(HistYear)
      n <- array2DF(n)
      
      n <- n |> dplyr::group_by(Sim, Year, Fleet) |>
        dplyr::summarise(Value=sum(Value), .groups='drop')
      
      # if (!byArea & !byAge) {
      # 
      # } else if (!byArea) {
      #   n <- n |> dplyr::group_by(Sim, Year, Age) |>
      #     dplyr::summarise(Value=sum(Value), .groups='drop')
      # } else if (!byAge) {
      #   n <- n |> dplyr::group_by(Sim, Year, Area) |>
      #     dplyr::summarise(Value=sum(Value), .groups='drop')
      # }
      # 
      n$Stock <- names(slot(Hist, slot))[i]
      n$Period <- 'Historical'
      n$Variable <- slot
      histN[[i]] <- n
    }
  } else {
    n <- val|> ArraySubsetYear(HistYear)
    n <- array2DF(n)
    
    n <- n |> dplyr::group_by(Sim, Year, Fleet) |>
      dplyr::summarise(Value=sum(Value), .groups='drop')
    
    n$Stock <- names(slot(Hist, slot))
    n$Period <- 'Historical'
    n$Variable <- slot
    histN[[1]] <- n
  }

  if (length(histN)>1) {
    hist <- do.call('rbind', histN)
  } else {
    hist <- histN[[1]]
  }
  
  hist <- ConvertDF(hist)
  class(hist) <- c( tolower(slot), class(hist))
  hist
}