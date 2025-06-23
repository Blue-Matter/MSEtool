#' Return Removals from Hist or MSE object
#' 
#' Returns `Removals` 
#' @param object A [Hist()] or [MSE()] object
#' @return A [data.frame()] or a multi-dimensional [array()] if `df=FALSE`
#' @export
Removals <- function(object, df=TRUE, hist=TRUE) {
  CheckClass(object, c('mse', 'hist'), 'MSE')
  if (inherits(object, 'hist')) {
    return(RemovalsHist(object, df))
  }
  if (!df)
    return(object@Removals)
  
 proj <- RemovalsProj(object, df, hist)
  
  if (hist) {
    temp <- new('hist')
    temp@OM <- object@OM
    temp@Removals <- object@Hist@Removals
    hist <- RemovalsHist(temp, df)
    proj <- dplyr::bind_rows(hist, proj) |>
      dplyr::arrange(Sim, TimeStep, Period, MP)
  }
  class(proj) <- c("removals", class(proj))
  proj
}


RemovalsProj <- function(object, df=TRUE, hist=TRUE, slot='Removals') {
  projN <- list()
  val <- slot(object, slot)
  proj <- array2DF(val)
  proj$Period <- 'Projection'
  proj$Variable <- slot
  proj <- ConvertDF(proj) |>
    dplyr::arrange(Sim, TimeStep, Period, MP)
  proj
  
}

RemovalsHist <- function(Hist, df=TRUE, units=c('Biomass', 'Number'), slot='Removals') {
  CheckClass(Hist, 'hist', 'Hist')
  # units <- match.arg(units) # TODO - option to return removals in number instead of weight
  
  if (!df)
    return(slot(Hist, slot))
  
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  histN <- list()
  val <- slot(Hist, slot)
  for (i in seq_along(val)) {
    n <- val[[i]] |> ArraySubsetTimeStep(HistTimeStep)
    n <- array2DF(n)
    
    n <- n |> dplyr::group_by(Sim, TimeStep, Fleet) |>
      dplyr::summarise(Value=sum(Value), .groups='drop')
    
    # if (!byArea & !byAge) {
    # 
    # } else if (!byArea) {
    #   n <- n |> dplyr::group_by(Sim, TimeStep, Age) |>
    #     dplyr::summarise(Value=sum(Value), .groups='drop')
    # } else if (!byAge) {
    #   n <- n |> dplyr::group_by(Sim, TimeStep, Area) |>
    #     dplyr::summarise(Value=sum(Value), .groups='drop')
    # }
    # 
    n$Stock <- names(slot(Hist, slot))[i]
    n$Period <- 'Historical'
    n$Variable <- slot
    histN[[i]] <- n
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

#' @describeIn Removals Return `Landings`
#' @export
Landings <- function(object, df=TRUE, hist=TRUE) {
  CheckClass(object, c('mse', 'hist'), 'MSE')
  if (inherits(object, 'hist')) {
    return(RemovalsHist(object, df, slot='Landings'))
  }
  if (!df)
    return(object@Landings)
  
  proj <- RemovalsProj(object, df, hist, slot='Landings')
  
  if (hist) {
    temp <- new('hist')
    temp@OM <- object@OM
    temp@Landings <- object@Hist@Landings
    hist <- RemovalsHist(temp, df, slot='Landings')
    proj <- dplyr::bind_rows(hist, proj) |>
      dplyr::arrange(Sim, TimeStep, Period, MP)
  }
  class(proj) <- c("landings", class(proj))
  proj
}
