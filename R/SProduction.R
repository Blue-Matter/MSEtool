

#' Return Spawning Production from Hist or MSE object
#' 
#' Returns `SProduction` 
#' @param object A [Hist()] or [MSE()] object
#' @return A [data.frame()] or a multi-dimensional [array()] if `df=FALSE`
#' @export
SProduction <- function(object, df=TRUE, hist=TRUE) {
  CheckClass(object, c('mse', 'hist'), 'MSE')
  if (inherits(object, 'hist')) {
    return(SBiomassHist(object, df))
  }
  if (!df)
    return(object@SProduction)
  proj <- array2DF(object@SProduction)
  proj$Period <- 'Projection'
  proj$Variable <- "Spawning SProduction"
  proj <- ConvertDF(proj)
  units <- lapply(object@OM@Stock, slot, 'Fecundity') |> lapply(Units) |> unlist() 
  proj <- proj |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
  if (hist) {
    temp <- new('hist')
    temp@OM <- object@OM
    temp@SProduction <- object@Hist@SProduction
    hist <- SProductionHist(temp, df)
    proj <- dplyr::bind_rows(hist, proj) |>
      dplyr::arrange(Sim, TimeStep, Period)
  }
  class(proj) <- c("sproduction", class(proj))
  proj
}

SProductionHist <- function(Hist, df=TRUE) {
  CheckClass(Hist, 'hist', 'Hist')
  if (!df)
    return(Hist@SProduction)
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  hist <- array2DF(Hist@SProduction)
  hist$Period <- 'Historical'
  hist$Variable <- "Spawning Production"
  
  hist <- ConvertDF(hist)
  
  units <- lapply(Hist@OM@Stock, slot, 'Fecundity') |> lapply(Units) |> unlist()
  hist <- hist |>
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  class(hist) <- c("sproduction", class(hist))
  hist
}

