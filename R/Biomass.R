
#' Return Biomass from Hist or MSE object
#' 
#' Returns `Biomass` 
#' @param object A [Hist()] or [MSE()] object
#' @return A [data.frame()] or a multi-dimensional [array()] if `df=FALSE`
#' @export
Biomass <- function(object, df=TRUE, hist=TRUE) {
  CheckClass(object, c('mse', 'hist'), 'MSE')
  if (inherits(object, 'hist')) {
      return(BiomassHist(object, df))
  }
  if (!df)
    return(object@Biomass)
  proj <- array2DF(object@Biomass)
  proj$Period <- 'Projection'
  proj$Variable <- "Biomass"
  proj <- ConvertDF(proj)
  units <- lapply(object@OM@Stock, slot, 'Weight') |> lapply(Units) |> unlist() 
  proj <- proj |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
 
  if (hist) {
    temp <- new('hist')
    temp@OM <- object@OM
    temp@Biomass <- object@Hist@Biomass
    hist <- BiomassHist(temp, df)
    proj <- dplyr::bind_rows(hist, proj) |>
      dplyr::arrange(Sim, TimeStep, Period)
  }
  class(proj) <- c("biomass", class(proj))
  proj
}

BiomassHist <- function(Hist, df=TRUE) {
  CheckClass(Hist, 'hist', 'Hist')
  if (!df)
    return(Hist@Biomass)
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  hist <- array2DF(Hist@Biomass)
  hist$Period <- 'Historical'
  hist$Variable <- "Biomass"

  hist <- ConvertDF(hist)
  
  units <- lapply(Hist@OM@Stock, slot, 'Weight') |> lapply(Units) |> unlist()
  hist <- hist |>
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  class(hist) <- c("biomass", class(hist))
  hist
}

