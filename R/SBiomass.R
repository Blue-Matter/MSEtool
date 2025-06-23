

#' Return Spawning Biomass from Hist or MSE object
#' 
#' Returns `SBiomass` 
#' @param object A [Hist()] or [MSE()] object
#' @return A [data.frame()] or a multi-dimensional [array()] if `df=FALSE`
#' @export
SBiomass <- function(object, df=TRUE, hist=TRUE) {
  CheckClass(object, c('mse', 'hist'), 'MSE')
  if (inherits(object, 'hist')) {
    return(SBiomassHist(object, df))
  }
  if (!df)
    return(object@SBiomass)
  proj <- array2DF(object@SBiomass)
  proj$Period <- 'Projection'
  proj$Variable <- "Spawning Biomass"
  proj <- ConvertDF(proj)
  units <- lapply(object@OM@Stock, slot, 'Weight') |> lapply(Units) |> unlist() 
  proj <- proj |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  
  if (hist) {
    temp <- new('hist')
    temp@OM <- object@OM
    temp@SBiomass <- object@Hist@SBiomass
    hist <- SBiomassHist(temp, df)
    proj <- dplyr::bind_rows(hist, proj) |>
      dplyr::arrange(Sim, TimeStep, Period)
  }
  class(proj) <- c("sbiomass", class(proj))
  proj
}

SBiomassHist <- function(Hist, df=TRUE) {
  CheckClass(Hist, 'hist', 'Hist')
  if (!df)
    return(Hist@SBiomass)
  HistTimeStep <- TimeSteps(Hist@OM, "Historical")
  hist <- array2DF(Hist@SBiomass)
  hist$Period <- 'Historical'
  hist$Variable <- "Spawning Biomass"
  
  hist <- ConvertDF(hist)
  
  units <- lapply(Hist@OM@Stock, slot, 'Weight') |> lapply(Units) |> unlist()
  hist <- hist |>
    dplyr::filter(TimeStep%in%HistTimeStep) |> 
    dplyr::left_join(data.frame(Stock=names(units), Unit=units), by='Stock') 
  class(hist) <- c("sbiomass", class(hist))
  hist
}

