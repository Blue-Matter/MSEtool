
#' Return Biomass from Hist or MSE object
#' 
#' Returns `Biomass` 
#' @param object A [Hist()] or [MSE()] object
#' @return A [data.frame()] or a multi-dimensional [array()] if `df=FALSE`
#' @export
Biomass <- function(object, df=TRUE, hist=TRUE, RelTo=NULL) {
  
  CheckClass(object, c('mse', 'hist'), 'MSE')
  
  if (!is.null(RelTo)) {
    if (RelTo!='B0')
      stop("Currently `RelTo` can only be `B0`")
    RefVal <- array2DF(object@Unfished@Equilibrium@Biomass, 'B0') |> ConvertDF()
    RefVal <- RefVal |> dplyr::rename(relto=RelTo)
    
  } else {
    RefVal <- NULL
  }
  
  if (inherits(object, 'hist')) {
    Biomass <- BiomassHist(object, df)
    if (is.null(RefVal))
      return(Biomass)
    
    Biomass <- Biomass |>
      dplyr::left_join(RefVal) |>
      dplyr::mutate(Value=Value/relto)
    
    return(Biomass)
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
  
  if (!is.null(RefVal)) {
    proj <- proj |>
      dplyr::left_join(RefVal) |>
      dplyr::mutate(Value=Value/relto)
  }
  
  
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

