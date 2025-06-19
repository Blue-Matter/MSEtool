MakeFactor <- function(x) {
  factor(x, ordered = TRUE, levels=unique(x))
}

ConvertDF <- function(df) {
  nms <- colnames(df)
  if ('Sim' %in% nms)
    df$Sim <- as.numeric(df$Sim)
  if ('Stock' %in% nms)
    df$Stock <- MakeFactor(df$Stock)
  if ('Fleet' %in% nms)
    df$Fleet <- MakeFactor(df$Fleet)
  if ('TimeStep' %in% nms)
    df$TimeStep <- as.numeric(df$TimeStep)
  if ('Value' %in% nms)
    df$Value <- as.numeric(df$Value)
  
  df
}

#' Return Biomass from Hist or MSE object
#' 
#' Returns `Biomass` from a [Hist()] or [MSE()] object object, either 
#' as a [data.frame()] or a multi-dimensional [array()]
#' 
#' @export
setGeneric('Biomass', function(object, df=TRUE, ...)
  standardGeneric('Biomass')
)

#' @rdname Biomass
#' @param object A [Hist()] or [MSE()] object
#' @param df Logical. Return as a `data.frame`? Otherwise returns multi-dimensional `array`
#' @export
setMethod('Biomass', c('hist', 'ANY'),
          function(object, df=TRUE) {
            if (!df)
              return(object@Biomass)
            
            HistTimeStep <- TimeSteps(object@OM, "Historical")
            hist <- array2DF(object@Biomass)
            hist$Period <- 'Historical'
            hist$Variable <- "Biomass"
            
            # TODO add units
            units <- lapply(object@OM@Stock, slot, 'Weight') |> lapply(Units) |> unlist()
            
            
            
            hist <- ConvertDF(hist)
            hist <- hist |> dplyr::filter(TimeStep%in%HistTimeStep)
            class(hist) <- c('biomass', 'data.frame')
            hist
          })

#' @rdname Biomass
#' @param hist Logical. Include historical biomass? Only if `df=TRUE`
#' @export
setMethod('Biomass', c('mse', 'ANY'),
          function(object, df=TRUE, hist=TRUE) {
            if (!df)
              return(object@Biomass)
            
            proj <- array2DF(object@Biomass)
            proj$Period <- 'Projection'
            proj$Variable <- "Biomass"
            proj <- ConvertDF(proj)
            
            if (hist) {
              temp <- new('hist')
              temp@OM <- object@OM
              temp@Biomass <- object@Hist@Biomass
              hist <- Biomass(temp)
              return(dplyr::bind_rows(hist, proj) |>
                       dplyr::arrange(TimeStep, Period))
            } 
            proj
          })
