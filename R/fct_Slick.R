# function for creating Slick object
# MSE <- readRDS("C:\\Users\\Admin\\AppData\\Local\\Temp\\RtmpquR2Vi\\file5014192b3c3e")

# TODO - update ref points to grab MP specific ref points when applicable 

SlickChecks <- function(MSE) {
  CheckClass(MSE, c('mse', 'list'), 'MSE')
  
  if (!requireNamespace("Slick", quietly = TRUE)) 
    cli::cli_inform(c("Package {.pkg Slick} is required for this function", 
                      "i"="Install from CRAN with `pak::pkg_install('Slick')` or from GitHub with `pak::pkg_install('blue-matter/Slick')`"))
  
  if (inherits(MSE, 'list')) {
    purrr::map(MSE, SlickChecks)
    
    SamenSim <- nSim(MSE) |> unique() |> length() == 1
    if (!SamenSim)
      cli::cli_abort("All MSE objects must have the same number of simulations. Use {.run nSim(MSE)} to check.")
    
    SameMPs <- MPs(MSE) |> unique() |> length() == 1
    if (!SameMPs)
      cli::cli_abort("All MSE objects must have the same MPs. Use {.run MPs(MSE)} to check.")
    
    SameTSs <-  TimeSteps(MSE) |> unique() |> length() == 1
    if (!SameTSs)
      cli::cli_abort("All MSE objects must have the Time Steps. Use {.run TimeSteps(MSE)} to check.")
    
  } else {
    if (nStock(MSE@OM)>1)
      cli::cli_abort('Multiple stocks currently not supported')  
  }
}



#' Create a `Slick` object 
#' 
#' @param MSE An `mse` class object or a list of `mse` objects
#' 
#' If `MSE` is a list, each `mse` object must have identical MPs, number of time steps,
#' and number of simulations; i.e., each `mse` object should represent a different 
#' OM.
#' 
#' @export
MSE2Slick <- function(MSE) {
  SlickChecks(MSE)
  Slick <- Slick::Slick()
  if (is.list(MSE)) {
    Slick@Title <- MSE[[1]]@OM@Name  
    Slick@MPs <- MSE2MPs(MSE[[1]])
  } else {
    Slick@Title <- MSE@OM@Name  
    Slick@MPs <- MSE2MPs(MSE)
  }
  
  Slick@Kobe <- MSE2Kobe(MSE)
  
  Slick@Timeseries <- MSE2Timeseries(MSE)
  
  Slick
}

MSE2MPs <- function(MSE) {
  SlickChecks(MSE)
  MPs <- Slick::MPs()
  MPs@Code <- names(MSE@MPs)
  MPs@Label <- names(MSE@MPs)
  MPs@Description
  MPs@Color <- Slick::default_mp_colors(length(MPs@Code))
  MPs@Preset
  MPs
}

MSE2Kobe <- function(MSE) {
  SlickChecks(MSE)
  Kobe <- Slick::Kobe()
  Kobe@Code <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Label <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Description <- c('Spawning Biomass (SB) relative to SB corresponding with maximum sustainable yield (MSY)',
                        'Fishing mortality (F) relative to F corresponding MSY')
  
  if (is.list(MSE)) {
    Kobe@Time <- TimeSteps(MSE[[1]]@OM, 'Projection')
    Kobe@TimeLab <- firstup(MSE[[1]]@OM@TimeUnits)
    MPs <- names(MSE[[1]]@MPs)
    nsim <- MSE[[1]]@OM@nSim
    nOM <- length(MSE)
    ProjectionTS <- TimeSteps(MSE[[1]]@OM, 'Projection')
  } else {
    Kobe@Time <- TimeSteps(MSE@OM, 'Projection')
    Kobe@TimeLab <- firstup(MSE@OM@TimeUnits)  
    MPs <- names(MSE@MPs)
    nsim <- MSE@OM@nSim
    nOM <- 1
    ProjectionTS <- TimeSteps(MSE@OM, 'Projection')
  }
  nMP <- length(MPs)
  nTS <- length(Kobe@Time)
  nPI <- 2
  
  Kobe@Value <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))
  Kobe@Target <- rep(1,2)
  
  for (om in 1:nOM) {
    if (is.list(MSE)) {
      mse <- MSE[[om]]
    } else {
      mse <- MSE
    }
    Stocks <- StockNames(mse@OM)
    
    SB_SBMSY <- SB_SBMSY(mse) |> dplyr::filter(TimeStep%in%ProjectionTS, Stock%in%Stocks[1])
    F_FMSY <- F_FMSY(mse) |> dplyr::filter(TimeStep%in%ProjectionTS, Stock%in%Stocks[1])
    # TODO  MSE@Misc$Failed
    # TODO get MP specific MSY ref points if applicable
    
    for (mm in 1:nMP) {
      Kobe@Value[,om,mm,1,] <- SB_SBMSY |> 
        dplyr::filter(MP==MPs[mm]) |> 
        dplyr::pull(Value) |>
        matrix(nrow=nsim, ncol=nTS, byrow=TRUE)
      
      Kobe@Value[,om,mm,2,] <- F_FMSY |> 
        dplyr::filter(MP==MPs[mm]) |> 
        dplyr::pull(Value) |>
        matrix(nrow=nsim, ncol=nTS, byrow=TRUE)
    }
  }
  Kobe 
} 

MSE2Timeseries <- function(MSE, 
                           Code=c('SBiomass', 'apicalF', 'Landings', 'SB_SBMSY', 'F_FMSY'),
                           Label=c('Spawning Biomass', 'Fishing Mortality', 'Landings', 'SB/SBMSY', 
                                   'F/FMSY')) {
  SlickChecks(MSE)
  
  Timeseries <- Slick::Timeseries()
  Timeseries@Code <- Code
  Timeseries@Label <- Label
  Timeseries@Description
  
  if (is.list(MSE)) {
    Timeseries@Time <- TimeSteps(MSE[[1]])
    Timeseries@TimeNow <- TimeSteps(MSE[[1]]@OM, 'Historical') |> max()
    Timeseries@TimeLab <- firstup(MSE[[1]]@OM@TimeUnits)
    nsim <- nSim(MSE[[1]])
    nOM <- length(MSE)
    nMP <- length(MSE[[1]]@MPs)
  } else {
    Timeseries@Time <- TimeSteps(MSE@OM)
    Timeseries@TimeNow <- TimeSteps(MSE@OM, 'Historical') |> max()
    Timeseries@TimeLab <- firstup(MSE@OM@TimeUnits)
    nsim <- MSE@OM@nSim
    nOM <- 1
    nMP <- length(MSE@MPs)
  }
  
  nPI <- length(Code)
  nTS <- length(Timeseries@Time)

  Timeseries@Value <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))
  
  for (om in 1:nOM) {
    if (is.list(MSE)) {
      mse <- MSE[[om]]
    } else {
      mse <- MSE
    }
    for (i in seq_along(Code)) {
      Timeseries@Value [,om,,i,] <- GetTimeseriesVariable(Code[i], mse)
    }
  }

  Timeseries@Target <- c(NA, NA, NA, 1, NA)
  Timeseries@Limit <- c(NA, NA, NA, 0.4, 1)

  Timeseries
}


GetTimeseriesVariable <- function(Var, MSE) {
  nsim <- MSE@OM@nSim
  nMP <- length(MSE@MPs)
  nTS <- length(TimeSteps(MSE@OM))
  Array <- array(NA, dim=c(nsim, nMP, nTS))
  
  DF <- do.call(Var, c(list(MSE)))
  
  DF$MP <- as.character(DF$MP)
  MPs <- DF$MP |> unique()
  nHistTS <- DF |> dplyr::filter(Period=='Historical') |>
    dplyr::pull(TimeStep) |>
    unique() |> length()
  
  HistValues <- DF |> dplyr::filter(MP=='Historical') |> dplyr::pull(Value) 
  HistValues <- matrix(HistValues, nrow=nsim, ncol=nHistTS, byrow=TRUE)
  
  MPs <- MPs[!MPs=='Historical']
  for (mm in seq_along(MPs)) {
    ProjValues <- DF |> dplyr::filter(MP==MPs[mm]) |> dplyr::pull(Value) 
    ProjValues <- matrix(ProjValues, nrow=nsim, ncol=nTS-nHistTS, byrow=TRUE)
    Array[,mm,] <- cbind(HistValues, ProjValues)
  }
  Array
}








