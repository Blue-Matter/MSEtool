#' Validate an `mse` Object or List for Slick Conversion
#'
#' Checks that `MSE` is a valid [mse-class] object or list of [mse-class]
#' objects for conversion to a [Slick::Slick()] object. Aborts with an
#' informative message if the `Slick` package is not installed, or if a list
#' of [mse-class] objects has inconsistent simulations, MPs, or time steps.
#' Also aborts if any object contains multiple stocks, which are not currently
#' supported.
#'
#' @param MSE An [mse-class] object or a named list of [mse-class] objects.
#'
#' @return `NULL` invisibly if all checks pass. Otherwise throws an error.
#' @keywords internal
SlickChecks <- function(MSE) {
  MPs <- NULL # CRAN checks 
  CheckClass(MSE, c('mse', 'list'), 'MSE')
  
  if (!requireNamespace("Slick", quietly=TRUE))
    cli::cli_abort(c(
      "The {.pkg Slick} package is required.",
      "i" = "Install from CRAN: {.code install.packages('Slick')}",
      "i" = "Or from GitHub: {.code pak::pkg_install('blue-matter/Slick')}"
    ))
  
  if (inherits(MSE, 'list')) {
    purrr::walk(MSE, SlickChecks)
    
    if (length(unique(nSim(MSE))) != 1)
      cli::cli_abort(
        "All MSE objects must have the same number of simulations. \\
         Use {.run nSim(MSE)} to check."
      )
    if (length(unique(MPs(MSE))) != 1)
      cli::cli_abort(
        "All MSE objects must have the same MPs. \\
         Use {.run MPs(MSE)} to check."
      )
    if (length(unique(Years(MSE))) != 1)
      cli::cli_abort(
        "All MSE objects must have the same time steps. \\
         Use {.run Years(MSE)} to check."
      )
  } else {
    if (nStock(MSE@OM) > 1)
      cli::cli_abort("Multiple stocks are not currently supported.")
  }
  invisible(NULL)
}


#' Convert an `mse` Object to a Slick Object
#'
#' Converts an [mse-class] object (or a list of [mse-class] objects) into a
#' [Slick::Slick()] data object for interactive visualization in the
#' [Slick App](https://shiny.bluematterscience.com/app/slick).
#'
#' A [Slick::Slick()] object is a standardized data structure containing MSE
#' results, MP metadata, and performance indicators. It can be passed directly
#' to [Slick::App()] for interactive exploration, or saved and uploaded to the
#' online Slick App.
#'
#' When `MSE` is a list, each element should represent a different Operating
#' Model (OM) hypothesis tested with the same set of MPs. All [mse-class]
#' objects in the list must have identical MPs, number of simulations, and
#' number of time steps.
#'
#' The resulting [Slick::Slick()] object includes:
#' - **MPs**: management procedure codes, labels, and display colours.
#' - **Kobe**: SB/SBMSY and F/FMSY time series for the Kobe plot.
#' - **Timeseries**: projection-period time series for spawning biomass,
#'   fishing mortality, landings, SB/SBMSY, and F/FMSY.
#'
#' @param MSE An [mse-class] object or a named list of [mse-class] objects,
#'   where each element represents a different OM. All objects in the list
#'   must have identical MPs, number of simulations, and number of time steps.
#'
#' @return A [Slick::Slick()] object ready for use with [Slick::App()].
#'
#' @examples
#' \dontrun{
#' Slick <- MSE2Slick(MSE)
#' Slick::App(slick=Slick)
#'
#' # Multiple OMs
#' Slick <- MSE2Slick(list(MSE_Base, MSE_LowM, MSE_HighM))
#' Slick::App(slick=Slick)
#' }
#'
#' @seealso [Slick::Slick()], [Slick::App()], [mse-class]
#' @export
MSE2Slick <- function(MSE) {
  SlickChecks(MSE)
  
  mse_ref <- if (is.list(MSE)) MSE[[1]] else MSE
  
  Slick         <- Slick::Slick()
  Slick@Title   <- mse_ref@OM@Name
  Slick@MPs     <- MSE2MPs(mse_ref)
  Slick@Kobe    <- MSE2Kobe(MSE)
  Slick@Timeseries <- MSE2Timeseries(MSE)
  Slick
}


#' Extract MP Metadata for a Slick Object
#'
#' Constructs a [Slick::MPs()] object from an [mse-class] object, populating
#' MP codes, labels, and default display colours.
#'
#' @param MSE An [mse-class] object.
#'
#' @return A [Slick::MPs()] object.
#' @keywords internal
MSE2MPs <- function(MSE) {
  SlickChecks(MSE)
  
  MPs        <- Slick::MPs()
  MPs@Code   <- names(MSE@MPs)
  MPs@Label  <- names(MSE@MPs)
  MPs@Color  <- Slick::default_mp_colors(length(MPs@Code))
  MPs
}

#' Build a Kobe Plot Object from an `mse` Object
#'
#' Constructs a [Slick::Kobe()] object containing SB/SBMSY and F/FMSY
#' time series for the projection period, for use in the Slick Kobe plot.
#' The Kobe plot compares MP performance with respect to biomass (x-axis)
#' and fishing mortality (y-axis) relative to MSY-based reference points.
#'
#' @param MSE An [mse-class] object or a named list of [mse-class] objects.
#'
#' @return A [Slick::Kobe()] object with `Value` array of dimensions
#'   `Sim × OM × MP × PI × Year`, where `PI` contains SB/SBMSY (index 1)
#'   and F/FMSY (index 2).
#' @keywords internal
MSE2Kobe <- function(MSE) {
  SlickChecks(MSE)
  
  SB_SBMSY <- F_FMSY <- NULL 
  
  cli::cli_abort("Not currently working until `SB_SBMSY` and `F_FMSY` are complete ")
  
  mse_ref <- if (is.list(MSE)) MSE[[1]] else MSE
  nOM     <- if (is.list(MSE)) length(MSE) else 1L
  
  Kobe              <- Slick::Kobe()
  Kobe@Code         <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Label        <- c('SB/SBMSY', 'F/FMSY')
  Kobe@Description  <- c(
    'Spawning biomass (SB) relative to SB at maximum sustainable yield (SBMSY)',
    'Fishing mortality (F) relative to F at maximum sustainable yield (FMSY)'
  )
  Kobe@Time    <- Years(mse_ref@OM, 'Projection')
  Kobe@TimeLab <- firstup(mse_ref@OM@TimeUnits)
  Kobe@Target  <- rep(1, 2)
  
  MPs_names    <- names(mse_ref@MPs)
  nsim         <- mse_ref@OM@nSim
  nMP          <- length(MPs_names)
  nTS          <- length(Kobe@Time)
  ProjectionTS <- Kobe@Time
  
  Kobe@Value <- array(NA, dim=c(nsim, nOM, nMP, 2L, nTS))
  
  for (om in seq_len(nOM)) {
    mse    <- if (is.list(MSE)) MSE[[om]] else MSE
    Stocks <- StockNames(mse@OM)
    
    sb_sbmsy <- SB_SBMSY(mse) |>
      dplyr::filter(Year %in% ProjectionTS, Stock %in% Stocks[1]) |>
      dplyr::arrange(Sim, Year, MP)
    
    f_fmsy <- F_FMSY(mse) |>
      dplyr::filter(Year %in% ProjectionTS, Stock %in% Stocks[1]) |>
      dplyr::arrange(Sim, Year, MP)
    
    for (mm in seq_len(nMP)) {
      Kobe@Value[, om, mm, 1, ] <- sb_sbmsy |>
        dplyr::filter(MP == MPs_names[mm]) |>
        dplyr::pull(Value) |>
        matrix(nrow=nsim, ncol=nTS, byrow=TRUE)
      
      Kobe@Value[, om, mm, 2, ] <- f_fmsy |>
        dplyr::filter(MP == MPs_names[mm]) |>
        dplyr::pull(Value) |>
        matrix(nrow=nsim, ncol=nTS, byrow=TRUE)
    }
  }
  Kobe
}

#' Build a Timeseries Plot Object from an `mse` Object
#'
#' Constructs a [Slick::Timeseries()] object containing historical and
#' projection time series for a set of performance indicators, for use in the
#' Slick time series plot. By default includes spawning biomass, fishing
#' mortality, landings, SB/SBMSY, and F/FMSY.
#'
#' @param MSE An [mse-class] object or a named list of [mse-class] objects.
#' @param Code Character vector of performance indicator codes. Must
#'   correspond to functions that accept an [mse-class] object and return a
#'   tidy data frame with columns `Sim`, `Year`, `MP`, `Period`, and `Value`.
#'   Default `c('SBiomass', 'apicalF', 'Landings', 'SB_SBMSY', 'F_FMSY')`.
#' @param Label Character vector of display labels for each performance
#'   indicator, matched by position to `Code`. Default
#'   `c('Spawning Biomass', 'Fishing Mortality', 'Landings', 'SB/SBMSY', 'F/FMSY')`.
#'
#' @return A [Slick::Timeseries()] object with `Value` array of dimensions
#'   `Sim × OM × MP × PI × Year`, spanning both historical and projection
#'   periods.
#' @keywords internal
MSE2Timeseries <- function(MSE,
                           Code  = c('SBiomass', 'apicalF', 'Landings',
                                     'SB_SBMSY', 'F_FMSY'),
                           Label = c('Spawning Biomass', 'Fishing Mortality',
                                     'Landings', 'SB/SBMSY', 'F/FMSY')) {
  SlickChecks(MSE)
  
  mse_ref <- if (is.list(MSE)) MSE[[1]] else MSE
  nOM     <- if (is.list(MSE)) length(MSE) else 1L
  
  Timeseries              <- Slick::Timeseries()
  Timeseries@Code         <- Code
  Timeseries@Label        <- Label
  Timeseries@Time         <- Years(mse_ref@OM)
  Timeseries@TimeNow      <- max(Years(mse_ref@OM, 'Historical'))
  Timeseries@TimeLab      <- firstup(mse_ref@OM@TimeUnits)
  Timeseries@Target       <- c(NA, NA, NA, 1,   NA)
  Timeseries@Limit        <- c(NA, NA, NA, 0.4,  1)
  
  nsim <- mse_ref@OM@nSim
  nMP  <- length(mse_ref@MPs)
  nPI  <- length(Code)
  nTS  <- length(Timeseries@Time)
  
  Timeseries@Value <- array(NA, dim=c(nsim, nOM, nMP, nPI, nTS))
  
  for (om in seq_len(nOM)) {
    mse <- if (is.list(MSE)) MSE[[om]] else MSE
    for (i in seq_along(Code)) {
      Timeseries@Value[, om, , i, ] <- GetTimeseriesVariable(Code[i], mse)
    }
  }
  Timeseries
}

#' Extract a Time Series Variable from an `mse` Object
#'
#' Calls the function named `Var` with `MSE` as its argument and reshapes the
#' result into a `Sim × MP × Year` array spanning both historical and
#' projection periods. Historical values are replicated across all MPs since
#' they are MP-invariant.
#'
#' @param Var Character string. Name of a function that accepts an
#'   [mse-class] object and returns a tidy data frame with columns `Sim`,
#'   `Year`, `MP`, `Period`, and `Value`.
#' @param MSE An [mse-class] object.
#'
#' @return A numeric array with dimensions `Sim × MP × Year`.
#' @keywords internal
GetTimeseriesVariable <- function(Var, MSE) {
  nsim <- MSE@OM@nSim
  nMP  <- length(MSE@MPs)
  nTS  <- length(Years(MSE@OM))
  
  DF <- do.call(Var, list(MSE)) |>
    dplyr::arrange(Sim, Year, MP) |>
    dplyr::mutate(MP=as.character(MP))
  
  MPs_proj <- DF$MP |> unique()
  MPs_proj <- MPs_proj[MPs_proj != 'Historical']
  
  nHistTS <- DF |>
    dplyr::filter(Period == 'Historical') |>
    dplyr::pull(Year) |>
    unique() |>
    length()
  
  HistValues <- DF |>
    dplyr::filter(MP == 'Historical') |>
    dplyr::pull(Value) |>
    matrix(nrow=nsim, ncol=nHistTS, byrow=TRUE)
  
  Array <- array(NA, dim=c(nsim, nMP, nTS))
  
  for (mm in seq_along(MPs_proj)) {
    ProjValues <- DF |>
      dplyr::filter(MP == MPs_proj[mm]) |>
      dplyr::pull(Value) |>
      matrix(nrow=nsim, ncol=nTS - nHistTS, byrow=TRUE)
    Array[, mm, ] <- cbind(HistValues, ProjValues)
  }
  Array
}

