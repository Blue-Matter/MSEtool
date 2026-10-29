#' Import iSCAM Data into a Data Object
#'
#' `ImportiSCAMData()` is not currently available.
#' The iSCAM importer is not yet implemented, and calling this function
#' returns an error. Please contact Blue Matter support
#' (\email{office@bluematterscience.com}) for help building an operating
#' model from an iSCAM assessment.
#'
#' @param iSCAMdir Character string giving the path to a directory containing
#'   iSCAM output files, or a list already returned by `load.iscam.files()`.
#' @param Name Character; name for the `Data` object.
#' @param FleetName Character; name for the fleet.
#' @param silent Logical; suppress informational output. Default `FALSE`.
#'
#' @return None. The function stops with an error.
#'
#' @seealso [ImportiSCAM()]
#' @export
ImportiSCAMData <- function(iSCAMdir,
                            Name      = "Imported by ImportiSCAMData",
                            FleetName = "iSCAM Fleet",
                            silent    = FALSE) {

  .iSCAMNotAvailable()
  replist <- if (is.character(iSCAMdir)) load.iscam.files(iSCAMdir) else iSCAMdir
  dat <- replist$dat
  mpd <- replist$mpd

  Years <- dat$start.yr:dat$end.yr
  nYear <- length(Years)

  Data            <- Data(Name = Name)
  Data@Name       <- Name
  Data@Years      <- Years
  Data@YearLH     <- max(Years)
  Data@nArea      <- 1

  CatchDF <- as.data.frame(dat$catch)
  CatchByYear <- stats::aggregate(value ~ year, data = CatchDF, FUN = sum)
  CatchArr <- rep(NA_real_, nYear)
  CatchArr[match(CatchByYear$year, Years)] <- CatchByYear$value

  Landings <- CatchData(Name = FleetName)
  Landings@Value <- matrix(CatchArr, nrow = nYear, ncol = 1,
                           dimnames = list(Year = Years, Fleet = FleetName))
  Landings@Units <- 'Biomass'
  Data@Landings <- Landings

  if (!is.null(dat$indices) && length(dat$indices)) {
    nIndex <- length(dat$indices)
    idxNames <- names(dat$indices)
    if (is.null(idxNames)) idxNames <- paste0('Index', seq_len(nIndex))

    Value <- matrix(NA_real_, nYear, nIndex, dimnames = list(Year = Years, Fleet = idxNames))
    CV    <- matrix(NA_real_, nYear, nIndex, dimnames = list(Year = Years, Fleet = idxNames))

    for (i in seq_len(nIndex)) {
      idx <- as.data.frame(dat$indices[[i]])
      ind <- match(idx$iyr, Years)
      Value[ind, i] <- idx$it
      CV[ind, i]    <- 1 / idx$wt
    }

    Survey             <- IndicesData(Name = idxNames)
    Survey@Value       <- Value
    Survey@CV          <- CV
    Survey@Timing      <- rep(0, nIndex)
    Data@Survey        <- Survey
  }

  if (!silent)
    cli::cli_alert_success("Imported iSCAM data: {.val {nYear}} years, {.val {if (!is.null(dat$indices)) length(dat$indices) else 0}} indices")

  Data
}
