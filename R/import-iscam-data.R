#' Import iSCAM Data into a Data Object
#'
#' Populates a [Data()] object with observed time series extracted from an
#' iSCAM assessment: aggregate landings (summed across gears, matching the
#' single-fleet scope of [ImportiSCAM()]) and abundance indices.
#'
#' @param iSCAMdir Character string giving the path to a directory containing
#'   iSCAM output files, or a list already returned by `load.iscam.files()`.
#' @param Name Character; name for the `Data` object.
#' @param FleetName Character; name for the single aggregate fleet (must
#'   match the fleet name used by [ImportiSCAM()] when combining the two).
#' @param silent Logical; suppress informational output. Default `FALSE`.
#'
#' @return A populated [Data()] object with `@Landings` and (if present in
#'   the iSCAM output) `@Survey` populated. Age/size composition data is not
#'   yet imported -- see [ImportiSCAM()] Details.
#'
#' @seealso [ImportiSCAM()], [Data()], [CatchData()], [IndicesData()]
#' @export
ImportiSCAMData <- function(iSCAMdir,
                            Name      = "Imported by ImportiSCAMData",
                            FleetName = "iSCAM Fleet",
                            silent    = FALSE) {

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

  # Landings: aggregate catch across gears, matching ImportiSCAM()'s
  # single-fleet scope (see its @details).
  CatchDF <- as.data.frame(dat$catch)
  CatchByYear <- stats::aggregate(value ~ year, data = CatchDF, FUN = sum)
  CatchArr <- rep(NA_real_, nYear)
  CatchArr[match(CatchByYear$year, Years)] <- CatchByYear$value

  Landings <- CatchData(Name = FleetName)
  Landings@Value <- matrix(CatchArr, nrow = nYear, ncol = 1,
                           dimnames = list(Year = Years, Fleet = FleetName))
  Landings@Units <- 'Biomass'
  Data@Landings <- Landings

  # Abundance indices -> Survey (iSCAM does not distinguish fleet-CPUE from
  # independent survey indices the way SS3/BAM report structures do, so all
  # indices are imported as independent surveys).
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
    # iSCAM's report structure doesn't indicate per-survey vulnerability;
    # "Biomass" (all ages equally vulnerable) is the safest generic default
    # -- see .ConditionObsIndex()'s handling of IndicesData@Selectivity.
    Survey@Selectivity <- rep('Biomass', nIndex)
    Data@Survey        <- Survey
  }

  if (!silent)
    cli::cli_alert_success("Imported iSCAM data: {.val {nYear}} years, {.val {if (!is.null(dat$indices)) length(dat$indices) else 0}} indices")

  Data
}
