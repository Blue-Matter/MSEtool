#' Import an iSCAM Model into an Operating Model
#'
#' `ImportiSCAM()` is not currently available.
#' The iSCAM importer is not yet implemented, and calling this function
#' returns an error. Please contact Blue Matter support
#' (\email{adrian@bluematterscience.com}) for help building an operating
#' model from an iSCAM assessment.
#'
#' @param iSCAMdir Character string giving the path to a directory containing
#'   iSCAM output files, or a list already returned by `load.iscam.files()`.
#' @param Name Character string giving the name of the imported operating
#'   model.
#' @param nSim Integer specifying the number of simulations.
#' @param pYear Integer specifying the number of projection years.
#' @param Agency,Author,Email,Region,Latitude,Longitude,Sponsor Character/
#'   numeric metadata stored on the resulting `OM`, matching [ImportSS()].
#' @param StockName,FleetName Character. Names for the stock and fleet.
#' @param CommonName,Species Optional character metadata for the stock.
#' @param Interval Integer specifying the number of years between management
#'   actions.
#' @param DataLag Integer specifying the observation lag in years.
#' @param MPStartYear Numeric or `NULL`. First calendar year in which MPs are
#'   applied; see [OM()]/[om-class]. Default `NULL`.
#' @param InterimAdvice A `data.frame` or `NULL`. Fixed or stochastic
#'   TAC/Effort values for years before `MPStartYear`; see [InterimAdvice()].
#'   Default `NULL`.
#' @param Seed Integer. Random number seed for the OM (`OM@Seed`), used for all
#'   stochastic values generated when the OM is populated (e.g., recruitment
#'   deviations, observation and implementation error, values sampled from
#'   parameter ranges) and for the random draws made in [Simulate()] and
#'   [Project()]. OMs imported with the same `Seed` share these random draws;
#'   use a different `Seed` for each OM (or each batch of OMs) to obtain
#'   independent draws. Default `101`.
#' @param silent Logical; if `TRUE`, suppress informational output.
#' @param Populate Logical; if `TRUE` (default), populate the OM using
#'   [PopulateOM()]. If `FALSE`, return the partially constructed OM.
#'
#' @return None. The function stops with an error.
#'
#' @seealso [ImportSS()], [ImportBAM()]
#' @export
ImportiSCAM <- function(iSCAMdir,
                        Name       = "Imported iSCAM Model",
                        nSim       = 48,
                        pYear      = 30,
                        Agency     = "",
                        Author     = "",
                        Email      = "",
                        Region     = "",
                        Latitude   = numeric(),
                        Longitude  = numeric(),
                        Sponsor    = "",
                        StockName  = "iSCAM Stock",
                        CommonName = NULL,
                        Species    = NULL,
                        FleetName  = "iSCAM Fleet",
                        Interval   = 1,
                        DataLag    = 0,
                        MPStartYear   = NULL,
                        InterimAdvice = NULL,
                        silent     = FALSE,
                        Populate   = TRUE,
                        Seed       = 101) {

  .iSCAMNotAvailable()
  .OnExit()
  replist <- if (is.character(iSCAMdir)) load.iscam.files(iSCAMdir) else iSCAMdir

  if (!is.null(replist$mpd$F_dd))
    cli::cli_abort(c(
      "x" = "`ImportiSCAM()` does not currently support delay-difference iSCAM models.",
      "i" = "Use the legacy {.fn iSCAM2OM} for delay-difference support."
    ))

  dat <- replist$dat
  mpd <- replist$mpd

  startYr <- dat$start.yr
  endYr   <- dat$end.yr
  Years   <- startYr:endYr
  nYear   <- length(Years)

  sage    <- dat$start.age
  maxage  <- dat$end.age
  ages    <- sage:maxage

  if (!silent) {
    cli::cli_h3("Importing OM from iSCAM Output")
    cli::cli_ul()
    cli::cli_li("Years: {.val {startYr} - {endYr}}")
    cli::cli_li("Ages: {.val {sage} - {maxage}}")
    cli::cli_end()
  }

  dimnames2 <- list(Age = ages, Year = Years)

  MAA <- t(mpd$M)
  dimnames(MAA) <- dimnames2
  MAA <- .IscamToSimAgeYear(MAA)

  WAA <- t(mpd$d3_wt_mat)[, seq_len(nYear), drop = FALSE] / mpd$ma
  dimnames(WAA) <- dimnames2
  WAA <- .IscamToSimAgeYear(WAA)

  MatAA <- matrix(as.numeric(mpd$ma), nrow = length(ages), ncol = nYear,
                  dimnames = dimnames2)
  MatAA <- .IscamToSimAgeYear(MatAA)

  LAA <- dat$linf[1] * (1 - exp(-dat$k[1] * (ages - dat$to[1])))
  LAA <- matrix(LAA, nrow = length(ages), ncol = nYear, dimnames = dimnames2)
  LAA <- .IscamToSimAgeYear(LAA)

  StockObj <- Stock(
    Name             = StockName,
    CommonName       = CommonName,
    Species          = Species,
    Ages             = Ages(MaxAge = maxage, MinAge = sage),
    Length           = Length(MeanAtAge = LAA),
    Weight           = Weight(MeanAtAge = WAA),
    Maturity         = Maturity(MeanAtAge = MatAA),
    NaturalMortality = NaturalMortality(MeanAtAge = MAA),
    SRR              = SRR(Pars = list(h = mpd$steepness), R0 = mpd$ro)
  )

  FAA <- t(mpd$F)
  dimnames(FAA) <- dimnames2

  SelAA <- apply(FAA, 2, \(f) if (max(f, na.rm = TRUE) > 0) f / max(f, na.rm = TRUE) else f)
  dimnames(SelAA) <- dimnames2
  SelAA <- .IscamToSimAgeYear(SelAA)

  ApicalF <- apply(FAA, 2, max, na.rm = TRUE)
  EffortSeries <- if (max(ApicalF, na.rm = TRUE) > 0) ApicalF / max(ApicalF, na.rm = TRUE) else ApicalF
  EffortSeries <- matrix(EffortSeries, nrow = 1, ncol = nYear,
                         dimnames = list(Sim = 1, Year = Years))

  FleetObj <- Fleet(
    Name        = FleetName,
    Selectivity = Selectivity(MeanAtAge = SelAA),
    Effort      = Effort(Effort = EffortSeries)
  )

  OM <- OM(
    Name        = Name,
    Agency      = Agency,
    Author      = Author,
    Email       = Email,
    Region      = Region,
    Latitude    = Latitude,
    Longitude   = Longitude,
    Sponsor     = Sponsor,
    Interval    = Interval,
    DataLag     = DataLag,
    MPStartYear   = MPStartYear,
    InterimAdvice = InterimAdvice,
    nSim        = nSim,
    Seed        = Seed,
    nYear       = nYear,
    pYear       = pYear,
    CurrentYear = endYr
  )

  OM@Stock <- MakeNamedList(StockName, StockObj)
  OM@Fleet <- MakeNamedList(StockName, MakeNamedList(FleetName, FleetObj))

  DataObj <- ImportiSCAMData(replist, Name = Name, FleetName = FleetName, silent = silent)
  OM@Data <- MakeNamedList(StockName, DataObj)

  SurveyNames <- DataObj@Survey@Name
  AllNames    <- unique(c(FleetName, SurveyNames))
  OM@Obs      <- MakeNamedList(StockName, MakeNamedList(AllNames, methods::new('obs')))
  for (nm in SurveyNames)
    OM@Obs[[1]][[nm]]@Survey@Selectivity <- 'Biomass'

  if (Populate) {
    out <- try(PopulateOM(OM), silent = TRUE)
    if (inherits(out, "om")) return(out)
    cli::cli_warn("`PopulateOM(OM)` failed with error:\n\n{.val {as.character(out)}} \n \nReturning OM object")
    return(OM)
  }
  OM
}

.IscamToSimAgeYear <- function(mat) {
  out <- array(mat, dim = c(1, dim(mat)),
              dimnames = c(list(Sim = 1), dimnames(mat)))
  out
}

.iSCAMNotAvailable <- function() {
  cli::cli_abort(c(
    "x" = "The iSCAM importer is not yet implemented.",
    "i" = "Please contact Blue Matter support ({.email office@bluematterscience.com}) for help importing iSCAM models."
  ), call = NULL)
}
