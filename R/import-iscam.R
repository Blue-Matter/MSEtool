#' Import an iSCAM Model into an Operating Model
#'
#' Import an iSCAM (Integrated Statistical Catch-Age Model) MPD fit and
#' convert it into an [OM()] object, targeting the current `om`/`hist`/`data`
#' class system (unlike the legacy [iSCAM2OM()], which targets the older
#' `OM`/`Data` classes).
#'
#' @details
#' `ImportiSCAM()` reads iSCAM output files from `iSCAMdir` (via
#' `load.iscam.files()`, the same reader used by the legacy [iSCAM2OM()]) and
#' constructs an [OM()] object:
#'
#' - Growth (`Length`), weight-at-age, maturity-at-age, and natural
#'   mortality-at-age are read directly from the MPD fit
#'   (`replist$mpd$M`/`replist$mpd$d3_wt_mat`/`replist$mpd$ma`, and
#'   `replist$dat$linf`/`k`/`to` for the von Bertalanffy growth curve).
#' - The stock-recruit relationship uses iSCAM's fitted steepness and R0
#'   (`replist$mpd$steepness`/`replist$mpd$ro`).
#' - **Only a single, aggregate fleet is built.** iSCAM's MPD reconstruction
#'   (`replist$mpd$F`) is a single combined fishing-mortality-at-age series,
#'   not broken out by gear -- this matches the scope of the legacy
#'   [iSCAM2OM()]/[iSCAM2Data()], which also collapse multi-gear catch into
#'   one total rather than building per-gear `Fleet` dynamics. The fleet's
#'   relative selectivity-at-age is derived by normalising `replist$mpd$F`
#'   within each year (`F_a,y / max_a(F_a,y)`), and its historical `Effort`
#'   is the resulting apical F series, normalised to its historical maximum
#'   -- i.e. effort is assumed proportional to apical F under constant
#'   catchability. **This approximates iSCAM's fitted exploitation history;
#'   it is not a byte-for-byte reconstruction of iSCAM's own (state-space)
#'   historical trajectory**, since [Simulate()] derives numbers-at-age from
#'   this effort series and the biology above, rather than reusing
#'   `replist$mpd$N` directly.
#' - `ImportiSCAMData()` (called internally, or usable standalone) populates
#'   `OM@Data` with aggregate landings and any abundance indices
#'   (`replist$dat$indices`). Age/size composition data is not yet imported
#'   -- like [ImportBAMData()]'s equivalent gap, this is a documented TODO;
#'   see [compdata-class] for the per-fleet convention to follow.
#'
#' **Not currently supported**: delay-difference models (`replist$mpd$F_dd`)
#' and MCMC-based import (multiple simulation replicates drawn from
#' `read.mcmc()`) -- both are handled by the legacy [iSCAM2OM()] but are out
#' of scope for this first version; `ImportiSCAM()` errors if either
#' is detected.
#'
#' @param iSCAMdir Character string giving the path to a directory containing
#'   iSCAM output files, or a list already returned by `load.iscam.files()`.
#' @param Name Character string giving the name of the imported operating
#'   model.
#' @param nSim Integer specifying the number of simulations. iSCAM's MPD fit
#'   is a single point estimate, so every simulation starts from the same
#'   biology/exploitation history; simulation-to-simulation variability comes
#'   from [Simulate()]'s own process/observation error, not from iSCAM.
#' @param pYear Integer specifying the number of projection years.
#' @param Agency,Author,Email,Region,Latitude,Longitude,Sponsor Character/
#'   numeric metadata stored on the resulting `OM`, matching [ImportSS()].
#' @param StockName,FleetName Character. Names for the single stock/fleet
#'   built from the iSCAM model.
#' @param CommonName,Species Optional character metadata for the stock.
#' @param Interval Integer specifying the number of years between management
#'   actions.
#' @param DataLag Integer specifying the observation lag in years.
#' @param MPStartYear Numeric or `NULL`. First calendar year in which MPs are
#'   applied; see [OM()]/[om-class]. Default `NULL`.
#' @param InterimAdvice A `data.frame` or `NULL`. Fixed or stochastic
#'   TAC/Effort values for years before `MPStartYear`; see [OM()]/[om-class].
#'   Default `NULL`.
#' @param silent Logical; if `TRUE`, suppress informational output.
#' @param Populate Logical; if `TRUE` (default), populate the OM using
#'   [PopulateOM()]. If `FALSE`, return the partially constructed OM.
#'
#' @return An [om-class] object.
#'
#' @seealso [OM()], [PopulateOM()], [CompareiSCAM()], [iSCAM2OM()] (legacy),
#'   [ImportSS()], [ImportBAM()]
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
                        Populate   = TRUE) {

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
