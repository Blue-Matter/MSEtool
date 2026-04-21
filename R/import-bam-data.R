# Units for catches? Number vs Biomass ??
# add as an argument and provide manually ...

#' Import BAM Assessment Data into an Operating Model
#'
#' Populates a [data-class] object with observed time-series data (CPUE,
#' survey indices, landings, and discards) extracted from a BAM stock
#' assessment. 
#'
#' @param OM An operating model object. The stock name is taken from
#'   `OM@Stock[[1]]@Name`, and fleet names are derived via [FleetNames()].
#' @param BAMdata A named list produced by the `BAMextras` package containing
#'   BAM assessment output for a single stock. Expected to contain at minimum:
#'   - `$t.series` — a data frame with a `year` column and columns for
#'     observed indices (`U.*`), CVs (`cv.*`), landings (`L.*`), and
#'     discards (`D.*`).
#'   - `$sel.age` — a named list of selectivity-at-age vectors.
#' @param SurveyNames A character vector of user-facing names for survey
#'   indices detected in `BAMdata`. Must have the same length as the number of
#'   survey indices found (i.e. indices not matched to any fleet name). Use
#'   this to override the default names derived from column names.
#' @param UnitsLandings A character vector of length equal to the number of
#'   landings fleets detected. Each element must be one of:
#'   - `"1000 lb"` — values will be converted from thousands of pounds to kg.
#'   - `"1000 n"` — values will be multiplied by 1,000 (number in thousands).
#' @param UnitsDiscards A character vector of length equal to the number of
#'   discard fleets detected. Accepts the same values as `UnitsLandings`.
#'
#' @return The `OM` object with the `Data` slot populated with a [data-class]
#' object with:
#'   - `@CPUE` — an [indicesdata-class] object with fleet-matched CPUE
#'     indices and CVs.
#'   - `@Survey` — an [indicesdata-class] object with non-fleet survey
#'     indices and CVs.
#'   - `@Landings` — a [catchdata-class] object with landings by fleet,
#'     converted to metric units.
#'   - `@Discards` — a [catchdata-class] object with discards by fleet,
#'     converted to metric units.
#'
#' Fleet matching is performed by searching for fleet names (from
#' [FleetNames()]) within the `U.*` observed-index column names of
#' `BAMdata$t.series`. Columns matched to a fleet are treated as CPUE;
#' unmatched `U.*` columns are treated as independent survey indices.
#' Discard fleet CPUE columns (those containing `.D.`) are excluded from the
#' CPUE object.
#'
#' Unit conversion for landings and discards is applied fleet-by-fleet based
#' on `UnitsLandings` and `UnitsDiscards`. Unrecognised unit strings result in
#' no conversion and an empty `Units` string for that fleet.
#'
#' Age and length composition data are not yet imported (TODO).
#'
#' @seealso [FleetNames()], [Data()], [IndicesData()], [CatchData()]
#'
#' @examples
#' \dontrun{
#' bam <- BAMextras::readBAM("mystock.rdat")
#' dat <- ImportBAMData(
#'   OM            = myOM,
#'   BAMdata       = bam,
#'   SurveyNames   = c("VideoSurvey", "TrawlSurvey"),
#'   UnitsLandings = c("1000 lb", "1000 lb"),
#'   UnitsDiscards = c("1000 n")
#' )
#' }
#'
#' @export
ImportBAMData <- function(OM,
                          BAMdata,
                          SurveyNames = NULL,
                          UnitsLandings = NULL,
                          UnitsDiscards = NULL,
                          DiscFleets    = NULL) {
  
  # create new data object 
  OM@Data <- MakeNamedList(StockNames(OM), Data(Name = OM@Stock[[1]]@Name))
  
  # Landings & Discards
  OM <- ImportBAM_Catch(OM, BAMdata, Units = UnitsLandings, type = 'Landings') 
  OM <- ImportBAM_Catch(OM, BAMdata, Units = UnitsDiscards, type = 'Discards')
  if (!is.null(DiscFleets)) 
    OM@Data[[1]]@Discards <- Rename_Fleet(object=OM@Data[[1]]@Discards, Fleets=as.list(strip_between_periods(DiscFleets)))
  
  # CPUE
  OM <- ImportBAM_CPUE(OM, BAMdata)
  
  # Surveys 
  OM <- ImportBAM_Survey(OM, BAMdata, SurveyNames)
  
  # Composition 
  # TODO: Age composition   — BAMdata$comp.mats$*age*
  # TODO: Length composition — BAMdata$comp.mats$lcomp.*.ob

  OM
}

strip_between_periods <- function(x) sub("^[^.]*\\.([^.]*)\\..*$", "\\1", x)

extract_cv <- function(t.series, obs.names) {
  cv.names <- gsub(".ob", "", paste0("cv.", obs.names))
  as.matrix(t.series[cv.names])
}

convert_BAM_units <- function(value, units_arg, n_fleets, fleet_label) {
  units_out <- rep("", n_fleets)
  
  ind_lb <- grepl("1000 lb", units_arg)
  if (any(ind_lb)) {
    i_ind <- which(ind_lb)
    value[, i_ind] <- lb2kg(value[, i_ind, drop = FALSE] * 1000)
    units_out[i_ind] <- "Biomass"
  }
  
  ind_n <- grepl("1000 n", units_arg)
  if (any(ind_n)) {
    i_ind <- which(ind_n)
    value[, i_ind] <- value[, i_ind, drop = FALSE] * 1000
    units_out[i_ind] <- "Number"
  }
  
  list(value = value, units = units_out)
}


ImportBAM_Catch <- function(OM, BAMdata, Units, type=c('Landings', 'Discards')) {
  type <- match.arg(type)
  
  if (is.null(OM@Obs)) 
    OM@Obs <- MakeNamedList(StockNames(OM), MakeNamedList(FleetNames(OM), new('obs')))
  
  t.series <- BAMdata$t.series
  years    <- t.series$year
  years <- years[years %in% Years(OM,'H')]
  
  n.year   <- length(years)
  cnames   <- colnames(t.series)
  obs.names <- cnames[grepl("\\.ob", cnames)]
  
  fleet.names <- FleetNames(OM)
  if (type=='Landings') {
    catch.names <- obs.names[grepl("^L\\.", obs.names)]
    catch.data.names <- gsub("^L\\.", "", gsub("\\.ob", "", catch.names))  
  } else {
    catch.names <- obs.names[grepl("^D\\.", obs.names)]
    catch.data.names <- gsub("^D\\.", "", gsub("\\.ob", "", catch.names))
  }
  
  fleet.ind <- which(catch.data.names %in% fleet.names)
  catch.names <- catch.names[fleet.ind]
  catch.data.names <- catch.data.names[fleet.ind]
  n.catch <- length(catch.names)
  
  if (!n.catch) return(OM)
  
  if (is.null(Units)) {
    cli::cli_alert_warning('{.val {paste0(type, "Units")}} not specified. Assuming `1000 lb` for all fleets')
    Units <- rep('1000 lb', n.catch)
  }
  
  if (length(Units) != n.catch)
    cli::cli_abort(c(
      "x" = "`{paste0('Units', type)}` must be provided and have one element per {tolower(type)} fleet.",
      "i" = "{n.catch} {tolower(type)} fleet(s) detected: {.val {catch.names}}.",
      "i" = "Provided units: {.val {Units}}."
    ))
  
  
  catchData <- CatchData(Name = catch.data.names)
  catchData@Value <- array(NA, c(n.year, n.catch),
                           dimnames = list(Year  = years,
                                           Fleet = catch.data.names))
  catchData@Value[] <- as.matrix(t.series[catch.names])[seq_len(n.year),,drop=FALSE]
  
  conv <- convert_BAM_units(catchData@Value, Units, n.catch, tolower(type))
  catchData@Value <- conv$value
  catchData@Units <- conv$units

  slot(OM@Data[[1]],type) <- catchData  
  OM
}

ImportBAM_CPUE <- function(OM, BAMdata) {
  
  t.series <- BAMdata$t.series
  years    <- t.series$year
  years <- years[years %in% Years(OM,'H')]
  
  fleet.names <- FleetNames(OM)
  n.year   <- length(years)
  cnames   <- colnames(t.series)
  obs.names <- cnames[grepl("\\.ob", cnames)]
  
  indices.names <- obs.names[grepl("^U\\.", obs.names)]
  
  # CPUE  (fleet-matched indices) 
  cpue.names.all <- NULL
  for (i in seq_along(fleet.names))
    cpue.names.all <- c(cpue.names.all,
                        indices.names[grepl(fleet.names[i], indices.names)])
  
  cpue.names <- cpue.names.all[!grepl("\\.D\\.", cpue.names.all)]
  n.cpue     <- length(cpue.names)
  
  if (!n.cpue) return(OM)
  
  cpue.data.names <- gsub("\\.ob", "", gsub("^U\\.", "", cpue.names))
  cpue.data.object <- IndicesData(Name = cpue.data.names)
  
  cpue.data.object@Value <- array(NA, c(n.year, n.cpue),
                                  dimnames = list(Year  = years,
                                                  Fleet = cpue.data.names))
  
  cpue.data.object@Value[] <- as.matrix(t.series[cpue.names])[seq_len(n.year),,drop=FALSE]
  cpue.data.object@CV      <- cpue.data.object@Value
  cpue.data.object@CV[]    <- extract_cv(t.series, cpue.names)[seq_len(n.year),,drop=FALSE]
  
  OM@Data[[1]]@CPUE <- cpue.data.object
  OM
}


ImportBAM_Survey <- function(OM, BAMdata, SurveyNames=NULL) {
  
  t.series <- BAMdata$t.series
  years    <- t.series$year
  years <- years[years %in% Years(OM,'H')]
  
  n.year   <- length(years)
  cnames   <- colnames(t.series)
  obs.names <- cnames[grepl("\\.ob", cnames)]
  fleet.names <- FleetNames(OM)
  indices.names <- obs.names[grepl("^U\\.", obs.names)]
  
  indices.fleet.names <- gsub('^U\\.', '', gsub('\\.ob', '', indices.names))
  
  # Survey (non-fleet-matched indices) 
  survey.names.all <- indices.names[!indices.fleet.names %in% fleet.names]
  
  survey.names <- survey.names.all[!grepl("\\.D\\.", survey.names.all)]
  
  n.survey <- length(survey.names)
  
  if (!n.survey) return(OM)
  
  if (!is.null(SurveyNames)) {
    if (length(SurveyNames) != n.survey)
      cli::cli_abort(c(
        "x" = "`length(SurveyNames)` must equal the number of surveys detected.",
        "i" = "`SurveyNames` has length {length(SurveyNames)}.",
        "i" = "Detected {n.survey} survey(s): {.val {survey.names}}."
      ))
    survey.data.names <- SurveyNames
  } else {
    survey.data.names <- gsub("\\.ob", "", survey.names)
    survey.data.names <- gsub("^U\\.", "", survey.data.names)
  }
  
  survey.data.object <- IndicesData(Name = survey.data.names)
  
  survey.data.object@Value <- array(NA, c(n.year, n.survey),
                                    dimnames = list(Year  = years,
                                                    Fleet = survey.data.names))
  survey.data.object@CV      <- survey.data.object@Value
  
  survey.data.object@Value[] <- as.matrix(t.series[survey.names])[seq_len(n.year),,drop=FALSE]
  survey.data.object@Value[ survey.data.object@Value<=-9999] <- NA
  survey.data.object@CV[]    <- extract_cv(t.series, survey.names)[seq_len(n.year),,drop=FALSE]
  
  survey.data.object@Selectivity <- rep("Obs", n.survey)
  
  OM@Data[[1]]@Survey <- survey.data.object
  
  # Add Obs object for surveys
  nms <- gsub("^U\\.", "", survey.names)
  nms <- gsub("\\.ob", "", nms)
  sel.names    <- names(BAMdata$sel.age)
  select.names <- NULL
  
  for (i in seq_along(nms)) {
    matched.sel <- sel.names[grepl(nms[i], sel.names)]
    if (!length(matched.sel)) {
      cli::cli_alert_warning("No matching selectivity schedule found for Survey: {.val {nms[i]}}")
      cli::cli_alert('Assuming all age class fully selected')
      survey.data.object@Selectivity[i] <- 'Biomass'
      OM@Data[[1]]@Survey <- survey.data.object
      
      ObsList <- list(Obs())
      names(ObsList) <- survey.data.names[i]
      OM@Obs[[1]] <- c(OM@Obs[[1]], ObsList)
      
      next
    } else {
      if (length(matched.sel)>1)
        matched.sel <- matched.sel[1]
      survey.sel.age <- BAMdata$sel.age[[matched.sel]]
      
      if (is.array(survey.sel.age)) {
        survey.years <- rownames(survey.sel.age)
        survey.ages <- colnames(survey.sel.age)  
      } else {
        survey.ages <- names(survey.sel.age)
        survey.sel.age <- matrix(survey.sel.age, length(survey.ages), 1)
        survey.years <- rownames(OM@Data[[1]]@Survey@Value)
      }
      
      SurveyObs <- Obs()
      SurveyObs@Survey@Selectivity <- array(t(survey.sel.age), 
                                            dim=c(length(survey.ages),
                                                  length(survey.years)),
                                            dimnames = list(
                                              Age = survey.ages,
                                              Year = survey.years
                                            )) |> AddDimension('Sim', pos=1)
      
      ObsList <- list(SurveyObs)
      names(ObsList) <- survey.data.names[i]
      OM@Obs[[1]] <- c(OM@Obs[[1]], ObsList)
    }
  }
  
  OM
}

