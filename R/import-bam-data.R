#' Import BAM Assessment Data into an Operating Model
#'
#' Populates the `Data` and `Obs` slots of an [om-class] object with observed
#' time-series data extracted from a BAM stock assessment. Landings, discards,
#' fleet CPUE indices, and independent survey indices are imported and stored in
#' the appropriate [data-class] sub-objects. Survey selectivity schedules are
#' stored in the corresponding [obs-class] objects so that the data object holds
#' the observed values and the obs object records how those data were generated.
#'
#' @param OM An [om-class] object. 
#' @param BAMdata A named list returned by [GetBAMOutput()] or directly
#' from the `bamExtras` pacakge.

#' @param SurveyNames A character vector of user-facing names for survey
#'   indices detected in `BAMdata` (i.e. `U.*` columns not matched to any
#'   fleet name, excluding discard indices). Must have the same length as the
#'   number of such surveys detected. If `NULL`, names are derived from the
#'   BAM column names directly.
#' @param UnitsLandings A named character vector or list mapping fleet names to
#'   unit strings. Names must be a subset of [FleetNames()]; fleets not present
#'   are assigned `NA` units with no conversion applied. Supported unit strings:
#'   - `"1000 lb"`: converts from thousands of pounds to kg.
#'   - `"1000 n"`: multiplies by 1,000 (numbers in thousands).
#'   If `NULL`, `"1000 lb"` is assumed for all detected landings fleets with a
#'   warning.
#' @param UnitsDiscards A named character vector or list following the same
#'   convention as `UnitsLandings`, applied to discard columns (`D.*`).
#' @param DiscFleets A character vector of BAM discard column name stubs used
#'   to rename discard fleets after import. Names are expected in the form
#'   `"F.FleetName.D"`, from which the middle component (e.g. `"FleetName"`)
#'   is extracted and used as the fleet label. If `NULL`, no renaming is
#'   applied.
#'
#' @return The `OM` object with two slots updated:
#'   - `OM@Data[[1]]`: a [data-class] object containing:
#'     - `@CPUE`: an [indicesdata-class] object with fleet-matched CPUE
#'       indices and CVs.
#'     - `@Survey`: an [indicesdata-class] object with unmatched (non-fleet)
#'       survey indices and CVs.
#'     - `@Landings`: a [catchdata-class] object with landings by fleet,
#'       converted to metric units where specified.
#'     - `@Discards`: a [catchdata-class] object with discards by fleet,
#'       converted to metric units where specified.
#'   - `OM@Obs[[1]]`: updated with a named [obs-class] entry for each survey,
#'     containing the selectivity-at-age schedule matched from `BAMdata$sel.age`.
#'     If no matching selectivity is found for a survey, full selection across
#'     all age classes is assumed and a warning is issued.
#'
#' @details
#' 
#' ## Fleet matching
#' Fleet CPUE columns are identified by searching `U.*` column names in
#' `BAMdata$t.series` for strings matching [FleetNames()]. Matched columns are
#' stored in `@CPUE`; unmatched `U.*` columns (excluding those containing
#' `.D.`, which are discard CPUE) are stored in `@Survey`.
#'
#' ## Unit conversion
#' Conversion is applied fleet-by-fleet using `UnitsLandings` and
#' `UnitsDiscards`. Fleets present in the OM but absent from the units argument
#' receive `NA` units and their values are left unconverted.
#'
#' ## Age and length composition
#' Composition data are not yet imported. Planned sources are
#' `BAMdata$comp.mats$*age*` (age composition) and
#' `BAMdata$comp.mats$lcomp.*.ob` (length composition).
#'
#' @seealso [GetBAMOutput()], [Data()], [IndicesData()], [CatchData()], [Obs()]
#'
#' @examples
#' \dontrun{
#' bam <- GetBAMOutput('Red Snapper')
#' OM <- ImportBAMData(
#'   OM            = myOM,
#'   BAMdata       = bam,
#'   SurveyNames   = c("VideoSurvey", "TrawlSurvey"),
#'   UnitsLandings = c(CommFleet = "1000 lb", RecFleet = "1000 lb"),
#'   UnitsDiscards = c(CommFleet = "1000 n"),
#'   DiscFleets    = c("F.CommFleet.D", "F.RecFleet.D")
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
  OM@Data[[1]]@Years <- Years(OM,'H')
  OM@Data[[1]]@YearLH <- max(OM@Data[[1]]@Years)
  
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

convert_BAM_units <- function(value, units_arg, matched_names, fleet_names) {
  units_out <- setNames(rep(NA_character_, length(fleet_names)), fleet_names)
  
  units_arg <- trimws(units_arg)
  
  nms <- dimnames(value)$Fleet
  for (i in seq_len(ncol(value))) {
    unit <- units_arg[nms[i]]  # lookup by fleet name
    if (is.na(unit)) next()
    
    if (unit == '1000 lb') {
      value[, i]    <- lb2kg(value[, i, drop = FALSE] * 1000)
      units_out[nms[i]] <- 'Biomass'
    } else if (unit == '1000 n') {
      value[, i]    <- value[, i, drop = FALSE] * 1000
      units_out[nms[i]] <- 'Number'
    }
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
  n.fleet <- length(fleet.names)
  
  if (type=='Landings') {
    catch.names <- obs.names[grepl("^L\\.", obs.names)]
    catch.data.names <- gsub("^L\\.", "", gsub("\\.ob", "", catch.names))  
  } else {
    catch.names <- obs.names[grepl("^D\\.", obs.names)]
    catch.data.names <- gsub("^D\\.", "", gsub("\\.ob", "", catch.names))
  }
  
  fleet.ind <- which(catch.data.names %in% fleet.names)
  catch.names.matched <- catch.names[fleet.ind]
  catch.data.names.matched <- catch.data.names[fleet.ind]
  n.catch <- length(catch.names.matched)
  
  if (!n.catch) return(OM)
  
  if (is.null(Units)) {
    cli::cli_alert_warning(
      '`{paste0("Units", type)}` not specified. Assuming `1000 lb` for all detected fleets.'
    )
    Units <- setNames(rep('1000 lb', n.catch), catch.data.names.matched)
  } else if (is.null(names(Units))) {
    cli::cli_abort(c(
      "x" = "`{paste0('Units', type)}` must be a named character vector.",
      "i" = "Names must be a subset of `FleetNames(OM)`: {.val {fleet.names}}."
    ))
  } else {
    invalid_names <- setdiff(names(Units), fleet.names)
    if (length(invalid_names))
      cli::cli_abort(c(
        "x" = "`{paste0('Units', type)}` contains name(s) not found in `FleetNames(OM)`.",
        "i" = "Invalid name(s): {.val {invalid_names}}.",
        "i" = "Valid fleet names: {.val {fleet.names}}."
      ))
  }
  
  catchData <- CatchData(Name = fleet.names)
  catchData@Value <- array(NA, c(n.year, n.fleet),
                           dimnames = list(Year  = years,
                                           Fleet = fleet.names))
  
  # Only fill in values for fleets that exist in BAM data
  catch.series <- as.matrix(t.series[seq_len(n.year), catch.names.matched])
  dimnames(catch.series) <- list(
    Year  = years,
    Fleet = catch.data.names.matched
  )
  
  if (is.null(Units)) {
    cli::cli_alert_warning(
      '`{paste0("Units", type)}` not specified. Assuming `1000 lb` for all detected fleets.'
    )
    Units <- setNames(rep('1000 lb', n.catch), catch.data.names.matched)
  } else if (is.null(names(Units))) {
    cli::cli_abort(c(
      "x" = "`{paste0('Units', type)}` must be a named character vector.",
      "i" = "Names must be a subset of `FleetNames(OM)`: {.val {fleet.names}}."
    ))
  } else {
    invalid_names <- setdiff(names(Units), fleet.names)
    if (length(invalid_names))
      cli::cli_abort(c(
        "x" = "`{paste0('Units', type)}` contains name(s) not found in `FleetNames(OM)`.",
        "i" = "Invalid name(s): {.val {invalid_names}}.",
        "i" = "Valid fleet names: {.val {fleet.names}}."
      ))
  }
  
  catchData <- CatchData(Name = fleet.names)
  catchData@Value <- array(NA, c(n.year, n.fleet),
                           dimnames = list(Year  = years,
                                           Fleet = fleet.names))
  
  # Only fill in values for fleets that exist in BAM data
  catch.series <- as.matrix(t.series[seq_len(n.year), catch.names.matched])
  dimnames(catch.series) <- list(
    Year  = years,
    Fleet = catch.data.names.matched
  )
  
  conv <- convert_BAM_units(value         = catch.series,
                            units_arg     = Units,
                            matched_names = catch.data.names.matched,
                            fleet_names   = fleet.names
  )

  
  ArrayFill(catchData@Value) <- conv$value
  catchData@Units <- conv$units

  slot(OM@Data[[1]],type) <- catchData  
  OM
}

ImportBAM_CPUE <- function(OM, BAMdata) {
  
  t.series  <- BAMdata$t.series
  years     <- t.series$year
  years     <- years[years %in% Years(OM, 'H')]
  
  fleet.names <- FleetNames(OM)
  n.fleet     <- length(fleet.names)
  n.year      <- length(years)
  cnames      <- colnames(t.series)
  obs.names   <- cnames[grepl("\\.ob", cnames)]
  
  indices.names <- obs.names[grepl("^U\\.", obs.names)]
  
  # Strip prefix/suffix to get plain data names for all indices
  all.data.names <- gsub("\\.ob", "", gsub("^U\\.", "", indices.names))
  
  # Match to OM fleet names, excluding discard indices
  fleet.ind             <- which(all.data.names %in% fleet.names & !grepl("\\.D\\.", indices.names))
  cpue.names.matched    <- indices.names[fleet.ind]
  cpue.data.names.matched <- all.data.names[fleet.ind]
  n.cpue                <- length(cpue.names.matched)
  
  if (!n.cpue) return(OM)
  
  cpue.data.object <- IndicesData(Name = fleet.names)
  cpue.data.object@Value <- array(NA, c(n.year, n.fleet),
                                  dimnames = list(Year  = years,
                                                  Fleet = fleet.names))
  cpue.data.object@CV <- cpue.data.object@Value
  
  cpue.series <- as.matrix(t.series[seq_len(n.year), cpue.names.matched])
  dimnames(cpue.series) <- list(Year  = years,
                                Fleet = cpue.data.names.matched)
  
  cv.series <- as.matrix(extract_cv(t.series, cpue.names.matched)[seq_len(n.year), ])
  dimnames(cv.series) <- list(Year  = years,
                              Fleet = cpue.data.names.matched)
  
  ArrayFill(cpue.data.object@Value) <- cpue.series
  ArrayFill(cpue.data.object@CV)    <- cv.series
  
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

