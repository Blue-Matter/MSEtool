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
#' @param UnitsLandings Optional named character vector or list mapping fleet
#'   names to unit strings. Names must be a subset of [FleetNames()]. Supported
#'   unit strings:
#'   - `"1000 lb"`: converts from thousands of pounds to kg.
#'   - `"1000 n"`: multiplies by 1,000 (numbers in thousands).
#'   Fleets not named here (all fleets if `NULL`) have their units detected
#'   from `BAMdata` - see *Unit conversion*.
#' @param UnitsDiscards A named character vector or list following the same
#'   convention as `UnitsLandings`, applied to discard columns (`D.*`). When
#'   `DiscFleets` is supplied, names here must be the final (retained-fleet)
#'   names it maps to, not the raw BAM discard stub - see `DiscFleets`.
#' @param DiscFleets A named character vector mapping final fleet names (i.e.
#'   names appearing in [FleetNames()]) to raw BAM discard column stubs, in
#'   the form `FinalName = "F.RawStub.D"` (the middle component, `"RawStub"`,
#'   is extracted and matched against detected `D.*` columns). Used when a
#'   BAM assessment's discard columns use a different fleet code than the
#'   corresponding retained fleet (e.g. a discard-only code distinct from the
#'   landings code) - matching raw discard columns are remapped to their
#'   final fleet name before import, so `UnitsDiscards` and the resulting
#'   `@Discards` data are keyed by final fleet names throughout, consistently
#'   with `@Landings`. If `NULL` (default), discard columns are matched
#'   directly against [FleetNames()] with no remapping.
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
#' Conversion is applied fleet-by-fleet. Units given in `UnitsLandings` and
#' `UnitsDiscards` are used as supplied. For all other fleets, the units are
#' detected from `BAMdata$LD.pr.tseries`: the fleet's predicted series
#' (`L.<fleet>.pr` or `D.<fleet>.pr` in `BAMdata$t.series`) is matched against
#' the predictions in thousands of pounds (`.klb`, `.gutklb`; `"1000 lb"`) and
#' thousands of fish (`.knum`; `"1000 n"`). If no match is found, the units
#' are taken from `BAMdata$info$units.landings` or
#' `BAMdata$info$units.discards`, with a warning. For discards, any raw BAM
#' column stub named in `DiscFleets` is remapped to its final fleet name before
#' units are matched (see `DiscFleets`).
#'
#' ## Age and length composition
#' Composition data are not imported.
#'
#' @seealso [GetBAMOutput()], [Data()], [CatchData()], [Obs()]
#'
#' @examples
#' \dontrun{
#' bam <- GetBAMOutput('Red Snapper')
#' OM  <- ImportBAM(bam, nSim = 2, incData = FALSE)
#'
#' # landings in 1000 lb, discards in 1000 fish; surveys are the chevron trap and video indices
#' OM <- ImportBAMData(
#'   OM            = OM,
#'   BAMdata       = bam,
#'   SurveyNames   = c("ChevronTrap", "Video"),
#'   UnitsLandings = c(cHL = "1000 lb", rHB = "1000 lb", rGN = "1000 lb"),
#'   UnitsDiscards = c(cHL = "1000 n", rHB = "1000 n", rGN = "1000 n")
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
  OM <- .ImportBAMCatch(OM, BAMdata, Units = UnitsLandings, type = 'Landings')
  OM <- .ImportBAMCatch(OM, BAMdata, Units = UnitsDiscards, type = 'Discards', DiscFleets = DiscFleets)

  # CPUE
  OM <- .ImportBAMCPUE(OM, BAMdata)
  
  # Surveys 
  OM <- .ImportBAMSurvey(OM, BAMdata, SurveyNames)
  
  # Composition
  # TODO: Age composition   — BAMdata$comp.mats$*age*
  # TODO: Length composition — BAMdata$comp.mats$lcomp.*.ob
  #   `Classes` must be a named list, one vector per fleet (see
  #   compdata-class and .ImportSSDataAtSize() for a worked example) --
  #   fleets are not required to share a length-bin grid.

  OM
}

.StripBetweenPeriods <- function(x) sub("^[^.]*\\.([^.]*)\\..*$", "\\1", x)

.ExtractCv <- function(t.series, obs.names) {
  cv.names <- gsub(".ob", "", paste0("cv.", obs.names))
  as.matrix(t.series[cv.names])
}

.ConvertBAMUnits <- function(value, units_arg, matched_names, fleet_names) {
  units_out <- setNames(rep(NA_character_, length(fleet_names)), fleet_names)
  
  units_arg <- trimws(unlist(units_arg))
  
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


# Units per fleet from the LD.pr.tseries column that reproduces the fleet's `.pr` series
.DetectBAMCatchUnits <- function(BAMdata, raw.names, final.names, type) {
  prefix   <- if (type == 'Landings') 'L' else 'D'
  t.series <- BAMdata$t.series
  ld       <- BAMdata$LD.pr.tseries
  suffix.units <- c(klb = '1000 lb', gutklb = '1000 lb', knum = '1000 n')

  units <- setNames(rep(NA_character_, length(raw.names)), final.names)
  if (!is.null(ld) && !is.null(ld$year)) {
    yr <- match(t.series$year, ld$year)
    for (i in seq_along(raw.names)) {
      stub <- paste0(prefix, '.', raw.names[i])
      pr   <- t.series[[paste0(stub, '.pr')]]
      if (is.null(pr)) next
      err <- vapply(names(suffix.units), \(sfx) {
        x <- ld[[paste0(stub, '.', sfx)]]
        if (is.null(x)) return(Inf)
        x  <- x[yr]
        ok <- !is.na(pr) & !is.na(x) & pr > 0
        if (!any(ok)) return(Inf)
        stats::median(abs(x[ok] / pr[ok] - 1))
      }, numeric(1))
      if (min(err) < 1e-3) units[i] <- suffix.units[[which.min(err)]]
    }
  }

  missing <- is.na(units)
  if (any(missing)) {
    info <- BAMdata$info[[paste0('units.', tolower(type))]]
    fallback <- if (!is.null(info) && grepl('fish|num', info, ignore.case = TRUE)) '1000 n' else '1000 lb'
    units[missing] <- fallback
    cli::cli_alert_warning(
      'Could not detect {tolower(type)} units for fleet{?s} {.val {final.names[missing]}} from `BAMdata$LD.pr.tseries`. Assuming {.val {fallback}}; set `{paste0("Units", type)}` to override.'
    )
  }
  units
}

.ImportBAMCatch <- function(OM, BAMdata, Units, type=c('Landings', 'Discards'), DiscFleets = NULL) {
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
    raw.names <- catch.data.names
  } else {
    catch.names <- obs.names[grepl("^D\\.", obs.names)]
    catch.data.names <- gsub("^D\\.", "", gsub("\\.ob", "", catch.names))
    raw.names <- catch.data.names

    # A BAM assessment's discard columns may use a different fleet code than
    # the corresponding retained fleet (e.g. Gray Triggerfish: `rHDs` discard
    # data belongs to the `rHBs` retained fleet). `DiscFleets` maps
    # FinalName = "F.RawStub.D"; remap any matching raw stub to its final
    # fleet name here, before matching against `FleetNames(OM)` below, so
    # `Units` and the resulting data are keyed by final fleet names
    # throughout - consistently with `Landings`.
    if (!is.null(DiscFleets)) {
      raw_to_final <- setNames(names(DiscFleets), .StripBetweenPeriods(DiscFleets))
      ind <- catch.data.names %in% names(raw_to_final)
      catch.data.names[ind] <- raw_to_final[catch.data.names[ind]]
    }
  }

  fleet.ind <- which(catch.data.names %in% fleet.names)
  catch.names.matched <- catch.names[fleet.ind]
  catch.data.names.matched <- catch.data.names[fleet.ind]
  n.catch <- length(catch.names.matched)

  if (!n.catch) return(OM)

  if (!is.null(Units) && is.null(names(Units))) {
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

  Detected <- .DetectBAMCatchUnits(BAMdata, raw.names[fleet.ind], catch.data.names.matched, type)
  Units <- unlist(Units)
  Units <- c(Units, Detected[setdiff(names(Detected), names(Units))])

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

  conv <- .ConvertBAMUnits(value         = catch.series,
                            units_arg     = Units,
                            matched_names = catch.data.names.matched,
                            fleet_names   = fleet.names
  )

  
  ArrayFill(catchData@Value) <- conv$value
  catchData@Units <- conv$units

  slot(OM@Data[[1]],type) <- catchData  
  OM
}

.ImportBAMCPUE <- function(OM, BAMdata) {
  
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
  
  cv.series <- as.matrix(.ExtractCv(t.series, cpue.names.matched)[seq_len(n.year), ])
  dimnames(cv.series) <- list(Year  = years,
                              Fleet = cpue.data.names.matched)
  
  ArrayFill(cpue.data.object@Value) <- cpue.series
  ArrayFill(cpue.data.object@CV)    <- cv.series
  
  OM@Data[[1]]@CPUE <- cpue.data.object
  OM
}


.ImportBAMSurvey <- function(OM, BAMdata, SurveyNames=NULL) {
  
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
  survey.data.object@CV[]    <- .ExtractCv(t.series, survey.names)[seq_len(n.year),,drop=FALSE]
  
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

      SurveyObs <- Obs()
      SurveyObs@Survey@Selectivity <- 'Biomass'

      ObsList <- list(SurveyObs)
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
