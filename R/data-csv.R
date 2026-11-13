# Read and write `data` objects as CSV files

.DataCSVSlots <- list(
  Catch   = c("Landings", "Discards"),
  Effort  = "Effort",
  Index   = c("CPUE", "Survey"),
  Comp    = c("LandingsAtAge", "DiscardsAtAge", "LandingsAtSize", "DiscardsAtSize")
)

.DataCSVMeta <- c("Name", "CommonName", "Species", "Agency", "Author", "Email", "Region",
                  "Latitude", "Longitude", "FirstYear", "LastYear", "Seasons")

.DataCSVColumns <- c("Slot", "Name", "Year", "Season", "Class", "Value", "CV", "Units", "Timing")

# accessor for a data slot, by slot name
.DataCSVGet <- function(Data, sl) {
  match.fun(sl)(Data)
}

# calendar year and season of each time step
.CSVSteps <- function(Steps) {
  Year <- floor(Steps + 1e-8)
  data.frame(Step = Steps, Year = Year, Season = stats::ave(seq_along(Steps), Year, FUN = seq_along))
}

#' Read and Write a `data` Object as a CSV File
#'
#' `ReadDataCSV()` creates a [data-class] object from a CSV file in long format, with one row per
#' observation. `WriteDataCSV()` writes a [data-class] object to a CSV file in the same format, e.g., to make a
#' template to fill in.
#'
#' @param file Path to the CSV file.
#' @param Years Optional numeric vector of the historical calendar years. Defaults to the `FirstYear` and
#' `LastYear` metadata rows, or else to the range of years in the file.
#' @param silent Logical. Suppress messages?
#'
#' @details
#' ## File format
#' One row per observation, with columns:
#'
#' | Column | Required | Description |
#' |---|---|---|
#' | `Slot` | yes | The [data-class] slot: `Landings`, `Discards`, `Effort`, `CPUE`, `Survey`, `LandingsAtAge`, `DiscardsAtAge`, `LandingsAtSize`, `DiscardsAtSize`, or `Metadata` |
#' | `Name` | yes | Fleet name (catch, effort and composition slots) or index name (`CPUE`, `Survey`). For `Metadata`, the field name |
#' | `Year` | yes | Calendar year of the observation. Empty for `Metadata` |
#' | `Season` | seasonal | Season within the year, from 1 to `Seasons`. Only used when `Seasons > 1` |
#' | `Class` | comps | Age class (`*AtAge`) or length-bin midpoint (`*AtSize`). Empty for other slots |
#' | `Value` | yes | Observed value. For `Metadata`, the field value |
#' | `CV` | no | Coefficient of variation of `Value` (catch, effort and index slots) |
#' | `Units` | no | Units of the series, e.g. `Biomass` or `Number` for catch and indices, `cm` for length compositions. One value per `Slot` and `Name` |
#' | `Timing` | no | Fraction of the time step when an index is observed (`CPUE`, `Survey`). Default 0 |
#'
#' `Metadata` rows set the `data` slots `Name`, `CommonName`, `Species`, `Agency`, `Author`, `Email`, `Region`,
#' `Latitude` and `Longitude`, the historical years with `FirstYear` and `LastYear`, and the number of seasons
#' per year with `Seasons` (default 1).
#'
#' Fleets are ordered by their first appearance in any catch, effort or composition slot, and every one of these
#' slots has the same fleets in that order; a fleet with no rows in a slot is `NA` there. Indices are ordered by
#' their first appearance within each index slot. Years without rows are `NA`. For the
#' composition slots, a class with no row in a year and fleet that has other rows is 0, so only non-zero counts
#' need to be listed. All fleets in a composition slot share one set of classes, the sorted classes in the file,
#' so a class must appear in at least one row (with value 0 if need be) to be included. `WriteDataCSV()` writes
#' every class, including zeros, in each year with data.
#'
#' ## Seasonal data
#' With `Seasons > 1`, each row is identified by `Year` and `Season`, and the time steps of the `data` object
#' are the decimal years from [CalcYears()] for `FirstYear` to `LastYear`. `Seasons` must be one of the values
#' accepted by [CalcTSUnits()]. In a seasonal file, a row with an empty
#' `Season` may instead give the decimal time step in `Year`. `WriteDataCSV()` numbers the time steps within
#' each calendar year as seasons, and omits the `Season` column for annual data.
#'
#' @return `ReadDataCSV()` returns a [data-class] object. `WriteDataCSV()` invisibly returns the data frame it
#' wrote.
#' @examples
#' file <- system.file("extdata", "ExampleData.csv", package = "MSEtool")
#' dat <- ReadDataCSV(file)
#' dat
#'
#' WriteDataCSV(dat, file.path(tempdir(), "ExampleData.csv"), overwrite = TRUE)
#' @export
ReadDataCSV <- function(file, Years = NULL, silent = FALSE) {
  if (!file.exists(file)) cli::cli_abort("File {.file {file}} not found.")
  df <- utils::read.csv(file, stringsAsFactors = FALSE, na.strings = c("", "NA"),
                        colClasses = "character", strip.white = TRUE, check.names = FALSE)
  names(df) <- trimws(names(df))

  missing_cols <- setdiff(c("Slot", "Name", "Value"), names(df))
  if (length(missing_cols)) cli::cli_abort("Column{?s} {.val {missing_cols}} missing from {.file {file}}.")
  for (cl in setdiff(.DataCSVColumns, names(df))) df[[cl]] <- NA_character_
  unknown_cols <- setdiff(names(df), .DataCSVColumns)
  if (length(unknown_cols) && !silent) cli::cli_alert_info("Ignoring column{?s} {.val {unknown_cols}}.")

  df <- df[!is.na(df$Slot), , drop = FALSE]
  valid_slots <- c(unlist(.DataCSVSlots), "Metadata")
  bad <- setdiff(unique(df$Slot), valid_slots)
  if (length(bad)) {
    cli::cli_abort(c("Unknown {.field Slot} value{?s}: {.val {bad}}.", "i" = "Valid values: {.val {valid_slots}}."))
  }

  # Metadata
  meta_df <- df[df$Slot == "Metadata", , drop = FALSE]
  meta <- stats::setNames(as.list(meta_df$Value), meta_df$Name)
  valid_meta <- .DataCSVMeta
  bad <- setdiff(names(meta), valid_meta)
  if (length(bad)) cli::cli_abort(c("Unknown {.field Metadata} field{?s}: {.val {bad}}.", "i" = "Valid fields: {.val {valid_meta}}."))

  obs <- df[df$Slot != "Metadata", , drop = FALSE]
  if (any(is.na(obs$Name))) cli::cli_abort("{.field Name} is missing in {sum(is.na(obs$Name))} row{?s}.")
  num <- function(x, col) {
    out <- suppressWarnings(as.numeric(x))
    bad <- !is.na(x) & is.na(out)
    if (any(bad)) cli::cli_abort("Non-numeric {.field {col}} value{?s}: {.val {unique(x[bad])}}.")
    out
  }
  obs$Year   <- num(obs$Year, "Year")
  obs$Season <- num(obs$Season, "Season")
  obs$Class  <- num(obs$Class, "Class")
  obs$Value  <- num(obs$Value, "Value")
  obs$CV     <- num(obs$CV, "CV")
  obs$Timing <- num(obs$Timing, "Timing")
  if (any(is.na(obs$Year))) cli::cli_abort("{.field Year} is missing in {sum(is.na(obs$Year))} row{?s}.")

  Seasons <- if (!is.null(meta$Seasons)) num(meta$Seasons, "Seasons") else 1
  if (length(Seasons) != 1 || is.na(Seasons) || Seasons < 1 || Seasons %% 1 != 0)
    cli::cli_abort("{.field Seasons} must be a positive integer.")
  if (Seasons > 1) {
    # rows with an empty Season give the decimal time step in Year
    obs$Step <- ifelse(is.na(obs$Season), obs$Year, NA)
    obs$Year <- floor(obs$Year + 1e-8)
  } else if (any(!is.na(obs$Season) & obs$Season != 1)) {
    cli::cli_abort("{.field Season} values other than 1 require a {.field Seasons} {.field Metadata} row > 1.")
  }

  # Historical years
  if (is.null(Years)) {
    if (!is.null(meta$FirstYear) && !is.null(meta$LastYear)) {
      Years <- seq(num(meta$FirstYear, "FirstYear"), num(meta$LastYear, "LastYear"))
    } else if (nrow(obs)) {
      Years <- seq(min(obs$Year), max(obs$Year))
    } else {
      cli::cli_abort("No observations or {.field FirstYear}/{.field LastYear} metadata in {.file {file}}.")
    }
  }
  outside <- setdiff(obs$Year, Years)
  YearRange <- paste0(min(Years), "-", max(Years))
  if (length(outside)) cli::cli_abort("{cli::qty(length(outside))}Year{?s} {.val {outside}} outside the historical years ({YearRange}).")

  YearLH <- max(Years)
  if (Seasons > 1) {
    Steps <- .CSVSteps(CalcYears(max(Years) - min(Years) + 1, 0, max(Years), Seasons))
    by_season <- !is.na(obs$Season)
    obs$Step[by_season] <- Steps$Step[match(paste(obs$Year, obs$Season)[by_season], paste(Steps$Year, Steps$Season))]
    obs$Step[!by_season] <- Steps$Step[match(round(obs$Step[!by_season], 4), round(Steps$Step, 4))]
    bad <- is.na(obs$Step)
    if (any(bad)) {
      lab <- unique(ifelse(by_season, paste0(obs$Year, " season ", obs$Season), "decimal year")[bad])
      cli::cli_abort("{.val {lab}} {?is/are} not {?a/} time step{?s} with {.code Seasons = {Seasons}}.")
    }
    obs$Year <- obs$Step
    Years <- Steps$Step
  }

  is_comp <- obs$Slot %in% .DataCSVSlots$Comp
  if (any(is_comp & is.na(obs$Class))) cli::cli_abort("{.field Class} is missing in {sum(is_comp & is.na(obs$Class))} composition row{?s}.")
  key <- paste(obs$Slot, obs$Name, obs$Year, ifelse(is_comp, obs$Class, ""))
  if (anyDuplicated(key)) {
    cli::cli_abort("Duplicated observation{?s}: {.val {unique(key[duplicated(key)])}}.")
  }

  args <- list(Years = Years, YearLH = YearLH, Seasons = Seasons)
  for (fld in intersect(names(meta), c("Name", "CommonName", "Species", "Agency", "Author", "Email", "Region"))) {
    args[[fld]] <- meta[[fld]]
  }
  for (fld in intersect(names(meta), c("Latitude", "Longitude"))) args[[fld]] <- num(meta[[fld]], fld)

  Fleets <- unique(obs$Name[!obs$Slot %in% .DataCSVSlots$Index])
  for (sl in unique(obs$Slot)) {
    d <- obs[obs$Slot == sl, , drop = FALSE]
    nms <- if (sl %in% .DataCSVSlots$Index) unique(d$Name) else Fleets
    args[[sl]] <- if (sl %in% .DataCSVSlots$Comp) {
      .CSV2CompData(d, sl, Years, nms)
    } else {
      .CSV2SeriesData(d, sl, Years, nms)
    }
  }

  dat <- do.call(Data, args)
  if (!silent) {
    n <- vapply(split(obs$Name, obs$Slot), function(x) length(unique(x)), integer(1))
    cli::cli_alert_success("Read {.file {basename(file)}}: {YearRange}{if (Seasons > 1) paste0(' (', Seasons, ' seasons)') else ''}; {paste0(names(n), ' (', n, ')', collapse = ', ')}.")
  }
  dat
}

# Units: one value per series, or NULL when none are given
.CSVUnits <- function(d, names, default = NA_character_) {
  u <- vapply(names, function(nm) {
    x <- unique(stats::na.omit(d$Units[d$Name == nm]))
    if (length(x) > 1) cli::cli_abort("{.val {nm}} in {.field {d$Slot[1]}} has more than one {.field Units}: {.val {x}}.")
    if (length(x)) x else default
  }, character(1), USE.NAMES = FALSE)
  if (all(is.na(u))) NULL else u
}

# Catch, effort and index slots: Value and CV arrays [Year, Fleet]
.CSV2SeriesData <- function(d, sl, Years, nms = unique(d$Name)) {
  dn <- list(Year = Years, Fleet = nms)
  Value <- CV <- array(NA_real_, c(length(Years), length(nms)), dimnames = dn)
  idx <- cbind(match(d$Year, Years), match(d$Name, nms))
  Value[idx] <- d$Value
  CV[idx] <- d$CV
  if (all(is.na(CV))) CV <- NULL
  Units <- .CSVUnits(d, nms)

  if (sl %in% .DataCSVSlots$Index) {
    Timing <- vapply(nms, function(nm) {
      x <- unique(stats::na.omit(d$Timing[d$Name == nm]))
      if (length(x) > 1) cli::cli_abort("Index {.val {nm}} has more than one {.field Timing}.")
      if (length(x)) x else 0
    }, numeric(1), USE.NAMES = FALSE)
    return(IndicesData(Name = nms, Value = Value, CV = CV, Units = Units, Timing = Timing))
  }
  if (sl == "Effort") return(EffortData(Name = nms, Value = Value, CV = CV, Units = Units))
  CatchData(Name = nms, Value = Value, CV = CV, Units = Units)
}

# Composition slots: Value array [Year, Fleet, Class]
.CSV2CompData <- function(d, sl, Years, nms = unique(d$Name)) {
  cls <- sort(unique(d$Class))
  Value <- array(NA_real_, c(length(Years), length(nms), length(cls)),
                 dimnames = list(Year = Years, Fleet = nms, Class = cls))
  # Year x fleet with any observation: unlisted classes are zero
  obs_yf <- unique(cbind(match(d$Year, Years), match(d$Name, nms)))
  for (k in seq_len(nrow(obs_yf))) Value[obs_yf[k, 1], obs_yf[k, 2], ] <- 0
  Value[cbind(match(d$Year, Years), match(d$Name, nms), match(d$Class, cls))] <- d$Value

  at_age <- grepl("AtAge", sl)
  Units <- .CSVUnits(d, nms)
  if (is.null(Units)) Units <- if (at_age) "years" else NULL
  else if (length(unique(stats::na.omit(Units))) == 1) Units <- unique(stats::na.omit(Units))
  # Age classes as one vector; length classes as a list by fleet, as elsewhere in MSEtool
  Classes <- if (at_age) as.numeric(cls) else stats::setNames(rep(list(as.numeric(cls)), length(nms)), nms)
  CompData(Name = nms, Value = Value, Classes = Classes, Units = Units)
}

#' @rdname ReadDataCSV
#' @param Data A [data-class] object.
#' @param overwrite Logical. Overwrite an existing file?
#' @export
WriteDataCSV <- function(Data, file, overwrite = FALSE, silent = FALSE) {
  if (!inherits(Data, "data")) cli::cli_abort("{.arg Data} must be a {.cls data} object.")
  if (file.exists(file) && !overwrite) cli::cli_abort("{.file {file}} exists. Use {.code overwrite = TRUE}.")
  Seasons <- Seasons(Data)
  if (!length(Seasons)) Seasons <- 1
  Years <- Years(Data)
  Steps <- if (length(Years)) .CSVSteps(Years) else NULL
  # calendar year and season of time steps yrs
  steps <- function(yrs) {
    if (Seasons == 1) return(list(Year = yrs, Season = NA))
    k <- match(round(yrs, 4), round(Steps$Step, 4))
    if (anyNA(k)) cli::cli_abort("Time step{?s} {.val {yrs[is.na(k)]}} {?is/are} not in {.code Years(Data)}.")
    list(Year = Steps$Year[k], Season = Steps$Season[k])
  }
  rows <- list()
  blank <- function(n) rep(NA, n)

  # Metadata
  meta <- list()
  for (fld in c("Name", "CommonName", "Species", "Agency", "Author", "Email", "Region", "Latitude", "Longitude")) {
    v <- .DataCSVGet(Data, fld)
    if (length(v)) meta[[fld]] <- as.character(v[1])
  }
  if (length(Years)) {
    meta$FirstYear <- as.character(min(Steps$Year))
    meta$LastYear <- as.character(max(Steps$Year))
  }
  if (Seasons > 1) meta$Seasons <- as.character(Seasons)
  if (length(meta)) {
    rows$meta <- data.frame(Slot = "Metadata", Name = names(meta), Year = NA, Season = NA, Class = NA,
                            Value = unlist(meta), CV = NA, Units = NA, Timing = NA, stringsAsFactors = FALSE)
  }

  for (sl in c(.DataCSVSlots$Catch, .DataCSVSlots$Effort, .DataCSVSlots$Index)) {
    obj <- .DataCSVGet(Data, sl)
    if (!length(Value(obj))) next
    Value <- as.matrix(Value(obj))
    yrs <- if (length(rownames(Value))) as.numeric(rownames(Value)) else Years
    nms <- if (length(Name(obj))) Name(obj) else paste0(sl, seq_len(ncol(Value)))
    CV <- if (length(CV(obj))) as.matrix(CV(obj)) else NULL
    for (j in seq_len(ncol(Value))) {
      keep <- !is.na(Value[, j])
      if (!any(keep)) next
      units <- if (length(Units(obj))) rep(Units(obj), length.out = ncol(Value))[j] else NA
      timing <- if (!sl %in% .DataCSVSlots$Index) NA else if (length(Timing(obj))) rep(Timing(obj), length.out = ncol(Value))[j] else 0
      ys <- steps(yrs[keep])
      rows[[paste(sl, j)]] <- data.frame(
        Slot = sl, Name = nms[j], Year = ys$Year, Season = ys$Season, Class = NA, Value = Value[keep, j],
        CV = if (is.null(CV)) NA else CV[keep, j], Units = units, Timing = timing, stringsAsFactors = FALSE
      )
    }
  }

  for (sl in .DataCSVSlots$Comp) {
    obj <- .DataCSVGet(Data, sl)
    if (!length(Value(obj))) next
    Value <- Value(obj)
    if (length(dim(Value)) != 3) cli::cli_abort("{.code Data@{sl}@Value} must be an array [Year, Fleet, Class].")
    yrs <- if (length(dimnames(Value)[[1]])) as.numeric(dimnames(Value)[[1]]) else Years
    nms <- if (length(Name(obj))) Name(obj) else paste0("Fleet", seq_len(dim(Value)[2]))
    for (j in seq_len(dim(Value)[2])) {
      cls <- Classes(obj)
      if (is.list(cls)) cls <- if (!is.null(names(cls)) && nms[j] %in% names(cls)) cls[[nms[j]]] else cls[[j]]
      if (!length(cls)) cls <- as.numeric(dimnames(Value)[[3]])
      units <- if (length(Units(obj))) rep(Units(obj), length.out = dim(Value)[2])[j] else NA
      for (i in seq_len(dim(Value)[1])) {
        v <- Value[i, j, ]
        if (all(is.na(v))) next
        # Zeros are written so that the full set of classes is kept
        keep <- !is.na(v)
        ys <- steps(yrs[i])
        rows[[paste(sl, j, i)]] <- data.frame(
          Slot = sl, Name = nms[j], Year = ys$Year, Season = ys$Season, Class = cls[keep], Value = v[keep],
          CV = NA, Units = units, Timing = NA, stringsAsFactors = FALSE
        )
      }
    }
  }

  out <- do.call(rbind, unname(rows))
  rownames(out) <- NULL
  if (Seasons == 1) out$Season <- NULL
  utils::write.csv(out, file, row.names = FALSE, na = "")
  if (!silent) cli::cli_alert_success("Wrote {nrow(out)} row{?s} to {.file {file}}.")
  invisible(out)
}
