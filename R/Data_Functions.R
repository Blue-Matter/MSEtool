# Functions that operate on Data Object


#' Initialize Data Input Files
#'
#' Creates template for the Data input file (Excel or CSV) and Data documentation file (Markdown)
#' in the working directory or the directory specified by the `dir` argument
#'
#' @param name Name of the data input files. Default is 'Data'. Use 'Example'
#' to create populated example Data Input and Data Documentation files.
#' @param ext Optional file extension for input file. 'xlsx' (default) or 'csv'
#' @param overwrite Logical. Overwrite existing files?
#' @param dir Optional directory path to create the Data files. Default is `getwd()``
#'
#' @return Nothing. Creates template data files in the working directory.
#' @export
#'
#' @author A. Hordyk
#' @examples
#' \dontrun{
#' DataInit("Example") # populated example
#' DataInit("myData") # empty template
#' }
DataInit <- function(name="Data", ext=c("xlsx", "csv"), overwrite=FALSE, dir=NULL) {
  ext <- match.arg(ext)
  if (is.null(dir)) dir <- getwd()
  mdname <- paste(name, "md", sep=".")
  if (name == "Example") {
    name <- paste(name, ext, sep=".")
    md.path <- system.file("Data_Example.md", package = "MSEtool")
    md.pathout <- gsub("Data_Example.md", mdname, md.path)
    if (ext == "xlsx") {
      par.path <- system.file("Data_Example.xlsx", package = "MSEtool")
      pathout <- gsub("Data_Example.xlsx", name, par.path)
      pathout <- gsub(dirname(pathout), dir, pathout)
    } else {
      par.path <- system.file("Data_Example.csv", package = "MSEtool")
      pathout <- gsub("Data_Example.csv", name, par.path)
      pathout <- gsub(dirname(pathout), dir, pathout)
    }

  } else{
    name <- paste(name, ext, sep=".")
    md.path <- system.file("Rmd/Data/Data.md", package = "MSEtool")
    md.pathout <- gsub("Data.md", mdname, md.path)
    if (ext == "xlsx") {
      par.path <- system.file("Data.xlsx", package = "MSEtool")
      pathout <- gsub("Data.xlsx", name, par.path)
      pathout <- gsub(dirname(pathout), dir, pathout)
    } else {
      par.path <- system.file("Data.csv", package = "MSEtool")
      pathout <- gsub("Data.csv", name, par.path)
      pathout <- gsub(dirname(pathout), dir, pathout)
    }
  }


  # Copy xlsx file over to working directory

  message("Creating ", name, " and ", mdname, " in ", dir)

  # Check if file exists
  exist <- file.exists(pathout)
  if (exist & !overwrite) stop(name, " already exists in ", dir, ". Use 'overwrite=TRUE' to overwrite", call.=FALSE)
  copy <- file.copy(par.path, pathout, overwrite = overwrite)
  if (!copy) stop("Excel file not copied from ", par.path)

  # Check if file exists
  md.pathout <- gsub(dirname(md.pathout), dir, md.pathout)
  exist <- file.exists(md.pathout)
  if (exist & !overwrite) stop(mdname, " already exists in ", dir, ". Use 'overwrite=TRUE' to overwrite", call.=FALSE)
  copy <- file.copy(md.path, md.pathout, overwrite = overwrite)
  if (!copy) stop("Markdown file not copied from ", md.path)

}



#' Import a Data object from Excel file
#'
#' @param name Name of the data file, with or without file extension.
#' Include full file path if not in working directory
#' @param dec the character used in the file for decimal points.
#' @param sheet Sheet number if importing Data from XL file
#' @param silent Logical. Hide messages?
#' @return An object of class 'Data'
#' @export
#' @author A. Hordyk
#' @examples
#' \dontrun{
#' MyData <- XL2Data("MyData.xlsx")
#' }
XL2Data <- function(name, dec=c(".", ","), sheet=1, silent=FALSE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package \"readxl\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  dec <- match.arg(dec)
  dir <- dirname(name)
  if (dir ==".") {
    dir <- NULL
  } else {
    name <- basename(name)
  }
  if (is.null(dir)) dir <- getwd()
  if (nchar(tools::file_ext(name)) == 0) {
    xl.fname1 <- paste0(name, ".xlsx")
    xl.fname2 <- paste0(name, ".csv")
    fls <- file.exists(c(file.path(dir, xl.fname1), file.path(dir,xl.fname2)))
    if (sum(fls) == 0) stop(xl.fname1, " or ", xl.fname2, " not found in ", dir)
    if (sum(fls) > 1) stop(name, " found with multiple extensions. Specify file extension.", call.=FALSE)
    name <- c(xl.fname1, xl.fname2)[fls]
  }
  if (tools::file_ext(name) == "csv") {
    tempin <- strsplit(readLines(file.path(dir,name)), ",")
    # detect header
    header <- ifelse(tempin[[2]][1] == 'Name', TRUE, FALSE)
    somechar <- function(x) sum(nchar(x) > 0)
    Ncol <- max(unlist(lapply(tempin, somechar)))
    col.names <- paste0("V", 1:Ncol)
    if (dec == ".") {
      datasheet <- read.csv(file.path(dir,name), header = header,
                            colClasses = "character", col.names=col.names,
                            stringsAsFactors = FALSE, row.names = NULL)
    } else {
      datasheet <- read.csv2(file.path(dir,name), header = header,
                             colClasses = "character", col.names=col.names,
                             stringsAsFactors = FALSE, row.names = NULL)
    }

  } else if(tools::file_ext(name) %in% c("xls", "xlsx")) {
    datasheet <- readxl::read_excel(file.path(dir,name), sheet = sheet,
                                    col_names = TRUE, .name_repair = "minimal")

    if (datasheet[1,1] == "Common Name") { # no header in data file
      datasheet <- readxl::read_excel(file.path(dir,name), sheet = sheet,
                                      col_names = FALSE, .name_repair = "minimal")
    }
  } else {
    stop("File extension must be .csv, .xls, or .xlsx")
  }

  if (ncol(datasheet) == 1)  stop("Data file has no data") # no data in data file
  if (all(is.na(datasheet[,2]))) stop("Data file has no data in column B") # no data in data file

  # Add column names
  colnames(datasheet) <- c("Name", "Data")

  # check names in Column 1
  input <- file.path(system.file(package = 'MSEtool'), "Data.csv")
  valnames <- read.csv(input, header=FALSE, stringsAsFactors = FALSE)
  valnames <- valnames[,1]
  valnames <- c(valnames, "LHYear", "MPrec", 'MPeff')
  InNames <- datasheet$Name
  inval <- InNames[!InNames %in% valnames]
  inval <- inval[!grepl("CAL", inval)]
  inval <- inval[!grepl("CAA", inval)]
  inval <- inval[!grepl("Index", inval)]
  inval <- inval[!grepl("CV Index", inval)]
  inval <- inval[!grepl("Index", inval)]
  inval <- inval[!grepl("Modal length", inval)]
  inval <- inval[!grepl("Mean length Lc", inval)]
  inval <- inval[!grepl("Current spawning stock abundance", inval)]
  inval <- inval[!grepl("Cref", inval)]
  inval <- inval[!grepl("Iref", inval)]
  inval <- inval[!grepl("Bref", inval)]
  inval <- inval[!grepl("CV Cref", inval)]
  inval <- inval[!grepl("CV Iref", inval)]
  inval <- inval[!grepl("CV Bref", inval)]
  inval <- inval[!grepl("CV Rec", inval)]
  inval <- inval[!grepl("Recruitment", inval)]
  inval <- inval[!grepl("LenCV", inval)]

  inval <- inval[!is.na(inval)]
  if (length(inval)>0)
    warning("These rows in the Data file are not valid names and were not imported:\n", paste(inval, collapse=', '))

  datasheet$Name[datasheet$Name == "MPrec"] <- 'Previous TAC'
  datasheet$Name[datasheet$Name == "MPeff"] <- 'Previous TAE'

  datasheet$Name[datasheet$Name == "Modal length"] <- 'Modal length (Lc)'
  datasheet$Name[datasheet$Name == "Mean length Lc"] <- 'Mean length above Lc'
  datasheet$Name[datasheet$Name == "Current spawning stock abundance"] <- 'Current spawning abundance'
  datasheet$Name[datasheet$Name == "Cref"] <- 'Catch Reference'
  datasheet$Name[datasheet$Name == "Iref"] <- 'Index Reference'
  datasheet$Name[datasheet$Name == "Bref"] <- 'Biomass Reference'
  datasheet$Name[datasheet$Name == "CV Cref"] <- 'CV Catch Reference'
  datasheet$Name[datasheet$Name == "CV Iref"] <- 'CV Index Reference'
  datasheet$Name[datasheet$Name == "CV Bref"] <- 'CV Biomass Reference'
  datasheet$Name[datasheet$Name == "Recruitment"] <- 'Recruitment Index'
  datasheet$Name[datasheet$Name == "LenCV"] <- 'CV of length-at-age'

  Data <- new("Data")

  # ---- Main ----
  import_convert <- function(Name, datasheet, numeric=TRUE) {
    temp <- datasheet$Data[which(datasheet$Name==Name)]
    if (length(temp)<1) {
      if(numeric) return(as.numeric(NA))
      if(!numeric) return(as.character(NA))
    }
    if (is.na(temp)&& numeric) return(as.numeric(NA))
    if (is.na(temp)) return(temp)
    if (temp == 'NA' && numeric) return(as.numeric(NA))
    if (is.na(temp)) return(temp)
    if (numeric) temp <- as.numeric(temp)
    temp
  }

  DataSlots <- MSEtool::DataSlots

  # loop over DataSlots and populate slots in Data object with imported values
  for(i in 1:nrow(DataSlots)) {
    if (!is.na(DataSlots$Slot[i]) & !DataSlots$Timeseries[i]) {
      isNumeric <- DataSlots$Numeric[i]
      Name <- DataSlots$Name[i]
      Slot <- DataSlots$Slot[i]
      slot(Data, Slot) <- import_convert(Name, datasheet, isNumeric)
    }
  }

  # ---- Time-Series ----
  import_convert_ts <- function(Name, datasheet, Data, matrix=TRUE,
                                checkLength=TRUE) {
    row <- which(datasheet$Name==Name)
    if (Name =="Year") {
      columns <- 2:ncol(datasheet)
      temp <- datasheet[row, columns]
      temp <- suppressWarnings(as.numeric(temp))
      temp <- temp[!is.na(temp)]
    } else {
      columns <- 2:(length(Data@Year)+1)
      temp <- datasheet[row, columns]
    }
    DataSlots <- MSEtool::DataSlots
    ind <- which(DataSlots$Name == Name)
    Slot <- DataSlots$Slot[ind]
    isNumeric <- DataSlots$Numeric[ind]
    if (isNumeric) temp <- suppressWarnings(as.numeric(temp))

    if (grepl('CV', Name)) {
      if (!is.na(temp[1]) & all(is.na(temp[2:length(temp)]))) {
        message_info('Only one value found for ', paste0(Name, '.'), 'Assuming this value for all years')
        temp <- rep(temp[1], length(Data@Year))
      }
    }

    if (matrix) temp <- matrix(temp, nrow=1)
    if (checkLength) {
      if (length(Data@Year)<=0)
        stop(Name, ' time-series data detected, but no data found for Year', call.=FALSE)

      if (length(temp) != length(Data@Year))
        stop(Name, ' time-series data must be the same length as Year (use NA for years with missing data)', call.=FALSE)
    }

    slot(Data, Slot) <- temp
    Data
  }

  # Year index
  Data <- import_convert_ts('Year', datasheet, Data, FALSE, FALSE)

  # extra check for LHYear (used multiple names in CSV in the past)
  tryLHyear <- datasheet$Data[which(datasheet$Name=="LHYear")]
  if (length(tryLHyear)<1) {
    tryLHyear <- datasheet$Data[which(datasheet$Name=="Last Historical Year")]
  }
  tryLHyear <- as.numeric(tryLHyear)
  Data@LHYear <- tryLHyear
  if (length(tryLHyear)<1 | is.na(tryLHyear)) {
    if (is.finite(max(Data@Year, na.rm=TRUE))) {
      message_info(paste0('Last Historical Year (`LHYear`) missing, assuming it is maximum value in `Year` (', max(Data@Year, na.rm=TRUE), ')'))
      Data@LHYear <- max(Data@Year, na.rm=TRUE)
    } else {
      stop("Last Historical Year must be specified (single numeric value) if no values in `Year`")
    }
  }
    
  if (!Data@LHYear %in% Data@Year)
    stop("`Year` must include Last Historical Year", call. = FALSE)
  if (!all(seq(Data@Year[1], Data@Year[length(Data@Year)], 1) == Data@Year))
    stop("`Year` must be sequential and include all years")
  Nyears <- length(Data@Year)

  # Catch time-series
  Data <- import_convert_ts('Catch', datasheet, Data)
  Data <- import_convert_ts('CV Catch', datasheet, Data)

  # Effort time-series
  Data <- import_convert_ts('Effort', datasheet, Data)
  Data <- import_convert_ts('CV Effort', datasheet, Data)

  # Total abundance index - fishery dependant
  Data <- import_convert_ts('Abundance index', datasheet, Data)
  Data <- import_convert_ts('CV Abundance index', datasheet, Data)

  # Spawning abundance index - fishery dependant
  Data <- import_convert_ts('Spawning Abundance index', datasheet, Data)
  Data <- import_convert_ts('CV Spawning Abundance index', datasheet, Data)

  # Vulnerable abundance index - fishery dependant
  Data <- import_convert_ts('Vulnerable Abundance index', datasheet, Data)
  Data <- import_convert_ts('CV Vulnerable Abundance index', datasheet, Data)

  # Additional indices
  ind <- grepl("Index", datasheet$Name)
  index_text <- datasheet$Name[ind]
  index_text <- index_text[! index_text %in% c("Index Reference", "CV Index Reference")]
  n_indices <- sum(sub(" .*$", "", index_text) == "Index")
  n_cv <- sum(sub(" .*$", "", index_text) == "CV")
  n_vuln <- sum(sub(" .*$", "", index_text) == "Vuln")
  if (n_cv != n_indices) stop("CV missing for some or all additional indices", call. = FALSE)
  if (n_vuln != n_indices)
    stop("Vulnerability-at-age schedule missing for some or all additional indices", call. = FALSE)

  addInd <- datasheet[which(datasheet$Name == "Index 1"),2:(Nyears+1)] %>% unlist()
  n.temp <- nchar(addInd)

  if (!(all(n.temp[!is.na(n.temp)]==0)) & !all(is.na(addInd)) & !(length(addInd)<1)) {
    indexexist <- TRUE
  } else {
    indexexist <- FALSE
  }
  if (indexexist) {
    if (is.na(Data@MaxAge))
      stop("Require `Maximum age` to use Additional Indices", call. = FALSE)
    Data@AddInd <- Data@CV_AddInd <- array(NA, dim=c(1, n_indices, Nyears))
    Data@AddIndV <- array(NA, dim=c(1, n_indices, Data@MaxAge+1))
  } else {
    Data@AddInd <- Data@CV_AddInd <- array(NA, dim=c(1,1,1))
    Data@AddIndV <- array(NA, dim=c(1,1,1))
  }

  # Loop over additional indices
  if (indexexist) {
    for (x in 1:n_indices) {
      ind <- which(datasheet$Name == paste("Index", x))
      if (length(ind)>1)
        stop('Additional indices must be uniquely numbered.',
             paste(" Index", x), ' appears ', length(ind), ' times', call.=FALSE)
      Data@AddInd[1,x,] <- suppressWarnings(datasheet[ind, 2:(Nyears+1)] %>% as.numeric())
      ind <- which(datasheet$Name == paste("CV Index", x))
      Data@CV_AddInd[1,x,] <- suppressWarnings(datasheet[ind, 2:(Nyears+1)] %>% as.numeric())
      ind <- which(datasheet$Name == paste("Vuln Index", x))
      Data@AddIndV[1,x,] <- suppressWarnings(datasheet[ind, 2:(Data@MaxAge+2)] %>% as.numeric())
      if (!all(is.na(Data@AddIndV[1,x,])) & any(is.na(Data@AddIndV[1,x,])))
        warning("Vuln Index ", x, " must be length `Maximum age`+1 and contain only numeric values (no NA)")
    }

    ind <- which(datasheet$Name == 'AddIndType')
    Data@AddIndType <- suppressWarnings(as.numeric(datasheet[ind,2:(n_indices+1)]))

    ind <- which(datasheet$Name == 'AddIunits')
    Data@AddIunits <- suppressWarnings(as.numeric(datasheet[ind,2:(n_indices+1)]))
  }

  # Recruitment index
  Data <- import_convert_ts('Recruitment index', datasheet, Data)
  Data <- import_convert_ts('CV Recruitment index', datasheet, Data)

  # Mean length
  Data <- import_convert_ts('Mean length', datasheet, Data)
  Data <- import_convert_ts('Modal length (Lc)', datasheet, Data)
  Data <- import_convert_ts('Mean length above Lc', datasheet, Data)

  # ---- Catch-at-Age ----
  ind <- which(datasheet$Name == "Vuln CAA")
  if (length(ind)>0 && !is.na(datasheet[ind, 2])) {
    VulnCAA <- datasheet[ind, 2:(Data@MaxAge+2)]
    VulnCAA <- suppressWarnings(as.numeric(VulnCAA))
    if (!all(is.na(VulnCAA))) {
      if (any(is.na(VulnCAA)))
        stop('Missing values in Vuln CAA. Must be length `Maximum age`+1 will values for each age (or all NA to ignore)')
    }
    Data@Vuln_CAA <- matrix(VulnCAA, nrow=1)
  }

  ind <- which(grepl('CAA', datasheet$Name))
  ind2 <-  which(datasheet$Name == "Vuln CAA")
  if (length(ind2)>0)
    ind <- ind[!ind ==ind2]
  CAAexists <- grepl('CAA', datasheet$Name[ind])
  if (length(CAAexists) < 1) CAAexists <- FALSE
  if (length(ind) <=1 || !all(CAAexists)) {
    CAA_Yrs <- numeric(0)
  } else {
    CAA_Yrs <- sapply(strsplit(datasheet$Name[ind], " "), function(x) unlist(strsplit(x[2], " ")))
    if (any(is.na(CAA_Yrs)))
      stop("Missing years for CAA data. Must be of the form `CAA YEAR` e.g., `CAA 2019`", call. = FALSE)

    if(!all(CAA_Yrs %in% Data@Year)) stop("All CAA Years must be included in `Year`")
  }

  if (length(CAA_Yrs)>0) {
    Data@CAA <- array(NA, dim=c(1, Nyears, Data@MaxAge+1))

    CAAdat <- matrix(NA, nrow=length(ind), ncol=Data@MaxAge+1)
    for (i in seq_along(ind)) {
      vals <- datasheet[ind[i], 2:(Data@MaxAge+2)]
      vals <- suppressWarnings(as.numeric(vals))
      if (!all(is.na(vals)) && any(is.na(vals))) {
        warning('NA values found in CAA data Year ', paste0(CAA_Yrs[i], '.'),
                " Is each row length `Maximum age`+1?")
      }
      CAAdat[i,] <- vals
    }
    yrind <- match(CAA_Yrs, Data@Year)
    Data@CAA[1, yrind,] <- CAAdat
  } else{
    Data@CAA <- array(NA, dim=c(1,1,1))
  }

  # ---- Catch-at-Length ----
  CAL_bins <- suppressWarnings(datasheet[which(datasheet$Name == "CAL_bins"),] %>% as.numeric())
  CAL_bins <- CAL_bins[!is.na(CAL_bins)]

  CAL_mids <- suppressWarnings(datasheet[which(datasheet$Name == "CAL_mids"),] %>% as.numeric())
  CAL_mids <- CAL_mids[!is.na(CAL_mids)]

  ind <- which(datasheet$Name == "Vuln CAL")
  if (length(ind)>0 && !is.na(datasheet[ind, 2])) {
    VulnCAL <- datasheet[ind, 2:(length(CAL_mids)+2)]
    VulnCAL <- suppressWarnings(as.numeric(VulnCAL))
    if (!all(is.na(VulnCAL))) {
      if (any(is.na(VulnCAL)))
        stop('Missing values in Vuln CAL. Must be length `Maximum age`+1 will values for each age (or all NA to ignore)')
    }
    Data@Vuln_CAL <- matrix(VulnCAL, nrow=1)
  }

  ind <- which(grepl('CAL', datasheet$Name) & !grepl('CAL_bins', datasheet$Name) &
                 !grepl('Vuln CAL', datasheet$Name) &
                 !grepl('CAL_mids', datasheet$Name))

  CALexists <- grepl("CAL", datasheet$Name[ind])
  if (length(CALexists) < 1) CALexists <- FALSE
  if (length(ind) <=1 || !any(CALexists)) {
    ind <- numeric(0)
    CAL_Yrs <- numeric(0)
  }
  if (length(ind)>0) {
    # CAL data exists
    if (length(CAL_bins)<1 & length(CAL_mids) < 1)
      stop("Require either CAL_mids or CAL_bins", call. = FALSE)
    if (length(CAL_bins)>1  & length(CAL_mids) > 1) {
      if (!length(CAL_mids) == length(CAL_bins)-1)
        stop("CAL_mids should be of length `length(CAL_bins)-1`", call. = FALSE)
      by <- CAL_mids[2] - CAL_mids[1]
      if (all(CAL_mids != seq(CAL_bins[1]+0.5*by, by=by, length.out = length(CAL_bins)-1)))
        stop("CAL_mids should be mid-points of CAL_bins", call. = FALSE)
    }
    if (length(CAL_bins)>1) {
      if (!all(diff(CAL_bins[2:length(CAL_bins)]) == diff(CAL_bins)[2]))
        stop("Inconsistent bin width in CAL_bins", call. = FALSE)
    }
    if (length(CAL_mids)>1) {
      if(!all(diff(CAL_mids) == mean(diff(CAL_mids))))
        stop("Inconsistent bin width in CAL_mids", call. = FALSE)
    }
    if (length(CAL_bins)>1 & length(CAL_mids)<=1) {
      by <- CAL_bins[2] - CAL_bins[1]
      CAL_mids <- seq(CAL_bins[1]+0.5*by, by=by, length.out = length(CAL_bins)-1)
    }
    if (length(CAL_mids)>1 & length(CAL_bins)<=1) {
      by <- CAL_mids[2] - CAL_mids[1]
      CAL_bins <- seq(CAL_mids[1]-0.5*by, by=by, length.out = length(CAL_mids)+1)
    }

    CAL_Yrs <- sapply(strsplit(datasheet$Name[ind], " "), function(x) unlist(strsplit(x[2], " ")))

  }

  ind2 <- which(datasheet$Name == "Vuln CAL")
  if (length(ind2)>01)
    ind <- ind[!ind == ind2]

  CAL_Yrs <- sapply(strsplit(datasheet$Name[ind], " "), function(x) unlist(strsplit(x[2], " ")))

  if(!all(CAL_Yrs %in% Data@Year)) {
    CAL_Yrs <- sapply(strsplit(datasheet$Name[ind], "_"), function(x) unlist(strsplit(x[2], " ")))
  }
  if(!all(CAL_Yrs %in% Data@Year)) {
    stop("All CAL Years must be included in `Year`. Entries must be formatted 'CAL YEAR' or 'CAL_YEAR'")
  }

  NMids <- length(CAL_mids)
  Data@CAL <- array(NA, dim=c(1, Nyears, NMids))

  CALdat <- matrix(NA, nrow=length(ind), ncol=NMids)
  for (i in seq_along(ind)) {
    vals <- datasheet[ind[i], 2:(NMids+1)]
    vals <- suppressWarnings(as.numeric(vals))
    if (!all(is.na(vals)) && any(is.na(vals))) {
      warning('NA values found in CAL data Year ', paste0(CAL_Yrs[i], '.'),
              " Is each row same length as `CAL_mids`? (or `length(CAL_bins)-1`)")
    }
    CALdat[i,] <- vals
  }

  yrind <- match(CAL_Yrs, Data@Year)
  Data@CAL_bins <- CAL_bins
  Data@CAL_mids <- CAL_mids
  Data@CAL[1, yrind,] <- CALdat


  # Default values
  if (all(is.na(Data@CV_Cat))) Data@CV_Cat <- matrix(0.2, nrow=1, ncol=Nyears)
  if (all(is.na(Data@CV_Ind))) Data@CV_Ind <- matrix(0.2, nrow=1, ncol=Nyears)
  # if (all(is.na(Data@CV_SpInd))) Data@CV_SpInd <- matrix(0.2, nrow=1, ncol=Nyears)
  if (all(is.na(Data@CV_Effort))) Data@CV_Effort <- matrix(0.2, nrow=1, ncol=Nyears)
  if (all(is.na(Data@CV_Rec))) Data@CV_Rec <- matrix(0.2, nrow=1, ncol=Nyears)

  if (NAor0(Data@LenCV)) Data@LenCV <- 0.1
  if (NAor0(Data@CV_Dt)) Data@CV_Dt <- 0.25
  if (NAor0(Data@CV_AvC)) Data@CV_AvC <- 0.2
  if (NAor0(Data@CV_Mort)) Data@CV_Mort <- 0.2
  if (NAor0(Data@CV_FMSY_M)) Data@CV_FMSY_M <- 0.2
  if (NAor0(Data@CV_BMSY_B0)) Data@CV_BMSY_B0 <- 0.045
  if (NAor0(Data@CV_Cref)) Data@CV_Cref <- 0.2
  if (NAor0(Data@CV_Bref)) Data@CV_Bref <- 0.2
  if (NAor0(Data@CV_Iref)) Data@CV_Iref <- 0.2
  if (NAor0(Data@CV_Dep)) Data@CV_Dep <- 0.25
  if (NAor0(Data@CV_Abun)) Data@CV_Abun <- 0.25
  if (NAor0(Data@CV_vbK)) Data@CV_vbK <- 0.1
  if (NAor0(Data@CV_vbLinf)) Data@CV_vbLinf <- 0.1
  if (NAor0(Data@CV_vbt0)) Data@CV_vbt0 <- 0.1
  if (NAor0(Data@CV_L50))  Data@CV_L50 <- 0.1
  if (NAor0(Data@CV_LFC))  Data@CV_LFC <- 0.2
  if (NAor0(Data@CV_LFS))  Data@CV_LFS <- 0.2
  if (NAor0(Data@CV_wla))  Data@CV_wla <- 0.1
  if (NAor0(Data@CV_wlb))  Data@CV_wlb <- 0.1
  if (NAor0(Data@CV_steep)) Data@CV_steep <- 0.2
  if (NAor0(Data@nareas)) Data@nareas <- 2

  if (length(Data@CAA) == 0) Data@CAA <- array(NA, c(1, 1, 1))
  if (length(Data@CAL) == 0) Data@CAL <- array(NA, c(1, 1, 1))
  if (length(Data@CAL_bins) == 0) Data@CAL_bins <- 1
  if (length(Data@TAC) == 0) Data@TAC <- array(1, c(1, 1))
  # if (length(Data@TACbias) == 0) Data@TACbias <- array(1, c(1, 1))
  if (length(Data@Sense) == 0) Data@Sense <- array(1, c(1, 1))
  if (length(Data@ML) == 0)  Data@ML <- array(NA, c(1, 1))
  if (length(Data@Lbar) == 0) Data@Lbar <- array(NA, c(1, 1))
  if (length(Data@Lc) == 0) Data@Lc <- array(NA, c(1, 1))

  if (is.na(Data@MPeff) || length(Data@MPeff)==0) Data@MPeff <- 1

  Data@Log[[1]] <- paste("Created:", Sys.time())
  Data@params <- new("list")
  Data@OM <- data.frame(NA)
  Data@Obs <- data.frame(NA)
  Data@PosMPs <- NA
  Data@MPs <- NA

  Data
}

# XL2Data <- function(name="Data") {
#   if (class(name) != 'character') stop("file name must be provided", call.=FALSE)
#
#   dir <- dirname(name)
#   if (dir ==".") {
#     dir <- NULL
#   } else {
#     name <- basename(name)
#   }
#
#   if (is.null(dir)) dir <- getwd()
#   if (nchar(tools::file_ext(name)) == 0) {
#     xl.fname1 <- paste0(name, ".xlsx")
#     xl.fname2 <- paste0(name, ".csv")
#     fls <- file.exists(c(file.path(dir, xl.fname1), file.path(dir,xl.fname2)))
#     if (sum(fls) == 0) stop(xl.fname1, " or ", xl.fname2, " not found in ", dir)
#     if (sum(fls) > 1) stop(name, " found with multiple extensions. Specify file extension.", call.=FALSE)
#     name <- c(xl.fname1, xl.fname2)[fls]
#   }
#
#   if (!file.exists(file.path(dir, name))) stop(file.path(dir, name), " not found", call.=FALSE)
#
#   isCSV <- grepl('.csv', name)
#   message("Reading ", name)
#   if (isCSV) {
#     Data <- new("Data", file.path(dir,name))
#   } else {
#     sheetnames <- readxl::excel_sheets(file.path(dir,name))  # names of the sheets
#
#     # DataXLSlot <- MSEtool:::DataXLSlot
#     NewSheetNames <- names(DataXLSlot)
#     if (all(NewSheetNames %in% sheetnames)) {
#       Data <- importnewXLData(dir,name, NewSheetNames)
#     } else {
#       datasheet <- suppressMessages(as.data.frame(readxl::read_excel(file.path(dir,name), sheet = 1, col_names = FALSE)))
#       if (datasheet[1,1]== "Slot")
#         datasheet <- suppressMessages(as.data.frame(readxl::read_excel(file.path(dir,name), sheet = 1, col_names = FALSE, skip=1)))
#
#       if (all(dim(datasheet) == 0)) stop("Nothing found in first sheet", call.=FALSE)
#       tmpfile <- tempfile(fileext=".csv")
#       writeCSV2(inobj = datasheet, tmpfile, objtype = "Data")
#
#       if (ncol(datasheet)<2) {
#         unlink(tmpfile)
#         stop("No parameter values found in first worksheet ", call.=FALSE)
#       } else {
#         Data <- new("Data", tmpfile)
#         unlink(tmpfile)
#       }
#     }
#     return(Data)
#   }
# }





# importnewXLData_old <- function(dir,name, NewSheetNames) {
#   Data <- new("Data", silent=TRUE)
#   BlankDat <-new("Data", silent=TRUE)
#   ignoreSheet <- NULL
#
#   # sh <- NewSheetNames[1]
#   for (sh in NewSheetNames) { # import from individual worksheets
#     datasheet <- suppressMessages(as.data.frame(readxl::read_excel(file.path(dir,name),
#                                                   sheet = sh, col_names = FALSE)))
#     if (dim(datasheet)[2] <= 1) {
#       message('No data found in sheet: ', sh)
#     } else {
#       message("Importing from: ", sh)
#       dname <- datasheet[, 1]
#       dat  <- datasheet[, 2:ncol(datasheet), drop=FALSE]
#       df <- data.frame(XLRow=DataXLSlot[[sh]]$XLRow,
#                        Slot=DataXLSlot[[sh]]$Slot,
#                        Class=DataXLSlot[[sh]]$Class,
#                        Ignore=DataXLSlot[[sh]]$Ignore,
#                        stringsAsFactors = FALSE)
#       df$Ignore[is.na(df$Slot)] <- TRUE
#       df$Ignore[df$XLRow=="Fleet Type"] <- TRUE # temporary until new build
#       df$Ignore <- as.logical(df$Ignore)
#       df <- df[!df$Ignore,]
#
#       # if (sh %in% c("Main", "Biology", "Reference")) {
#       if (sh %in% c("Main", "Biology", "Selectivity", "Reference")) {
#         for (sl in 1:nrow(df)) {
#           temp <- dat[match(df$XLRow[sl], dname),1]
#           if (substr(df$Class[sl],start=1, stop=1) == "c")
#             slot(Data, df$Slot[sl]) <- temp
#           if (substr(df$Class[sl],start=1, stop=1) == "n")
#             slot(Data, df$Slot[sl]) <- as.numeric(temp)
#         }
#       } else if (sh == "Time-Series") {
#         YearInd <- match("Year", dname)
#         Year <- dat[YearInd,]
#         Year <- Year[!is.na(Year)]
#         Data@Year <- as.numeric(Year)
#         if (!is.finite(Data@LHYear))
#           stop("'Current Year' in 'Main' sheet is missing", call.=FALSE)
#         if (max(Data@Year) != Data@LHYear)
#           stop("Last year should be equal to 'Current Year' in 'Main' sheet", call.=FALSE)
#         ncol_ts <- length(Year)
#         ncol_cv <- 1
#         for (sl in 1:nrow(df)) {
#           ncol <- ifelse(grepl("CV_", df$Slot[sl]), ncol_cv, ncol_ts)
#           temp <- as.numeric(dat[match(df$XLRow[sl], dname),1:ncol])
#
#           if (substr(df$Class[sl],start=1, stop=1) == "n")
#             slot(Data, df$Slot[sl]) <- temp
#           if (substr(df$Class[sl],start=1, stop=1) == "m")
#             slot(Data, df$Slot[sl]) <- matrix(temp, nrow=1)
#           if(all(is.na(slot(Data, df$Slot[sl]))))
#             slot(Data, df$Slot[sl]) <- slot(BlankDat, df$Slot[sl])
#         }
#
#       } else if (sh == "CAA") {
#         if (!is.finite(Data@MaxAge))
#           stop("'Maximum age' in 'Biology' sheet is missing", call.=FALSE)
#         if (any(!is.finite(Data@Year)))
#           stop("'Year' in 'Time-Series' sheet is missing", call.=FALSE)
#         CAAMat <- array(NA, dim=c(1,length(Data@Year),Data@MaxAge))
#         Year <- Data@Year
#
#         CAAYr <- as.numeric(dname[dname !="Year"])
#         if (length(CAAYr)<1) {
#           message("No catch-at-age data found")
#         } else {
#           YrInd <- match(CAAYr, Year) # match years
#           if (max(CAAYr) > max(Year))
#             stop("More years in CAA than Time-Series Year", call.=FALSE)
#           CAAdat <- data.matrix(dat[2:nrow(dat),])
#
#           if (ncol(CAAdat)> Data@MaxAge)
#             stop("Number of age-classes in CAA data > MaxAge", call.=FALSE)
#           if (ncol(CAAdat) < Data@MaxAge) {
#             message("Number of age-classes in CAA data < MaxAge. Filling with 0s")
#             zeromat <- matrix(0, nrow=nrow(CAAdat), ncol=Data@MaxAge-ncol(CAAdat))
#             CAAdat <- cbind(CAAdat, zeromat)
#           }
#           CAAMat[1, YrInd, 1:ncol(CAAdat)] <- CAAdat
#           Data@CAA <- CAAMat
#         }
#       } else if (sh == "CAL") {
#         CAL_bins <- as.numeric(dat[1,])
#         if (!all(diff(CAL_bins) == diff(CAL_bins)))
#           stop('Length bins do not have equal intervals' , call.=FALSE)
#         if (any(!is.finite(Data@Year)))
#           stop("'Year' in 'Time-Series' sheet is missing", call.=FALSE)
#         CALMat <- array(NA, dim=c(1,length(Data@Year), length(CAL_bins)-1))
#         Year <- Data@Year
#         CALYr <- as.numeric(dname[dname !="Year" & dname !="CAL_bins"])
#         if (length(CALYr)<1) {
#           message("No catch-at-length data found")
#         } else {
#           YrInd <- match(CALYr, Year) # match years
#           if (max(CALYr) > max(Year))
#             stop("More years in CAL than Time-Series Year", call.=FALSE)
#           ncol <- ncol(dat)
#           CALdat <- data.matrix(dat[3:nrow(dat),1:ncol])
#           if (!all(is.na(CALdat[,ncol]))) {
#             stop("Number of Catch-at-Length bins (CAL_bins) should \nbe 1 greater than number columns of CAL data", call.=FALSE)
#           }
#           CALdat <- CALdat[,1:(ncol-1)]
#           CALMat[1, YrInd, ] <- CALdat
#         }
#         Data@CAL_bins <- CAL_bins
#         Data@CAL <- CALMat
#       }
#     }
#
#   } # end sheet loop
#   Data
# }




#' Run a Management Procedure
#'
#' @param Data A MSEtool Data object
#' @param MPs The name of the MP to run (or a vector or names)
#' @param reps Number of repetitions
#' @param perc Percentile to summarize reps (default is median)
#' @param chkMPs Logical. Should the MPs be checked before attempting to run them?
#' @param silent Logical. Should messages by suppressed?
#'
#' @export
#' @examples
#' Data_TAc <- runMP(MSEtool::Cobia)
#' @return invisibly returns the Data object
#'
runMP <- function(Data, MPs = NA, reps = 100, perc=0.5, chkMPs=FALSE, silent=FALSE) {
  if (!methods::is(MPs, 'character') && !all(is.na(MPs))) stop('MPs must be character string', call.=FALSE)
  if (!methods::is(Data, 'Data')) stop("Data must be class 'Data'", call.=FALSE)
  if (all(is.na(MPs))) {
    MPs <- avail("MP", msg = !silent)
    if (!silent) message("running all available MPs")
  }
  if (chkMPs) {
    cans <- Can(Data, MPs=MPs, silent=silent)
    MPs <- MPs[MPs %in% cans]
  }
  if (length(MPs) <1) stop("No MPs possible")

  MPrecs <- applyMP(Data, MPs, reps, nsims=1, silent=silent)

  if (is.na(Data@nareas)) Data@nareas <- 2
  names <- c("TAC", "Effort", "LR5", "LFR", "HS", "Rmaxlen",
             "L5", "LFS", 'Vmaxlen', 'Spatial')
  mat <- matrix(0, nrow=length(MPs), ncol=length(names)+Data@nareas-1)
  for (x in seq_along(names)) {
    temp <- lapply(MPrecs[[1]], '[[', names[x])
    if (names[x]!="Spatial") {
      mat[,x] <- unlist(lapply(temp, quantile, probs=perc, na.rm=TRUE))
    } else {
      mat[,x:ncol(mat)] <- t(matrix(unlist(temp), nrow=Data@nareas, ncol=length(MPs)))
    }
  }
  rownames(mat) <- MPs
  names[length(names)] <- "Area 1"
  if (Data@nareas > 1) names <- c(names, paste('Area', 2:Data@nareas))
  colnames(mat) <- names

  if (nrow(mat) > 1) {
    allNA <- colSums(apply(mat, 2, is.na)) == length(MPs)
    matout <- data.frame(round(mat[,!allNA], 2), stringsAsFactors = FALSE)
    names(matout) <- names[!allNA]
    if (!silent) print(as.matrix(matout), na.print="")
  }
  if (nrow(mat) == 1) {
    mat <- data.frame(mat)
    matout <- mat[!is.na(mat)]
    matout <- matrix(matout, nrow=nrow(mat))
    colnames(matout) <- names[!is.na(mat)]
    rownames(matout) <- MPs
    if (!silent) print(as.matrix(round(matout,2)), na.print="")
  }

  invisible(MPrecs[[2]])
}



# applyMP2 <- function(Data, MPs = NA, reps = 100, nsims=NA, silent=FALSE) {
#   if (class(Data) != "Data") stop("First argument must be object of class 'Data'", call.=FALSE)
#   Data <- updateMSE(Data)
#   if (is.na(nsims)) nsims <- length(Data@Mort)
#   nMPs <- length(MPs)
#
#   if (.hasSlot(Data, "nareas")) {
#     nareas <- Data@nareas
#   } else {
#     nareas <- 2
#   }
#   returnList <- list() # a list nMPs long containing MPs recommendations
#   recList <- list() # a list containing nsim recommendations from a single MP
#   TACout <- array(NA, dim=c(nMPs, reps, nsims))
#   # if (!sfIsRunning() | (nMPs < 8 & nsims < 8)) {
#     for (mp in 1:nMPs) {
#       temp <- sapply(1:nsims, MPs[mp], Data = Data, reps = reps)
#       slots <- slotNames(temp[[1]])
#       for (X in slots) { # sequence along recommendation slots
#         if (X == "Misc") { # convert to a list nsim by nareas
#           rec <- lapply(temp, slot, name=X)
#         } else {
#           rec <- do.call("cbind", lapply(temp, slot, name=X)) # unlist(lapply(temp, slot, name=X))
#         }
#         if (X == "Spatial") { # convert to a matrix nsim by nareas
#           rec <- matrix(rec, nareas, nsims, byrow=FALSE)
#         }
#         recList[[X]] <- rec
#         for (x in 1:nsims) Data@Misc[[x]] <- recList$Misc[[x]]
#         recList$Misc <- NULL
#       }
#       if (length(recList$TAC)>0)  TACout[mp,,] <- recList$TAC
#       returnList[[mp]] <- recList
#       if (!silent && sum(is.na(recList$TAC)) > 0.5 * reps)
#         message("Method ", MPs[mp], " produced greater than 50% NA values")
#     }
#   # } else {
#   #   for (mp in 1:nMPs) {
#   #     temp <- sfSapply(1:nsims, MPs[mp], Data = Data, reps = reps)
#   #     slots <- slotNames(temp[[1]])
#   #     for (X in slots) { # sequence along recommendation slots
#   #       if (X == "Misc") { # convert to a list nsim by nareas
#   #         rec <- lapply(temp, slot, name=X)
#   #       } else {
#   #         rec <- do.call("cbind", lapply(temp, slot, name=X)) # unlist(lapply(temp, slot, name=X))
#   #       }
#   #       if (X == "Spatial") { # convert to a matrix nsim by nareas
#   #         rec <- matrix(rec, nareas, nsims, byrow=FALSE)
#   #       }
#   #       recList[[X]] <- rec
#   #       for (x in 1:nsims) Data@Misc[[x]] <- recList$Misc[[x]]
#   #       recList$Misc <- NULL
#   #     }
#   #     if (length(recList$TAC)>0) TACout[mp,,] <- recList$TAC
#   #     returnList[[mp]] <- recList
#   #
#   #     if (!silent && sum(is.na(recList$TAC)) > 0.5 * reps)
#   #       message("Method ", MPs[mp], " produced greater than 50% NA values")
#   #   }
#   #
#   # }
#
#   Data@TAC <- TACout
#   Data@MPs <- MPs
#
#   list(returnList, Data)
# }


#' Identify management procedures (MPs) based on data availability
#'
#' Diagnostic tools that look up the slot requirements of each MP and
#' compares to the data available in the Data object.
#'
#' @param Data A data-limited methods data object (class Data)
#' @param timelimit The maximum time (seconds) taken for an MP to undertake
#' 5 reps (this filters out methods that are too slow)
#' @param MPs Optional list of MP names
#' @param dev Logical. Run in development mode?
#' @seealso \link{avail} \linkS4class{Data}
#' @examples
#' CanMPs <- Can(MSEtool::Cobia)
#' CantMPs <- Cant(MSEtool::Cobia)
#' Needs <- Needed(MSEtool::Cobia)
#' @describeIn Can Identifies MPs that have the correct data, do not produce errors,
#' and run within the time limit.
#' @export
Can <- function(Data, timelimit = 1, MPs=NA, dev=FALSE, silent=FALSE) {
  DLMdiag(Data, "available",  timelimit = timelimit, funcs1=MPs, dev=dev, silent=silent)
}


#' @describeIn Can Identifies MPs that don't have sufficient data, lead to errors, or don't run in
#' time along with a list of their data requirements.
#' @export Cant
Cant <- function(Data, timelimit = 1, silent=FALSE) {
  DLMdiag(Data, "not available", timelimit = timelimit, silent=silent)
}

#' @describeIn Can Internal function called by `Can` and `Cant`
#' @param command What to calculate? Character. Options = c("available", "not available", "needed")
#' @param reps The number of replicates for the MP
#' @param funcs1 A character vector of the MP names (optional)
#' @param silent Logical Display messages?
#' @export
DLMdiag <- function(Data, command = c("available", "not available", "needed"), reps = 5,
                    timelimit = 1, funcs1=NA, dev=FALSE, silent=FALSE) {
  command <- match.arg(command)
  if (!methods::is(Data,"Data")) stop("First argument must be object of class 'Data'", call.=FALSE)
  set.seed(101)
  Data <- updateMSE(Data)
  if (all(is.na(funcs1))) funcs1 <- avail("MP", msg=FALSE)
  isMP <- vapply(funcs1, function(x) inherits(get(x), "MP"), logical(1))
  if (any(!isMP)) stop(paste0("Not an MP: ", paste(funcs1[!isMP], collapse = ", ")))

  good <- rep(TRUE, length(funcs1))
  report <- rep("Passed test.", length(funcs1))
  test <- list()
  timey <- numeric(length(funcs1))

  rr <- try(slot(Data, "Misc"), silent = TRUE)
  if (inherits(rr,"try-error")) Data@Misc <- list()
  if (!dev) {
    # ReqData <- MSEtool:::ReqData # ReqData in internal sysdata
    builtin <- funcs1[funcs1 %in% ReqData$MP]
    custom <- funcs1[!funcs1 %in% ReqData$MP]
    inMPind <- which(funcs1 %in% builtin)
    cMPind <- which(funcs1 %in% custom)
    repp <- rep('', length(funcs1))
    # built in MPs (doesn't require 'dependencies' line) 
    reqdata <-  ReqData[match(builtin, ReqData$MP),2]

    repp[inMPind] <- vapply(reqdata, match_slots, character(1), Data = Data, internal=TRUE)

    # custom MPs
    temp <- lapply(custom, function(x) paste(format(match.fun(x)), collapse = " "))
    repp[cMPind] <- vapply(temp, match_slots, character(1), Data = Data)

    chk_needed <- nzchar(repp) # TRUE = has missing data
  } else {
    repp <- rep('', length(funcs1))
    chk_needed <- rep(FALSE, length(funcs1))
  }


  if (command == "needed") {
    # repp[!chk_needed] <- "All required data are present. Test MP with Can()"
    repp[chk_needed] <- paste0("Missing data: ", repp[chk_needed])
    ind <- nchar(repp)>0
    output <- matrix(repp[ind], ncol = 1, dimnames = list(funcs1[ind]))
    return(output)
  }

  report[chk_needed] <- paste0("Missing data: ", repp[chk_needed])
  good[chk_needed] <- FALSE

  for (y in 1:length(funcs1)) {
    if (!silent) message('Checking ', funcs1[y], ' (', y, '/', length(funcs1), ')')
    if(!chk_needed[y]) {
      setTimeLimit(timelimit * 1.5)
      time1 <- Sys.time()
      test[[y]] <- tryCatch(do.call(funcs1[y], list(x = 1, Data = Data, reps = reps)),
                            error = function(e) as.character(e))

      time2 <- Sys.time()
      setTimeLimit(Inf)
      timey[[y]] <- time2 - time1

      if (timey[[y]] > timelimit) {
        report[y] <- "Exceeded the user-specified time limit."
        good[y] <- FALSE
        # } else if (is.character(test[[y]]) | is.na(test[[y]])) { # Error message
      } else if (is.character(test[[y]])) { # Error message
        report[y] <- "MP returned an error. Check MP function and/or Data object."
        good[y] <- FALSE
      } else if (inherits(test[[y]], "Rec")) {
        # Rec_test <- vapply(slotNames("Rec"), function(x) NAor0(slot(test[[y]], x)), logical(1))
        slots <- slotNames("Rec")
        slots <- slots[slots!="Misc"]
        Rec_test <- vapply(slots, function(x) all(is.na(slot(test[[y]], x))), logical(1))
        if(all(Rec_test)) { # If all NAor0
          report[y] <- "Produced all NA scores. Check MP function and/or Data object."
          good[y] <- FALSE
        }
      }
    }
  }

  if (command == "available") {
    # return(matrix(report[good], ncol = 1, dimnames = list(funcs1[good])))
    return(funcs1[good])
  }
  if (command == "not available") {
    # return(matrix(report[!good], ncol = 1, dimnames = list(funcs1[!good])))
    return(matrix(c(funcs1[!good], report[!good]), ncol = 2))
  }
}

#' Function to run a set of input control methods
#'
#' Runs a set of input control methods are returns the output in a single table
#'
#' @param Data A Data object
#' @param MPs A list of input MPs, if NA all available input MPs are run
#' @param reps Number of repetitions (for those methods that use them)
#' @param timelimit Maximum timelimit to run MP (in seconds)
#' @param CheckMPs Logical, the Can function is run if this is TRUE
#' @param msg Logical. Should messages be printed?
#' @author A. Hordyk
#' @examples
#' \dontrun{
#' library(MSEtool)
#' Input(MSEtool::Cobia)
#' }
#'
#' @export
Input <- function(Data, MPs = NA, reps = 100, timelimit = 10, CheckMPs = TRUE,
                  msg=TRUE) {
  if (!methods::is(Data, "Data")) stop("First argument must be object of class 'Data'", call.=FALSE)
  Data <- updateMSE(Data)
  if (msg) message("Checking which MPs can be run")

  if (CheckMPs) PosMPs <- Can(Data, timelimit = timelimit)
  if (!CheckMPs) PosMPs <- MPs
  PosMPs <- PosMPs[PosMPs %in% avail("Input", msg=FALSE)]
  if (!is.na(MPs[1])) Data@MPs <- MPs[MPs %in% PosMPs]
  if (is.na(MPs[1])) Data@MPs <- PosMPs
  funcs <- Data@MPs

  if (length(funcs) == 0) {
    stop("None of the methods 'MPs' are possible given the data available")
  } else {
    nareas <- Data@nareas
    areacol <- paste("Area", 1:nareas)
    Out <- matrix(NA, nrow = length(funcs), ncol = 4+nareas)
    colnames(Out) <- c("Effort", "LR5", "LFR", "Harvest Slot Limit", areacol)
    rownames(Out) <- funcs

    for (mm in 1:length(funcs)) {
      if (msg) message("Running ", mm, " of ", length(funcs), " - ", funcs[mm])

      runIn <- runInMP(Data, MPs = funcs[mm], reps = reps)[[1]][[1]]
      if (length(runIn$Effort) > 0) Out[mm, 1] <- runIn$Effort[1]
      if (length(runIn$LR5) > 0) Out[mm, 2] <- runIn$LR5[1]
      if (length(runIn$LFR) > 0) Out[mm, 3] <- runIn$LFR[1]
      if (length(runIn$HS) > 0) Out[mm, 4] <- runIn$HS[1]
      Out[mm, 5:ncol(Out)] <- runIn$Spatial[1,]
    }
  }
  Out <- round(Out,2)
  print(Out, na.print='')
  invisible(Out)

}

needed <- function(Data, funcs) {
  rr <- try(slot(Data, "Misc"), silent = TRUE)
  if (inherits(rr, "try-error")) Data@Misc <- list()
  Data@Misc <- list()

  temp <- lapply(funcs, function(x) paste(format(match.fun(x)), collapse = " "))
  repp <- vapply(temp, match_slots, character(1), Data = Data)
  repp[!nzchar(repp)] <- "All data available. MP returns NA."
  matrix(repp, ncol = 1, dimnames = list(funcs))
}



# Internal function for:
## Required():
## needed(): matches slotnames of Data class to those that are required in an MP func
##           but also return NAor0 = TRUE
match_slots <- function(func, slotnams = paste0("Data@", slotNames("Data")),
                        slots = slotNames("Data"), Data = NULL, internal=FALSE) {
  # check if each slotname in Data class is required in an MP (T/F)
  slot2 <- paste0('\\b', slots, '\\b')
  if(internal) ind_MP <- vapply(slot2, grepl, logical(1), x = func)
  if(!internal) ind_MP <- vapply(slotnams, grepl, logical(1), x = func)
  if(!is.null(Data) && inherits(Data, "Data")) { # check if Data slots return NA or zero
    ind_NAor0 <- vapply(slots, function(x) all(is.na(slot(Data, x))), logical(1))
    repp <- slots[ind_MP & ind_NAor0] # returns slots where both tests are true
  } else {
    repp <- slots[ind_MP]
  }
  return(paste(repp, collapse = ", "))
}


#' @describeIn Can Identifies what data are needed to run
#' the MPs that are currently not able to run given a Data
#' object
#' @export Needed
Needed <- function(Data, timelimit = 1, silent=FALSE) {
  DLMdiag(Data, "needed", timelimit = timelimit, silent=silent)
}


#' Make stochastic variables certain for only one rep
#'
#' As title.
#'
#' @param Data An object of class Data that has been run though TAC()
#' @author T. Carruthers
#' @export
#' @keywords internal
OneRep <- function(Data) {
  if (!methods::is(Data,"Data")) stop("First argument must be object of class 'Data'", call.=FALSE)
  Data <- updateMSE(Data)
  Data@CV_Cat =  Data@CV_Ind = Data@CV_Rec = matrix(tiny, nrow=1, ncol=1)
  Data@CV_Dt = Data@CV_AvC = Data@CV_Mort = Data@CV_FMSY_M = Data@CV_BMSY_B0 =
    Data@CV_Cref = Data@CV_Bref = Data@CV_Iref = Data@CV_Dep = Data@CV_Abun =
    Data@CV_L50 = Data@CV_vbK = Data@CV_vbLinf = Data@CV_vbt0 = Data@CV_LFC =
    Data@CV_LFS = Data@CV_wla = Data@CV_wlb = Data@CV_steep = tiny
  Data
}



#' A generic OFL plot for NOAA use
#'
#' As title.
#'
#'
#' @param Data An object of class Data that has been run though TAC()
#' @param xlims x axis limits
#' @param perc The percentile of the OFL distribution to be plotted
#' @return A table of performance metrics.
#' @author T. Carruthers
#' @export
plotOFL <- function(Data, xlims = NA, perc = 0.5) {
  if (!methods::is(Data, "Data")) stop("First argument must be object of class 'Data'", call.=FALSE)
  Data <- updateMSE(Data)
  cols <- rep(c("black", "red", "green", "blue", "orange", "brown", "purple",
                "dark grey", "violet", "dark red", "pink", "dark blue", "grey"),  4)
  ltys <- rep(1:4, each = 13)

  funcs <- Data@MPs
  nMPs <- length(funcs)

  if (is.na(xlims[1]) | length(xlims) != 2) {
    xlims <- quantile(Data@TAC, c(0.005, 0.9), na.rm = T)
    if (xlims[1] < 0) xlims[1] <- 0
  }
  if (!NAor0(Data@Ref)) {
    if (xlims[1] > Data@Ref) xlims[1] <- max(0, 0.98 * Data@Ref)
    if (xlims[2] < Data@Ref) xlims[2] <- 1.02 * Data@Ref
    if (xlims[2] > Data@Ref * 2) xlims[2] <- 2 * Data@Ref
  }
  ylims <- c(0, 1)

  plot(NA, NA, xlim = xlims, ylim = ylims, main = "", xlab = "", ylab = "",
       col = "white", lwd = 3, type = "l")
  abline(h = 0)
  if (!NAor0(Data@Ref)) {
    abline(v = Data@Ref, col = "light grey", lwd = 3)
    if (!NAor0(Data@Ref_type[1]))
      legend("bottomright", Data@Ref_type, text.col = "grey",
             bty = "n")
  }

  for (m in 1:nMPs) {

    if (sum(!is.na(Data@TAC[m, , 1])) > 10) {
      # only plot if there are sufficient non-NA TAC samples
      x <- density(Data@TAC[m, , 1], from = 0, na.rm = T)$x
      y <- density(Data@TAC[m, , 1], from = 0, na.rm = T)$y
      y <- y/max(y)
      lines(x, y, col = cols[m])
    } else {
      print(paste("Method ", funcs[m], " produced too many NA TAC values for plotting densities",
                  sep = ""))
    }
    if (!is.na(perc[1]))
      abline(v = quantile(Data@TAC[m, , 1], p = perc, na.rm = T),
             col = cols[m], lty = 2)
  }

  cind <- 1:nMPs
  legend("topright", funcs, text.col = cols[cind], col = cols[cind], lty = 1, bty = "n", cex = 0.75)

  mtext(paste("OFL (", Data@Units, ")", sep = ""), 1, outer = F, line = 2.6)
  mtext(paste("Standardized relative frequency", sep = ""), 2, outer = F, line = 2.6)
  # mtext(paste('OFL calculation for
  # ',Data@Name,sep=''),3,outer=F,line=1)

}

#' Enlarge (replicate) a DLM data object to create an additional dimension for
#' simulation / sensitivity testing
#'
#' Replicates position 1 data to multiple positions for sensitivity testing etc
#'
#'
#' @param Data A data-limited methods data object
#' @param nrep The number of positions to expand the DLM object to
#' @author T. Carruthers
#' @export
replic8 <- function(Data, nrep) {

  slotnam <- slotNames(Data)
  slotnam <- slotnam[slotnam != "Ref" & slotnam != "OM" & slotnam !=
                       "MaxAge" & slotnam != "CAL_bins" & slotnam != "Year"]

  for (sl in 1:length(slotnam)) {
    slt <- attr(Data, slotnam[sl])
    if (inherits(slt, "matrix")) {
      attr(Data, slotnam[sl]) <- matrix(rep(slt, each = nrep),
                                        nrow = nrep, ncol = ncol(slt))
    } else if (inherits(slt, "numeric")) {
      attr(Data, slotnam[sl]) <- rep(slt, nrep)
    } else if (inherits(slt, "array")) {
      attr(Data, slotnam[sl]) <- array(rep(slt, each = nrep),
                                       dim = c(nrep, dim(slt)[2:3]))
    }
  }
  Data
}


#' Sensitivity analysis
#'
#' A function that determines the inputs for a given data-limited method of
#' class Output and then analyses the sensitivity of TAC estimates to
#' marginal differences in each input. The range used for sensitivity is based
#' on the user-specified CV for that input (e.g. CV_Mort, Mort)
#'
#' @param Data A data-limited methods data object
#' @param MP A character string representing an MP applied in calculating the TAC recommendations in the DLM object
#' @param nsense The number of points over which to calculate the TAC (resolution)
#' @param reps The number of samples of the quota taken for the calculation of the TAC
#' @param perc The percentile of the sample TAC
#' @param ploty A logical switch, (T/F, should a plot be drawn?)
#'
#' @author T. Carruthers
#' @export
#' @examples
#' \dontrun{
#' Data <- Sense(MSEtool::Cobia, "AvC")
#' }
Sense <- function(Data, MP, nsense = 6, reps = 100, perc = c(0.05, 0.5, 0.95), ploty = T) {
  if (!methods::is(Data, "Data")) stop("First argument must be object of class 'Data'", call.=FALSE)
  Data <- updateMSE(Data)

  DLM_data2 <- Data
  nm <- deparse(substitute(DLM_data2))
  # refTAC <- quantile(getTAC(DLM_data2, MP, reps)[[1]], perc, na.rm = T)
  refTAC <- quantile(applyMP(DLM_data2, MPs=MP, reps=reps)[[1]][[1]]$TAC, perc, na.rm = T)

  Data <- DLM_data2
  reqs <- Required(MP)  #read.csv(paste(getwd(),'/Data/Data requirements.csv',sep=''),header=T)
  # ind <- (1:nrow(reqs))[reqs[, match(MP, names(reqs))] == "Y"]
  # for(i in 1:length(reqs))

  slotsCV <- slotNames("Data")[grep("CV_", slotNames("Data"))]
  slots <- rep("", length(slotsCV))
  for (i in 1:length(slotsCV)) slots[i] <- substr(slotsCV[i], 4, nchar(slotsCV[i]))

  ind <- slots %in% unlist(strsplit(reqs[1,2], ", "))
  slots <- slots[ind]
  slotsCV <- slotsCV[ind]
  sname <- slots
  nslots <- length(slots)

  nrep <- nslots * nsense
  Data <- replic8(Data, nrep)
  pss <- seq(0, 1, length.out = nsense + 2)[2:(nsense + 1)]
  vals <- array(NA, dim = c(nslots, nsense))

  for (i in 1:nslots) {
    ind <- (((i - 1) * nsense + 1):(i * nsense))
    mn <- attr(Data, slots[i])[1]
    cv <- attr(Data, slotsCV[i])[1] * 2  # twice the CV of the variable specified in the DLM object
    if ('numeric' %in% class(attr(Data, slots[i]))) {
      if (mn > 0) {
        attr(Data, slots[i])[ind] <- qlnorm(pss, mconv(mn,
                                                       cv * mn), sdconv(mn, cv * mn))
        vals[i, ] <- qlnorm(pss, mconv(mn, cv * mn), sdconv(mn,
                                                            cv * mn))
      } else {
        attr(Data, slots[i])[ind] <- -qlnorm(pss, mconv(-mn,
                                                        cv * -mn), sdconv(-mn, cv * -mn))
        vals[i, ] <- -qlnorm(pss, mconv(-mn, cv * -mn), sdconv(-mn,
                                                               cv * -mn))
      }
    } else {
      cv <- attr(Data, slotsCV[i])[1]
      attr(Data, slots[i])[ind, ] <- attr(Data, slots[i])[ind, ] * qlnorm(pss, mconv(1, cv), sdconv(1, cv))
      vals[i, ] <- qlnorm(pss, mconv(1, cv), sdconv(1, cv))
    }
  }

  # TACa <- getTAC(Data, MPs = MP, reps = reps)[[1]]
  TACa <- applyMP(Data, MPs=MP, reps=reps)[[1]][[1]]$TAC
  TACa <- apply(TACa, 2, quantile, p = perc, na.rm = T)
  LB <- ((1:nslots) - 1) * 4 + 1
  UB <- (1:nslots) * 4
  sense <- matrix(data = NA, nrow = 4 * nslots, ncol = nsense + 1)

  for (i in 1:nslots) {
    ind <- ((i - 1) * nsense + 1):(i * nsense)
    dat <- TACa[, ind]

    sense[LB[i], 2:(nsense + 1)] <- vals[i, ]
    sense[(LB[i] + 1):UB[i], 2:(nsense + 1)] <- dat
    sense[LB[i], 1] <- slots[i]
    sense[(LB[i] + 1):UB[i], 1] <- perc
  }

  DLM_data2@Sense <- sense

  if (ploty) {
    ylimy <- range(TACa)
    # dev.new2(width=10,height=0.5+3*ceiling(nslots/2))
    par(mfrow = c(ceiling(nslots/2), 2), mai = c(0.4, 0.4, 0.01, 0.01),
        omi = c(0.4, 0.4, 0.4, 0.01))
    for (i in 1:nslots) {
      ind <- (((i - 1) * nsense + 1):(i * nsense))
      dat <- TACa[, ind]
      xlimy <- range(vals[i, ])
      plot(xlimy, rep(refTAC[2], 2), ylim = ylimy, xlim = xlimy,
           type = "l", col = "#99999960", main = "", xlab = "", ylab = "")
      abline(h = refTAC[c(1, 3)], col = "#99999960", lty = 2)
      abline(v = slot(DLM_data2, slots[i]), col = "#99999960", lty = 2)
      lines(vals[i, ], dat[2, ], col = "red", lwd = 1.5)
      lines(vals[i, ], dat[1, ], col = "red", lty = 2, lwd = 1.5)
      lines(vals[i, ], dat[3, ], col = "red", lty = 2, lwd = 1.5)
      legend("top", legend = sname[i], text.col = "blue", bty = "n")
    }

    mtext(paste("Output control (", Data@Units, ")", sep = ""),
          2, outer = T, line = 0.5)
    mtext("Parameter / variable input level", 1, outer = T, line = 0.5)
    mtext(paste("Sensitivity analysis for ", Data@Name, ": ", MP,
                sep = ""), 3, outer = T, line = 0.5)
  }
  # assign(nm,DLM2,envir=.GlobalEnv)
  DLM_data2
}



#' Calculate TAC recommendations for more than one MP
#'
#' A function that returns the stochastic TAC recommendations from a vector of
#' output control MPs given a data object Data
#'
#'
#' @param Data A data-limited methods data object
#' @param MPs optional vector of MP names
#' @param reps Number of repetitions
#' @param timelimit The maximum time (seconds) taken to complete 10 reps
#' @param checkMP Logical. Check if the MP can be run first?
#' @param silent Logical. Suppress messages?
#' @author T. Carruthers
#' @examples
#' \dontrun{
#' library(MSEtool)
#' Data <- TAC(MSEtool::Cobia)
#' plot(Data)
#' }
#' @export
TAC <- function(Data, MPs = NA, reps = 100, timelimit = 1, checkMP=TRUE, silent=FALSE) {
  if (!methods::is(Data, "Data")) stop("First argument must be object of class 'Data'", call.=FALSE)
  Data <- updateMSE(Data)
  nm <- deparse(substitute(Data))
  if (checkMP) {
    PosMPs <- Can(Data, timelimit = timelimit, silent = silent)
  } else {
    PosMPs <- avail("Output", msg=!silent)
  }

  PosMPs <- PosMPs[PosMPs %in% avail("Output", msg=FALSE)]
  Data@PosMPs <- PosMPs
  if (!is.na(MPs[1])) Data@MPs <- MPs[MPs %in% PosMPs]
  if (is.na(MPs[1])) Data@MPs <- PosMPs
  funcs <- Data@MPs

  if (length(funcs) == 0) {
    stop("None of the methods 'MPs' are possible given the data available")
  } else {
    Data <- applyMP(Data, MPs = funcs, reps, silent = silent)[[2]]
    return(Data)
  }

}


#' Join Data objects present in a list
#'
#' A function that combined a list of data objects into a single data object (same dimensions but can have different numbers of simulations)
#'
#'
#' @param DataList A list of data objects of identical dimension (except for simulation)
#' @author T. Carruthers
#' @seealso \link{joinMSE} \link{joinHist}
#' @export
joinData<-function(DataList){

  if (!methods::is(DataList,"list")) stop("DataList must be a list")
  if (length(DataList) < 2) stop("DataList list doesn't contain multiple MSE objects")

  Data <- new("Data")
  #Data<-DataList[[1]]
  nD <- length(DataList)

  slots <- slotNames(Data)
  slots_identical <- c("Name", "Common_Name", "Species", "Region", "Year", "MaxAge", "Units", "Ref_type", "PosMPs", "MPs", "nareas", "LHYear",
                       "AddIunits", "AddIndType")
  slots <- slots[!slots %in% slots_identical]

  nslots <- length(slots)
  getslot <- function(obj, name) slot(obj, name) # weird issue with namespace conflict and the @Cat slot
  #getslotclass<-function(obj,name)class(slot(obj,name))
  #sclass<-sapply(1:nslots,function(x,obj,slots) getslotclass(obj,slots[x]),obj=DataList[[1]],slots=slots)

  for (sn in slots) {
    templist <- lapply(DataList, getslot, name = sn)
    tempval <- templist[[1]]

    if (inherits(tempval, "numeric") || inherits(tempval, "integer")) {
      
      if (sn == "CAL_bins" || sn == "CAL_mids") {
        nbin <- vapply(templist, length, numeric(1))
        slot(Data, sn) <- templist[[which.max(nbin)]]
        
        if (length(unique(nbin)) > 1) {
          warning(paste0("joinData() found Data@", sn, " of various lengths. Using the vector in DataList[[", which.max(nbin), "]]."))
        }
      } else {
        slot(Data, sn) <- unlist(templist)
      }
      
    } else if (is.array(tempval) || is.matrix(tempval)) {

      if (sn == "CAL") {
        
        nbin <- vapply(templist, function(x) dim(x)[3], numeric(1))
        templist2 <- vector("list", nD)
        for (i in 1:nD) {
          templist2[[i]] <- array(0, dim = c(dim(templist[[i]])[1:2], max(nbin)))
          templist2[[i]][ , , 1:nbin[i]] <- templist[[i]]
        }
        if (all(diff(do.call("rbind", lapply(templist2, dim))[,2]) == 0)) {
          # arrays may be different dimensions if MPs fail
          slot(Data, sn) <- abind::abind(templist2, along=1)
        }
        
      } else {
        
        if (all(diff(do.call("rbind", lapply(templist, dim))[,2]) == 0)) {
          # arrays may be different dimensions if MPs fail
          attr(Data, sn) <- abind::abind(templist, along=1)
        }
      }

    } else if (inherits(tempval, "list") & !inherits(tempval, "data.frame")) {
      if (sn == "Misc") { # Ignore StockPars, FleetPars, ReferencePoints from Hist object
        slot(Data, sn) <- do.call(c, lapply(templist, function(x) x[names(x) == ""]))
      } else {
        slot(Data, sn) <- do.call(c, templist)
      }
      
    } else if (inherits(tempval, "data.frame")) {
      slot(Data, sn) <- bind_rows(templist)
    }
  }

  for (sn in slots_identical) {
    templist <- lapply(DataList, slot, name = sn) # debugging
    slot(Data, sn) <- templist[[1]]
  }

  return(Data)
}


#' Find the Management Procedures that use a particular data slot
#'
#' @param slot A slot from an object of class `Data`. Character string.
#' @param silent Logical. Should messages be printed?
#'
#' @return A character string of MPs that use the slot.
#' @author A. Hordyk
#' @export
#'
#' @examples
#' Uses("Mort")
Uses <- function(slot, silent=FALSE) {
  if (!methods::is(slot, "character")) stop("Slot must be character", call. = FALSE)
  if(length(slot)>1) stop("Slot must be length 1", call. = FALSE)
  if (!slot %in% slotNames('Data')) stop("Slot is not a valid slot in Data object. Use slotNames('Data')", call.=FALSE)
  MPs <- avail("MP", msg=FALSE)
  List <- lapply(seq_along(MPs), function(x) Required(MPs[x]))
  df <- data.frame(matrix(unlist(List), nrow=length(MPs), byrow=T), stringsAsFactors = FALSE)
  mps <- df[grepl(slot, df[,2]),1]
  if (length(mps) >0) {
    if(!silent)
      message("MPs that require Data slot '" , slot, "' are:")
    return(mps)
  } else {
    if(!silent)
      message("No MPs used Data slot '" , slot, "'.")
    return(NULL)
  }
}





# parallelMPs <- function(x, Data, reps, MPs, ss) sapply(ss, MPs[x], Data, reps = reps)

# getTAC <- function(Data, MPs = NA, reps = 100) {
#   if (class(Data) != "Data") stop("First argument must be object of class 'Data'", call.=FALSE)
#   Data <- updateMSE(Data)
#   nsims <- length(Data@Mort)
#   nMPs <- length(MPs)
#   TACa <- array(NA, dim = c(nMPs, reps, nsims))
#
#   if (!sfIsRunning()) {
#     for (ff in 1:nMPs) {
#       temp <- sapply(1:nsims, MPs[ff], Data = Data, reps = reps)
#       tt <- sapply(1:1, AvC,  Data=Data, reps=reps)
#
#       tt@TAC
#
#
#       if (mode(temp) == "numeric")
#         TACa[ff, , ] <- temp
#       if (mode(temp) == "list") {
#         TACa[ff, , ] <- unlist(temp[1, ])
#         for (x in 1:nsims) Data@Misc[[x]] <- temp[2, x][[1]]
#       }
#     }
#   } else {
#     sfExport("Data")
#     if (nsims < 8) {
#       sfExport(list = c("MPs", "reps"))
#       for (ss in 1:nsims) {
#         temp <- (sfSapply(1:length(MPs), parallelMPs, Data = Data,
#           reps = reps, MPs = MPs, ss = ss))
#         if (mode(temp) == "list") {
#           Lens <- unlist(lapply(temp, length))
#           for (X in 1:length(Lens)) {
#           Classes <- unlist(lapply(temp[, X][[1]], class))
#           if (length(unique(Classes)) == 1) {
#             # no Misc object
#             TACa[X, , ss] <- unlist(temp[, X])
#           } else {
#             # a Misc object is include
#             ind <- which(Classes == "list")
#             TACa[X, , ss] <- unlist(temp[, X][[1]][1:(ind - 1),
#             ])
#             Data@Misc[[ss]] <- temp[, X][[1]][ind, ]
#           }
#           }
#         } else {
#           temp <- matrix(temp, nrow = nMPs, ncol = reps, byrow = TRUE)
#           TACa[, , ss] <- temp
#         }
#       }
#     } else {
#       for (ff in 1:nMPs) {
#         temp <- sfSapply(1:nsims, MPs[ff], Data = Data,
#           reps = reps)
#         if (mode(temp) == "numeric")
#           TACa[ff, , ] <- temp
#         if (mode(temp) == "list") {
#           TACa[ff, , ] <- unlist(temp[1, ])
#           for (x in 1:nsims) Data@Misc[[x]] <- temp[2, x][[1]]
#         }
#       }
#     }
#   }
#   for (ff in 1:nMPs) {
#     if (sum(is.na(TACa[ff, , ])) > sum(!is.na(TACa[ff, , ]))) {
#       # only plot if there are sufficient non-NA TAC samples
#       print(paste("Method ", MPs[ff], " produced greater than 50% NA values",
#         sep = ""))
#     }
#   }
#   out <- list(TACa, Data)
#   return(out)
# }
#
#

# #' Conduct stock assessment
# #'
# #' A wrapper function that gets the OFL recommendation in cases where a method
# #' of DLM quota has been specified
# #'
# #'
# #' @usage Sam(Data, MPs = NA, reps = 100, perc = 0.5)
# #' @param Data A data-limited methods data object
# #' @param MPs A character vector of methods of DLM quota, DLM space or DLM size
# #' @param reps The number of samples of quota recommendations by method
# #' @param perc quantile of the recommendation to use
# #' @author T. Carruthers
# #' @export Sam
# Sam <- function(Data, MPs = NA, reps = 100, perc = 0.5) {
#   if (class(Data) != "Data") stop("First argument must be object of class 'Data'", call.=FALSE)
#   Data <- updateMSE(Data)
#   nm <- deparse(substitute(DLM))
#   Data@PosMPs <- MPs
#   funcs <- Data@PosMPs
#   nMPs <- length(funcs)
#   Data@MPs <- funcs
#   temp <- getTAC(Data, MPs = funcs, reps)
#   TACa <- temp[[1]]
#   Data <- temp[[2]]
#   nsim <- length(Data@Mort)
#   ref <- array(rep(Data@Ref, nMPs), c(nsim, nMPs))
#   TACm <- apply(TACa, c(3, 1), quantile, p = perc, na.rm = T)
#   TACbias <- (TACm - ref)/ref * 100
#   POF <- round(apply(TACbias > 0, 2, sum)/length(Data@Mort) * 100,
#     1)
#   Data@TAC <- TACa
#   Data@TACbias <- TACbias
#   Data
# }
#

#' Generate a Data Report
#'
#' A HTML Data Report is generated and opened in a web browser
#'
#' @param Data Either an object of class `Data` or the file path to a valid
#' file to be imported with `XL2Data`
#' @param md Full file path to a valid text file documenting the Data
#' @param name Optional. Name of the output file
#' @param title Title for the Report. Title in the markdown file will override this value
#' @param author Author of the Report. Author in the markdown file will override this value
#' @param date Date of the Report. Date in the markdown file will override this value
#' @param output_format Output file format: `html_document` or `pdf_document`
#' @param open Logical. Open the compiled report?
#' @param quiet Logical.An option to suppress printing of the pandoc command line.
#' @param dir Optional. Directory to save the file. Defaults to `getwd()`
#' @param overwrite Logical. Overwrite an existing file with the same name?
#'
#' @return Nothing. A Data Report is generated and saved in `dir`
#' @export
#' @author A. Hordyk
#' @examples
#' \dontrun{
#' DataInit('Example') # generate example Data Input and Documentation files
#' Report('Example', 'Example.md')
#' }
Report <- function(Data=NULL, md=NULL, name="Data-Report",
                   title="Data Documentation",
                   author="Author Name",
                   date=Sys.Date(),
                   output_format=c("html_document", "pdf_document"),
                   open=TRUE, quiet=TRUE,
                   dir=NULL,
                   overwrite=FALSE) {
  output_format <- match.arg(output_format)
  if (!methods::is(Data, "Data") & !methods::is(Data,"character"))
    stop("Must provide a Data object or file path to import a Data Object")

  if (methods::is(Data,"character")) {
    Data <- XL2Data(Data)
  }

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package \"knitr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package \"rmarkdown\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Load md documentation
  if (is.null(md)) {
    message("No Data Documentation file provided")
    mdtext <- NULL
  } else {
    if (!file.exists(md)) {
      stop(md, " not found")
    }
    mdtext <- readLines(md)

    try.title <- mdtext[grepl("title", mdtext)]
    try.title <- strsplit(try.title, ":")[[1]][2] %>% trimws()
    if (nchar(try.title)>0) {
      message('Using title from ', basename(md))
      title <- try.title
    }
    try.author <- mdtext[grepl("author", mdtext)]
    try.author <- strsplit(try.author, ":")[[1]][2] %>% trimws()
    if (nchar(try.author)>0) {
      message('Using author from ', basename(md))
      author <- try.author
    }

    try.date <- mdtext[grepl("date", mdtext)]
    try.date <- strsplit(try.date, ":")[[1]][2] %>% trimws()
    if (nchar(try.date)>0) {
      message('Using date from ', basename(md))
      date <- try.date
    }

  }

  # Create Rmd file
  if (is.null(dir)) dir <- getwd()
  rmdfile <- file.path(dir, paste0(name, '.Rmd'))
  if (file.exists(rmdfile) & !overwrite)
    stop(rmdfile, " already exists. Use `overwrite=TRUE` to overwrite")
  file.create(rmdfile)

  cat("---\n", file = rmdfile, sep = " ", append = TRUE)
  cat(paste("title:", title, "\n"), file = rmdfile, sep = " ", append = TRUE)
  cat(paste("author:", author, "\n"), file = rmdfile, sep = " ", append = TRUE)
  cat(paste("date:", date, "\n"), file = rmdfile, sep = " ", append = TRUE)
  cat("always_allow_html: yes\n", file = rmdfile, sep = " ", append = TRUE)
  cat("output:
        html_document:
          toc: true
          toc_float: true
        pdf_document:
          toc: true\n", file = rmdfile, sep = " ", append = TRUE)

  cat("---\n", file = rmdfile, sep = " ", append = TRUE)
  cat("\n", file = rmdfile, sep = " ", append = TRUE)
  cat("\n", file = rmdfile, sep = " ", append = TRUE)

  # Metadata section
  cat("## Metadata\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    mdloc <- grepl("## Metadata", mdtext)
    if (all(!mdloc)) stop("'## Metadata' heading missing from ", md)
    temp <- grepl("## Biology", mdtext)
    if (all(!temp)) stop("'## Biology' heading missing from ", md)
    ind1 <- which(mdloc) +1
    if (length(ind1)>1) ind1 <- ind1[1]
    ind2 <- which(temp) -1

    text <- mdtext[ind1:ind2]
    for (i in seq_along(text)){
      cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
    }
  }

  cat("```{r, echo=FALSE} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("metadatatable(Data, output_format=output_format)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n", file = rmdfile, sep = " ", append = TRUE)

  # Biology section
  cat("## Biology\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    mdloc <- grepl("## Biology", mdtext)
    if (all(!mdloc)) stop("'## Biology' heading missing from ", md)
    temp <- grepl("## Selectivity", mdtext)
    if (all(!temp)) stop("'## Selectivity' heading missing from ", md)
    ind1 <- which(mdloc) +1
    ind2 <- which(temp) -1

    text <- mdtext[ind1:ind2]
    for (i in seq_along(text)){
      cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
    }
  }
  cat("```{r, echo=FALSE, out.width='90%', tidy=FALSE, fig.align='center', fig.show='asis'} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- biology_plots(Data)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- growth_plots(Data, fignum=fignum)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)


  # Selectivity section
  cat("## Selectivity\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    mdloc <- grepl("## Selectivity", mdtext)
    if (all(!mdloc)) stop("'## Selectivity' heading missing from ", md)
    temp <- grepl("## Time-Series", mdtext)
    if (all(!temp)) stop("'## Time-Series' heading missing from ", md)
    ind1 <- which(mdloc) +1
    ind2 <- which(temp) -1

    text <- mdtext[ind1:ind2]
    for (i in seq_along(text)){
      cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
    }

  }

  cat("```{r, echo=FALSE, out.width='90%', tidy=FALSE, fig.align='center', fig.show='asis'} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- select_plots(Data, fignum=fignum+1)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)

  # Time-Series section
  cat("## Time-Series\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    mdloc <- grepl("## Time-Series", mdtext)
    if (all(!mdloc)) stop("'## Time-Series' heading missing from ", md)
    temp <- grepl("## Catch-at-Age", mdtext)
    if (all(!temp)) stop("'## Catch-at-Age' heading missing from ", md)
    ind1 <- which(mdloc) +1
    ind2 <- which(temp) -1

    text <- mdtext[ind1:ind2]
    for (i in seq_along(text)){
      cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
    }

  }



  cat("```{r, echo=FALSE} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("ts_data <- ts_plots(Data,fignum=fignum+1)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("DF <- ts_data[[1]] \n", file = rmdfile, sep = " ", append = TRUE)
  cat("p1 <- ts_data[[2]] \n", file = rmdfile, sep = " ", append = TRUE)
  cat("height <- length(levels(DF$Data))/2 *2\n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- ts_data[[3]] \n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)

  cat("```{r, echo=FALSE, out.width='90%', fig.height=height} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("suppressWarnings(suppressMessages(plot(p1))) \n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)

  cat("```{r, echo=FALSE, out.width='90%'} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- vuln_addInd_plot(Data,fignum=fignum)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- meanLen_plot(Data,fignum=fignum)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)



  # Catch-at-Age section
  cat("## Catch-at-Age\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    mdloc <- grepl("## Catch-at-Age", mdtext)
    if (all(!mdloc)) stop("'## Catch-at-Age' heading missing from ", md)
    temp <- grepl("## Catch-at-Length", mdtext)
    if (all(!temp)) stop("'## Catch-at-Length' heading missing from ", md)
    ind1 <- which(mdloc) +1
    ind2 <- which(temp) -1

    text <- mdtext[ind1:ind2]
    for (i in seq_along(text)){
      cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
    }

  }

  cat("```{r, echo=FALSE, out.width='90%', fig.align='center', fig.show='asis'} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- caa_plot(Data, fignum=fignum+1)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)

  # Catch-at-Length section
  cat("## Catch-at-Length\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    mdloc <- grepl("## Catch-at-Length", mdtext)
    if (all(!mdloc)) stop("'## Catch-at-Length' heading missing from ", md)
    temp <- grepl("## Reference", mdtext)
    if (all(!temp)) stop("'## Reference' heading missing from ", md)
    temp2 <- grepl("## Reference List", mdtext)
    if (all(!temp2)) stop("'## Reference List' heading missing from ", md)
    ind1 <- which(mdloc) +1
    ind2 <- which(temp!=temp2) -1

    text <- mdtext[ind1:ind2]
    for (i in seq_along(text)){
      cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
    }
  }

  cat("```{r, echo=FALSE, out.width='90%', fig.align='center', fig.show='asis'} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- cal_plot(Data, fignum=fignum)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)

  # Reference section
  cat("## Reference\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    ind1 <- ind2 +2
    ind2 <- which(temp2)-1

    text <- mdtext[ind1:ind2]
    for (i in seq_along(text)){
      cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
    }
  }

  cat("```{r, echo=FALSE, out.width='90%'} \n", file = rmdfile, sep = " ", append = TRUE)
  cat("fignum <- ref_plots(Data, fignum=fignum)\n", file = rmdfile, sep = " ", append = TRUE)
  cat("```\n\n", file = rmdfile, sep = " ", append = TRUE)

  # Reference List
  cat("## Reference List\n", file = rmdfile, sep = " ", append = TRUE)
  if (!is.null(mdtext)) {
    temp <- grepl("## Reference List", mdtext)
    ind1 <- which(temp) + 1
    ind2 <- length(mdtext)
    if (ind2 > ind1) {
      text <- mdtext[ind1:ind2]
      for (i in seq_along(text)){
        cat(text[i], "\n", file = rmdfile, sep = "", append = TRUE)
      }
    }
  }
  # Render output file
  message("Rendering to ", dir)
  if (grepl('html', output_format))
    output_file <- file.path(dir, paste0(name, '.html'))
  if (grepl('pdf', output_format))
    output_file <- file.path(dir, paste0(name, '.pdf'))
  rmarkdown::render(rmdfile, output_format =output_format,
                    output_file=output_file,
                    output_dir = dir, params=Data, quiet = quiet, clean=FALSE)

  if (open) browseURL(output_file)

}


metadatatable <- function(Data, i=1, output_format) {
  df <- data.frame(Data@Name[1],
                   Data@Common_Name[1],
                   Data@Species[1],
                   Data@Region[1],
                   Data@LHYear[1],
                   Data@MPrec[i],
                   Data@Units[1],
                   Data@MPeff[i],
                   Data@nareas[1])
  df <- t(df)
  rownames(df) = c(
    'Name',
    'Common Name',
    'Species',
    'Region',
    'Last Historical Year',
    'Last TAC',
    'Units',
    'Last TAE',
    'Number of areas')

  if (output_format == "html_document") {
    suppressWarnings(tab <- knitr::kable(df, caption='Table 1. Summary of metadata'))
  }
  if (output_format == "pdf_document") {
    suppressWarnings(tab <- knitr::kable(df, caption='Table 1. Summary of metadata', format='markdown'))
  }
  suppressWarnings(
    tab %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "responsive"), full_width = F,
        position = "left"
      ) %>%
      kableExtra::column_spec(1, bold=TRUE))
}

biology_plots <- function(Data, i=1, n=20000) {
  Var <- val <- ..scaled.. <- Text <- NA

  slots <- c("Mort", "vbLinf", 'vbK', 'vbt0', 'wla', 'wlb', 'steep', 'sigmaR',
             'L50', 'L95')

  lout <- list()
  for (x in seq_along(slots)) {
    sl <- slots[x]
    mean <- slot(Data, sl)[i]
    if (sl == "L95") {
      cv <- slot(Data, "CV_L50")[i]
    } else {
      cv <- slot(Data, paste0("CV_",sl))[i]
    }

    if (is.na(cv) | length(cv)<1) {
      vals <- rep(mean,n)
    } else {
      if (is.na(mean)) {
        vals <- NA
      } else {
        if (sl =="steep") {
          vals <- sample_steepness2(n, mean, cv)
        } else {

          if (mean < 0) {
            vals <- -trlnorm(n, -mean, cv)
          } else {
            vals <- trlnorm(n, mean, cv)
          }
        }
      }
    }
    lout[[x]] <- data.frame(Var=sl, val=vals, mean=mean, cv=cv)
  }

  plist <- do.call("rbind", lout)
  df <- plist %>% group_by(Var) %>% dplyr::distinct(mean, cv)
  df$x <- df$mean
  # df$mean[df$mean>0.01] <-round(df$mean[df$mean>0.01],)
  # df$mean[df$mean < -0.01] <-round(df$mean[df$mean < -0.01],2)
  df$cv <- round(df$cv,2)
  lab <- sprintf("mu ==%G", df$mean)
  lab2 <- sprintf("CV ==%G", df$cv)

  plist <- dplyr::distinct(plist)
  if (all(is.na(plist$mean))) return(0)

  addText <- FALSE
  textdf <- plist %>% filter(is.na(val))
  if (nrow(textdf) > 0) {
    textdf$Text <- "No values"
    textdf <- dplyr::distinct(textdf)
    addText <- TRUE
  }

  fignum <- 1
  p1 <- ggplot2::ggplot(plist, ggplot2::aes(x=val, y=..scaled..)) +
    ggplot2::geom_density(show.legend = F, fill="lightgray") +
    ggplot2::facet_wrap(~Var, scales="free") +
    ggplot2::theme_minimal() + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                              axis.text.y = ggplot2::element_blank(),
                                              axis.title.x=ggplot2::element_blank()) +
    ggplot2::geom_text(data=df, ggplot2::aes(x=x, y=1.3), parse=TRUE, label=lab) +
    ggplot2::geom_text(data=df, ggplot2::aes(x=x, y=1.1), parse=TRUE, label=lab2) +

    ggplot2::labs(title=paste0('Figure ', fignum, '. Density plots of biological parameters')) +
    ggplot2::expand_limits(y=1.4) +
    ggplot2::theme(strip.text = ggplot2::element_text(size=14))
  if (addText) p1 <- p1 + ggplot2::geom_text(data=textdf, ggplot2::aes(x=0, y=3, label=Text) )
  suppressWarnings(plot(p1))
  fignum


}

growth_plots <- function(Data, i=1, fignum=1) {
  # Mean length-at-age
  lo <- up <- NA
  doGrowth <- FALSE
  if (length(Data@vbLinf[i]) > 0 && !is.na(Data@vbLinf[i])) {
    doGrowth <- TRUE
    fignum <- fignum+1
    vonB <- function(Linf, t0, K, ages) Linf * (1-exp(-K*(ages-t0)))

    meanL <- vonB(Data@vbLinf[i], Data@vbt0[i], Data@vbK[i], 0:Data@MaxAge)
    Lup <- meanL + 1.96*meanL * Data@LenCV[i]
    Llow <- meanL - 1.96*meanL * Data@LenCV[i]
    df <- data.frame(mean=meanL, up=Lup, lo=Llow)

    p2 <- ggplot2::ggplot(df, ggplot2::aes(x=0:Data@MaxAge, ymin=lo, ymax=up)) +
      ggplot2::geom_ribbon() +
      ggplot2::geom_line(ggplot2::aes(y=mean), size=1.2) +
      ggplot2::expand_limits(y=0) +
      ggplot2::labs(x="Age", y="Length",
                    title=paste0('\n\nFigure ', fignum, '. Distribution of length-at-age'),
                    subtitle='Mean length-at-age (solid line) and  2 standard deviations (shaded region)') +
      ggplot2::theme_minimal() +
      ggplot2::geom_text(x=0.75*Data@MaxAge, y=0.5*Data@vbLinf[i],
                         label=paste0("LenCV = ", round(Data@LenCV[i],3)))
    suppressWarnings(plot(p2))
  }
  fignum
}

select_plots <- function(Data, i=1, n=20000, fignum=1) {
  Var <- val <- ..scaled.. <- Text <- Select <- Length <- NA
  lout <- list()
  slots <- c('LFC', 'LFS', 'Vmaxlen')
  for (x in seq_along(slots)) {
    sl <- slots[x]
    mean <- slot(Data, sl)[i]
    if (sl == "L95") {
      cv <- slot(Data, "CV_L50")[i]
    } else if (sl=="Vmaxlen"){
      cv <- NA
    } else {
      cv <- slot(Data, paste0("CV_",sl))[i]
    }

    if (is.na(cv) | length(cv)<1) {
      vals <- rep(mean,n)
    } else {
      if (is.na(mean)) {
        vals <- NA
      } else {
        if (sl =="steep") {
          vals <- sample_steepness2(n, mean, cv)
        } else {

          if (mean < 0) {
            vals <- -trlnorm(n, -mean, cv)
          } else {
            vals <- trlnorm(n, mean, cv)
          }
        }
      }
    }
    lout[[x]] <- data.frame(Var=sl, val=vals, mean=mean, cv=cv)
  }

  plist <- do.call("rbind", lout)


  df <- plist %>% group_by(Var) %>% dplyr::distinct(mean, cv)
  df$x <- df$mean
  # df$mean[df$mean>0.01] <-round(df$mean[df$mean>0.01],)
  # df$mean[df$mean < -0.01] <-round(df$mean[df$mean < -0.01],2)
  df$cv <- round(df$cv,2)
  lab <- sprintf("mu ==%G", df$mean)
  lab2 <- sprintf("CV ==%G", df$cv)
  lab2[3] <- ''

  addText <- FALSE
  textdf <- plist %>% filter(is.na(val))
  if (nrow(textdf) > 0) {
    textdf$Text <- "No values"
    textdf <- dplyr::distinct(textdf)
    addText <- TRUE
  }

  inc <- 0
  if (!all(is.na(plist$mean))) {
    inc <- 1
    p3 <- ggplot2::ggplot(plist, ggplot2::aes(x=val, y=..scaled..)) +
      ggplot2::geom_density(show.legend = F, fill="lightgray") +
      ggplot2::facet_wrap(~Var, scales="free") +
      ggplot2::theme_minimal() + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                axis.text.y = ggplot2::element_blank(),
                                                axis.title.x=ggplot2::element_blank()) +
      ggplot2::geom_text(data=df, ggplot2::aes(x=x, y=1.3), parse=TRUE, label=lab) +
      ggplot2::geom_text(data=df, ggplot2::aes(x=x, y=1.1), parse=TRUE, label=lab2) +

      ggplot2::labs(title=paste0('Figure ', fignum, '. Density plots of selectivity parameters')) +
      ggplot2::expand_limits(y=1.4) +
      ggplot2::theme(strip.text = ggplot2::element_text(size=14))
    if (addText) p3 <- p3 + ggplot2::geom_text(data=textdf, ggplot2::aes(x=0, y=3, label=Text) )

    suppressWarnings(plot(p3))
  }


  if (all(!is.na(df$mean)) && all(df$mean >0) ) {
    if(!is.na(Data@MaxAge) & !is.na(Data@vbLinf[i])) {
      # selectivity-at-age

      Lens <- 0:Data@vbLinf[i]
      iVB2 <- function(t0, K, Linf, L) {
        (-log(1 - L/Linf))/K + t0
      }
      Age <- sapply(seq_along(Lens), function(x) iVB2(Data@vbt0[i], Data@vbK[i],
                                                      Data@vbLinf[i], Lens[x]))
      srs <- (Data@vbLinf[i] - Data@LFS[i]) / ((-log(Data@Vmaxlen[i],2))^0.5)
      srs[!is.finite(srs)] <- Inf
      sls <- (Data@LFS[i] - Data@LFC[i]) /((-log(0.05,2))^0.5)
      SelA <- getsel(lens=Lens[Age>0], lfs=Data@LFS[i], sls=sls, srs=srs)
      SelL <- getsel(lens=Lens, lfs=Data@LFS[i], sls=sls, srs=srs)

      DF <- data.frame(Age=Age[Age>0], Select=SelA)

      fignum <- fignum +inc
      p4 <- ggplot2::ggplot(DF, ggplot2::aes(x=Age, y=Select)) +
        ggplot2::geom_line(size=1.2) +
        ggplot2::expand_limits(y=c(0,1), x=0) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x="Age", y="Selectivity",
                      title=paste0('\n\nFigure ', fignum, '. Selectivity-at-age '))+
        ggplot2::theme(strip.text = ggplot2::element_text(size=14))


      DF <- data.frame(Length=Lens, Select=SelL)

      fignum <- fignum +1
      p5 <- ggplot2::ggplot(DF, ggplot2::aes(x=Length, y=Select)) +
        ggplot2::geom_line(size=1.2) +
        ggplot2::expand_limits(y=c(0,1), x=0) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x="Length", y="Selectivity",
                      title=paste0('\n\nFigure ', fignum, '. Selectivity-at-length '))+
        ggplot2::theme(strip.text = ggplot2::element_text(size=14))

      suppressWarnings(plot(p4))
      suppressWarnings(plot(p5))
    }
  }
  fignum
}


makeDF <- function(Data, slot, i ) {
  Year <- Data@Year
  if (slot=="AddInd") {
    dat <- slot(Data, slot)[i,,]
    dat_cv <- slot(Data, "CV_AddInd")[i,,]
    dat_v <- slot(Data, "AddIndV")[i,,]

    if (!all(is.na(dat))) {
      nind <- dim(dat)[1]
      tlist <- list()
      vlist <- list()
      for (x in 1:nind) {
        tdat <- dat[x,]
        tcv <- dat_cv[x,]
        tv <- dat_v[x,]

        sdvec <- sdconv(tdat,  tdat * tcv)
        muvec <- log(tdat)
        up <- exp(muvec + 1.96*sdvec)
        dw <- exp(muvec - 1.96*sdvec)

        tDF <- data.frame(Year, y=tdat, up, dw)
        tDF$Data <- paste0("Add. Index ", x)
        vlist[[x]] <- data.frame(V=tv, Ind=x)
        tlist[[x]] <- tDF
      }
      DF <- do.call('rbind', tlist)
      DF2 <- do.call('rbind', vlist)
      return(list(DF, DF2))
    } else {
      return(NULL)
    }
  } else {
    dat <- slot(Data, slot)[i,]
    dat_cv <- slot(Data, paste0("CV_",slot))[i,]
    # find first non NA CV
    if (!is.na(dat_cv[1]) & all(is.na(dat_cv[2:length(dat_cv)]))) {
      message(paste0("CV_", slot, " only provided for first year. Assumed for all years"))
      dat_cv[2:length(dat_cv)] <- dat_cv[1]
    }
    sdvec <- sdconv(dat,  dat * dat_cv)
    muvec <- log(dat)
    up <- exp(muvec + 1.96*sdvec)
    dw <- exp(muvec - 1.96*sdvec)

    DF <- data.frame(Year, y=dat, up, dw)
    DF$Data <- slot
  }

  DF
}

ts_plots <- function(Data, i=1, fignum=1) {
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package \"tidyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  Year <- y <- dw <- up <- X <- Ind <- value <- key <- NA
  DF <- makeDF(Data, "Cat", i)
  DF <- rbind(DF, makeDF(Data, "Effort", i))
  DF <- rbind(DF, makeDF(Data, "Ind", i))
  DF <- rbind(DF, makeDF(Data, "VInd", i))
  DF <- rbind(DF, makeDF(Data, "SpInd", i))

  AddInd <- makeDF(Data, "AddInd", i)
  vDF <- NULL
  if (!is.null(AddInd)) {
    DF <- rbind(DF, AddInd[[1]])
    vDF <- AddInd[[2]]
  }

  DF <- rbind(DF, makeDF(Data, "Rec", i))

  DF$Data[DF$Data == "Cat"] <- paste0("Catch (", Data@Units, ")")
  DF$Data[DF$Data == "Ind"] <- "Index"
  DF$Data[DF$Data == "VInd"] <- "Vulnerable Index"
  DF$Data[DF$Data == "SpInd"] <- "Spawning Index"
  DF$Data[DF$Data == "Rec"] <- "Recruitment"
  DF$Data <- factor(DF$Data, ordered = TRUE,
                    levels=unique(DF$Data))

  # drop NAs
  Range <- suppressWarnings(
    DF %>% dplyr::group_by(Data) %>% dplyr::summarise(Range=range(y, na.rm=T), .groups="keep")
  )
  Groups <- Range %>% dplyr::filter(is.finite(Range)==TRUE)
  DF <- DF %>% dplyr::filter(DF$Data %in% unique(Groups$Data))

  p1 <- ggplot2::ggplot(DF, ggplot2::aes(x=Year, y=y, ymin=dw, ymax=up)) +
    ggplot2::facet_wrap(~Data, scales="free", ncol=2) +
    ggplot2::expand_limits(y=0) +
    ggplot2::geom_ribbon(fill='lightgray') + ggplot2::geom_line(size=1.1) +
    ggplot2::geom_point() +
    ggplot2::labs(x="Year", y="Mean (95% intervals)",
                  title=paste0('Figure ', fignum, '. Time-Series Data')) +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(size=14))


  p1

  list(DF, p1, fignum+1)
}


vuln_addInd_plot <- function(Data, i=1, fignum=1) {
  Year <- y <- dw <- up <- X <- Ind <- value <- key <- NA
  AddInd <- makeDF(Data, "AddInd", i)
  vDF <- NULL
  if (!is.null(AddInd)) {
    vDF <- AddInd[[2]]
  }

  p2 <- NULL
  if (!is.null(vDF)) {
    fignum <- fignum+1
    vDF$Ind <- factor(vDF$Ind)
    nind <- length(levels(vDF$Ind))
    vDF$X <- rep(1:(Data@MaxAge+1), nind)
    p2 <- ggplot2::ggplot(vDF, ggplot2::aes(x=X, y=V, linetype=Ind)) +
      ggplot2::geom_line() +
      ggplot2::expand_limits(y=c(0,1)) +
      ggplot2::labs(x="Age", y="Vulnerability",
                    title=paste0('\n\nFigure ',
                                 fignum, '. Vulnerability-at-age schedules for Additional Indices'),
                    linetype="Additional Index") +
      ggplot2::theme_minimal()

  }

  if (!is.null(p2)) suppressWarnings(plot(p2))
  fignum
}


meanLen_plot <- function(Data, i=1, fignum=1) {
  Year <- y <- dw <- up <- X <- Ind <- value <- key <- NA
  DF <- data.frame(Year=Data@Year, ML=Data@ML[i,], Lc=Data@Lc[i,], Lbar=Data@Lbar[i,])
  p3 <- NULL
  if (!all(is.na(DF[,2:4]))) {
    fignum <- fignum+1
    DF <- tidyr::gather(DF, "key", "value", 2:4)
    DF$key[DF$key == "ML"] <- "Mean length"
    DF$key[DF$key == "Lc"] <- "Modal length (Lc)"
    DF$key[DF$key == "Lbar"] <- "Mean length above Lc"
    DF$key <- factor(DF$key, ordered = TRUE,
                     levels=c("Mean length",
                              "Modal length (Lc)",
                              "Mean length above Lc"))

    p3 <- ggplot2::ggplot(DF, ggplot2::aes(x=Year, y=value, linetype=key)) +
      ggplot2::geom_line(size=1.1) +
      ggplot2::expand_limits(y=0) +
      ggplot2::labs(y="Length", linetype="Legend",
                    title=paste0('\n\nFigure ', fignum, '. Mean Length Time-Series')) +
      ggplot2::theme_minimal()

  }

  if (!is.null(p3)) suppressWarnings(plot(p3))

  fignum
}




caa_plot <- function(Data, i=1, fignum=1) {
  # CAA
  Year <- Freq <- n <- NA
  CAA <- Data@CAA[i,,]
  nyrs <- nrow(CAA); maxage <- ncol(CAA)
  if  (all(is.na(CAA))){
    P2 <- NULL
  } else {
    P2 <- TRUE
    dimnames(CAA) <- list(1:nyrs, 1:maxage)

    df1 <- as.data.frame.table(CAA, stringsAsFactors = FALSE)
    colnames(df1) <- c("Year", "Val", "Freq")
    df1$Val <- as.numeric(df1$Val)

    df1$Year <- as.numeric(df1$Year)
    df1$Year <- Data@Year

    yr.n <- df1 %>% dplyr::group_by(Year) %>% dplyr::summarise(n=sum(Freq))
    yr.ind <- yr.n %>% dplyr::filter(n>0) %>% dplyr::select(Year)

    Years <- Data@Year
    nyears <- length(unique(df1$Year))
    df1$Year_val <- (Years[(length(Years)-nyears+1):length(Years)])

    if (nrow(df1)>0 ) {
      nyears <- length(unique(df1$Year))
      nayears <- df1 %>% group_by(Year) %>% summarize(isna=all(is.na(Freq)))
      nyears <- sum(!nayears$isna)

      nbins <- length(unique(df1$Val))
      tplot <- 16 # total plots per page

      if (nyears > tplot) {
        npages <- ceiling(nyears/tplot)
        ncol <- 4
        nrow <- 4
        nplot <- ncol * nrow
      } else {
        npages <- 1
        nrow <- ceiling(sqrt(nyears))
        ncol <- ceiling(nyears/nrow)
        nplot <- nyears
      }
      pmat <- matrix(1:(nrow*ncol), nrow=nrow, ncol=ncol, byrow=TRUE)
      pmat[pmat >nplot] <- NA

      op <- par(mfrow=c(nrow, ncol), no.readonly = TRUE, mar=c(2,2,2,1), oma=c(4,4,4,0))
      on.exit(par(op))

      yr1 <- 1
      col <- "grey"
      for (pg in 1:npages) {
        if (pg ==1) {
          yrind <- yr.ind$Year[1:(nplot)]
        } else {
          t1 <- nplot * (pg-1) +1
          t2 <- t1+nplot
          yrind <- yr.ind$Year[t1:t2]
        }
        yr1 <- max(yrind) + 1
        dat <- df1 %>% dplyr::filter(Year %in% yrind)
        un.yrs_val <- as.numeric(unique(dat$Year_val))
        un.yrs <- as.numeric(unique(dat$Year))

        if (pg >1 && pg == npages) {
          nplot <- nyears - (npages-1) * tplot
          ncol <- ceiling(sqrt(nplot))
          nrow <- ceiling(nplot/ncol)
          op <- par(mfrow=c(nrow, ncol), no.readonly = TRUE, mar=c(2,2,2,1), oma=c(4,4,4,0))
          on.exit(par(op))
          suppressWarnings(
            pmat <- matrix(1:nplot, nrow=nrow, ncol=ncol, byrow=TRUE))
          pmat[pmat>nplot] <- NA
        }
        for (p in 1:nplot) {
          pdat <- dat %>% dplyr::filter(Year==un.yrs[p])
          if (nrow(pdat) > 0) {
            if (all(is.na(pdat$Freq))) {

            } else{

              if (p %in% pmat[nrow,]) {
                barplot(pdat$Freq, names=round(pdat$Val, 2), axes=FALSE, col=col)
              } else {
                barplot(pdat$Freq, names=FALSE, axes=FALSE, col=col)
              }
              if (p %in% pmat[,1]) axis(side=2)
              if (!p %in% pmat[,1]) axis(side=2, labels=TRUE)
              ncount <- round(sum(pdat$Freq),0)
              title(un.yrs_val[p])
              text(max(pdat$Val), max(pdat$Freq), paste('n = ', ncount),
                   xpd=NA)
            }
          }
          if (p == 1) {
            title(main=paste0('Figure ', fignum, ". Catch-at-Age (Years ", min(un.yrs), " - ", max(un.yrs), ")"),
                  xpd=NA, line=1, outer=TRUE)
            fignum <- fignum + 1
          }
        }
        mtext(side=1, outer=TRUE, "Age", line=2, cex=1.1)
        mtext(side=2, outer=TRUE, "Frequency", line=2, cex=1.1)

      }
    }
  }

  fignum
}


cal_plot <- function(Data, i=1, fignum=1) {
  Year <- Freq <- n <- NA
  CAL <- Data@CAL[i,,]
  if (all(is.na(CAL))) {
    P3 <- NULL
  } else {
    P3 <- TRUE
    nyrs <- nrow(CAL); nbins <- length(Data@CAL_bins) - 1
    By <- Data@CAL_bins[2] - Data@CAL_bins[1]
    BinsMid <- seq(Data@CAL_bins[1] + 0.5*By, by=By,length.out = nbins)
    dimnames(CAL) <- list(1:nyrs, BinsMid)

    df1 <- as.data.frame.table(CAL, stringsAsFactors = FALSE)
    colnames(df1) <- c("Year", "Val", "Freq")
    df1$Val <- as.numeric(df1$Val)

    df1$Year <- as.numeric(df1$Year)
    df1$Year <- Data@Year

    yr.n <- df1 %>% dplyr::group_by(Year) %>% dplyr::summarise(n=sum(Freq))
    yr.ind <- yr.n %>% dplyr::filter(n>0) %>% dplyr::select(Year)

    Years <- Data@Year
    nyears <- length(unique(df1$Year))
    df1$Year_val <- (Years[(length(Years)-nyears+1):length(Years)])

    if (nrow(df1) > 0) {
      nyears <- length(unique(df1$Year))
      nayears <- df1 %>% group_by(Year) %>% summarize(isna=all(is.na(Freq)))
      nyears <- sum(!nayears$isna)

      nbins <- length(unique(df1$Val))
      tplot <- 16 # total plots per page
      if (nyears > tplot) {
        npages <- ceiling(nyears/tplot)
        ncol <- 4
        nrow <- 4
        nplot <- ncol * nrow
      } else {
        npages <- 1
        nrow <- ceiling(sqrt(nyears))
        ncol <- ceiling(nyears/nrow)
        nplot <- nyears
      }
      pmat <- matrix(1:(ncol*nrow), nrow=nrow, ncol=ncol, byrow=TRUE)
      pmat[pmat>nplot] <- NA

      op <- par(mfrow=c(nrow, ncol), no.readonly = TRUE, mar=c(2,2,2,1), oma=c(4,4,4,0))
      on.exit(par(op))

      yr1 <- 1
      col <- "grey"
      for (pg in 1:npages) {
        if (pg ==1) {
          yrind <- yr.ind$Year[1:(nplot)]
        } else {
          t1 <- nplot * (pg-1) +1
          t2 <- t1+nplot
          yrind <- yr.ind$Year[t1:t2]
        }

        yr1 <- max(yrind) + 1
        dat <- df1 %>% dplyr::filter(Year %in% yrind)
        un.yrs_val <- as.numeric(unique(dat$Year_val))
        un.yrs <- as.numeric(unique(dat$Year))

        if (pg >1 && pg == npages) {
          nplot <- nyears - (npages-1) * tplot
          ncol <- ceiling(sqrt(nplot))
          nrow <- ceiling(nplot/ncol)
          op <- par(mfrow=c(nrow, ncol), no.readonly = TRUE, mar=c(2,2,2,1), oma=c(4,4,4,0))
          on.exit(par(op))
          suppressWarnings(
            pmat <- matrix(1:(ncol*nrow), nrow=nrow, ncol=ncol, byrow=TRUE)
          )
          pmat[pmat>nplot] <- NA
        }
        for (p in 1:nplot) {
          pdat <- dat %>% dplyr::filter(Year==un.yrs[p])
          if (nrow(pdat) > 0) {
            if (all(is.na(pdat$Freq))) {

            } else{
              if (p %in% pmat[nrow,]) {
                barplot(pdat$Freq, names.arg=round(pdat$Val, 2), axes=FALSE, col=col, las=2)
              } else {
                barplot(pdat$Freq, names.arg=FALSE, axes=FALSE, col=col)
              }
              if (p %in% pmat[,1]) axis(side=2)
              if (!p %in% pmat[,1]) axis(side=2, labels=TRUE)
              ncount <- round(sum(pdat$Freq),0)
              title(un.yrs_val[p])
              text(length(unique(df1$Val)), max(pdat$Freq), paste('n = ', ncount), xpd=NA)
            }

          }
          if (p == 1) {
            title(main=paste0('Figure ', fignum, ". Catch-at-Length (Years ", min(un.yrs), " - ", max(un.yrs), ")"),
                  xpd=NA, line=1, outer=TRUE)
            fignum <- fignum + 1
          }
        }
        mtext(side=1, outer=TRUE, "Length", line=2, cex=1.1)
        mtext(side=2, outer=TRUE, "Frequency", line=2, cex=1.1)

      }
    }
  }

  fignum
}

ref_plots <- function(Data, i=1, n=20000, fignum=1) {
  Var <- val <- ..scaled.. <- Text <- NA
  slots <- c('Dep', 'Abun', 'SpAbun', 'FMSY_M', 'BMSY_B0', 'Cref', 'Bref', 'Iref',
             't', "AvC", 'Dt', 'Ref')

  lout <- list()
  for (x in seq_along(slots)) {
    sl <- slots[x]
    mean <- slot(Data, sl)[i]
    if (sl %in% c("t", "Ref")) {
      cv <- NA
    } else {
      cv <- slot(Data, paste0("CV_",sl))[i]
    }


    if (is.na(cv) | length(cv)<1) {
      vals <- rep(mean,n)
    } else {
      if (is.na(mean)) {
        vals <- NA
      } else {
        if (mean < 0) {
          vals <- -trlnorm(n, -mean, cv)
        } else {
          vals <- trlnorm(n, mean, cv)
        }
      }

    }
    lout[[x]] <- data.frame(Var=sl, val=vals, mean=mean, cv=cv)
  }

  plist <- do.call("rbind", lout)
  df <- plist %>% group_by(Var) %>% dplyr::distinct(mean, cv)
  df$x <- df$mean
  # df$mean[df$mean>0.01] <-round(df$mean[df$mean>0.01],)
  # df$mean[df$mean < -0.01] <-round(df$mean[df$mean < -0.01],2)
  df$cv <- round(df$cv,2)
  lab <- sprintf("mu ==%G", df$mean)
  lab2 <- sprintf("CV ==%G", df$cv)

  # plist <- dplyr::distinct(plist)

  addText <- FALSE
  textdf <- plist %>% filter(is.na(val))
  if (nrow(textdf) > 0) {
    textdf$Text <- "No values"
    textdf <- dplyr::distinct(textdf)
    addText <- TRUE
  }

  if (!all(is.na(plist$mean))) {
    p1 <- ggplot2::ggplot(plist, ggplot2::aes(x=val, y=..scaled..)) +
      ggplot2::geom_density(show.legend = F, fill="lightgray") +
      ggplot2::facet_wrap(~Var, scales="free") +
      ggplot2::theme_minimal() + ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                                                axis.text.y = ggplot2::element_blank(),
                                                axis.title.x=ggplot2::element_blank()) +
      ggplot2::geom_text(data=df, ggplot2::aes(x=x, y=1.3), parse=TRUE, label=lab) +
      ggplot2::geom_text(data=df, ggplot2::aes(x=x, y=1.1), parse=TRUE, label=lab2) +

      ggplot2::labs(title=paste0('Figure ', fignum, '. Density plots of Reference parameters')) +
      ggplot2::expand_limits(y=1.4) +
      ggplot2::theme(strip.text = ggplot2::element_text(size=14))
    if (addText) p1 <- p1 + ggplot2::geom_text(data=textdf, ggplot2::aes(x=0, y=3, label=Text) )

    suppressWarnings(plot(p1))
  }


  fignum


}



#' Apply Management Procedures to an object of class Data
#'
#' @param Data An object of class Data
#' @param MPs Name(s) of the MPs to run
#' @param reps Number of samples
#' @param nsims Optional. Number of simulations.
#' @param silent Logical. Should messages be suppressed?
#' @param parallel Logical. Whether to run MPs in parallel. Can be a vector of length(MPs)
#'
#' @return A list with the first element a list of management recommendations,
#' and the second the updated Data object
#' @export
#'
applyMP <- function(Data, MPs = NA, reps = 100, nsims=NA, silent=FALSE, parallel = snowfall::sfIsRunning()) {
  if (!methods::is(Data, "Data")) stop("First argument must be object of class 'Data'", call.=FALSE)
  Dataout <- Data
  if (is.na(nsims)) nsims <- nrow(Data@Cat)
  nMPs <- length(MPs)

  if (.hasSlot(Data, "nareas")) {
    nareas <- Data@nareas
  } else {
    nareas <- 2
  }
  returnList <- list() # a list nMPs long containing MPs recommendations
  recList <- list() # a list containing nsim recommendations from a single MP
  TACout <- array(NA, dim=c(nMPs, reps, nsims))

  # refMPs <- avail('MP', 'MSEtool', msg=FALSE)
  refMPs <- c("FMSYref", "FMSYref50", "FMSYref75", "NFref")#  refMPs[grepl('ref', refMPs)]
  
  if (length(parallel) < length(MPs)) {
    if (length(parallel) > 1) {
      stop("length(parallel) must be equal to length(MPs)")
    } else {
      parallel <- rep(parallel, length(MPs))
    }
  }
  
  runParallel <- snowfall::sfIsRunning()
  # if (nMPs < 8 & nsims < 8) runParallel <- FALSE

  if (!silent)
    message_info('Attempting to run ', length(MPs), ' MPs:')

  for (mp in 1:nMPs) {
    if (!silent)  message(MPs[mp])
    
    if (runParallel && parallel[mp]) {
      temp <- try(snowfall::sfLapply(1:nsims, MPs[mp], Data = Data, reps = reps), silent=TRUE)
    } else {
      temp <- try(lapply(1:nsims, MPs[mp], Data = Data, reps = reps), silent=TRUE)
    }
   
    if (methods::is(temp, 'try-error')) {
      warning("Method ", MPs[mp], " failed with error: ", temp)
      returnList[[mp]] <- temp
    } else {
      slots <- slotNames(temp[[1]])
      for (X in slots) { # sequence along recommendation slots
        if (X == "Misc") { # convert to a list nsim by nareas
          rec <- lapply(temp, slot, name=X)
        } else {
          rec <- do.call("cbind", lapply(temp, slot, name=X)) # unlist(lapply(temp, slot, name=X))
        }
        if (X == "Spatial") { # convert to a matrix nsim by nareas
          rec <- matrix(rec, nareas, nsims, byrow=FALSE)
        }
        recList[[X]] <- rec
        for (x in 1:nsims) Dataout@Misc[[x]] <- recList$Misc[[x]]
        recList$Misc <- NULL

        if (MPs[mp] %in% refMPs) {
          recList$type <- 'reference'
        } else {
          recList$type <- 'mp'
        }
      }
      if (length(recList$TAC)>0)  TACout[mp,,] <- recList$TAC
      returnList[[mp]] <- recList
      if (!silent && any(apply(is.na(recList$TAC), 2, sum) > rep(0.5 * reps, nsims)))
        message("Method ", MPs[mp], " produced greater than 50% NA values")
    }
  }

  Dataout@TAC <- TACout
  Dataout@MPs <- MPs
  nms <- names(Data@Misc)
  nms <- nms[nchar(nms)>0]
  Dataout@Misc[nms] <- Data@Misc[nms]

  list(returnList, Dataout)
}


SubData_sim <- function(x, Data) {
  if(!methods::is(Data, 'Data')) stop('Object must be class `Data`')
  subdata <- new("Data")
  sltType <- getSlots('Data')
  slts <- slotNames(Data)
  
  for (i in seq_along(slts)) {
    if (sltType[i] == "character")
      slot(subdata, slts[i]) <- slot(Data, slts[i])
    if (sltType[i] == "numeric")
      slot(subdata, slts[i]) <- slot(Data, slts[i])
    if (sltType[i] == "vector")
      if (slts[i]=='Year') {
        slot(subdata, slts[i]) <- slot(Data, slts[i])
      } else {
        slot(subdata, slts[i]) <- slot(Data, slts[i])[x]  
      }
      
    if (sltType[i] == "matrix")
      slot(subdata, slts[i]) <- matrix(slot(Data, slts[i])[x,], nrow=1)
    if (sltType[i] == "array") {
      tt <- slot(Data, slts[i])
      dd <- dim(tt)
      ll <- length(dd)
      if (ll ==2) {
        slot(subdata, slts[i]) <- array(slot(Data, slts[i])[x,], dim=c(1,dd[2:length(dd)]))
      }
      if (ll ==3) {
        slot(subdata, slts[i]) <- array(slot(Data, slts[i])[x,,], dim=c(1,dd[2:length(dd)]))
      }
      
    }
    if (sltType[i] == "list") {
      tt <- slot(Data, slts[i])
      newlist <- list()
      if (length(tt)>0) {
        if (slts[i] == 'Misc') {
          np <- length(tt)
          nf <- length(tt[[1]])
          if (nf>0) {
            # from MMSE
            n_sim <- length(tt[[1]][[1]])
            for (p in 1:np) {
              newlist[[p]] <- list()
              for (f in 1:nf) {
                newlist[[p]][[f]] <- list()  
                newlist[[p]][[f]][[1]] <- tt[[1]][[1]][[x]]
              }
            }
          } else {
            # from MSE
            # not currently implemented
            
          }
        }
      }
      slot(subdata, slts[i]) <- newlist
    }
    if (sltType[i] == "data.frame") {
      slot(subdata, slts[i]) <- slot(Data, slts[i])[x,]
    }
  }
  subdata
}

MP_wrapper <- function(x, Data, MP, ...) {
  subdat <- SubData_sim(x, Data)
  fun <- get(MP)
  fun(1, subdat, ...)
}


lag_slot <- function(sl, lag, n_yr, Data) {
  lag_ind <- 1:(n_yr-lag)
  val <- slot(Data, sl)
  dd <- dim(val)
  if (length(dd)==2) {
    # matrix
    val <- val[,lag_ind, drop=FALSE]
  }
  if (length(dd)==3) {
    # array
    ind <- ifelse(sl%in% c('AddInd', 'CV_AddInd'), 3, 2)
    if (ind ==2) {
      val <- val[,lag_ind,, drop=FALSE]
    }
    if (ind ==3) {
      val <- val[,,lag_ind, drop=FALSE]
    }
  }
  slot(Data, sl) <- val
  
  Data
}

#' Lag the time-series slots in a `Data` object by a specified number of time-steps
#' 
#' 
#'
#' @param Data An object of class `Data`
#' @param Data_Lag Either a numeric vector of length 1 with a positive number 
#' specifying the number of time-steps to lag all time-series data, or a named 
#' list with numeric values (length 1). See details for more information.
#' @param msg Logical. Display the messages?
#'
#' @return An object of class `Data` with time-series slots lagged. 
#' @export
#' 
#' @details 
#' By default, all simulated data in the forward projections are provided up to the
#' previous time-step. That is, in projection year `t`, the simulated data are up to
#' and including `t-1`.
#' This function will lag the time-series values by the specified value. For example, 
#' `Data_Lag=5` will mean in projection time-step `t` the data will be up to and 
#' including `t-6`. 
#' 
#' *Note*: The `Data@Year` slot is *not* lagged by this function. 
#' Many built-in MPs use the length of `Data@Year` to determine the number of
#' years of data for smoothing over recent years etc. This may not be appropriate
#' so check the MP is behaving as you expect if you use `Lag_Data`.
#'
#' @examples
#' # Lag all time-series slots by 2 time-steps (usually years)
#' Data <- Example_datafile
#' Lagged_1 <- Lag_Data(Data, 2)
#' length(Data@Year)
#' length(Lagged_1@Year)
#' length(Data@Cat[1,])
#' length(Lagged_1@Cat[1,])
#' length(Data@Ind[1,])
#' length(Lagged_1@Ind[1,])
#' 
#' # Lag CAA by 5 and Ind by 3 time-steps
#' Lagged_2 <- Lag_Data(Data, Data_Lag=list(CAA=5, Ind=3))
#' length(Lagged_2@Year)
#' length(Lagged_2@Cat[1,])
#' dim(Data@CAA[1,,])
#' dim(Lagged_2@CAA[1,,])
#' 
#' length(Data@Ind[1,])
#' length(Lagged_2@Ind[1,])
Lag_Data <- function(Data, Data_Lag=0, msg=FALSE) {
  if(!inherits(Data, 'Data'))
    stop('Object must be class `Data`')
  n_yr <- length(Data@Year)
  
  # Data slots with a year index
  data_slots <- slotNames('Data')
  slot_vals <- sapply(data_slots, function(x) slot(Data, x))
  slot_dims <- lapply(slot_vals, dim)
  slot_yr <- slot_vals[grepl(n_yr, slot_dims)]
  slot_yr_nms <- names(slot_yr)
  
  if (inherits(Data_Lag, 'list')) {
    slots <- names(Data_Lag)
  } else {
    slots <- slotNames('Data')[grepl(n_yr, slot_dims)]
    if (!inherits(Data_Lag, 'numeric'))
      stop('`Data_Lag` must be numeric of length 1 or a named list.')
    List <- vector('list', length(slots))
    names(List) <- slot_yr_nms
    List[] <- Data_Lag
    Data_Lag <- List
  }
  
  slot_dims <- slot_dims[grepl(n_yr, slot_dims)]
  # check if slots in Data_Lag list have data
  not_data <- !slots %in% slot_yr_nms
  if (sum(not_data)) {
    if (msg)
      message_info('Data lag specified for slots:',
                   paste(slots[not_data], collapse=', '),
                   'but these slots do not have any data. Ignoring.')
    
    slots <- slots[!not_data]
  }
  
  for (sl in slots) {
    if (Data_Lag[[sl]]<0 & msg) 
        message_info('Data lag specified for slot:', sl, 'is negative. Ignoring.')
    
    if (length(Data_Lag[[sl]])>1 & msg) 
      message_info('`Data_Lag` is numeric vector > length 1. Only using the first value.')
    
    if (Data_Lag[[sl]]>0) {
      Data <- lag_slot(sl, Data_Lag[[sl]], n_yr, Data)
    }
  }
  Data
}

