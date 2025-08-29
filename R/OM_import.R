#' Load OM from Excel file
#' 
#' Imports an OM from a correctly formatted Excel file. Create the Excel spreadsheet template
#' using `OMinit` and document each slot in the corresponding text file.
#' 
#' An error message will alert if any slots are missing values, or if the Excel file is missing
#' the required tabs.
#'
#' @param name Name of the OM Excel file. Provide full file path if not in current directory.
#' @param cpars An optional list of custom parameters (single parameters are a vector nsim 
#' long, time series are a matrix nsim rows by nyears columns)
#' @param msg Should messages be printed?
#'
#' @return An object of class OM
#' @export
#' @author A. Hordyk
#'
#' @examples 
#' \dontrun{
#' OMinit('myOM', templates=list(Stock='Herring', Fleet='Generic_Fleet', Obs='Generic_Obs',
#' Imp='Perfect_Imp'), overwrite=TRUE)
#' myOM <- XL2OM('myOM.xlsx')
#' 
#' }
XL2OM <- function(name=NULL, cpars=NULL, msg=TRUE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package \"readxl\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Load the Excel File ####
  if (is.null(name)) {
    fls <- list.files(pattern=".xlsx", ignore.case = TRUE)
    fls <- fls[!grepl('~', fls)]
    if (length(fls) == 0) stop('Name not provided and no .xlsx files found.', call.=FALSE)
    if (length(fls) > 1) stop("Name not provided and multiple .xlsx files found", call.=FALSE)
    name <- fls
  }
  
  if (!methods::is(name, 'character')) stop("file name must be provided", call.=FALSE)
  
  if (nchar(tools::file_ext(name)) == 0) {
    xl.fname1 <- paste0(name, ".xlsx")
    xl.fname2 <- paste0(name, ".xls")
    fls <- file.exists(c(xl.fname1, xl.fname2))
    if (sum(fls) == 0) stop(xl.fname1, " or ", xl.fname2, " not found")
    if (sum(fls) > 1) stop(name, " found with multiple extensions. Specify file extension.", call.=FALSE)
    name <- c(xl.fname1, xl.fname2)[fls]
  }
  if (!file.exists(name)) stop(name, " not found", call.=FALSE) 
  message("Reading ", name)
  sheetnames <- readxl::excel_sheets(name)  # names of the sheets 
  reqnames <- c("OM", "Stock", "Fleet", "Obs", "Imp") 
  ind <- which(!reqnames%in% sheetnames)
  if (length(ind)>0) stop("Sheets: ", paste(reqnames[ind], ""), "not found in ", name, call.=FALSE)
  
  count <- 1
  tempObj <- vector("list", 4)
  for (obj in c("Stock", "Fleet", "Obs", "Imp")) {
    sht <- suppressMessages(as.data.frame(readxl::read_excel(name, sheet = obj, col_names = FALSE)))
    rows <- sht[,1] 
    rows <- rows[!rows == "Slot"]
    rows <- rows[!is.na(rows)]
    ind <- which(!rows %in% slotNames(obj))
    if (length(ind)>0) {
      warning(paste(rows[ind], ""), "are not valid slots in object class ", obj)
    }
    
    if (all(dim(sht) == 0)) stop("Nothing found in sheet: ", obj, call.=FALSE)
    tmpfile <- tempfile(fileext=".csv")
    writeCSV2(inobj = sht, tmpfile, objtype = obj)
    if (ncol(sht)<2) {
      unlink(tmpfile)
      warning("No parameter values found in Sheet: ", obj, ". Using defaults", call.=FALSE)
      tempObj[[count]] <- new(obj)
    } else {
      tempObj[[count]] <- new(obj, tmpfile)  
    }
    unlink(tmpfile)
    count <- count + 1
  }
  
  # Operating Model
  OM <- new("OM", Stock = tempObj[[1]], Fleet = tempObj[[2]], 
            Obs = tempObj[[3]], Imp=tempObj[[4]])
  
  # Read in the OM sheet
  sht <- suppressMessages(as.data.frame(readxl::read_excel(name, sheet = "OM", col_names = FALSE)))
  dat <- sht # sht[,1:2] 
  dat <- dat[which(dat[,1] != "Slot"),]
  # if (ncol(sht)>2) warning("More than two columns found in Sheet OM. Values in columns C+ are ignored")
  if (ncol(sht)<2) {
    message("No values found for OM slots in Sheet OM. Using defaults")
  } else {
    for (xx in 1:nrow(dat)) {
      val <- dat[xx, 2:ncol(dat)]
      if (length(val)) {
        if (!dat[xx,1] %in% c("Name", "Agency", "Region", "Sponsor")) {
          options(warn=-1)
          val <- as.numeric(val)
          options(warn=1)
          val <- val[!is.na(val)]
          if (.hasSlot(OM, dat[xx,1])) slot(OM, dat[xx, 1]) <- val
        } else  {
          val <- val[!is.na(val)]
          if (.hasSlot(OM, dat[xx,1])) slot(OM, dat[xx, 1]) <- val
        }
        
      } else{
        message("No value found for OM slot ", dat[xx,1], ". Using default: ", slot(OM, dat[xx, 1]))
      }
    }
  }
  
  if (!is.null(cpars)) {
    if (methods::is(cpars,"list")) {
      OM@cpars <- cpars
    } else {
      stop("'cpars' must be a list", call.=FALSE)
    }
  }
  
  OM <- CheckOM(OM)
  
  # tt <- ChkObj(OM, FALSE)
  if (msg) {
    message('OM successfully imported\n')
    message("Document OM slots in .rmd file (probably ", tools::file_path_sans_ext(name), ".rmd),
  and run 'OMdoc' if OM parameter values have changed." )
  }
  
  OM

}



writeCSV2 <- function(inobj, tmpfile = NULL, objtype = c("Stock", "Fleet", 
                                                         "Obs", "Imp", "Data", "OM")) {
  objtype <- match.arg(objtype)
  
  for (X in 1:nrow(inobj)) {
    indat <- inobj[X, ]
    index <- which(!is.na(indat))
    if (length(index) >1) {
      index <- 2:max(index)
      if (X == 1) 
        write(do.call(paste, c(indat[1], as.list(indat[index]), sep = ",")), tmpfile, 1)
      if (X > 1) 
        write(do.call(paste, c(indat[1], as.list(indat[index]), sep = ",")), tmpfile, 1, append = TRUE)    
    } else if (indat[1] != "Slot") {
      write(unlist(indat[1]), tmpfile, 1, append = TRUE)  
    }
    
  }
}
