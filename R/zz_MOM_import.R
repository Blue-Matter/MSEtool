#' Import Stock Object from Excel file
#' 
#' Imports a Stock Object from a correctly formatted Excel file. 
#' 
#' An error message will alert if any slots are missing values, or if the Excel file is missing
#' the required tabs.
#'
#' @param name Name of the OM Excel file. Provide full file path if not in current directory.
#' @param cpars An optional list of custom parameters (single parameters are a vector nsim 
#' long, time series are a matrix nsim rows by nyears columns)
#' @param msg Should messages be printed?
#'
#' @return An object of class Stock
#' @export
#' @author A. Hordyk
#'
XL2Stock <- function(name=NULL, cpars=NULL, msg=TRUE) {
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
  if (msg) message("Reading ", name)
  sheetnames <- readxl::excel_sheets(name)  # names of the sheets 
  reqnames <- "Stock"
  ind <- which(!reqnames%in% sheetnames)
  if (length(ind)>0) stop("Sheets: ", paste(reqnames[ind], ""), "not found in ", name, call.=FALSE)
  
  
  obj <- 'Stock'
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
  
  Stock <- new(obj, tmpfile)  
  Stock
}


#' Import Fleet Object from Excel file
#' 
#' Imports a Fleet Object from a correctly formatted Excel file. 
#' 
#' An error message will alert if any slots are missing values, or if the Excel file is missing
#' the required tabs.
#'
#' @param name Name of the OM Excel file. Provide full file path if not in current directory.
#' @param cpars An optional list of custom parameters (single parameters are a vector nsim 
#' long, time series are a matrix nsim rows by nyears columns)
#' @param msg Should messages be printed?
#'
#' @return An object of class Fleet
#' @export
#' @author A. Hordyk
#'
XL2Fleet <- function(name=NULL, cpars=NULL, msg=TRUE) {
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
  if (msg) message("Reading ", name)
  sheetnames <- readxl::excel_sheets(name)  # names of the sheets 
  reqnames <- "Fleet"
  ind <- which(!reqnames%in% sheetnames)
  if (length(ind)>0) stop("Sheets: ", paste(reqnames[ind], ""), "not found in ", name, call.=FALSE)
  
  
  obj <- 'Fleet'
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
  
  Fleet <- new(obj, tmpfile)  
  Fleet
}
