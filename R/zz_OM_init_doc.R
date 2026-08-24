#' Copy example OM XL and OM Documentation
#'
#' @param dir the file path to copy the files to.
#' @export
#'
#' @examples
#' \dontrun{
#' OMexample()
#' }
OMexample <- function(dir=getwd()) {
  fromRMD <- system.file("Example_Chile_Hake.Rmd", package="MSEtool")
  tt <- file.copy(fromRMD, dir, overwrite = TRUE)
  fromXL <- system.file("Example_Chile_hake.xlsx", package="MSEtool")
  tt <- file.copy(fromXL, dir, overwrite = TRUE)
}

#' Initialize Operating Model
#'
#' Generates an Excel spreadsheet and a source.rmd file in the current working directory for
#' specifying and documenting a MSEtool Operating Model.
#'
#' @param name The name of the Excel and source.rmd file to be created in the working directory (character).
#' Use 'example' for a populated example OM XL and documentation file.
#' @param ... Optional MSEtool objects to use as templates: OM, Stock, Fleet, Obs, or Imp objects
#' @param files What files should be created: 'xlsx', 'rmd', or c('xlsx', 'rmd') (default: both)
#' to use as templates for the Operating Model.
#' @param dir Optional file path to create the xlsx and rmd files. Default is `getwd()`
#' @param overwrite Logical. Should files be overwritten if they already exist?
#'
# #' @templateVar url creating-a-new-operating-model
# #' @templateVar ref initialize-a-new-om
# #' @template userguide_link
#'
#' @return name.xlsx and name.rmd files are created in the working directory.
#' @export
#' @author A. Hordyk
#'
#' @examples
#' \dontrun{
#' # Create an Excel OM template and rmd file called 'myOM.xlsx' and 'myOM.rmd':
#' OMinit('myOM')
#'
#' # Create an Excel OM template and text file called 'myOM.rmd' and 'myOM.rmd', using
#' # another OM as a template:
#' OMinit('myOM', myOM)
#'
#' # Create an Excel OM template and text file called 'myOM.rmd' and 'myOM.rmd', using
#' # the Stock object 'Herring' as a template:
#' OMinit('myOM', Herring)
#'
#' # Create an Excel OM template and text file called 'myOM.rmd' and 'myOM.rmd', using
#' # the Stock object 'Herring', and Obs object 'Generic_obs' as templates:
#' OMinit('myOM', Herring, Generic_obs)
#' }
#'
OMinit <- function(name=NULL, ..., files=c('xlsx', 'rmd'), dir=NULL, overwrite=FALSE) {
  files <- match.arg(files, several.ok = TRUE)

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package \"openxlsx\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if(is.null(dir)) dir <- getwd()
  if (is.null(name)) stop("Require OM name", call.=FALSE)

  if (tolower(name) == 'example') {
    OMexample(dir)
    return(message("Creating Example Files in ", dir))
  }
  if (!methods::is(name, 'character')) stop("name must be text", call.=FALSE)

  ## Create Folders ####
  if(!dir.exists(file.path(dir,'data'))) dir.create(file.path(dir,'data'))
  if(!dir.exists(file.path(dir,'docs'))) dir.create(file.path(dir,'docs'))
  if(!dir.exists(file.path(dir,'images'))) dir.create(file.path(dir,'images'))
  if(!dir.exists(file.path(dir,'robustness'))) dir.create(file.path(dir,'robustness'))

  ## Write Excel skeleton ####
  if (nchar(tools::file_ext(name)) == 0) {
    nameNoExt <- name
    name <- paste0(name, ".xlsx")
  } else {
    ext <- tools::file_ext(name)
    if (!ext %in% c("xlsx", "xls")) stop("File extension must be 'xlsx' or 'xls'", call.=FALSE)
    nameNoExt <- tools::file_path_sans_ext(name)
  }

  InTemplates <- list(...)
  ObTemplates <- list()
  useTemp <- FALSE
  if (length(InTemplates) >0) {
    inclasses <- unlist(lapply(InTemplates, class))
    if (!is.null(inclasses)) {
      # check if zip application exists
      # chck <- Sys.which("zip") # requires 'zip.exe' on file path
      chck <- Sys.getenv("R_ZIPCMD", "zip")

      if (!'zip' %in% chck) {
        message('zip application is required for templates. If a zip application is installed on your machine you may need to add it to the path. Try:')
        message('path <- Sys.getenv("PATH")')
        message('Sys.setenv("PATH" = paste(path, "path_to_zip.exe", sep = ";"))')
        stop("Can't use templates without zip application. You may need to install Rtools to use templates", call.=FALSE)
      }
    }


    for (x in seq_along(inclasses)) {
      if (inclasses[x] == 'character') {
        InTemplates[[x]] <- get(InTemplates[[x]])
        inclasses[x] <- class(InTemplates[[x]])
      }
      if (!inclasses[x] %in% c("Stock", "Fleet", "Obs", "Imp", "OM")) stop(InTemplates[[x]], " is not a valid MSEtool object")
    }
    isOM <- which(inclasses == "OM")
    if (length(isOM)>0) {
      message("\nUsing OM Template")
      ObTemplates$Stock <- SubOM(InTemplates[[isOM]], "Stock")
      if (is.na(ObTemplates$Stock@Name) || nchar(ObTemplates$Stock@Name)==0) ObTemplates$Stock@Name <- InTemplates[[isOM]]@Name
      ObTemplates$Fleet <- SubOM(InTemplates[[isOM]], "Fleet")
      if (is.na(ObTemplates$Fleet@Name) || nchar(ObTemplates$Fleet@Name)==0) ObTemplates$Fleet@Name <- InTemplates[[isOM]]@Name
      ObTemplates$Obs <- SubOM(InTemplates[[isOM]], "Obs")
      if (is.na(ObTemplates$Obs@Name) || nchar(ObTemplates$Obs@Name)==0) ObTemplates$Obs@Name <- InTemplates[[isOM]]@Name
      ObTemplates$Imp <- SubOM(InTemplates[[isOM]], "Imp")
      if (is.na(ObTemplates$Imp@Name) || nchar(ObTemplates$Imp@Name)==0) ObTemplates$Imp@Name <- InTemplates[[isOM]]@Name
      useTemp <- TRUE
    } else {
      for (x in seq_along(inclasses)) {
        if (inclasses[x] == 'Stock') ObTemplates$Stock <- InTemplates[[x]]
        if (inclasses[x] == 'Fleet') ObTemplates$Fleet <- InTemplates[[x]]
        if (inclasses[x] == 'Obs') ObTemplates$Obs <- InTemplates[[x]]
        if (inclasses[x] == 'Imp') ObTemplates$Imp <- InTemplates[[x]]
      }
      nm <- names(ObTemplates)
      message("\n\nUsing Object Templates:")
      useTemp <- TRUE
      for (X in nm) {
        message(ObTemplates[[X]]@Name)
      }
    }
  }

  if ('xlsx' %in% files) {

    # Copy xlsx file over to working directory
    # Copy the Excel File ####
    message("Creating ", name, " in ", dir)
    path <- system.file("OM.xlsx", package = "MSEtool")
    pathout <- gsub("OM.xlsx", name, path)
    pathout <- gsub(dirname(pathout), dir, pathout)

    # Check if file exists
    exist <- file.exists(pathout)
    if (exist & !overwrite) stop(name, " already exists in working directory. Use 'overwrite=TRUE' to overwrite", call.=FALSE)
    copy <- file.copy(path, pathout, overwrite = overwrite)
    if (!copy) stop("Excel file not copied from ", path)

    # loop through slot values if Obj template provided
    if (useTemp) {
      wb <- openxlsx::loadWorkbook(file.path(dir, name))
      names <- c("Stock", "Fleet", "Obs", "Imp")
      for (objname in names) {
        if (!is.null(ObTemplates[objname])) {
          obj <- ObTemplates[objname][[1]]
          slots <- slotNames(obj)
          # ignore grad slots
          slots <- slots[!grepl("grad", slots)]
          shtdata <- openxlsx::read.xlsx(wb, objname)
          for (sl in seq_along(slots)) {
            row <- match(slots[sl], shtdata[,1])
            val <- slot(obj, slots[sl])
            ln <- length(val)
            if (ln >0 && !is.na(val)) {
              df <- data.frame(t(val))
              openxlsx::writeData(wb, sheet = objname, x = df,
                                  startCol = 2, startRow = row+1,
                                  colNames = FALSE, rowNames = FALSE,
                                  withFilter = FALSE,
                                  keepNA = FALSE)
            }
          }
          # openxlsx::setColWidths(wb, sheet = objname, cols = 1, widths = 'auto')
        }
      }

      # OM tab not currently updated
      openxlsx::saveWorkbook(wb, file.path(dir,name), overwrite = TRUE)
    }
  }

  if ('rmd' %in% files) {
    # RMD File ####
    rmdname <- paste0(nameNoExt, '.rmd')
    message("Creating ", rmdname, " in ", dir)
    path <- system.file("OM.rmd", package = "MSEtool")
    if (nchar(path) <1) stop("OM.rmd not found in MSEtool package")
    pathout <- gsub("OM.rmd", rmdname, path)
    pathout <- gsub(dirname(pathout), dir, pathout)

    # Check if file exists
    exist <- file.exists(pathout)
    if (exist & !overwrite) stop(rmdname, " alread exists in ", dir, ". Use 'overwrite=TRUE' to overwrite", call.=FALSE)
    copy <- file.copy(path, pathout, overwrite = overwrite)
    if (!copy) stop("Rmd file not copied from ", path)

    # Copy over templates - if used ####
    if (length(ObTemplates)>0) {
      names <- c("Stock", "Fleet", "Obs", "Imp")
      textIn <- readLines(file.path(dir,rmdname))
      for (objname in names) {
        if (!is.null(ObTemplates[objname])) {
          obj <- ObTemplates[objname][[1]]
          slots <- slotNames(obj)

          for (sl in slots) {
            if (!sl %in% c("Name", "Source")) {
              lineno <- grep(paste0("^## ", sl, "$"), textIn)
              textIn[lineno+1] <- paste("Borrowed from:", obj@Name)
            }

          }
        }
      }
      writeLines(textIn, con = file.path(dir, rmdname), sep = "\n", useBytes = FALSE)
    }
  }

}


# #' Load OM from Excel file
# #' 
# #' Imports an OM from a correctly formatted Excel file. Create the Excel spreadsheet template
# #' using `OMinit` and document each slot in the corresponding text file.
# #' 
# #' An error message will alert if any slots are missing values, or if the Excel file is missing
# #' the required tabs.
# #' 
# #' @param name Name of the OM Excel file. Provide full file path if not in current directory.
# #' @param cpars An optional list of custom parameters (single parameters are a vector nsim
# #' long, time series are a matrix nsim rows by nyears columns)
# #' @param msg Should messages be printed?
# #' 
# #' @return An object of class OM
# #' @export
# #' @author A. Hordyk
# #' 
# #' @examples
# #' \dontrun{
# #' OMinit('myOM', templates=list(Stock='Herring', Fleet='Generic_Fleet', Obs='Generic_Obs',
# #' Imp='Perfect_Imp'), overwrite=TRUE)
# #' myOM <- XL2OM('myOM.xlsx')
# #' 
# #' }
# XL2OM <- function(name=NULL, cpars=NULL, msg=TRUE) {
#   if (!requireNamespace("readxl", quietly = TRUE)) {
#     stop("Package \"readxl\" needed for this function to work. Please install it.",
#          call. = FALSE)
#   }
#   # Load the Excel File ####
#   if (is.null(name)) {
#     fls <- list.files(pattern=".xlsx", ignore.case = TRUE)
#     fls <- fls[!grepl('~', fls)]
#     if (length(fls) == 0) stop('Name not provided and no .xlsx files found.', call.=FALSE)
#     if (length(fls) > 1) stop("Name not provided and multiple .xlsx files found", call.=FALSE)
#     name <- fls
#   }
# 
#   if (!methods::is(name, 'character')) stop("file name must be provided", call.=FALSE)
# 
#   if (nchar(tools::file_ext(name)) == 0) {
#     xl.fname1 <- paste0(name, ".xlsx")
#     xl.fname2 <- paste0(name, ".xls")
#     fls <- file.exists(c(xl.fname1, xl.fname2))
#     if (sum(fls) == 0) stop(xl.fname1, " or ", xl.fname2, " not found")
#     if (sum(fls) > 1) stop(name, " found with multiple extensions. Specify file extension.", call.=FALSE)
#     name <- c(xl.fname1, xl.fname2)[fls]
#   }
#   if (!file.exists(name)) stop(name, " not found", call.=FALSE)
#   message("Reading ", name)
#   sheetnames <- readxl::excel_sheets(name)  # names of the sheets
#   reqnames <- c("OM", "Stock", "Fleet", "Obs", "Imp")
#   ind <- which(!reqnames%in% sheetnames)
#   if (length(ind)>0) stop("Sheets: ", paste(reqnames[ind], ""), "not found in ", name, call.=FALSE)
# 
#   count <- 1
#   tempObj <- vector("list", 4)
#   for (obj in c("Stock", "Fleet", "Obs", "Imp")) {
#     sht <- suppressMessages(as.data.frame(readxl::read_excel(name, sheet = obj, col_names = FALSE)))
#     rows <- sht[,1]
#     rows <- rows[!rows == "Slot"]
#     rows <- rows[!is.na(rows)]
#     ind <- which(!rows %in% slotNames(obj))
#     if (length(ind)>0) {
#       warning(paste(rows[ind], ""), "are not valid slots in object class ", obj)
#     }
# 
#     if (all(dim(sht) == 0)) stop("Nothing found in sheet: ", obj, call.=FALSE)
#     tmpfile <- tempfile(fileext=".csv")
#     writeCSV2(inobj = sht, tmpfile, objtype = obj)
#     if (ncol(sht)<2) {
#       unlink(tmpfile)
#       warning("No parameter values found in Sheet: ", obj, ". Using defaults", call.=FALSE)
#       tempObj[[count]] <- new(obj)
#     } else {
#       tempObj[[count]] <- new(obj, tmpfile)
#     }
#     unlink(tmpfile)
#     count <- count + 1
#   }
# 
#   # Operating Model
#   OM <- new("OM", Stock = tempObj[[1]], Fleet = tempObj[[2]],
#             Obs = tempObj[[3]], Imp=tempObj[[4]])
# 
#   # Read in the OM sheet
#   sht <- suppressMessages(as.data.frame(readxl::read_excel(name, sheet = "OM", col_names = FALSE)))
#   dat <- sht # sht[,1:2]
#   dat <- dat[which(dat[,1] != "Slot"),]
#   # if (ncol(sht)>2) warning("More than two columns found in Sheet OM. Values in columns C+ are ignored")
#   if (ncol(sht)<2) {
#     message("No values found for OM slots in Sheet OM. Using defaults")
#   } else {
#     for (xx in 1:nrow(dat)) {
#       val <- dat[xx, 2:ncol(dat)]
#       if (length(val)) {
#         if (!dat[xx,1] %in% c("Name", "Agency", "Region", "Sponsor")) {
#           options(warn=-1)
#           val <- as.numeric(val)
#           options(warn=1)
#           val <- val[!is.na(val)]
#           if (.hasSlot(OM, dat[xx,1])) slot(OM, dat[xx, 1]) <- val
#         } else  {
#           val <- val[!is.na(val)]
#           if (.hasSlot(OM, dat[xx,1])) slot(OM, dat[xx, 1]) <- val
#         }
# 
#       } else{
#         message("No value found for OM slot ", dat[xx,1], ". Using default: ", slot(OM, dat[xx, 1]))
#       }
#     }
#   }
# 
#   if (!is.null(cpars)) {
#     if (methods::is(cpars,"list")) {
#       OM@cpars <- cpars
#     } else {
#       stop("'cpars' must be a list", call.=FALSE)
#     }
#   }
#   # tt <- ChkObj(OM, FALSE)
#   if (msg) {
#     message('OM successfully imported\n')
#     message("Document OM slots in .rmd file (probably ", tools::file_path_sans_ext(name), ".rmd),
#   and run 'OMdoc' if OM parameter values have changed." )
#   }
# 
#   OM
# }
# 
# 



#' Generate OM Documentation Report (deprecated)
#'
#' `OMdoc` has been deprecated and is no longer available. Install an earlier version of
#' MSEtool (e.g. `remotes::install_version('MSEtool', version = '3.7.0')`) to use it.
#'
#' @param OM An object of class 'OM' or the name of an OM xlsx file
#' @param rmd.source Optional. Name of the source.rmd file corresponding to the 'OM'. Default assumption
#' is that the file is 'OM@Name.Rmd'
#' @param overwrite Logical. Should existing files be overwritten?
#' @param out.file Optional. Character. Name of the output file. Default is the same as the text file.
#' @param inc.plot Logical. Should the plots be included?
#' @param render Logical. Should the document be compiled? May be useful to turn off if
#' there are problems with compiling the Rmd file.
#' @param output Character. Output file type. Default is 'html_document'. 'pdf_document' is available
#' but may require additional software and have some formatting issues.
#' @param openFile Logical. Should the compiled file be opened in web browser?
#' @param quiet TRUE to suppress printing of the pandoc command line.
#' @param dir Optional file path to read the xlsx and rmd files. Default is `getwd()`
#' @param ... Optional additional named arguments provided to `runMSE`
#'
#' @return Nothing. Always errors.
#' @export
#' @author A. Hordyk
OMdoc <- function(OM=NULL, rmd.source=NULL, overwrite=FALSE, out.file=NULL,
                  inc.plot=TRUE, render=TRUE, output="html_document",
                  openFile=TRUE, quiet=FALSE,
                  dir=NULL, ...) {
  stop("'OMdoc' has been deprecated and is no longer available in this version of MSEtool. ",
       "To use 'OMdoc', install an earlier version of the package, e.g.:\n",
       "remotes::install_version('MSEtool', version = '3.7.0')", call.=FALSE)
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


#' Read in Data object from Excel spreadsheet
#'
#' A function to read in Data object from an Excel spreadsheet
#' with tabs named following specific convention.
#'
#' The Excel spreadsheet must have tabs named with the following convention.
#' For example if \code{stkname} is 'myFish', the Data parameters are in a tab
#' named 'myFishData'.
#'
#' @param fname Name of the Excel spreadsheet file. Must include file
#' extension.
#' @param stkname Name of the Stock.
#' @param fpath Full file path, if file is not in current working directory
#' @param saveCSV Do you also want to the Data parameters to a CSV file?
#' @return A object of class Data
#' @author A. Hordyk
#' @examples
#'
#' \dontrun{
#' OM <- OM_xl(fname='OMTables.xlsx', stkname='myFish')
#' }
#'
#' @export
Data_xl <- function(fname, stkname, fpath = "", saveCSV = FALSE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package \"readxl\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  infile <- paste0(fpath, fname)  # full path and name
  shtname <- readxl::excel_sheets(infile)  # names of the sheets
  # Data
  index <- which(pmatch(shtname, paste0(stkname, "Data")) == 1)
  if (length(index) > 1)  stop("More than one match")
  data <- suppressMessages(readxl::read_excel(infile, sheet = index, col_names = FALSE))
  data <- as.data.frame(data)
  tmpfile <- paste0(fpath, stkname, "Data.csv")
  if (file.exists(tmpfile)) unlink(tmpfile)
  writeCSV(inobj = data, tmpfile, objtype = "Data")
  tmpimp <- new("Data", tmpfile)
  data <- new("Data",tmpfile)
  if (!saveCSV) unlink(tmpfile)
  return(data)

}

# #' Read in feasibility parameters from Excel spreadsheet
# #'
# #' A function to read in feasibility parameters from an Excel spreadsheet with
# #' tabs named following specific convention
# #'
# #' The Excel spreadsheet must have tabs named with the following convention.
# #' For example if \code{stkname} is 'myFish', the tab must be named
# #' 'myFishFease,
# #'
# #' @usage Fease_xl(fname, stkname, fpath = '', saveCSV = FALSE)
# #' @param fname Name of the Excel spreadsheet file. Must include file
# #' extension.
# #' @param stkname Name of the Stock.
# #' @param fpath Full file path, if file is not in current working directory
# #' @param saveCSV Do you also want to save the Stock, Fleet and Observation
# #' parameters to CSV files?
# #' @return A object of class Fease
# #' @author A. Hordyk
# #' @examples
# #'
# #'  \dontrun{
# #'  myFease <- Fease_xl(fname='FeaseTables.xlsx', stkname='myFish')
# #' }
# #'
# #' @export Fease_xl
# Fease_xl <- function(fname, stkname, fpath = "", saveCSV = FALSE) {
#   infile <- paste0(fpath, fname)  # full path and name
#   shtname <- readxl::excel_sheets(infile)  # names of the sheets
#   # Fease
#   feasedat <- readxl::read_excel(infile, sheet = grep(paste0(stkname, "Fease"),
#                                                       shtname), col_names = FALSE)
#   feasedat <- feasedat[, 1:2]
#   tmpfile <- paste0(fpath, stkname, "Fease.csv")
#   if (file.exists(tmpfile))
#     unlink(tmpfile)
#   writeCSV(inobj = feasedat, tmpfile, objtype = "Fease")
#   fease <- new("Fease", tmpfile)
#   if (!saveCSV)
#     unlink(tmpfile)
#
#   fease
# }
#


#' Internal function to write CSVs for objects
#'
#' Used internally in the DLMtool package to write CSV files from an existing
#' DLMtool object
#'
#'
#' @param inobj A object of class Stock, Fleet, Obs, Imp, Data, or OM
#'
#' @param tmpfile The full file path and name for the saved CSV file
#' @param objtype The class corresonding to the \code{inobj}
#' @author A. Hordyk
writeCSV <- function(inobj, tmpfile = NULL, objtype = c("Stock", "Fleet",
                                                        "Obs", "Imp", "Data", "OM")) {
  objtype <- match.arg(objtype)

  for (X in 1:nrow(inobj)) {
    indat <- inobj[X, ]
    index <- which(!is.na(indat))
    index <- 2:max(index)
    if (X == 1)
      write(do.call(paste, c(indat[1], as.list(indat[index]), sep = ",")), tmpfile, 1)
    if (X > 1)
      write(do.call(paste, c(indat[1], as.list(indat[index]), sep = ",")), tmpfile, 1, append = TRUE)
  }


  # tmpobj <- new(objtype)
  # sn <- slotNames(tmpobj)
  # ind <- which(inobj[, 1] %in% sn == FALSE)
  # if (length(ind) > 0) {
  #   message("Input file names don't match slot names for ", objtype, " object")
  #   message("Unknown input name:", inobj[ind, 1])
  #   stop("Check the input file row names")
  # }
  # for (X in seq_along(sn)) {
  #   ind <- match(sn[X], inobj[, 1])
  #   if (!is.na(ind)) {
  #     indat <- inobj[ind, ]
  #     index <- which(!is.na(indat))
  #     index <- 2:max(index)
  #     if (X == 1)
  #       write(do.call(paste, c(sn[X], as.list(indat[index]), sep = ",")),
  #         tmpfile, 1)
  #     if (X > 1)
  #       write(do.call(paste, c(sn[X], as.list(indat[index]), sep = ",")),
  #         tmpfile, 1, append = TRUE)
  #   }
  # }
}


