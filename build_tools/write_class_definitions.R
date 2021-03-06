library(readxl)

# ---- Function to write description ----
Write_Description <- function(class="Stock") {
  In <- readxl::read_xlsx('build_tools/Class_definitions/Class_definitions.xlsx', 
                          sheet=class)
  
  front_text <- c(
    "# This file is auto-generated by build_tools/write_class_definitions.R",
    "# Do not edit by hand",
    "\n"
  )
  text_out <- NULL
  for (i in seq_along(In$Slot)) {
    slot <- In$Slot[i]
    desc <- In$Desc[i]
    text <- paste0("#' @slot ", slot, " ", desc )
    text <- strwrap(text, 80)
    if (length(text)>1) {
      for (l in 2:length(text))
        text[l] <- paste0("#'  ", text[l])
    }
    text_out <- append(text_out, text)
  }
  
  text_out <- append(front_text, text_out)
  fileConn<-file(paste0("man-roxygen/", class, "_template.R"))
  writeLines(text_out, fileConn)
  close(fileConn)
  
}

# ---- Stock Object Description -----
Write_Description('Stock')

# ---- Fleet Object Description -----
Write_Description('Fleet')

# ---- Obs Object Description -----
Write_Description('Obs')

# ---- Imp Object Description -----
Write_Description('Imp')

# ---- MSE Object Description -----
Write_Description('MSE')

