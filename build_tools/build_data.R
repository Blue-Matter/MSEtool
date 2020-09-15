library(usethis)

# ---- Source Object Classes ----
source('R/Class_definitions.r')

# --- Create R file for roxygen code ----
RoxygenFile <- "Data_documentation.r" # name of R script with roxygen 
file.remove(file.path('R/', RoxygenFile)) # delete 
file.create(file.path('R/', RoxygenFile)) # make empty file 

cat("# This file is automatically built by build_tools/build_data.r \n",
    "# Don't edit by hand!\n", 
    "# \n\n", sep="", append=TRUE, 
    file=file.path('R/', RoxygenFile)) 

# ---- Build Stock Objects ----
files <- list.files("build_tools/Objects/Stock", full.names = TRUE)

for (fl in files) {
  temp <- new("Stock", fl)
  name <- strsplit(basename(fl), '.csv')[[1]]
  assign(name, temp)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
  
  cat("#' @rdname ", "Stock-class-objects ", 
      "\n", '"', name, '"', "\n\n",
      sep = "", append = TRUE, file = file.path("R/", RoxygenFile))
  
  
}

ObjectClass <- 'Stock'
cat("#' ", ObjectClass, " class objects",
    "\n#' ",
    "\n#' Example objects of class ", ObjectClass,
    "\n#' ",
    "\n#' @name ", ObjectClass, "-class-objects",
    "\n#' @format ", "NULL",
    "\n#' @examples",
    "\n#' avail(", '"', ObjectClass, '")',
    "\nNULL", "\n\n",
    sep = "", append = TRUE, file = file.path("R/", RoxygenFile))

# ---- Build Fleet Objects ----

files <- list.files("build_tools/Objects/Fleet", full.names = TRUE)

for (fl in files) {
  temp <- new("Fleet", fl)
  name <- strsplit(basename(fl), '.csv')[[1]]
  assign(name, temp)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
  
  cat("#' @rdname ", "Fleet-class-objects ", 
      "\n", '"', name, '"', "\n\n",
      sep = "", append = TRUE, file = file.path("R/", RoxygenFile))
  
  
}

ObjectClass <- 'Fleet'
cat("#' ", ObjectClass, " class objects",
    "\n#' ",
    "\n#' Example objects of class ", ObjectClass,
    "\n#' ",
    "\n#' @name ", ObjectClass, "-class-objects",
    "\n#' @format ", "NULL",
    "\n#' @examples",
    "\n#' avail(", '"', ObjectClass, '")',
    "\nNULL", "\n\n",
    sep = "", append = TRUE, file = file.path("R/", RoxygenFile))

# ---- Build Obs Objects ----
files <- list.files("build_tools/Objects/Obs", full.names = TRUE)
for (fl in files) {
  temp <- new("Obs", fl)
  name <- strsplit(basename(fl), '.csv')[[1]]
  assign(name, temp)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
  
  cat("#' @rdname ", "Obs-class-objects ", 
      "\n", '"', name, '"', "\n\n",
      sep = "", append = TRUE, file = file.path("R/", RoxygenFile))
  
  
}

ObjectClass <- 'Obs'
cat("#' ", ObjectClass, " class objects",
    "\n#' ",
    "\n#' Example objects of class ", ObjectClass,
    "\n#' ",
    "\n#' @name ", ObjectClass, "-class-objects",
    "\n#' @format ", "NULL",
    "\n#' @examples",
    "\n#' avail(", '"', ObjectClass, '")',
    "\nNULL", "\n\n",
    sep = "", append = TRUE, file = file.path("R/", RoxygenFile))


# ---- Build Imp Objects ----
files <- list.files("build_tools/Objects/Imp", full.names = TRUE)
for (fl in files) {
  temp <- new("Obs", fl)
  name <- strsplit(basename(fl), '.csv')[[1]]
  assign(name, temp)
  do.call("use_data", list(as.name(name), overwrite = TRUE))
  
  cat("#' @rdname ", "Imp-class-objects ", 
      "\n", '"', name, '"', "\n\n",
      sep = "", append = TRUE, file = file.path("R/", RoxygenFile))
  
  
}

ObjectClass <- 'Imp'
cat("#' ", ObjectClass, " class objects",
    "\n#' ",
    "\n#' Example objects of class ", ObjectClass,
    "\n#' ",
    "\n#' @name ", ObjectClass, "-class-objects",
    "\n#' @format ", "NULL",
    "\n#' @examples",
    "\n#' avail(", '"', ObjectClass, '")',
    "\nNULL", "\n\n",
    sep = "", append = TRUE, file = file.path("R/", RoxygenFile))

# ---- Build testOM Objects ----
Stock <- new("Stock", "build_tools/Objects/Stock/Albacore.csv")
Fleet <- new("Fleet", "build_tools/Objects/Fleet/Generic_Fleet.csv")
Obs  <- new("Obs", "build_tools/Objects/Obs/Generic_Obs.csv")
Imp <- new("Imp", "build_tools/Objects/Imp/Perfect_Imp.csv")

testOM <- new('OM', Stock, Fleet, Obs, Imp, nsim=3)
usethis::use_data(testOM, overwrite = TRUE)

cat("#' @rdname ", "OM-class-objects ", 
    "\n", '"', 'testOM', '"', "\n\n",
    sep = "", append = TRUE, file = file.path("R/", RoxygenFile))

ObjectClass <- 'OM'
cat("#' ", ObjectClass, " class objects",
    "\n#' ",
    "\n#' Example objects of class ", ObjectClass,
    "\n#' ",
    "\n#' @name ", ObjectClass, "-class-objects",
    "\n#' @format ", "NULL",
    "\n#' @examples",
    "\n#' avail(", '"', ObjectClass, '")',
    "\nNULL", "\n\n",
    sep = "", append = TRUE, file = file.path("R/", RoxygenFile))


