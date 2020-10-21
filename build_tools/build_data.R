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
  temp <- new("Imp", fl)
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

# ---- Build testOM Object ----
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


# ---- Build testMOM Object ----

nsim <- 20

Albacore@Msd <- Albacore@Ksd <- Albacore@Linfsd <- c(0, 0)

BB_fleet_1 <- IncE_HDom

BB_fleet_1@L5 <- c(45, 50)
BB_fleet_1@LFS <- c(55, 60)
BB_fleet_1@Vmaxlen <- c(0.05, 0.2)
BB_fleet_1@isRel <- FALSE

BB_fleet_1@EffLower[3] <- 0.3
BB_fleet_1@EffUpper[3] <- 0.4

LL_fleet_2 <- FlatE_NDom

LL_fleet_2@L5 <- c(75, 80)
LL_fleet_2@LFS <- c(100, 110)

LL_fleet_2@EffLower[3] <- 0.2
LL_fleet_2@EffUpper[3] <- 0.4

LL_fleet_2@isRel <- FALSE

Stocks <- list(Albacore)
Fleets <- list(list(BB_fleet_1, LL_fleet_2))
Obs <- list(list(Precise_Unbiased, Precise_Unbiased))
Imps <- list(list(Perfect_Imp, Perfect_Imp))
CatchFrac <- list(matrix(rep(c(0.3, 0.7), each = nsim), nrow = nsim)) # Terminal year catch is 30%-70% ratio baitboat and longline, respectively


Albacore_TwoFleet <- new("MOM", Stocks = Stocks, Fleets = Fleets, Obs = Obs, 
                         Imps = Imps, CatchFrac = CatchFrac, nsim = nsim)
usethis::use_data(Albacore_TwoFleet, overwrite = TRUE)


cat("#' @rdname ", "MOM-class-objects ", 
    "\n", '"', 'Albacore_TwoFleet', '"', "\n\n",
    sep = "", append = TRUE, file = file.path("R/", RoxygenFile))

ObjectClass <- 'MOM'
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


# ---- Simulated Data ----
Hist <- Simulate()
SimulatedData <- Hist@Data 

usethis::use_data(SimulatedData, overwrite = TRUE)

clss <- 'Data'
name <- 'SimulatedData'
cat("#'  ", name, " ", clss,
    "\n#'", 
    "\n#'  An object of class ", clss, 
    "\n#'\n",
    '"', name, '"\n\n\n', sep="", append=TRUE, 
    file=file.path('R/', RoxygenFile))  




