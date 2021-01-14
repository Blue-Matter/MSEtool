library(usethis)
library(MSEtool)
library(SAMtool)
library(DLMtool)
library(dplyr)

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

# ---- Import Data XL (internal data) ----
DataSlots <- readxl::read_xlsx('build_tools/Class_definitions/Class_definitions.xlsx',
                                sheet='Data')
DataSlots$Numeric[is.na(DataSlots$Numeric)] <- TRUE
DataSlots$Timeseries[is.na(DataSlots$Timeseries)] <- FALSE

usethis::use_data(DataSlots, internal = FALSE, overwrite = TRUE)

clss <- class(DataSlots)
name <- "DataSlots"
cat("#'  ", name, " ",
    "\n#'",
    "\n#'  Dataframe with details of slots in Dat object",
    "\n#'",
    "\n#'\n",
    '"', name, '"\n\n\n', sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))


# ---- Build Data Objects ----
files <- list.files("build_tools/Objects/Data", full.names = TRUE)
for (fl in files) {
  temp <- new("Data", fl)
  name <- strsplit(basename(fl), '.csv')[[1]]
  assign(name, temp)
  do.call("use_data", list(as.name(name), overwrite = TRUE))

  cat("#' @rdname ", "Data-class-objects ",
      "\n", '"', name, '"', "\n\n",
      sep = "", append = TRUE, file = file.path("R/", RoxygenFile))
}

ObjectClass <- 'Data'
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
OM <- testOM
OM@reps <- 3
Hist <- Simulate(OM)
SimulatedData <- Hist@Data

dims <- dim(SimulatedData@Ind)
n.ind <- 15 # add 15 additional indices for Simulated Data for robustness
SimulatedData@AddInd <- array(SimulatedData@Ind, dim=c(dims[1],n.ind,dims[2]))
SimulatedData@AddIndV <- array(1, dim=c(dims[1],n.ind,SimulatedData@MaxAge+1))
SimulatedData@AddIndType <- rep(1, n.ind)
SimulatedData@AddIunits <- rep(1, n.ind)
SimulatedData@CV_AddInd <- array(0.1, dim=c(dims[1],n.ind,dims[2]))

usethis::use_data(SimulatedData, overwrite = TRUE)

clss <- 'Data'
name <- 'SimulatedData'
cat("#'  ", name, " ", clss,
    "\n#'",
    "\n#'  An object of class ", clss,
    "\n#'\n",
    '"', name, '"\n\n\n', sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))




# ---- Build MP Required Data Object -----

message("\nLooping over Data slots to determine required Data for each built-in MP")

MPs <- avail("MP")

Data <- SimulatedData

ig.slots <- c("Name", "Species", "Common_Name", "Region", "Misc", "OM", "TAC", "Sense", "Units", "Ref", "Ref_type",
              "Log", "params", "PosMPs", "MPs", "Obs", "nareas")
slts <- slotNames(Data)
slts <- slts[!slts %in% ig.slots]

mat <- matrix(NA, nrow=length(MPs), ncol=length(slts))
colnames(mat) <- slts
rownames(mat) <- MPs
mat <- as.data.frame(mat)

for (sl in slts) {
  message("Slot: ", sl, " (", match(sl, slts), " of ", length(slts), ")")
  tData <- Data
  cls <- class(slot(tData, sl) )

  if ("matrix" %in% cls) {
    slot(tData, sl) <-  get(cls)(NA)
  } else if ("integer" %in% cls) {
    slot(tData, sl) <-  get(cls)(0)
  } else if ("array" %in% cls){
    slot(tData, sl) <-  get(cls)(0)
  } else{
    slot(tData, sl) <- as.numeric(NA)
  }

  canMPs <- Can(tData, dev=TRUE)

  cant <- MPs[!MPs %in% canMPs]

  mat[[sl]][match(cant, MPs)] <- TRUE
  mat[[sl]][match(canMPs, MPs)] <- FALSE
}

ReqData <- matrix(NA, nrow=length(MPs), ncol=2)
for (r in 1:nrow(mat)) {
  ReqData[r,1] <- rownames(mat)[r]
  ReqData[r,2] <- paste(sort(colnames(mat)[which(mat[r,]==TRUE)]), collapse=", ")
}
ReqData <- as.data.frame(ReqData, stringsAsFactors=FALSE)
colnames(ReqData) <- c("MP", "Data")

usethis::use_data(ReqData, internal = FALSE, overwrite = TRUE)

clss <- class(ReqData)
name <- "ReqData"
cat("#'  ", name, " ",
    "\n#'",
    "\n#'  Dataframe with required data slots for built-in MPs",
    "\n#'",
    "\n#'\n",
    '"', name, '"\n\n\n', sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))



# ---- Add life-history data-base as data object  ----

# devtools::install_github("james-thorson/FishLife")
library(FishLife)

db <- FishLife::FishBase

LHdatabase <- list(Cov_gjj=db$Cov_gvv, ParentChild_gz=db$ParentChild_gz,
                   ParHat=db$ParHat)

usethis::use_data(LHdatabase, internal = FALSE, overwrite = TRUE)

# Write roxygen
chk <- file.exists(file.path('R/', RoxygenFile))
if(!chk) file.create(file.path('R/', RoxygenFile)) # make empty file
clss <- class(LHdatabase)
name <- "LHdatabase"
cat("#'  ", name, " ",
    "\n#'",
    "\n#'  Database from the FishLife package with predicted life-history parameters for all species on FishBase",
    "\n#' ",
    "\n#' @references Thorson, J. T., S. B. Munch, J. M. Cope, and J. Gao. 2017. ",
    "\n#' Predicting life history parameters for all fishes worldwide. Ecological Applications. 27(8): 2262--2276  ",
    "\n#' @source \\url{https://github.com/James-Thorson-NOAA/FishLife/} ",
    "\n#'",
    "\n#'\n",
    '"', name, '"\n\n\n', sep="", append=TRUE,
    file=file.path('R/', RoxygenFile))



# --- Object Descriptions ----
slot_ripper<-function(filenam,slots){

  ns<-length(slots)
  sind<-rep(TRUE,ns)
  out<-readLines(filenam,skipNul=T)
  no<-length(out)
  out2<-data.frame(matrix(NA,ncol=2,nrow=ns))
  names(out2)<-c("Slot","Description")

  k=TRUE # Before slot text?
  ss<-0 # Slot counter

  for(i in 1:no){
    test<-scan(filenam,skip=i-1,what='character',nlines=1)
    # readline(test)
    nt<-length(test)
    if(nt>0)
      if(k & test[1]=="_\bS_\bl_\bo_\bt_\bs:")k=FALSE

    if(nt==0&!k){ # new slot?
      moretext=FALSE
      ss<-ss+1
    }

    if(!(nt==0|substr(test[1],1,1)=="_"|k)){ # text, not a header, after slot text starts

      if(test[1]%in%slots[sind]){
        sind[match(test[1],slots)]=FALSE
        out2[ss,1]<-test[1]
        out2[ss,2]<-paste(test[2:nt],collapse=" ")
        moretext=TRUE
      }else{
        bg <- 1 # max(2, length(test))
        if (is.na(out2[ss,2])) out2[ss,2] <- ""
        # detect bullet point
        ttt <- test[bg:nt]
        if (any(grepl("•", ttt))) {
          ttt[grepl("•", ttt)] <- "-"
          out2[ss,2]<-paste(c(out2[ss,2],ttt),collapse=" ")
          out2[ss,1] <- ''
        } else {
          if(moretext) out2[ss,2]<-paste(c(out2[ss,2],test[bg:nt]),collapse=" ")
        }

      }

    }

  }
  ind <- which(rowSums(apply(out2, 2, is.na)) ==  0)
  out2[ind,]

}

getDescription <- function(class=c("Stock", "Fleet", "Obs", "Imp", "Data", "OM", "MSE"),
                           Rdloc='man',
                           Outloc=NULL) {
  class <- match.arg(class)
  if (is.null(Outloc)) Outloc <- tempdir()

  rdloc <- paste0(file.path(Rdloc, class), "-class.Rd")
  outloc <- paste0(file.path(Outloc, class), "-class.txt")
  call <- paste("R CMD Rd2txt", rdloc, "-o", outloc)
  system(call)
  tt <- slot_ripper(filenam=paste0(file.path(Outloc, class), "-class.txt"), slots=slotNames(class))
  name <- paste0(class, "Description")
  assign(name, tt)


  do.call("use_data", list(as.name(name), overwrite = TRUE))

  # Write roxygen
  chk <- file.exists(file.path('R/', RoxygenFile))
  if(!chk) file.create(file.path('R/', RoxygenFile)) # make empty file

  cat("#'  ", name, " ",
      "\n#'",
      "\n#'  A data.frame with description of slots for class ", class,
      "\n#'\n",
      '"', name, '"\n\n\n', sep="", append=TRUE,
      file=file.path('R/', RoxygenFile))

  file.remove(paste0(file.path(Outloc, class), "-class.txt"))


}

Outloc <- tempdir()
getDescription("Stock", Outloc=Outloc)
getDescription("Fleet", Outloc=Outloc)
getDescription("Obs", Outloc=Outloc)
getDescription("Imp", Outloc=Outloc)
getDescription("Data", Outloc=Outloc)
getDescription("OM", Outloc=Outloc)
getDescription("MSE", Outloc=Outloc)


### Add cpars_info to sysdata.rdata ####
makeDF <- function(df_in, type=NULL) {
  df_in$ValidCpars[is.na(df_in$ValidCpars)] <- TRUE
  df_in <- df_in %>% dplyr::filter(ValidCpars!=FALSE)
  data.frame(Var=df_in$Slot, Dim=df_in$Cpars_dim, Desc=df_in$Cpars_desc, Type=type)
}

cpars_Stock <- openxlsx::read.xlsx("build_tools/Class_definitions/Class_definitions.xlsx",
                                   sheet="Stock") %>%
  makeDF(., 'Stock')
cpars_Fleet <- openxlsx::read.xlsx("build_tools/Class_definitions/Class_definitions.xlsx",
                                   sheet="Fleet") %>%
  makeDF(., 'Fleet')
cpars_Obs <- openxlsx::read.xlsx("build_tools/Class_definitions/Class_definitions.xlsx",
                                 sheet="Obs") %>%
  makeDF(., 'Obs')
cpars_Imp <- openxlsx::read.xlsx("build_tools/Class_definitions/Class_definitions.xlsx",
                                 sheet="Imp")%>%
  makeDF(., 'Imp')


cpars_internal <- openxlsx::read.xlsx("build_tools/Class_definitions/Class_definitions.xlsx",
                                      sheet="cpars")


cpars_info <- dplyr::bind_rows(cpars_Stock,cpars_Fleet,cpars_Obs, cpars_Imp,cpars_internal)

usethis::use_data(cpars_info, internal = TRUE, overwrite = TRUE)
