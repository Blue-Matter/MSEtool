library(MSEtool)

la()

OM_orig <- readRDS("C:/Users/Admin/Documents/GitHub/SALB-MSE/OM/Grid_h0.7_M0.3.om")

GridDir <- 'G:/Shared drives/BM shared/1. Projects/TOF-MSE-SALB/ALB-S_Unc-Grid/ALB-S_Unc-Grid'
GridDirs <- list.dirs(file.path(GridDir), full.names = FALSE, recursive = FALSE)

RepList <- ImportSSReport(file.path(GridDir, GridDirs[1]))

OM <- ImportSS(RepList, populate=FALSE, StockName='Albacore')
OM <- PopulateOM(OM)

Hist <- Simulate_om(OM)

CompareSSNumber(RepList[[1]], Hist)
CompareSSLandings(RepList[[1]], Hist)


replist <- RepList$`1`

Natage <- GetSSNatAge(replist, OM, yrs=1956)

Number(Hist, byAge=TRUE) |> dplyr::filter(TimeStep==1956)

Natage


birthseas <- ifelse(is.null(replist$birthseas), 
                    1, 
                    max(replist$birthseas)) 


