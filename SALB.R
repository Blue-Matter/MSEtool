la <- devtools::load_all

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




TS <- TimeSteps(Hist, 'H')
replist <- RepList$`1`
ageclasses <- GetSSAgeClasses(replist)

replist$recruit |> head()
plot(replist$recruit$Yr, replist$recruit$exp_recr)
lines(replist$recruit$Yr[1:63], Hist@Number$Albacore[1,1,,1])


# Track N-at-Age 
N_OM <- Hist@Number$Albacore[1,,,1]
N_SS <- replist$natage

# Track F-at-Age 
fl <- 1

F_OM <- Hist@FDead$Albacore[1,,,fl]
F_SS <- replist$fatage |> dplyr::filter(Yr%in%TS, Fleet==fl)
F_SS <- F_SS[,as.character(ageclasses)] |> t()

ts <- 30
plot(F_SS[,ts])
lines(F_OM[,ts])






