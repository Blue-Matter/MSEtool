library(MSEtool)

la()

SSDir <- "G:/Shared drives/BM shared/1. Projects/EcoTest/Assessments/BET2021"

RepList <- ImportSSReport(SSDir)

LoadArgs('ImportSS')
OM <- ImportSS(RepList, populate=FALSE)

OM <- PopulateOM(OM)

Hist <- Simulate_om(OM)