
la <- devtools::load_all

la()

dir <- "C:/Users/Admin/Documents/GitHub/SAFMC-MSE"

source(file.path(dir,'0. Specifications.R'))

OM_Dir <- file.path(dir, 'OM_Objects/Base')


OM_RS <- ImportBAM('RedSnapper', nSim=nSim, pYear=pYear)
CompareBAM('RedSnapper', OM=OM_RS)

OM_GG <- ImportBAM(Stock='GagGrouper', nSim=nSim, pYear=pYear)
CompareBAM('GagGrouper', OM=OM_GG)

################################################################################

OM_BSB <- ImportBAM(Stock='BlackSeaBass', nSim=nSim, pYear=pYear)
CompareBAM('BlackSeaBass', OM=OM_BSB, ConvertUnits = 0.453592)

BAMdata <- BAMGetObject('BlackSeaBass')
Hist <- Simulate(OM_BSB)



################################################################################

# Add other Stocks ...




BAMDir <- "G:/Shared drives/BM shared/1. Projects/SAFMC Snapper - Grouper/BAM/RedSnapper"
BAMOutput <- list()
BAMOutput$rdat <- dget(file.path(BAMDir, 's73u.rdat'))
BAMOutput$dat <- readLines(file.path(BAMDir, 's73u.dat'))

OM_RS_Update <- ImportBAM(Stock=BAMOutput, nSim=nSim, pYear=pYear)

# TODO - need to simulate historical rec devs for terminal year (if it is NA)
# make option to simulate terminal year rec dev any ...
OM_RS_Update@Stock$`SA Red Snapper`@SRR@RecDevHist[1,]

CompareBAM(BAMOutput, OM_RS_Update, 2.205)



BAMdata <- BAMGetObject() 

BAMdata <- BAMGetObject(BAMOutput) 








