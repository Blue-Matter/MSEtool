
la <- devtools::load_all
la()


dir <- "C:/Users/Admin/Documents/GitHub/SAFMC-MSE"

dir <- "C:/Users/Adrian/Documents/GitHub/SAFMC-MSE"


source(file.path(dir,'0. Specifications.R'))

OM_Dir <- file.path(dir, 'OM_Objects/Base')

################################################################################

# Slot Names and documentation

OM <- ImportBAM(Stock='GagGrouper', 
                nSim, 
                pYear, StockName='Gag Grouper', 
                populate = FALSE)

PopulateOM()

Hist <- Simulate(OM)


parallel=FALSE
silent=FALSE
nSim=NULL
RefPointsMSY=TRUE
Reduce=TRUE

Hist2 <- Simulate(OM, RefPointsMSY=Hist@RefPointsMSY)

Hist@RefPointsMSY
Hist2@RefPointsMSY

################################################################################




################################################################################

# Reference Points

################################################################################


################################################################################

# Combine Stocks & Fleets

################################################################################

# TODO -
# Ref Points - compare BAM and OM 


# Red Snapper - latest
# TODO

# ---- Black Sea Bass ----

# SEDAR 76 
# 1978 - 2021
# https://sedarweb.org/documents/sedar-76-stock-assessment-report-south-atlantic-black-sea-bass/

# Issues:
# - Mismatch in MSY reference points. OM predicts FMSY ~ Inf vs BAM value of 0.31

DiscMortDF <- data.frame(Fleet=c('cHL', 'cPT', 'cPT', 'rHB', 'rGN'),
                         Value=c(0.19,   0.14, 0.068, 0.152, 0.137),
                         Year= c(1977, 1977, 2007, 1977, 1977))

OM_BSB <- ImportBAM(Stock='BlackSeaBass', nSim=nSim, pYear=pYear,
                    StockName='Black Sea Bass',
                    DiscMortDF=DiscMortDF)

CompareBAM(Stock='BlackSeaBass', OM=OM_BSB)

# ---- Gag Grouper ----

# SEDAR 71 
# 1962 - 2019
# https://sedarweb.org/documents/sedar-71-stock-assessment-report-south-atlantic-gag/
  
OM_GG <- ImportBAM(Stock='GagGrouper', nSim=nSim, pYear=pYear, StockName='Gag Grouper')
CompareBAM('GagGrouper', OM=OM_GG) 

# ---- Gray Triggerfish ----

# SEDAR 82
# 1982- 2021
# https://sedarweb.org/documents/sedar-82-south-atlantic-gray-triggerfish-final-stock-assessment-report/

OM_GT <- ImportBAM(Stock, 
                   nSim=nSim, 
                   pYear=pYear,
                   StockName='Gray Triggerfish',
                   DiscMortDF=data.frame(Fleet=c('cHLs', 'rHBs', 'rGNs', 'rGNn'),
                                         Value=c(0.589),
                                         Year= c(1981)),
                   DiscFleets=c(rHBs="F.rHDs.D", 
                                rGNs="F.rGDs.D",
                                rGNn="F.rGDn.D"),
                   DiscSelFleets=c(rHBs="sel.m.rHDs", 
                                   rGNs="sel.m.rGDs",
                                   rGNn="sel.m.rGDs"),
                   RetSelFleets= c(cHLn="cHLs", rGNn='rGNs')
)
CompareBAM('GrayTriggerfish', OM=OM_GT)

# ---- Greater Amberjack ----

# SEDAR 59
# 1980 - 2017
# https://sedarweb.org/documents/sedar-59-stock-assessment-report-south-atlantic-greater-amberjack/

OM_GA <- ImportBAM(Stock='GreaterAmberjack', nSim=nSim, pYear=pYear, StockName='Greater Amberjack')
CompareBAM('GreaterAmberjack', OM=OM_GA)

# ---- Red Grouper -----

# SEDAR 53  
# 1976 - 2015
# https://sedarweb.org/documents/sedar-53-stock-assessment-report-south-atlantic-red-grouper/
OM_RG <- ImportBAM(Stock='RedGrouper', nSim=nSim, pYear=pYear, StockName='Red Grouper')
CompareBAM('RedGrouper', OM=OM_RG)

# ---- Red Porgy ----
# SEDAR 60
# 1972 - 2017
# https://sedarweb.org/documents/sedar-60-stock-assessment-report-south-atlantic-red-porgy/

OM_RP <- ImportBAM(Stock='RedPorgy', nSim=nSim, pYear=pYear)
CompareBAM('RedPorgy', OM=OM_RP)

# ---- Red Snapper ----

# SEDAR 73
# 1950 - 2019
# https://sedarweb.org/documents/sedar-73-stock-assessment-report-south-atlantic-red-snapper/
  
OM_RS <- ImportBAM('RedSnapper', nSim=nSim, pYear=pYear StockName='Red Snapper')
CompareBAM('RedSnapper', OM=OM_RS)

# ---- Red Snapper - Update ----

#TODO 


# ---- Scamp Grouper / Yellowmouth ----

# SEDAR 68
# 1969 - 2021
# https://sedarweb.org/documents/sedar-68oa-south-atlantic-scamp-operational-assessment-final-stock-assessment-report/

OM_SCG <- ImportBAM(Stock='ScampGrouper', nSim=nSim, pYear=pYear, StockName = 'Scamp Grouper')
CompareBAM(Stock='ScampGrouper', OM=OM_SCG)

# ---- Snowy Grouper ----

# SEDAR 36 - Update 2020
# 1974 - 2018

OM_SG <- ImportBAM(Stock='SnowyGrouper', nSim=nSim, pYear=pYear, StockName='Snowy Grouper')
CompareBAM('SnowyGrouper', OM=OM_SG)

# ---- Tilefish ----

# SEDAR 66
# 1972 - 2018
# https://sedarweb.org/documents/sedar-66-stock-assessment-report-south-atlantic-tilefish/

OM_TF <- ImportBAM(Stock='Tilefish', nSim=nSim, pYear=pYear)
CompareBAM('Tilefish', OM=OM_TF)

# ---- Vermilion Snapper -----

# SEDAR 55 
# 1946 - 2016 
# https://sedarweb.org/documents/sedar-55-stock-assessment-report-south-atlantic-vermilion-snapper/
  
OM_VS <- ImportBAM(Stock='VermilionSnapper', nSim=nSim, pYear=pYear,
                   StockName='Vermilion Snapper',
                   DiscSelFleets=c(rGN="sel.m.rHB.D"))
CompareBAM('VermilionSnapper', OM=OM_VS)




















