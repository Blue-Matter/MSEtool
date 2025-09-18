
devtools::load_all()

dir <- "G:/Shared drives/BM shared/1. Projects/TOF Advisory/Blue Shark MSE Workshop/MaterialToShare"


SSdir <- file.path(dir, 'SS3_Output', 'IndianOcean')
SSOutput <- ImportSSReport(SSdir)

OM <- ImportSS(SSOutput)

Hist <- Simulate(OM)


Hist@Biomass[1,,]

# TODO - check all these and compare against current version