
# pak::pkg_install('blue-matter/MSEtool@prerelease')

library(MSEtool)

SSDir <- "G:/Shared drives/BM shared/1. Projects/openMSE v2/CaseStudies/BrazilLobster"

nSim <- 5 # number of simulations - set low for testing
pYear <- 10 # number of projection years

# SS3 model not seasonal? 
# ImportSS crashes in `GetSS_R0` 
# SS model has platoons - currently not supported
OM <- ImportSS(SSDir,
               Name='Lobster',
               nSim = nSim,
               pYear = pYear)


# ---- Development & Testing -----

la() # load all internal functions
LoadArgs(ImportSS) # loads default arguments where values don't exist in global

ImportSS
