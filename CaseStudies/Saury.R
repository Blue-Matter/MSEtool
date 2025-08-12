library(MSEtool)

la <- devtools::load_all
la()

replist <- readRDS("C:/Users/Adrian/Downloads/Step19_r4ss.rds")

OM <- ImportSS(replist)




Hist <- Simulate(OM)
