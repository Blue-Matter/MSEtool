library(MSEtool)

SingleStockOM <- OM(
  "Single Stock - Single Fleet",
  nSim  = 8,
  nYear = 20,
  pYear = 30
)

Stock(SingleStockOM) <- AlbacoreExStock

Fleet(SingleStockOM) <- AsympExFleet

Obs(SingleStockOM) <- AgeStructuredObs

usethis::use_data(SingleStockOM, overwrite = TRUE)
