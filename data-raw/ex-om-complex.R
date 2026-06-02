library(MSEtool)

ComplexOM <- OM(
  "Stock Complex - Single Fleet",
  nSim      = 8,
  nYear     = 20,
  pYear     = 30,
  Complexes = list(Stock_Complex = 1:2)
)

Stock(ComplexOM) <- list(AlbacoreExStock, ButterfishExStock)

Fleet(ComplexOM) <- AsympExFleet

Obs(ComplexOM) <- AgeStructuredObs

usethis::use_data(ComplexOM, overwrite = TRUE)

