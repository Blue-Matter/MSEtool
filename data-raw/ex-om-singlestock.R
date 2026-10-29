library(MSEtool)

## ---- singlestock-create-om ----
SingleStockOM <- OM(
  Name   = "Single Stock - Single Fleet",
  nSim   = 8,
  nYear  = 20,
  pYear  = 30,
  Stock  = AlbacoreExStock,
  Fleet  = AsympExFleet,
  Obs    = AgeStructuredObs,
  Imp    = FullComplianceImp
)

## ---- save object ----
usethis::use_data(SingleStockOM, overwrite = TRUE)


## ---- singlestock-assignment ----
Stock(SingleStockOM) <- AlbacoreExStock
Fleet(SingleStockOM) <- AsympExFleet
Obs(SingleStockOM)   <- AgeStructuredObs
Imp(SingleStockOM)   <- FullComplianceImp

## ---- singlestock-assignment2 ----
Stock(SingleStockOM) <- list(AlbacoreExStock)
Fleet(SingleStockOM) <- list(list(AsympExFleet))
Obs(SingleStockOM)   <- list(list(AgeStructuredObs))
Imp(SingleStockOM)   <- list(list(FullComplianceImp))

