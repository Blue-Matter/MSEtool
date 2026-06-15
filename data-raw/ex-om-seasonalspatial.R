library(MSEtool)

## ---- seasonalspatial-create-om ----
SeasonalSpatialOM <- OM(
  Name    = "Seasonal Spatial - Single Fleet",
  nSim    = 8,
  nYear   = 20,
  pYear   = 30,
  Seasons = 12,
  Stock   = SeasonalSpatialExStock,
  Fleet   = DomeExFleet,
  Obs     = CommercialFleetObs
)

## ---- save object ----
usethis::use_data(SeasonalSpatialOM, overwrite = TRUE)
