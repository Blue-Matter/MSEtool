# Populate a full operating model
OM <- Populate(SingleStockOM)

# Populate a stock with specific arguments
Stock <- Populate(AlbacoreExStock, nYear = 40, pYear = 20, nSim = 10)

# Populate a fleet using an already-populated stock
Fleet <- Populate(AsympExFleet, Stock = Stock)
