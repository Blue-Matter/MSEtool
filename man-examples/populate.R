\dontrun{
# Populate a full operating model
OM <- Populate(OM)

# Populate a stock with specific arguments
Stock <- Populate(Stock, nYear = 40, pYear = 20, nSim = 100)

# Populate a fleet using an already-populated stock
Fleet <- Populate(Fleet, Stock = Stock, nSim = 100)
}
