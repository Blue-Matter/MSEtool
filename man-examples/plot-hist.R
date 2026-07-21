\dontrun{
Hist <- Simulate(SingleStockOM)
PlotBiomass(Hist)
PlotF(Hist, relative = 'FMSY')
PlotDynamics(Hist)

MSE <- Project(Hist, ExampleMPs())
PlotBiomass(MSE, byMP = TRUE)
PlotF(MSE, relative = 'FMSY')
PlotDynamics(MSE)

MHist <- Simulate(MultiStockOM)
PlotSBiomass(MHist, byStock = TRUE)
PlotLandings(MHist, byStock = TRUE, byFleet = TRUE)

# restrict to specific stocks, by name or index
PlotSBiomass(MHist, Stocks = 'Female')
}
