Hist <- Simulate(SingleStockOM, nSim = 2)
PlotBiomass(Hist)
PlotF(Hist, relative = 'FMSY')
PlotDynamics(Hist)

MSE <- Project(Hist, c('CurrentEffort', 'CurrentCatch'))
PlotBiomass(MSE, byMP = TRUE)
PlotF(MSE, relative = 'FMSY')
PlotDynamics(MSE)

\dontrun{
MHist <- Simulate(MultiStockOM)
PlotSBiomass(MHist, byStock = TRUE)
PlotLandings(MHist, byStock = TRUE, byFleet = TRUE)

# restrict to specific stocks, by name or index
PlotSBiomass(MHist, Stocks = 'Example Butterfish Stock')
}
