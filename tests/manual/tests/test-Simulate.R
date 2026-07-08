library(MSEtool)

MPs <- ExampleMPs()


# ---- SingleStockOM: 1 stock, 1 fleet ----

SingleStock_Hist <- Simulate(SingleStockOM, silent = TRUE)

PlotNumber(SingleStock_Hist)
PlotBiomass(SingleStock_Hist)
PlotSBiomass(SingleStock_Hist)
PlotSProduction(SingleStock_Hist)
PlotLandings(SingleStock_Hist)
PlotDiscards(SingleStock_Hist)
PlotRemovals(SingleStock_Hist)
PlotDynamics(SingleStock_Hist)
plot(SingleStock_Hist)

# option checks: nsim overlay, fixed y-scale, year subset, custom quantiles
PlotBiomass(SingleStock_Hist, nsim = 10)
PlotBiomass(SingleStock_Hist, Years = 2015:2026)
PlotSBiomass(SingleStock_Hist, probs = c(0.2, 0.8))

SingleStock_MSE <- Project(SingleStock_Hist, MPs = MPs, silent = TRUE)

PlotNumber(SingleStock_MSE)
PlotBiomass(SingleStock_MSE)
PlotSBiomass(SingleStock_MSE)
PlotSProduction(SingleStock_MSE)
PlotLandings(SingleStock_MSE)
PlotDiscards(SingleStock_MSE)
PlotRemovals(SingleStock_MSE)
PlotDynamics(SingleStock_MSE)
plot(SingleStock_MSE)

PlotBiomass(SingleStock_MSE, nsim = 5)   # Sim x MP grouping


# ---- TwoFleetOM: 1 stock, 2 fleets ----

TwoFleet_Hist <- Simulate(TwoFleetOM, silent = TRUE)

PlotNumber(TwoFleet_Hist)
PlotBiomass(TwoFleet_Hist)
PlotSBiomass(TwoFleet_Hist)
PlotSProduction(TwoFleet_Hist)

PlotLandings(TwoFleet_Hist)                 
PlotLandings(TwoFleet_Hist, byFleet = FALSE)
PlotDiscards(TwoFleet_Hist)
PlotDiscards(TwoFleet_Hist, byFleet = FALSE)

PlotRemovals(TwoFleet_Hist)
PlotRemovals(TwoFleet_Hist, byFleet = FALSE)

PlotDynamics(TwoFleet_Hist)
plot(TwoFleet_Hist)

TwoFleet_MSE <- Project(TwoFleet_Hist, MPs = MPs, silent = TRUE)

PlotNumber(TwoFleet_MSE)
PlotNumber(TwoFleet_MSE, byMP = TRUE)
PlotNumber(TwoFleet_MSE, byMP = TRUE, IncHist = FALSE)
PlotNumber(TwoFleet_MSE, IncHist = FALSE)

PlotBiomass(TwoFleet_MSE)
PlotSBiomass(TwoFleet_MSE)
PlotSProduction(TwoFleet_MSE)

PlotLandings(TwoFleet_MSE)                   # Fleet facets x MP color
PlotLandings(TwoFleet_MSE, byFleet = FALSE)
PlotLandings(TwoFleet_MSE, byFleet = FALSE, byMP = TRUE)
PlotDiscards(TwoFleet_MSE)
PlotRemovals(TwoFleet_MSE)                   
PlotRemovals(TwoFleet_MSE, byFleet = TRUE)   

PlotDynamics(TwoFleet_MSE)
plot(TwoFleet_MSE)


# ---- MultiStockOM: multiple stocks, multiple fleets ----
MultiStock_Hist <- Simulate(MultiStockOM, silent = TRUE)

PlotNumber(MultiStock_Hist)
PlotNumber(MultiStock_Hist, byStock = FALSE)
PlotBiomass(MultiStock_Hist)
PlotBiomass(MultiStock_Hist, byStock = FALSE)
PlotSBiomass(MultiStock_Hist)
PlotSProduction(MultiStock_Hist)


PlotLandings(MultiStock_Hist)
PlotLandings(MultiStock_Hist, byFleet = FALSE)
PlotLandings(MultiStock_Hist, byStock = FALSE, byFleet = FALSE)  
PlotDiscards(MultiStock_Hist)
PlotRemovals(MultiStock_Hist)
PlotDynamics(MultiStock_Hist)
plot(MultiStock_Hist)
plot(MultiStock_Hist, byStock = FALSE)

MultiStock_MSE <- Project(MultiStock_Hist, MPs = MPs, silent = TRUE)

PlotBiomass(MultiStock_MSE)
PlotSBiomass(MultiStock_MSE, byStock = FALSE)
PlotLandings(MultiStock_MSE, byStock = TRUE, byFleet = TRUE)   # Stock x Fleet facets, MP color
PlotRemovals(MultiStock_MSE)
PlotDynamics(MultiStock_MSE)
plot(MultiStock_MSE)


# ---- ComplexOM: 2 stocks sharing one complex, single shared fleet type ----
Complex_Hist <- Simulate(ComplexOM, silent = TRUE)

PlotNumber(Complex_Hist)
PlotBiomass(Complex_Hist)
PlotSBiomass(Complex_Hist)
PlotSProduction(Complex_Hist)
PlotLandings(Complex_Hist)
PlotDiscards(Complex_Hist)
PlotRemovals(Complex_Hist)

PlotDynamics(Complex_Hist)
plot(Complex_Hist)

Complex_MSE <- Project(Complex_Hist, MPs = MPs, silent = TRUE)

PlotBiomass(Complex_MSE)
PlotRemovals(Complex_MSE)
PlotDynamics(Complex_MSE)
plot(Complex_MSE)


# ---- SeasonalSpatialOM: single stock, seasonal + spatial structure ----
Seasonal_Hist <- Simulate(SeasonalSpatialOM, silent = TRUE)

PlotNumber(Seasonal_Hist)
PlotBiomass(Seasonal_Hist)
PlotSBiomass(Seasonal_Hist)
PlotSProduction(Seasonal_Hist)
PlotLandings(Seasonal_Hist)
PlotDiscards(Seasonal_Hist)
PlotRemovals(Seasonal_Hist)
PlotDynamics(Seasonal_Hist)
plot(Seasonal_Hist)

Seasonal_MSE <- Project(Seasonal_Hist, MPs = MPs, silent = TRUE)

PlotBiomass(Seasonal_MSE)
PlotRemovals(Seasonal_MSE)
PlotDynamics(Seasonal_MSE)
plot(Seasonal_MSE)


