devtools::load_all()



# ---- Plot Stock ----

SingleStockOM@nSim <- 50
OM <- SingleStockOM |> Populate()
Hist <- Simulate(OM)
MSE <- Project(Hist, ExampleMPs()[1:2])

# Effort/TAC solver: TAC unachievable within the effort ceiling (saturated). (MP: "CurrentCatch", Year: 2027, Sim: 6)

MSE@Misc$Advice$CurrentCatch$`2027`$`6`$`Example Albacore Stock`@TAC

d <- PPD(MSE)
d$CurrentCatch$`1`$`Example Albacore Stock`@Landings@Value[,1]

d$CurrentCatch$`6`$`Example Albacore Stock`@Landings@Value[,1]



OM@Obs$`Example Albacore Stock`$AsympExFleet@Landings@Bias[6]
MSE@OM@Obs$`Example Albacore Stock`$AsympExFleet@Landings@Error[6,21]


r <- Removals(MSE) |> dplyr::filter(MP == 'CurrentCatch')
r

LoadArgs(Project)


for (object in list(OM, Hist, MSE, Stock(OM)[[1]])) {
  print(class(PlotLength(object)))
  print(class(PlotWeight(object)))
  print(class(PlotNaturalMortality(object)))
  print(class(PlotMaturity(object)))
  print(class(PlotFecundity(object)))
  print(class(PlotSRR(object)))
  print(class(PlotDepletion(object)))
  print(class(PlotSpatial(object)))
}

PlotStock(Stock(OM)[[1]])
PlotStock(OM)
PlotStock(Hist)

# ---- Plot Fleet ----

# should handle in the same way as stock:
# - fleet (single fleet, needs an optional Stock -- falls back to an
#   example stock, with a message, when one isn't supplied)
# - om (single or multi-fleet)
# - hist
# - mse

bareFleet <- OM@Fleet[[1]][[1]]

for (object in list(OM, Hist, MSE, bareFleet)) {
  print(class(PlotSelectivity(object)))
  print(class(PlotRetention(object)))
  print(class(PlotDiscardMortality(object)))
  print(class(PlotEffort(object)))
  print(class(PlotCatchability(object)))
  print(class(PlotClosure(object)))   # NULL unless a closure is actually specified
}

# explicit Stock instead of the example-stock fallback
PlotSelectivity(bareFleet, Stock = AlbacoreExStock)

# ---- Plot Obs ----
# TODO
# should handle in the same way as stock:
# - obs
# - om
# - hist
# - mse
PlotObs(object)
# everything else

PlotCompShift(object)

# ---- Plot Imp ----
# TODO
# should handle in the same way as stock:
# - imp
# - om
# - hist
# - mse

PlotImp(object)
# everything else
