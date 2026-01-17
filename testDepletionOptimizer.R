library(MSEtool)
la()

# update code and test for depletion optimizer
OM <- ExampleOM

LoadArgs(Simulate_om)

Hist <- Simulate_om(OM)

OM <- Populate(OM)
OM@Fleet$`Example Stock`$`Example Fleet`@Retention@MeanAtLength
ExtendAreas