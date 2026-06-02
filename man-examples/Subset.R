nSim(SingleStockOM) <- 10
OM <- Populate(SingleStockOM)

# Subset Sims and Years
RecDevs <- OM |>
  Stock(1) |>
  SRR() |>
  RecDevHist()

Subset(RecDevs, Sims=1:2, Years = 1997:2000)


# Subset Ages
MeanLength <- OM |>
  Stock(1) |>
  Length() |>
  MeanAtAge()

Subset(MeanLength, Sim=1:2, Ages=0:3)




