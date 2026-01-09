
OM <- Populate(ExampleOM)
nSim(OM)
OM |> Stock(1) |> Length() |> MeanAtAge() |> dim()

OM_reduced <- ReduceNSim(OM, 2)
nSim(OM_reduced)
OM_reduced |> Stock(1) |> Length() |> MeanAtAge() |> dim()
