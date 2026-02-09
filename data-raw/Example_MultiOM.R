
Mackerel <- Convert(MSEtool::Mackerel)
Herring <- Convert(MSEtool::Herring)

Fleet_1 <- Convert(MSEtool::Generic_DecE)
Fleet_2 <- Convert(MSEtool::Generic_FlatE)

OM <- OM(Name = "Example 2 Stock 2 Fleet OM",
         Author = 'Adrian Hordyk',
         Email = 'adrian@bluematterscience.com',
         nYear = 50,
         pYear = 20,
         CurrentYear = 2026,
         Stock = list(
           Mackerel=Mackerel,
           Herring=Herring
         ),
         Fleet = list(
           Mackerel = list(
             Fleet_1 = Fleet_1,
             Fleet_2 = Fleet_2
           ),
           Herring = list(
             Fleet_1 = Fleet_1,
             Fleet_2 = Fleet_2
           )
         )
)

la()
LoadArgs(PopulateOM)

OM <- PopulateOM(OM)

LoadArgs(Simulate_om)


Hist <- Simulate(OM)
