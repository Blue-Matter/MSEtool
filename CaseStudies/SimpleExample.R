library(MSEtool)

# Based on MSEtool::testOM

# ---- Operating Model ----

SimpleOM <- OM('Example OM',
               Author='Adrian Hordyk',
               Agency='Blue Matter Science',
               Email='adrian@bluematterscience.com',
               nYear=30,
               pYear=20,
               nSim=2)

nStock(SimpleOM)
nFleet(SimpleOM)

Populate(SimpleOM)


## ---- Stock -----
SimpleStock <- Stock('Example Stock',
                     CommonName = 'Albacore',
                     Species = "Thunnus alalunga")

Ages(SimpleStock) <- Ages(MaxAge=20)
MaxAge(SimpleStock)
MinAge(SimpleStock)

MaxAge(SimpleStock) <- 15
MaxAge(SimpleStock)

SimpleStock |> Ages() |> Classes()
SimpleStock |> Ages() |> Units()



## ---- Fleet ----



SimpleOM
