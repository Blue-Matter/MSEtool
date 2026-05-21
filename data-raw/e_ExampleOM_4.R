library(MSEtool)

## ---- create-om ----
ExampleOM4 <- OM("Multi Stock - Multi Fleet",
                 nYear = 30,
                 pYear = 20,
                 nSim = 5
)


# ---- Example Obs ----

Stock(ExampleOM4) <- list(ExampleStock, ExampleStock2)

Fleet(ExampleOM4) <- list(list(ExampleFleet, ExampleFleet2),
                          list(ExampleFleet, ExampleFleet2)
                          )

usethis::use_data(ExampleOM4, overwrite = TRUE)
