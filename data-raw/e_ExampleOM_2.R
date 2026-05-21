library(MSEtool)

## ---- create-om ----
ExampleOM2 <- OM("Single Stock - Multi Fleet",
                nYear = 30,
                pYear = 20,
                nSim = 5
)


# ---- Example Obs ----

Stock(ExampleOM2) <- ExampleStock
Fleet(ExampleOM2) <- list(list(ExampleFleet,
                               ExampleFleet2)
)

usethis::use_data(ExampleOM2, overwrite = TRUE)