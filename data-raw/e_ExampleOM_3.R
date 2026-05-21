library(MSEtool)

## ---- create-om ----
ExampleOM3 <- OM("Multi Stock - Single Fleet",
                 nYear = 30,
                 pYear = 20,
                 nSim = 5
)
ExampleOM3@Complexes <- list(Stock_Complex = 1:2)


# ---- Example Obs ----

Stock(ExampleOM3) <- list(ExampleStock, ExampleStock2)

Fleet(ExampleOM3) <- list(list(ExampleFleet),
                          list(ExampleFleet)
                          )

usethis::use_data(ExampleOM3, overwrite = TRUE)
