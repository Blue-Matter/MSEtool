fls <- list.files('data-raw', full.names = TRUE)

stocks <- fls[grepl('ex-stock', fls)]
fleets <- fls[grepl('ex-fleet', fls)]
obs    <- fls[grepl('ex-obs', fls)]
imps   <- fls[grepl('ex-imp', fls)]
oms    <- fls[grepl('ex-om', fls)]

purrr::map(stocks, source)

purrr::map(fleets, source)

# rebuild because fleets code uses Populate on some stocks
purrr::map(stocks, source)

purrr::map(obs, source)

purrr::map(imps, source)

purrr::map(oms, source)

