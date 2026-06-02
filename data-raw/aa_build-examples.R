fls <- list.files('data-raw', full.names = TRUE)

stocks <- fls[grepl('ex-stock', fls)]
fleets <- fls[grepl('ex-fleet', fls)]
obs    <- fls[grepl('ex-obs', fls)]
oms    <- fls[grepl('ex-om', fls)]

purrr::map(stocks, source)

purrr::map(fleets, source)

purrr::map(obs, source)

purrr::map(oms, source)

